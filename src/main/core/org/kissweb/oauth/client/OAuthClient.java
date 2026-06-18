package org.kissweb.oauth.client;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.RestClient;
import org.kissweb.json.JSONObject;

import java.io.IOException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;
import java.sql.SQLException;
import java.time.Duration;
import java.util.Base64;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * High-level entry point for using a remote OAuth 2.1-protected server as
 * a client.  One instance is bound to one configured provider
 * (see {@link OAuthClientConfig}); obtain it with
 * {@link #forProvider(String)}.
 * <br><br>
 * <h2>Typical use</h2>
 * <pre>
 * OAuthClient client = OAuthClient.forProvider("ownsona");
 * if (!client.isAuthorized()) {
 *     String url = client.beginAuthorization(baseUrl);   // send the browser here
 *     ...
 * }
 * String token = client.getAccessToken();                // refreshes if needed
 * headers.put("Authorization", "Bearer " + token);       // call the remote API
 * </pre>
 * <h2>What it handles</h2>
 * <ul>
 *   <li>Authorization-server discovery (cached) --- {@link OAuthMetadataDiscovery}.</li>
 *   <li>Client registration: a configured {@code ClientId} or Dynamic
 *       Client Registration (cached) --- {@link DynamicClientRegistration}.</li>
 *   <li>The authorization-code + PKCE redirect flow, completed by
 *       {@link OAuthCallbackServlet}.</li>
 *   <li>Access-token refresh, with single-flight per provider and
 *       refresh-token rotation.</li>
 * </ul>
 * When no usable token (and no refresh token) is available,
 * {@link #getAccessToken()} throws {@link OAuthAuthorizationRequiredException}
 * to signal that the interactive flow must run.
 */
public final class OAuthClient {

    private static final Logger logger = LogManager.getLogger(OAuthClient.class);

    private static final SecureRandom RANDOM = new SecureRandom();
    /** Per-provider locks so only one refresh runs at a time for a provider. */
    private static final Map<String, Object> REFRESH_LOCKS = new ConcurrentHashMap<>();

    private final String              name;
    private final OAuthClientProvider provider;

    private OAuthClient(String name, OAuthClientProvider provider) {
        this.name     = name;
        this.provider = provider;
    }

    /**
     * Get the client for a configured provider.
     *
     * @param name the provider label (the {@code <name>} in
     *             {@code [OAuthClient <name>]})
     * @return the client
     * @throws OAuthClientException if no such provider is configured
     */
    public static OAuthClient forProvider(String name) {
        final OAuthClientProvider p = OAuthClientConfig.get().getProvider(name);
        if (p == null)
            throw new OAuthClientException("No OAuth client provider named '" + name
                    + "' is configured (expected an [OAuthClient " + name + "] section in application.ini)");
        return new OAuthClient(name, p);
    }

    /**
     * Get the provider name this client is bound to.
     * @return the provider name
     */
    public String getProviderName() {
        return name;
    }

    /**
     * Whether a usable token is held --- either a still-valid access token
     * or a refresh token that can mint one.
     *
     * @return true if {@link #getAccessToken()} can likely succeed without
     *         interactive authorization
     */
    public boolean isAuthorized() {
        try {
            final TokenSet ts = OAuthClientStore.get().loadTokens(name);
            if (ts == null)
                return false;
            final int skew = OAuthClientConfig.get().getRefreshSkewSeconds();
            return !ts.isExpired(skew) || ts.hasRefreshToken();
        } catch (SQLException e) {
            logger.warn("Could not read tokens for provider '" + name + "'", e);
            return false;
        }
    }

    /**
     * Begin the authorization-code + PKCE flow.  Performs discovery and
     * (if necessary) client registration, records the PKCE verifier and
     * state, and returns the authorization-server URL the browser must be
     * sent to.  The flow completes at {@link OAuthCallbackServlet}.
     *
     * @param redirectBaseUrl the base URL for the redirect URI
     *                        ({@code <base>/oauth/client/callback}); if
     *                        null, falls back to
     *                        {@code OAuthClientRedirectBaseUrl}
     * @return the authorization URL to redirect the browser to
     * @throws OAuthClientException if discovery, registration, or
     *                              configuration fails
     */
    public String beginAuthorization(String redirectBaseUrl) {
        final String base = redirectBaseUrl != null && !redirectBaseUrl.isEmpty()
                ? trimTrailingSlash(redirectBaseUrl)
                : OAuthClientConfig.get().getRedirectBaseUrl();
        if (base == null || base.isEmpty())
            throw new OAuthClientException("No redirect base URL available for provider '" + name
                    + "': pass one to beginAuthorization() or set OAuthClientRedirectBaseUrl in application.ini");
        final String redirectUri = base + OAuthClientConfig.CALLBACK_PATH;

        final DiscoveryMetadata d = ensureDiscovery();
        final ClientRegistration reg = ensureRegistration(d, redirectUri);

        final Pkce pkce  = Pkce.generate();
        final String state = randomToken();
        PendingAuthorization.store(new PendingAuthorization(name, state, pkce.getCodeVerifier(), redirectUri));

        final StringBuilder url = new StringBuilder(d.getAuthorizationEndpoint());
        url.append(d.getAuthorizationEndpoint().contains("?") ? '&' : '?');
        appendParam(url, "response_type", "code");
        appendParam(url, "client_id", reg.getClientId());
        appendParam(url, "redirect_uri", redirectUri);
        if (!provider.getScopeString().isEmpty())
            appendParam(url, "scope", provider.getScopeString());
        appendParam(url, "state", state);
        appendParam(url, "code_challenge", pkce.getCodeChallenge());
        appendParam(url, "code_challenge_method", pkce.getMethod());
        // RFC 8707: bind the issued token's audience to the target resource.
        appendParam(url, "resource", provider.getUrl());

        logger.info("Beginning OAuth authorization for provider '" + name + "'");
        return url.toString();
    }

    /**
     * Complete the flow: exchange the authorization code for tokens and
     * persist them.  Called by {@link OAuthCallbackServlet} after it has
     * validated the returned {@code state}.
     *
     * @param pending the pending authorization matched by state
     * @param code    the authorization code returned by the server
     * @throws OAuthClientException                  if the exchange fails
     * @throws OAuthAuthorizationRequiredException   if the code was rejected
     */
    public void completeAuthorization(PendingAuthorization pending, String code) {
        final OAuthClientStore store = OAuthClientStore.get();
        final DiscoveryMetadata d;
        final ClientRegistration reg;
        try {
            d   = store.loadDiscovery(name);
            reg = store.loadRegistration(name);
        } catch (SQLException e) {
            throw new OAuthClientException("Could not read OAuth client state for provider '" + name + "'", e);
        }
        if (d == null || reg == null)
            throw new OAuthClientException("Authorization state was lost for provider '" + name
                    + "'; please restart the login");

        final StringBuilder form = new StringBuilder();
        appendParam(form, "grant_type", "authorization_code");
        appendParam(form, "code", code);
        appendParam(form, "redirect_uri", pending.getRedirectUri());
        appendParam(form, "client_id", reg.getClientId());
        appendParam(form, "code_verifier", pending.getCodeVerifier());
        appendParam(form, "resource", provider.getUrl());

        final TokenSet ts = postToken(d.getTokenEndpoint(), form.toString(), reg, null);
        try {
            store.saveTokens(name, ts);
        } catch (SQLException e) {
            throw new OAuthClientException("Could not persist tokens for provider '" + name + "'", e);
        }
        logger.info("Authorization complete for provider '" + name + "'");
    }

    /**
     * Get a valid access token, refreshing transparently if the current
     * one is expired.  This is the method outbound callers use right
     * before making an authenticated request.
     *
     * @return a currently-valid access token
     * @throws OAuthAuthorizationRequiredException if no token is held and
     *         none can be obtained without interactive authorization
     * @throws OAuthClientException                on an unexpected failure
     */
    public String getAccessToken() {
        final TokenSet ts;
        try {
            ts = OAuthClientStore.get().loadTokens(name);
        } catch (SQLException e) {
            throw new OAuthClientException("Could not read tokens for provider '" + name + "'", e);
        }
        if (ts == null)
            throw new OAuthAuthorizationRequiredException(name);

        final int skew = OAuthClientConfig.get().getRefreshSkewSeconds();
        if (!ts.isExpired(skew))
            return ts.getAccessToken();
        if (!ts.hasRefreshToken())
            throw new OAuthAuthorizationRequiredException(name);
        return refresh(skew).getAccessToken();
    }

    /**
     * Discard any stored tokens for this provider (logout).  Discovery and
     * client registration are retained so a later login skips re-discovery.
     */
    public void clearTokens() {
        try {
            OAuthClientStore.get().deleteTokens(name);
        } catch (SQLException e) {
            logger.warn("Could not delete tokens for provider '" + name + "'", e);
        }
    }

    // ==================================================================
    // Internals
    // ==================================================================

    private DiscoveryMetadata ensureDiscovery() {
        final OAuthClientStore store = OAuthClientStore.get();
        try {
            final DiscoveryMetadata cached = store.loadDiscovery(name);
            if (cached != null)
                return cached;
            final DiscoveryMetadata d = OAuthMetadataDiscovery.discover(provider);
            store.saveDiscovery(name, d);
            return d;
        } catch (SQLException e) {
            throw new OAuthClientException("Could not cache discovery for provider '" + name + "'", e);
        }
    }

    private ClientRegistration ensureRegistration(DiscoveryMetadata d, String redirectUri) {
        final OAuthClientStore store = OAuthClientStore.get();
        try {
            if (provider.hasConfiguredClient()) {
                final ClientRegistration reg = new ClientRegistration(
                        provider.getClientId(), provider.getClientSecret(), redirectUri);
                store.saveRegistration(name, reg);
                return reg;
            }
            final ClientRegistration existing = store.loadRegistration(name);
            if (existing != null && redirectUri.equals(existing.getRedirectUri()))
                return existing;
            final ClientRegistration reg = DynamicClientRegistration.register(provider, d, redirectUri);
            store.saveRegistration(name, reg);
            return reg;
        } catch (SQLException e) {
            throw new OAuthClientException("Could not cache client registration for provider '" + name + "'", e);
        }
    }

    private TokenSet refresh(int skew) {
        final Object lock = REFRESH_LOCKS.computeIfAbsent(name, k -> new Object());
        synchronized (lock) {
            final OAuthClientStore store = OAuthClientStore.get();
            final TokenSet current;
            final DiscoveryMetadata d;
            final ClientRegistration reg;
            try {
                current = store.loadTokens(name);
                // Another thread may have refreshed while we waited for the lock.
                if (current != null && !current.isExpired(skew))
                    return current;
                d   = store.loadDiscovery(name);
                reg = store.loadRegistration(name);
            } catch (SQLException e) {
                throw new OAuthClientException("Could not read OAuth client state for provider '" + name + "'", e);
            }
            if (current == null || !current.hasRefreshToken())
                throw new OAuthAuthorizationRequiredException(name);
            if (d == null || reg == null)
                throw new OAuthAuthorizationRequiredException(name);

            final StringBuilder form = new StringBuilder();
            appendParam(form, "grant_type", "refresh_token");
            appendParam(form, "refresh_token", current.getRefreshToken());
            appendParam(form, "client_id", reg.getClientId());
            appendParam(form, "resource", provider.getUrl());

            final TokenSet refreshed;
            try {
                // Keep the existing refresh token if the server does not rotate it.
                refreshed = postToken(d.getTokenEndpoint(), form.toString(), reg, current.getRefreshToken());
            } catch (OAuthAuthorizationRequiredException e) {
                // Refresh token was rejected --- drop it so state reflects reality.
                clearTokens();
                throw e;
            }
            try {
                store.saveTokens(name, refreshed);
            } catch (SQLException e) {
                throw new OAuthClientException("Could not persist refreshed tokens for provider '" + name + "'", e);
            }
            logger.info("Refreshed access token for provider '" + name + "'");
            return refreshed;
        }
    }

    /**
     * POST a form body to the token endpoint and parse the token response.
     * On an {@code invalid_grant} error, throws
     * {@link OAuthAuthorizationRequiredException}; on any other non-2xx,
     * throws {@link OAuthClientException}.
     */
    private TokenSet postToken(String tokenEndpoint, String formBody,
                              ClientRegistration reg, String fallbackRefresh) {
        final RestClient rc = new RestClient().setTimeouts(Duration.ofSeconds(15), Duration.ofSeconds(15));
        final JSONObject headers = reg.isConfidential()
                ? RestClient.basicAuthenticationHeader(reg.getClientId(), reg.getClientSecret())
                : new JSONObject();
        headers.put("Content-Type", "application/x-www-form-urlencoded");
        headers.put("Accept", "application/json");

        final int status;
        try {
            status = rc.performService("POST", tokenEndpoint, formBody, headers);
        } catch (IOException e) {
            throw new OAuthClientException("Token request to " + tokenEndpoint
                    + " failed for provider '" + name + "'", e);
        }
        final String responseBody = rc.getResponseString();

        if (status < 200 || status >= 300) {
            String errorCode = null;
            try {
                errorCode = new JSONObject(responseBody == null ? "{}" : responseBody).getString("error", null);
            } catch (RuntimeException ignored) {
            }
            if ("invalid_grant".equals(errorCode))
                throw new OAuthAuthorizationRequiredException(name);
            throw new OAuthClientException("Token endpoint " + tokenEndpoint + " returned HTTP "
                    + status + " for provider '" + name + "': " + responseBody);
        }

        final JSONObject j;
        try {
            j = new JSONObject(responseBody);
        } catch (RuntimeException e) {
            throw new OAuthClientException("Token endpoint " + tokenEndpoint
                    + " returned an unparseable body for provider '" + name + "': " + responseBody, e);
        }
        final String accessToken = j.getString("access_token", null);
        if (accessToken == null || accessToken.isEmpty())
            throw new OAuthClientException("Token response for provider '" + name
                    + "' had no access_token: " + responseBody);
        final String refreshToken = j.getString("refresh_token", fallbackRefresh);
        final String tokenType    = j.getString("token_type", "Bearer");
        final String scope        = j.getString("scope", null);
        final Long expiresIn      = j.getLong("expires_in", null);
        final long expiresAt = (expiresIn == null || expiresIn <= 0)
                ? 0L
                : System.currentTimeMillis() / 1000L + expiresIn;
        return new TokenSet(accessToken, refreshToken, tokenType, scope, expiresAt);
    }

    private static void appendParam(StringBuilder sb, String key, String value) {
        if (sb.length() > 0) {
            final char last = sb.charAt(sb.length() - 1);
            if (last != '?' && last != '&')
                sb.append('&');
        }
        sb.append(key).append('=').append(URLEncoder.encode(value, StandardCharsets.UTF_8));
    }

    private static String randomToken() {
        final byte[] bytes = new byte[32];
        RANDOM.nextBytes(bytes);
        return Base64.getUrlEncoder().withoutPadding().encodeToString(bytes);
    }

    private static String trimTrailingSlash(String s) {
        while (s.endsWith("/"))
            s = s.substring(0, s.length() - 1);
        return s;
    }
}
