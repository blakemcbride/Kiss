package org.kissweb.oauth;

import org.kissweb.restServer.MainServlet;

import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Reads the OAuth 2.1 resource-server settings from {@code application.ini}
 * via {@link MainServlet#getEnvironment(String)}.
 * <br><br>
 * Values are loaded lazily on first access and cached for the lifetime of
 * the JVM.  If the authorization-server URL is not configured, the package
 * is considered disabled --- {@link #isEnabled()} returns false and the
 * discovery endpoint short-circuits to 404.
 * <br><br>
 * Recognized keys:
 * <ul>
 *   <li>{@code OAuthAuthorizationServer} --- issuer URL of the authorization server (required to enable)</li>
 *   <li>{@code OAuthResourceIdentifier} --- canonical URL of this resource (the {@code aud} value tokens must carry; defaults to the authorization-server URL if absent, but real deployments should set this)</li>
 *   <li>{@code OAuthRequiredScopes} --- comma- or space-separated list of scopes every token must carry (optional)</li>
 *   <li>{@code OAuthJwksUri} --- explicit JWKS URL; if absent, discovered from {@code <issuer>/.well-known/openid-configuration}</li>
 *   <li>{@code OAuthJwksCacheSeconds} --- TTL for cached keys, default 3600</li>
 *   <li>{@code OAuthAllowedAlgorithms} --- comma-separated list of acceptable JWS algorithms, default {@code RS256}</li>
 *   <li>{@code OAuthClockSkewSeconds} --- allowed clock skew when validating {@code exp}/{@code nbf}, default 60</li>
 * </ul>
 */
public final class OAuthConfig {

    private static volatile OAuthConfig instance;

    private final boolean enabled;
    private final String authorizationServer;
    private final String resourceIdentifier;
    private final List<String> requiredScopes;
    private final String jwksUri;
    private final int jwksCacheSeconds;
    private final Set<String> allowedAlgorithms;
    private final int clockSkewSeconds;

    private OAuthConfig() {
        this.authorizationServer  = trimTrailingSlash(getString("OAuthAuthorizationServer"));
        this.enabled              = authorizationServer != null && !authorizationServer.isEmpty();

        final String resource     = getString("OAuthResourceIdentifier");
        this.resourceIdentifier   = resource != null && !resource.isEmpty() ? resource : authorizationServer;

        this.requiredScopes       = splitList(getString("OAuthRequiredScopes"));
        this.jwksUri              = getString("OAuthJwksUri");
        this.jwksCacheSeconds     = getInt("OAuthJwksCacheSeconds", 3600);

        final List<String> algs   = splitList(getString("OAuthAllowedAlgorithms"));
        this.allowedAlgorithms    = algs.isEmpty()
                ? Collections.singleton("RS256")
                : Collections.unmodifiableSet(new LinkedHashSet<>(algs));

        this.clockSkewSeconds     = getInt("OAuthClockSkewSeconds", 60);
    }

    /**
     * Get the singleton configuration, loading from {@code application.ini}
     * on first call.
     *
     * @return the OAuth configuration
     */
    public static OAuthConfig get() {
        OAuthConfig local = instance;
        if (local == null) {
            synchronized (OAuthConfig.class) {
                local = instance;
                if (local == null) {
                    local = new OAuthConfig();
                    instance = local;
                }
            }
        }
        return local;
    }

    /**
     * Reset the cached configuration so the next call to {@link #get()}
     * re-reads {@code application.ini}.  Intended for tests; production
     * code should never need this.
     */
    public static synchronized void reset() {
        instance = null;
    }

    /**
     * Check whether OAuth resource-server support is configured.
     *
     * @return true if the authorization-server URL is configured
     */
    public boolean isEnabled() {
        return enabled;
    }

    /**
     * Get the authorization-server (issuer) URL.
     *
     * @return the issuer URL, or null if not configured
     */
    public String getAuthorizationServer() {
        return authorizationServer;
    }

    /**
     * Get the canonical URL of this resource server, used as the expected
     * {@code aud} value when validating tokens.
     *
     * @return the resource identifier
     */
    public String getResourceIdentifier() {
        return resourceIdentifier;
    }

    /**
     * Get the scopes every token must carry.
     *
     * @return the required scopes; empty list means no scope requirement
     */
    public List<String> getRequiredScopes() {
        return requiredScopes;
    }

    /**
     * Get the explicit JWKS URI if configured.
     *
     * @return the JWKS URI, or null if it should be discovered
     */
    public String getJwksUri() {
        return jwksUri;
    }

    /**
     * Get the JWKS cache TTL.
     *
     * @return how long parsed JWKS keys may be cached, in seconds
     */
    public int getJwksCacheSeconds() {
        return jwksCacheSeconds;
    }

    /**
     * Get the acceptable JWS {@code alg} header values.
     *
     * @return the set of allowed algorithms
     */
    public Set<String> getAllowedAlgorithms() {
        return allowedAlgorithms;
    }

    /**
     * Get the allowed clock skew when validating {@code exp} and {@code nbf}.
     *
     * @return the clock skew in seconds
     */
    public int getClockSkewSeconds() {
        return clockSkewSeconds;
    }

    // ------------------------------------------------------------------

    private static String getString(String key) {
        Object v = MainServlet.getEnvironment(key);
        if (v == null)
            return null;
        String s = v.toString().trim();
        return s.isEmpty() ? null : s;
    }

    private static int getInt(String key, int defaultValue) {
        String s = getString(key);
        if (s == null)
            return defaultValue;
        try {
            return Integer.parseInt(s);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    private static List<String> splitList(String raw) {
        if (raw == null || raw.isEmpty())
            return Collections.emptyList();
        final String[] parts = raw.split("[\\s,]+");
        final List<String> out = new ArrayList<>(parts.length);
        for (String p : parts) {
            String t = p.trim();
            if (!t.isEmpty())
                out.add(t);
        }
        return Collections.unmodifiableList(out);
    }

    private static String trimTrailingSlash(String s) {
        if (s == null)
            return null;
        while (s.endsWith("/"))
            s = s.substring(0, s.length() - 1);
        return s;
    }
}
