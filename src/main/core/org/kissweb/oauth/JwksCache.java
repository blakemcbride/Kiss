package org.kissweb.oauth;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.RestClient;
import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;

import java.io.IOException;
import java.math.BigInteger;
import java.security.KeyFactory;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.spec.InvalidKeySpecException;
import java.security.spec.RSAPublicKeySpec;
import java.util.Base64;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

/**
 * Fetches and caches the authorization server's JSON Web Key Set (JWKS).
 * <br><br>
 * The cache is keyed by JWK {@code kid} and refreshed when (a) the TTL
 * configured by {@code OAuthJwksCacheSeconds} has elapsed, or (b) a token
 * arrives with an unknown {@code kid} (which indicates the authorization
 * server has rotated its signing keys).
 * <br><br>
 * The JWKS URI is resolved in this order on first access:
 * <ol>
 *   <li>An explicit {@code OAuthJwksUri} in {@code application.ini}.</li>
 *   <li>The {@code jwks_uri} field of the RFC 8414 OAuth 2.0
 *       authorization-server metadata document served at
 *       {@code <issuer>/.well-known/oauth-authorization-server}.
 *       This is the OAuth 2.1 / MCP standard.</li>
 *   <li>The {@code jwks_uri} field of the OpenID Connect discovery
 *       document at {@code <issuer>/.well-known/openid-configuration}.
 *       This is the older OIDC path, kept as a fallback for
 *       OIDC-only authorization servers.</li>
 * </ol>
 * In practice this means apps that pair Kiss's RS with Kiss's
 * own AS (which publishes RFC 8414 but not OIDC discovery) need
 * no {@code OAuthJwksUri} setting at all; the RS finds the JWKS
 * via the same metadata document MCP clients use for discovery.
 */
public final class JwksCache {

    private static final Logger logger = LogManager.getLogger(JwksCache.class);

    private static volatile JwksCache instance;

    private final OAuthConfig config;
    private final ReentrantLock refreshLock = new ReentrantLock();

    private volatile Map<String, PublicKey> keysByKid = new HashMap<>();
    private volatile long lastRefreshedEpochSeconds;
    private volatile String resolvedJwksUri;

    private JwksCache(OAuthConfig config) {
        this.config = config;
    }

    /**
     * Get the process-wide JWKS cache, creating it on first call.
     *
     * @return the cache instance
     */
    public static JwksCache get() {
        JwksCache local = instance;
        if (local == null) {
            synchronized (JwksCache.class) {
                local = instance;
                if (local == null) {
                    local = new JwksCache(OAuthConfig.get());
                    instance = local;
                }
            }
        }
        return local;
    }

    /**
     * Reset the singleton.  Intended for tests.
     */
    public static synchronized void reset() {
        instance = null;
    }

    /**
     * Look up a signing key by {@code kid}.  If the cache is stale or
     * the kid is unknown, the JWKS is re-fetched.
     *
     * @param kid the key id from the JWT header
     * @return the public key, or null if the authorization server does
     *         not advertise a key with that id
     * @throws IOException if the JWKS fetch fails
     */
    public PublicKey getKey(String kid) throws IOException {
        if (kid == null || kid.isEmpty())
            return null;

        PublicKey key = keysByKid.get(kid);
        if (key != null && !isStale())
            return key;

        // Either the key is unknown or the cache is stale --- refresh
        // under the lock so only one thread hits the JWKS endpoint.
        refreshLock.lock();
        try {
            // Re-check after acquiring the lock: another thread may have
            // just refreshed.
            key = keysByKid.get(kid);
            if (key != null && !isStale())
                return key;
            refresh();
            return keysByKid.get(kid);
        } finally {
            refreshLock.unlock();
        }
    }

    /**
     * Force a JWKS fetch immediately.  Useful for tests and for warming
     * the cache at startup.
     *
     * @throws IOException if the JWKS fetch fails
     */
    public void refresh() throws IOException {
        final String uri = resolveJwksUri();
        logger.info("Fetching JWKS from " + uri);

        final RestClient client = new RestClient();
        final JSONObject jwks = client.jsonCall("GET", uri);
        if (jwks == null)
            throw new IOException("JWKS endpoint returned empty body: " + uri);

        final Map<String, PublicKey> next = new HashMap<>();
        if (jwks.has("keys")) {
            final JSONArray keys = jwks.getJSONArray("keys");
            for (int i = 0; i < keys.length(); i++) {
                final JSONObject jwk = keys.getJSONObject(i);
                final String kid = jwk.getString("kid", null);
                if (kid == null || kid.isEmpty())
                    continue;
                final PublicKey pk = parseJwk(jwk);
                if (pk != null)
                    next.put(kid, pk);
            }
        }
        if (next.isEmpty())
            logger.warn("JWKS at " + uri + " contained no usable keys");

        this.keysByKid = next;
        this.lastRefreshedEpochSeconds = nowEpochSeconds();
    }

    // ------------------------------------------------------------------

    private boolean isStale() {
        final long age = nowEpochSeconds() - lastRefreshedEpochSeconds;
        return age >= config.getJwksCacheSeconds();
    }

    private String resolveJwksUri() throws IOException {
        if (resolvedJwksUri != null)
            return resolvedJwksUri;

        // 1. Explicit override wins.
        final String explicit = config.getJwksUri();
        if (explicit != null && !explicit.isEmpty()) {
            resolvedJwksUri = explicit;
            return resolvedJwksUri;
        }

        final String issuer = config.getAuthorizationServer();

        // 2. RFC 8414 OAuth authorization-server metadata.  This is
        //    the OAuth 2.1 / MCP standard discovery doc, and the one
        //    Kiss's own AS publishes (it does not publish OIDC's
        //    openid-configuration).  Try this first.
        final String rfc8414 = issuer + "/.well-known/oauth-authorization-server";
        final String fromRfc8414 = tryMetadataDoc(rfc8414);
        if (fromRfc8414 != null) {
            resolvedJwksUri = fromRfc8414;
            return resolvedJwksUri;
        }

        // 3. OpenID Connect discovery document.  Kept as a fallback
        //    for OIDC-only authorization servers.
        final String oidc = issuer + "/.well-known/openid-configuration";
        final String fromOidc = tryMetadataDoc(oidc);
        if (fromOidc != null) {
            resolvedJwksUri = fromOidc;
            return resolvedJwksUri;
        }

        // Both metadata documents were reachable but neither published a
        // usable jwks_uri (each returned 404/410, or 2xx without the
        // field).  Hard failures (network, auth, 5xx) would have been
        // propagated from tryMetadataDoc with a specific diagnostic.
        throw new IOException("Could not resolve JWKS URI for issuer " + issuer
                + ".  Neither " + rfc8414 + " nor " + oidc
                + " published a jwks_uri.  Set OAuthJwksUri in application.ini "
                + "if your authorization server publishes neither metadata document.");
    }

    /**
     * GET the given metadata URL and return its {@code jwks_uri} field,
     * or {@code null} if the endpoint legitimately does not publish a
     * metadata document at this URL.
     * <br><br>
     * "Legitimately not published" covers three soft-failure cases ---
     * HTTP 404/410, a 2xx response that is not valid JSON, and a 2xx
     * JSON document that simply lacks a {@code jwks_uri} field --- in
     * each of which the caller can sensibly fall through to the next
     * discovery option.
     * <br><br>
     * Hard failures --- network errors, timeouts, auth-required
     * responses, 5xx server errors --- are propagated as
     * {@link IOException} so the caller can surface a meaningful
     * diagnostic rather than the misleading "tried everything, nothing
     * worked" message that would result from silently falling through
     * (OIDC discovery would fail the same way against the same host).
     */
    private static String tryMetadataDoc(String url) throws IOException {
        logger.info("Looking for jwks_uri in " + url);
        final RestClient client = new RestClient();
        final JSONObject meta;
        try {
            meta = client.jsonCall("GET", url);
        } catch (IOException e) {
            throw new IOException("Could not fetch OAuth metadata from " + url
                    + ": " + e.getMessage(), e);
        }
        final int status = client.getResponseCode();
        final boolean ok = status >= 200 && status < 300;
        final boolean notPublished = status == 404 || status == 410;
        if (!ok && !notPublished)
            throw new IOException("OAuth metadata fetch from " + url
                    + " returned HTTP " + status);
        if (notPublished) {
            logger.debug("Metadata not published at " + url + " (HTTP " + status + ")");
            return null;
        }
        if (meta == null) {
            logger.debug("Metadata at " + url + " is not valid JSON --- treating as not published");
            return null;
        }
        final String uri = meta.getString("jwks_uri", null);
        if (uri == null || uri.isEmpty()) {
            logger.debug("Metadata at " + url + " has no jwks_uri field --- treating as not published");
            return null;
        }
        return uri;
    }

    /**
     * Turn a single JWK entry into a {@link PublicKey}.  Currently
     * supports RSA keys (kty=RSA).  Returns null for unsupported key
     * types --- we just skip them so a JWKS with mixed key types still
     * yields the keys we can use.
     */
    private static PublicKey parseJwk(JSONObject jwk) {
        final String kty = jwk.getString("kty", null);
        if (!"RSA".equals(kty))
            return null;

        final String n = jwk.getString("n", null);
        final String e = jwk.getString("e", null);
        if (n == null || e == null)
            return null;

        try {
            final BigInteger modulus  = new BigInteger(1, Base64.getUrlDecoder().decode(n));
            final BigInteger exponent = new BigInteger(1, Base64.getUrlDecoder().decode(e));
            final RSAPublicKeySpec spec = new RSAPublicKeySpec(modulus, exponent);
            return KeyFactory.getInstance("RSA").generatePublic(spec);
        } catch (NoSuchAlgorithmException | InvalidKeySpecException | IllegalArgumentException ex) {
            logger.warn("Skipping unparseable JWK kid=" + jwk.getString("kid", "(none)") + ": " + ex.getMessage());
            return null;
        }
    }

    private static long nowEpochSeconds() {
        return System.currentTimeMillis() / 1000L;
    }
}
