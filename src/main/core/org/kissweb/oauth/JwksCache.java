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
 * The JWKS URI is either taken from {@code OAuthJwksUri} or discovered
 * from {@code <authorization-server>/.well-known/openid-configuration} on
 * first access.
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

        // Explicit override wins.
        final String explicit = config.getJwksUri();
        if (explicit != null && !explicit.isEmpty()) {
            resolvedJwksUri = explicit;
            return resolvedJwksUri;
        }

        // Discover via the OpenID Connect discovery document.  This is
        // also what RFC 8414 (OAuth 2.0 Authorization Server Metadata)
        // recommends --- the discovery URL and field names align.
        final String discovery = config.getAuthorizationServer() + "/.well-known/openid-configuration";
        logger.info("Discovering JWKS URI from " + discovery);
        final RestClient client = new RestClient();
        final JSONObject meta = client.jsonCall("GET", discovery);
        if (meta == null)
            throw new IOException("Discovery endpoint returned empty body: " + discovery);
        final String uri = meta.getString("jwks_uri", null);
        if (uri == null || uri.isEmpty())
            throw new IOException("Discovery document does not contain jwks_uri: " + discovery);
        resolvedJwksUri = uri;
        return resolvedJwksUri;
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
