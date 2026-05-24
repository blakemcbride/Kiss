package org.kissweb.oauth;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;
import org.kissweb.restServer.MainServlet;

import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.PublicKey;
import java.security.interfaces.RSAPublicKey;
import java.util.Base64;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Validates the multi-step JWKS-URI resolution waterfall in
 * {@link JwksCache}: explicit {@code OAuthJwksUri} --&gt; RFC 8414
 * ({@code /.well-known/oauth-authorization-server}) --&gt; OIDC
 * ({@code /.well-known/openid-configuration}).
 * <br><br>
 * Each test reconfigures the mock authorization server's two metadata
 * endpoints --- to publish, to return 404 ("not published"), or to fail
 * with a hard error --- and verifies that {@link JwksCache} either
 * resolves the JWKS or surfaces a meaningful diagnostic.
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class JwksDiscoveryTest {

    private static final String KID = "discovery-test-key";

    /** Per-test behavior of a metadata endpoint. */
    enum Mode {
        NOT_PUBLISHED,    // 404
        PUBLISH,          // 200 with valid {jwks_uri}
        STATUS_503,       // hard failure: transient server error
        STATUS_401,       // hard failure: auth required
        MALFORMED_JSON,   // 200 but body is not JSON --- treated as soft failure
        NO_JWKS_FIELD     // 200 with valid JSON but no jwks_uri --- soft failure
    }

    private static HttpServer server;
    private static String     issuer;
    private static KeyPair    keyPair;
    private static String     jwksBody;

    private static final AtomicReference<Mode> rfc8414Mode = new AtomicReference<>(Mode.NOT_PUBLISHED);
    private static final AtomicReference<Mode> oidcMode    = new AtomicReference<>(Mode.NOT_PUBLISHED);

    private static final AtomicInteger rfc8414Hits = new AtomicInteger();
    private static final AtomicInteger oidcHits    = new AtomicInteger();
    private static final AtomicInteger jwksHits    = new AtomicInteger();

    // ------------------------------------------------------------------
    // Fixture setup / teardown
    // ------------------------------------------------------------------

    @BeforeAll
    static void startFixture() throws Exception {
        final KeyPairGenerator gen = KeyPairGenerator.getInstance("RSA");
        gen.initialize(2048);
        keyPair  = gen.generateKeyPair();
        jwksBody = buildJwksJson((RSAPublicKey) keyPair.getPublic(), KID);

        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        issuer = "http://127.0.0.1:" + server.getAddress().getPort();

        server.createContext("/.well-known/oauth-authorization-server",
                ex -> handleMetadata(ex, rfc8414Mode.get(), rfc8414Hits));
        server.createContext("/.well-known/openid-configuration",
                ex -> handleMetadata(ex, oidcMode.get(), oidcHits));
        server.createContext("/jwks", ex -> {
            jwksHits.incrementAndGet();
            respondJson(ex, 200, jwksBody);
        });
        server.start();
    }

    @AfterAll
    static void stopFixture() {
        if (server != null)
            server.stop(0);
        // Don't leak our env settings into any subsequent test class
        // running in the same JVM.
        for (String key : new String[]{"OAuthAuthorizationServer",
                "OAuthResourceIdentifier", "OAuthJwksUri",
                "OAuthRequiredScopes", "OAuthJwksCacheSeconds",
                "OAuthAllowedAlgorithms", "OAuthClockSkewSeconds"})
            MainServlet.putEnvironment(key, "");
        OAuthConfig.reset();
        JwksCache.reset();
    }

    @BeforeEach
    void resetState() {
        rfc8414Mode.set(Mode.NOT_PUBLISHED);
        oidcMode.set(Mode.NOT_PUBLISHED);
        rfc8414Hits.set(0);
        oidcHits.set(0);
        jwksHits.set(0);

        for (String key : new String[]{"OAuthJwksUri", "OAuthRequiredScopes"})
            MainServlet.putEnvironment(key, "");
        MainServlet.putEnvironment("OAuthAuthorizationServer", issuer);
        MainServlet.putEnvironment("OAuthResourceIdentifier",  issuer);
        MainServlet.putEnvironment("OAuthJwksCacheSeconds",    "3600");
        MainServlet.putEnvironment("OAuthAllowedAlgorithms",   "RS256");
        MainServlet.putEnvironment("OAuthClockSkewSeconds",    "60");
        OAuthConfig.reset();
        JwksCache.reset();
    }

    // ==================================================================
    // 1. RFC 8414 primary path
    // ==================================================================

    @Test
    void rfc8414_primaryPath_findsJwks() throws Exception {
        rfc8414Mode.set(Mode.PUBLISH);
        oidcMode.set(Mode.PUBLISH);  // also published, but RFC 8414 must win

        final PublicKey key = JwksCache.get().getKey(KID);
        assertNotNull(key);
        assertEquals(1, rfc8414Hits.get(), "RFC 8414 must be tried first");
        assertEquals(0, oidcHits.get(),    "OIDC must not be tried once RFC 8414 succeeds");
        assertEquals(1, jwksHits.get());
    }

    // ==================================================================
    // 2. Soft-failure fall-through to OIDC
    // ==================================================================

    @Test
    void rfc8414_returns404_fallsBackToOidc() throws Exception {
        rfc8414Mode.set(Mode.NOT_PUBLISHED);
        oidcMode.set(Mode.PUBLISH);

        final PublicKey key = JwksCache.get().getKey(KID);
        assertNotNull(key);
        assertEquals(1, rfc8414Hits.get(), "RFC 8414 must be attempted first");
        assertEquals(1, oidcHits.get(),    "OIDC must be used as fallback on 404");
        assertEquals(1, jwksHits.get());
    }

    @Test
    void rfc8414_malformedJson_fallsBackToOidc() throws Exception {
        rfc8414Mode.set(Mode.MALFORMED_JSON);
        oidcMode.set(Mode.PUBLISH);

        final PublicKey key = JwksCache.get().getKey(KID);
        assertNotNull(key);
        assertEquals(1, oidcHits.get(),
                "A 2xx response with non-JSON body must be treated as 'not published' and fall through");
    }

    @Test
    void rfc8414_noJwksField_fallsBackToOidc() throws Exception {
        rfc8414Mode.set(Mode.NO_JWKS_FIELD);
        oidcMode.set(Mode.PUBLISH);

        final PublicKey key = JwksCache.get().getKey(KID);
        assertNotNull(key);
        assertEquals(1, oidcHits.get(),
                "A 2xx response missing jwks_uri must be treated as 'not published' and fall through");
    }

    // ==================================================================
    // 3. Hard-failure propagation
    // ==================================================================

    @Test
    void rfc8414_returns503_propagatesAndDoesNotFallThrough() {
        rfc8414Mode.set(Mode.STATUS_503);
        oidcMode.set(Mode.PUBLISH);  // would succeed --- but we must not get there

        final IOException ex = assertThrows(IOException.class,
                () -> JwksCache.get().getKey(KID));
        assertTrue(ex.getMessage().contains("503"),
                "Diagnostic must mention the HTTP status; got: " + ex.getMessage());
        assertEquals(0, oidcHits.get(),
                "Hard 5xx failure must not be silently masked by an OIDC fallback");
    }

    @Test
    void rfc8414_returns401_propagatesAndDoesNotFallThrough() {
        rfc8414Mode.set(Mode.STATUS_401);
        oidcMode.set(Mode.PUBLISH);

        final IOException ex = assertThrows(IOException.class,
                () -> JwksCache.get().getKey(KID));
        assertTrue(ex.getMessage().contains("401"),
                "Diagnostic must mention the HTTP status; got: " + ex.getMessage());
        assertEquals(0, oidcHits.get(),
                "Hard auth failure must not be silently masked by an OIDC fallback");
    }

    // ==================================================================
    // 4. Both docs missing --- meaningful final diagnostic
    // ==================================================================

    @Test
    void neitherPublished_finalErrorListsBothTriedUrls() {
        rfc8414Mode.set(Mode.NOT_PUBLISHED);
        oidcMode.set(Mode.NOT_PUBLISHED);

        final IOException ex = assertThrows(IOException.class,
                () -> JwksCache.get().getKey(KID));
        final String msg = ex.getMessage();
        assertTrue(msg.contains("oauth-authorization-server"),
                "Diagnostic must mention RFC 8414 path; got: " + msg);
        assertTrue(msg.contains("openid-configuration"),
                "Diagnostic must mention OIDC path; got: " + msg);
        assertTrue(msg.contains("OAuthJwksUri"),
                "Diagnostic must point operator at OAuthJwksUri override; got: " + msg);
    }

    // ==================================================================
    // 5. Explicit OAuthJwksUri bypasses discovery entirely
    // ==================================================================

    @Test
    void explicitOAuthJwksUri_bypassesBothDiscoveryPaths() throws Exception {
        MainServlet.putEnvironment("OAuthJwksUri", issuer + "/jwks");
        OAuthConfig.reset();
        JwksCache.reset();
        // Both metadata endpoints would 404 --- but we should never touch them.
        rfc8414Mode.set(Mode.NOT_PUBLISHED);
        oidcMode.set(Mode.NOT_PUBLISHED);

        final PublicKey key = JwksCache.get().getKey(KID);
        assertNotNull(key);
        assertEquals(0, rfc8414Hits.get(), "Explicit OAuthJwksUri must skip RFC 8414 discovery");
        assertEquals(0, oidcHits.get(),    "Explicit OAuthJwksUri must skip OIDC discovery");
        assertEquals(1, jwksHits.get());
    }

    // ==================================================================
    // Mock-server handlers
    // ==================================================================

    private static void handleMetadata(HttpExchange ex, Mode mode, AtomicInteger hits) throws IOException {
        hits.incrementAndGet();
        switch (mode) {
            case NOT_PUBLISHED:
                respondText(ex, 404, "Not Found");
                return;
            case PUBLISH: {
                final JSONObject meta = new JSONObject();
                meta.put("issuer",   issuer);
                meta.put("jwks_uri", issuer + "/jwks");
                respondJson(ex, 200, meta.toString());
                return;
            }
            case STATUS_503:
                respondText(ex, 503, "Service Unavailable");
                return;
            case STATUS_401:
                respondText(ex, 401, "Unauthorized");
                return;
            case MALFORMED_JSON:
                respondText(ex, 200, "this is not json {{{");
                return;
            case NO_JWKS_FIELD: {
                final JSONObject meta = new JSONObject();
                meta.put("issuer", issuer);
                respondJson(ex, 200, meta.toString());
                return;
            }
            default:
                respondText(ex, 500, "unhandled mode in test");
        }
    }

    // ==================================================================
    // Helpers (mirrors of BearerTokenValidatorTest's helpers)
    // ==================================================================

    private static String buildJwksJson(RSAPublicKey pub, String kid) {
        final JSONObject jwk = new JSONObject();
        jwk.put("kty", "RSA");
        jwk.put("use", "sig");
        jwk.put("alg", "RS256");
        jwk.put("kid", kid);
        jwk.put("n",   Base64.getUrlEncoder().withoutPadding()
                .encodeToString(toUnsignedBytes(pub.getModulus())));
        jwk.put("e",   Base64.getUrlEncoder().withoutPadding()
                .encodeToString(toUnsignedBytes(pub.getPublicExponent())));
        final JSONArray keys = new JSONArray();
        keys.put(jwk);
        final JSONObject doc = new JSONObject();
        doc.put("keys", keys);
        return doc.toString();
    }

    /** BigInteger.toByteArray() prepends a sign byte; JWK wants the unsigned magnitude. */
    private static byte[] toUnsignedBytes(java.math.BigInteger v) {
        final byte[] raw = v.toByteArray();
        if (raw[0] == 0) {
            final byte[] out = new byte[raw.length - 1];
            System.arraycopy(raw, 1, out, 0, out.length);
            return out;
        }
        return raw;
    }

    private static void respondJson(HttpExchange exchange, int status, String body) throws IOException {
        final byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().add("Content-Type", "application/json");
        exchange.sendResponseHeaders(status, bytes.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
        }
    }

    private static void respondText(HttpExchange exchange, int status, String body) throws IOException {
        final byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().add("Content-Type", "text/plain");
        exchange.sendResponseHeaders(status, bytes.length == 0 ? -1 : bytes.length);
        if (bytes.length > 0) {
            try (OutputStream os = exchange.getResponseBody()) {
                os.write(bytes);
            }
        }
    }
}
