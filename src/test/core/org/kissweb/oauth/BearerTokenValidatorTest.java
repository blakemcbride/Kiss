package org.kissweb.oauth;

import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;
import org.kissweb.restServer.MainServlet;

import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.security.KeyPair;
import java.security.KeyPairGenerator;
import java.security.Signature;
import java.security.interfaces.RSAPublicKey;
import java.util.Base64;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Validates the OAuth 2.1 bearer-token resource-server pipeline end-to-end:
 * generates an RSA keypair in-process, runs a tiny JDK-built-in HTTP server
 * that publishes the public key as a JWKS and exposes the OIDC discovery
 * document, then exercises {@link BearerTokenValidator#validate(String)}
 * against tokens it builds and signs against that keypair.
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class BearerTokenValidatorTest {

    private static final String KID                 = "test-key-1";
    private static final String RESOURCE_IDENTIFIER = "https://mcp.example.test";

    private static HttpServer  server;
    private static KeyPair     keyPair;
    private static KeyPair     rotatedKeyPair;
    private static int         port;
    private static String      issuer;

    private static final AtomicReference<String> jwksBody       = new AtomicReference<>();
    private static final AtomicInteger            jwksFetchCount = new AtomicInteger();

    // ------------------------------------------------------------------
    // Fixture setup / teardown
    // ------------------------------------------------------------------

    @BeforeAll
    static void startFixture() throws Exception {
        // 1. Generate a primary signing keypair and a second one we can swap in to test rotation.
        final KeyPairGenerator gen = KeyPairGenerator.getInstance("RSA");
        gen.initialize(2048);
        keyPair        = gen.generateKeyPair();
        rotatedKeyPair = gen.generateKeyPair();

        // 2. Publish the primary key as a JWKS document.
        jwksBody.set(buildJwksJson((RSAPublicKey) keyPair.getPublic(), KID));

        // 3. Start a tiny HTTP server that serves the discovery doc and the JWKS.
        server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        port   = server.getAddress().getPort();
        issuer = "http://127.0.0.1:" + port;

        server.createContext("/.well-known/openid-configuration", exchange -> {
            final JSONObject meta = new JSONObject();
            meta.put("issuer",   issuer);
            meta.put("jwks_uri", issuer + "/jwks");
            respondJson(exchange, 200, meta.toString());
        });

        server.createContext("/jwks", exchange -> {
            jwksFetchCount.incrementAndGet();
            respondJson(exchange, 200, jwksBody.get());
        });

        server.start();

        // 4. Wire OAuth config to point at this mock authorization server.
        MainServlet.putEnvironment("OAuthAuthorizationServer", issuer);
        MainServlet.putEnvironment("OAuthResourceIdentifier",  RESOURCE_IDENTIFIER);
        MainServlet.putEnvironment("OAuthRequiredScopes",      "");           // none by default
        MainServlet.putEnvironment("OAuthJwksCacheSeconds",    "3600");
        MainServlet.putEnvironment("OAuthAllowedAlgorithms",   "RS256");
        MainServlet.putEnvironment("OAuthClockSkewSeconds",    "60");
    }

    @AfterAll
    static void stopFixture() {
        if (server != null)
            server.stop(0);
    }

    @BeforeEach
    void resetSingletons() {
        // Each test gets a fresh config + JWKS cache so the order of tests
        // doesn't matter and per-test config changes take effect.
        OAuthConfig.reset();
        JwksCache.reset();
        BearerTokenValidator.clearCurrentToken();
        jwksBody.set(buildJwksJson((RSAPublicKey) keyPair.getPublic(), KID));
        jwksFetchCount.set(0);
        // Restore defaults that some tests may have changed.
        MainServlet.putEnvironment("OAuthRequiredScopes",   "");
        MainServlet.putEnvironment("OAuthAllowedAlgorithms", "RS256");
    }

    // ==================================================================
    // Happy path
    // ==================================================================

    @Test
    void validToken_returnsClaimsAndScopes() throws Exception {
        final String token = signToken(KID, keyPair,
                defaultClaims().put("sub", "user-1").put("scope", "mcp:read mcp:write"));

        final ValidatedToken vt = BearerTokenValidator.validate(token);

        assertEquals("user-1",            vt.getSubject());
        assertEquals(issuer,              vt.getIssuer());
        assertTrue (vt.getAudience().contains(RESOURCE_IDENTIFIER));
        assertTrue (vt.hasScope("mcp:read"));
        assertTrue (vt.hasScope("mcp:write"));
        assertFalse(vt.hasScope("admin"));
    }

    @Test
    void validToken_isCached_secondCallDoesNotRefetchJwks() throws Exception {
        final String token = signToken(KID, keyPair, defaultClaims().put("sub", "user-x"));
        BearerTokenValidator.validate(token);
        final int afterFirst = jwksFetchCount.get();
        BearerTokenValidator.validate(token);
        // Second validate of the same token should be served from the
        // token cache --- no extra JWKS fetches.
        assertEquals(afterFirst, jwksFetchCount.get(), "Token cache should suppress repeated work");
    }

    @Test
    void scpClaim_isReadAsScopes() throws Exception {
        // Azure AD and some others use "scp" array instead of "scope" string.
        final JSONArray scp = new JSONArray();
        scp.put("api.read");
        scp.put("api.write");
        final String token = signToken(KID, keyPair, defaultClaims().put("scp", scp));

        final ValidatedToken vt = BearerTokenValidator.validate(token);
        assertTrue(vt.hasScope("api.read"));
        assertTrue(vt.hasScope("api.write"));
    }

    @Test
    void audAsArray_isAccepted_whenItIncludesResource() throws Exception {
        final JSONArray aud = new JSONArray();
        aud.put("https://other-resource.test");
        aud.put(RESOURCE_IDENTIFIER);
        final String token = signToken(KID, keyPair, defaultClaims().put("aud", aud));

        assertDoesNotThrow(() -> BearerTokenValidator.validate(token));
    }

    // ==================================================================
    // Claim failures
    // ==================================================================

    @Test
    void expiredToken_rejected() {
        final long pastExp = System.currentTimeMillis() / 1000 - 3600;
        final String token = signToken(KID, keyPair, defaultClaims().put("exp", pastExp));

        BearerTokenValidator.TokenValidationException ex =
                assertThrows(BearerTokenValidator.TokenValidationException.class,
                        () -> BearerTokenValidator.validate(token));
        assertTrue(ex.getMessage().toLowerCase().contains("expired"));
    }

    @Test
    void notYetValid_rejected() {
        final long futureNbf = System.currentTimeMillis() / 1000 + 3600;
        final String token = signToken(KID, keyPair, defaultClaims().put("nbf", futureNbf));

        BearerTokenValidator.TokenValidationException ex =
                assertThrows(BearerTokenValidator.TokenValidationException.class,
                        () -> BearerTokenValidator.validate(token));
        assertTrue(ex.getMessage().toLowerCase().contains("not yet valid"));
    }

    @Test
    void wrongIssuer_rejected() {
        final String token = signToken(KID, keyPair, defaultClaims().put("iss", "https://evil.example"));

        assertThrows(BearerTokenValidator.TokenValidationException.class,
                () -> BearerTokenValidator.validate(token));
    }

    @Test
    void wrongAudience_rejected() {
        final String token = signToken(KID, keyPair, defaultClaims().put("aud", "https://wrong-resource.test"));

        BearerTokenValidator.TokenValidationException ex =
                assertThrows(BearerTokenValidator.TokenValidationException.class,
                        () -> BearerTokenValidator.validate(token));
        assertTrue(ex.getMessage().toLowerCase().contains("aud"));
    }

    @Test
    void missingExp_rejected() {
        final JSONObject claims = defaultClaims();
        claims.remove("exp");
        final String token = signToken(KID, keyPair, claims);

        assertThrows(BearerTokenValidator.TokenValidationException.class,
                () -> BearerTokenValidator.validate(token));
    }

    @Test
    void missingScope_rejected_whenScopeIsRequired() {
        MainServlet.putEnvironment("OAuthRequiredScopes", "mcp:admin");
        OAuthConfig.reset();
        final String token = signToken(KID, keyPair,
                defaultClaims().put("scope", "mcp:read"));

        assertThrows(BearerTokenValidator.InsufficientScopeException.class,
                () -> BearerTokenValidator.validate(token));
    }

    @Test
    void allRequiredScopesPresent_passes() throws Exception {
        MainServlet.putEnvironment("OAuthRequiredScopes", "mcp:read mcp:write");
        OAuthConfig.reset();
        final String token = signToken(KID, keyPair,
                defaultClaims().put("scope", "mcp:read mcp:write extra"));

        assertDoesNotThrow(() -> BearerTokenValidator.validate(token));
    }

    // ==================================================================
    // Signature / structural failures
    // ==================================================================

    @Test
    void malformedJwt_rejected() {
        assertThrows(BearerTokenValidator.TokenValidationException.class,
                () -> BearerTokenValidator.validate("not.a.jwt.at.all"));
        assertThrows(BearerTokenValidator.TokenValidationException.class,
                () -> BearerTokenValidator.validate("only-two.segments"));
        assertThrows(BearerTokenValidator.TokenValidationException.class,
                () -> BearerTokenValidator.validate(""));
    }

    @Test
    void tamperedSignature_rejected() {
        final String token = signToken(KID, keyPair, defaultClaims());
        // Flip the last char of the signature.
        final char last = token.charAt(token.length() - 1);
        final char alt  = (last == 'a') ? 'b' : 'a';
        final String tampered = token.substring(0, token.length() - 1) + alt;

        assertThrows(BearerTokenValidator.TokenValidationException.class,
                () -> BearerTokenValidator.validate(tampered));
    }

    @Test
    void signedByDifferentKey_rejected() {
        // Token signed with the rotated key but JWKS still advertises the primary key.
        final String token = signToken(KID, rotatedKeyPair, defaultClaims());

        assertThrows(BearerTokenValidator.TokenValidationException.class,
                () -> BearerTokenValidator.validate(token));
    }

    @Test
    void unknownKid_triggersJwksRefresh() throws Exception {
        // Initial fetch caches kid="test-key-1".  Now publish a JWKS that
        // advertises a different kid and sign with that key; validator
        // should re-fetch when it sees the new kid.
        final String token = signToken(KID, keyPair, defaultClaims());
        BearerTokenValidator.validate(token);            // primes the cache
        final int beforeRotation = jwksFetchCount.get();

        // Rotate: publish a new JWKS with the rotated key under a new kid.
        final String newKid = "test-key-2";
        jwksBody.set(buildJwksJson((RSAPublicKey) rotatedKeyPair.getPublic(), newKid));
        final String newToken = signToken(newKid, rotatedKeyPair, defaultClaims());

        assertDoesNotThrow(() -> BearerTokenValidator.validate(newToken));
        assertTrue(jwksFetchCount.get() > beforeRotation,
                "Unknown kid should have triggered a JWKS re-fetch");
    }

    @Test
    void disallowedAlgorithm_rejected() {
        MainServlet.putEnvironment("OAuthAllowedAlgorithms", "RS512");
        OAuthConfig.reset();
        final String token = signToken(KID, keyPair, defaultClaims());   // signed RS256

        assertThrows(BearerTokenValidator.TokenValidationException.class,
                () -> BearerTokenValidator.validate(token));
    }

    @Test
    void noneAlgorithm_alwaysRejected() {
        // Craft an unsigned "alg=none" token --- the classic attack.
        final JSONObject header = new JSONObject();
        header.put("alg", "none");
        header.put("kid", KID);
        header.put("typ", "JWT");
        final String headerB64  = b64u(header.toString());
        final String payloadB64 = b64u(defaultClaims().toString());
        final String token = headerB64 + "." + payloadB64 + ".";

        assertThrows(BearerTokenValidator.TokenValidationException.class,
                () -> BearerTokenValidator.validate(token));
    }

    // ==================================================================
    // Helpers
    // ==================================================================

    private static JSONObject defaultClaims() {
        final long now = System.currentTimeMillis() / 1000;
        final JSONObject c = new JSONObject();
        c.put("iss", issuer);
        c.put("aud", RESOURCE_IDENTIFIER);
        c.put("sub", "test-user");
        c.put("iat", now);
        c.put("exp", now + 3600);
        return c;
    }

    private static String signToken(String kid, KeyPair kp, JSONObject claims) {
        try {
            final JSONObject header = new JSONObject();
            header.put("alg", "RS256");
            header.put("kid", kid);
            header.put("typ", "JWT");

            final String headerB64  = b64u(header.toString());
            final String payloadB64 = b64u(claims.toString());
            final String signingInput = headerB64 + "." + payloadB64;

            final Signature sig = Signature.getInstance("SHA256withRSA");
            sig.initSign(kp.getPrivate());
            sig.update(signingInput.getBytes(StandardCharsets.US_ASCII));
            final byte[] signature = sig.sign();

            return signingInput + "." + Base64.getUrlEncoder().withoutPadding().encodeToString(signature);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private static String b64u(String s) {
        return Base64.getUrlEncoder().withoutPadding().encodeToString(s.getBytes(StandardCharsets.UTF_8));
    }

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

    private static void respondJson(com.sun.net.httpserver.HttpExchange exchange, int status, String body)
            throws java.io.IOException {
        final byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().add("Content-Type", "application/json");
        exchange.sendResponseHeaders(status, bytes.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
        }
    }
}
