package org.kissweb.oauth.as;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.kissweb.IniFile;
import org.kissweb.json.JSONObject;
import org.kissweb.oauth.BearerTokenValidator;
import org.kissweb.oauth.JwksCache;
import org.kissweb.oauth.OAuthConfig;
import org.kissweb.oauth.ValidatedToken;
import org.kissweb.restServer.MainServlet;

import com.sun.net.httpserver.HttpServer;

import java.io.File;
import java.io.IOException;
import java.io.OutputStream;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Exercises the OAuth 2.1 authorization-server end-to-end at the
 * library level: drives the stores, issuer, and PKCE/key components
 * directly, then verifies issued tokens against the existing
 * {@link BearerTokenValidator} from the resource-server package.  The
 * HTTP wiring is a thin wrapper around these primitives and is covered
 * by the live smoke test.
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class AuthorizationServerTest {

    private static final String ISSUER             = "https://test-as.example";
    private static final String RESOURCE           = "https://mcp.example.test";
    private static final String DB_FILE_RELATIVE   = "oauth.db";

    private static Path        tempDir;
    /** Local HTTP server that publishes the AS's JWKS so the resource-server validator can fetch it. */
    private static HttpServer  jwksServer;
    private static String      jwksServerOrigin;

    @BeforeAll
    void setupOnce() throws Exception {
        tempDir = Files.createTempDirectory("kiss-oauth-as-");
        MainServlet.setApplicationPath(tempDir.toString() + "/");

        // Clear every OAuth* key first so we don't inherit state from a
        // test class that ran earlier in the same JVM (e.g.
        // BearerTokenValidatorTest leaves OAuthRequiredScopes set when
        // its last test happens to be one that mutates that value).
        for (String key : new String[]{
                "OAuthAuthorizationServer", "OAuthResourceIdentifier",
                "OAuthJwksUri", "OAuthRequiredScopes", "OAuthJwksCacheSeconds",
                "OAuthAllowedAlgorithms", "OAuthClockSkewSeconds",
                "OAuthAsEnabled", "OAuthAsIssuer", "OAuthAsSqliteFile", "OAuthAsIniFile",
                "OAuthAccessTokenTtlSeconds", "OAuthRefreshTokenTtlSeconds",
                "OAuthAuthCodeTtlSeconds", "OAuthPruneIntervalSeconds",
                "OAuthAllowDynamicRegistration", "OAuthSessionTtlSeconds",
                "OAuthKeyId"})
            MainServlet.putEnvironment(key, "");

        // AS config
        MainServlet.putEnvironment("OAuthAsEnabled",                  "true");
        MainServlet.putEnvironment("OAuthAsIssuer",                   ISSUER);
        MainServlet.putEnvironment("OAuthAsSqliteFile",               DB_FILE_RELATIVE);
        MainServlet.putEnvironment("OAuthAccessTokenTtlSeconds",      "3600");
        MainServlet.putEnvironment("OAuthRefreshTokenTtlSeconds",     "1209600");
        MainServlet.putEnvironment("OAuthAuthCodeTtlSeconds",         "60");
        MainServlet.putEnvironment("OAuthAllowDynamicRegistration",   "true");
        MainServlet.putEnvironment("OAuthKeyId",                      "test-kid-1");

        // Start a JWKS-publishing HTTP server backed by KeyManager so the
        // resource-server validator can verify tokens we issue.
        jwksServer = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        final int port = jwksServer.getAddress().getPort();
        jwksServerOrigin = "http://127.0.0.1:" + port;

        jwksServer.createContext("/.well-known/openid-configuration", exchange -> {
            final JSONObject meta = new JSONObject();
            meta.put("issuer",   ISSUER);
            meta.put("jwks_uri", jwksServerOrigin + "/jwks");
            respondJson(exchange, 200, meta.toString());
        });
        jwksServer.createContext("/jwks", exchange -> {
            // KeyManager publishes its current key here.
            final String body = KeyManager.get().buildJwks().toString();
            respondJson(exchange, 200, body);
        });
        jwksServer.start();

        // Resource-server config: point the validator at our jwks endpoint
        // but use the AS issuer claim that tokens will actually carry.
        MainServlet.putEnvironment("OAuthAuthorizationServer", ISSUER);
        MainServlet.putEnvironment("OAuthResourceIdentifier",  RESOURCE);
        MainServlet.putEnvironment("OAuthJwksUri",             jwksServerOrigin + "/jwks");
    }

    @AfterAll
    void teardownOnce() throws IOException {
        if (jwksServer != null) jwksServer.stop(0);
        if (tempDir != null)
            deleteRecursive(tempDir.toFile());
    }

    @BeforeEach
    void freshState() throws IOException {
        // Wipe the oauth.db file so each test starts clean.  Singletons
        // must be reset before the file delete so any open SQLite
        // handle is closed first (Windows in particular will refuse to
        // delete an open file).
        AuthorizationServerConfig.reset();
        KeyManager.reset();
        ClientStore.reset();
        RefreshTokenStore.reset();
        OAuthSqliteStore.reset();
        OAuthConfig.reset();
        JwksCache.reset();
        // Re-clear the legacy ini key in case a migration test set it.
        MainServlet.putEnvironment("OAuthAsIniFile", "");
        final File db = new File(tempDir.toFile(), DB_FILE_RELATIVE);
        if (db.exists())
            db.delete();
        final File legacyIni = new File(tempDir.toFile(), "oauth.ini");
        if (legacyIni.exists())
            legacyIni.delete();
    }

    // ==================================================================
    // KeyManager
    // ==================================================================

    @Test
    void keyManager_generatesAndPersistsAcrossReload() throws IOException {
        final KeyManager first = KeyManager.get();
        final String firstKid = first.getKid();
        assertNotNull(first.getPrivateKey());
        assertNotNull(first.getPublicKey());

        // Now reset singletons and reload from disk --- should be the same key.
        KeyManager.reset();
        OAuthSqliteStore.reset();
        final KeyManager reloaded = KeyManager.get();
        assertEquals(firstKid, reloaded.getKid());
        assertEquals(first.getPublicKey(), reloaded.getPublicKey());
        assertEquals(first.getPrivateKey(), reloaded.getPrivateKey());
    }

    // ==================================================================
    // PKCE
    // ==================================================================

    @Test
    void pkce_s256_acceptsMatchingVerifier() {
        // Per RFC 7636 Appendix B example.
        final String verifier  = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk";
        final String challenge = "E9Melhoa2OwvFrEMTJguCHaoeK1t8URWbuGJSstw-cM";
        assertTrue(PkceValidator.verify("S256", verifier, challenge));
    }

    @Test
    void pkce_rejectsPlainMethod() {
        // OAuth 2.1 forbids the "plain" method even if challenge == verifier.
        assertFalse(PkceValidator.verify("plain", "abc", "abc"));
        assertFalse(PkceValidator.verify(null,    "abc", "abc"));
    }

    @Test
    void pkce_rejectsShortOrTamperedVerifier() {
        final String v = "dBjftJeZ4CVP-mB92K27uhbUJU1p1r_wW1gFWFOEjXk";
        final String c = PkceValidator.sha256Base64Url(v);
        // Too short
        assertFalse(PkceValidator.verify("S256", "short", c));
        // Tampered verifier
        assertFalse(PkceValidator.verify("S256", v.substring(0, v.length() - 1) + "X", c));
    }

    // ==================================================================
    // Token issuance / cross-validation
    // ==================================================================

    @Test
    void issuedAccessToken_validatesViaResourceServer() throws Exception {
        final AuthenticatedUser user = new AuthenticatedUser("user-123",
                jsonObj("email", "alice@example.com"));
        final Set<String> scopes = setOf("mcp:read", "mcp:write");
        final String token = TokenIssuer.issueAccessToken(user, "client-A", scopes, RESOURCE);

        // Round-trip through the resource server's validator --- the same
        // code an MCP server would run.
        final ValidatedToken vt = BearerTokenValidator.validate(token);
        assertEquals("user-123",           vt.getSubject());
        assertEquals(ISSUER,               vt.getIssuer());
        assertTrue(vt.getAudience().contains(RESOURCE));
        assertTrue(vt.hasScope("mcp:read"));
        assertTrue(vt.hasScope("mcp:write"));
        assertEquals("alice@example.com",  vt.getClaimString("email"));
        assertEquals("client-A",           vt.getClaimString("client_id"));
        assertEquals("at+jwt",             vt.getClaimString("typ"));
    }

    // ==================================================================
    // Client store
    // ==================================================================

    @Test
    void clientStore_registerLookupRemove_persistsAcrossReload() throws IOException {
        final ClientStore store = ClientStore.get();
        final RegisteredClient c = new RegisteredClient(
                "client-A", "sha256:dummyhash", "Test Client",
                listOf("http://localhost:9999/cb"),
                setOf("mcp:read"),
                setOf("authorization_code", "refresh_token"),
                System.currentTimeMillis() / 1000L);
        store.register(c);
        assertNotNull(store.get("client-A"));

        // Reload from disk.
        ClientStore.reset();
        OAuthSqliteStore.reset();
        final RegisteredClient reloaded = ClientStore.get().get("client-A");
        assertNotNull(reloaded);
        assertEquals("Test Client", reloaded.getClientName());
        assertEquals("sha256:dummyhash", reloaded.getClientSecretHash());
        assertTrue(reloaded.hasRedirectUri("http://localhost:9999/cb"));
        assertTrue(reloaded.getAllowedScopes().contains("mcp:read"));
        assertTrue(reloaded.getAllowedGrantTypes().contains("authorization_code"));

        ClientStore.get().remove("client-A");
        assertNull(ClientStore.get().get("client-A"));
    }

    // ==================================================================
    // Refresh-token rotation
    // ==================================================================

    @Test
    void refreshToken_rotation_invalidatesOldAndIssuesNew() throws IOException {
        final RefreshTokenStore store = RefreshTokenStore.get();
        final long now = System.currentTimeMillis() / 1000L;
        final String familyId = "fam-1";
        final RefreshToken original = new RefreshToken(
                "rt-old", familyId, "client-A", "user-1", null,
                setOf("mcp:read"), RESOURCE,
                now, now + 86400, null);
        store.store(original);

        final RefreshToken successor = new RefreshToken(
                "rt-new", familyId, "client-A", "user-1", null,
                setOf("mcp:read"), RESOURCE,
                now, now + 86400, null);
        store.rotate("rt-old", successor);

        final RefreshToken oldAfter = store.get("rt-old");
        final RefreshToken newAfter = store.get("rt-new");
        assertNotNull(oldAfter);
        assertTrue(oldAfter.isRotated(), "Old token should be marked rotated");
        assertEquals("rt-new", oldAfter.getRotatedToJti());
        assertNotNull(newAfter);
        assertFalse(newAfter.isRotated(), "Successor should not be marked rotated");
    }

    @Test
    void refreshToken_reuseOfRotated_revokesEntireFamily() throws IOException {
        final RefreshTokenStore store = RefreshTokenStore.get();
        final long now = System.currentTimeMillis() / 1000L;
        final String familyId = "fam-attack";

        // Build a chain of three rotations: rt-1 -> rt-2 -> rt-3.
        final RefreshToken rt1 = new RefreshToken("rt-1", familyId, "client-A", "user-1", null,
                setOf("mcp:read"), null, now, now + 86400, null);
        store.store(rt1);
        store.rotate("rt-1", new RefreshToken("rt-2", familyId, "client-A", "user-1", null,
                setOf("mcp:read"), null, now, now + 86400, null));
        store.rotate("rt-2", new RefreshToken("rt-3", familyId, "client-A", "user-1", null,
                setOf("mcp:read"), null, now, now + 86400, null));

        assertEquals(3, store.all().size());

        // Now an attacker replays rt-1.  The server detects it and revokes
        // the whole family.
        store.revokeFamily(familyId);
        assertNull(store.get("rt-1"));
        assertNull(store.get("rt-2"));
        assertNull(store.get("rt-3"));
        assertEquals(0, store.all().size());
    }

    @Test
    void refreshToken_pruneRemovesExpiredOnly() throws IOException {
        final RefreshTokenStore store = RefreshTokenStore.get();
        final long now = System.currentTimeMillis() / 1000L;
        final RefreshToken expired = new RefreshToken("rt-old",  "fam-1", "client-A", "user-1", null,
                setOf("mcp:read"), null, now - 100000, now - 1, null);
        final RefreshToken live    = new RefreshToken("rt-live", "fam-2", "client-A", "user-1", null,
                setOf("mcp:read"), null, now, now + 86400, null);
        store.store(expired);
        store.store(live);

        final int removed = store.pruneExpired();
        assertEquals(1, removed);
        assertNull(store.get("rt-old"));
        assertNotNull(store.get("rt-live"));
    }

    @Test
    void refreshToken_extraClaims_roundTripAcrossPersistence() throws IOException {
        final RefreshTokenStore store = RefreshTokenStore.get();
        final long now = System.currentTimeMillis() / 1000L;
        final JSONObject extras = new JSONObject();
        extras.put("email",  "bob@example.com");
        extras.put("groups", "admins users");

        final RefreshToken rt = new RefreshToken("rt-X", "fam-X", "client-A", "user-2",
                extras, setOf("mcp:read"), RESOURCE,
                now, now + 86400, null);
        store.store(rt);

        // Reload from disk.
        RefreshTokenStore.reset();
        OAuthSqliteStore.reset();
        final RefreshToken reloaded = RefreshTokenStore.get().get("rt-X");
        assertNotNull(reloaded);
        assertEquals("user-2",            reloaded.getUserSubject());
        assertEquals("bob@example.com",   reloaded.getUserExtraClaims().getString("email"));
        assertEquals("admins users",      reloaded.getUserExtraClaims().getString("groups"));
    }

    // ==================================================================
    // One-shot ini -> SQLite migration
    // ==================================================================

    @Test
    void iniMigration_importsClientsAndTokens_thenDeletesIniFile() throws Exception {
        // Build a populated oauth.ini in the temp dir.
        final String legacyIniName = "oauth.ini";
        final File legacyIni = new File(tempDir.toFile(), legacyIniName);
        final IniFile ini = new IniFile(legacyIni.getAbsolutePath());
        ini.put("client.legacy-client", "client_secret_hash",  "sha256:legacy");
        ini.put("client.legacy-client", "client_name",         "Legacy Client");
        ini.put("client.legacy-client", "redirect_uris",       "http://localhost/cb");
        ini.put("client.legacy-client", "allowed_scopes",      "mcp:read mcp:write");
        ini.put("client.legacy-client", "allowed_grant_types", "authorization_code refresh_token");
        ini.put("client.legacy-client", "created_at",          "1700000000");
        ini.put("refresh.legacy-rt", "family_id",         "fam-legacy");
        ini.put("refresh.legacy-rt", "client_id",         "legacy-client");
        ini.put("refresh.legacy-rt", "user_sub",          "user-legacy");
        ini.put("refresh.legacy-rt", "user_extra_claims", "");
        ini.put("refresh.legacy-rt", "scopes",            "mcp:read");
        ini.put("refresh.legacy-rt", "audience",          "");
        ini.put("refresh.legacy-rt", "created_at",        "1700000000");
        ini.put("refresh.legacy-rt", "expires_at",        String.valueOf(System.currentTimeMillis() / 1000L + 86400));
        ini.put("refresh.legacy-rt", "rotated_to_jti",    "");
        ini.save();
        assertTrue(legacyIni.exists(), "precondition: legacy ini file written");

        // Configure the migration trigger and re-read AS config.
        MainServlet.putEnvironment("OAuthAsIniFile", legacyIniName);
        AuthorizationServerConfig.reset();

        // First call into any store opens the SQLite DB and runs the
        // migration as part of OAuthSqliteStore.open().
        final RegisteredClient migratedClient = ClientStore.get().get("legacy-client");
        assertNotNull(migratedClient,            "client should be imported");
        assertEquals("Legacy Client",            migratedClient.getClientName());
        assertEquals("sha256:legacy",            migratedClient.getClientSecretHash());
        assertTrue(migratedClient.hasRedirectUri("http://localhost/cb"));
        assertTrue(migratedClient.getAllowedScopes().contains("mcp:read"));
        assertTrue(migratedClient.getAllowedScopes().contains("mcp:write"));

        final RefreshToken migratedToken = RefreshTokenStore.get().get("legacy-rt");
        assertNotNull(migratedToken,             "refresh token should be imported");
        assertEquals("fam-legacy",               migratedToken.getFamilyId());
        assertEquals("legacy-client",            migratedToken.getClientId());
        assertEquals("user-legacy",              migratedToken.getUserSubject());

        assertFalse(legacyIni.exists(),
                "ini file should be deleted after a successful migration");
    }

    // ==================================================================
    // Authorization-code single-use
    // ==================================================================

    @Test
    void authCode_consumedOnce_thenNull() {
        final AuthorizationCodeStore store = AuthorizationCodeStore.get();
        final long now = System.currentTimeMillis() / 1000L;
        final AuthenticatedUser u = new AuthenticatedUser("user-1");
        final AuthorizationCode c = new AuthorizationCode("abc", "client-A", u,
                "http://localhost/cb", setOf("mcp:read"), RESOURCE,
                "challenge-value", "S256", null, now + 60);
        store.store(c);

        assertNotNull(store.consume("abc"));
        assertNull(store.consume("abc"), "Second consume of the same code must return null");
    }

    @Test
    void authCode_expiredCodeIsRejected() {
        final AuthorizationCodeStore store = AuthorizationCodeStore.get();
        final long now = System.currentTimeMillis() / 1000L;
        final AuthorizationCode c = new AuthorizationCode("abc-exp", "client-A",
                new AuthenticatedUser("user-1"),
                "http://localhost/cb", setOf("mcp:read"), null,
                "challenge", "S256", null, now - 1);
        store.store(c);
        assertNull(store.consume("abc-exp"));
    }

    // ==================================================================
    // Helpers
    // ==================================================================

    private static Set<String> setOf(String... values) {
        final Set<String> s = new LinkedHashSet<>();
        Collections.addAll(s, values);
        return s;
    }

    private static List<String> listOf(String... values) {
        final List<String> l = new ArrayList<>(values.length);
        Collections.addAll(l, values);
        return l;
    }

    private static JSONObject jsonObj(String key, String value) {
        final JSONObject o = new JSONObject();
        o.put(key, value);
        return o;
    }

    private static void respondJson(com.sun.net.httpserver.HttpExchange exchange, int status, String body) throws IOException {
        final byte[] bytes = body.getBytes(StandardCharsets.UTF_8);
        exchange.getResponseHeaders().add("Content-Type", "application/json");
        exchange.sendResponseHeaders(status, bytes.length);
        try (OutputStream os = exchange.getResponseBody()) {
            os.write(bytes);
        }
    }

    private static void deleteRecursive(File f) {
        if (f == null || !f.exists()) return;
        if (f.isDirectory())
            for (File c : f.listFiles())
                deleteRecursive(c);
        f.delete();
    }
}
