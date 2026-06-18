package org.kissweb.oauth.client;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.kissweb.oauth.as.AuthorizationServerConfig;
import org.kissweb.oauth.as.OAuthSqliteStore;
import org.kissweb.restServer.MainServlet;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Exercises {@link OAuthClientStore}: that it persists into the shared
 * authorization-server database ({@code oauth.db}), lazy table creation,
 * and round-trips for discovery, client registration, and token sets
 * (including the insert-or-replace that implements refresh-token
 * rotation).
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class OAuthClientStoreTest {

    /** The shared OAuth database (authorization-server default). */
    private static final String DB_FILE = "oauth.db";

    private static Path   tempDir;
    private static String priorAppPath;

    @BeforeAll
    void setupOnce() throws IOException {
        priorAppPath = MainServlet.getApplicationPath();
        tempDir = Files.createTempDirectory("kiss-oauth-client-store-");
        MainServlet.setApplicationPath(tempDir.toString() + "/");
        // A minimal application.ini so OAuthClientConfig loads cleanly.
        Files.write(tempDir.resolve("application.ini"),
                "[main]\nDatabaseType = SQLite\n".getBytes(StandardCharsets.UTF_8));
    }

    @AfterAll
    void teardownOnce() throws IOException {
        OAuthSqliteStore.reset();
        OAuthClientStore.reset();
        OAuthClientConfig.reset();
        AuthorizationServerConfig.reset();
        // Restore the global application path for later test classes.
        MainServlet.setApplicationPath(priorAppPath);
        deleteRecursive(tempDir.toFile());
    }

    @BeforeEach
    void freshState() {
        OAuthSqliteStore.reset();   // close the shared handle before deleting the file
        OAuthClientStore.reset();
        OAuthClientConfig.reset();
        AuthorizationServerConfig.reset();
        for (String k : new String[]{"OAuthAsSqliteFile", "OAuthAsIniFile",
                "OAuthClientConfigFile", "OAuthClientRefreshSkewSeconds"})
            MainServlet.putEnvironment(k, "");
        final File db = new File(tempDir.toFile(), DB_FILE);
        if (db.exists())
            assertTrue(db.delete());
    }

    @Test
    void sharedDatabaseFileCreatedLazily() throws Exception {
        final File db = new File(tempDir.toFile(), DB_FILE);
        assertFalse(db.exists(), "no db file should exist before first store use");
        // First client-store operation should open/create the shared oauth.db.
        assertNull(OAuthClientStore.get().loadTokens("nobody"));
        assertTrue(db.exists(), "client store should persist into the shared oauth.db");
        // And it must be the same file OAuthSqliteStore reports.
        assertEquals(new File(tempDir.toFile(), DB_FILE).getAbsolutePath(),
                OAuthClientStore.get().getDbPath());
    }

    @Test
    void discoveryRoundTrip() throws Exception {
        final OAuthClientStore store = OAuthClientStore.get();
        assertNull(store.loadDiscovery("p"));

        store.saveDiscovery("p", new DiscoveryMetadata(
                "https://issuer.example", "https://issuer.example/authorize",
                "https://issuer.example/token", "https://issuer.example/register"));
        final DiscoveryMetadata d = store.loadDiscovery("p");
        assertNotNull(d);
        assertEquals("https://issuer.example", d.getIssuer());
        assertEquals("https://issuer.example/authorize", d.getAuthorizationEndpoint());
        assertEquals("https://issuer.example/token", d.getTokenEndpoint());
        assertEquals("https://issuer.example/register", d.getRegistrationEndpoint());
    }

    @Test
    void discoveryWithoutRegistrationEndpoint() throws Exception {
        final OAuthClientStore store = OAuthClientStore.get();
        store.saveDiscovery("p", new DiscoveryMetadata(
                "https://issuer.example", "https://issuer.example/authorize",
                "https://issuer.example/token", null));
        assertNull(store.loadDiscovery("p").getRegistrationEndpoint());
    }

    @Test
    void registrationRoundTripPublicAndConfidential() throws Exception {
        final OAuthClientStore store = OAuthClientStore.get();

        store.saveRegistration("pub", new ClientRegistration("cid-pub", null, "https://app/cb"));
        final ClientRegistration pub = store.loadRegistration("pub");
        assertEquals("cid-pub", pub.getClientId());
        assertNull(pub.getClientSecret());
        assertFalse(pub.isConfidential());
        assertEquals("https://app/cb", pub.getRedirectUri());

        store.saveRegistration("conf", new ClientRegistration("cid-conf", "shh", "https://app/cb"));
        final ClientRegistration conf = store.loadRegistration("conf");
        assertEquals("shh", conf.getClientSecret());
        assertTrue(conf.isConfidential());
    }

    @Test
    void tokenRoundTripAndRotation() throws Exception {
        final OAuthClientStore store = OAuthClientStore.get();
        assertNull(store.loadTokens("p"));

        final long exp = System.currentTimeMillis() / 1000L + 3600;
        store.saveTokens("p", new TokenSet("access1", "refresh1", "Bearer", "mcp:read", exp));
        TokenSet t = store.loadTokens("p");
        assertEquals("access1", t.getAccessToken());
        assertEquals("refresh1", t.getRefreshToken());
        assertEquals("Bearer", t.getTokenType());
        assertEquals("mcp:read", t.getScope());
        assertEquals(exp, t.getExpiresAtEpochSeconds());
        assertFalse(t.isExpired(60));

        // Rotation: a second save for the same provider replaces the row.
        store.saveTokens("p", new TokenSet("access2", "refresh2", "Bearer", "mcp:read", exp));
        t = store.loadTokens("p");
        assertEquals("access2", t.getAccessToken());
        assertEquals("refresh2", t.getRefreshToken());

        store.deleteTokens("p");
        assertNull(store.loadTokens("p"));
    }

    @Test
    void expiredTokenDetected() throws Exception {
        final OAuthClientStore store = OAuthClientStore.get();
        final long past = System.currentTimeMillis() / 1000L - 10;
        store.saveTokens("p", new TokenSet("a", "r", "Bearer", null, past));
        assertTrue(store.loadTokens("p").isExpired(0));
    }

    @Test
    void coexistsWithAuthServerTablesInSameFile() throws Exception {
        // Opening via the AS store first creates the AS schema; the client
        // store must then add its tables to the very same file.
        OAuthSqliteStore.get();
        final OAuthClientStore store = OAuthClientStore.get();
        store.saveTokens("p", new TokenSet("a", "r", "Bearer", null, 0L));
        assertEquals("a", store.loadTokens("p").getAccessToken());
        // Exactly one database file exists.
        final File[] dbs = tempDir.toFile().listFiles((dir, n) -> n.startsWith("oauth") && n.endsWith(".db"));
        assertNotNull(dbs);
        assertEquals(1, dbs.length, "there must be exactly one oauth*.db file");
        assertEquals(DB_FILE, dbs[0].getName());
    }

    private static void deleteRecursive(File f) {
        final File[] kids = f.listFiles();
        if (kids != null)
            for (File k : kids)
                deleteRecursive(k);
        f.delete();
    }
}
