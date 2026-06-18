package org.kissweb.oauth.client;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.TestInstance;
import org.kissweb.restServer.MainServlet;

import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Exercises {@link OAuthClientConfig}: multi-provider section parsing,
 * the inertness guarantee when no provider sections exist, and that a
 * provider without a Url is skipped.
 */
@TestInstance(TestInstance.Lifecycle.PER_CLASS)
class OAuthClientConfigTest {

    private static Path   tempDir;
    private static String priorAppPath;

    @BeforeAll
    void setupOnce() throws IOException {
        priorAppPath = MainServlet.getApplicationPath();
        tempDir = Files.createTempDirectory("kiss-oauth-client-cfg-");
        MainServlet.setApplicationPath(tempDir.toString() + "/");
    }

    @AfterAll
    void teardownOnce() throws IOException {
        // Restore the global application path so later test classes in the
        // same JVM are not affected by our temp directory being deleted.
        MainServlet.setApplicationPath(priorAppPath);
        OAuthClientConfig.reset();
        deleteRecursive(tempDir.toFile());
    }

    private void writeIni(String body) throws IOException {
        Files.write(tempDir.resolve("application.ini"), body.getBytes(StandardCharsets.UTF_8));
        // Clear any client keys a prior test left, then reset the singleton.
        for (String k : new String[]{"OAuthClientRedirectBaseUrl",
                "OAuthClientConfigFile", "OAuthClientRefreshSkewSeconds"})
            MainServlet.putEnvironment(k, "");
        OAuthClientConfig.reset();
    }

    @Test
    void parsesMultipleProviders() throws IOException {
        writeIni(""
                + "[main]\n"
                + "DatabaseType = SQLite\n"
                + "\n"
                + "[OAuthClient ownsona]\n"
                + "Url = https://example.com/mcp\n"
                + "Scopes = mcp:read mcp:write\n"
                + "ClientId =\n"
                + "ClientSecret =\n"
                + "\n"
                + "[OAuthClient other]\n"
                + "Url = https://other.example.com\n"
                + "ClientId = abc123\n"
                + "ClientSecret = sekret\n");

        final OAuthClientConfig cfg = OAuthClientConfig.get();
        assertTrue(cfg.isEnabled());

        final List<String> names = cfg.getProviderNames();
        assertEquals(2, names.size());
        assertTrue(names.contains("ownsona"));
        assertTrue(names.contains("other"));

        final OAuthClientProvider ownsona = cfg.getProvider("ownsona");
        assertNotNull(ownsona);
        assertEquals("https://example.com/mcp", ownsona.getUrl());
        assertEquals(List.of("mcp:read", "mcp:write"), ownsona.getScopes());
        assertEquals("mcp:read mcp:write", ownsona.getScopeString());
        assertFalse(ownsona.hasConfiguredClient(), "blank ClientId must mean DCR");
        assertNull(ownsona.getClientId());

        final OAuthClientProvider other = cfg.getProvider("other");
        assertTrue(other.hasConfiguredClient());
        assertEquals("abc123", other.getClientId());
        assertEquals("sekret", other.getClientSecret());
        assertTrue(other.getScopes().isEmpty());
    }

    @Test
    void inertWithNoProviderSections() throws IOException {
        writeIni("[main]\nDatabaseType = SQLite\n");
        final OAuthClientConfig cfg = OAuthClientConfig.get();
        assertFalse(cfg.isEnabled());
        assertTrue(cfg.getProviderNames().isEmpty());
        assertNull(cfg.getProvider("anything"));
    }

    @Test
    void providerWithoutUrlIsSkipped() throws IOException {
        writeIni(""
                + "[OAuthClient good]\n"
                + "Url = https://good.example.com\n"
                + "\n"
                + "[OAuthClient bad]\n"
                + "Scopes = a b\n");
        final OAuthClientConfig cfg = OAuthClientConfig.get();
        assertNotNull(cfg.getProvider("good"));
        assertNull(cfg.getProvider("bad"), "a provider with no Url must be ignored");
        assertEquals(1, cfg.getProviderNames().size());
    }

    @Test
    void scopesSplitOnCommasAndSpaces() throws IOException {
        writeIni(""
                + "[OAuthClient p]\n"
                + "Url = https://p.example.com\n"
                + "Scopes = a, b ,c   d\n");
        final OAuthClientProvider p = OAuthClientConfig.get().getProvider("p");
        assertEquals(List.of("a", "b", "c", "d"), p.getScopes());
    }

    private static void deleteRecursive(File f) {
        final File[] kids = f.listFiles();
        if (kids != null)
            for (File k : kids)
                deleteRecursive(k);
        f.delete();
    }
}
