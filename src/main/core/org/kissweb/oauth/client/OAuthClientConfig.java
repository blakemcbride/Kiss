package org.kissweb.oauth.client;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.IniFile;
import org.kissweb.restServer.MainServlet;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

/**
 * Reads the outbound OAuth 2.1 <em>client</em> settings from
 * {@code application.ini}.  Lazy singleton --- parsed once on first
 * access, then cached for the JVM.
 * <br><br>
 * <h2>Provider sections</h2>
 * Each remote OAuth-protected server is declared in its own section
 * named {@code [OAuthClient <name>]}, where {@code <name>} is the
 * operator's label.  Multiple providers are supported --- one section
 * each.  These sections are read directly from the ini file (the
 * framework's flat {@code environment} map only carries the {@code [main]}
 * section, so {@link MainServlet#getEnvironment} cannot see them).
 * <pre>
 * [OAuthClient ownsona]
 * Url          = https://example.com/mcp   ; remote resource/server URL (required)
 * Scopes       = mcp:read mcp:write        ; space- or comma-separated (optional)
 * ClientId     =                           ; blank =&gt; Dynamic Client Registration
 * ClientSecret =                           ; only if pre-registered + confidential
 * </pre>
 * <h2>Optional {@code [main]} keys</h2>
 * <ul>
 *   <li>{@code OAuthClientRedirectBaseUrl} --- base URL used to build the
 *       {@code redirect_uri} ({@code <base>/oauth/client/callback}).  If
 *       unset, the base is derived from the incoming request that starts
 *       the flow.</li>
 *   <li>{@code OAuthClientConfigFile} --- the ini file the provider
 *       sections are read from.  Default {@code application.ini}.  Exists
 *       mainly so tests can point at a fixture.</li>
 *   <li>{@code OAuthClientRefreshSkewSeconds} --- treat an access token as
 *       expired this many seconds before its real expiry.  Default 60.</li>
 * </ul>
 * <h2>Persistence</h2>
 * Cached registrations and tokens are stored in the same SQLite database
 * the authorization-server side uses ({@code oauth.db} by default,
 * configured via {@code OAuthAsSqliteFile}); the client does not open a
 * separate database.  See {@link OAuthClientStore}.
 * <br><br>
 * <h2>Inertness</h2>
 * With no {@code [OAuthClient *]} section, {@link #isEnabled()} returns
 * false and the whole client package does nothing --- the client never
 * touches the database, the callback servlet returns 404, and no
 * application code path reaches the network.
 */
public final class OAuthClientConfig {

    private static final Logger logger = LogManager.getLogger(OAuthClientConfig.class);

    /** Section-name prefix that marks a provider section (note the trailing space). */
    public static final String SECTION_PREFIX = "OAuthClient ";

    /** Path segment the callback servlet is mounted at. */
    public static final String CALLBACK_PATH = "/oauth/client/callback";

    private static volatile OAuthClientConfig instance;

    private final Map<String, OAuthClientProvider> providers;
    private final String redirectBaseUrl;
    private final int    refreshSkewSeconds;

    private OAuthClientConfig() {
        this.providers          = Collections.unmodifiableMap(loadProviders());
        this.redirectBaseUrl    = trimTrailingSlash(getString("OAuthClientRedirectBaseUrl"));
        this.refreshSkewSeconds = getInt("OAuthClientRefreshSkewSeconds", 60);
    }

    /**
     * Get the process-wide singleton, parsing {@code application.ini} on
     * first call.
     *
     * @return the client configuration
     */
    public static OAuthClientConfig get() {
        OAuthClientConfig local = instance;
        if (local == null) {
            synchronized (OAuthClientConfig.class) {
                local = instance;
                if (local == null) {
                    local = new OAuthClientConfig();
                    instance = local;
                }
            }
        }
        return local;
    }

    /**
     * Reset the cached configuration so the next {@link #get()} re-reads
     * {@code application.ini}.  Intended for tests.
     */
    public static synchronized void reset() {
        instance = null;
    }

    /**
     * Whether any client provider is configured.
     * @return true if at least one {@code [OAuthClient *]} section exists
     */
    public boolean isEnabled() {
        return !providers.isEmpty();
    }

    /**
     * Get a configured provider by name.
     *
     * @param name the provider label
     * @return the provider, or null if no such provider is configured
     */
    public OAuthClientProvider getProvider(String name) {
        return providers.get(name);
    }

    /**
     * Get the names of all configured providers, in declaration order.
     * @return the provider names
     */
    public List<String> getProviderNames() {
        return new ArrayList<>(providers.keySet());
    }

    /**
     * Get the configured redirect base URL, or null if it should be
     * derived from the request.
     * @return the redirect base URL, or null
     */
    public String getRedirectBaseUrl() {
        return redirectBaseUrl;
    }

    /**
     * Get the access-token expiry safety margin.
     * @return the refresh skew in seconds
     */
    public int getRefreshSkewSeconds() {
        return refreshSkewSeconds;
    }

    // ------------------------------------------------------------------

    private static Map<String, OAuthClientProvider> loadProviders() {
        final Map<String, OAuthClientProvider> out = new LinkedHashMap<>();
        final String configFile = orDefault(getString("OAuthClientConfigFile"), "application.ini");
        final IniFile ini;
        try {
            ini = IniFile.load(configFile);
        } catch (IOException e) {
            logger.warn("Could not read OAuth client config file '" + configFile + "'; "
                    + "client support disabled", e);
            return out;
        }
        if (ini == null)
            return out;   // file does not exist --- inert

        for (String section : ini.getSectionNames()) {
            if (section == null || !section.startsWith(SECTION_PREFIX))
                continue;
            final String name = section.substring(SECTION_PREFIX.length()).trim();
            if (name.isEmpty()) {
                logger.warn("Ignoring OAuth client section with empty name: [" + section + "]");
                continue;
            }
            final String url = ini.get(section, "Url");
            if (url == null || url.trim().isEmpty()) {
                logger.warn("Ignoring OAuth client provider '" + name + "': no Url configured");
                continue;
            }
            final OAuthClientProvider provider = new OAuthClientProvider(
                    name,
                    url.trim(),
                    splitScopes(ini.get(section, "Scopes")),
                    ini.get(section, "ClientId"),
                    ini.get(section, "ClientSecret"));
            out.put(name, provider);
            logger.info("Configured OAuth client provider '" + name + "' -> " + url.trim());
        }
        return out;
    }

    private static List<String> splitScopes(String raw) {
        if (raw == null || raw.trim().isEmpty())
            return Collections.emptyList();
        final List<String> out = new ArrayList<>();
        for (String s : raw.trim().split("[\\s,]+"))
            if (!s.isEmpty())
                out.add(s);
        return out;
    }

    private static String getString(String key) {
        final Object v = MainServlet.getEnvironment(key);
        if (v == null)
            return null;
        final String s = v.toString().trim();
        return s.isEmpty() ? null : s;
    }

    private static int getInt(String key, int defaultValue) {
        final String s = getString(key);
        if (s == null)
            return defaultValue;
        try {
            return Integer.parseInt(s);
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    private static String orDefault(String value, String dflt) {
        return value == null || value.isEmpty() ? dflt : value;
    }

    private static String trimTrailingSlash(String s) {
        if (s == null)
            return null;
        while (s.endsWith("/"))
            s = s.substring(0, s.length() - 1);
        return s;
    }
}
