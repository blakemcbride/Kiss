package org.kissweb.oauth.as;

import org.kissweb.restServer.MainServlet;

/**
 * Reads the OAuth 2.1 authorization-server settings from
 * {@code application.ini} via {@link MainServlet#getEnvironment(String)}.
 * Lazy singleton --- read once on first access, then cached.
 * <br><br>
 * Recognized keys:
 * <ul>
 *   <li>{@code OAuthAsEnabled} --- {@code true} to enable the AS.  If
 *       not set or false, all AS endpoints return 404 and the AS does
 *       nothing.</li>
 *   <li>{@code OAuthAsIssuer} --- the canonical URL of this AS.  Used
 *       as the {@code iss} claim in issued tokens and as the
 *       {@code issuer} field in the metadata document.  Defaults to
 *       the value of {@code OAuthAuthorizationServer} from the
 *       resource-server config, then to the empty string.</li>
 *   <li>{@code OAuthAsSqliteFile} --- path to the SQLite database that
 *       holds the AS's persistent state (signing keys, registered
 *       clients, refresh tokens).  Default {@code oauth.db}, resolved
 *       against the application root (the deployed
 *       {@code WEB-INF/backend/} directory, or its dev-mode equivalent
 *       {@code src/main/backend/}).  May also be given as an absolute
 *       path (e.g. {@code /home/myapp/oauth.db}), which is used
 *       verbatim --- useful for placing the runtime state file outside
 *       the deployed webapp so a WAR redeploy cannot clobber it.  If
 *       the file does not exist on startup it is created automatically;
 *       its schema is materialized via {@code CREATE TABLE IF NOT EXISTS}
 *       on every open.</li>
 *   <li>{@code OAuthAccessTokenTtlSeconds} --- lifetime of issued
 *       access tokens.  Default 3600 (one hour).</li>
 *   <li>{@code OAuthRefreshTokenTtlSeconds} --- lifetime of issued
 *       refresh tokens.  Default 2592000 (30 days).</li>
 *   <li>{@code OAuthAuthCodeTtlSeconds} --- lifetime of authorization
 *       codes.  Default 60.</li>
 *   <li>{@code OAuthPruneIntervalSeconds} --- minimum seconds between
 *       auto-prune sweeps of expired tokens / codes.  Default 900
 *       (15 minutes).  Pruning is opportunistic --- it runs only when
 *       an OAuth call arrives after the interval has elapsed.</li>
 *   <li>{@code OAuthAllowDynamicRegistration} --- whether the
 *       {@code /oauth/register} endpoint is enabled.  Default
 *       {@code true} (required for MCP).</li>
 *   <li>{@code OAuthSessionTtlSeconds} --- lifetime of the AS login
 *       session cookie set after a user authenticates.  Default 1800
 *       (30 minutes).  When the cookie is valid, subsequent
 *       {@code /authorize} requests from the same user skip the login
 *       step and go straight to consent.</li>
 *   <li>{@code OAuthKeyId} --- key id ({@code kid}) embedded in JWT
 *       headers and the JWKS.  Default {@code kiss-key-1}.</li>
 * </ul>
 */
public final class AuthorizationServerConfig {

    /** Default issuer used when no OAuthAsIssuer or OAuthAuthorizationServer is set. */
    public static final String DEFAULT_ISSUER = "";

    /**
     * Default location of the SQLite persistent state file, relative to
     * {@code MainServlet.getApplicationPath()}.  The application path
     * already points at the backend directory --- in dev mode that's
     * {@code src/main/backend/}, in a deployed WAR it's
     * {@code WEB-INF/backend/} --- so the file is colocated with
     * {@code application.ini}.
     */
    public static final String DEFAULT_SQLITE_FILE = "oauth.db";

    private static volatile AuthorizationServerConfig instance;

    private final boolean enabled;
    private final String  issuer;
    private final String  sqliteFile;
    private final String  iniFile;            // migration-only; null if unset
    private final int     accessTokenTtlSeconds;
    private final int     refreshTokenTtlSeconds;
    private final int     authCodeTtlSeconds;
    private final int     pruneIntervalSeconds;
    private final boolean allowDynamicRegistration;
    private final int     sessionTtlSeconds;
    private final String  keyId;

    private AuthorizationServerConfig() {
        this.enabled                  = getBool("OAuthAsEnabled", false);

        String iss                    = getString("OAuthAsIssuer");
        if (iss == null || iss.isEmpty())
            iss                       = getString("OAuthAuthorizationServer");
        this.issuer                   = iss == null ? DEFAULT_ISSUER : trimTrailingSlash(iss);

        final String sqlite           = getString("OAuthAsSqliteFile");
        this.sqliteFile               = sqlite == null || sqlite.isEmpty() ? DEFAULT_SQLITE_FILE : sqlite;

        // OAuthAsIniFile: drives the one-shot ini-to-SQLite migration.
        // Null when unset; never defaulted, so the migration trigger
        // requires an explicit setting from the operator.
        final String ini              = getString("OAuthAsIniFile");
        this.iniFile                  = (ini == null || ini.isEmpty()) ? null : ini;

        this.accessTokenTtlSeconds    = getInt("OAuthAccessTokenTtlSeconds",  3_600);
        this.refreshTokenTtlSeconds   = getInt("OAuthRefreshTokenTtlSeconds", 30 * 24 * 3_600);
        this.authCodeTtlSeconds       = getInt("OAuthAuthCodeTtlSeconds",     60);
        this.pruneIntervalSeconds     = getInt("OAuthPruneIntervalSeconds",   900);
        this.allowDynamicRegistration = getBool("OAuthAllowDynamicRegistration", true);
        this.sessionTtlSeconds        = getInt("OAuthSessionTtlSeconds",      1_800);

        final String kid              = getString("OAuthKeyId");
        this.keyId                    = kid == null || kid.isEmpty() ? "kiss-key-1" : kid;
    }

    /**
     * Get the process-wide singleton, loading from {@code application.ini}
     * on first call.
     *
     * @return the AS configuration
     */
    public static AuthorizationServerConfig get() {
        AuthorizationServerConfig local = instance;
        if (local == null) {
            synchronized (AuthorizationServerConfig.class) {
                local = instance;
                if (local == null) {
                    local = new AuthorizationServerConfig();
                    instance = local;
                }
            }
        }
        return local;
    }

    /**
     * Reset the singleton.  Intended for tests; production code should
     * not need to call this.
     */
    public static synchronized void reset() {
        instance = null;
    }

    /**
     * Check whether the AS is enabled.
     * @return true if the AS is enabled (the {@code OAuthAsEnabled} key is true)
     */
    public boolean isEnabled() { return enabled; }

    /**
     * Get the canonical issuer URL announced by the AS.
     * @return the canonical issuer URL announced by the AS
     */
    public String getIssuer() { return issuer; }

    /**
     * Get the path to the SQLite persistent state file.  May be absolute
     * (used verbatim) or relative (resolved against
     * {@code MainServlet.getApplicationPath()}).  Never null --- defaults
     * to {@link #DEFAULT_SQLITE_FILE}.
     *
     * @return path to the SQLite state file
     */
    public String getSqliteFile() { return sqliteFile; }

    /**
     * Get the configured ini-file path used by the one-shot
     * ini-to-SQLite migration on startup.  Returns null when
     * {@code OAuthAsIniFile} is not set, in which case no migration is
     * attempted.
     *
     * @return the ini file path, or null if not configured
     */
    public String getIniFile() { return iniFile; }

    /**
     * Get the lifetime of issued access tokens, in seconds.
     * @return lifetime of issued access tokens, in seconds
     */
    public int getAccessTokenTtlSeconds() { return accessTokenTtlSeconds; }

    /**
     * Get the lifetime of issued refresh tokens, in seconds.
     * @return lifetime of issued refresh tokens, in seconds
     */
    public int getRefreshTokenTtlSeconds() { return refreshTokenTtlSeconds; }

    /**
     * Get the lifetime of authorization codes, in seconds.
     * @return lifetime of authorization codes, in seconds
     */
    public int getAuthCodeTtlSeconds() { return authCodeTtlSeconds; }

    /**
     * Get the minimum seconds between automatic prune sweeps.
     * @return minimum seconds between automatic prune sweeps
     */
    public int getPruneIntervalSeconds() { return pruneIntervalSeconds; }

    /**
     * Check whether dynamic client registration is allowed.
     * @return true if dynamic client registration is allowed (default true)
     */
    public boolean isAllowDynamicRegistration() { return allowDynamicRegistration; }

    /**
     * Get the lifetime of the AS login session cookie, in seconds.
     * @return lifetime of the AS login session cookie, in seconds
     */
    public int getSessionTtlSeconds() { return sessionTtlSeconds; }

    /**
     * Get the key id ({@code kid}) embedded in issued tokens.
     * @return the key id ({@code kid}) embedded in issued tokens
     */
    public String getKeyId() { return keyId; }

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

    private static boolean getBool(String key, boolean defaultValue) {
        String s = getString(key);
        if (s == null)
            return defaultValue;
        s = s.toLowerCase();
        return "true".equals(s) || "yes".equals(s) || "1".equals(s);
    }

    private static String trimTrailingSlash(String s) {
        if (s == null) return null;
        while (s.endsWith("/")) s = s.substring(0, s.length() - 1);
        return s;
    }
}
