package org.kissweb.oauth.as;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.database.Connection;
import org.kissweb.restServer.MainServlet;

import java.io.File;
import java.sql.SQLException;

/**
 * The single source of truth for the OAuth authorization server's
 * persistent state: signing keys, registered clients, and refresh
 * tokens.  Wraps a Kiss {@link Connection} opened against a SQLite
 * file that is intentionally separate from the application's main
 * database --- the AS persists itself in its own file so it has no
 * dependency on, and shares no connection pool with, the application
 * database.
 * <br><br>
 * <h2>Path resolution</h2>
 * The configured {@code OAuthAsSqliteFile} value may be relative
 * (resolved against {@link MainServlet#getApplicationPath()}, the
 * historical default colocated with {@code application.ini}) or
 * absolute (used verbatim --- the recommended choice for production,
 * so that a WAR redeploy cannot touch the runtime state file).
 * <br><br>
 * <h2>Lifecycle</h2>
 * On the first call to {@link #get()} per JVM:
 * <ol>
 *   <li>The path is resolved and any missing parent directories are
 *       created.</li>
 *   <li>A JDBC connection is opened against the SQLite file.  If the
 *       file does not exist, the driver creates it.</li>
 *   <li>The schema is materialized via {@code CREATE TABLE IF NOT
 *       EXISTS} on every open --- safe both for a fresh database and
 *       an existing one with the current schema.</li>
 *   <li>The legacy ini import runs <em>only when the SQLite file did
 *       not already exist</em> (i.e. it was just created).  In that
 *       case, if {@code OAuthAsIniFile} is configured and points at an
 *       existing file, {@link OAuthIniMigration#runIfNeeded} imports
 *       that file into SQLite and deletes it.  Once the SQLite store
 *       exists it is the sole source of truth and {@code OAuthAsIniFile}
 *       is ignored entirely.</li>
 * </ol>
 * <h2>Concurrency</h2>
 * One instance per JVM.  All access funnels through {@link #get()};
 * every read-modify-write sequence must hold the monitor on this
 * object (callers use <code>synchronized (store) { ... }</code>) so
 * that concurrent threads share a consistent view of the underlying
 * JDBC connection.  SQLite itself serializes writers internally; the
 * synchronization here protects the shared JDBC {@link Connection}
 * object from concurrent {@code PreparedStatement} use.
 */
public final class OAuthSqliteStore {

    private static final Logger logger = LogManager.getLogger(OAuthSqliteStore.class);

    private static volatile OAuthSqliteStore instance;

    private final String      dbPath;        // absolute path to oauth.db
    private final Connection  db;
    private volatile long     lastPrunedAtEpochSeconds;  // 0 = never

    private OAuthSqliteStore(String dbPath, Connection db) {
        this.dbPath = dbPath;
        this.db     = db;
    }

    /**
     * Get the singleton, opening (and if necessary creating + initializing)
     * the underlying SQLite database on the first call.
     *
     * @return the store
     */
    public static OAuthSqliteStore get() {
        OAuthSqliteStore local = instance;
        if (local == null) {
            synchronized (OAuthSqliteStore.class) {
                local = instance;
                if (local == null) {
                    local = open();
                    instance = local;
                }
            }
        }
        return local;
    }

    /**
     * Reset the singleton.  Intended for tests; closes the underlying
     * JDBC connection so the next {@link #get()} re-opens it.  Does not
     * delete the database file.
     */
    public static synchronized void reset() {
        final OAuthSqliteStore local = instance;
        if (local != null) {
            try {
                local.db.close();
            } catch (SQLException ignored) {
            }
            instance = null;
        }
    }

    private static OAuthSqliteStore open() {
        final AuthorizationServerConfig cfg = AuthorizationServerConfig.get();
        final String configured = cfg.getSqliteFile();
        final String absolutePath = new File(configured).isAbsolute()
                ? configured
                : MainServlet.getApplicationPath() + configured;

        final File f = new File(absolutePath);
        final File parent = f.getParentFile();
        if (parent != null && !parent.exists() && !parent.mkdirs())
            throw new IllegalStateException("Cannot create directory for OAuth state file: " + parent);

        final boolean preexisting = f.exists();

        Connection conn = null;
        try {
            conn = new Connection(Connection.ConnectionType.SQLite, "jdbc:sqlite:" + absolutePath);
            initSchema(conn);
            if (preexisting)
                logger.info("Opened OAuth state SQLite database at " + absolutePath);
            else
                logger.info("Created and initialized OAuth state SQLite database at " + absolutePath);

            // One-shot import from the legacy ini file --- ONLY when the
            // SQLite database did not already exist.  Once the SQLite store
            // is present we trust it as the sole source of truth and ignore
            // OAuthAsIniFile entirely; re-running the import against a
            // populated database would collide on its unique keys, fail the
            // open, and take the whole authorization server down.
            if (!preexisting)
                OAuthIniMigration.runIfNeeded(conn, cfg);

            return new OAuthSqliteStore(absolutePath, conn);
        } catch (Exception e) {
            if (conn != null) {
                try {
                    conn.close();
                } catch (SQLException ignored) {
                }
            }
            throw new IllegalStateException("Failed to open OAuth state SQLite database at " + absolutePath, e);
        }
    }

    private static void initSchema(Connection conn) throws SQLException {
        conn.executeImmediate(
                "CREATE TABLE IF NOT EXISTS oauth_keys (" +
                "  kid TEXT PRIMARY KEY," +
                "  private_key TEXT NOT NULL," +
                "  public_key TEXT NOT NULL," +
                "  created_at INTEGER NOT NULL" +
                ")");
        conn.executeImmediate(
                "CREATE TABLE IF NOT EXISTS oauth_clients (" +
                "  client_id TEXT PRIMARY KEY," +
                "  client_secret_hash TEXT," +
                "  client_name TEXT," +
                "  redirect_uris TEXT," +
                "  allowed_scopes TEXT," +
                "  allowed_grant_types TEXT," +
                "  created_at INTEGER NOT NULL" +
                ")");
        conn.executeImmediate(
                "CREATE TABLE IF NOT EXISTS oauth_refresh_tokens (" +
                "  jti TEXT PRIMARY KEY," +
                "  family_id TEXT NOT NULL," +
                "  client_id TEXT NOT NULL," +
                "  user_sub TEXT," +
                "  user_extra_claims TEXT," +
                "  scopes TEXT," +
                "  audience TEXT," +
                "  created_at INTEGER NOT NULL," +
                "  expires_at INTEGER NOT NULL," +
                "  rotated_to_jti TEXT" +
                ")");
        conn.executeImmediate(
                "CREATE INDEX IF NOT EXISTS idx_oauth_refresh_family " +
                "ON oauth_refresh_tokens (family_id)");
        conn.executeImmediate(
                "CREATE INDEX IF NOT EXISTS idx_oauth_refresh_expires " +
                "ON oauth_refresh_tokens (expires_at)");
        conn.executeImmediate(
                "CREATE TABLE IF NOT EXISTS oauth_meta (" +
                "  meta_key TEXT PRIMARY KEY," +
                "  meta_value TEXT NOT NULL" +
                ")");
        conn.execute(
                "INSERT OR IGNORE INTO oauth_meta (meta_key, meta_value) VALUES (?, ?)",
                "schema_version", "1");
        conn.commit();
    }

    /**
     * Direct access to the underlying Kiss {@link Connection}.  Callers
     * must {@code synchronized (store)} around any read-modify-write
     * sequence and call {@link Connection#commit()} after mutations.
     *
     * @return the wrapped connection
     */
    public Connection connection() {
        return db;
    }

    /**
     * Get the absolute path to the on-disk SQLite file.  Intended for
     * logging and diagnostics.
     *
     * @return absolute path
     */
    public String getDbPath() {
        return dbPath;
    }

    /**
     * Check whether the configured prune interval has elapsed since the
     * last successful prune, and atomically record ``now'' as the new
     * last-prune time if so.  Returns {@code true} on the call that
     * wins the check, {@code false} on every subsequent call inside the
     * interval.
     * <br><br>
     * The last-pruned timestamp is kept in memory only.  A JVM restart
     * resets it, which means the first OAuth call after startup always
     * triggers a prune --- intentionally, as a cheap way to drop any
     * expired tokens accumulated while the process was down.
     *
     * @return true if this call should perform the prune sweep
     */
    public synchronized boolean tryStartPrune() {
        final int interval = AuthorizationServerConfig.get().getPruneIntervalSeconds();
        final long now = System.currentTimeMillis() / 1000L;
        if (now - lastPrunedAtEpochSeconds < interval)
            return false;
        lastPrunedAtEpochSeconds = now;
        return true;
    }
}
