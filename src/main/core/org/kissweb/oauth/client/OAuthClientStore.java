package org.kissweb.oauth.client;

import org.kissweb.database.Connection;
import org.kissweb.database.Record;
import org.kissweb.oauth.as.OAuthSqliteStore;

import java.sql.SQLException;

/**
 * Persistent state for the OAuth 2.1 client: per-provider discovery
 * documents, client registrations, and token sets.
 * <br><br>
 * <h2>Shared database</h2>
 * The client does <em>not</em> open its own database.  It persists into
 * the same SQLite file the authorization-server side uses
 * ({@code oauth.db} by default, configured by {@code OAuthAsSqliteFile}),
 * reusing {@link OAuthSqliteStore}'s single connection and lock.  The
 * client's three tables ({@code oauth_client_discovery},
 * {@code oauth_client_registration}, {@code oauth_client_tokens}) live
 * alongside the AS tables in that one file.
 * <br><br>
 * <h2>Lazy creation (inertness)</h2>
 * Nothing is opened or created until the first call that reads or writes
 * client state --- which only happens when an application actually drives
 * a login or reads a token.  An application with no {@code [OAuthClient *]}
 * sections never reaches this class.
 * <br><br>
 * <h2>Concurrency</h2>
 * Because the underlying JDBC {@link Connection} is shared with the AS,
 * every operation here synchronizes on the same monitor the AS uses ---
 * the {@link OAuthSqliteStore} instance --- so concurrent AS and client
 * threads never use the shared connection's {@code PreparedStatement}s at
 * the same time.
 */
public final class OAuthClientStore {

    private static final OAuthClientStore INSTANCE = new OAuthClientStore();

    /**
     * The connection the client tables were last created against.  Guarded
     * by the {@link OAuthSqliteStore} monitor.  Compared by reference so
     * that after {@link OAuthSqliteStore#reset()} (which opens a new
     * connection) the tables are re-created in the new file.
     */
    private Connection schemaInitedFor;

    private OAuthClientStore() { }

    /**
     * Get the process-wide singleton.  Does not open the database ---
     * that happens on the first read/write call.
     *
     * @return the store
     */
    public static OAuthClientStore get() {
        return INSTANCE;
    }

    /**
     * Reset client-store state so the next operation re-creates the client
     * tables.  Intended for tests; does not touch the shared
     * {@link OAuthSqliteStore} (reset that separately to close the file).
     */
    public static synchronized void reset() {
        INSTANCE.schemaInitedFor = null;
    }

    /**
     * Get the absolute on-disk path of the shared SQLite file (for
     * diagnostics).
     *
     * @return the absolute path
     */
    public String getDbPath() {
        return OAuthSqliteStore.get().getDbPath();
    }

    // ==================================================================
    // Discovery
    // ==================================================================

    /**
     * Load the cached discovery document for a provider.
     *
     * @param provider the provider name
     * @return the cached discovery, or null if none is cached
     * @throws SQLException on a database error
     */
    public DiscoveryMetadata loadDiscovery(String provider) throws SQLException {
        final OAuthSqliteStore store = OAuthSqliteStore.get();
        synchronized (store) {
            final Connection db = ensureSchema(store);
            final Record r = fetchByProvider(db, "oauth_client_discovery", provider);
            if (r == null)
                return null;
            return new DiscoveryMetadata(
                    r.getString("issuer"),
                    r.getString("authorization_endpoint"),
                    r.getString("token_endpoint"),
                    r.getString("registration_endpoint"));
        }
    }

    /**
     * Cache (insert or replace) the discovery document for a provider.
     *
     * @param provider the provider name
     * @param d        the discovery document
     * @throws SQLException on a database error
     */
    public void saveDiscovery(String provider, DiscoveryMetadata d) throws SQLException {
        final OAuthSqliteStore store = OAuthSqliteStore.get();
        synchronized (store) {
            final Connection db = ensureSchema(store);
            db.execute("INSERT OR REPLACE INTO oauth_client_discovery " +
                            "(provider, issuer, authorization_endpoint, token_endpoint, registration_endpoint, fetched_at) " +
                            "VALUES (?, ?, ?, ?, ?, ?)",
                    provider, d.getIssuer(), d.getAuthorizationEndpoint(),
                    d.getTokenEndpoint(), d.getRegistrationEndpoint(),
                    System.currentTimeMillis() / 1000L);
            db.commit();
        }
    }

    // ==================================================================
    // Client registration
    // ==================================================================

    /**
     * Load the cached client registration for a provider.
     *
     * @param provider the provider name
     * @return the cached registration, or null if none is cached
     * @throws SQLException on a database error
     */
    public ClientRegistration loadRegistration(String provider) throws SQLException {
        final OAuthSqliteStore store = OAuthSqliteStore.get();
        synchronized (store) {
            final Connection db = ensureSchema(store);
            final Record r = fetchByProvider(db, "oauth_client_registration", provider);
            if (r == null)
                return null;
            return new ClientRegistration(
                    r.getString("client_id"),
                    r.getString("client_secret"),
                    r.getString("redirect_uri"));
        }
    }

    /**
     * Cache (insert or replace) the client registration for a provider.
     *
     * @param provider the provider name
     * @param reg      the registration
     * @throws SQLException on a database error
     */
    public void saveRegistration(String provider, ClientRegistration reg) throws SQLException {
        final OAuthSqliteStore store = OAuthSqliteStore.get();
        synchronized (store) {
            final Connection db = ensureSchema(store);
            db.execute("INSERT OR REPLACE INTO oauth_client_registration " +
                            "(provider, client_id, client_secret, redirect_uri, created_at) " +
                            "VALUES (?, ?, ?, ?, ?)",
                    provider, reg.getClientId(), reg.getClientSecret(), reg.getRedirectUri(),
                    System.currentTimeMillis() / 1000L);
            db.commit();
        }
    }

    // ==================================================================
    // Tokens
    // ==================================================================

    /**
     * Load the current token set for a provider.
     *
     * @param provider the provider name
     * @return the token set, or null if no tokens are held
     * @throws SQLException on a database error
     */
    public TokenSet loadTokens(String provider) throws SQLException {
        final OAuthSqliteStore store = OAuthSqliteStore.get();
        synchronized (store) {
            final Connection db = ensureSchema(store);
            final Record r = fetchByProvider(db, "oauth_client_tokens", provider);
            if (r == null)
                return null;
            final Long expiresAt = r.getLong("expires_at");
            return new TokenSet(
                    r.getString("access_token"),
                    r.getString("refresh_token"),
                    r.getString("token_type"),
                    r.getString("scope"),
                    expiresAt == null ? 0L : expiresAt);
        }
    }

    /**
     * Store (insert or replace) the current token set for a provider.
     *
     * @param provider the provider name
     * @param t        the token set
     * @throws SQLException on a database error
     */
    public void saveTokens(String provider, TokenSet t) throws SQLException {
        final OAuthSqliteStore store = OAuthSqliteStore.get();
        synchronized (store) {
            final Connection db = ensureSchema(store);
            db.execute("INSERT OR REPLACE INTO oauth_client_tokens " +
                            "(provider, access_token, refresh_token, token_type, scope, expires_at, created_at) " +
                            "VALUES (?, ?, ?, ?, ?, ?, ?)",
                    provider, t.getAccessToken(), t.getRefreshToken(), t.getTokenType(),
                    t.getScope(), t.getExpiresAtEpochSeconds(),
                    System.currentTimeMillis() / 1000L);
            db.commit();
        }
    }

    /**
     * Delete any stored tokens for a provider (e.g. on logout or after a
     * refresh token is rejected).
     *
     * @param provider the provider name
     * @throws SQLException on a database error
     */
    public void deleteTokens(String provider) throws SQLException {
        final OAuthSqliteStore store = OAuthSqliteStore.get();
        synchronized (store) {
            final Connection db = ensureSchema(store);
            db.execute("DELETE FROM oauth_client_tokens WHERE provider = ?", provider);
            db.commit();
        }
    }

    // ------------------------------------------------------------------

    /**
     * Ensure the client tables exist in the shared database.  Called under
     * the {@link OAuthSqliteStore} monitor; idempotent and re-runs if the
     * underlying connection has been replaced (e.g. after a test reset).
     */
    private Connection ensureSchema(OAuthSqliteStore store) throws SQLException {
        final Connection db = store.connection();
        if (schemaInitedFor == db)
            return db;
        db.executeImmediate(
                "CREATE TABLE IF NOT EXISTS oauth_client_discovery (" +
                "  provider TEXT PRIMARY KEY," +
                "  issuer TEXT," +
                "  authorization_endpoint TEXT NOT NULL," +
                "  token_endpoint TEXT NOT NULL," +
                "  registration_endpoint TEXT," +
                "  fetched_at INTEGER NOT NULL" +
                ")");
        db.executeImmediate(
                "CREATE TABLE IF NOT EXISTS oauth_client_registration (" +
                "  provider TEXT PRIMARY KEY," +
                "  client_id TEXT NOT NULL," +
                "  client_secret TEXT," +
                "  redirect_uri TEXT NOT NULL," +
                "  created_at INTEGER NOT NULL" +
                ")");
        db.executeImmediate(
                "CREATE TABLE IF NOT EXISTS oauth_client_tokens (" +
                "  provider TEXT PRIMARY KEY," +
                "  access_token TEXT NOT NULL," +
                "  refresh_token TEXT," +
                "  token_type TEXT," +
                "  scope TEXT," +
                "  expires_at INTEGER NOT NULL," +
                "  created_at INTEGER NOT NULL" +
                ")");
        db.commit();
        schemaInitedFor = db;
        return db;
    }

    private static Record fetchByProvider(Connection db, String table, String provider) throws SQLException {
        try {
            return db.fetchOne("SELECT * FROM " + table + " WHERE provider = ?", provider);
        } catch (SQLException e) {
            throw e;
        } catch (Exception e) {
            throw new SQLException("Error reading " + table + " for provider '" + provider + "'", e);
        }
    }
}
