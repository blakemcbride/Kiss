package org.kissweb.oauth.as;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.database.Connection;
import org.kissweb.database.Record;

import java.io.IOException;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Persistent store of {@link RegisteredClient}s, backed by the
 * {@code oauth_clients} table in the OAuth SQLite database.  The full
 * client set is read into memory on construction and thereafter
 * maintained as a cache that mirrors the on-disk state; mutations
 * update the cache and execute a single {@code INSERT}/{@code DELETE}
 * against the database.
 * <br><br>
 * The in-memory cache is retained because the client set is small
 * (typically a handful of entries) and is read on every token
 * validation --- pulling it out of memory keeps that hot path
 * allocation-free.
 */
public final class ClientStore {

    private static final Logger logger = LogManager.getLogger(ClientStore.class);

    private static volatile ClientStore instance;

    private final OAuthSqliteStore               store;
    private final Map<String, RegisteredClient>  byId = new LinkedHashMap<>();

    private ClientStore(OAuthSqliteStore store) {
        this.store = store;
        loadAll();
    }

    /**
     * Get the singleton instance.
     * @return the singleton, loading from the OAuth SQLite database on first call
     */
    public static ClientStore get() {
        ClientStore local = instance;
        if (local == null) {
            synchronized (ClientStore.class) {
                local = instance;
                if (local == null) {
                    local = new ClientStore(OAuthSqliteStore.get());
                    instance = local;
                }
            }
        }
        return local;
    }

    /** Reset the singleton.  Intended for tests. */
    public static synchronized void reset() {
        instance = null;
    }

    /**
     * Persist a newly-registered client.
     *
     * @param client the client to register
     * @throws IOException if writing to the database fails
     */
    public synchronized void register(RegisteredClient client) throws IOException {
        synchronized (store) {
            final Connection db = store.connection();
            try {
                final Record r = db.newRecord("oauth_clients");
                r.set("client_id",           client.getClientId());
                r.set("client_secret_hash",  client.getClientSecretHash());
                r.set("client_name",         orEmpty(client.getClientName()));
                r.set("redirect_uris",       joinSpaces(client.getRedirectUris()));
                r.set("allowed_scopes",      joinSpaces(client.getAllowedScopes()));
                r.set("allowed_grant_types", joinSpaces(client.getAllowedGrantTypes()));
                r.set("created_at",          client.getCreatedAtEpochSeconds());
                r.addRecord();
                db.commit();
            } catch (SQLException e) {
                rollbackQuietly(db);
                throw new IOException("Failed to register OAuth client " + client.getClientId(), e);
            }
            byId.put(client.getClientId(), client);
        }
        logger.info("Registered OAuth client " + client.getClientId() + " (" + client.getClientName() + ")");
    }

    /**
     * Look up a client by id.
     *
     * @param clientId the client identifier
     * @return the client, or null if no such client is registered
     */
    public synchronized RegisteredClient get(String clientId) {
        return clientId == null ? null : byId.get(clientId);
    }

    /**
     * Remove a client from the registry.  Tokens already issued to the
     * client remain valid until their natural expiry --- this does not
     * revoke them.
     *
     * @param clientId the client to remove
     * @throws IOException if writing to the database fails
     */
    public synchronized void remove(String clientId) throws IOException {
        if (clientId == null || !byId.containsKey(clientId))
            return;
        synchronized (store) {
            final Connection db = store.connection();
            try {
                db.execute("DELETE FROM oauth_clients WHERE client_id = ?", clientId);
                db.commit();
            } catch (SQLException e) {
                rollbackQuietly(db);
                throw new IOException("Failed to remove OAuth client " + clientId, e);
            }
            byId.remove(clientId);
        }
        logger.info("Unregistered OAuth client " + clientId);
    }

    /**
     * Snapshot the current set of registered clients.
     *
     * @return an immutable copy of all currently-registered clients
     */
    public synchronized Collection<RegisteredClient> all() {
        return Collections.unmodifiableList(new ArrayList<>(byId.values()));
    }

    // ------------------------------------------------------------------

    private void loadAll() {
        synchronized (store) {
            final Connection db = store.connection();
            try {
                final List<Record> rows = db.fetchAll(
                        "SELECT client_id, client_secret_hash, client_name, redirect_uris, " +
                        "allowed_scopes, allowed_grant_types, created_at " +
                        "FROM oauth_clients ORDER BY created_at");
                for (Record r : rows) {
                    final RegisteredClient c = fromRow(r);
                    if (c != null)
                        byId.put(c.getClientId(), c);
                }
            } catch (Exception e) {
                throw new IllegalStateException("Failed to load OAuth clients from SQLite", e);
            }
        }
        if (!byId.isEmpty())
            logger.info("Loaded " + byId.size() + " OAuth client(s) from " + store.getDbPath());
    }

    private static RegisteredClient fromRow(Record r) {
        try {
            final String clientId   = r.getString("client_id");
            final String secretHash = r.getString("client_secret_hash");
            final String name       = orEmpty(r.getString("client_name"));
            final List<String> uris = splitSpaces(r.getString("redirect_uris"));
            final Set<String> scopes = new LinkedHashSet<>(splitSpaces(r.getString("allowed_scopes")));
            final Set<String> grants = new LinkedHashSet<>(splitSpaces(r.getString("allowed_grant_types")));
            final Long created      = r.getLong("created_at");
            final long createdAt    = created == null ? 0L : created;
            return new RegisteredClient(clientId,
                    (secretHash == null || secretHash.isEmpty()) ? null : secretHash,
                    name, uris, scopes, grants, createdAt);
        } catch (Exception e) {
            logger.warn("Skipping unreadable client row: " + e.getMessage());
            return null;
        }
    }

    private static void rollbackQuietly(Connection db) {
        try {
            db.rollback();
        } catch (SQLException ignored) {
        }
    }

    private static List<String> splitSpaces(String s) {
        if (s == null || s.isEmpty())
            return Collections.emptyList();
        final String[] parts = s.trim().split("\\s+");
        final List<String> out = new ArrayList<>(parts.length);
        out.addAll(Arrays.asList(parts));
        return out;
    }

    private static String joinSpaces(Collection<String> items) {
        if (items == null || items.isEmpty())
            return "";
        return String.join(" ", items);
    }

    private static String orEmpty(String s) {
        return s == null ? "" : s;
    }
}
