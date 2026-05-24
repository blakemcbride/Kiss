package org.kissweb.oauth.as;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.IniFile;

import java.io.IOException;
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
 * Persistent store of {@link RegisteredClient}s.  Each client lives in a
 * separate {@code [client.<id>]} section of {@code oauth.ini}.  The
 * store loads every {@code client.*} section on construction and
 * thereafter maintains an in-memory cache that mirrors the on-disk
 * state; mutations write the cache change and call {@link OAuthIniStore#save()}.
 */
public final class ClientStore {

    private static final Logger logger = LogManager.getLogger(ClientStore.class);

    private static final String SECTION_PREFIX = "client.";

    private static volatile ClientStore instance;

    private final OAuthIniStore                  store;
    private final Map<String, RegisteredClient>  byId = new LinkedHashMap<>();

    private ClientStore(OAuthIniStore store) {
        this.store = store;
        loadAll();
    }

    /**
     * Get the singleton instance.
     * @return the singleton, loading from {@code oauth.ini} on first call
     */
    public static ClientStore get() {
        ClientStore local = instance;
        if (local == null) {
            synchronized (ClientStore.class) {
                local = instance;
                if (local == null) {
                    local = new ClientStore(OAuthIniStore.get());
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
     * @throws IOException if writing {@code oauth.ini} fails
     */
    public synchronized void register(RegisteredClient client) throws IOException {
        byId.put(client.getClientId(), client);
        synchronized (store) {
            writeSection(client);
            store.save();
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
     * @throws IOException if writing {@code oauth.ini} fails
     */
    public synchronized void remove(String clientId) throws IOException {
        if (clientId == null || !byId.containsKey(clientId))
            return;
        byId.remove(clientId);
        synchronized (store) {
            store.ini().removeSection(SECTION_PREFIX + clientId);
            store.save();
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
            final IniFile ini = store.ini();
            for (String section : ini.getSectionNames()) {
                if (section == null || !section.startsWith(SECTION_PREFIX))
                    continue;
                final String clientId = section.substring(SECTION_PREFIX.length());
                final RegisteredClient c = readSection(ini, section, clientId);
                if (c != null)
                    byId.put(clientId, c);
            }
        }
        if (!byId.isEmpty())
            logger.info("Loaded " + byId.size() + " OAuth client(s) from " + AuthorizationServerConfig.get().getIniFile());
    }

    private static RegisteredClient readSection(IniFile ini, String section, String clientId) {
        try {
            final String secretHash = ini.get(section, "client_secret_hash");
            final String name       = orEmpty(ini.get(section, "client_name"));
            final List<String> uris = splitSpaces(ini.get(section, "redirect_uris"));
            final Set<String> scopes = new LinkedHashSet<>(splitSpaces(ini.get(section, "allowed_scopes")));
            final Set<String> grants = new LinkedHashSet<>(splitSpaces(ini.get(section, "allowed_grant_types")));
            final Integer createdInt = ini.getInt(section, "created_at");
            final long createdAt = createdInt == null ? 0L : createdInt.longValue();
            return new RegisteredClient(clientId,
                    (secretHash == null || secretHash.isEmpty()) ? null : secretHash,
                    name, uris, scopes, grants, createdAt);
        } catch (RuntimeException e) {
            logger.warn("Skipping unreadable client section [" + section + "]: " + e.getMessage());
            return null;
        }
    }

    private static void writeSection(RegisteredClient c) {
        final IniFile ini = OAuthIniStore.get().ini();
        final String section = SECTION_PREFIX + c.getClientId();
        ini.put(section, "client_secret_hash",  c.getClientSecretHash() == null ? "" : c.getClientSecretHash());
        ini.put(section, "client_name",         c.getClientName() == null ? "" : c.getClientName());
        ini.put(section, "redirect_uris",       joinSpaces(c.getRedirectUris()));
        ini.put(section, "allowed_scopes",      joinSpaces(c.getAllowedScopes()));
        ini.put(section, "allowed_grant_types", joinSpaces(c.getAllowedGrantTypes()));
        ini.put(section, "created_at",          String.valueOf(c.getCreatedAtEpochSeconds()));
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
