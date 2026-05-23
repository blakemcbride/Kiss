package org.kissweb.oauth.as;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.IniFile;
import org.kissweb.json.JSONObject;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * Persistent store of {@link RefreshToken}s with rotation and
 * stolen-token detection.
 * <br><br>
 * <h2>Rotation</h2>
 * Every successful refresh issues a brand-new refresh token (with a new
 * {@code jti}) in the same family ({@link RefreshToken#getFamilyId}) and
 * marks the old one as rotated.  The client must use the new token from
 * here on; the old one is permanently consumed.
 * <br><br>
 * <h2>Reuse detection</h2>
 * If a refresh token marked as rotated is presented again --- which
 * should never happen for an honest client, because it has already
 * upgraded to the successor --- the store treats this as evidence that
 * the rotated token has been stolen and was replayed.  The framework
 * reacts by {@link #revokeFamily revoking the entire family}, forcing
 * the legitimate user to authenticate again.  This is the OAuth 2.1
 * mechanism described in RFC 6749bis \S{}6.1.
 * <br><br>
 * <h2>Pruning</h2>
 * Refresh tokens expire (default 30 days).  Expired entries are dropped
 * by {@link #pruneExpired}, which the {@link OAuthIniStore#tryStartPrune}
 * gate causes the token and authorize endpoints to call periodically
 * --- no cron required.
 */
public final class RefreshTokenStore {

    private static final Logger logger = LogManager.getLogger(RefreshTokenStore.class);

    private static final String SECTION_PREFIX = "refresh.";

    private static volatile RefreshTokenStore instance;

    private final OAuthIniStore                store;
    private final Map<String, RefreshToken>    byJti = new LinkedHashMap<>();

    private RefreshTokenStore(OAuthIniStore store) {
        this.store = store;
        loadAll();
    }

    /** @return the singleton, loading from {@code oauth.ini} on first call */
    public static RefreshTokenStore get() {
        RefreshTokenStore local = instance;
        if (local == null) {
            synchronized (RefreshTokenStore.class) {
                local = instance;
                if (local == null) {
                    local = new RefreshTokenStore(OAuthIniStore.get());
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
     * Persist a freshly-issued refresh token (called when an
     * authorization code is exchanged for the first time).
     *
     * @param token the new refresh token
     * @throws IOException if writing {@code oauth.ini} fails
     */
    public synchronized void store(RefreshToken token) throws IOException {
        byJti.put(token.getJti(), token);
        synchronized (store) {
            writeSection(token);
            store.save();
        }
    }

    /**
     * Look up a refresh token by its {@code jti} value (which is also
     * the secret string the client holds).
     *
     * @param jti the refresh-token value
     * @return the token, or null if no such token exists in the store
     */
    public synchronized RefreshToken get(String jti) {
        return jti == null ? null : byJti.get(jti);
    }

    /**
     * Consume an old refresh token and store its successor in a single
     * atomic operation.  The old token is marked as rotated with its
     * {@code rotated_to_jti} pointer set to the successor; the new
     * token is added to the same family.  One disk write covers both
     * mutations.
     *
     * @param oldJti    the JTI of the token being consumed
     * @param successor the freshly-minted replacement
     * @throws IOException if writing {@code oauth.ini} fails
     */
    public synchronized void rotate(String oldJti, RefreshToken successor) throws IOException {
        final RefreshToken existing = byJti.get(oldJti);
        if (existing == null)
            throw new IllegalStateException("Refresh token to rotate is unknown: " + oldJti);
        final RefreshToken rotated = existing.withRotation(successor.getJti());
        byJti.put(oldJti, rotated);
        byJti.put(successor.getJti(), successor);
        synchronized (store) {
            writeSection(rotated);
            writeSection(successor);
            store.save();
        }
    }

    /**
     * Revoke every refresh token that belongs to the given family.
     * Called when the framework detects that a rotated token has been
     * replayed (probable theft).
     *
     * @param familyId the family id (from {@link RefreshToken#getFamilyId})
     * @throws IOException if writing {@code oauth.ini} fails
     */
    public synchronized void revokeFamily(String familyId) throws IOException {
        if (familyId == null || familyId.isEmpty())
            return;
        int removed = 0;
        synchronized (store) {
            final IniFile ini = store.ini();
            final List<String> jtiList = new ArrayList<>();
            for (RefreshToken t : byJti.values())
                if (familyId.equals(t.getFamilyId()))
                    jtiList.add(t.getJti());
            for (String jti : jtiList) {
                byJti.remove(jti);
                ini.removeSection(SECTION_PREFIX + jti);
                removed++;
            }
            if (removed > 0)
                store.save();
        }
        logger.warn("Revoked refresh-token family " + familyId + " (" + removed + " token(s))");
    }

    /**
     * Drop expired tokens.  Called periodically when {@link OAuthIniStore#tryStartPrune}
     * returns true.
     *
     * @return the number of tokens removed
     * @throws IOException if writing {@code oauth.ini} fails
     */
    public synchronized int pruneExpired() throws IOException {
        final long now = System.currentTimeMillis() / 1000L;
        int removed = 0;
        synchronized (store) {
            final IniFile ini = store.ini();
            final List<String> jtiList = new ArrayList<>();
            for (RefreshToken t : byJti.values())
                if (t.getExpiresAtEpochSeconds() <= now)
                    jtiList.add(t.getJti());
            for (String jti : jtiList) {
                byJti.remove(jti);
                ini.removeSection(SECTION_PREFIX + jti);
                removed++;
            }
            if (removed > 0)
                store.save();
        }
        if (removed > 0)
            logger.info("Pruned " + removed + " expired refresh token(s)");
        return removed;
    }

    /**
     * Snapshot the currently-stored tokens.  Intended for tests and
     * monitoring; production OAuth flows look up by JTI.
     *
     * @return an immutable copy of every token currently held
     */
    public synchronized Collection<RefreshToken> all() {
        return Collections.unmodifiableList(new ArrayList<>(byJti.values()));
    }

    // ------------------------------------------------------------------

    private void loadAll() {
        synchronized (store) {
            final IniFile ini = store.ini();
            for (String section : ini.getSectionNames()) {
                if (section == null || !section.startsWith(SECTION_PREFIX))
                    continue;
                final String jti = section.substring(SECTION_PREFIX.length());
                final RefreshToken t = readSection(ini, section, jti);
                if (t != null)
                    byJti.put(jti, t);
            }
        }
        if (!byJti.isEmpty())
            logger.info("Loaded " + byJti.size() + " refresh token(s) from " + AuthorizationServerConfig.get().getIniFile());
    }

    private static RefreshToken readSection(IniFile ini, String section, String jti) {
        try {
            final String familyId = ini.get(section, "family_id");
            final String clientId = ini.get(section, "client_id");
            final String userSub  = ini.get(section, "user_sub");
            final JSONObject extras = decodeExtras(ini.get(section, "user_extra_claims"));
            final Set<String> scopes = new LinkedHashSet<>(splitSpaces(ini.get(section, "scopes")));
            final String audience = ini.get(section, "audience");
            final Integer createdInt = ini.getInt(section, "created_at");
            final Integer expiresInt = ini.getInt(section, "expires_at");
            final long createdAt = createdInt == null ? 0L : createdInt.longValue();
            final long expiresAt = expiresInt == null ? 0L : expiresInt.longValue();
            final String rotated = ini.get(section, "rotated_to_jti");
            return new RefreshToken(jti, familyId, clientId, userSub, extras, scopes,
                    (audience == null || audience.isEmpty()) ? null : audience,
                    createdAt, expiresAt,
                    (rotated == null || rotated.isEmpty()) ? null : rotated);
        } catch (RuntimeException e) {
            logger.warn("Skipping unreadable refresh-token section [" + section + "]: " + e.getMessage());
            return null;
        }
    }

    private static void writeSection(RefreshToken t) {
        final IniFile ini = OAuthIniStore.get().ini();
        final String section = SECTION_PREFIX + t.getJti();
        ini.put(section, "family_id",         t.getFamilyId());
        ini.put(section, "client_id",         t.getClientId());
        ini.put(section, "user_sub",          t.getUserSubject());
        ini.put(section, "user_extra_claims", encodeExtras(t.getUserExtraClaims()));
        ini.put(section, "scopes",            joinSpaces(t.getScopes()));
        ini.put(section, "audience",          t.getAudience() == null ? "" : t.getAudience());
        ini.put(section, "created_at",        String.valueOf(t.getCreatedAtEpochSeconds()));
        ini.put(section, "expires_at",        String.valueOf(t.getExpiresAtEpochSeconds()));
        ini.put(section, "rotated_to_jti",    t.getRotatedToJti() == null ? "" : t.getRotatedToJti());
    }

    /** Serialize extra claims as base64(UTF-8 JSON) so they survive INI's single-line value model. */
    private static String encodeExtras(JSONObject claims) {
        if (claims == null || claims.length() == 0)
            return "";
        return Base64.getEncoder().encodeToString(claims.toString().getBytes(StandardCharsets.UTF_8));
    }

    private static JSONObject decodeExtras(String s) {
        if (s == null || s.isEmpty())
            return new JSONObject();
        try {
            return new JSONObject(new String(Base64.getDecoder().decode(s), StandardCharsets.UTF_8));
        } catch (RuntimeException e) {
            return new JSONObject();
        }
    }

    private static List<String> splitSpaces(String s) {
        if (s == null || s.isEmpty())
            return Collections.emptyList();
        return Arrays.asList(s.trim().split("\\s+"));
    }

    private static String joinSpaces(Collection<String> items) {
        if (items == null || items.isEmpty())
            return "";
        return String.join(" ", items);
    }
}
