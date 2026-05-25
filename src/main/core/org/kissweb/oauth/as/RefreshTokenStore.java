package org.kissweb.oauth.as;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.database.Connection;
import org.kissweb.database.Record;
import org.kissweb.json.JSONObject;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

/**
 * Persistent store of {@link RefreshToken}s with rotation and
 * stolen-token detection, backed by the {@code oauth_refresh_tokens}
 * table of the OAuth SQLite database.
 * <br><br>
 * Unlike {@link ClientStore} this class does <em>not</em> maintain an
 * in-memory cache of the whole table.  Refresh tokens churn quickly,
 * may accumulate into the thousands between prune sweeps, and are only
 * read on the token endpoint --- a SQLite point query by primary key
 * is faster than walking a HashMap of every token ever issued.
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
 * by {@link #pruneExpired}, which the {@link OAuthSqliteStore#tryStartPrune}
 * gate causes the token and authorize endpoints to call periodically
 * --- no cron required.  The {@code expires_at} column is indexed so
 * the prune sweep is cheap regardless of table size.
 */
public final class RefreshTokenStore {

    private static final Logger logger = LogManager.getLogger(RefreshTokenStore.class);

    private static volatile RefreshTokenStore instance;

    private final OAuthSqliteStore store;

    private RefreshTokenStore(OAuthSqliteStore store) {
        this.store = store;
    }

    /**
     * Get the singleton instance.
     * @return the singleton, opening the OAuth SQLite database on first call
     */
    public static RefreshTokenStore get() {
        RefreshTokenStore local = instance;
        if (local == null) {
            synchronized (RefreshTokenStore.class) {
                local = instance;
                if (local == null) {
                    local = new RefreshTokenStore(OAuthSqliteStore.get());
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
     * @throws IOException if writing to the database fails
     */
    public void store(RefreshToken token) throws IOException {
        synchronized (store) {
            final Connection db = store.connection();
            try {
                insertRow(db, token);
                db.commit();
            } catch (SQLException e) {
                rollbackQuietly(db);
                throw new IOException("Failed to store refresh token " + token.getJti(), e);
            }
        }
    }

    /**
     * Look up a refresh token by its {@code jti} value (which is also
     * the secret string the client holds).
     *
     * @param jti the refresh-token value
     * @return the token, or null if no such token exists in the store
     */
    public RefreshToken get(String jti) {
        if (jti == null)
            return null;
        synchronized (store) {
            final Connection db = store.connection();
            try {
                final Record r = db.fetchOne(
                        "SELECT jti, family_id, client_id, user_sub, user_extra_claims, scopes, " +
                        "audience, created_at, expires_at, rotated_to_jti " +
                        "FROM oauth_refresh_tokens WHERE jti = ?", jti);
                return r == null ? null : fromRow(r);
            } catch (Exception e) {
                throw new IllegalStateException("Failed to look up refresh token " + jti, e);
            }
        }
    }

    /**
     * Consume an old refresh token and store its successor in a single
     * atomic operation.  The old token is marked as rotated with its
     * {@code rotated_to_jti} pointer set to the successor; the new
     * token is added to the same family.  Both writes occur inside one
     * SQLite transaction so the on-disk state is never observed
     * mid-rotation.
     *
     * @param oldJti    the JTI of the token being consumed
     * @param successor the freshly-minted replacement
     * @throws IOException if writing to the database fails
     */
    public void rotate(String oldJti, RefreshToken successor) throws IOException {
        synchronized (store) {
            final Connection db = store.connection();
            try {
                final int updated = updateRotation(db, oldJti, successor.getJti());
                if (updated == 0) {
                    rollbackQuietly(db);
                    throw new IllegalStateException("Refresh token to rotate is unknown: " + oldJti);
                }
                insertRow(db, successor);
                db.commit();
            } catch (SQLException e) {
                rollbackQuietly(db);
                throw new IOException("Failed to rotate refresh token " + oldJti, e);
            }
        }
    }

    /**
     * Revoke every refresh token that belongs to the given family.
     * Called when the framework detects that a rotated token has been
     * replayed (probable theft).
     *
     * @param familyId the family id (from {@link RefreshToken#getFamilyId})
     * @throws IOException if writing to the database fails
     */
    public void revokeFamily(String familyId) throws IOException {
        if (familyId == null || familyId.isEmpty())
            return;
        int removed;
        synchronized (store) {
            final Connection db = store.connection();
            try {
                removed = countByFamily(db, familyId);
                if (removed == 0)
                    return;
                db.execute("DELETE FROM oauth_refresh_tokens WHERE family_id = ?", familyId);
                db.commit();
            } catch (Exception e) {
                rollbackQuietly(db);
                throw new IOException("Failed to revoke refresh-token family " + familyId, e);
            }
        }
        logger.warn("Revoked refresh-token family " + familyId + " (" + removed + " token(s))");
    }

    /**
     * Drop expired tokens.  Called periodically when {@link OAuthSqliteStore#tryStartPrune}
     * returns true.
     *
     * @return the number of tokens removed
     * @throws IOException if writing to the database fails
     */
    public int pruneExpired() throws IOException {
        final long now = System.currentTimeMillis() / 1000L;
        int removed;
        synchronized (store) {
            final Connection db = store.connection();
            try {
                removed = countExpired(db, now);
                if (removed == 0)
                    return 0;
                db.execute("DELETE FROM oauth_refresh_tokens WHERE expires_at <= ?", now);
                db.commit();
            } catch (Exception e) {
                rollbackQuietly(db);
                throw new IOException("Failed to prune expired refresh tokens", e);
            }
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
    public Collection<RefreshToken> all() {
        synchronized (store) {
            final Connection db = store.connection();
            try {
                final List<Record> rows = db.fetchAll(
                        "SELECT jti, family_id, client_id, user_sub, user_extra_claims, scopes, " +
                        "audience, created_at, expires_at, rotated_to_jti " +
                        "FROM oauth_refresh_tokens ORDER BY created_at");
                final List<RefreshToken> out = new ArrayList<>(rows.size());
                for (Record r : rows)
                    out.add(fromRow(r));
                return Collections.unmodifiableList(out);
            } catch (Exception e) {
                throw new IllegalStateException("Failed to enumerate refresh tokens", e);
            }
        }
    }

    // ------------------------------------------------------------------

    private static void insertRow(Connection db, RefreshToken t) throws SQLException {
        final Record r = db.newRecord("oauth_refresh_tokens");
        r.set("jti",               t.getJti());
        r.set("family_id",         t.getFamilyId());
        r.set("client_id",         t.getClientId());
        r.set("user_sub",          t.getUserSubject());
        r.set("user_extra_claims", encodeExtras(t.getUserExtraClaims()));
        r.set("scopes",            joinSpaces(t.getScopes()));
        r.set("audience",          t.getAudience());
        r.set("created_at",        t.getCreatedAtEpochSeconds());
        r.set("expires_at",        t.getExpiresAtEpochSeconds());
        r.set("rotated_to_jti",    t.getRotatedToJti());
        r.addRecord();
    }

    private static int updateRotation(Connection db, String oldJti, String successorJti) throws SQLException {
        // Kiss Connection.execute() does not surface UPDATE row counts,
        // so we probe for the row first.
        try {
            if (!db.exists("SELECT 1 FROM oauth_refresh_tokens WHERE jti = ?", oldJti))
                return 0;
        } catch (Exception e) {
            throw new SQLException("rotation existence check failed", e);
        }
        db.execute("UPDATE oauth_refresh_tokens SET rotated_to_jti = ? WHERE jti = ?",
                successorJti, oldJti);
        return 1;
    }

    private static int countByFamily(Connection db, String familyId) throws Exception {
        return (int) db.fetchCount(
                "SELECT 1 FROM oauth_refresh_tokens WHERE family_id = ?", familyId);
    }

    private static int countExpired(Connection db, long now) throws Exception {
        return (int) db.fetchCount(
                "SELECT 1 FROM oauth_refresh_tokens WHERE expires_at <= ?", now);
    }

    private static RefreshToken fromRow(Record r) throws SQLException {
        final String jti      = r.getString("jti");
        final String familyId = r.getString("family_id");
        final String clientId = r.getString("client_id");
        final String userSub  = r.getString("user_sub");
        final JSONObject extras = decodeExtras(r.getString("user_extra_claims"));
        final Set<String> scopes = new LinkedHashSet<>(splitSpaces(r.getString("scopes")));
        final String audience = r.getString("audience");
        final Long createdL   = r.getLong("created_at");
        final Long expiresL   = r.getLong("expires_at");
        final long createdAt  = createdL == null ? 0L : createdL;
        final long expiresAt  = expiresL == null ? 0L : expiresL;
        final String rotated  = r.getString("rotated_to_jti");
        return new RefreshToken(jti, familyId, clientId, userSub, extras, scopes,
                (audience == null || audience.isEmpty()) ? null : audience,
                createdAt, expiresAt,
                (rotated == null || rotated.isEmpty()) ? null : rotated);
    }

    private static void rollbackQuietly(Connection db) {
        try {
            db.rollback();
        } catch (SQLException ignored) {
        }
    }

    /** Serialize extra claims as base64(UTF-8 JSON); kept symmetric with the legacy ini encoding. */
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
