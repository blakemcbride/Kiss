package org.kissweb.oauth.as;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.IniFile;
import org.kissweb.database.Connection;
import org.kissweb.database.Record;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

/**
 * One-shot importer that copies an existing {@code oauth.ini} file into
 * the SQLite database used by the OAuth authorization server, then
 * deletes the ini file so the migration cannot run a second time.
 * <br><br>
 * The migration runs at most once per JVM, at the end of
 * {@link OAuthSqliteStore#get()}'s open sequence.  It does nothing if
 * either of these is not true:
 * <ul>
 *   <li>{@code OAuthAsIniFile} is configured in {@code application.ini}.</li>
 *   <li>The referenced ini file actually exists on disk.</li>
 * </ul>
 * The import is performed in a single transaction.  If any row already
 * exists in the destination tables (unique-key collision), the
 * transaction rolls back and the ini file is left in place --- this
 * surfaces accidental overlap with a partially-populated SQLite
 * database rather than silently choosing between the two copies.
 */
final class OAuthIniMigration {

    private static final Logger logger = LogManager.getLogger(OAuthIniMigration.class);

    private OAuthIniMigration() { }

    /**
     * Run the migration if conditions are met; do nothing otherwise.
     *
     * @param db   open SQLite connection (schema already initialized)
     * @param cfg  the AS configuration
     */
    static void runIfNeeded(Connection db, AuthorizationServerConfig cfg) {
        final String configured = cfg.getIniFile();
        if (configured == null)
            return;
        final IniFile ini;
        try {
            ini = IniFile.load(configured);
        } catch (IOException e) {
            throw new IllegalStateException("OAuth ini migration failed reading " + configured, e);
        }
        if (ini == null)
            return;   // configured but does not exist on disk --- nothing to migrate
        final String absoluteIni = ini.getFilename();

        try {
            int keysImported     = 0;
            int clientsImported  = 0;
            int tokensImported   = 0;

            for (String section : ini.getSectionNames()) {
                if (section == null)
                    continue;
                if ("keys".equals(section)) {
                    if (importKey(db, ini))
                        keysImported++;
                } else if (section.startsWith("client.")) {
                    final String clientId = section.substring("client.".length());
                    importClient(db, ini, section, clientId);
                    clientsImported++;
                } else if (section.startsWith("refresh.")) {
                    final String jti = section.substring("refresh.".length());
                    importRefreshToken(db, ini, section, jti);
                    tokensImported++;
                }
            }
            db.commit();

            // Delete the ini file so the migration cannot run again.
            Files.delete(Path.of(absoluteIni));

            logger.info("Imported OAuth state from " + absoluteIni + ": "
                    + clientsImported + " client(s), "
                    + tokensImported  + " refresh token(s), "
                    + keysImported    + " signing key(s); ini file deleted");
        } catch (Exception e) {
            try {
                db.rollback();
            } catch (Exception ignored) {
            }
            throw new IllegalStateException("OAuth ini migration failed; ini file " + absoluteIni
                    + " left in place", e);
        }
    }

    private static boolean importKey(Connection db, IniFile ini) throws Exception {
        final String kid     = ini.get("keys", "current_kid");
        final String priv    = ini.get("keys", "current_private");
        final String pub     = ini.get("keys", "current_public");
        if (kid == null || priv == null || pub == null)
            return false;
        final long createdAt = parseLong(ini.get("keys", "created_at"), System.currentTimeMillis() / 1000L);
        final Record r = db.newRecord("oauth_keys");
        r.set("kid",         kid);
        r.set("private_key", priv);
        r.set("public_key",  pub);
        r.set("created_at",  createdAt);
        r.addRecord();
        return true;
    }

    private static void importClient(Connection db, IniFile ini, String section, String clientId) throws Exception {
        final Record r = db.newRecord("oauth_clients");
        r.set("client_id",           clientId);
        r.set("client_secret_hash",  orNull(ini.get(section, "client_secret_hash")));
        r.set("client_name",         orEmpty(ini.get(section, "client_name")));
        r.set("redirect_uris",       orEmpty(ini.get(section, "redirect_uris")));
        r.set("allowed_scopes",      orEmpty(ini.get(section, "allowed_scopes")));
        r.set("allowed_grant_types", orEmpty(ini.get(section, "allowed_grant_types")));
        r.set("created_at",          parseLong(ini.get(section, "created_at"), 0L));
        r.addRecord();
    }

    private static void importRefreshToken(Connection db, IniFile ini, String section, String jti) throws Exception {
        final Record r = db.newRecord("oauth_refresh_tokens");
        r.set("jti",               jti);
        r.set("family_id",         orEmpty(ini.get(section, "family_id")));
        r.set("client_id",         orEmpty(ini.get(section, "client_id")));
        r.set("user_sub",          orNull(ini.get(section, "user_sub")));
        r.set("user_extra_claims", orNull(ini.get(section, "user_extra_claims")));
        r.set("scopes",            orEmpty(ini.get(section, "scopes")));
        r.set("audience",          orNull(ini.get(section, "audience")));
        r.set("created_at",        parseLong(ini.get(section, "created_at"), 0L));
        r.set("expires_at",        parseLong(ini.get(section, "expires_at"), 0L));
        r.set("rotated_to_jti",    orNull(ini.get(section, "rotated_to_jti")));
        r.addRecord();
    }

    private static long parseLong(String s, long defaultValue) {
        if (s == null || s.isEmpty())
            return defaultValue;
        try {
            return Long.parseLong(s.trim());
        } catch (NumberFormatException e) {
            return defaultValue;
        }
    }

    private static String orEmpty(String s) {
        return s == null ? "" : s;
    }

    private static String orNull(String s) {
        return (s == null || s.isEmpty()) ? null : s;
    }
}
