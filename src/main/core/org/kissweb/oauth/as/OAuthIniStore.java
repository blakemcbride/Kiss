package org.kissweb.oauth.as;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.IniFile;
import org.kissweb.restServer.MainServlet;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.StandardCopyOption;

/**
 * The single source of truth for the OAuth authorization server's
 * persistent state: signing keys, registered clients, and refresh
 * tokens.  Wraps an {@link IniFile} with two things it does not provide
 * out of the box:
 * <ol>
 *   <li>Atomic save --- writes go to {@code oauth.ini.tmp} first, then
 *       atomically rename onto {@code oauth.ini}.  A crash mid-write
 *       leaves the previous file intact rather than producing an empty
 *       or half-written one.</li>
 *   <li>An in-memory ``last pruned'' timestamp, used by the stores to
 *       throttle expired-entry cleanup without persisting the timestamp
 *       itself (a restart simply re-runs the prune sooner, which is
 *       harmless).</li>
 * </ol>
 * One instance per JVM.  All access funnels through {@link #get()};
 * every mutator must hold the monitor on this object (callers use
 * <code>synchronized (store) { ... }</code>) so that whole-file save
 * operations see a consistent snapshot.
 */
public final class OAuthIniStore {

    private static final Logger logger = LogManager.getLogger(OAuthIniStore.class);

    private static volatile OAuthIniStore instance;

    private final String   filePath;       // absolute path to oauth.ini
    private final IniFile  ini;
    private volatile long  lastPrunedAtEpochSeconds;  // 0 = never

    private OAuthIniStore(String filePath, IniFile ini) {
        this.filePath = filePath;
        this.ini      = ini;
    }

    /**
     * Get the singleton.  Loads {@code oauth.ini} on first call, or
     * creates an empty in-memory one if the file does not yet exist
     * (subsequent saves materialize it on disk).
     *
     * @return the store
     */
    public static OAuthIniStore get() {
        OAuthIniStore local = instance;
        if (local == null) {
            synchronized (OAuthIniStore.class) {
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
     * Reset the singleton.  Intended for tests; production code should
     * not need this.  The on-disk file is not deleted.
     */
    public static synchronized void reset() {
        instance = null;
    }

    private static OAuthIniStore open() {
        final String relative = AuthorizationServerConfig.get().getIniFile();
        final String absolute = MainServlet.getApplicationPath() + relative;
        try {
            // IniFile.load expects a path relative to applicationPath
            // and prepends it itself --- so pass the relative path.
            IniFile loaded = IniFile.load(relative);
            if (loaded == null) {
                logger.info("OAuth state file " + absolute + " does not exist; starting fresh");
                loaded = new IniFile(absolute);
            } else {
                logger.info("Loaded OAuth state from " + absolute);
            }
            return new OAuthIniStore(absolute, loaded);
        } catch (IOException e) {
            throw new IllegalStateException("Failed to load OAuth state from " + absolute, e);
        }
    }

    /**
     * Direct access to the underlying ini file.  Callers must
     * {@code synchronized (store)} around any read-modify-write
     * sequence and call {@link #save()} after mutations.
     *
     * @return the wrapped IniFile
     */
    public IniFile ini() {
        return ini;
    }

    /**
     * Atomically persist the in-memory state to disk.  Writes to a
     * sibling {@code .tmp} file first, then renames atomically.
     *
     * @throws IOException if the write or rename fails
     */
    public synchronized void save() throws IOException {
        final Path target = new File(filePath).toPath();
        final Path tmp    = new File(filePath + ".tmp").toPath();
        ini.save(tmp.toString());
        try {
            Files.move(tmp, target, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException atomicFailed) {
            // Some filesystems (notably Windows when the target is open
            // for reading) do not support atomic replace.  Fall back to
            // non-atomic --- still safer than writing in place.
            Files.move(tmp, target, StandardCopyOption.REPLACE_EXISTING);
        }
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
