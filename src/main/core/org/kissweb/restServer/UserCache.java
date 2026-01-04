package org.kissweb.restServer;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

/**
 * This class manages all the users currently logged into the system.
 * <br><br>
 * Author: Blake McBride<br>
 * Date: 3/23/18
 */
public class UserCache {

    private static final Logger logger = LogManager.getLogger(UserCache.class);

    /**
     * Private constructor to prevent instantiation of this utility class.
     */
    private UserCache() {
    }

    private static final Hashtable<String, UserData> uuidTable = new Hashtable<>();
    private static LocalDateTime lastPurge = LocalDateTime.now();
    private static int inactiveUserMaxSeconds;
    private static ScheduledExecutorService scheduler;
    private static Consumer<UserData> globalLogoutHandler;

    /**
     * Create a new user and add it to the cache.
     *
     * @param user the username
     * @param pw the password
     * @param userId the user ID object
     * @return the created UserData object
     */
    static public UserData newUser(String user, String pw, Object userId) {
        UserData ud = new UserData(user, pw, userId);
        uuidTable.put(ud.getUuid(), ud);
        return ud;
    }

    static UserData findUser(String uuid) {
        if (uuid == null  ||  uuid.isEmpty())
            return null;
        purgeOld();
        return uuidTable.get(uuid);
    }

    static void removeUser(String uuid) {
        UserData ud = uuidTable.get(uuid);
        try {
            if (ud != null) {
                logger.info("User " + ud.getUsername() + " is being logged out");
                if (globalLogoutHandler != null) {
                    try {
                        globalLogoutHandler.accept(ud);
                    } catch (Exception e) {
                        logger.error("Error executing global logout handler for " + ud.getUsername(), e);
                    }
                }
            }
        } finally {
            // Always remove user from cache, even if lambda throws exception
            uuidTable.remove(uuid);
        }
    }

    private static void purgeOld() {
        // Don't purge more than once per minute
        if (60 > ChronoUnit.SECONDS.between(lastPurge, LocalDateTime.now()))
            return;
        ArrayList<String> purge = new ArrayList<>();
        LocalDateTime old = LocalDateTime.now();
        int maxSeconds = inactiveUserMaxSeconds;
        if (maxSeconds == 0)
            maxSeconds = 3600;  // no more than 60 minutes
        old = old.minusSeconds(maxSeconds);
        for (UserData ud : uuidTable.values()) {
            if (ud.getLastAccessDate().isBefore(old)) {
                purge.add(ud.getUuid());
            }
        }
        for (String s : purge)
            removeUser(s);
        lastPurge = LocalDateTime.now();
    }

    static void startAutoPurge() {
        scheduler = Executors.newSingleThreadScheduledExecutor();

        scheduler.scheduleAtFixedRate(
                UserCache::purgeOld,
                0,
                60,
                TimeUnit.SECONDS
        );
    }

    static void stopAutoPurge() {
        if (scheduler != null) {
            scheduler.shutdown();
            try {
                if (!scheduler.awaitTermination(5, TimeUnit.SECONDS)) {
                    scheduler.shutdownNow();
                }
            } catch (InterruptedException e) {
                scheduler.shutdownNow();
                Thread.currentThread().interrupt();
            }
        }
    }

    /**
     * Set the maximum seconds a user can be inactive before being purged.
     *
     * @param inactiveUserMaxSeconds the maximum inactive time in seconds
     */
    public static void setInactiveUserMaxSeconds(int inactiveUserMaxSeconds) {
        UserCache.inactiveUserMaxSeconds = inactiveUserMaxSeconds;
    }

    /**
     * Set a global logout handler that will be called whenever any user logs out.
     * This should be called from KissInit.init() to configure custom logout behavior.
     *
     * @param handler the logout handler (Consumer that accepts UserData)
     */
    public static void setLogoutHandler(Consumer<UserData> handler) {
        globalLogoutHandler = handler;
    }
}
