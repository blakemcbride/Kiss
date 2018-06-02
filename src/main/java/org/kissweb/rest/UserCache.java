package org.kissweb.rest;

import java.time.LocalDateTime;
import java.time.temporal.ChronoUnit;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.UUID;

/**
 * Author: Blake McBride
 * Date: 3/23/18
 */
public class UserCache {

    private static final Hashtable<String, UserData> uuidTable = new Hashtable<>();
    private static LocalDateTime lastPurge = LocalDateTime.now();
    private static int inactiveUserMaxSeconds;

    static class UserData {
        String username;
        String password;
        int user_id;
        String uuid;
        LocalDateTime lastAccessDate;

        UserData(String user, String pw) {
            username = user;
            password = pw;
            uuid = UUID.randomUUID().toString();
            lastAccessDate = LocalDateTime.now();
        }
    }

    static UserData newUser(String user, String pw) {
        UserData ud = new UserData(user, pw);
        synchronized (uuidTable) {
            uuidTable.put(ud.uuid, ud);
        }
        return ud;
    }

    static UserData findUser(String uuid) {
        UserData ud;
        synchronized (uuidTable) {
            purgeOld();
            ud = uuidTable.get(uuid);
         }
        return ud;
    }

    static void removeUser(String uuid) {
        synchronized (uuidTable) {
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
        for (UserData ud : uuidTable.values())
            if (ud.lastAccessDate.isBefore(old))
                purge.add(ud.uuid);
        for (String s : purge)
            uuidTable.remove(s);
        lastPurge = LocalDateTime.now();
    }

    public static void setInactiveUserMaxSeconds(int inactiveUserMaxSeconds) {
        UserCache.inactiveUserMaxSeconds = inactiveUserMaxSeconds;
    }
}
