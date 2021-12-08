package org.kissweb.restServer;

import java.time.LocalDateTime;
import java.util.Hashtable;
import java.util.UUID;

/**
 * Instances of this class keep information about a particular user.
 *
 * Author: Blake McBride
 * Date: 12/8/21
 */
public class UserData {

    private final String username;
    private final String password;
    private final Object userId;
    private final String uuid;
    private LocalDateTime lastAccessDate;
    private final Hashtable<String,Object> data = new Hashtable<>();

    UserData(String user, String pw, Object userId) {
        username = user;
        password = pw;
        this.userId = userId;
        uuid = UUID.randomUUID().toString();
        lastAccessDate = LocalDateTime.now();
    }

    public String getUsername() {
        return username;
    }

    public String getPassword() {
        return password;
    }

    public Object getUserId() {
        return userId;
    }

    public String getUuid() {
        return uuid;
    }

    public LocalDateTime getLastAccessDate() {
        return lastAccessDate;
    }

    public void setLastAccessDate(LocalDateTime lastAccessDate) {
        this.lastAccessDate = lastAccessDate;
    }

    public void putUserData(String key, Object value) {
        if (value == null)
            data.remove(key);
        else
            data.put(key, value);
    }

    public Object getUserData(String key) {
        return data.get(key);
    }
}
