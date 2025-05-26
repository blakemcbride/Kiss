package org.kissweb.restServer;

import java.time.LocalDateTime;
import java.util.Hashtable;
import java.util.UUID;

/**
 * Instances of this class keep information about a particular user.
 * <br><br>
 * Author: Blake McBride<br>
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

    /**
     * Get the username.
     *
     * @return the username
     */
    public String getUsername() {
        return username;
    }

    /**
     * Get the password.
     *
     * @return the password
     */
    public String getPassword() {
        return password;
    }

    /**
     * Get the user ID.
     *
     * @return the user ID object
     */
    public Object getUserId() {
        return userId;
    }

    /**
     * Get the UUID.
     *
     * @return the UUID string
     */
    public String getUuid() {
        return uuid;
    }

    /**
     * Get the last access date.
     *
     * @return the last access date
     */
    public LocalDateTime getLastAccessDate() {
        return lastAccessDate;
    }

    /**
     * Set the last access date.
     *
     * @param lastAccessDate the last access date
     */
    public void setLastAccessDate(LocalDateTime lastAccessDate) {
        this.lastAccessDate = lastAccessDate;
    }

    /**
     * Store user data by key.
     *
     * @param key the key
     * @param value the value (null removes the key)
     */
    public void putUserData(String key, Object value) {
        if (value == null)
            data.remove(key);
        else
            data.put(key, value);
    }

    /**
     * Get user data by key.
     *
     * @param key the key
     * @return the value or null if not found
     */
    public Object getUserData(String key) {
        return data.get(key);
    }
}
