import org.kissweb.json.JSONObject
import org.kissweb.database.Connection
import org.kissweb.database.Record
import org.kissweb.restServer.ProcessServlet
import org.kissweb.restServer.UserCache
import org.kissweb.restServer.UserData
import org.kissweb.PasswordHash

/**
 * This module handles user authentication.  Passwords are stored as salted PBKDF2 hashes
 * (see {@link org.kissweb.PasswordHash}).  Legacy plain-text and 64-character SHA-256 values
 * are still accepted so pre-existing accounts continue to work until their password is next changed.
 */
class Login {

    /**
     * Verify a candidate password against the stored value, accepting the current PBKDF2 hash format
     * as well as legacy plain-text and SHA-256 values.
     */
    private static boolean passwordMatches(String stored, String entered) {
        if (PasswordHash.isHashed(stored))
            return PasswordHash.verify(entered, stored)
        // Legacy formats (pre-hashing): 64-character SHA-256 hex, or plain text.
        if (stored.length() == 64)
            return stored.equals(entered.sha256())
        return stored.equals(entered)
    }

    /**
     * Validate a user's login name and password.  May also associate user specific data.
     *
     * @param db
     * @param user
     * @param password
     * @param outjson  extra data sent back to the front-end
     * @return
     */
    public static UserData login(Connection db, String user, String password, JSONObject outjson, ProcessServlet servlet) {
        Record rec = db.fetchOne("select user_id, user_password from users where user_name = ? and user_active = 'Y'", user)
        if (rec == null)
            return null    //  invalid user
        String pw = rec.getString("user_password")
        if (pw == null)
            return null;
        if (!passwordMatches(pw, password))
            return null
        UserData ud = UserCache.newUser(user, password, (Integer) rec.getInt("user_id"))
//        ud.putUserData("abc", 5)    add any user specific data to save on the back-end
//        outjson.put("user_type", "xxx")    data sent back to the front-end
        return ud
    }

    /**
     * Re-validate a user.
     *
     * Users get re-validated about once every two minutes.  This assures that a user is logged out if their login
     * gets disabled while they're in the system.
     *
     * @param db
     * @param ud
     * @return true if the user is still valid, false if not
     */
    public static Boolean checkLogin(Connection db, UserData ud, ProcessServlet servlet) {
        Record rec = db.fetchOne("select user_password from users where user_name = ? and user_active = 'Y'", ud.getUsername())
        if (rec == null)
            return false    //  invalid user
        String pw = rec.getString("user_password")
        if (pw == null)
            return false;
        return passwordMatches(pw, ud.getPassword())
    }
}
