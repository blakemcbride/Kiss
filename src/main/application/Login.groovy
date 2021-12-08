import org.kissweb.database.Connection
import org.kissweb.database.Record
import org.kissweb.restServer.UserCache
import org.kissweb.restServer.UserData

/**
 * Author: Blake McBride
 * Date: 12/8/21
 */
class Login {

    /**
     * Validate a user's login name and password.  May also associate user specific data.
     *
     * @param db
     * @param user
     * @param password
     * @return
     */
    public static UserData login(Connection db, String user, String password) {
        Record rec = db.fetchOne("select * from users where user_name = ? and user_password = ? and user_active = 'Y'", user, password)
        if (rec == null)
            return null    //  invalid user
        UserData ud = UserCache.newUser(user, password, (Integer) rec.getInt("user_id"))
//        ud.addUserData("abc", 5)    add any user specific data
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
    public static Boolean checkLogin(Connection db, UserData ud) {
        Record rec = db.fetchOne("select * from users where user_name = ? and user_password = ? and user_active = 'Y'", ud.getUsername(), ud.getPassword())
        return rec != null
    }
}
