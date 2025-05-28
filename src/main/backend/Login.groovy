import org.kissweb.json.JSONObject
import org.kissweb.database.Connection
import org.kissweb.database.Record
import org.kissweb.restServer.ProcessServlet
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
     * @param outjson  extra data sent back to the front-end
     * @return
     */
    public static UserData login(Connection db, String user, String password, JSONObject outjson, ProcessServlet servlet) {
        Record rec = db.fetchOne("select * from users where user_name = ? and user_password = ? and user_active = 'Y'", user, password)
        if (rec == null)
            return null    //  invalid user
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
        Record rec = db.fetchOne("select * from users where user_name = ? and user_password = ? and user_active = 'Y'", ud.getUsername(), ud.getPassword())
        return rec != null
    }
}
