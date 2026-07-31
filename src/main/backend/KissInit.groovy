import org.kissweb.database.Connection
import org.kissweb.restServer.MainServlet
import org.kissweb.restServer.UserCache
import org.kissweb.restServer.UserData
import java.util.function.Consumer

class KissInit {

    /**
     * Configure the system.
     */
    static void init() {

        MainServlet.readIniFile "application.ini", "main"

        // Example of how to specify a method that is allowed without authentication
    //    MainServlet.allowWithoutAuthentication("services.MyGroovyService", "addNumbers")

        // Example of how to set up a global logout handler that runs whenever any
        // user logs out.  This can be used for cleanup tasks such as closing
        // user-specific resources, updating a database logout timestamp, sending
        // notifications, or removing temporary files.
    //    UserCache.setLogoutHandler({ UserData ud ->
    //        // Add any custom cleanup code here
    //    } as Consumer<UserData>)

    }

    /**
     * Code to run once the database is open but before the app is running.
     */
    static void init2(Connection db) {
        // If you use db, make sure you commit.
    }
}
