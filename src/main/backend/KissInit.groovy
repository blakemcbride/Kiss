import org.kissweb.database.Connection
import org.kissweb.restServer.MainServlet
import org.kissweb.restServer.UserCache

class KissInit {

    /**
     * Configure the system.
     */
    static void init() {
        MainServlet.setConnectionType Connection.ConnectionType.SQLite
        MainServlet.setHost "localhost"
        //MainServlet.setPort 5432                // specify the database port if not the database default
        if (!MainServlet.isSunOS && !MainServlet.isHaiku)
            // Under SunOS (OpenIndiana) and Haiku SQLite doesn't work unless you build a custom jar
            // Other databases work fine but you have to setup their server
            MainServlet.setDatabase "DB.sqlite"   // the name of the database, leave blank for none (and no authentication)
        MainServlet.setUser ""                    // database user (not application user login)
        MainServlet.setPassword ""                // database password (not application user password)
        MainServlet.setMaxWorkerThreads 30        // max number of simultaneous REST services (any additional are put on a queue)
        //MainServlet.setConnectionParameters("")   // used to set additional database connection parameters to the connection string

        UserCache.setInactiveUserMaxSeconds 900   // seconds user is allowed to be idle before auto-logoff

        // Example of how to specify a method that is allowed without authentication
    //    MainServlet.allowWithoutAuthentication("services.MyGroovyService", "addNumbers")

    }

    /**
     * Code to run once the database is open but before the app is running.
     */
    static void init2(Connection db) {
        // If you use db, make sure you commit.
    }
}
