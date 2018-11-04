import org.kissweb.database.Connection
import org.kissweb.rest.ServiceBase
import org.kissweb.rest.UserCache

class KissInit {

    static void init() {
        ServiceBase.setConnectionType Connection.ConnectionType.PostgreSQL
        ServiceBase.setHost "localhost"
        ServiceBase.setDatabase ""
        ServiceBase.setUser "postgres"            // database user (not application user login)
        ServiceBase.setPassword "postgres"        // database password (not application user password)

        UserCache.setInactiveUserMaxSeconds 900   // seconds user is allowed to be idle before auto-logoff

        ServiceBase.setDebug false                // if true print debug info
    }
}
