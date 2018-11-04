package org.kissweb.rest;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import org.kissweb.database.Connection;
import org.kissweb.database.Record;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import java.beans.PropertyVetoException;
import java.io.File;
import java.sql.SQLException;
import java.time.LocalDateTime;

/**
 * Author: Blake McBride
 * Date: 5/4/18
 */
public class ServiceBase extends HttpServlet {

    private static Connection.ConnectionType connectionType;
    private static String host;                      // set by KissInit.groovy
    private static String database;                  // set by KissInit.groovy
    private static String user;                      // set by KissInit.groovy
    private static String password;                  // set by KissInit.groovy
    private static String applicationPath;
    private static boolean underIDE = false;
    private static String IDEPath;
    private static ComboPooledDataSource cpds;
    protected static boolean debug = false;          // set by KissInit.groovy
    protected static boolean hasDatabase;            // determined by KissInit.groovy
    protected Connection DB;

    public static String getApplicationPath() {
        return applicationPath;
    }

    static void setApplicationPath(HttpServletRequest request) {
    	String cpath = request.getServletContext().getRealPath("/");
    	System.out.println("* * * Context path = " + cpath);
    	ServiceBase.applicationPath = System.getenv("KISS_DEBUG_ROOT");
    	if (ServiceBase.applicationPath == null  ||  ServiceBase.applicationPath.isEmpty()) {
    		underIDE = false;
    		System.out.println("* * * Is not running under IDE");
    		if (cpath.endsWith("/build/inplaceWebapp/"))
                ServiceBase.applicationPath = cpath + "../../src/main/application/";  // gradle tomcatRun
    		else
                ServiceBase.applicationPath = cpath + (cpath.endsWith("/") ? "" : "/") + "WEB-INF/application/";
    	} else {
    		System.out.println("* * * Is running under IDE");
    		underIDE = true;
        	ServiceBase.applicationPath = ServiceBase.applicationPath.replaceAll("\\\\", "/");
        	ServiceBase.applicationPath = ServiceBase.applicationPath + (ServiceBase.applicationPath.endsWith("/") ? "" : "/") + "src/main/application/";
    	}
    	System.out.println("* * * Application path set to " + ServiceBase.applicationPath);
    }

    public static boolean isUnderIDE() {
        return underIDE;
    }

    protected String login(String user, String password) throws Exception {
        UserCache.UserData ud;
        if (hasDatabase) {
            Record rec = DB.fetchOne("select * from users where user_name = ? and user_password = ? and user_active = 'Y'", user, password);
            if (rec == null)
                throw new Exception("Invalid login.");
            ud = UserCache.newUser(user, password);
            ud.user_id = rec.getInt("user_id");
        } else
            ud = UserCache.newUser(user, password);
        return ud.uuid;
    }

    protected int checkLogin(String uuid) throws Exception {
        UserCache.UserData ud = UserCache.findUser(uuid);
        if (ud == null)
            throw new Exception("Login timed out; please re-login.");
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime timeout = ud.lastAccessDate.plusSeconds(120);  // cache user data for 120 seconds
        if (hasDatabase && now.isAfter(timeout)) {
            Record rec = DB.fetchOne("select * from users where user_name = ? and user_password = ? and user_active = 'Y'", ud.username, ud.password);
            if (rec == null) {
                UserCache.removeUser(uuid);
                throw new Exception("Invalid login.");
            }
        }
        ud.lastAccessDate = LocalDateTime.now();
        return ud.user_id;
    }

    protected void closeSession() {
        java.sql.Connection sconn = null;
        try {
            if (DB != null) {
                sconn = DB.getSQLConnection();
                DB.close();
            }
        } catch (SQLException e) {
            e.printStackTrace();
        } finally {
            DB = null;
        }
        try {
            if (sconn != null)
                sconn.close();
        } catch (SQLException e) {
            e.printStackTrace();
        }
    }

    protected static String getDynamicClassPath() {
        return "";
    }

    protected void makeDatabaseConnection () throws PropertyVetoException, SQLException, ClassNotFoundException {
        if (!hasDatabase) {
            System.err.println("* * * No database configured; bypassing login requirements");
            return;
        }
        if (cpds == null) {
            System.out.println("* * * Attempting to connect to database " + host + ":" + database + ":" + user);
            String cstr = Connection.makeConnectionString(connectionType, host, database, user, password);
            Connection con;
            try {
                con = new Connection(connectionType, cstr);
            } catch (Exception e) {
                System.out.println("* * * Database connection failed (see application/KissInit.groovy)");
                System.out.println("* * * " + e.getMessage());
                throw e;
            }
            con.close();
            System.out.println("* * * Database connection succeeded");

            cpds = new ComboPooledDataSource();

            cpds.setJdbcUrl(cstr);

            cpds.setDriverClass( Connection.getDriverName(connectionType) );

            cpds.setMaxStatements( 180 );
        }
    }

    protected void newDatabaseConnection () throws SQLException {
        if (!hasDatabase)
            return;
        if (debug)
            System.err.println("Previous open database connections = " + cpds.getNumBusyConnections());
        DB = new Connection(cpds.getConnection());
        if (debug)
            System.err.println("New database connection obtained");
    }

    public static Connection.ConnectionType getConnectionType() {
        return connectionType;
    }

    public static void setConnectionType(Connection.ConnectionType connectionType) {
        ServiceBase.connectionType = connectionType;
    }

    public static String getHost() {
        return host;
    }

    public static void setHost(String host) {
        ServiceBase.host = host;
    }

    public static String getDatabase() {
        return database;
    }

    public static void setDatabase(String database) {
        ServiceBase.database = database;
    }

    public static String getUser() {
        return user;
    }

    public static void setUser(String user) {
        ServiceBase.user = user;
    }

    public static String getPassword() {
        return password;
    }

    public static void setPassword(String password) {
        ServiceBase.password = password;
    }

    public static boolean isDebug() {
        return debug;
    }

    public static void setDebug(boolean debug) {
        ServiceBase.debug = debug;
    }
}
