package org.kissweb.restServer;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.kissweb.Cron;
import org.kissweb.database.Connection;

import javax.servlet.ServletContextEvent;
import javax.servlet.ServletOutputStream;
import javax.servlet.annotation.MultipartConfig;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.beans.PropertyVetoException;
import java.io.File;
import java.io.IOException;
import java.sql.SQLException;
import java.util.HashSet;
import java.util.Hashtable;
import java.util.Set;


/**
 * Author: Blake McBride
 * Date: 5/4/18
 */
@WebServlet(urlPatterns="/rest", asyncSupported = true)
@MultipartConfig
public class MainServlet extends HttpServlet {

    private static final Logger logger = Logger.getLogger(MainServlet.class);
    private static Connection.ConnectionType connectionType;
    private static String host;                      // set by KissInit.groovy
    private static String database;                  // set by KissInit.groovy
    private static String user;                      // set by KissInit.groovy
    private static String password;                  // set by KissInit.groovy
    private static String applicationPath;           // where the application files are
    private static String rootPath;                  // the root of the entire application
    private static boolean underIDE = false;
    private static ComboPooledDataSource cpds;
    private static boolean hasDatabase;              // determined by KissInit.groovy
    private static int maxWorkerThreads;
    private static Cron cron;
    private static final Set<String> allowedWithoutAuthentication = new HashSet<>();
    private static final Hashtable<String,Object> environment = new Hashtable<>();  // general application-specific values

    private QueueManager queueManager;

    /**
     * Returns <code>true</code> if in development mode.  Returns <code>false</code> if in production.
     */
    public static boolean isUnderIDE() {
        return underIDE;
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        if (queueManager == null)
            queueManager = new org.kissweb.restServer.QueueManager(maxWorkerThreads);
        ServletOutputStream out = response.getOutputStream();
        response.setStatus(202);
        out.flush();  //  this causes the first response

        queueManager.add(request, response, out);
    }

    /**
     * Return the root of the application back-end source code.
     * This takes into account where the source code is.
     */
    public static String getApplicationPath() {
        return applicationPath;
    }

    /**
     * Get the root path of the entire application.
     */
    public static String getRootPath() {
        return rootPath;
    }

    /**
     * Manually set root path if needed.
     * This is needed when Kiss is not used to process the REST services.
     * This rootPath is used by the reporting facility.
     *
     * @param path
     */
    public static void setRootPath(String path) {
        rootPath = path;
    }

    /**
     * Set the application path when it is determined outside of Kiss.
     * This is needed when an application is using the Kiss library but not its REST server.
     *
     * @param path
     */
    public static void setApplicationPath(String path) {
        applicationPath = path;
    }

    /**
     * This is a very important method.  It tells the difference between running in some sort of development
     * environment or a production environment.  If running in a production environment, you want to get the
     * source files that came with the distribution.  However, if you're in a development environment, you want to get
     * the development source files so that you can develop while the system is running.
     * <br><br>
     * This method sets the <code>rootPath</code> and <code>applicationPath</code>.  <code>rootPath</code> is the root
     * of the application.  <code>applicationPath</code> is the root of the application files.
     *
     * @param _rootPath
     */
    static void setApplicationPathInternal(String _rootPath) {
        Level level = logger.getLevel();
        logger.setLevel(Level.ALL);
        rootPath = _rootPath;
        logger.info("* * * Root path = " + rootPath);
        applicationPath = System.getenv("KISS_ROOT");
        if (applicationPath == null || applicationPath.isEmpty()) {
            if ((new File(rootPath + "../../../src/main/backend/" + "KissInit.groovy")).exists()) {
                applicationPath = rootPath + "../../../src/main/backend/";
                underIDE = true;
            } else if ((new File(rootPath + "../../src/main/backend/" + "KissInit.groovy")).exists()) {
                applicationPath = rootPath + "../../src/main/backend/";
                underIDE = true;
            } else if ((new File(rootPath + "../../../../src/main/backend/" + "KissInit.groovy")).exists()) {
                applicationPath = rootPath + "../../../../src/main/backend/";
                underIDE = true;
            } else {
                applicationPath = rootPath + (rootPath.endsWith("/") ? "" : "/") + "WEB-INF/backend/";
                underIDE = false;
            }
        } else {
            underIDE = true;
            applicationPath = applicationPath.replaceAll("\\\\", "/");
            applicationPath = applicationPath + (applicationPath.endsWith("/") ? "" : "/") + "src/main/backend/";
        }
        try {
            applicationPath = (new File(applicationPath)).getCanonicalPath() + "/";
        } catch (IOException e) {
            // ignore
        }
        logger.info(underIDE ? "* * * Is running with source" : "* * * Is not running with source");
        logger.info("* * * Application path set to " + applicationPath);
        logger.setLevel(level);
    }

    /**
     * Initialize the system.
     *
     * @see StartupListener#contextInitialized(ServletContextEvent)
     */
    static void initializeSystem(String path) {
        Level level = logger.getLevel();
        logger.setLevel(Level.ALL);
        setApplicationPathInternal(path);
        org.kissweb.restServer.ProcessServlet.ExecutionReturn res = (new GroovyService()).internalGroovy(null, null, null, "KissInit", "init");
        if (res == ProcessServlet.ExecutionReturn.Success) {
            hasDatabase = database != null  &&  !database.isEmpty();
            if (hasDatabase) {
                try {
                    makeDatabaseConnection();
                } catch (PropertyVetoException | SQLException | ClassNotFoundException e) {
                    logger.error("", e);
                    System.exit(-1);
                }
                logger.info("* * * Database " + database + " opened successfully");
            }
            else
                logger.info("* * * No database configured; bypassing login requirements");
        } else
            logger.error("* * * Error executing KissInit.groovy");

        try {
            cron = new Cron(MainServlet.getApplicationPath() + "CronTasks/crontab",
                    MainServlet::getConnection,
                    MainServlet::success,
                    MainServlet::failure);
        } catch (IOException e) {
            logger.error(e);
        }

        logger.setLevel(level);
    }

    private static Connection getConnection() {
        if (!hasDatabase)
            return null;
        Connection db = null;
        try {
            java.sql.Connection c = MainServlet.getCpds().getConnection();
            c.setAutoCommit(false);
            db = new Connection(c);
        } catch (SQLException e) {
            logger.error("", e);
        }
        return db;
    }

    private static void success(Object p) {
        Connection db = (Connection) p;
        try {
            db.commit();
            java.sql.Connection sconn = null;
            try {
                sconn = db.getSQLConnection();
                db.close();
            } catch (SQLException e) {
                logger.error("", e);
            }
            try {
                if (sconn != null)
                    sconn.close();
            } catch (SQLException e) {
                logger.error("", e);
            }
        } catch (SQLException e) {
            logger.error("", e);
        }
    }

    private static void failure(Object p) {
        Connection db = (Connection) p;
        try {
            db.rollback();
            java.sql.Connection sconn = null;
            try {
                sconn = db.getSQLConnection();
                db.close();
            } catch (SQLException e) {
                logger.error("", e);
            }
            try {
                if (sconn != null)
                    sconn.close();
            } catch (SQLException e) {
                logger.error("", e);
            }
        } catch (SQLException e) {
            logger.error("", e);
        }
    }

    static void stopCron() {
        cron.cancel();
    }

    static void makeDatabaseConnection() throws PropertyVetoException, SQLException, ClassNotFoundException {
        Level level = logger.getLevel();
        logger.setLevel(Level.ALL);
        if (!hasDatabase) {
            logger.info("* * * No database configured; bypassing login requirements");
            return;
        }
        if (cpds == null) {
            logger.info("* * * Attempting to connect to database " + host + ":" + database + ":" + user);
            if (connectionType == Connection.ConnectionType.SQLite  &&  database != null  &&  !database.isEmpty() &&  database.charAt(0) != '/')
                database = applicationPath + database;
            String cstr = Connection.makeConnectionString(connectionType, host, database, user, password);
            Connection con;
            try {
                con = new Connection(connectionType, cstr);
            } catch (Exception e) {
                logger.error("* * * Database connection failed (see backend/KissInit.groovy) " + e.getMessage());
                throw e;
            }
            con.close();
            logger.info("* * * Database connection succeeded");

            cpds = new ComboPooledDataSource();

            cpds.setJdbcUrl(cstr);

            cpds.setDriverClass(Connection.getDriverName(connectionType));

            cpds.setMaxStatements(180);
        }
        logger.setLevel(level);
    }

    static String getDynamicClassPath() {
        return "";
    }

    public static void setConnectionType(Connection.ConnectionType connectionType) {
        MainServlet.connectionType = connectionType;
    }

    public static Connection.ConnectionType getConnectionType() {
        return connectionType;
    }

    public static void setHost(String hostp) {
        host = hostp;
    }

    public static String getHost() {
        return host;
    }

    public static String getUser() {
        return user;
    }

    public static void setUser(String userp) {
        user = userp;
    }

    public static String getPassword() {
        return password;
    }

    public static void setPassword(String passwordp) {
        password = passwordp;
    }

    public static String getDatabase() {
        return database;
    }

    public static void setDatabase(String databasep) {
        database = databasep;
    }

    public static void setMaxWorkerThreads(int maxThreads) {
        maxWorkerThreads = maxThreads;
    }

    public static boolean hasDatabase() {
        return hasDatabase;
    }

    static ComboPooledDataSource getCpds() {
        return cpds;
    }

    /**
     * Be default, all web service methods are authenticated.
     * This method is used to declare specific rest service methods that should not be authenticated.
     * This allows specific web service methods to be executed prior to logging in.
     *
     * @param className
     * @param methodName
     */
    public static void allowWithoutAuthentication(String className, String methodName) {
        className = className.replaceAll("\\.", "/");
        allowedWithoutAuthentication.add(className + ":" + methodName);
    }

    static boolean shouldAllowWithoutAuthentication(String className, String methodName) {
        return allowedWithoutAuthentication.contains(className + ":" + methodName);
    }

    /**
     * Add an application-specific key / value pair.
     *
     * @param key
     * @param value
     *
     * @see #getEnvironment(String)
     */
    public static void putEnvironment(String key, Object value) {
        environment.put(key, value);
    }

    /**
     * Retrieve an application-specific value previously set with <code>putEnvironment</code>
     *
     * @param key
     * @return
     *
     * @see #putEnvironment(String, Object)
     */
    public static Object getEnvironment(String key) {
        return environment.get(key);
    }

}
