package org.kissweb.restServer;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.kissweb.Cron;
import org.kissweb.database.Connection;

import jakarta.servlet.ServletContextEvent;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.annotation.MultipartConfig;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
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
 *
 * This is the main entry point for asynchronous server requests used by Kiss.
 */
@WebServlet(urlPatterns="/rest", asyncSupported = true)
@MultipartConfig
public class MainServlet extends HttpServlet {

    private static final Logger logger = Logger.getLogger(MainServlet.class);
    private static Connection.ConnectionType connectionType;
    private static String host;                      // set by KissInit.groovy
    private static Integer port;                     // optionally set by KissInit.groovy
    private static String connectionParameters;      // optional string to append to the database connection string
    private static String database;                  // set by KissInit.groovy
    private static String user;                      // database username, set by KissInit.groovy
    private static String password;                  // database password, set by KissInit.groovy
    private static String applicationPath;           // where the application files are
    private static String rootPath;                  // the root of the entire application
    private static boolean underIDE = false;
    private static ComboPooledDataSource cpds;
    private static boolean hasDatabase;              // determined by KissInit.groovy
    private static int maxWorkerThreads;
    private static Cron cron;
    private static final Set<String> allowedWithoutAuthentication = new HashSet<>();
    private static final Hashtable<String,Object> environment = new Hashtable<>();  // general application-specific values
    /** True if running on Linux. */
    public static boolean isLinux = false;
    /** True if running on macOS. */
    public static boolean isMacOS = false;
    /** True if running on Windows. */
    public static boolean isWindows = false;
    /** True if running on SunOS (also includes Solaris and OpenIndiana). */
    public static boolean isSunOS = false;
    /** True if running on Haiku OS. */
    public static boolean isHaiku = false;
    /** True if running on FreeBSD. */
    public static boolean isFreeBSD;

    /** The queue manager for handling asynchronous operations. */
    private QueueManager queueManager;

    /**
     * Returns <code>true</code> if in development mode.  Returns <code>false</code> if in production.
     *
     * @return true if in development mode, false if in production
     */
    public static boolean isUnderIDE() {
        return underIDE;
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        if (queueManager == null)
            queueManager = new org.kissweb.restServer.QueueManager(maxWorkerThreads);
        ServletOutputStream out = response.getOutputStream();

        queueManager.add(request, response, out);
    }

    /**
     * Return the root of the application back-end source code.
     * This takes into account where the source code is.
     *
     * @return the application path
     */
    public static String getApplicationPath() {
        return applicationPath;
    }

    /**
     * Get the root path of the entire application.
     *
     * @return the root path
     */
    public static String getRootPath() {
        return rootPath;
    }

    /**
     * Manually set root path if needed.
     * This is needed when Kiss is not used to process the REST services.
     * This rootPath is used by the reporting facility.
     *
     * @param path the root path to set
     */
    public static void setRootPath(String path) {
        rootPath = path;
    }

    /**
     * Set the application path when it is determined outside of Kiss.
     * This is needed when an application is using the Kiss library but not its REST server.
     *
     * @param path the application path to set
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
     * @param _rootPath the root path to set
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
        final String osName = System.getProperty("os.name");
        isLinux = osName.startsWith("Linux");
        isMacOS = osName.startsWith("Mac OS X");
        isWindows = osName.startsWith("Windows");
        isSunOS = osName.startsWith("SunOS");  // includes OpenIndiana and Solaris
        isHaiku = osName.startsWith("Haiku");
        isFreeBSD = osName.startsWith("FreeBSD");
        setApplicationPathInternal(path);
        org.kissweb.restServer.ProcessServlet.ExecutionReturn res = (new GroovyService()).internalGroovy(null, null, null, "KissInit", "init");
        if (res == ProcessServlet.ExecutionReturn.Success) {
            hasDatabase = database != null  &&  !database.isEmpty();
            if (hasDatabase) {
                try {
                    makeDatabaseConnection();
                } catch (PropertyVetoException | SQLException | ClassNotFoundException e) {
                    logger.error(e);
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
                    MainServlet::openNewConnection,
                    MainServlet::success,
                    MainServlet::failure);
        } catch (IOException e) {
            logger.error(e);
        }

        logger.setLevel(level);
    }

    /**
     * Returns a new connection to the database if one is configured.
     * If you explicitly open a new connection with this method you must explicitly close it via the closeConnection method.
     * <br><br>
     * Each thread (e.g., web service, cron job) must open its own connection to the database. Otherwise, the SQL operations in
     * one thread will interfere with the SQL operations in another thread. Kiss automatically handles this for web services and cron jobs.
     * However, if you start additional threads that need to access the database, you must open a new connection for each thread.
     * <br><br>
     * It is critical that any thread that opens a new connection must be sure to explicitly close that connection upon exit.
     *
     * @return a new connection to the database if one is configured, otherwise null.
     * @see #closeConnection(Connection db, boolean success)
     */
    public static Connection openNewConnection() {
        if (!hasDatabase)
            return null;
        Connection db = null;
        try {
            java.sql.Connection c = MainServlet.getCpds().getConnection();
            c.setAutoCommit(false);
            db = new Connection(c);
        } catch (SQLException e) {
            logger.error(e);
        }
        return db;
    }

    /**
     * Closes a connection to the database opened with openNewConnection.
     * Commits or rolls back the transaction according to the success parameter.
     * @param db the Connection instance to close
     * @see #openNewConnection()
     * @see #closeConnection(Connection db, boolean success)
     */
    public static void closeConnection(Connection db) {
        java.sql.Connection sconn = null;
        try {
            sconn = db.getSQLConnection();
            db.close();
        } catch (SQLException e) {
            logger.error(e);
        }
        try {
            if (sconn != null)
                sconn.close();
        } catch (SQLException e) {
            logger.error(e);
        }
    }

    /**
     * Closes a connection to the database opened with openNewConnection.
     * Commits or rolls back the transaction according to the success parameter.
     * @param db the Connection instance to close
     * @param success if true, any changes made to the database are committed; otherwise, the transaction is rolled back
     * @see #openNewConnection()
     * @see #closeConnection(Connection db)
     */
    public static void closeConnection(Connection db, boolean success) {
        try {
            if (success)
                db.commit();
            else
                db.rollback();
            closeConnection(db);
        } catch (SQLException e) {
            logger.error(e);
        }
    }

    private static void success(Object p) {
        closeConnection((Connection) p, true);
    }

    private static void failure(Object p) {
        closeConnection((Connection) p, false);
    }


    /**
     * Cancels the cron daemon.
     * <br><br>
     * Cron jobs scheduled after this call will not run.
     * <br><br>
     * Has no effect if cron daemon is not running.
     */
    static void stopCron() {
        cron.cancel();
    }

    private static void makeDatabaseConnection() throws PropertyVetoException, SQLException, ClassNotFoundException {
        Level level = logger.getLevel();
        logger.setLevel(Level.ALL);
        if (!hasDatabase) {
            logger.info("* * * No database configured; bypassing login requirements");
            return;
        }
        if (cpds == null) {
            if (port == null)
                logger.info("* * * Attempting to connect to database " + host + ":" + database + ":" + user);
            else
                logger.info("* * * Attempting to connect to database " + host + ":" + port + ":" + database + ":" + user);
            if (connectionType == Connection.ConnectionType.SQLite  &&  database != null  &&  !database.isEmpty() &&  database.charAt(0) != '/')
                database = applicationPath + database;
            String cstr = Connection.makeConnectionString(connectionType, host, port, database, user, password);
            if (connectionParameters != null)
                cstr += connectionParameters;
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

            cpds.setMinPoolSize(5);
            cpds.setInitialPoolSize(5);
            cpds.setAcquireIncrement(5);
            cpds.setCheckoutTimeout(10_000);
            cpds.setMaxStatements(180);
            cpds.setUnreturnedConnectionTimeout(60);
            cpds.setDebugUnreturnedConnectionStackTraces(isUnderIDE());
            cpds.setMaxPoolSize(defaultMaxPoolSize()); // see below
        }
        logger.setLevel(level);
    }

    private static int defaultMaxPoolSize() {
        return Integer.parseInt(
                System.getProperty("db.maxPoolSize",
                        String.valueOf(Runtime.getRuntime().availableProcessors() * 2)));
    }

    static String getDynamicClassPath() {
        return "";
    }

    /**
     * Sets the database connection type.
     *
     * @param connectionType the connection type to set
     */
    public static void setConnectionType(Connection.ConnectionType connectionType) {
        MainServlet.connectionType = connectionType;
    }

    /**
     * Gets the database connection type.
     *
     * @return the connection type
     */
    public static Connection.ConnectionType getConnectionType() {
        return connectionType;
    }

    /**
     * Sets the database host.
     *
     * @param hostp the host to set
     */
    public static void setHost(String hostp) {
        host = hostp;
    }

    /**
     * Sets the database port.
     *
     * @param portp the port to set
     */
    public static void setPort(int portp) {
        port = portp;
    }

    /**
     * Gets the database host.
     *
     * @return the host
     */
    public static String getHost() {
        return host;
    }

    /**
     * Gets the database port.
     *
     * @return the port
     */
    public static Integer getPort() {
        return port;
    }

    /**
     * Gets the database user.
     *
     * @return the user
     */
    public static String getUser() {
        return user;
    }

    /**
     * Sets the database user.
     *
     * @param userp the user to set
     */
    public static void setUser(String userp) {
        user = userp;
    }

    /**
     * Gets the database password.
     *
     * @return the password
     */
    public static String getPassword() {
        return password;
    }

    /**
     * Sets the database password.
     *
     * @param passwordp the password to set
     */
    public static void setPassword(String passwordp) {
        password = passwordp;
    }

    /**
     * Gets the database name.
     *
     * @return the database name
     */
    public static String getDatabase() {
        return database;
    }

    /**
     * Sets the database name.
     *
     * @param databasep the database name to set
     */
    public static void setDatabase(String databasep) {
        database = databasep;
    }

    /**
     * Sets a string to be appended to the calculated database connection string.
     * If the database requires an '&' or ';' before any connection parameters you must include it.
     *
     * @param cp additional database connection parameters
     */
    public static void setConnectionParameters(String cp) {
        connectionParameters = cp;
    }

    /**
     * Sets the maximum number of worker threads.
     *
     * @param maxThreads the maximum number of threads
     */
    public static void setMaxWorkerThreads(int maxThreads) {
        maxWorkerThreads = maxThreads;
    }

    /**
     * Checks if a database is configured.
     *
     * @return true if database is configured
     */
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
     * @param className the class name of the service
     * @param methodName the method name to allow without authentication
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
     * @param key the key for the environment variable
     * @param value the value to associate with the key
     *
     * @see #getEnvironment(String)
     */
    public static void putEnvironment(String key, Object value) {
        environment.put(key, value);
    }

    /**
     * Retrieve an application-specific value previously set with <code>putEnvironment</code>
     *
     * @param key the key to retrieve
     * @return the value associated with the key
     *
     * @see #putEnvironment(String, Object)
     */
    public static Object getEnvironment(String key) {
        return environment.get(key);
    }

}
