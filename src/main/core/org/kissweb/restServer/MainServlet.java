package org.kissweb.restServer;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.kissweb.Cron;
import org.kissweb.IniFile;
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
import java.util.HashMap;
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
    private static String databaseName;              // database name, set by application.ini
    private static String applicationPath;           // where the application files are
    private static String rootPath;                  // the root of the entire application
    private static boolean underIDE = false;
    private static ComboPooledDataSource cpds;
    private static boolean hasDatabase;              // determined by application.ini
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
        if (queueManager == null) {
            Integer maxWorkerThreads = getEnvironmentInt("MaxWorkerThreads");
            queueManager = new org.kissweb.restServer.QueueManager(maxWorkerThreads);
        }
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
        Integer userInactiveSeconds = getEnvironmentInt("UserInactiveSeconds");
        if (userInactiveSeconds != null)
            UserCache.setInactiveUserMaxSeconds(userInactiveSeconds);
        setApplicationPathInternal(path);
        ProcessServlet.ExecutionReturn res = (new GroovyService()).internalGroovy(null, "KissInit", "init");
        String databaseType = (String) environment.get("DatabaseType");
        databaseName = (String) environment.get("DatabaseName");
        if (res == ProcessServlet.ExecutionReturn.Success) {
            hasDatabase = databaseType != null  &&  !databaseType.isEmpty()  &&  databaseName != null  &&  !databaseName.isEmpty();
            if (hasDatabase) {
                try {
                    makeDatabaseConnection();
                } catch (PropertyVetoException | SQLException | ClassNotFoundException e) {
                    logger.error(e);
                    System.exit(-1);
                }
                logger.info("* * * Database " + databaseName + " opened successfully");
            }
            else
                logger.info("* * * No database configured; bypassing login requirements");
        } else
            logger.error("* * * Error executing KissInit.groovy");

        Connection db = MainServlet.openNewConnection();
        (new GroovyService()).internalGroovy(null, "KissInit", "init2", db);
        MainServlet.closeConnection(db);

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
        if (db != null &&  db.isOpen()) {
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
        boolean doClose = true;
        try {
            if (db != null && db.isOpen()) {
                if (success)
                    db.commit();
                else
                    db.rollback();
            } else
                doClose = false;
        } catch (SQLException ex) {
            logger.error("Commit/rollback failed – continuing to close.", ex);
        } finally {
            if (doClose)
                closeConnection(db);
        }
    }

    /**
     * Reads an INI file and loads values from a specified section into the environment.
     *
     * @param file   the name of the file to read
     * @param section the section of the INI file to read
     * @throws IOException if an I/O error occurs while reading the file
     */
    public static void readIniFile(String file, String section) throws IOException {
        IniFile ini = IniFile.load(file);
        if (ini == null)
            return;
        HashMap<String,String> map = ini.getSection(section);
        if (map != null)
            for (String key : map.keySet())
                environment.put(key, map.get(key));
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

    /**
     * Retrieves an environment variable by its key and converts it to an Integer.
     * <br><br>
     * If the environment variable is not found or is empty, returns null.
     *
     * @param s the key of the environment variable to retrieve
     * @return the Integer value of the environment variable, or null if the variable
     *         is not found or is empty
     */
    public static Integer getEnvironmentInt(String s) {
        String val = (String) environment.get(s);
        if (val == null || val.trim().isEmpty())
            return null;
        return Integer.valueOf(val.trim());
    }

    private static void makeDatabaseConnection() throws PropertyVetoException, SQLException, ClassNotFoundException {
        Level level = logger.getLevel();
        logger.setLevel(Level.ALL);
        if (!hasDatabase) {
            logger.info("* * * No database configured; bypassing login requirements");
            return;
        }
        if (cpds == null) {
            String host = (String) environment.get("DatabaseHost");
            Integer port = getEnvironmentInt("DatabasePort");
            String user = (String) environment.get("DatabaseUser");
            String password = (String) environment.get("DatabasePassword");
            if (port == null)
                logger.info("* * * Attempting to connect to database " + host + ":" + databaseName + ":" + user);
            else
                logger.info("* * * Attempting to connect to database " + host + ":" + port + ":" + databaseName + ":" + user);

            Connection.ConnectionType connectionType = null;
            String val = (String) environment.get("DatabaseType");
            if (val != null)
                switch (val.toLowerCase()) {
                    case "mysql":
                        connectionType = Connection.ConnectionType.MySQL;
                        break;
                    case "sqlite":
                        connectionType = Connection.ConnectionType.SQLite;
                        break;
                    case "postgresql":
                        connectionType = Connection.ConnectionType.PostgreSQL;
                        break;
                    case "mssql":
                        connectionType = Connection.ConnectionType.MicrosoftServer;
                        break;
                    case "oracle":
                        connectionType = Connection.ConnectionType.Oracle;
                        break;
                }

            if ((isSunOS || isHaiku) &&  connectionType == Connection.ConnectionType.SQLite)
                connectionType = null;  // not supported on these platforms

            if (connectionType == Connection.ConnectionType.SQLite  &&  databaseName != null  &&  !databaseName.isEmpty() &&  databaseName.charAt(0) != '/')
                databaseName = applicationPath + databaseName;
            String cstr = Connection.makeConnectionString(connectionType, host, port, databaseName, user, password);
            String connectionParameters = (String) environment.get("DatabaseConnectionParameters");
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
     * Set the database type.
     * This has been replaced with the application.ini file.
     * @param connectionType
     */
    @Deprecated
    public static void setConnectionType(Connection.ConnectionType connectionType) {
        if (connectionType == Connection.ConnectionType.MySQL)
            environment.put("DatabaseType", "mysql");
        else if (connectionType == Connection.ConnectionType.SQLite)
            environment.put("DatabaseType", "sqlite");
        else if (connectionType == Connection.ConnectionType.PostgreSQL)
            environment.put("DatabaseType", "postgresql");
        else if (connectionType == Connection.ConnectionType.MicrosoftServer)
            environment.put("DatabaseType", "mssql");
        else if (connectionType == Connection.ConnectionType.Oracle)
            environment.put("DatabaseType", "oracle");
    }

    /**
     * Set the database host.
     * This has been replaced with the application.ini file.
     * @param hostp
     */
    @Deprecated
    public static void setHost(String hostp) {
        environment.put("DatabaseHost", hostp);
    }

    /**
     * Set the database port.
     * This has been replaced with the application.ini file.
     * @param portp
     */
    @Deprecated
    public static void setPort(int portp) {
        environment.put("DatabasePort", String.valueOf(portp));
    }

    /**
     * Set the database user.
     * This has been replaced with the application.ini file.
     * @param userp
     */
    @Deprecated
    public static void setUser(String userp) {
        environment.put("DatabaseUser", userp);
    }

    /**
     * Set the database password.
     * This has been replaced with the application.ini file.
     * @param passwordp
     */
    @Deprecated
    public static void setPassword(String passwordp) {
        environment.put("DatabasePassword", passwordp);
    }

    /**
     * Set the database name.
     * This has been replaced with the application.ini file.
     * @param databasep
     * @deprecated
     */
    @Deprecated
    public static void setDatabase(String databasep) {
        environment.put("DatabaseName", databasep);
    }

    /**
     * Set the maximum number of worker threads.
     * This has been replaced with the application.ini file.
     * @param maxThreads
     */
    @Deprecated
    public static void setMaxWorkerThreads(int maxThreads) {
        environment.put("MaxWorkerThreads", String.valueOf(maxThreads));
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
