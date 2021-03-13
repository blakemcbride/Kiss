package org.kissweb.restServer;

import com.mchange.v2.c3p0.ComboPooledDataSource;
import org.apache.log4j.Logger;
import org.kissweb.database.Connection;

import javax.servlet.annotation.MultipartConfig;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.beans.PropertyVetoException;
import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.sql.SQLException;


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
    private static boolean debug = false;          // set by KissInit.groovy
    private static boolean hasDatabase;            // determined by KissInit.groovy
    private static boolean systemInitialized = false;
    private static int maxWorkerThreads;

    public static boolean isUnderIDE() {
        return underIDE;
    }

    private org.kissweb.restServer.QueueManager queueManager;

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        if (!systemInitialized) {
            try {
                initializeSystem(request, response);
                if (!systemInitialized)
                    return;
            } catch (Exception e) {
                return;
            }
        }
        if (queueManager == null)
            queueManager = new org.kissweb.restServer.QueueManager(maxWorkerThreads);

        PrintWriter out = response.getWriter();
        response.setStatus(202);
        out.flush();  //  this causes the first response

        queueManager.add(request, response, out);
    }

    private void initializeSystem(HttpServletRequest request, HttpServletResponse response) throws ClassNotFoundException, PropertyVetoException, SQLException {
        setApplicationPath(request);
        org.kissweb.restServer.ProcessServlet.ExecutionReturn res = (new GroovyService()).internalGroovy(null, response, null, "KissInit", "init");
        if (res == ProcessServlet.ExecutionReturn.Success) {
            hasDatabase = database != null  &&  !database.isEmpty();
            if (hasDatabase)
                makeDatabaseConnection();
            else
                System.out.println("* * * No database configured; bypassing login requirements");
            systemInitialized = true;
        }
    }

    /**
     * Return the root of the application back-end source code
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

    private static void setApplicationPath(HttpServletRequest request) {
        rootPath = request.getServletContext().getRealPath("/");
        System.out.println("* * * Context path = " + rootPath);
        applicationPath = System.getenv("KISS_ROOT");
        if (applicationPath == null || applicationPath.isEmpty()) {
            if ((new File(rootPath + "../../../src/main/application/" + "KissInit.groovy")).exists()) {
                applicationPath = rootPath + "../../../src/main/application/";
                underIDE = true;
            } else if ((new File(rootPath + "../../src/main/application/" + "KissInit.groovy")).exists()) {
                applicationPath = rootPath + "../../src/main/application/";
                underIDE = true;
            } else if ((new File(rootPath + "../../../../src/main/application/" + "KissInit.groovy")).exists()) {
                applicationPath = rootPath + "../../../../src/main/application/";
                underIDE = true;
            } else {
                applicationPath = rootPath + (rootPath.endsWith("/") ? "" : "/") + "WEB-INF/application/";
                underIDE = false;
            }
        } else {
            underIDE = true;
            applicationPath = applicationPath.replaceAll("\\\\", "/");
            applicationPath = applicationPath + (applicationPath.endsWith("/") ? "" : "/") + "src/main/application/";
        }
        try {
            applicationPath = (new File(applicationPath)).getCanonicalPath() + "/";
        } catch (IOException e) {
            // ignore
        }
        System.out.println(underIDE ? "* * * Is running with source" : "* * * Is not running with source");
        System.out.println("* * * Application path set to " + applicationPath);
    }

    private void makeDatabaseConnection() throws PropertyVetoException, SQLException, ClassNotFoundException {
        if (!hasDatabase) {
            System.out.println("* * * No database configured; bypassing login requirements");
            return;
        }
        if (cpds == null) {
            System.out.println("* * * Attempting to connect to database " + host + ":" + database + ":" + user);
            if (connectionType == Connection.ConnectionType.SQLite  &&  database != null  &&  !database.isEmpty() &&  database.charAt(0) != '/')
                database = applicationPath + database;
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

            cpds.setDriverClass(Connection.getDriverName(connectionType));

            cpds.setMaxStatements(180);
        }
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

    static boolean isDebug() {
        return debug;
    }

    public static void setDebug(boolean debugp) {
        debug = debugp;
    }

    public static String getDatabase() {
        return database;
    }

    public static void setDatabase(String databasep) {
        database = databasep;
    }

    public static void setMaxWorkerThreads(int maxThreads) {
        MainServlet.maxWorkerThreads = maxThreads;
    }

    public static boolean hasDatabase() {
        return hasDatabase;
    }

    static ComboPooledDataSource getCpds() {
        return cpds;
    }
}
