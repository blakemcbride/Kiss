package org.kissweb.restServer;

import org.apache.log4j.Logger;
import org.kissweb.json.JSONObject;
import org.kissweb.FileUtils;
import org.kissweb.FrontendException;
import org.kissweb.LogException;
import org.kissweb.database.Connection;

import jakarta.servlet.AsyncContext;
import jakarta.servlet.ServletContext;
import jakarta.servlet.ServletOutputStream;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import jakarta.servlet.http.Part;
import java.io.*;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.util.Enumeration;
import java.util.stream.Collectors;

/**
 * Author: Blake McBride
 * Date: 11/26/19
 */
public class ProcessServlet implements Runnable {

    private static final Logger logger = Logger.getLogger(ProcessServlet.class);

    static final int MaxHold = 600;         // number of seconds to cache microservices before unloading them
    static final int CheckCacheDelay = 60;  // how often to check to unload microservices in seconds
    private ServletContext servletContext;

    private final HttpServletRequest request;
    private final HttpServletResponse response;
    private final AsyncContext asyncContext;
    private final ServletOutputStream out;
    private UserData ud;
    /** Database connection for the current request. */
    protected Connection DB;
    private byte [] binaryData;
    private boolean isBinaryReturn = false;
    private static final ThreadLocal<ProcessServlet> instance = new ThreadLocal<>();
    private JSONObject injson;
    private JSONObject outjson;

    ProcessServlet(org.kissweb.restServer.QueueManager.Packet packet) {
        request = (HttpServletRequest) packet.asyncContext.getRequest();
        response = (HttpServletResponse) packet.asyncContext.getResponse();
        asyncContext = packet.asyncContext;
        out = packet.out;
    }

    @Override
    public void run() {
        try {
            run2();
        } catch (Throwable e) {
            logger.error("", e);
        }
    }

    enum ExecutionReturn {
        Success,
        NotFound,
        Error
    }

    /**
     * Get the absolute path of the root of the back-end application.
     *
     * @return the absolute path of the back-end application root
     */
    public String getRealPath() {
        return servletContext.getRealPath("/");
    }

    /**
     * Returns the ServletContext.
     *
     * @return the ServletContext instance
     */
    public ServletContext getServletContext() {
        return servletContext;
    }

    /**
     * Returns the HttpServletRequest.
     *
     * @return the HttpServletRequest instance
     */
    public HttpServletRequest getRequest() {
        return request;
    }

    /**
     * Return the number of files being uploaded.
     *
     * @return the number of files being uploaded
     * @see #getUploadFileName(int)
     * @see #getUploadBufferedInputStream(int)
     * @see #saveUploadFile(int)
     */
    public int getUploadFileCount() {
        int i = 0;
        for ( ; true ; i++) {
            Part filePart = null;
            try {
                filePart = request.getPart("_file-" + i);
            } catch (Exception ignored) {
            }
            if (filePart == null)
                break;
        }
        return i;
    }

    private String getFileName(final Part part) {
        final String partHeader = part.getHeader("content-disposition");
        for (String content : part.getHeader("content-disposition").split(";")) {
            if (content.trim().startsWith("filename")) {
                return content.substring(
                        content.indexOf('=') + 1).trim().replace("\"", "");
            }
        }
        return null;
    }

    /**
     * Returns the name of the file being uploaded.
     *
     * @param i beginning at 0
     * @return the name of the uploaded file, or null if not found
     * @see #getUploadFileCount()
     * @see #getUploadBufferedInputStream(int)
     * @see #saveUploadFile(int)
     */
    public String getUploadFileName(int i) {
        try {
            Part filePart = request.getPart("_file-" + i);
            return getFileName(filePart);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Returns the file extension of a given file upload.
     *
     * @param i the file index starting from 0
     * @return the file extension, or null if not found
     */
    public String getUploadFileType(int i) {
        try {
            final Part filePart = request.getPart("_file-" + i);
            final String fn = getFileName(filePart);
            final int idx = fn.lastIndexOf('.');
            return idx == -1 ? "" : fn.substring(idx + 1);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * In file upload scenarios, this method returns a BufferedInputStream
     * associated with file number i.  When done, the stream must be
     * closed by the application.
     *
     * @param i starting from 0
     * @return a BufferedInputStream for the uploaded file, or null if not found
     * @see BufferedInputStream#close()
     * @see #getUploadFileCount()
     * @see #getUploadFileName(int)
     * @see #saveUploadFile(int)
     */
    public BufferedInputStream getUploadBufferedInputStream(int i) {
        try {
            Part filePart = request.getPart("_file-" + i);
            return new BufferedInputStream(filePart.getInputStream());
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * In a file upload scenario, this method returns a byte array of the data that was uploaded.
     *
     * @param i starting from 0
     * @return the uploaded file data as a byte array
     * @throws IOException if an error occurs reading the file
     */
    public byte [] getUploadBytes(int i) throws IOException {
        BufferedInputStream bis = getUploadBufferedInputStream(i);
        if (bis == null)
            return null;
        byte [] ba = FileUtils.readAllBytes(bis);
        bis.close();
        return ba;
    }

    /**
     * Reads upload file "n", saves it to a temporary file, and returns the path to that file.
     *
     * @param n file number
     * @return the absolute path to the saved temporary file
     * @throws IOException if an error occurs during file operations
     *
     * @see #getUploadFileName(int)
     * @see #getUploadBufferedInputStream(int)
     */
    public String saveUploadFile(int n) throws IOException {
        File f = FileUtils.createReportFile("save", "tmp");
        try (
                BufferedInputStream bis = getUploadBufferedInputStream(n);
                BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(f));
        ) {
            int c;
            while (-1 != (c = bis.read()))
                bos.write(c);
        }
        return f.getAbsolutePath();
    }

    /**
     * Reads upload file "n", saves it to the file name passed in, and returns the file name passed in
     *
     * @param n file number
     * @param fileName the name of the file to save the upload to
     * @return the fileName passed in
     * @throws IOException if an error occurs during file operations
     *
     * @see #getUploadFileName(int)
     * @see #getUploadBufferedInputStream(int)
     */
    public String saveUploadFile(int n, String fileName) throws IOException {
        try (
                BufferedInputStream bis = getUploadBufferedInputStream(n);
                BufferedOutputStream bos = new BufferedOutputStream(new FileOutputStream(fileName));
        ) {
            int c;
            while (-1 != (c = bis.read()))
                bos.write(c);
        }
        return fileName;
    }

    /**
     * When doing the file-upload method of REST service, everything gets transmitted as strings.
     * I need to convert them back to their correct type.
     * I am using 'S' as a standard to signify string.
     */
    private static Object getObject(HttpServletRequest request, String name) {
        final String value = request.getParameter(name);
        if (value != null) {
            if (!value.isEmpty()) {
                if (name.charAt(0) == '_') // no special processing - all strings
                    return value;
                if (value.charAt(0) == 'S')
                    return value.substring(1);
            }
            if (value.equals("true"))
                return true;
            if (value.equals("false"))
                return false;
            if (value.equals("null"))
                return null;
            if (value.contains("."))
                return Double.parseDouble(value);
            long lnum = Long.parseLong(value);
            if (lnum <= Integer.MAX_VALUE && lnum >= Integer.MIN_VALUE)
                return Integer.parseInt(value);
            else
                return lnum;
        } else
            return null;
    }

    private void run2() throws IOException {
        instance.set(this);
        servletContext = request.getServletContext();
        String _className;
        String _method;
        outjson = new JSONObject();
        ProcessServlet.ExecutionReturn res;

        try {
            newDatabaseConnection();
        } catch (Throwable e) {
            errorReturn(response, "Unable to connect to the database", e);
            return;
        }

        _className = request.getParameter("_class");
        if (_className != null) {
            //  is file upload
            _method = request.getParameter("_method");
            logger.info("Enter back-end seeking UPLOAD service " + _className + "." + _method + "()");
            injson = new JSONObject();
            Enumeration<String> names = request.getParameterNames();
            while (names.hasMoreElements()) {
                String name = names.nextElement();
                Object value = getObject(request, name);
                injson.put(name, value);
            }
        } else {
            String instr;
            try {
                instr = request.getReader().lines().collect(Collectors.joining(System.lineSeparator()));
                injson = new JSONObject(instr);
            } catch (Exception e) {
                errorReturn(response, "Unable to parse request json", e);
                return;
            }
            _className = injson.getString("_class");
            _method = injson.getString("_method");
            logger.info("Enter back-end seeking REST service " + _className + "." + _method + "()");
        }

        if (_method == null  ||  _method.isEmpty()) {
            errorReturn(response, "missing _method", null);
            return;
        }

        if (_className.isEmpty()) {
            // Core method
            if (_method.equals("LoginRequired")) {
                logger.info("Login is " + (MainServlet.hasDatabase() ? "" : "not ") + "required");
                outjson.put("LoginRequired", MainServlet.hasDatabase());
                successReturn(response, outjson);
                return;
            } else if (_method.equals("Login")) {
                logger.info("Attempting user login for " + injson.getString("username"));
                try {
                    String uuid = login(injson.getString("username"), injson.getString("password"), outjson);
                    outjson.put("uuid", uuid);
                    successReturn(response, outjson);
                    logger.info("Login successful");
                    return;
                } catch (Exception e) {
                    logger.info("Login failure");
                    loginFailure(response, e);
                    return;
                }
            } else {
                logger.error("Incorrect internal method call.");
                errorReturn(response, "Incorrect internal method call.", null);
            }
        } else {
            // User defined method
            if (MainServlet.hasDatabase()) {
                if (MainServlet.shouldAllowWithoutAuthentication(_className, _method)) {
                    ud = UserCache.findUser(injson.getString("_uuid"));  // in case they are logged in
                    logger.info("Method " + _className + "." + _method + "() allowed without authentication");
                } else {
                    try {
                        logger.info("Validating uuid " + injson.getString("_uuid"));
                        ud = UserCache.findUser(injson.getString("_uuid"));
                        checkLogin(ud);
                    } catch (Exception e) {
                        logger.info("Login failure.");
                        loginFailure(response, e);
                        return;
                    }
                    logger.info("Login success");
                }
            } else {
                ud = UserCache.findUser(injson.getString("_uuid"));
                if (ud == null  &&  !MainServlet.shouldAllowWithoutAuthentication(_className, _method))
                    loginFailure(response, null);
            }
        }

        res = (new GroovyService()).tryGroovy(this, response, _className, _method, injson, outjson);
        if (res == ProcessServlet.ExecutionReturn.Error)
            return;

        if (res == ProcessServlet.ExecutionReturn.NotFound) {
            res = (new org.kissweb.restServer.JavaService()).tryJava(this, response, _className, _method, injson, outjson);
            if (res == ProcessServlet.ExecutionReturn.Error)
                return;
        }

        if (res == ProcessServlet.ExecutionReturn.NotFound) {
            res = (new LispService()).tryLisp(this, response, _className, _method, injson, outjson);
            if (res == ProcessServlet.ExecutionReturn.Error)
                return;
        }
        if (res == ProcessServlet.ExecutionReturn.NotFound) {
            res = (new CompiledJavaService()).tryCompiledJava(this, response, _className, _method, injson, outjson);
            if (res == ProcessServlet.ExecutionReturn.Error)
                return;
        }

        if (res == ProcessServlet.ExecutionReturn.NotFound) {
            errorReturn(response, "No back-end code found for " + _className, null);
        } else {
            logger.info("REST service " + _className + "." + _method + "() executed successfully");
            successReturn(response, outjson);
        }
    }

    /**
     * Return binary data to the front-end.
     *
     * @param data the binary data to return
     */
    public void returnBinary(byte [] data) {
        isBinaryReturn = true;
        binaryData = data;
    }

    private void successReturn(HttpServletResponse response, JSONObject outjson) {
        try {
            if (DB != null)
                DB.commit();
            outjson.put("_Success", true);
            outjson.put("_ErrorCode", 0);  // success
            response.setStatus(200);
            if (!isBinaryReturn) {
                response.setContentType("application/json");
                out.print(outjson.toString());
            } else {
                response.setContentType("application/octet-stream");
                out.print(outjson.toString() + "\003");
                if (binaryData != null) {
                    out.write(binaryData);
                    binaryData = null;
                    isBinaryReturn = false;
                }
            }
            out.flush();
            out.close();     // this causes the second response
        } catch (SQLException | IOException ignored) {
        } finally {
            asyncContext.complete();
            closeSession();
        }
    }

    void errorReturn(HttpServletResponse response, String msg, Throwable e) {
        try {
            if (DB != null) {
                try {
                    DB.rollback();
                } catch (SQLException ignored) {
                }
            }
            response.setContentType("application/json");
            response.setStatus(200);
            JSONObject outjson = new JSONObject();
            outjson.put("_Success", false);
            outjson.put("_ErrorMessage", msg != null ? msg :(e != null ? e.getMessage() : "unspecified"));
            outjson.put("_ErrorCode", 1);  // general error
            log_error(msg, e);
            out.print(outjson.toString());
            out.flush();
            out.close();  //  this causes the second response
        } catch (Exception ignored) {
        } finally {
            asyncContext.complete();
            closeSession();
        }
    }

    private void loginFailure(HttpServletResponse response, Throwable e) {
        String msg = null;
        if (e != null) {
            msg = e.getMessage();
            if (msg == null) {
                Throwable t = e.getCause();
                if (t != null)
                    msg = t.getMessage();
            }
        }
        if (msg == null)
            msg = "Login failure.";
        if (DB != null) {
            try {
                DB.rollback();
            } catch (SQLException ignored) {
            }
        }
        closeSession();
        response.setContentType("application/json");
        response.setStatus(200);
        JSONObject outjson = new JSONObject();
        outjson.put("_Success", false);
        outjson.put("_ErrorMessage", msg);
        outjson.put("_ErrorCode", 2);  // login failure
        try {
            out.print(outjson.toString());
            out.flush();
            out.close();  //  this causes the second response
        } catch (IOException ignore) {
        }
        asyncContext.complete();
    }

    boolean isEmpty(final String str) {
        return (str == null || str.equals(""));
    }

    /**
     * Get all user data.
     *
     * @return the UserData instance for the current user
     */
    public UserData getUserData() {
        return ud;
    }

    /**
     * Get a specific user data element.
     *
     * @param key the key for the user data element
     * @return the user data element, or null if not found
     */
    public Object getUserData(String key) {
        return ud.getUserData(key);
    }

    /**
     * Returns the IP address of the client.
     *
     * @return the client's IP address
     */
    public String getRemoteAddr() {
        String remoteAddr = "";

        if (request != null) {
            remoteAddr = request.getHeader("X-FORWARDED-FOR");
            if (remoteAddr == null || "".equals(remoteAddr))
                remoteAddr = request.getRemoteAddr();
        }
        return remoteAddr;
    }

    /**
     * Get the current instance of the ProcessServlet.
     * This allows us a global way of getting the ProcessServlet instance associated with a particular thread / request.
     *
     * @return  the current instance
     */
    public static ProcessServlet getInstance() {
        return instance.get();
    }

    /**
     * Get the current connection to the database associated with this request.
     * This may be null if the database is not available.
     *
     * @return the current connection
     */
    public static Connection getConnection() {
        return instance.get().DB;
    }

    /**
     * Returns the input JSON object associated with this request.
     *
     * @return the JSON object
     */
    public static JSONObject getInjson() {
        return instance.get().injson;
    }

    /**
     * Returns the output JSON object associated with this request.
     * This is the object where the web service should place any data it wants to send back to the front-end.
     *
     * @return the JSON object
     */
    public static JSONObject getOutjson() {
        return instance.get().outjson;
    }

    private void log_error(final String str, final Throwable e) {
        if (e instanceof FrontendException)
            return;  //  no log
        if (e instanceof LogException)
            logger.warn(str + " " + e.getMessage());
        else {
            logger.error(str, e);
        }
    }

    private String login(String user, String password, JSONObject outjson) throws Exception {
        UserData ud;
        if (MainServlet.hasDatabase()) {
            ud = (UserData) GroovyClass.invoke(true, "Login", "login", null, DB, user, password, outjson, this);
            if (ud == null)
                throw new LogException("Invalid login.");
        } else
            ud = UserCache.newUser(user, password, null);
        return ud.getUuid();
    }

    private void checkLogin(UserData ud) throws Exception {
        if (ud == null)
            throw new FrontendException("You have been logged out due to inactivity. Please log in again.");
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime timeout = ud.getLastAccessDate().plusSeconds(120);  // cache user data for 120 seconds
        if (MainServlet.hasDatabase() && now.isAfter(timeout)) {
            Boolean good = (Boolean) GroovyClass.invoke(true, "Login", "checkLogin", null, DB, ud, this);
            if (!good) {
                UserCache.removeUser(ud.getUuid());
                throw new LogException("Invalid login.");
            }
        }
        ud.setLastAccessDate(LocalDateTime.now());
    }

    private void newDatabaseConnection() throws SQLException {
        if (!MainServlet.hasDatabase())
            return;
        logger.info("Previous open database connections = " + MainServlet.getCpds().getNumBusyConnections());
        final java.sql.Connection conn = MainServlet.getCpds().getConnection();
        conn.setAutoCommit(false);  //  all SQL operations require a commit but Kiss does a commit at the end of each service
        DB = new Connection(conn);
        logger.info("New database connection obtained");
    }

    private void closeSession() {
        instance.remove();
        java.sql.Connection sconn = null;
        try {
            if (DB != null) {
                sconn = DB.getSQLConnection();
                DB.close();
            }
        } catch (SQLException e) {
            logger.error(e);
        } finally {
            DB = null;
        }
        try {
            if (sconn != null)
                sconn.close();
        } catch (SQLException e) {
            logger.error(e);
        }
    }

}
