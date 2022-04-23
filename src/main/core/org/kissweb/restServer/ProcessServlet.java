package org.kissweb.restServer;

import org.apache.log4j.Logger;
import org.json.JSONObject;
import org.kissweb.FileUtils;
import org.kissweb.FrontendException;
import org.kissweb.LogException;
import org.kissweb.database.Connection;

import javax.servlet.AsyncContext;
import javax.servlet.ServletContext;
import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.http.Part;
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
    protected Connection DB;
    private byte [] binaryData;
    private boolean isBinaryReturn = false;

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
            logger.error(e);
        }
    }

    enum ExecutionReturn {
        Success,
        NotFound,
        Error
    }

    /**
     * Get the absolute path of the root of the back-end application.
     */
    public String getRealPath() {
        return servletContext.getRealPath("/");
    }

    /**
     * Returns the ServletContext.
     */
    public ServletContext getServletContext() {
        return servletContext;
    }

    /**
     * Returns the HttpServletRequest.
     */
    public HttpServletRequest getRequest() {
        return request;
    }

    /**
     * Return the number of files being uploaded.
     *
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
     *
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
     * In file upload scenarios, this method returns a BufferedInputStream
     * associated with file number i.  When done, the stream must be
     * closed by the application.
     *
     * @param i starting from 0
     *
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
     * Reads upload file "n", saves it to a temporary file, and returns the path to that file.
     *
     * @param n file number
     * @return
     * @throws IOException
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

    private void run2() throws IOException {
        servletContext = request.getServletContext();
        String _className;
        String _method;
        JSONObject injson;
        JSONObject outjson = new JSONObject();
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
                String value = request.getParameter(name);
                injson.put(name, value);
            }
        } else {
            String instr = request.getReader().lines().collect(Collectors.joining(System.lineSeparator()));
            try {
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
     * @param data
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
            outjson.put("_ErrorMessage", e != null ? e.getMessage() : msg);
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
        String msg;
        if (e != null)
            msg = e.getMessage();
        else
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

    public UserData getUserData() {
        return ud;
    }

    /**
     * Returns the IP address of the client.
     *
     * @return
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
            logger.error(e);
        }
    }

}
