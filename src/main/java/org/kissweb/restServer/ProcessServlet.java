package org.kissweb.restServer;

import org.apache.log4j.Logger;
import org.json.JSONObject;
import org.kissweb.DateUtils;
import org.kissweb.FileUtils;
import org.kissweb.database.Connection;
import org.kissweb.database.Record;

import javax.servlet.AsyncContext;
import javax.servlet.ServletContext;
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

    private HttpServletRequest request;
    private HttpServletResponse response;
    private AsyncContext asyncContext;
    private PrintWriter out;
    protected Connection DB;

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
        } catch (IOException e) {
            e.printStackTrace();
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
            } catch (Exception e) {
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
            if (MainServlet.isDebug())
                System.err.println("Enter back-end seeking UPLOAD service " + _className + "." + _method + "()");
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
            if (MainServlet.isDebug())
                System.err.println("Enter back-end seeking REST service " + _className + "." + _method + "()");
        }

        if (_method == null  ||  _method.isEmpty()) {
            System.err.println("Missing _method");
            errorReturn(response, "missing _method", null);
            return;
        }

        if (_className.isEmpty())
            if (_method.equals("LoginRequired")) {
                if (MainServlet.isDebug())
                    System.err.println("Login is " + (MainServlet.hasDatabase() ? "" : "not ") + "required");
                outjson.put("LoginRequired", MainServlet.hasDatabase());
                successReturn(response, outjson);
                return;
            } else if (_method.equals("Login")) {
                if (MainServlet.isDebug())
                    System.err.println("Attempting user login for " + injson.getString("username"));
                try {
                    String uuid = login(injson.getString("username"), injson.getString("password"));
                    outjson.put("uuid", uuid);
                    successReturn(response, outjson);
                    if (MainServlet.isDebug())
                        System.err.println("Login successful");
                    return;
                } catch (Exception e) {
                    errorReturn(response, "Login failure.", e);
                    if (MainServlet.isDebug())
                        System.err.println("Login failure.");
                    return;
                }
            } else if (MainServlet.hasDatabase()) {
                try {
                    if (MainServlet.isDebug())
                        System.err.println("Validating uuid " + injson.getString("_uuid"));
                    checkLogin(injson.getString("_uuid"));
                } catch (Exception e) {
                    if (MainServlet.isDebug())
                        System.err.println("Login failure.");
                    errorReturn(response, "Login failure.", e);
                    return;
                }
                if (MainServlet.isDebug())
                    System.err.println("Login success");
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
            if (MainServlet.isDebug())
                System.err.println("No back-end code found for " + _className);
            errorReturn(response, "No back-end code found for " + _className, null);
        } else {
            if (MainServlet.isDebug())
                System.err.println("REST service " + _className + "." + _method + "() executed successfully");
            successReturn(response, outjson);
        }
    }

    private void successReturn(HttpServletResponse response, JSONObject outjson) throws IOException {
        try {
            if (DB != null)
                DB.commit();
        } catch (SQLException e) {

        }
        response.setContentType("application/json");
        response.setStatus(200);
        outjson.put("_Success", true);
        out.print(outjson);
        out.flush();
        out.close();     // this causes the second response
        closeSession();
        asyncContext.complete();
    }

    void errorReturn(HttpServletResponse response, String msg, Throwable e) {
        if (DB != null) {
            try {
                DB.rollback();
            } catch (SQLException e1) {
            }
        }
        closeSession();
        response.setContentType("application/json");
        response.setStatus(200);
        JSONObject outjson = new JSONObject();
        outjson.put("_Success", false);
        String finalMsg = msg;
        if (e != null) {
            String m = e.getMessage();
            if (m != null)
                finalMsg = msg + " " + m;
            Throwable cause = e.getCause();
            if (cause != null) {
                m = cause.getMessage();
                if (m != null)
                    finalMsg = msg + " " + m;
            }
        }
        outjson.put("_ErrorMessage", finalMsg);
        log_error(finalMsg, e);
        out.print(outjson);
        out.flush();
        out.close();  //  this causes the second response
        asyncContext.complete();
    }

    boolean isEmpty(final String str) {
        return (str == null || str.equals(""));
    }

    private void log_error(final String str, final Throwable e) {
        String time = DateUtils.todayDate() + " ";
        logger.error(time + str, e);
        if (e != null) {
            e.printStackTrace();
            final StringWriter sw = new StringWriter();
            e.printStackTrace(new PrintWriter(sw));
            logger.error(sw.toString());
        }
    }

    private String login(String user, String password) throws Exception {
        UserCache.UserData ud;
        if (MainServlet.hasDatabase()) {
            Record rec = DB.fetchOne("select * from users where user_name = ? and user_password = ? and user_active = 'Y'", user, password);
            if (rec == null)
                throw new Exception("Invalid login.");
            ud = UserCache.newUser(user, password);
            ud.user_id = rec.getInt("user_id");
        } else
            ud = UserCache.newUser(user, password);
        return ud.uuid;
    }

    private int checkLogin(String uuid) throws Exception {
        UserCache.UserData ud = UserCache.findUser(uuid);
        if (ud == null)
            throw new Exception("Login timed out; please re-login.");
        LocalDateTime now = LocalDateTime.now();
        LocalDateTime timeout = ud.lastAccessDate.plusSeconds(120);  // cache user data for 120 seconds
        if (MainServlet.hasDatabase() && now.isAfter(timeout)) {
            Record rec = DB.fetchOne("select * from users where user_name = ? and user_password = ? and user_active = 'Y'", ud.username, ud.password);
            if (rec == null) {
                UserCache.removeUser(uuid);
                throw new Exception("Invalid login.");
            }
        }
        ud.lastAccessDate = LocalDateTime.now();
        return ud.user_id;
    }

    private void newDatabaseConnection() throws SQLException {
        if (!MainServlet.hasDatabase())
            return;
        if (MainServlet.isDebug())
            System.err.println("Previous open database connections = " + MainServlet.getCpds().getNumBusyConnections());
        DB = new Connection(MainServlet.getCpds().getConnection());
        if (MainServlet.isDebug())
            System.err.println("New database connection obtained");
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
            e.printStackTrace();
        }
    }

}
