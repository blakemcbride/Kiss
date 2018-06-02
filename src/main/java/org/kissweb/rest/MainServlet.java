package org.kissweb.rest;

import org.apache.log4j.Logger;
import org.json.JSONObject;

import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.beans.PropertyVetoException;
import java.io.IOException;
import java.io.PrintWriter;
import java.sql.SQLException;
import java.util.stream.Collectors;


/**
 * Author: Blake McBride
 * Date: 5/4/18
 */
@WebServlet("/rest")
public class MainServlet extends ServiceBase {

    private static final transient Logger logger = Logger.getLogger(MainServlet.class);

    static final int MaxHold = 600;         // number of seconds to cache microservices before unloading them
    static final int CheckCacheDelay = 60;  // how often to check to unload microservices in seconds
    private static boolean systemInitialized = false;

    enum ExecutionReturn {
        Success,
        NotFound,
        Error
    }

    private void initializeSystem(HttpServletRequest request, HttpServletResponse response) throws ClassNotFoundException, PropertyVetoException, SQLException {
        setApplicationPath(request);
        ExecutionReturn res = (new GroovyService()).internalGroovy(this, response, null, "KissInit", "init");
        if (res == ExecutionReturn.Success) {
            makeDatabaseConnection();
            systemInitialized = true;
        }
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        String instr = request.getReader().lines().collect(Collectors.joining(System.lineSeparator()));
        JSONObject injson = new JSONObject(instr);
        JSONObject outjson = new JSONObject();
        ExecutionReturn res;

        if (!systemInitialized) {
            try {
                initializeSystem(request, response);
                if (!systemInitialized)
                    return;
            } catch (Exception e) {
                return;
            }
        }

        try {
            newDatabaseConnection();
        } catch (SQLException e) {
            errorReturn(response, "Unable to connect to the database", e);
            return;
        }

        final String _package = injson.getString("_package");
        final String _className = injson.getString("_class");
        final String _method = injson.getString("_method");

        if (_package.isEmpty() && _className.equals("Login"))
            try {
                String uuid = login(injson.getString("username"), injson.getString("password"));
                outjson.put("uuid", uuid);
                successReturn(response, outjson);
                return;
            } catch (Exception e) {
                errorReturn(response, "Login failure", e);
                return;
            }
        else
            try {
                checkLogin(injson.getString("_uuid"));
            } catch (Exception e) {
                errorReturn(response, "Login failure", e);
                return;
            }

        res = (new GroovyService()).tryGroovy(this, response, _package, _className, _method, injson, outjson);
        if (res == ExecutionReturn.Error)
            return;

        if (res == ExecutionReturn.NotFound) {
            res = (new JavaService()).tryJava(this, response, _package, _className, _method, injson, outjson);
            if (res == ExecutionReturn.Error)
                return;
        }

        if (res == ExecutionReturn.NotFound) {
            res = (new LispService()).tryLisp(this, response, _package, _className, _method, injson, outjson);
            if (res == ExecutionReturn.Error)
                return;
        }

        if (res == ExecutionReturn.NotFound) {
            res = (new CompiledJavaService()).tryCompiledJava(this, response, _package, _className, _method, injson, outjson);
            if (res == ExecutionReturn.Error)
                return;
        }

        if (res == ExecutionReturn.NotFound)
            errorReturn(response, "No back-end code found for " + _package + "." + _className, null);
        else
            successReturn(response, outjson);
    }

    private void successReturn(HttpServletResponse response, JSONObject outjson) throws IOException {
        try {
            DB.commit();
        } catch (SQLException e) {

        }
        outjson.put("_Success", true);
        PrintWriter out = response.getWriter();
        out.print(outjson);
        out.flush();
        response.setContentType("application/json");
        response.setStatus(200);
        closeSession();
    }

    void errorReturn(HttpServletResponse response, String msg, Exception e) {
        if (DB != null) {
            try {
                DB.rollback();
            } catch (SQLException e1) {
            }
        }
        closeSession();
        JSONObject outjson = new JSONObject();
        outjson.put("_Success", false);
        String finalMsg = msg;
        if (e != null) {
            String m = e.getMessage();
            if (m != null)
                finalMsg = m;
            Throwable cause = e.getCause();
            if (cause != null) {
                m = cause.getMessage();
                if (m != null)
                    finalMsg = m;
            }
        }
        outjson.put("_ErrorMessage", finalMsg);
        PrintWriter out = null;
        try {
            out = response.getWriter();
        } catch (IOException e1) {
            // ignore
        }
        out.print(outjson);
        out.flush();
        response.setContentType("application/json");
        response.setStatus(200);
    }

    boolean isEmpty(final String str) {
        return (str == null || str.equals(""));
    }


}
