package org.kissweb.restServer;

import org.kissweb.database.Connection;
import org.kissweb.json.JSONObject;

import javax.servlet.http.HttpServletResponse;
import java.lang.reflect.Method;


/**
 * Author: Blake McBride
 * Date: 5/5/18
 */
public class CompiledJavaService {


    ProcessServlet.ExecutionReturn tryCompiledJava(ProcessServlet ms, HttpServletResponse response, String _className, String _method, JSONObject injson, JSONObject outjson) {

        if (true)
            return ProcessServlet.ExecutionReturn.NotFound;   // not done yet


        Class<?> cls = null;
        String dynamicClassPath = MainServlet.getDynamicClassPath();
        if (!ms.isEmpty(dynamicClassPath)) {
            if (dynamicClassPath.charAt(dynamicClassPath.length() - 1) != '/')
                dynamicClassPath += "/";
            String classPath = dynamicClassPath + _className.replace(".", "/") + "/";
            try {
                cls = ProcessServlet.class.getClassLoader().loadClass(_className);
            } catch (Throwable e) {
                // ignore
            }
        }
        if (cls == null)
            try {
                cls = Class.forName(_className + "." + _className);
            } catch (ClassNotFoundException classNotFoundException) {
                // ignore
            }
        if (cls != null) {
            Class<?>[] ca = {
                    JSONObject.class,
                    JSONObject.class,
                    Connection.class,
                    ProcessServlet.class
            };

            Method methp;
            try {
                methp = cls.getMethod(_method, ca);  // had to use tmethp to be able to use @SuppressWarnings
            } catch (Exception e) {
                ms.errorReturn(response, "Java method " + _method + " not found in class " + this.getClass().getName(), e);
                return ProcessServlet.ExecutionReturn.Error;
            }
            try {
                methp.invoke(null, injson, outjson, ms.DB, this);
            } catch (Exception e) {
                ms.errorReturn(response, "Error executing Java method " + _method + " not found in class " + this.getClass().getName(), e);
                return ProcessServlet.ExecutionReturn.Error;
            }
        }
        return ProcessServlet.ExecutionReturn.NotFound;
    }


}
