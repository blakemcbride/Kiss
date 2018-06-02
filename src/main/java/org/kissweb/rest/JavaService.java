package org.kissweb.rest;

import org.dvare.dynamic.compiler.DynamicCompiler;
import org.kissweb.database.Connection;
import org.apache.log4j.Logger;
import org.json.JSONObject;

import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileNotFoundException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.Date;
import java.util.Hashtable;
import java.util.Map;


import static org.kissweb.rest.MainServlet.getApplicationPath;


/**
 * Author: Blake McBride
 * Date: 5/5/18
 */
public class JavaService {

    private static final transient Logger logger = Logger.getLogger(JavaService.class);

    private static final Hashtable<String, JavaClassInfo> javaClassCache = new Hashtable<>();

    private static class JavaClassInfo {
        static long cacheLastChecked = 0;   // last time cache unload checked
        Class jclass;
        long lastModified;
        long lastAccess;
        int executing;

        JavaClassInfo(Class jc, long lm) {
            jclass = jc;
            lastModified = lm;
            lastAccess = (new Date()).getTime() / 1000L;
            executing = 0;
        }
    }

    MainServlet.ExecutionReturn tryJava(MainServlet ms, HttpServletResponse response, String _package, String _className, String _method, JSONObject injson, JSONObject outjson) {
        JavaClassInfo ci;
        final String _fullClassPath = _package != null ? _package + "." + _className : _className;
        String fileName = getApplicationPath() + _fullClassPath.replace(".", "/") + ".java";
        try {
            ci = loadJavaClass(_className, fileName, false);
        } catch (ClassNotFoundException e) {
            ms.errorReturn(response, "Class not found: " + e.getMessage(), null);
            return MainServlet.ExecutionReturn.Error;
        } catch (Exception e) {
            ms.errorReturn(response, e.getMessage(), null);
            return MainServlet.ExecutionReturn.Error;
        }
        if (ci != null) {
            Class[] ca = {
                    JSONObject.class,
                    JSONObject.class,
                    Connection.class,
                    MainServlet.class
            };

            try {
                @SuppressWarnings("unchecked")
                Method methp = ci.jclass.getMethod(_method, ca);
                if (methp == null) {
                    ms.errorReturn(response, "Method " + _method + " not found in class " + this.getClass().getName(), null);
                    return MainServlet.ExecutionReturn.Error;
                }
                try {
                    ci.executing++;
                    methp.invoke(null, injson, outjson, ms.DB, ms);
                } finally {
                    ci.executing--;
                }
                return MainServlet.ExecutionReturn.Success;
            } catch (Exception e) {
                ms.errorReturn(response, "Error running method " + _method + " in class " + this.getClass().getName(), e);
                return MainServlet.ExecutionReturn.Error;
            }
        }
        return MainServlet.ExecutionReturn.NotFound;
    }

    private static class CustomClassLoader extends ClassLoader {
        public CustomClassLoader(ClassLoader parent) {
            super(parent);
        }

    }

    private synchronized static JavaClassInfo loadJavaClass(String className, String fileName, boolean report) throws Exception {
        Class jclass;
        JavaClassInfo ci;
        if (javaClassCache.containsKey(fileName)) {
            ci = javaClassCache.get(fileName);
            /* This must be done by checking the file date rather than a directory change watcher for two reasons:
                1) directory change watchers don't work on sub-directories
                2) there is no notification for file moves
             */
            if (((new File(fileName)).lastModified()) == ci.lastModified) {
                ci.lastAccess = (new Date()).getTime() / 1000L;
                cleanJavaCache();
                return ci;
            }
            javaClassCache.remove(fileName);
        }
        cleanJavaCache();
        try {
            String code = new String(Files.readAllBytes(Paths.get(fileName)), StandardCharsets.UTF_8);

            DynamicCompiler dc = new DynamicCompiler();
            dc.addSource("page1." + className, code);
            Map<String, Class<?>> compiled = dc.build();
            jclass = compiled.get("page1." + className);

            javaClassCache.put(fileName, ci = new JavaClassInfo(jclass, (new File(fileName)).lastModified()));
        } catch (FileNotFoundException | NoSuchFileException e) {
            if (report)
                logger.error("File " + fileName + " not found", e);
            return null;
        } catch (Exception e) {
            if (report)
                logger.error("Error loading " + fileName, e);
            throw e;
        }
        return ci;
    }

    private static void cleanJavaCache() {
        long current = (new Date()).getTime() / 1000L;
        if (current - JavaClassInfo.cacheLastChecked > MainServlet.CheckCacheDelay) {
            ArrayList<String> keys = new ArrayList<>();
            for (Map.Entry<String, JavaClassInfo> itm : javaClassCache.entrySet()) {
                JavaClassInfo ci = itm.getValue();
                if (ci.executing > 0)
                    ci.lastAccess = current;
                else if (current - ci.lastAccess > MainServlet.MaxHold)
                    keys.add(itm.getKey());
            }
            for (String key : keys)
                javaClassCache.remove(key);
            JavaClassInfo.cacheLastChecked = current;
        }
    }

}
