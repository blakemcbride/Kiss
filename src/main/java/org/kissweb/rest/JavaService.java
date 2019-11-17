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
import java.util.*;


import static org.kissweb.rest.MainServlet.getApplicationPath;


/**
 * Author: Blake McBride
 * Date: 5/5/18
 */
class JavaService {

    private static final transient Logger logger = Logger.getLogger(JavaService.class);

    private static final HashMap<String, JavaClassInfo> javaClassCache = new HashMap<>();

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

    @SuppressWarnings("unchecked")
    MainServlet.ExecutionReturn tryJava(MainServlet ms, HttpServletResponse response, String _className, String _method, JSONObject injson, JSONObject outjson) {
        JavaClassInfo ci;
        String fileName = getApplicationPath() + _className.replace(".", "/") + ".java";
        if (ServiceBase.debug)
            System.err.println("Attempting to load " + fileName);
        try {
            ci = loadJavaClass(_className, fileName);
        } catch (ClassNotFoundException e) {
            ms.errorReturn(response, "Class not found: " + e.getMessage(), null);
            System.err.println("Not found");
            return MainServlet.ExecutionReturn.Error;
        } catch (Throwable e) {
            ms.errorReturn(response, e.getMessage(), null);
            System.err.println("Not found");
            return MainServlet.ExecutionReturn.Error;
        }
        if (ci != null) {
            Object instance;
            Method meth;

            if (ServiceBase.debug)
                System.err.println("Found");
            try {
                instance = ci.jclass.newInstance();
            } catch (Exception e) {
                ms.errorReturn(response, "Error creating instance of of " + fileName, null);
                return MainServlet.ExecutionReturn.Error;
            }
            try {
                if (ServiceBase.debug)
                    System.err.println("Seeking method " + _method);
                meth = ci.jclass.getMethod(_method, JSONObject.class, JSONObject.class, Connection.class, MainServlet.class);
            } catch (NoSuchMethodException e) {
                ms.errorReturn(response, "Method " + _method + " not found in class " + this.getClass().getName(), null);
                System.err.println("Method failed");
                System.err.println(e.getMessage());
                return MainServlet.ExecutionReturn.Error;
            }
            try {
                if (ServiceBase.debug)
                    System.err.println("Evoking method " + _method);
                meth.invoke(instance, injson, outjson, ms.DB, ms);
            } catch (Exception e) {
                ms.errorReturn(response, fileName + " " + _method + "()", e);
                System.err.println("Method failed");
                System.err.println(e.getMessage());
                return MainServlet.ExecutionReturn.Error;
            }
            if (ServiceBase.debug)
                System.err.println("Method completed successfully");
            return MainServlet.ExecutionReturn.Success;
        }
        System.err.println("Not found or not loaded");
        return MainServlet.ExecutionReturn.NotFound;
    }

    private static class CustomClassLoader extends ClassLoader {
        public CustomClassLoader(ClassLoader parent) {
            super(parent);
        }

    }

    private synchronized static JavaClassInfo loadJavaClass(String className, String fileName) throws Exception {
        Class jclass;
        JavaClassInfo ci;
        if (javaClassCache.containsKey(fileName)) {
            ci = javaClassCache.get(fileName);
            /* This must be done by checking the file date rather than a directory change watcher for two reasons:
                1) directory change watchers don't work on sub-directories
                2) there is no notification for file moves
             */
            long lastModified = ((new File(fileName)).lastModified());
            if (lastModified == 0L) {
                javaClassCache.remove(fileName);
                logger.error(fileName + " not found");
                return null;
            }
            if (lastModified == ci.lastModified) {
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
            dc.addSource(className, code);
            Map<String, Class<?>> compiled = dc.build();
            jclass = compiled.get(className.replace('/', '.'));

            javaClassCache.put(fileName, ci = new JavaClassInfo(jclass, (new File(fileName)).lastModified()));
        } catch (FileNotFoundException | NoSuchFileException e) {
            logger.error("File " + fileName + " not found", e);
            return null;
        } catch (Exception e) {
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
