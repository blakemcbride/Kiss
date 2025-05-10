package org.kissweb.restServer;

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


/**
 * Author: Blake McBride
 * Date: 5/5/18
 */
class JavaService {

    private static final Logger logger = Logger.getLogger(JavaService.class);

    private static final HashMap<String, JavaClassInfo> javaClassCache = new HashMap<>();

    private static class JavaClassInfo {
        static long cacheLastChecked = 0;   // last time cache unload checked
        Class<?> jclass;
        long lastModified;
        long lastAccess;
        int executing;

        JavaClassInfo(Class<?> jc, long lm) {
            jclass = jc;
            lastModified = lm;
            lastAccess = (new Date()).getTime() / 1000L;
            executing = 0;
        }
    }

    ProcessServlet.ExecutionReturn tryJava(ProcessServlet ms, HttpServletResponse response, String _className, String _method, JSONObject injson, JSONObject outjson) {
        JavaClassInfo ci;
        String fileName = MainServlet.getApplicationPath() + _className.replace(".", "/") + ".java";
        logger.info("Attempting to load " + fileName);
        try {
            ci = loadJavaClass(_className, fileName);
        } catch (ClassNotFoundException e) {
            ms.errorReturn(response, "Class not found: " + _className, e);
            return ProcessServlet.ExecutionReturn.Error;
        } catch (Throwable e) {
            ms.errorReturn(response, "Error loading class: " + _className + ".java", e);
            return ProcessServlet.ExecutionReturn.Error;
        }
        if (ci != null && ci.jclass != null) {
            Object instance;
            Method meth;

            logger.info("Found");
            try {
                instance = ci.jclass.newInstance();
            } catch (Exception e) {
                ms.errorReturn(response, "Error creating instance of " + fileName, e);
                return ProcessServlet.ExecutionReturn.Error;
            }
            try {
                logger.info("Seeking method " + _method);
                meth = ci.jclass.getMethod(_method, JSONObject.class, JSONObject.class, Connection.class, ProcessServlet.class);
            } catch (NoSuchMethodException e) {
                ms.errorReturn(response, "Method " + _method + " not found in class " + this.getClass().getName(), e);
                return ProcessServlet.ExecutionReturn.Error;
            }
            try {
                logger.info("Evoking method " + _method);
                meth.invoke(instance, injson, outjson, ms.DB, ms);
            } catch (Exception e) {
                ms.errorReturn(response, fileName + " " + _method + "()", e.getCause());
                return ProcessServlet.ExecutionReturn.Error;
            }
            logger.info("Method completed successfully");
            return ProcessServlet.ExecutionReturn.Success;
        }
        return ProcessServlet.ExecutionReturn.NotFound;
    }

    private static class CustomClassLoader extends ClassLoader {
        public CustomClassLoader(ClassLoader parent) {
            super(parent);
        }

    }

    private static Boolean isJava11Flag = null;

    private static boolean isJava11() {
        if (isJava11Flag == null)
            isJava11Flag = !System.getProperty("java.version").startsWith("1.8");
        return isJava11Flag;
    }

    private synchronized static JavaClassInfo loadJavaClass(String className, String fileName) throws Exception {
        Class<?> jclass;
        JavaClassInfo ci;
        if (javaClassCache.containsKey(fileName)) {
            ci = javaClassCache.get(fileName);
            /* This must be done by checking the file date rather than a directory change watcher for two reasons:
                1) directory change watchers don't work on sub-directories
                2) there is no notification for file moves
             */
            long lastModified = (new File(fileName)).lastModified();
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
        if (!(new File(fileName)).exists()) {
            logger.info("File " + fileName + " not found");
            return null;
        }
        try {
            String code = new String(Files.readAllBytes(Paths.get(fileName)), StandardCharsets.UTF_8);

            DynamicCompiler dc = new DynamicCompiler();
            String cname = className.replaceAll("/", ".");
            dc.addSource(cname, code);
            Map<String, Class<?>> compiled = dc.build();
            jclass = compiled.get(cname);

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
        if (current - JavaClassInfo.cacheLastChecked > ProcessServlet.CheckCacheDelay) {
            ArrayList<String> keys = new ArrayList<>();
            for (Map.Entry<String, JavaClassInfo> itm : javaClassCache.entrySet()) {
                JavaClassInfo ci = itm.getValue();
                if (ci.executing > 0)
                    ci.lastAccess = current;
                else if (current - ci.lastAccess > ProcessServlet.MaxHold)
                    keys.add(itm.getKey());
            }
            for (String key : keys)
                javaClassCache.remove(key);
            JavaClassInfo.cacheLastChecked = current;
        }
    }

}
