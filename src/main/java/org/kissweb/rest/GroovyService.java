package org.kissweb.rest;

import org.kissweb.StringUtils;
import org.kissweb.database.Connection;
import org.apache.log4j.Logger;
import org.json.JSONObject;

import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.lang.reflect.Method;
import java.util.*;

/**
 * Author: Blake McBride
 * Date: 5/5/18
 */
public class GroovyService {

    private static final transient Logger logger = Logger.getLogger(GroovyService.class);

    private static final HashMap<String, GroovyClassInfo> groovyClassCache = new HashMap<>();

    private static class GroovyClassInfo {
        static long cacheLastChecked = 0;   // last time cache unload checked
        GroovyClass gclass;
        long lastModified;
        long lastAccess;
        int executing;

        GroovyClassInfo(GroovyClass gc, long lm) {
            gclass = gc;
            lastModified = lm;
            lastAccess = (new Date()).getTime() / 1000L;
            executing = 0;
        }
    }

    /**
     * This is the method that allows Groovy to be used as a scripting language.
     * This method can be used to execute a static or instance method.
     * On the Groovy side, all arguments are received in boxed form.  Groovy
     * must also return a boxed object.
     * <p>
     * On the Java side, boxed or unboxed arguments may be used but a boxed type is always returned.
     * <p>
     * if ignoreMissing is true and the file, class, or method are missing a NULL is returned.
     * If ignoreMissing is false and the file, class, or method are missing an exception is thrown.
     * <p>
     * filePath can be an absolute path or a string containing a "~".  The "~" is replaced with the root of
     * the application.
     *
     * @param ignoreMissing
     * @param filePath
     * @param className
     * @param methodName
     * @param inst          the instance the method is evoked against or null if static method
     * @param args          boxed or unboxed arguments (variable number)
     * @return The boxed value returned by the Groovy method call
     * @throws Exception
     */
    public static Object run(boolean ignoreMissing, String filePath, String className, String methodName, Object inst, Object... args) throws Exception {
        String rootPath = MainServlet.getApplicationPath();
        rootPath = StringUtils.drop(rootPath, -1);  //  drop the trailing slash
        if (filePath == null || filePath.isEmpty())
            filePath = "~";
        filePath = filePath.replace("~", rootPath);
        final String fileName = filePath + "/" + className + ".groovy";
        if (ignoreMissing && !(new File(fileName)).exists())
            return null;
        final GroovyClassInfo ci = loadGroovyClass(fileName);
        Method methp;
        if (ci == null) {
            if (ignoreMissing)
                return null;
            throw new Exception("Groovy file " + fileName + " not found.");
        }
        Class<?>[] ca = new Class<?>[args.length];
        for (int i = 0; i < args.length; i++)
            ca[i] = args[i].getClass();
        try {
            methp = ci.gclass.getMethod(methodName, ca);
            if (methp == null) {
                if (ignoreMissing)
                    return null;
                throw new Exception();
            }
        } catch (Exception e) {
            throw new Exception("Method " + methodName + " not found in Groovy file " + fileName, e);
        }
        try {
            return methp.invoke(inst, args);
        } catch (Exception e) {
            throw new Exception("Error executing method " + methodName + " of Groovy file " + fileName, e);
        }
    }

    /**
     * Execute a Groovy constructor.
     *
     * @param relativePath
     * @param className
     * @param args
     * @return
     * @throws Exception
     */
    public static Object constructor(String relativePath, String className, Object... args) throws Exception {
        String rootPath = MainServlet.getApplicationPath();
        final String fileName = rootPath + "/" + (relativePath != null && !relativePath.isEmpty() ? relativePath + "/" : "") + className + ".groovy";
        final GroovyClassInfo ci = loadGroovyClass(fileName);
        if (ci == null)
            throw new Exception("Groovy file " + fileName + " not found.");
        return ci.gclass.invokeConstructor(args);
    }

    ProcessServlet.ExecutionReturn internalGroovy(ProcessServlet ms, HttpServletResponse response, String _package, String _className, String _method) {
        final String _fullClassPath = _package != null ? _package + "." + _className : _className;
        final String fileName = MainServlet.getApplicationPath() + "/" + _fullClassPath.replace(".", "/") + ".groovy";
        final GroovyClassInfo ci = loadGroovyClass(fileName);
        if (ci != null) {
            Class<?>[] ca = {
            };

            try {
                @SuppressWarnings("unchecked")
                Method methp = ci.gclass.getMethod(_method, ca);
                if (methp == null) {
                    if (ms != null)
                        ms.errorReturn(response, "Method " + _method + " not found in class " + this.getClass().getName(), null);
                    return ProcessServlet.ExecutionReturn.Error;
                }
                try {
                    ci.executing++;
                    methp.invoke(null);
                } finally {
                    ci.executing--;
                }
                return ProcessServlet.ExecutionReturn.Success;
            } catch (Exception e) {
                if (ms != null)
                    ms.errorReturn(response, "Error running method " + _method + " in class " + this.getClass().getName(), e);
                return ProcessServlet.ExecutionReturn.Error;
            }
        }
        return ProcessServlet.ExecutionReturn.NotFound;
    }

    ProcessServlet.ExecutionReturn tryGroovy(ProcessServlet ms, HttpServletResponse response, String _className, String _method, JSONObject injson, JSONObject outjson) {
        GroovyClassInfo ci;
        String fileName = MainServlet.getApplicationPath() + _className.replace(".", "/") + ".groovy";
        if (MainServlet.isDebug())
            System.err.println("Attempting to load " + fileName);
        ci = loadGroovyClass(fileName);
        if (ci != null) {
            if (MainServlet.isDebug())
                System.err.println("Found and loaded");
            try {
                ci.executing++;
                Object instance;
                try {
                    instance = ci.gclass.invokeConstructor();
                } catch (Exception e) {
                    ms.errorReturn(response, "Error creating instance of " + fileName, null);
                    return ProcessServlet.ExecutionReturn.Error;
                }

                Method meth;

                try {
                    if (MainServlet.isDebug())
                        System.err.println("Searching for method " + _method);
                    meth = ci.gclass.getMethod(_method, JSONObject.class, JSONObject.class, Connection.class, ProcessServlet.class);
                } catch (Exception e) {
                    ms.errorReturn(response, fileName + " " + _method + "()", e);
                    System.err.println("Method not found");
                    System.err.println(e.getMessage());
                    return ProcessServlet.ExecutionReturn.Error;
                }

                try {
                    if (MainServlet.isDebug())
                        System.err.println("Evoking method " + _method);
                    meth.invoke(instance, injson, outjson, ms.DB, ms);
                } catch (Exception e) {
                    ms.errorReturn(response, fileName + " " + _method + "()", e);
                    System.err.println("Method failed");
                    System.err.println(e.getMessage());
                    return ProcessServlet.ExecutionReturn.Error;
                }
                if (MainServlet.isDebug())
                    System.err.println("Method completed successfully");
                return ProcessServlet.ExecutionReturn.Success;
            } finally {
                ci.executing--;
            }
        }
        System.err.println("Error loading or not found");
        return ProcessServlet.ExecutionReturn.NotFound;
    }

    private synchronized static GroovyClassInfo loadGroovyClass(String fileName) {
        GroovyClass gclass;
        GroovyClassInfo ci;
        if (groovyClassCache.containsKey(fileName)) {
            ci = groovyClassCache.get(fileName);
            /* This must be done by checking the file date rather than a directory change watcher for two reasons:
                1) directory change watchers don't work on sub-directories
                2) there is no notification for file moves
             */
            long lastModified = (new File(fileName)).lastModified();
            if (lastModified == 0L) {
                groovyClassCache.remove(fileName);
                logger.error(fileName + " not found");
                return null;
            }
            if (lastModified == ci.lastModified) {
                ci.lastAccess = (new Date()).getTime() / 1000L;
                cleanGroovyCache();
                return ci;
            }
            groovyClassCache.remove(fileName);
        }
        cleanGroovyCache();
        File fyle = new File(fileName);
        if (!fyle.exists()) {
            if (MainServlet.isDebug())
                logger.error(fileName + " not found");
            return null;
        }
        try {
            GroovyClass.reset();
            gclass = new GroovyClass(false, fileName);
            groovyClassCache.put(fileName, ci = new GroovyClassInfo(gclass, fyle.lastModified()));
        } catch (Exception e) {
            logger.error("Error loading " + fileName, e);
            return null;
        }
        return ci;
    }

    private static void cleanGroovyCache() {
        long current = (new Date()).getTime() / 1000L;
        if (current - GroovyClassInfo.cacheLastChecked > ProcessServlet.CheckCacheDelay) {
            ArrayList<String> keys = new ArrayList<>();
            for (Map.Entry<String, GroovyClassInfo> itm : groovyClassCache.entrySet()) {
                GroovyClassInfo ci = itm.getValue();
                if (ci.executing > 0)
                    ci.lastAccess = current;
                else if (current - ci.lastAccess > ProcessServlet.MaxHold)
                    keys.add(itm.getKey());
            }
            for (String key : keys)
                groovyClassCache.remove(key);
            GroovyClassInfo.cacheLastChecked = current;
        }
    }

}
