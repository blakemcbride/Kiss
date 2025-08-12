package org.kissweb.restServer;

import org.kissweb.UserException;
import org.kissweb.ServerException;
import org.kissweb.StringUtils;
import org.kissweb.database.Connection;
import org.apache.log4j.Logger;
import org.kissweb.json.JSONObject;

import jakarta.servlet.http.HttpServletResponse;
import java.io.File;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.*;

/**
 * Author: Blake McBride
 * Date: 5/5/18
 */
public class GroovyService {

    /**
     * Public constructor for GroovyService.
     * Creates a new instance of the GroovyService class.
     */
    public GroovyService() {
        // Default constructor
    }

    private static final Logger logger = Logger.getLogger(GroovyService.class);

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
     * This method is used to obtain a class or instance method from a groovy class.
     * The Groovy file is treated as a microservice.
     * This means you will always get the most current definition of the method.
     * Once the method is obtained, it may be evoked any number of times.
     * <br><br>
     * This method is mainly used in cases where a method will be evoked multiple times.  If it is only going to be evoked once,
     * then the <code>run</code> method (the one that doesn't take the method object) should be used instead.
     *
     * @param ignoreMissing if <code>true</code> ignore missing classes or methods and return null
     * @param filePath relative to the "backend" directory unless it is an absolute path
     * @param className the name of the Groovy class to load
     * @param methodName the name of the method to retrieve
     * @param args  the actual arguments or the argument types (classes)
     * @return the Method object or null if ignoreMissing is true and method not found
     * @throws Exception if the method cannot be found and ignoreMissing is false
     *
     * @see #run(Method, Object, Object...)
     * @see #run(String, String, String, Object, Object...)
     * @see #getMethod(String, String, String, Object...)
     */
    public static Method getMethod(boolean ignoreMissing, String filePath, String className, String methodName, Object... args) throws Exception {
        return getMethod2(ignoreMissing, filePath, className, methodName, args);
    }

    private static Method getMethod2(boolean ignoreMissing, String filePath, String className, String methodName, Object [] args) throws Exception {
        String rootPath = MainServlet.getApplicationPath();
        rootPath = StringUtils.drop(rootPath, -1);  //  drop the trailing slash
        if (filePath == null || filePath.isEmpty())
            filePath = rootPath;
        else if (!filePath.startsWith("/"))
            filePath = rootPath + "/" + filePath;
        final String fileName = filePath + "/" + className + ".groovy";
        if (ignoreMissing && !(new File(fileName)).exists())
            return null;
        final GroovyClassInfo ci = loadGroovyClass(fileName);
        Method methp;
        if (ci == null) {
            if (ignoreMissing)
                return null;
            throw new Exception("Groovy file " + new File(fileName).getAbsolutePath() + " not found.");
        }
        Class<?>[] ca = new Class<?>[args.length];
        for (int i=0 ; i < args.length ; i++) {
            if (args[i] == null)
                ca[i] = Object.class;
            else if (args[i] instanceof Class) {
                // The user is passing a class indicating the class of a null argument
                ca[i] = (Class<?>) args[i];
                args[i] = null;
            } else
                ca[i] = args[i].getClass();
        }
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
        return methp;
    }

    /**
     * This method is used to obtain a method from a groovy class.  The Groovy file is treated as a microservice.
     * This means you will always get the most current definition of the method.  Once the method is obtained, it may be evoked any number of times.
     * <br><br>
     * This method is mainly used in cases where a method will be evoked multiple times.  If it is only going to be evoked once,
     * then the <code>run</code> method (the one that doesn't take the method object) should be used instead.
     * <br><br>
     * On the Groovy side, all arguments are received in boxed form.  Groovy
     * must also return a boxed object.
     *
     * @param filePath relative to the "backend" directory unless it is an absolute path
     * @param className the name of the Groovy class to load
     * @param methodName the name of the method to retrieve
     * @param args  the actual arguments or the argument types (classes)
     * @return the Method object
     * @throws Exception if the method cannot be found or compiled
     *
     * @see #run(Method, Object, Object...)
     * @see #run(String, String, String, Object, Object...)
     * @see #getMethod(boolean, String, String, String, Object...)
     */
    public static Method getMethod(String filePath, String className, String methodName, Object... args) throws Exception {
        return getMethod2(false, filePath, className, methodName, args);
    }

    /**
     * This method is used to run a method on a groovy class.  The method would normally be returned from the
     * <code>getMethod</code> method.
     * <br><br>
     * The calling method may use boxed or unboxed arguments, but a boxed type will always be returned.
     * <br><br>
     * All arguments to the method being executed are received in boxed form.  It
     * must also return a boxed object.
     *
     * @param methp the method to evoke
     * @param inst instance or null if a class method
     * @param args boxed or unboxed
     * @return boxed
     * @throws Exception if method execution fails
     *
     * @see #getMethod(String, String, String, Object...)
     */
    public static Object run(Method methp, Object inst, Object... args) throws Exception {
        return run2(methp, inst, args);
    }

    private static Object run2(Method methp, Object inst, Object [] args) throws Exception {
        try {
            if (args == null) {
                args = new Object[1];
                args[0] = null;
            }
            return methp.invoke(inst, args);
        } catch (Exception e) {
            throw new Exception("Error executing method " + methp.getName() + " of class " + methp.getClass().getName(), e);
        }
    }

    /**
     * This method allows calls to Groovy microservices.
     * It can be used to execute a static or instance methods.
     * <br><br>
     * The calling method may use boxed or unboxed arguments, but a boxed type will always be returned.
     * <br><br>
     * All arguments to the method being executed are received in boxed form.  It
     * must also return a boxed object.
     * <p>
     * If <code>ignoreMissing</code> is <code>true</code> and the file, class, or method are missing a <code>NULL</code> is returned.
     * If <code>ignoreMissing</code> is <code>false</code> and the file, class, or method are missing an exception is thrown.
     * <p>
     * <code>filePath</code> is relative to the <code>backend</code> directory unless it is an absolute path.
     *
     * @param ignoreMissing if true, return null for missing files/classes/methods instead of throwing exception
     * @param filePath relative to the "backend" directory unless it is an absolute path
     * @param className the name of the Groovy class to load
     * @param methodName the name of the method to invoke
     * @param inst  the instance the method is evoked against or null if static method
     * @param args boxed or unboxed arguments (variable number)
     * @return The boxed value returned by the Groovy method call
     * @throws Exception if method execution fails and ignoreMissing is false
     * @see #run(String, String, String, Object, Object...)
     */
    public static Object run(boolean ignoreMissing, String filePath, String className, String methodName, Object inst, Object... args) throws Exception {
        return run2(ignoreMissing, filePath, className, methodName, inst, args);
    }

    private static Object run2(boolean ignoreMissing, String filePath, String className, String methodName, Object inst, Object [] args) throws Exception {
        Method meth = getMethod2(ignoreMissing, filePath, className, methodName, args);
        return run2(meth, inst, args);
    }

    /**
     * This method is used to execute a microservice.
     * It can be used to execute a static or instance method.
     * <br><br>
     * The calling method may use boxed or unboxed arguments, but a boxed type will always be returned.
     * <br><br>
     * All arguments to the method being executed are received in boxed form.  It
     * must also return a boxed object.
     * <br><br>
     * <code>filePath</code> is relative to the <code>backend</code> directory unless it is an absolute path.
     *
     * @param filePath relative to the "backend" directory unless it is an absolute path
     * @param className the name of the Groovy class to load
     * @param methodName the name of the method to invoke
     * @param inst the instance the method is evoked against or null if static method
     * @param args boxed or unboxed arguments (variable number)
     * @return The boxed value returned by the Groovy method call
     * @throws Exception if method execution fails
     * @see #run(boolean, String, String, String, Object, Object...)
     */
    public static Object run(String filePath, String className, String methodName, Object inst, Object... args) throws Exception {
        return run2(false, filePath, className, methodName, inst, args);
    }

    /**
     * Execute a Groovy constructor.
     *
     * @param relativePath the relative path to the Groovy file
     * @param className the name of the Groovy class
     * @param args the constructor arguments
     * @return the new instance created by the constructor
     * @throws Exception if constructor execution fails
     */
    public static Object constructor(String relativePath, String className, Object... args) throws Exception {
        String rootPath = MainServlet.getApplicationPath();
        final String fileName = rootPath + "/" + (relativePath != null && !relativePath.isEmpty() ? relativePath + "/" : "") + className + ".groovy";
        final GroovyClassInfo ci = loadGroovyClass(fileName);
        if (ci == null)
            throw new Exception("Groovy file " + new File(fileName).getAbsolutePath() + " not found.");
        if (args == null) {
            args = new Object[1];
            args[0] = null;
        }
        return ci.gclass.invokeConstructor(args);
    }

    /**
     * Execute some Groovy code unrelated to a web service.
     * Assumes the method take no arguments.
     *
     * @param _package the package name
     * @param _className the class name
     * @param _method the static method name
     * @param args boxed or unboxed arguments (variable number)
     * @return
     */
    ProcessServlet.ExecutionReturn internalGroovy(String _package, String _className, String _method, Object ... args) {
        final String _fullClassPath = _package != null ? _package + "." + _className : _className;
        final String fileName = MainServlet.getApplicationPath() + "/" + _fullClassPath.replace(".", "/") + ".groovy";
        final GroovyClassInfo ci = loadGroovyClass(fileName);
        if (ci != null) {
            Class<?>[] ca = new Class<?>[args.length];
            for (int i=0 ; i < args.length ; i++) {
                if (args[i] == null)
                    ca[i] = Object.class;
                else if (args[i] instanceof Class) {
                    // The user is passing a class indicating the class of a null argument
                    ca[i] = (Class<?>) args[i];
                    args[i] = null;
                } else
                    ca[i] = args[i].getClass();
            }

            try {
                Method methp = ci.gclass.getMethod(_method, ca);
                if (methp == null) {
                    return ProcessServlet.ExecutionReturn.Error;
                }
                try {
                    ci.executing++;
                    methp.invoke(null, args);
                } catch (InvocationTargetException e) {
                    Throwable te = e.getTargetException();
                    logger.error(te);
                    return ProcessServlet.ExecutionReturn.Error;
                } finally {
                    ci.executing--;
                }
                return ProcessServlet.ExecutionReturn.Success;
            } catch (Exception e) {
                return ProcessServlet.ExecutionReturn.Error;
            }
        }
        return ProcessServlet.ExecutionReturn.NotFound;
    }

    ProcessServlet.ExecutionReturn tryGroovy(ProcessServlet ms, HttpServletResponse response, String _className, String _method, JSONObject injson, JSONObject outjson) {
        GroovyClassInfo ci;
        String fileName = MainServlet.getApplicationPath() + _className.replace(".", "/") + ".groovy";
        logger.info("Attempting to load " + fileName);
        ci = loadGroovyClass(fileName);
        if (ci != null) {
            logger.info("Found and loaded");
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
                    logger.info("Searching for method " + _method);
                    meth = ci.gclass.getMethod(_method, JSONObject.class, JSONObject.class, Connection.class, ProcessServlet.class);
                } catch (Exception e) {
                    ms.errorReturn(response, "Error running " + fileName + " " + _method + "()", null);
                    return ProcessServlet.ExecutionReturn.Error;
                }

                try {
                    logger.info("Evoking method " + _method);
                    meth.invoke(instance, injson, outjson, ms.DB, ms);
                } catch (InvocationTargetException e) {
                    Throwable te = e.getTargetException();
                    if (te instanceof UserException) {
                        ms.errorReturn(response, te.getMessage(), te);
                        return ProcessServlet.ExecutionReturn.Error;
                    } else if (te instanceof ServerException) {
                        logger.error(te);
                        ms.errorReturn(response, te.getMessage(), te);
                        return ProcessServlet.ExecutionReturn.Error;
                    }
                    ms.errorReturn(response, te.getMessage(), te);
                    return ProcessServlet.ExecutionReturn.Error;
                } catch (Exception e) {  // Same as KissException
                    Throwable root = (e.getCause() != null) ? e.getCause() : e;
                    logger.error(root);
                    ms.errorReturn(response, fileName + " " + _method + "()", e.getCause());
                    return ProcessServlet.ExecutionReturn.Error;
                }
                logger.info("Method completed successfully");
                return ProcessServlet.ExecutionReturn.Success;
            } finally {
                ci.executing--;
            }
        } else if (new File(fileName).exists()) {
            ms.errorReturn(response, "Error loading: " + _className + ".groovy", null);
            return ProcessServlet.ExecutionReturn.Error;
        }
        return ProcessServlet.ExecutionReturn.NotFound;
    }

    /**
     * Returns the Groovy class defined in the specified file.
     * If the file has not been loaded before or has changed on disk, it is re-loaded and compiled.
     * If it has been loaded before and hasn't changed on disk, the cached version is returned.
     *
     * @param fileName the Groovy file containing the class to be loaded
     * @return a Groovy class object
     */
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
                logger.error(new File(fileName).getAbsolutePath() + " not found");
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
            logger.info(new File(fileName).getAbsolutePath() + " not found");
            return null;
        }
        try {
            GroovyClass.reset();
            gclass = new GroovyClass(false, fileName);
            groovyClassCache.put(fileName, ci = new GroovyClassInfo(gclass, fyle.lastModified()));
        } catch (Exception e) {
            logger.error("Error loading " + new File(fileName).getAbsolutePath(), e);
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
