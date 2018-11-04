package org.kissweb.rest;

import org.kissweb.database.Connection;
import org.apache.log4j.Logger;
import org.json.JSONObject;

import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.io.FileNotFoundException;
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
        public GroovyClass gclass;
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

    MainServlet.ExecutionReturn internalGroovy(MainServlet ms, HttpServletResponse response, String _package, String _className, String _method) {
        GroovyClassInfo ci;
        final String _fullClassPath = _package != null ? _package + "." + _className : _className;
        String fileName = ServiceBase.getApplicationPath() + "/" + _fullClassPath.replace(".", "/") + ".groovy";
        ci = loadGroovyClass(fileName, false);
        if (ci != null) {
            Class[] ca = {
            };

            try {
                @SuppressWarnings("unchecked")
                Method methp = ci.gclass.getMethod(_method, ca);
                if (methp == null) {
                    ms.errorReturn(response, "Method " + _method + " not found in class " + this.getClass().getName(), null);
                    return MainServlet.ExecutionReturn.Error;
                }
                try {
                    ci.executing++;
                    methp.invoke(null);
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

    MainServlet.ExecutionReturn tryGroovy(MainServlet ms, HttpServletResponse response, String _className, String _method, JSONObject injson, JSONObject outjson) {
        GroovyClassInfo ci;
        String fileName = ServiceBase.getApplicationPath() + _className.replace(".", "/") + ".groovy";
        if (ServiceBase.debug)
            System.err.println("Attempting to load " + fileName);
        ci = loadGroovyClass(fileName, false);
        if (ci != null) {
            if (ServiceBase.debug)
                System.err.println("Found");
            try {
                ci.executing++;
                Object instance;
                try {
                    instance = ci.gclass.invokeConstructor();
                } catch (Exception e) {
                    ms.errorReturn(response, "Error creating instance of of " + fileName, null);
                    return MainServlet.ExecutionReturn.Error;
                }

                Method meth;

                try {
                    if (ServiceBase.debug)
                        System.err.println("Searching for method " + _method);
                    meth = ci.gclass.getMethod(_method, JSONObject.class, JSONObject.class, Connection.class, MainServlet.class);
                } catch (Exception e) {
                    ms.errorReturn(response, fileName + " " + _method + "()", e);
                    if (ServiceBase.debug) {
                        System.err.println("Method not found");
                        System.err.println(e.getMessage());
                    }
                    return MainServlet.ExecutionReturn.Error;
                }


                try {
                    if (ServiceBase.debug)
                        System.err.println("Evoking method " + _method);
                    meth.invoke(instance, injson, outjson, ms.DB, ms);
                } catch (Exception e) {
                    ms.errorReturn(response, fileName + " " + _method + "()", e);
                    if (ServiceBase.debug) {
                        System.err.println("Method failed");
                        System.err.println(e.getMessage());
                    }
                    return MainServlet.ExecutionReturn.Error;
                }
                if (ServiceBase.debug)
                    System.err.println("Method completed successfully");
                return MainServlet.ExecutionReturn.Success;
            } finally {
                ci.executing--;
            }
        }
        if (ServiceBase.debug)
            System.err.println("Not found");
        return MainServlet.ExecutionReturn.NotFound;
    }

    private synchronized static GroovyClassInfo loadGroovyClass(String fileName, boolean report) {
        GroovyClass gclass;
        GroovyClassInfo ci;
        if (groovyClassCache.containsKey(fileName)) {
            ci = groovyClassCache.get(fileName);
            /* This must be done by checking the file date rather than a directory change watcher for two reasons:
                1) directory change watchers don't work on sub-directories
                2) there is no notification for file moves
             */
            if (((new File(fileName)).lastModified()) == ci.lastModified) {
                ci.lastAccess = (new Date()).getTime() / 1000L;
                cleanGroovyCache();
                return ci;
            }
            groovyClassCache.remove(fileName);
        }
        cleanGroovyCache();
        try {
            GroovyClass.reset();
            gclass = new GroovyClass(false, fileName);
            groovyClassCache.put(fileName, ci = new GroovyClassInfo(gclass, (new File(fileName)).lastModified()));
        } catch (FileNotFoundException e) {
            if (report)
                logger.error("File " + fileName + " not found", e);
            return null;
        } catch (Exception e) {
            if (report)
                logger.error("Error loading " + fileName, e);
            return null;
        }
        return ci;
    }

    private static void cleanGroovyCache() {
        long current = (new Date()).getTime() / 1000L;
        if (current - GroovyClassInfo.cacheLastChecked > MainServlet.CheckCacheDelay) {
            ArrayList<String> keys = new ArrayList<>();
            for (Map.Entry<String, GroovyClassInfo> itm : groovyClassCache.entrySet()) {
                GroovyClassInfo ci = itm.getValue();
                if (ci.executing > 0)
                    ci.lastAccess = current;
                else if (current - ci.lastAccess > MainServlet.MaxHold)
                    keys.add(itm.getKey());
            }
            for (String key : keys)
                groovyClassCache.remove(key);
            GroovyClassInfo.cacheLastChecked = current;
        }
    }

}
