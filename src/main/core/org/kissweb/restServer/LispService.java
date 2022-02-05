package org.kissweb.restServer;

import org.kissweb.lisp.ABCL;
import org.apache.log4j.Logger;
import org.armedbear.lisp.LispObject;
import org.json.JSONObject;

import javax.servlet.http.HttpServletResponse;
import java.io.File;
import java.util.*;


import static org.kissweb.restServer.ProcessServlet.ExecutionReturn;
import static org.kissweb.restServer.ProcessServlet.MaxHold;
import static org.kissweb.restServer.ProcessServlet.CheckCacheDelay;


/**
 * Author: Blake McBride
 * Date: 5/5/18
 */
class LispService {

    private static final transient Logger logger = Logger.getLogger(LispService.class);

    private static final HashMap<String, LispPackageInfo> lispPackageCache = new HashMap<>();

    private static boolean once = true;

    private static class LispPackageInfo {
        static long cacheLastChecked = 0;   // last time cache unload checked
        String packageName;
        String fileName;
        long lastModified;
        long lastAccess;
        int executing;

        LispPackageInfo(String pname, String fname, long lm) {
            packageName = pname;
            fileName = fname;
            lastModified = lm;
            lastAccess = (new Date()).getTime() / 1000L;
            executing = 0;
        }
    }

    ExecutionReturn tryLisp(ProcessServlet ms, HttpServletResponse response, String _className, String _method, JSONObject injson, JSONObject outjson) {
        String lispFileName = _className.replace(".", "/") + ".lisp";
        LispObject args;
        String fileName = MainServlet.getApplicationPath() + lispFileName;

        logger.info("Attempting to load " + fileName);
        if (!(new File(fileName)).exists()) {
            return ExecutionReturn.NotFound;
        }

        try {
            if (once) {
                logger.info("Performing one-time Lisp initialization");
                ABCL.init();
                once = false;
            } else if (MainServlet.isUnderIDE())
                ABCL.reset();
            args = ABCL.executeLispFunction(ABCL.getMakeRestServiceArgs(), injson, outjson, ms.DB, ms);
        } catch (Exception e) {
            ms.errorReturn(response, "", e);
            return ExecutionReturn.Error;
        }

        LispObject lispIn = args.NTH(0);
        LispObject lispOut = args.NTH(1);
        LispObject lispHSU = args.NTH(2);
        LispObject lispThis = args.NTH(3);

        LispPackageInfo res;
        if (!(new File(fileName)).exists()) {
            logger.info("File " + fileName + " not found");
            return ExecutionReturn.NotFound;
        }
        try {
            logger.info("Loading Lisp file");
            res = loadLispFile(_className, lispFileName, true);
        } catch (Exception e) {
            ms.errorReturn(response, "Error loading Lisp " + lispFileName, e);
            res = null;
        }
        if (res == null) {
            ms.errorReturn(response, "Error loading Lisp " + lispFileName, null);
            return ExecutionReturn.Error;
        }

        try {
            res.executing++;
            logger.info("Executing lisp function " + _method);
            ABCL.executeLisp(_className, _method, lispIn, lispOut, lispHSU, lispThis);
        } catch (Exception e) {
            ms.errorReturn(response, "Error executing Lisp " + lispFileName + " " + _method + "()", e.getCause());
            return ExecutionReturn.Error;
        } finally {
            res.executing--;
        }
        logger.info("Execution completed successfully");
        return ExecutionReturn.Success;
    }

    private synchronized static LispPackageInfo loadLispFile(String packageName, String fileName, boolean report) {
        LispPackageInfo ci;
        if (lispPackageCache.containsKey(fileName)) {
            ci = lispPackageCache.get(fileName);
            /* This must be done by checking the file date rather than a directory change watcher for two reasons:
                1) directory change watchers don't work on sub-directories
                2) there is no notification for file moves
             */
            if (((new File(MainServlet.getApplicationPath() + fileName)).lastModified()) == ci.lastModified) {
                ci.lastAccess = (new Date()).getTime() / 1000L;
                cleanLispCache();
                return ci;
            }
            lispPackageCache.remove(fileName);
        }
        cleanLispCache();
        try {
            ABCL.deletePackage(packageName);  // get rid of any old version first so they don't get merged
            ABCL.load(fileName);
            lispPackageCache.put(fileName, ci = new LispPackageInfo(packageName, fileName, (new File(fileName)).lastModified()));
        } catch (Exception e) {
            if (report)
                logger.error("Error loading " + fileName, e);
            return null;
        }
        return ci;
    }

    private static void cleanLispCache() {
        long current = (new Date()).getTime() / 1000L;
        if (current - LispPackageInfo.cacheLastChecked > CheckCacheDelay) {
            ArrayList<String> keys = new ArrayList<>();
            for (Map.Entry<String, LispPackageInfo> itm : lispPackageCache.entrySet()) {
                LispPackageInfo lpi = itm.getValue();
                if (lpi.executing > 0)
                    lpi.lastAccess = current;
                else if (current - lpi.lastAccess > MaxHold)
                    keys.add(itm.getKey());
            }
            for (String key : keys) {
                LispPackageInfo lp = lispPackageCache.get(key);
                ABCL.deletePackage(lp.packageName);
                lispPackageCache.remove(key);
            }
            LispPackageInfo.cacheLastChecked = current;
        }
    }

}
