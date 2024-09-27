package org.kissweb;

import groovy.lang.GroovyClassLoader;
import groovy.lang.GroovyShell;

/**
 * Miscellaneous Groovy utilities
 */
public class GroovyUtils {

    /**
     * Run arbitrary Groovy code.  The context of the Groovy code is erased once execution is complete.
     * An exception is thrown if execution fails.
     *
     * @param pgm the Groovy code to be run
     * @return the result of the executed code
     */
    public static Object runGroovyCode(String pgm) {

        // Create a custom GroovyClassLoader
        ClassLoader parentClassLoader = Thread.currentThread().getContextClassLoader();
        GroovyClassLoader customClassLoader = new GroovyClassLoader(parentClassLoader);

        // Create a GroovyShell with the custom class loader
        GroovyShell shell = new GroovyShell(customClassLoader);

        // Execute the code
        Object res = shell.evaluate(pgm);

        // Clear references to enable garbage collection
        shell = null;
        customClassLoader.clearCache();
        customClassLoader = null;

        return res;
    }
}
