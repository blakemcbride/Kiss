package org.kissweb;

import groovy.lang.Binding;
import groovy.lang.GroovyClassLoader;
import groovy.lang.GroovyShell;

import java.util.Map;

/**
 * Miscellaneous Groovy utilities
 */
public class GroovyUtils {

    /**
     * Private constructor to prevent instantiation of this utility class.
     * All methods in this class are static and should be called directly.
     */
    private GroovyUtils() {
        // Utility class - not meant to be instantiated
    }

    /**
     * Executes the given Groovy code with the specified arguments and returns the result.
     *
     * <p>This method allows dynamic execution of Groovy code provided as a string.
     * Variables can be passed to the Groovy script via a map of arguments, making them
     * accessible within the script's context.
     *
     * @param pgm  the Groovy code to execute, provided as a {@code String}
     * @param args a {@code Map<String, Object>} containing variable names and their values
     *             to be passed into the Groovy script; may be {@code null} if no variables are needed
     * @return the result of the Groovy script execution, which can be any Object returned by the script
     * @throws groovy.lang.GroovyRuntimeException if an error occurs during compilation or execution of the Groovy code
     * @throws IllegalArgumentException if the {@code pgm} parameter is {@code null} or empty
     *
     * @see groovy.lang.GroovyShell#evaluate(String)
     * @see groovy.lang.Binding
     *
     * <div><b>Example Usage:</b></div>
     * <pre>{@code
     * public static void main(String[] args) {
     *     String groovyCode = "return 'Hello, ' + name + '!'";
     *     Map<String, Object> arguments = new HashMap<>();
     *     arguments.put("name", "World");
     *
     *     Object result = runGroovyCode(groovyCode, arguments);
     *     System.out.println(result); // Outputs: Hello, World!
     * }
     * }</pre>
     */
    public static Object runGroovyCode(String pgm, Map<String, Object> args) {

        // Create a custom GroovyClassLoader
        final ClassLoader parentClassLoader = Thread.currentThread().getContextClassLoader();
        GroovyClassLoader customClassLoader = new GroovyClassLoader(parentClassLoader);

        // Create a Binding and set variables
        final Binding binding = new Binding();
        if (args != null)
            for (Map.Entry<String, Object> entry : args.entrySet())
                binding.setVariable(entry.getKey(), entry.getValue());

        // Create a GroovyShell with the custom class loader and binding
        GroovyShell shell = new GroovyShell(customClassLoader, binding);

        // Execute the code
        Object res = shell.evaluate(pgm);

        // Clear references to enable garbage collection
        shell = null;
        customClassLoader.clearCache();
        customClassLoader = null;

        return res;
    }

    /**
     * Executes the given Groovy code and returns the result.
     *
     * <p>This method allows dynamic execution of Groovy code provided as a string,
     * without passing any variables into the script. It is a convenience method
     * that calls {@link #runGroovyCode(String, Map)} with a {@code null} arguments map.
     *
     * @param pgm the Groovy code to execute, provided as a {@code String}
     * @return the result of the Groovy script execution, which can be any {@code Object} returned by the script
     * @throws groovy.lang.GroovyRuntimeException if an error occurs during compilation or execution of the Groovy code
     * @throws IllegalArgumentException if the {@code pgm} parameter is {@code null} or empty
     *
     * @see #runGroovyCode(String, Map)
     *
     * <div><b>Example Usage:</b></div>
     * <pre>{@code
     * public static void main(String[] args) {
     *     String groovyCode = "return 'Hello, World!'";
     *
     *     Object result = runGroovyCode(groovyCode);
     *     System.out.println(result); // Outputs: Hello, World!
     * }
     * }</pre>
     */
    public static Object runGroovyCode(String pgm) {
        return runGroovyCode(pgm, null);
    }
}
