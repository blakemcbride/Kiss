/*
 *  Copyright 2006-2013 Blake McBride
 *  All Rights Reserved.
*/

package org.kissweb.restServer;

import groovy.lang.GroovyClassLoader;
import groovy.lang.GroovyCodeSource;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

/**
 *
 * @author Blake McBride
 *  <br><br>
 * This class deals with dynamically loading groovy source files.
 * <br><br>
 * If you load the same .groovy file with the same GroovyClassLoader groovy
 * just uses the original file and doesn't actually re-load the source.
 * If you create a new GroovyClassLoader then groovy re-loads the groovy
 * source file.
 */
public class GroovyClass {

	private static GroovyClassLoader groovyLoader;
	
	private final Class<?> groovyClass;

	/**
	 * Load a groovy source file.  If the file was previously loaded the old version will be
	 * used to avoid a duplicate load.  If a reload is desired, call reset() first.
	 * <br><br>
	 * If calculatePath is true, the path to sourceFile is calculated relative to the running system.
	 * If it is false, sourceFile is used as-is.
	 * <br><br>
	 * A notable exception thrown by this method is FileNotFoundException.  That occurs if the source
	 * file is not found.  Other exceptions indicate a problem with the file.
	 * 
	 * @param calculatePath whether to calculate the full path to the source file
	 * @param sourceFile the path to the Groovy source file
	 * @throws Exception if the file cannot be found or compiled 
	 */
	public GroovyClass(boolean calculatePath, String sourceFile) throws Exception {
		String path;
		if (calculatePath)
			path = MainServlet.getApplicationPath();
		else
			path = "";

		if (!sourceFile.endsWith(".groovy"))
			sourceFile += ".groovy";

		if (groovyLoader == null)
			groovyLoader = new GroovyClassLoader(GroovyClass.class.getClassLoader());
		groovyClass = groovyLoader.parseClass(new GroovyCodeSource(new File(path + sourceFile)), false);
	}
	
	/**
	 * This method causes groovy to re-load groovy source files the next time they are used.
	 * Be careful with this.  If one Groovy file causes the load of a second Groovy file,
	 * and the second file causes a reload of the groovyLoader, then the first file will have been erased.
	 */
	static void reset() {
		groovyLoader = null;
	}

	/**
	 * Gets a method from the compiled Groovy class.
	 *
	 * @param methodName the name of the method to retrieve
	 * @param argTypes the parameter types of the method
	 * @return the Method object
	 * @throws Exception if the method cannot be found
	 */
	public Method getMethod(String methodName, Class<?>... argTypes) throws Exception {
		return groovyClass.getMethod(methodName, argTypes);
	}

	private Method getMethod(String methodName, Object... args) throws Exception {
		Class<?>[] argTypes = new Class[args.length];
		for (int i = 0; i < args.length; i++)
			argTypes[i] = args[i].getClass();  //   Object.class;

		return groovyClass.getMethod(methodName, argTypes);
	}
		
	/**
	 * Invokes a static method from a Groovy class.
	 *
	 * @param calculatePath whether to calculate the full path to the source file
	 * @param sourceFile the path to the Groovy source file
	 * @param methodName the name of the method to invoke
	 * @param instance the instance to invoke the method on (or null for static methods)
	 * @param args the arguments to pass to the method
	 * @return the result of the method invocation
	 * @throws Exception if the method cannot be found or invoked
	 */
	public static Object invoke(boolean calculatePath, String sourceFile, String methodName, Object instance, Object ... args) throws Exception {
		GroovyClass groovyClass = new GroovyClass(calculatePath, sourceFile);
		Method methp = groovyClass.getMethod(methodName, args);
		return methp.invoke(instance, args);
	}
		
	/**
	 * Invokes a method from the compiled Groovy class.
	 *
	 * @param methodName the name of the method to invoke
	 * @param instance the instance to invoke the method on (or null for static methods)
	 * @param args the arguments to pass to the method
	 * @return the result of the method invocation
	 * @throws Exception if the method cannot be found or invoked
	 */
	public Object invoke(String methodName, Object instance, Object ... args) throws Exception {
	    Method methp;
	    try {
            methp = getMethod(methodName, args);
        } catch (Exception e) {
            throw new Exception("Can't find method named " + methodName);
        }
		return methp.invoke(instance, args);
	}
		
	/**
	 * Gets a constructor from the compiled Groovy class.
	 *
	 * @param argTypes the parameter types of the constructor
	 * @return the Constructor object
	 * @throws Exception if the constructor cannot be found
	 */
	public Constructor<?> getConstructor(Class<?> ... argTypes) throws Exception {
		return groovyClass.getConstructor(argTypes);
	}
		
	private Constructor<?> getConstructor(Object... args) throws Exception {
		Class<?>[] argTypes = new Class[args.length];
		for (int i = 0; i < args.length; i++)
			argTypes[i] = args[i].getClass();  //   Object.class;
		return groovyClass.getConstructor(argTypes);
	}
		
	Object invokeConstructor(Object... args) throws Exception {
		Constructor<?> c = getConstructor(args);
		return c.newInstance(args);
	}

}
