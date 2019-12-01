/*
 *  Copyright 2006-2013 Arahant LLC
 *  All Rights Reserved.
*/

package org.kissweb.rest;

import groovy.lang.GroovyClassLoader;
import groovy.lang.GroovyCodeSource;

import java.io.File;
import java.lang.reflect.Constructor;
import java.lang.reflect.Method;

/**
 *
 * @author Blake McBride
 * 
 * This class deals with dynamically loading groovy source files.
 * 
 * If you load the same .groovy file with the same GroovyClassLoader groovy
 * just uses the original file and doesn't actually re-load the source.
 * If you create a new GroovyClassLoader then groovy re-loads the groovy
 * source file.
 */
public class GroovyClass {

	private static GroovyClassLoader groovyLoader;
	
	private final Class groovyClass;

	/**
	 * Load a groovy source file.  If the file was previously loaded the old version will be
	 * used to avoid a duplicate load.  If a reload is desired, call reset() first.
	 * 
	 * If calculatePath is true, the path to sourceFile is calculated relative to the running system.
	 * If it is false, sourceFile is used as-is.
	 * 
	 * A notable exception thrown by this method is FileNotFoundException.  That occurs if the source
	 * file is not found.  Other exceptions indicate a problem with the file.
	 * 
	 * @param calculatePath
	 * @param sourceFile
	 * @throws Exception 
	 */
	GroovyClass(boolean calculatePath, String sourceFile) throws Exception {
		String path;
		if (calculatePath)
			path = ServiceBase.getApplicationPath();
		else
			path = "";

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

	Method getMethod(String methodName, Class... argTypes) throws Exception {
		@SuppressWarnings("unchecked")
		Method methp = groovyClass.getMethod(methodName, argTypes);
		return methp;
	}

	private Method getMethod(String methodName, Object... args) throws Exception {
		Class[] argTypes = new Class[args.length];
		for (int i = 0; i < args.length; i++)
			argTypes[i] = args[i].getClass();  //   Object.class;

		@SuppressWarnings("unchecked")
		Method methp = groovyClass.getMethod(methodName, argTypes);
		return methp;
	}
		
	public static Object invoke(boolean calculatePath, String sourceFile, String methodName, Object instance, Object ... args) throws Exception {
		GroovyClass groovyClass = new GroovyClass(calculatePath, sourceFile);
		Method methp = groovyClass.getMethod(methodName, args);
		return methp.invoke(instance, args);
	}
		
	public Object invoke(String methodName, Object instance, Object ... args) throws Exception {
	    Method methp;
	    try {
            methp = getMethod(methodName, args);
        } catch (Exception e) {
            throw new Exception("Can't find method named " + methodName);
        }
		return methp.invoke(instance, args);
	}
		
	@SuppressWarnings("unchecked")
	public Constructor getConstructor(Class ... argTypes) throws Exception {
		return groovyClass.getConstructor(argTypes);
	}
		
	@SuppressWarnings("unchecked")
	private Constructor getConstructor(Object... args) throws Exception {
		Class[] argTypes = new Class[args.length];
		for (int i = 0; i < args.length; i++)
			argTypes[i] = args[i].getClass();  //   Object.class;
		return groovyClass.getConstructor(argTypes);
	}
		
	Object invokeConstructor(Object... args) throws Exception {
		Constructor c = getConstructor(args);
		return c.newInstance(args);
	}

}
