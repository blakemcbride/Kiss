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
 * Handles dynamic loading and execution of Groovy source files.
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
		return findMethod(groovyClass, methodName, argTypes);
	}

	/**
	 * Finds a public method by name from argument classes.  The exact
	 * <code>Class.getMethod</code> lookup is tried first.  If that fails, the public
	 * methods with the same name and arity are scanned and each parameter must be
	 * assignable from the corresponding argument class (primitives and their boxed
	 * types are treated alike).  The most specific match wins.
	 *
	 * @param cls the class to search
	 * @param methodName the method name
	 * @param argTypes the argument classes (use Object.class for a null argument)
	 * @return the Method
	 * @throws NoSuchMethodException if nothing matches or the match is ambiguous
	 */
	public static Method findMethod(Class<?> cls, String methodName, Class<?>... argTypes) throws NoSuchMethodException {
		try {
			return cls.getMethod(methodName, argTypes);
		} catch (NoSuchMethodException e) {
			Method best = null;
			boolean ambiguous = false;
			for (Method m : cls.getMethods()) {
				if (!m.getName().equals(methodName))
					continue;
				Class<?>[] pt = m.getParameterTypes();
				if (pt.length != argTypes.length)
					continue;
				boolean ok = true;
				for (int i = 0; ok && i < pt.length; i++)
					ok = box(pt[i]).isAssignableFrom(box(argTypes[i]));
				if (!ok)
					continue;
				if (best == null) {
					best = m;
					continue;
				}
				if (moreSpecific(m, best)) {
					best = m;
					ambiguous = false;
				} else if (!moreSpecific(best, m))
					ambiguous = true;
			}
			if (best == null)
				throw e;
			if (ambiguous)
				throw new NoSuchMethodException("Ambiguous call to " + cls.getName() + "." + methodName);
			return best;
		}
	}

	private static boolean moreSpecific(Method a, Method b) {
		Class<?>[] pa = a.getParameterTypes();
		Class<?>[] pb = b.getParameterTypes();
		for (int i = 0; i < pa.length; i++)
			if (!box(pb[i]).isAssignableFrom(box(pa[i])))
				return false;
		return true;
	}

	private static Class<?> box(Class<?> c) {
		if (!c.isPrimitive())
			return c;
		if (c == int.class) return Integer.class;
		if (c == long.class) return Long.class;
		if (c == double.class) return Double.class;
		if (c == boolean.class) return Boolean.class;
		if (c == float.class) return Float.class;
		if (c == short.class) return Short.class;
		if (c == byte.class) return Byte.class;
		if (c == char.class) return Character.class;
		return c;
	}

	private Method getMethod(String methodName, Object... args) throws Exception {
		// Check if any arguments are null
		boolean hasNull = false;
		for (Object arg : args)
			if (arg == null) {
				hasNull = true;
				break;
			}

		if (!hasNull) {
			// Fast path: all arguments are non-null, use exact type lookup
			Class<?>[] argTypes = new Class[args.length];
			for (int i = 0; i < args.length; i++)
				argTypes[i] = args[i].getClass();
			return findMethod(groovyClass, methodName, argTypes);
		}

		// Slow path: one or more arguments are null, search by name and parameter count
		// then verify type compatibility for non-null arguments
		for (Method m : groovyClass.getMethods()) {
			if (!m.getName().equals(methodName))
				continue;
			Class<?>[] paramTypes = m.getParameterTypes();
			if (paramTypes.length != args.length)
				continue;
			boolean match = true;
			for (int i = 0; i < args.length; i++) {
				if (args[i] != null && !paramTypes[i].isInstance(args[i])) {
					match = false;
					break;
				}
			}
			if (match)
				return m;
		}
		throw new NoSuchMethodException(methodName);
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
		// Check if any arguments are null
		boolean hasNull = false;
		for (Object arg : args)
			if (arg == null) {
				hasNull = true;
				break;
			}

		if (!hasNull) {
			// Fast path: all arguments are non-null, use exact type lookup
			Class<?>[] argTypes = new Class[args.length];
			for (int i = 0; i < args.length; i++)
				argTypes[i] = args[i].getClass();
			return groovyClass.getConstructor(argTypes);
		}

		// Slow path: one or more arguments are null, search by parameter count
		// then verify type compatibility for non-null arguments
		for (Constructor<?> c : groovyClass.getConstructors()) {
			Class<?>[] paramTypes = c.getParameterTypes();
			if (paramTypes.length != args.length)
				continue;
			boolean match = true;
			for (int i = 0; i < args.length; i++) {
				if (args[i] != null && !paramTypes[i].isInstance(args[i])) {
					match = false;
					break;
				}
			}
			if (match)
				return c;
		}
		throw new NoSuchMethodException("<init>");
	}
		
	Object invokeConstructor(Object... args) throws Exception {
		Constructor<?> c = getConstructor(args);
		return c.newInstance(args);
	}

}
