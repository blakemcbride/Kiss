
package org.kissweb.lisp;

import org.armedbear.lisp.*;
import org.kissweb.restServer.MainServlet;

import java.lang.reflect.Array;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.ListIterator;
import java.util.Set;

/**
 * Utility functions to make it easy to interface between Java and Lisp.
 *
 * @author Blake McBride
 */
public class ABCL {
    private static Interpreter interpreter;
    private static boolean invertCase = false;
    private static Function makeWebServiceArgs;
    private static Function makeRestServiceArgs;
    private static int lispRelease = 0;
    private static String UtilsSource;
    private static String appPath = null;

    /**
     * Initializes the ABCL Lisp interpreter and loads required utility packages.
     */
    public static void init() {
		interpreter = Interpreter.createInstance();
		invertCase();

		if (MainServlet.isUnderIDE())
            UtilsSource = "../core/org/kissweb/lisp/";
		else
            UtilsSource = "../classes/org/kissweb/lisp/";

        installDebuggerHook();
		eval("(let ((error-orig #'error))"
				+ "  (handler-bind ((style-warning #'(lambda (c)"
				+ "                                    (declare (ignore c))"
				+ "                                    (muffle-warning))))"
				+ "    (defun error (&rest a)"
				+ "      (let ()"
				+ "        (apply error-orig a)))))");


		load(UtilsSource + "clos-utils");
		load(UtilsSource + "package-lru");
		load(UtilsSource + "utils");
		load(UtilsSource + "mappings");
        //makeWebServiceArgs = findLispFunction("MAPPINGS", "make-web-service-args"); // these lines are repeated in multiple places
        makeRestServiceArgs = ABCL.findLispFunction("MAPPINGS", "make-rest-service-args");
    }

	private static void invertCase() {
	    interpreter.eval("(setf (readtable-case *readtable*) :invert)");  // make lisp case sensitive
		invertCase = true;
	}

	/**
	 * Fixes the case of a Lisp symbol based on the current readtable case settings.
	 * @param symbol the symbol to fix
	 * @return the symbol with appropriate case conversion
	 */
	public static String fixCase(String symbol) {
		if (invertCase) {
			int ucl = 0, lcl = 0;
			char[] vec = symbol.toCharArray();
			for (int i=0 ; i < vec.length && (ucl == 0  ||  lcl == 0) ; i++)
				if (Character.isUpperCase(vec[i]))
					ucl++;
				else if (Character.isLowerCase(vec[i]))
					lcl++;
			if (ucl != 0  &&  lcl != 0  ||  ucl == 0  &&  lcl == 0)
				return symbol;
			else
				if (ucl != 0)
					return symbol.toLowerCase();
				else
					return symbol.toUpperCase();
		} else
			return symbol.toUpperCase();
	}

	/**
	 * Deletes a Lisp package if it exists.
	 * @param pkg the name of the package to delete
	 */
	public static void deletePackage(String pkg) {
        if (interpreter == null)
            return;
        try {
            interpreter.eval("(delete-package \"" +  pkg + "\")");
        } catch (Throwable t) {
        }
    }

	/**
	 * Resets the Lisp environment by deleting utility packages and reloading them.
	 */
	public static void reset() {
		if (interpreter == null)
			return;
		try {
			interpreter.eval("(delete-package \"MAPPINGS\")");
		} catch (Throwable t) {
		}
		if (interpreter == null)
			return;
		try {
			interpreter.eval("(delete-package \"UTILS\")");
		} catch (Throwable t) {
		}
		if (interpreter == null)
			return;
		try {
			interpreter.eval("(delete-package \"PACKAGE-LRU\")");
		} catch (Throwable t) {
		}
		if (interpreter == null)
			return;
		try {
			interpreter.eval("(delete-package \"CLOS-UTILS\")");
		} catch (Throwable t) {
		}
        load(UtilsSource + "clos-utils");
        load(UtilsSource + "package-lru");
        load(UtilsSource + "utils");
        load(UtilsSource + "mappings");
		//makeWebServiceArgs = findLispFunction("MAPPINGS", "make-web-service-args"); // these lines are repeated in multiple places
        makeRestServiceArgs = ABCL.findLispFunction("MAPPINGS", "make-rest-service-args");
	}

	/**
	 * Loads a Lisp file.
	 * @param fileName the name of the file to load
	 * @return the result of the load operation
	 */
	public static LispObject load(String fileName) {
	    return eval("(load \"" + getAppPath() + fileName + "\")");
	}

	/**
	 * Compiles a Lisp file.
	 * @param fileName the name of the file to compile
	 * @return the result of the compile operation
	 */
	public static LispObject compileFile(String fileName) {
		return eval("(compile-file \"" + getAppPath() + fileName + "\")");
	}

	/**
	 * Loads a Lisp package from a file using the package-lru system.
	 * @param lispPackage the name of the Lisp package
	 * @param fileName the file to load the package from
	 * @throws Exception if the package fails to load
	 */
	public static void loadPackage(String lispPackage, String fileName) throws Exception {
		try {
			eval("(package-lru:load-package \"" + lispPackage + "\" \"" + getAppPath() + fileName + "\")");
		} catch (Throwable t) {
			// Convert Throwable to Exception
			throw new Exception("Error loading lisp file " + fileName, t);
		}
	}

	/**
	 * Marks a package as done loading and optionally unloads it based on IDE mode.
	 * @param lispPackage the name of the Lisp package
	 */
	public static void packageDone(String lispPackage) {
		if (MainServlet.isUnderIDE())
			eval("(package-lru:package-done-unload \"" + lispPackage + "\")");
		else
			eval("(package-lru:package-done \"" + lispPackage + "\")");
	}

    /**
     * Unloads a Lisp package.
     * @param lispPackage the name of the Lisp package to unload
     */
    public static void packageUnload(String lispPackage) {
	    eval("(package-lru:package-done-unload \"" + lispPackage + "\")");
    }

    /**
     * Evaluates a Lisp expression from a string.
     * @param str the Lisp expression to evaluate
     * @return the result of the evaluation
     */
    public static LispObject eval(String str) {
        return interpreter.eval(str);
    }

    /**
     * Finds a Lisp function in the specified package.
     * @param packageName the name of the package containing the function
     * @param funName the name of the function to find
     * @return the Function object
     * @throws RuntimeException if the package or function is not found
     */
    public static Function findLispFunction(String packageName, String funName) {
        if (packageName == null  ||  packageName.isEmpty())
            packageName = "CL-USER";
//        else
//            packageName = fixCase(packageName);
        org.armedbear.lisp.Package lispPackage = Packages.findPackage(packageName);
		if (lispPackage == null)
			throw new RuntimeException("Package " + packageName + " not found");
        Symbol symbol = lispPackage.findAccessibleSymbol(fixCase(funName));
		if (symbol == null)
			throw new RuntimeException("Symbol " + packageName + ":" + fixCase(funName) + " not found");
        Function fun = (Function) symbol.getSymbolFunction();
        return fun;
    }

    /**
     * Executes a Lisp function with the given arguments.
     * @param fun the Lisp function to execute
     * @param args the arguments to pass to the function
     * @return the result of the function execution
     */
    public static LispObject executeLispFunction(Function fun, Object ... args) {
        LispObject[] jargs;
        jargs = new LispObject[args.length];
        for (int i=0 ; i < args.length ; i++)
            jargs[i] = JavaObject.getInstance(args[i], true);
        return fun.execute(jargs);
    }

    /**
     * Finds and executes a Lisp function with the given arguments.
     * @param packageName the name of the package containing the function
     * @param funName the name of the function to execute
     * @param args the arguments to pass to the function
     * @return the result of the function execution, or null if function not found
     */
    public static LispObject executeLisp(String packageName, String funName, Object ... args) {
        Function fun = findLispFunction(packageName, funName);
		if (fun == null)
			return null;
        LispObject[] jargs;
        jargs = new LispObject[args.length];
        for (int i=0 ; i < args.length ; i++)
            jargs[i] = JavaObject.getInstance(args[i], true);
        return fun.execute(jargs);
    }

    /**
     * Finds and executes a Lisp function with arguments provided as an array.
     * @param packageName the name of the package containing the function
     * @param funName the name of the function to execute
     * @param args the array of arguments to pass to the function
     * @return the result of the function execution, or null if function not found
     */
    public static LispObject executeLispArray(String packageName, String funName, Object [] args) {
        Function fun = findLispFunction(packageName, funName);
		if (fun == null)
			return null;
        LispObject[] jargs;
        jargs = new LispObject[args.length];
        for (int i=0 ; i < args.length ; i++)
            jargs[i] = JavaObject.getInstance(args[i], true);
        return fun.execute(jargs);
    }

	/**
	 * Gets the cached make-web-service-args function.
	 * @return the Function object for making web service arguments
	 */
	public static Function getMakeWebServiceArgs() {
		return makeWebServiceArgs;
	}

    /**
     * Gets the cached make-rest-service-args function.
     * @return the Function object for making REST service arguments
     */
    public static Function getMakeRestServiceArgs() {
        return makeRestServiceArgs;
    }

	/**
	 * Converts a LispObject to a corresponding Java object.
	 * @param obj the LispObject to convert
	 * @return the corresponding Java object representation
	 */
	@SuppressWarnings("unchecked")
	public static Object LispObjectToJavaObject(LispObject obj) {
		if (obj.atom())
			if (obj.characterp())
				return obj.princToString().charAt(0);
			else if (obj.stringp())
				return obj.princToString();
			else if (obj.integerp())
				return obj.intValue();
			else if (obj.realp())
				return obj.doubleValue();
			else if (obj.listp())
				return null;
			else if (obj.constantp())
				return true;
			else
				return obj.princToString();
		else if (obj.listp()) {
			LinkedList ll = new LinkedList();
			while (!obj.endp()) {
				ll.addLast(LispObjectToJavaObject(obj.car()));
				obj = obj.cdr();
			}
			return ll;
		} else if (obj.vectorp()) {
			int len = obj.length();
			Object [] vec = new Object[len];
			for (int i=0 ; i < len ; i++)
				vec[i] = LispObjectToJavaObject(obj.AREF(i));
			return vec;
		} else
			return null;
	}

	/**
	 * Converts a Java object to a corresponding LispObject.
	 * @param jobj the Java object to convert
	 * @return the corresponding LispObject representation, or null if conversion not supported
	 */
	public static LispObject JavaObjectToLispObject(Object jobj) {
		if (jobj instanceof Boolean)
			return ((Boolean)jobj) ? Lisp.T : Lisp.NIL;
		else if (jobj instanceof Character)
			return LispCharacter.getInstance((Character)jobj);
		else if (jobj instanceof Short)
			return LispInteger.getInstance((Short)jobj);
		else if (jobj instanceof Integer)
			return LispInteger.getInstance((Integer)jobj);
		else if (jobj instanceof Long)
			return LispInteger.getInstance((Long)jobj);
		else if (jobj instanceof Float)
			return SingleFloat.getInstance((Float)jobj);
		else if (jobj instanceof Double)
			return DoubleFloat.getInstance((Double)jobj);
		else if (jobj instanceof String)
			return new SimpleString((String)jobj);
		else if (jobj instanceof StringBuilder)
			return new SimpleString((StringBuilder)jobj);
		else if (jobj instanceof LinkedList) {
			LispObject lobj = Lisp.NIL;
			ListIterator it = ((LinkedList) jobj).listIterator();
			while (it.hasNext())
				lobj = new Cons(JavaObjectToLispObject(it.next()), lobj);
			return lobj;
		} else if (jobj instanceof Set) {
			LispObject lobj = Lisp.NIL;
			Iterator it = ((Set) jobj).iterator();
			while (it.hasNext())
				lobj = new Cons(JavaObjectToLispObject(it.next()), lobj);
			return lobj;
		} else if (jobj instanceof Array) {
			Array a = (Array) jobj;
			int len = Array.getLength(a);
			SimpleVector vec = new SimpleVector(len);
			for (int i=0 ; i < len ; i++)
				vec.setSlotValue(i, JavaObjectToLispObject(Array.get(a, i)));
			return null;
		}
		return null;
	}

	/**
	 * Prints a stack trace including both Java and Lisp stack information.
	 * @param e the Throwable to print stack trace for
	 */
	public static void printStackTrace(Throwable e) {
		try {
			Function fun = findLispFunction("UTILS", "print-stack-trace");
			if (fun != null) {
				LispObject stackTrace = LispThread.currentThread().backtrace(0);
				if (stackTrace != null && stackTrace != Lisp.NIL) {
					System.err.println("Lisp execution error");
					fun.execute(stackTrace);
				}
			}
		} catch (Throwable t) {
		}
		e.printStackTrace();
	}

	/* disable the debugger. raise a RuntimeException instead */
	private static void installDebuggerHook() {
		Symbol.DEBUGGER_HOOK
				.setSymbolValue(new Function() {
                                        @Override
					public LispObject execute(LispObject c, LispObject h) {
						throw translateCondition(c);}});
	}

	private static RuntimeException translateCondition(LispObject c) {
		return new RuntimeException(
				c instanceof Condition
						? String.format("%s: %s",
						c.princToString(),
						((Condition) c).getConditionReport())
						: c.princToString());
	}

	/**
	 * Gets the current Lisp release number.
	 * @return the Lisp release number
	 */
	//  DO NOT REMOVE THIS METHOD
	public static int getLispRelease() {
		return lispRelease;
	}

	/**
	 * Sets the Lisp release number.
	 * @param lispRelease the Lisp release number to set
	 */
	//  DO NOT REMOVE THIS METHOD
	public static void setLispRelease(int lispRelease) {
		ABCL.lispRelease = lispRelease;
	}


	private static String getAppPath() {
		if (appPath == null)
			appPath = MainServlet.getApplicationPath().replaceAll("\\\\", "/");  //  for Windows
		return appPath;
	}


}
