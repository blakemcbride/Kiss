package org.kissweb.restServer;

import org.kissweb.database.Connection;
import org.apache.log4j.Logger;
import org.kissweb.json.JSONObject;

import javax.servlet.http.HttpServletResponse;
import javax.tools.*;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.OutputStream;
import java.lang.reflect.Method;
import java.net.URI;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Paths;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * Author: Blake McBride
 * Date: 5/5/18
 */
class JavaService {

    private static final Logger logger = Logger.getLogger(JavaService.class);

    private static final HashMap<String, JavaClassInfo> javaClassCache = new HashMap<>();

    private static class JavaClassInfo {
        static long cacheLastChecked = 0;   // last time cache unload checked
        Class<?> jclass;
        long lastModified;
        long lastAccess;
        int executing;

        JavaClassInfo(Class<?> jc, long lm) {
            jclass = jc;
            lastModified = lm;
            lastAccess = (new Date()).getTime() / 1000L;
            executing = 0;
        }
    }

    ProcessServlet.ExecutionReturn tryJava(ProcessServlet ms, HttpServletResponse response, String _className, String _method, JSONObject injson, JSONObject outjson) {
        JavaClassInfo ci;
        String fileName = MainServlet.getApplicationPath() + _className.replace(".", "/") + ".java";
        logger.info("Attempting to load " + fileName);
        try {
            ci = loadJavaClass(_className, fileName);
        } catch (ClassNotFoundException e) {
            ms.errorReturn(response, "Class not found: " + _className, e);
            return ProcessServlet.ExecutionReturn.Error;
        } catch (Throwable e) {
            ms.errorReturn(response, "Error loading class: " + _className + ".java", e);
            return ProcessServlet.ExecutionReturn.Error;
        }
        if (ci != null && ci.jclass != null) {
            Object instance;
            Method meth;

            logger.info("Found");
            try {
                instance = ci.jclass.newInstance();
            } catch (Exception e) {
                ms.errorReturn(response, "Error creating instance of " + fileName, e);
                return ProcessServlet.ExecutionReturn.Error;
            }
            try {
                logger.info("Seeking method " + _method);
                meth = ci.jclass.getMethod(_method, JSONObject.class, JSONObject.class, Connection.class, ProcessServlet.class);
            } catch (NoSuchMethodException e) {
                ms.errorReturn(response, "Method " + _method + " not found in class " + this.getClass().getName(), e);
                return ProcessServlet.ExecutionReturn.Error;
            }
            try {
                logger.info("Evoking method " + _method);
                meth.invoke(instance, injson, outjson, ms.DB, ms);
            } catch (Exception e) {
                ms.errorReturn(response, fileName + " " + _method + "()", e.getCause());
                return ProcessServlet.ExecutionReturn.Error;
            }
            logger.info("Method completed successfully");
            return ProcessServlet.ExecutionReturn.Success;
        }
        return ProcessServlet.ExecutionReturn.NotFound;
    }

    private synchronized static JavaClassInfo loadJavaClass(String className, String fileName) throws Exception {
        Class<?> jclass;
        JavaClassInfo ci;
        if (javaClassCache.containsKey(fileName)) {
            ci = javaClassCache.get(fileName);
            /* This must be done by checking the file date rather than a directory change watcher for two reasons:
                1) directory change watchers don't work on sub-directories
                2) there is no notification for file moves
             */
            long lastModified = (new File(fileName)).lastModified();
            if (lastModified == 0L) {
                javaClassCache.remove(fileName);
                logger.error(fileName + " not found");
                return null;
            }
            if (lastModified == ci.lastModified) {
                ci.lastAccess = (new Date()).getTime() / 1000L;
                cleanJavaCache();
                return ci;
            }
            javaClassCache.remove(fileName);
        }
        cleanJavaCache();
        if (!(new File(fileName)).exists()) {
            logger.info("File " + fileName + " not found");
            return null;
        }
        try {
            String code = new String(Files.readAllBytes(Paths.get(fileName)), StandardCharsets.UTF_8);
            String binName  = toBinaryName(new File(fileName), code);
            JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
            InMemoryClassFileManager fileManager = new InMemoryClassFileManager(compiler.getStandardFileManager(null, null, null));
            JavaFileObject sourceObject = new JavaSourceFromString(binName, code);

            String classpath = buildClassPath();
            Iterable<String> options = Arrays.asList("-classpath", classpath);
            Boolean result = compiler.getTask(null, fileManager, null, options, null, Collections.singletonList(sourceObject)).call();

            if (!result) {
                logger.error("Error compiling " + fileName);
                return null;
            }
            byte[] classBytes = fileManager.getClassBytes();
            InMemoryClassLoader classLoader = new InMemoryClassLoader(binName, classBytes);
            jclass = classLoader.loadClass(binName);

            javaClassCache.put(fileName, ci = new JavaClassInfo(jclass, (new File(fileName)).lastModified()));
        } catch (FileNotFoundException | NoSuchFileException e) {
            logger.error("File " + fileName + " not found", e);
            return null;
        } catch (Exception e) {
            logger.error("Error loading " + fileName, e);
            throw e;
        }
        return ci;
    }

    private static void cleanJavaCache() {
        long current = (new Date()).getTime() / 1000L;
        if (current - JavaClassInfo.cacheLastChecked > ProcessServlet.CheckCacheDelay) {
            ArrayList<String> keys = new ArrayList<>();
            for (Map.Entry<String, JavaClassInfo> itm : javaClassCache.entrySet()) {
                JavaClassInfo ci = itm.getValue();
                if (ci.executing > 0)
                    ci.lastAccess = current;
                else if (current - ci.lastAccess > ProcessServlet.MaxHold)
                    keys.add(itm.getKey());
            }
            for (String key : keys)
                javaClassCache.remove(key);
            JavaClassInfo.cacheLastChecked = current;
        }
    }

    // Custom Java source object
    private static class JavaSourceFromString extends SimpleJavaFileObject {
        final String code;

        JavaSourceFromString(String name, String code) {
            super(URI.create("string:///" + name.replace('.', '/') + Kind.SOURCE.extension), Kind.SOURCE);
            this.code = code;
        }

        public CharSequence getCharContent(boolean ignoreEncodingErrors) {
            return code;
        }
    }

    // In-memory class file manager
    private static class InMemoryClassFileManager extends ForwardingJavaFileManager<JavaFileManager> {
        private InMemoryClassFileObject classFileObject;

        protected InMemoryClassFileManager(JavaFileManager fileManager) {
            super(fileManager);
        }

        public JavaFileObject getJavaFileForOutput(Location location, String className, JavaFileObject.Kind kind, FileObject sibling) {
            classFileObject = new InMemoryClassFileObject(className, kind);
            return classFileObject;
        }

        public byte[] getClassBytes() {
            return classFileObject.getBytes();
        }
    }

    // In-memory class file object
     private static class InMemoryClassFileObject extends SimpleJavaFileObject {
        private final ByteArrayOutputStream outputStream = new ByteArrayOutputStream();

        protected InMemoryClassFileObject(String name, Kind kind) {
            super(URI.create("mem:///" + name.replace('.', '/') + kind.extension), kind);
        }

        public OutputStream openOutputStream() {
            return outputStream;
        }

        public byte[] getBytes() {
            return outputStream.toByteArray();
        }
    }

    // In-memory class loader
    private static class InMemoryClassLoader extends ClassLoader {
        private final String name;
        private final byte[] bytes;

        public InMemoryClassLoader(String name, byte[] bytes) {
            super(InMemoryClassLoader.class.getClassLoader());
            this.name = name;
            this.bytes = bytes;
        }

        protected Class<?> findClass(String className) throws ClassNotFoundException {
            if (!className.equals(name)) throw new ClassNotFoundException();
            return defineClass(className, bytes, 0, bytes.length);
        }
    }

    private static String buildClassPath() {
        String sep = File.pathSeparator;
        StringBuilder cp = new StringBuilder();

        // 1. Whatever the JVM started with
        cp.append(System.getProperty("java.class.path"));

        // 2. Everything visible to the context ClassLoader (usually web-app jars)
        collectURLs(Thread.currentThread().getContextClassLoader())
                .forEach(u -> cp.append(sep).append(new File(u.getPath())));

        // 3. Everything sitting in ./libs  (optional)
        File libs = new File("libs");
        if (libs.isDirectory()) {
            for (File f : Objects.requireNonNull(libs.listFiles((d, n) -> n.endsWith(".jar")))) {
                cp.append(sep).append(f.getAbsolutePath());
            }
        }
        return cp.toString();
    }

    private static List<URL> collectURLs(ClassLoader cl) {
        List<URL> urls = new ArrayList<>();
        while (cl != null) {
            if (cl instanceof java.net.URLClassLoader) {
                urls.addAll(Arrays.asList(((java.net.URLClassLoader) cl).getURLs()));
            }
            cl = cl.getParent();
        }
        return urls;
    }

    private static String toBinaryName(File javaFile, String sourceCode) {
        // try to read the package declaration
        Matcher m = Pattern.compile("^\\s*package\\s+([\\w\\.]+)\\s*;", Pattern.MULTILINE)
                .matcher(sourceCode);
        String pkg = m.find() ? m.group(1) + "." : "";
        String simple = javaFile.getName().replaceFirst("\\.java$", "");
        return pkg + simple;                 // e.g. services.MyJavaService
    }


}
