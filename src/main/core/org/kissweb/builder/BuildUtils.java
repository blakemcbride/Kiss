/*
 * Author: Blake McBride
 * Date: 2/16/20
 *
 * I've found that I sometimes spend more time messing with build programs (such 
 * as Maven, Gradle, and others) than the underlying application I am trying to 
 * build.  They all do the normal things very, very easily.  But when you try to
 * go off their beaten path it gets real difficult real fast.  Being sick and
 * tired of this, and having easily built a shell script to build what I want, I
 * needed a more portable solution.  The files in this directory are that solution.
 *
 * It should be noted, however, that unlike a shell script, this build system 
 * does not execute commands that are already done.  In other words, only the 
 * minimum steps necessary to rebuild a system are actually executed.  So, this 
 * build system runs as fast as the others.
 *
 * There are two classes as follows:
 *
 *     BuildUtils -  the generic utilities needed to build
 *     Tasks      -  the application-specific build procedures (or tasks)
 */

package org.kissweb.builder;

import org.apache.commons.compress.archivers.ArchiveStreamFactory;
import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveInputStream;
import org.apache.commons.compress.utils.IOUtils;

import java.io.*;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Type;
import java.net.URL;
import java.nio.file.*;
import java.util.ArrayList;
import java.util.regex.Pattern;
import java.util.zip.GZIPInputStream;

public class BuildUtils {

    private static final String Version = "1.0";
    private static String CACHE_DIR;
    private static boolean verbose = false;
    public static boolean isWindows;
    public static boolean isLinux;
    public static boolean isMacOS;
    public static boolean isSunOS;  // includes OpenIndiana (only tested on OpenIndiana)
    public static boolean isHaiku;
    public static boolean isFreeBSD;

    public static void build(String [] args, Class<?> tasksClass, Runnable listTasks) throws InstantiationException, IllegalAccessException {
        String osName = System.getProperty("os.name");
        isLinux = osName.startsWith("Linux");
        isMacOS = osName.startsWith("Mac OS X");
        isWindows = osName.startsWith("Windows");
        isSunOS = osName.startsWith("SunOS");  // includes OpenIndiana and Solaris
        isHaiku = osName.startsWith("Haiku");
        isFreeBSD = osName.startsWith("FreeBSD");
        if (args.length < 1)
            args = new String[]{ "help" };
        Object ins = tasksClass.newInstance();
        for (String arg : args)
            switch (arg) {
                case "list-tasks":
                    /*
                    Method [] meths = Tasks.class.getDeclaredMethods();

                    for (Method meth : meths) {
                        Type[] p = meth.getGenericParameterTypes();
                        int mods = meth.getModifiers();
                        if (!Modifier.isPrivate(mods)  &&  p.length == 0)
                            println(meth.getName());
                    }
                     */
                    listTasks.run();
                    println("list-tasks (builtin)");
                    println("help       (builtin)");
                    println("version    (builtin)");
                    break;
                case "version":
                case "--version":
                    println("bld version " + Version);
                    break;
                case "help":
                case "-h":
                case "--help":
                    println("Use: bld  list-tasks         to get a list of tasks");
                    println("Use: bld  [-v]  <task>       to execute the task");
                    println("Use: bld  help               this message");
                    println("Use: bld  version            display version of bld");
                    break;
                case "-v":
                case "--verbose":
                    verbose = true;
                    break;
                default:
                    printlnIfVerbose("running on " + osName);
                    Method meth;
                    try {
                        arg = convertToCamelCase(arg);

                        meth = tasksClass.getDeclaredMethod(arg);
                        int mods = meth.getModifiers();
                        Type[] p = meth.getGenericParameterTypes();
                        if (Modifier.isPrivate(mods)  ||  p.length != 0)
                            throw new NoSuchMethodException("");
                    } catch (NoSuchMethodException e) {
                        printError("can't find task " + arg);
                        return;
                    }

                    try {
                        meth.invoke(ins);
                    } catch (IllegalAccessException | InvocationTargetException e) {
                        if (e.getCause() == null)
                            printError(e.getMessage());
                        else
                            printError(e.getCause().getMessage());
                        printError("error executing task " + arg);
                        return;
                    }
                    break;
            }
        printlnIfVerbose("done");
    }

    private static File findJavaPathWindows(File f) {
        File[] files = f.listFiles();
        if (files != null)
            for (File f2 : files) {
                String name = f2.getName();
                if (name.equalsIgnoreCase("javac.exe"))
                    return f2;
                if (f2.isDirectory()) {
                    File ret = findJavaPathWindows(f2);
                    if (ret != null)
                        return ret;
                }
            }
        return null;  // not found
    }

    public static String getJavaPathOnWindows() {
        String path = System.getenv("JAVA_HOME");
        if (path != null)
            return path;
        File[] files = (new File("C:\\")).listFiles();
        if (files != null)
            for (File f : files)
                if (f.getName().toLowerCase().startsWith("program files")) {
                    File found = findJavaPathWindows(f);
                    if (found != null)
                        return found.getParentFile().getParentFile().getAbsolutePath();
                }
        return null;
    }

    public static String getTomcatPath() {
        return (new File("tomcat")).getAbsolutePath();
    }

    public static boolean exists(String fname) {
        return (new File(fname)).exists();
    }

    public static void download(String fname, String targetDir, String sourcePath) {
        mkdir(targetDir);
        String target = targetDir + File.separator + fname;
        if (!exists(target)) {
            String cache = cacheDir() + fname;
            if (exists(cache)) {
                printlnIfVerbose("copying " + target + " from cache");
                try {
                    Files.copy(Paths.get(cache), Paths.get(target), StandardCopyOption.REPLACE_EXISTING);
                } catch (IOException e) {
                    throw new RuntimeException("error copying file from cache " + fname);
                }
                return;
            }
            try {
                printlnIfVerbose("downloading " + target + " from " + sourcePath);
                try (BufferedInputStream in = new BufferedInputStream(new URL(sourcePath).openStream())) {
                    Path cpath = Paths.get(cache);
                    Files.copy(in, cpath, StandardCopyOption.REPLACE_EXISTING);
                    Files.copy(cpath, Paths.get(target), StandardCopyOption.REPLACE_EXISTING);
                }
            } catch (IOException e) {
                throw new RuntimeException("error downloading " + fname);
            }
        }
    }

    public static String getcwd() {
        return Paths.get(".").toAbsolutePath().normalize().toString();
    }

    static void downloadAll(ForeignDependencies deps) {
        for (ForeignDependency dep : deps.getDependencies())
            download(dep.filename, dep.targetPath, dep.source);
    }

    static void delete(ForeignDependencies deps) {
        for (ForeignDependency dep : deps.getDependencies())
            rm(dep.targetPath + File.separator + dep.filename);
    }

    public static void printlnIfVerbose(String str) {
        if (verbose)
            System.out.println(str == null ? "" : str);
    }

    public static void println(String str) {
        System.out.println(str == null ? "" : str);
    }

    public static void printError(String str) {
        System.err.println(str == null ? "" : str);
    }

    private static void mkdir(File d) {
        if (d.exists()  &&  d.isDirectory())
            return;
        if (d.exists()  &&  !d.isDirectory())
            throw new RuntimeException("mkdir: error creating directory " + d.getAbsolutePath() + "; file with that name exists");
        printlnIfVerbose("creating directory " + d.getAbsolutePath());
        if (!d.mkdirs())
            throw new RuntimeException("mkdir: error creating directory " + d.getAbsolutePath());
    }

    /**
     * Move a file from one location to another.
     *
     * @param from a file name with a path
     * @param to a file name with a path
     */
    public static void move(String from, String to) {
        if (!new File(from).exists())
            throw new RuntimeException("move: file \"" + from + "\" does not exist");
        try {
            File parent = new File(to).getParentFile();
            if (parent != null)
                parent.mkdirs();
            printlnIfVerbose("moving " + from + " to " + to);
            Files.move(Paths.get(from), Paths.get(to), StandardCopyOption.REPLACE_EXISTING);
        } catch (IOException e) {
            throw new RuntimeException("Error moving " + from + " to " + to);
        }
    }

    /**
     * Creates a directory including any missing parent directories.
     *
     * @param dname
     */
    public static void mkdir(String dname) {
        if (dname == null  ||  dname.isEmpty()  ||  dname.equals("."))
            return;
        mkdir(new File(dname));
    }

    /**
     * Copy file only if missing or earlier date.
     *
     * @param source file
     * @param dest  file or directory
     */
    public static void copy(String source, String dest) {
        File sfile = new File(source);
        File dfile = new File(dest);
        if (!sfile.exists())
            throw new RuntimeException("copy: " + source + " does not exist");
        if (sfile.isDirectory())
            throw new RuntimeException("copy: " + source + " is a directory");
        if (dfile.exists()  &&  dfile.isDirectory())
            dfile = new File(dfile, sfile.getName());
        if (!dfile.exists() || sfile.lastModified() > dfile.lastModified())
            try {
                printlnIfVerbose("copying " + source + " -> " + dest);
                mkdir(dfile.getParentFile());
                Files.copy(sfile.toPath(), dfile.toPath(), StandardCopyOption.REPLACE_EXISTING);
            } catch (Exception e) {
                throw new RuntimeException("error copying " + source + " to " +dest);
            }
    }

    /**
     * Copy file regardless of file date
     *
     * @param source file
     * @param dest  file or directory
     */
    public static void copyForce(String source, String dest) {
        File sfile = new File(source);
        File dfile = new File(dest);
        if (!sfile.exists())
            throw new RuntimeException("copy: " + source + " does not exist");
        if (sfile.isDirectory())
            throw new RuntimeException("copy: " + source + " is a directory");
        if (dfile.exists()  &&  dfile.isDirectory())
            dfile = new File(dfile, sfile.getName());
        try {
            printlnIfVerbose("copying " + source + " -> " + dest);
            mkdir(dfile.getParentFile());
            Files.copy(sfile.toPath(), dfile.toPath(), StandardCopyOption.REPLACE_EXISTING);
        } catch (Exception e) {
            throw new RuntimeException("error copying " + source + " to " +dest);
        }
    }

    /**
     * Copies all files that match a (Java!) regex from one directory to another excluding another (Java!) regex.
     * <br><br>
     * Normally only copies files that have a later date unless <code>force</code> is <code>true</code>
     *
     * @param srcDir
     * @param targetDir
     * @param includeRegex or null if all
     * @param excludeRegex or null of no exclusions
     * @param force if true ignore file dates
     */
    public static void copyRegex(String srcDir, String targetDir, String includeRegex, String excludeRegex, boolean force) {
        File sf = new File(srcDir);
        File df = new File(targetDir);
        if (!sf.exists())
            throw new RuntimeException("copyRegex: " + srcDir + " does not exist");
        if (!sf.isDirectory())
            throw new RuntimeException("copyRegex: " + srcDir + " is not a directory");
        mkdir(targetDir);
        File[] files = sf.listFiles();
        if (includeRegex == null)
            includeRegex = ".*";
        if (files != null) {
            Pattern pat = Pattern.compile(includeRegex);
            Pattern expat = null;
            if (excludeRegex != null)
                expat = Pattern.compile(excludeRegex);
            for (File file : files) {
                if (!file.isFile())
                    continue;
                String fname = file.getName();
                if (pat.matcher(fname).matches() && (excludeRegex == null || !expat.matcher(fname).matches())) {
                    File destFile = new File(df, file.getName());
                    if (force || !destFile.exists() || file.lastModified() > destFile.lastModified())
                        try {
                            printlnIfVerbose("copying " + file.getAbsolutePath() + " -> " + destFile.getAbsolutePath());
                            Files.copy(file.toPath(), destFile.toPath(), StandardCopyOption.REPLACE_EXISTING);
                        } catch (IOException e) {
                            throw new RuntimeException("error copying " + file.getAbsolutePath() + " to " + destFile.getAbsolutePath());
                        }
                }
            }
        }
    }

    /**
     * Delete all files in a directory that match a (Java!) regex.
     *
     * @param dirName
     * @param fnameRegex
     */
    public static void rmRegex(String dirName, String fnameRegex) {
        File dir = new File(dirName);
        if (dir.exists()  &&  dir.isDirectory()) {
            File [] files = dir.listFiles();
            if (files != null) {
                Pattern pat = Pattern.compile(fnameRegex);
                for (File file : files)
                    if (pat.matcher(file.getName()).matches()) {
                        printlnIfVerbose("removing " + file.getAbsolutePath());
                        if (!file.delete())
                            throw new RuntimeException("unable to delete " + file.getAbsolutePath());
                    }
            }
        }
    }

    /**
     * Copy one directory tree to another
     *
     * @param source
     * @param dest
     */
    public static void copyTree(String source, String dest) {
        copyTreeRegex(source, dest, null, null, false);
    }

    /**
     * Copy one directory tree to another.
     * Force copy regardless of file dates.
     *
     * @param source
     * @param dest
     */
    public static void copyTreeForce(String source, String dest) {
        copyTreeRegex(source, dest, null, null, true);
    }

    /**
     * Copy one directory tree to another
     * <br><br>
     * The regular expression applies to file names and not directory names.
     * If includeRegex is null, all files are included.
     *
     * @param source
     * @param dest
     * @param includeRegex regular expression for files to include or null
     */
    public static void copyTreeRegex(String source, String dest, String includeRegex) {
        copyTreeRegex(source, dest, includeRegex, null);
    }

    /**
     * Copy one directory tree to another
     *
     * The regular expression applies to file names and not directory names.
     * If includeRegex is null, all files are included.
     * If excludeRegex is null, no files are excluded.
     *
     * @param source
     * @param dest
     * @param includeRegex regular expression for files to include or null
     * @param excludeRegex regular expression for files to exclude or null
     */
    public static void copyTreeRegex(String source, String dest, String includeRegex, String excludeRegex) {
        copyTreeRegex(source, dest, includeRegex, excludeRegex, false);
    }

    /**
     * Copy one directory tree to another
     * <br><br>
     * The regular expression applies to file names and not directory names.
     * If includeRegex is null, all files are included.
     * If excludeRegex is null, no files are excluded.
     *
     * @param source
     * @param dest
     * @param includeRegex regular expression for files to include or null
     * @param excludeRegex regular expression for files to exclude or null
     * @param force force copy regardless of date
     */
    public static void copyTreeRegex(String source, String dest, String includeRegex, String excludeRegex, boolean force) {
        mkdir(dest);
        File sf = new File(source);
        File df = new File(dest);
        if (!sf.exists())
            throw new RuntimeException("copyTreeRegex: " + source + " does not exist");
        mkdir(dest);
        if (!df.exists())
            throw new RuntimeException("copyTreeRegex: can't create directory " + dest);
        if (!df.isDirectory())
            throw new RuntimeException("copyTreeRegex: can't create directory " + dest + "; file exists with that name");
        Pattern incPat, exPat;
        if (includeRegex != null)
            incPat = Pattern.compile(includeRegex);
        else
            incPat = null;
        if (excludeRegex != null)
            exPat = Pattern.compile(excludeRegex);
        else
            exPat = null;
        copyTree(new File(source), new File(dest), incPat, exPat, force);
    }

    private static void copyTree(File src, File dest, Pattern incPat, Pattern exPat, boolean force) {
        File[] files = src.listFiles();
        if (files != null)
            for (File f : files) {
                File d = new File(dest, f.getName());
                if (f.isFile()) {
                    if ((incPat == null || incPat.matcher(f.getName()).matches())
                            && (force || !d.exists()  ||  d.lastModified() < f.lastModified())) {
                        try {
                            if (exPat == null || !exPat.matcher(f.getName()).matches()) {
                                printlnIfVerbose("copying " + f.toString() + " -> " + d.toString());
                                (new File(d.getParent())).mkdirs();
                                Files.copy(f.toPath(), d.toPath(), StandardCopyOption.REPLACE_EXISTING);
                            }
                        } catch (IOException e) {
                            throw new RuntimeException("error copying " + f.toString() + " to " + d.toString());
                        }
                    }
                } else
                    copyTree(f, d, incPat, exPat, force);
            }
    }

    /**
     * Remove a file or empty directory
     *
     * @param file
     */
    public static void rm(String file) {
        if (file == null  ||  file.isEmpty())
            return;
        File f = new File(file);
        if (!f.exists())
            return;
        printlnIfVerbose("remove " + file);
        if (!f.delete())
            throw new RuntimeException("rm: error deleting " + file);
    }

    /**
     * Remove a file or empty directory
     *
     * @param file
     */
    public static void rmdir(String file) {
        if (file == null  ||  file.isEmpty())
            return;
        File f = new File(file);
        if (!f.exists())
            return;
        printlnIfVerbose("remove " + file);
        if (!f.delete())
            throw new RuntimeException("rmdir: error deleting " + file);
    }

    /**
     * Remove a file or entire directory tree
     *
     * @param name
     */
    public static void rmTree(String name) {
        if (name == null  ||  name.isEmpty())
            return;
        File f = new File(name);
        if (!f.exists())
            return;
        deleteFileOrDirectoryTree(f);
    }

    private static void deleteFileOrDirectoryTree(File fyle) {
        File[] files = fyle.listFiles();
        if (files != null)
            for (File file : files)
                deleteFileOrDirectoryTree(file);
        printlnIfVerbose("remove " + fyle.getAbsolutePath());
        if (!fyle.delete())
            throw new RuntimeException("error deleting " + fyle.getAbsolutePath());
    }

    /**
     * Writes a string to file if it doesn't already exist.
     *
     * @param fname
     * @param txt
     */
    public static void writeToFile(String fname, String txt) {
        File f = new File(fname);
        if (!f.exists()) {
            printlnIfVerbose("creating file " + fname);
            mkdir(f.getParentFile());
            try {
                Files.write(Paths.get(fname), txt.getBytes(), StandardOpenOption.WRITE, StandardOpenOption.CREATE);
            } catch (IOException e) {
                throw new RuntimeException("writeToFile:  error creating/writing " + fname);
            }
        }
    }

    /**
     * Create a Java manifest file
     *
     * @param manifest
     * @param mainClass
     */
    public static void createManifest(String manifest, String mainClass) {
        writeToFile(manifest, "Manifest-Version: 1.0\n" +
                "Main-Class: " + mainClass + "\n");
        String fname = mainClass.replaceAll("\\.", "/") + ".class";
        File mc = new File((new File(manifest)).getParentFile().getParentFile(), fname);
        touch(manifest, mc.lastModified());
    }

    /**
     * Set a file to executable
     *
     * @param file
     */
    public static void makeExecutable(String file) {
        File f = new File(file);
        if (!f.exists())
            throw new RuntimeException("can't make " + file + " executable; it doesn't exist");
        printlnIfVerbose("making " + file + " executable");
        if (!f.setExecutable(true))
            throw new RuntimeException("can't make " + file + " executable");
    }

    private static ArrayList<File> allSourceFiles(String dirStr, String ext) {
        return dirStr == null ? new ArrayList<>() : allSourceFiles(new ArrayList<>(), new File(dirStr), ext);
    }

    private static ArrayList<File> allSourceFiles(final ArrayList<File> lst, final File dir, final String ext) {
        if (dir == null || !dir.exists()  ||  !dir.isDirectory())
            return lst;
        final File [] fl = dir.listFiles();
        if (fl != null)
            for (File f : fl)
                if (f.isDirectory())
                    allSourceFiles(lst, f, ext);
                else
                if (f.getName().endsWith(ext))
                    lst.add(f);
        return lst;
    }

    private static long getLatestDate(final ArrayList<File> fl) {
        long dt = 0;
        for (File file : fl) {
            long m = file.lastModified();
            if (m > dt)
                dt = m;
        }
        return dt;
    }

    private static ArrayList<File> outOfDateSourceFiles(final ArrayList<File> sfl, final String srcDir, final String targetDir, final String srcExt, String targetExt) {
        final ArrayList<File> rlst = new ArrayList<>();
        final Path currentPath = Paths.get("").toAbsolutePath();
        final int slen = srcDir.length() + 1;
        for (File from : sfl)
            if (!from.getName().equals("package-info.java")) {
                String trel = currentPath.relativize(from.getAbsoluteFile().toPath()).toString();
                trel = trel.substring(0, trel.length() - srcExt.length());  // remove extension
                trel = trel.substring(slen);  // remove src starting point
                trel = targetDir + "/" + trel + targetExt;
                File to = new File(trel);
                if (!to.exists() || from.lastModified() > to.lastModified())
                    rlst.add(from);
            }
        return rlst;
    }

    /**
     * Create a command line input file (used in Windows)
     *
     * @param lst
     * @return
     */
    public static String writeArgsToFile(final ArrayList<File> lst) {
        File f;
        try {
            f = File.createTempFile("SourceFiles-", ".inp");
            printlnIfVerbose("writing source files names to " + f.getAbsolutePath());
            try (BufferedWriter bw = new BufferedWriter(new FileWriter(f))) {
                for (File f2 : lst) {
                    bw.write(f2.getPath());
                    bw.newLine();
                }
            }
        } catch (IOException e) {
            return null;
        }
        return f.getAbsolutePath();
    }

    public static String writeDocArgsToFile(final ArrayList<File> libs, final ArrayList<File> lst) {
        File f;
        try {
            f = File.createTempFile("DocFiles-", ".inp");
            boolean colon = false;
            try (BufferedWriter bw = new BufferedWriter(new FileWriter(f))) {
                if (!libs.isEmpty()) {
                    bw.write("-cp ");
                    for (File f2 : libs) {
                        if (colon)
                            bw.write(isWindows ? ';' : ':');
                        else
                            colon = true;
                        bw.write(f2.getPath());
                    }
                    bw.newLine();
                }
                for (File f2 : lst) {
                    bw.write(f2.getPath());
                    bw.newLine();
                }
            }
        } catch (IOException e) {
            return null;
        }
        return f.getAbsolutePath();
    }

    private static String writeDependencyArgsToFile(LocalDependencies ldep, ForeignDependencies fdep, String additionalSourceRoot) {
        File f;
        try {
            f = File.createTempFile("Dependencies-", ".inp");
            printlnIfVerbose("writing dependencies to " + f.getAbsolutePath());
            try (final BufferedWriter bw = new BufferedWriter(new FileWriter(f))) {
                bw.write("-cp ");
                if (ldep != null)
                    ldep.forEach(x -> {
                        try {
                            bw.write(x + (isWindows ? ";" : ":"));
                        } catch (IOException e) {
                            printError("error writing to " + f.getAbsolutePath());
                        }
                    });
                if (fdep != null)
                    for (ForeignDependency x : fdep.getDependencies()) {
                        try {
                            bw.write(x.targetPath + "/" + x.filename + (isWindows? ";" : ":"));
                        } catch (IOException e) {
                            printError("error writing to " + f.getAbsolutePath());
                        }
                    }
                if (additionalSourceRoot != null)
                    bw.write(additionalSourceRoot);
                bw.newLine();
            }
        } catch (IOException e) {
            return null;
        }
        return f.getAbsolutePath();
    }

    private static class StreamGobbler extends Thread {
        private final InputStream is;

        StreamGobbler(InputStream is) {
            this.is = is;
        }

        @Override
        public void run() {
            try {
                try (BufferedReader br = new BufferedReader(new InputStreamReader(is))) {
                    String line;
                    while ((line = br.readLine()) != null)
                        println(line);
                }
            } catch (IOException ioe) {
                ioe.printStackTrace();
            }
        }
    }

    public static void buildJava(String srcPath, String destDir, LocalDependencies localLibs, ForeignDependencies foreignLibs, String additionalSourceRoot) {
        File sf = new File(srcPath);
        if (!sf.exists())
            throw new RuntimeException("buildJava: directory \"" + srcPath + "\" does not exist");
        if (!sf.isDirectory())
            throw new RuntimeException("buildJava: \"" + srcPath + "\" is not a directory");
        mkdir(destDir);
        ArrayList<File> allFiles = allSourceFiles(srcPath, ".java");
        ArrayList<File> ood = outOfDateSourceFiles(allFiles, srcPath, destDir, ".java", ".class");
        if (ood != null  &&  !ood.isEmpty()) {
            String argsFile = writeArgsToFile(ood);
            javac(localLibs, foreignLibs, srcPath, destDir, argsFile, additionalSourceRoot);
            rm(argsFile);
        }
    }

    /**
     * Build JavaDocs
     *
     * @param srcPath
     * @param libPath
     * @param destDir
     */
    public static void buildJavadoc(String srcPath, String libPath, String destDir) {
        final boolean showOutput = true;  // good for debugging
        if (!new File(srcPath).exists())
            throw new RuntimeException("buildJavadoc: directory \"" + srcPath + "\" does not exist");
        mkdir(destDir);
        ArrayList<File> allFiles = allSourceFiles(srcPath, ".java");
        long latestSourceDate = getLatestDate(allFiles);
        File indexFile = new File(destDir + "/index.html");
        if (allFiles != null  &&  !allFiles.isEmpty()  &&  indexFile.lastModified() < latestSourceDate) {
            mkdir(destDir);
            String srcFiles = writeDocArgsToFile(allSourceFiles(libPath, ".jar"), allFiles);
            // for some reason, Windows hangs if we don't show the output
            runWait(showOutput || isWindows, "javadoc -d " + destDir + " @" + srcFiles);
            rm(srcFiles);
        }
    }

    public static int readChar() {
        try {
            return System.in.read();
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    /**
     * This is the main driving method for all external procedure calls.
     * <br><br>
     * If <code>useShell</code> is <code>true</code> than normal shell processing (such as
     * &gt;file and &lt;file) is performed.  Otherwise <code>exec</code> is called (which is faster).
     * <br><br>
     * If <code>wait</code> is <code>true</code> than this method waits for the process to complete.
     * Otherwise, it returns immediately with the process object.
     * <br><br>
     * If <code>echoCmd</code> is <code>true</code> then the command being executed will be echoed to stdout.
     * <br><br>
     * If <code>showOutput</code> is <code>true</code> the output of the process is displayed to stdout.
     * Otherwise, it is silent.
     * <br><br>
     * <code>startDir</code> provides the starting directory for the process.  If null, then the current
     * directory is used.
     * <br><br>
     * <code>cmd</code> represents the command (with arguments) to be executed.  If <code>useShell</code> is true,
     * then <code>cmd</code> can also use the shell syntax.
     *
     * @param useShell
     * @param wait
     * @param echoCmd
     * @param showOutput
     * @param startDir
     * @param cmd
     * @return the process either running or completed (depending on wait)
     */
    public static Process run(boolean useShell, boolean wait, boolean echoCmd, boolean showOutput, String startDir, String cmd) {
        Process proc;
        if (echoCmd)
            println(cmd);
        File sdir = null;
        if (startDir != null) {
            sdir = new File(startDir);
            if (!sdir.exists())
                mkdir(sdir);
            else if (sdir.isFile())
                throw new RuntimeException("run: " + startDir + " is a file");
        }
        try {
            String[] mscmd = new String[3];
            if (isWindows) {
                mscmd[0] = "cmd.exe";
                mscmd[1] = "/C";
                mscmd[2] = cmd;
                proc = Runtime.getRuntime().exec(mscmd, null, sdir);
            } else if (useShell) {
                mscmd[0] = "bash";
                mscmd[1] = "-c";
                mscmd[2] = cmd;
                proc = Runtime.getRuntime().exec(mscmd, null, sdir);
            } else
                proc = Runtime.getRuntime().exec(cmd, null, sdir);
            if (showOutput) {
                (new StreamGobbler(proc.getErrorStream())).start();
                (new StreamGobbler(proc.getInputStream())).start();
            }
            if (wait && proc.waitFor() != 0)
                throw new RuntimeException("error executing " + cmd);
        } catch (IOException | InterruptedException e) {
            throw new RuntimeException("error executing " + cmd);
        }
        return proc;
    }

    /**
     * Run a command in the underlying OS in the foreground.
     * Wait till it is done.
     * This command does not support shell processing such as &gt; and &lt; etc.
     *
     * @param showOutput true if output should be shown
     * @param startDir starting directory or null
     * @param cmd the command to execute
     * @see #run(boolean, boolean, boolean, boolean, String, String)
     */
    public static void runWait(boolean showOutput, String startDir, String cmd) {
        run(false, true, true, showOutput, startDir, cmd);
    }

    /**
     * Run a command in the underlying OS shell in the foreground.
     * Wait till it is done.
     * This command supports all of the shell processing such as &gt; and &lt; etc.
     *
     * @param startDir starting directory or null
     * @param cmd the command to execute
     * @see #run(boolean, boolean, boolean, boolean, String, String)
     */
    public static void runShell(String startDir, String cmd) {
        run(true, true, true, false, startDir, cmd);
    }

    /**
     * Run a command in the underlying OS in the foreground.
     *
     * @param showOutput
     * @param cmd
     * @see #run(boolean, boolean, boolean, boolean, String, String)
     */
    public static void runWait(boolean showOutput, String cmd) {
        runWait(showOutput, null, cmd);
    }    

    /**
     * Run a command in the underlying OS shell in the foreground.
     *
     * @param cmd
     * @see #run(boolean, boolean, boolean, boolean, String, String)
     */
    public static void runShell(String cmd) {
        runShell(null, cmd);
    }

    /**
     * Run a command in the underlying OS in the background.
     * Does not wait till it is done.
     * The Process is returned so that it can be killed.
     *
     * @param cmd the command to execute
     * @return the Process
     * @see #run(boolean, boolean, boolean, boolean, String, String)
     */
    public static Process runBackground(String cmd) {
        return run(false, false, true, false, null, cmd);
    }

    public static void killProcess(Process proc) {
        proc.destroyForcibly();
    }

    private static long getLatestFileDate(File f, long dt) {
        if (f.isFile()) {
            long d = f.lastModified();
            if (d > dt)
                dt = d;
            return dt;
        }
        File [] files = f.listFiles();
        if (files != null)
            for (File file : files)
                dt = getLatestFileDate(file, dt);
        return dt;
    }

    /**
     * Create a JAR file
     *
     * @param rootDir to be jar'ed up
     * @param jarFile name of the JAR file
     */
    public static void createJar(String rootDir, String jarFile) {
        File jf = new File(jarFile);
        if (jf.exists()) {
            long latestFileTime = getLatestFileDate(new File(rootDir), 0L);
            if (latestFileTime <= jf.lastModified())
                return;
        }
        File parentDir = jf.getParentFile();
        if (parentDir != null)
            mkdir(parentDir);
        String manifest = rootDir + "/META-INF/MANIFEST.MF";
        String cmd;
        if ((new File(manifest)).exists())
            cmd = "jar cmf " + manifest + " " + jarFile + " -C " + rootDir + " .";
        else
            cmd = "jar cf " + jarFile + " -C " + rootDir + " .";
        runWait(false, cmd);
    }

    /**
     * Unpack a JAR file.
     * <br><br>
     * A tag file is also created in order to track the date of the unjar vs. the date of the jar file.
     *
     * @param rootDir where to unjar the files
     * @param jarFile the jar file
     */
    public static void unJar(String rootDir, String jarFile) {
        final String tagDir = ".tags";
        File jarf = new File(jarFile);
        File tagf = new File(new File(rootDir, tagDir), jarf.getName() + ".tag");
        if (tagf.exists() && tagf.lastModified() >= jarf.lastModified())
            return;
        (new File(rootDir, tagDir)).mkdirs();
        String cmd = "jar xf " + jarf.getAbsolutePath();
        runWait(false, rootDir, cmd);
        touch(tagf.getAbsolutePath());
    }

    public static void touch(String fname, long millisecs) {
        File f = new File(fname);
        if (!f.exists()) {
            f.getParentFile().mkdirs();
            try {
                new FileOutputStream(f).close();
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }
        f.setLastModified(millisecs);
    }

    public static void touch(String fname) {
        touch(fname, System.currentTimeMillis());
    }

    public static void unJarAllLibs(final String rootDir, LocalDependencies ld, ForeignDependencies fd) {
        if (ld != null)
            ld.forEach(dep -> unJar(rootDir, dep));
        if (fd != null)
            for (ForeignDependency dep : fd.getDependencies())
                if (dep.filename.endsWith(".jar"))
                    unJar(rootDir, dep.targetPath + "/" + dep.filename);
    }

    public static void javac(LocalDependencies ldep, ForeignDependencies fdep, String sourcePath, String destPath, String filelist, String additionalSourceRoot) {
        if (!new File(sourcePath).exists())
            throw new RuntimeException("javac: \"" + sourcePath + "\" does not exist");
        mkdir(destPath);
        String cmd, argsFile = null;
        if (ldep != null  &&  !ldep.isEmpty()  ||  fdep != null  &&  !fdep.isEmpty()) {
            argsFile = writeDependencyArgsToFile(ldep, fdep, additionalSourceRoot);
            cmd = "javac -g @" + argsFile + " -sourcepath " + sourcePath + " -d " + destPath + " @" + filelist;
        } else
            cmd = "javac -g -sourcepath " + sourcePath + " -d " + destPath + " @" + filelist;
        runWait(true, cmd);
        rm(argsFile);
    }

    // Unfinished code to create WSDL's (I don't think they're needed anymore)
    public static void buildWS(LocalDependencies ldep, ForeignDependencies fdep, String dest, String sdir, String service) {
        String javaHome = java.lang.System.getProperty("java.home");  // to find tools.jar
        String deps = writeDependencyArgsToFile(ldep, fdep, null);
        String cmd = "java -classpath @" + deps + " com.sun.tools.ws.WsGen -d " + dest + " -Xendorsed -keep -wsdl -r " + sdir + " -s " + sdir + " " + service;
        mkdir(sdir);
        runWait(true, cmd);
        rm(deps);
    }

    /**
     * gunzip and untar a .gz file into the specified directory
     *
     * @param fname
     * @param dir
     * @param nPathsElementsToDelete number of leading path elements in tar file to eliminate
     */
    public static void gunzip(String fname, String dir, int nPathsElementsToDelete) {
        if (!new File(fname).exists())
            throw new RuntimeException("gunzip: file \"" + fname + "\" does not exist.");
        try {
            File tarfile = new File(fname.substring(0, fname.length() - 3));
            unGzip(fname, tarfile);
            //println((new File(dir).getAbsolutePath()));
            unTar(tarfile, new File(dir), nPathsElementsToDelete);
            tarfile.delete();
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage());
        }
    }

    /**
     * Untar an input file into an output directory.
     *
     * @param inputFile
     * @param outputDir
     * @param nPathsToEliminate number of leading path element in tar file to eliminate
     */
    private static void unTar(final File inputFile, final File outputDir, int nPathsToEliminate) {

        //println(String.format("Untaring %s to dir %s.", inputFile.getAbsolutePath(), outputDir.getAbsolutePath()));

        try {
            try (final InputStream is = new FileInputStream(inputFile)) {
                final TarArchiveInputStream debInputStream = (TarArchiveInputStream) new ArchiveStreamFactory().createArchiveInputStream("tar", is);
                TarArchiveEntry entry;
                while ((entry = (TarArchiveEntry) debInputStream.getNextEntry()) != null) {
                    String entryName = entry.getName();
                    entryName = removePathElements(nPathsToEliminate, entryName);
                    //println("The entry name is " + entryName);
                    final File outputFile = new File(outputDir, entryName);
                    if (entry.isDirectory()) {
                        //println(String.format("Attempting to write output directory %s.", outputFile.getAbsolutePath()));
                        if (!outputFile.exists()) {
                            //println(String.format("Attempting to create output directory %s.", outputFile.getAbsolutePath()));
                            if (!outputFile.mkdirs())
                                throw new IllegalStateException(String.format("Couldn't create directory %s.", outputFile.getAbsolutePath()));
                        }
                    } else {
                        //println(String.format("Creating output file %s.", outputFile.getAbsolutePath()));
                        try (final OutputStream outputFileStream = new FileOutputStream(outputFile)) {
                            IOUtils.copy(debInputStream, outputFileStream);
                            int mode = entry.getMode();
                            String oct = Integer.toOctalString(mode);
                            if (oct.length() >= 3) {
                                char oe = oct.charAt(oct.length() - 3);
                                if (oe == '7')
                                    outputFile.setExecutable(true, true);
                            }
                        }
                    }
                }
            }
        } catch (Exception e) {
            throw new RuntimeException(e.getMessage());
        }
    }

    private static String removePathElements(int nPathsToEliminate, String entryName) {
        while (nPathsToEliminate-- > 0)
            entryName = entryName.substring(entryName.indexOf('/'));
        return entryName;
    }

    /**
     * Ungzip an input file into an output file.
     * <br><br>
     * The output file is created in the output folder, having the same name
     * as the input file, minus the '.gz' extension.
     *
     */
    private static File unGzip(final String infname, final File outputFile) throws IOException {

        final File inputFile = new File(infname);

        //println(String.format("Ungzipping %s to %s.", inputFile.getAbsolutePath(), outputFile.getAbsolutePath()));

        try (GZIPInputStream in = new GZIPInputStream(new FileInputStream(inputFile));
             FileOutputStream out = new FileOutputStream(outputFile)) {
            IOUtils.copy(in, out);
        }

        return outputFile;
    }

    private static class ForeignDependency {
        String filename;
        String targetPath;
        String source;

        public ForeignDependency(String filename, String targetPath, String source) {
            this.filename = filename;
            this.targetPath = targetPath;
            this.source = source;
        }
    }

    static class ForeignDependencies {
        private final ArrayList<ForeignDependency> deps = new ArrayList<>();

        void add(String filename, String targetPath, String source) {
            deps.add(new ForeignDependency(filename, targetPath, source));
        }

        void add(String targetPath, String source) {
            final String filename = source.substring(source.lastIndexOf('/') + 1);
            deps.add(new ForeignDependency(filename, targetPath, source));
        }

        boolean isEmpty() {
            return deps.isEmpty();
        }

        ArrayList<ForeignDependency> getDependencies() {
            return deps;
        }

        void print() {
            for (ForeignDependency dep : deps)
                println(dep.filename + " -> " + dep.targetPath + " (" + dep.source + ")");
        }
    }

    public static class LocalDependencies extends ArrayList<String> {
    }

    public static String cwd() {
        return System.getProperty("user.dir");
    }

    private static String cacheDir() {
        if (CACHE_DIR == null) {
            String homeDir = System.getProperty("user.home");
            CACHE_DIR = homeDir + "/.bld.cache";
            mkdir(CACHE_DIR);
            CACHE_DIR += "/";
        }
        return CACHE_DIR;
    }

    public static void removeFromCache(String fname) {
        rm(cacheDir() + fname);
    }
    
    /**
     * Works like Unix "tail -F" command.  It never exits.
     * Useful for continuous display of a log file that may rotate.
     * 
     * @param fileName
     * @throws Exception 
     */
    public static void tail(String fileName) throws Exception {
        long lastKnownPosition = 0;
        final File file = new File(fileName);
        final byte [] buf = new byte[4096];
        final int sleepLength = 250;
        boolean dontNeedSleep;

        while (true) {
            long fileLength = file.length();
            if (fileLength < lastKnownPosition)
                lastKnownPosition = 0;
            if (dontNeedSleep = fileLength > lastKnownPosition) {
                RandomAccessFile fp = new RandomAccessFile(file, "r");
                fp.seek(lastKnownPosition);
                long nbytes = fileLength - lastKnownPosition;
                if (dontNeedSleep = nbytes > buf.length)
                    nbytes = buf.length;
                if (fp.read(buf, 0, (int) nbytes) != nbytes)
                    throw new Exception("read error");
                System.out.write(buf, 0, (int)nbytes);
                lastKnownPosition += nbytes;
                fp.close();
            } 
            if (!dontNeedSleep)
                Thread.sleep(sleepLength);
        }
    }

    /**
     * Convert a string from dashed notation to camel case notation.
     *
     * @param input
     *            The string in dashed notation.
     * @return The string in camel case notation.
     */
    public static String convertToCamelCase(String input) {
        if (input == null || input.isEmpty())
            return input;

        StringBuilder result = new StringBuilder();
        boolean capitalizeNext = false;

        for (char c : input.toCharArray()) {
            if (c == '-') {
                capitalizeNext = true;
            } else {
                if (capitalizeNext) {
                    result.append(Character.toUpperCase(c));
                    capitalizeNext = false;
                } else {
                    result.append(c);
                }
            }
        }

        return result.toString();
    }
}
