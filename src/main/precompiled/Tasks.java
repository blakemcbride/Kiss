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
 *
 *    Non-private instance methods with no parameters are considered tasks.
 */

import org.kissweb.BuildUtils;

import static org.kissweb.BuildUtils.*;

/**
 * This class contains the tasks that are executed by the build system.
 * <br><br>
 * The build system finds the names of the tasks through reflection.
 * It also does camelCase conversion.  So a task named abcDef may be evoked
 * as abc-def.
 * <br><br>
 * Each task must be declared as a public static method with no parameters.
 */
public class Tasks {

    // Things that change semi-often
    final static String groovyVer = "4.0.26";
    final static String postgresqlVer = "42.7.7";
    final static String tomcatVer = "11.0.8";
    final static String LIBS = "libs";  // compile time location
    final static ForeignDependencies foreignLibs = buildForeignDependencies();
    final static LocalDependencies localLibs = buildLocalDependencies();
    final static String tomcatTarFile = "apache-tomcat-" + tomcatVer + ".tar.gz";
    final static String BUILDDIR = "work";
    final static String explodedDir = BUILDDIR + "/" + "exploded";
    final static String postgresqlJar = "postgresql-" + postgresqlVer + ".jar";
    final static String groovyJar = "groovy-" + groovyVer + ".jar";
    final static String debugPort = "9000";

    /**
     * Main entry point for the build system.  It tells the build system what arguments were passed in
     * and what class contains all the tasks.
     *
     * @param args the arguments to the program
     * @throws Exception if exception is thrown
     * @throws InstantiationException if the class cannot be instantiated
     */
    public static void main(String[] args) throws Exception {
        BuildUtils.build(args, Tasks.class, LIBS);
    }

    /**
     * Display a list of valid tasks.  It is called by the build system
     * when the user selects the 'list-tasks' task.
     * <br><br>
     * The build system expects this method to be named listTasks.
     *
     * @see BuildUtils#build
     */
    public static void listTasks() {
        println("");
        println("develop                  build and run the entire system in the foreground");
        println("run-backend              build and run backend in the background");
        println("run-frontend             build and run frontend in the background");
        println("stop-backend             stop the background backend");
        println("stop-frontend            stop the background frontend");
        println("build                    build the entire system but don't run it");
        println("war                      create deployable war file");

        println("");
        println("clean                    remove all compiled files");
        println("realclean                + remove downloaded jar files and tomcat");
        println("ideclean                 + IDE files");
        println("");

        println("jar                      build Kiss.jar");
        println("javadoc                  build javadoc files");
        println("kisscmd                  build a command-line executable jar file");
        println("KissGP                   + include Groovy, and the PostgreSQL driver");
        println("");

        println("libs                     download foreign jar files");
        println("setup-tomcat             set up tomcat");
        println("unit-tests               build the system for unit testing (KissUnitTest.jar)");
        println("");
    }

    /**
     * Build the whole system
     * <br><br>
     * 1. download needed jar files<br>
     * 2. build the system into a deployable war file<br>
     * 3. set up a local tomcat server<br>
     * 4. deploy the war file to the local tomcat<br>
     * 5. build JavaDocs
     */
    public static void build() {
        war();
        setupTomcat();
        deployWar();
        javadoc();
    }

    /**
     * This creates a command-line executable jar file that runs the org.kissweb.Main class.
     * You can put your custom code in the org.kissweb.Main class and build the runnable jar.
     * For a possible better solution, see the KissGP target.
     */
    public static void kisscmd() {
        final String targetPath = BUILDDIR + "/cmdline";
        final String manifest = targetPath + "/META-INF/MANIFEST.MF";
        final String jarFile = BUILDDIR + "/kisscmd.jar";
        libs();

        unJarAllLibs(targetPath, localLibs, foreignLibs);
        buildJava("src/main/core", targetPath, localLibs, foreignLibs, null);
        rmTree(targetPath + "/META-INF");
        createManifest(manifest, "org.kissweb.Main");
        createJar(targetPath, jarFile);
    }

    /**
     * Download needed foreign libraries
     */
    public static void libs() {
        downloadAll(foreignLibs);
    }

    /**
     * Create Kiss.jar.  This is a JAR file that can be used in other apps as a
     * utility library.
     */
    private static void jar(boolean unitTest) {
        libs();
        buildJava("src/main/core", explodedDir + "/WEB-INF/classes", localLibs, foreignLibs, null);
        if (unitTest)
            buildJava("src/test/core", explodedDir + "/WEB-INF/classes", localLibs, foreignLibs, explodedDir + "/WEB-INF/classes");
        rm(explodedDir + "/WEB-INF/lib/jakarta.servlet-api-4.0.1.jar");
        createJar(explodedDir + "/WEB-INF/classes", BUILDDIR + "/Kiss.jar");
        //println("Kiss.jar has been created in the " + BUILDDIR + " directory");
    }

    /**
     * Build Kiss.jar<br><br>
     * This is a JAR file that can be used in other apps as a utility library.
     */
    public static void jar() {
        jar(false);
    }

    /**
     * Create an executable JAR that includes Kiss, Groovy, and the PostgreSQL driver.
     * It runs an arbitrary groovy file in the context of Kiss and PostgreSQL.
     * <br><br>
     * All that is needed is KissGP.jar
     * <br><br>
     * Usage:  java -jar KissGP.jar [groovy-file] [args]...
     * <br><br>
     * Other databases can be used also.  See the manual.
     */
    public static void KissGP() {
        final String name = "KissGP";
        final String workDir = BUILDDIR + "/" + name;
        final String jarName = workDir + ".jar";
        jar(false);
        rmTree(workDir);
        rm(jarName);
        unJar(workDir, BUILDDIR + "/Kiss.jar");
        unJar(workDir, "libs/" + postgresqlJar);
        rm(workDir + "/META-INF/MANIFEST.MF");
        unJar(workDir, "libs/" + groovyJar);
        createJar(workDir, jarName);
        rmTree(workDir);
    }

    /**
     * Build the system for unit testing. (KissUnitTest.jar)
     */
    public static void unitTests() {
        final String name = "KissUnitTest";
        final String workDir = BUILDDIR + "/" + name;
        final String jarName = workDir + ".jar";
        jar(true);
        rmTree(workDir);
        rm(jarName);
        unJar(workDir, BUILDDIR + "/Kiss.jar");
        unJar(workDir, "libs/" + postgresqlJar);

        // jUnit stuff
        unJar(workDir, "libs/junit-jupiter-engine-5.11.0.jar");
        unJar(workDir, "libs/junit-jupiter-api-5.11.0.jar");
        unJar(workDir, "libs/junit-jupiter-params-5.11.0.jar");
        unJar(workDir, "libs/junit-platform-console-1.11.0.jar");
        unJar(workDir, "libs/junit-platform-console-standalone-1.11.0.jar");

        unJar(workDir, "libs/" + groovyJar);
        rm(workDir + "/META-INF/MANIFEST.MF");
        writeToFile(workDir + "/META-INF/MANIFEST.MF", "Manifest-Version: 1.0\nMain-Class: org.junit.platform.console.ConsoleLauncher\nClass-Path: KissUnitTest.jar\n");
        createJar(workDir, jarName);
        rmTree(workDir);
    }

    /**
     * Build the system into explodedDir
     */
    public static void buildSystem() {
        libs();
        copyTree("src/main/frontend", explodedDir);
        writeToFile(explodedDir + "/META-INF/MANIFEST.MF", "Manifest-Version: 1.0\n");
        copyTree("src/main/backend", explodedDir + "/WEB-INF/backend");
        copyTree(LIBS, explodedDir + "/WEB-INF/lib");
        buildJava("src/main/core", explodedDir + "/WEB-INF/classes", localLibs, foreignLibs, null);
        buildJava("src/test/core", explodedDir + "/WEB-INF/test-classes", localLibs, foreignLibs, explodedDir + "/WEB-INF/classes");
        buildJava("src/main/precompiled", explodedDir + "/WEB-INF/classes", localLibs, foreignLibs, explodedDir + "/WEB-INF/classes");
        rm(explodedDir + "/WEB-INF/lib/jakarta.servlet-api-4.0.1.jar");
        copyRegex("src/main/core/org/kissweb/lisp", explodedDir + "/WEB-INF/classes/org/kissweb/lisp", ".*\\.lisp", null, false);
        copy("src/main/core/log4j2.xml", explodedDir + "/WEB-INF/classes");
        copyForce("src/main/core/WEB-INF/web-unsafe.xml", explodedDir + "/WEB-INF/web.xml");
    }

    /**
     * Build the system and create the deployable WAR file.
     */
    public static void war() {
        buildSystem();
        copyForce("src/main/core/WEB-INF/web-secure.xml", explodedDir + "/WEB-INF/web.xml");
        createJar(explodedDir, BUILDDIR + "/Kiss.war");
        copyForce("src/main/core/WEB-INF/web-unsafe.xml", explodedDir + "/WEB-INF/web.xml");
        //println("Kiss.war has been created in the " + BUILDDIR + " directory");
    }

    private static void deployWar() {
        copy(BUILDDIR + "/Kiss.war", "tomcat/webapps/ROOT.war");
    }

    /**
     * Unpack and install tomcat
     */
    public static void setupTomcat() {
        if (!exists("tomcat/bin/startup.sh")) {
            download(tomcatTarFile, ".", "https://dlcdn.apache.org/tomcat/tomcat-11/v" + tomcatVer + "/bin/apache-tomcat-" + tomcatVer + ".tar.gz");
            gunzip(tomcatTarFile, "tomcat", 1);
            rmTree("tomcat/webapps/ROOT");
            //run("tar xf apache-tomcat-9.0.31.tar.gz --one-top-level=tomcat --strip-components=1");
        }
        if (isWindows) {
            System.err.println("Setting up tomcat.  Please wait...");
            rm("tomcat\\conf\\tomcat-users.xml");
            // The following is needed by NetBeans
            writeToFile("tomcat\\conf\\tomcat-users.xml", "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
                    "<tomcat-users xmlns=\"http://tomcat.apache.org/xml\"\n" +
                    "              xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n" +
                    "              xsi:schemaLocation=\"http://tomcat.apache.org/xml tomcat-users.xsd\"\n" +
                    "              version=\"1.0\">\n" +
                    "  <user username=\"admin\" password=\"admin\" roles=\"tomcat,manager-script\" />\n" +
                    "</tomcat-users>\n");
            writeToFile("tomcat\\bin\\debug.cmd", "@echo off\n" +
                    "cd " + getcwd() + "\\tomcat\\bin\n" +
                    "set JAVA_HOME=" + getJavaPathOnWindows() + "\n" +
                    "set CATALINA_HOME=" + getTomcatPath() + "\n" +
                    "set JPDA_ADDRESS=" + debugPort + "\n" +
                    "set JPDA_TRANSPORT=dt_socket\n" +
                    "catalina.bat jpda start\n");
            writeToFile("tomcat\\bin\\stopdebug.cmd", "@echo off\n" +
                    "cd " + getcwd() + "\\tomcat\\bin\n" +
                    "set JAVA_HOME=" + getJavaPathOnWindows() + "\n" +
                    "set CATALINA_HOME=" + getTomcatPath() + "\n" +
                    "shutdown.bat\n");
        } else {
            rm("tomcat/conf/tomcat-users.xml");
            // The following is needed by NetBeans
            writeToFile("tomcat/conf/tomcat-users.xml", "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" +
                    "<tomcat-users xmlns=\"http://tomcat.apache.org/xml\"\n" +
                    "              xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"\n" +
                    "              xsi:schemaLocation=\"http://tomcat.apache.org/xml tomcat-users.xsd\"\n" +
                    "              version=\"1.0\">\n" +
                    "  <user username=\"admin\" password=\"admin\" roles=\"tomcat,manager-script\" />\n" +
                    "</tomcat-users>\n");
            writeToFile("tomcat/bin/debug", "#\n" +
                    "cd " + getcwd() + "/tomcat/bin\n" +
                    "export JPDA_ADDRESS=" + debugPort + "\n" +
                    "export JPDA_TRANSPORT=dt_socket\n" +
                    "./catalina.sh jpda start\n");
            makeExecutable("tomcat/bin/debug");
        }
        /* The SQLite jar file doesn't correctly support this.
        if (isSunOS) {
            writeToFile("tomcat/bin/setenv.sh","export JAVA_OPTS=\"-Dorg.sqlite.lib.path=/usr/lib/amd64 -Dorg.sqlite.lib.name=libsqlite3.so\"\n");
        }
         */
    }

    /**
     * Build and run both the front-end and back-end synchronously
     * <br><br>
     * 1. download needed jar files<br>
     * 2. build the system into a deployable war file<br>
     * 3. set up a local tomcat server<br>
     * 4. deploy the war file to the local tomcat<br>
     * 5. run the local tomcat backend<br>
     * 6. run the local tomcat frontend<br>
     */
    public static void develop() {
        Process proc;
        buildSystem();
        setupTomcat();
        copyTree(BUILDDIR + "/exploded", "tomcat/webapps/ROOT");
        if (isWindows)
            runWait(true, "tomcat\\bin\\debug.cmd");
        else
            runWait(true, "tomcat/bin/debug");
        proc = runBackground("java -jar SimpleWebServer.jar -d src/main/frontend");
        println("***** SERVER IS RUNNING *****");
        println("Server log can be viewed at " + cwd() + "/tomcat/logs/catalina.out or via the view-log command");
        println("You can browse to http://localhost:8000   (do not use port 8080)");
        println("The app can also be debugged at port " + debugPort);
        println("hit any key to stop tomcat");
        readChar();
        println("shutting down tomcat");
        if (isWindows)
            runWait(true, "tomcat\\bin\\stopdebug.cmd");
        else
            runWait(true, "tomcat/bin/shutdown.sh");
        killProcess(proc);
    }

    /**
     * Build and run the back-end asynchronously
     * <br><br>
     * 1. download needed jar files<br>
     * 2. build the system into a deployable war file<br>
     * 3. set up a local tomcat server<br>
     * 4. deploy the war file to the local tomcat<br>
     * 5. run the local tomcat backend<br>
     */
    public static void startBackend() {
        buildSystem();
        setupTomcat();
        copyTree(BUILDDIR + "/exploded", "tomcat/webapps/ROOT");
        if (isWindows)
            runWait(true, "tomcat\\bin\\debug.cmd");
        else
            runWait(true, "tomcat/bin/debug");
        println("***** SERVER IS RUNNING *****");
        println("Server log can be viewed at " + cwd() + "/tomcat/logs/catalina.out or via the view-log command");
        println("You can browse to http://localhost:8000   (do not use port 8080)");
        println("The app can also be debugged at port " + debugPort);
        println("To stop the backend, type 'bld stop-backend'");
    }

    /**
     * Stop the backend development server
     */
    public static void stopBackend() {
        println("shutting down tomcat");
        if (isWindows)
            runWait(true, "tomcat\\bin\\stopdebug.cmd");
        else
            runWait(true, "tomcat/bin/shutdown.sh");
    }

    /**
     * Start the frontend development server.
     */
    public static void startFrontend() {
        runBackground("java -jar SimpleWebServer.jar -d src/main/frontend");
        println("To stop the frontend, type 'bld stop-frontend'");
    }

    /**
     * Stop the front-end development server.
     */
    public static void stopFrontend() {
        stopFrontendServer();
    }

    /**
     * build the javdoc files
     */
    public static void javadoc() {
        libs();
        buildJavadoc("src/main/core", LIBS, BUILDDIR + "/javadoc", "JavaDocOverview.html");
    }

    /**
     * Remove:<br>
     * -- all files that were built<br><br>
     * Do not remove:<br>
     * -- the downloaded jar files, tomcat<br>
     * -- the IDE files
     */
    public static void clean() {
        rmTree(BUILDDIR);
        rmTree("build.work");  // used in the past
        rm("manual/Kiss.log");
        rm("manual/Kiss.aux");
        rm("manual/Kiss.toc");
    }

    /**
     * Remove:<br>
     * -- all files that were built<br>
     * -- the downloaded jar files, tomcat<br><br>
     * Do not remove:<br>
     * -- the IDE files
     */
    public static void realclean() {
        clean();
        rmRegex("src/main/frontend/lib", "jquery.*");
        delete(foreignLibs);
        rmTree("tomcat");
        rm(tomcatTarFile);
        rm("manual/Kiss.pdf");

        // remove old stuff
        rm("libs/json.jar");
        rmRegex(LIBS, "dynamic-loader-.*\\.jar");
        rmRegex(LIBS, "groovy-.*\\.jar");
        rmRegex(LIBS, "postgresql-.*\\.jar");
        rmRegex(LIBS, "sqlite-jdbc-.*\\.jar");

        /* libraries that don't have their version number in the file name
           must be removed from cache.
           Now we must include them with Kiss because they are no longer available through a CDN.
         */
        //removeFromCache("ag-grid-community.noStyle.min.js");
        //removeFromCache("ag-grid.min.css");
        //removeFromCache("ag-theme-balham.min.css");
    }

    /**
     * Remove:<br>
     * -- all files that were built<br>
     * -- the downloaded jar files, tomcat<br>
     * -- the IDE files
     */
    public static void ideclean() {
        realclean();

        rmTree(".project");
        rmTree(".settings");
        rmTree(".vscode");

        // IntelliJ
        rmTree(".idea");
        rmTree("out");
        rmRegex(".", ".*\\.iml");
        rmRegex("src", ".*\\.iml");

        // NetBeans
        rmTree("dist");
        rmTree("nbproject");
        rmTree("build");
        rm("nbbuild.xml");
    }

    /**
     * Specify the jars used by the system but not included in the distribution.
     * These are the jars that are to be downloaded by the build system.
     *
     * @return
     */
    private static ForeignDependencies buildForeignDependencies() {
        final ForeignDependencies dep = new ForeignDependencies();
        dep.add(LIBS, "https://repo1.maven.org/maven2/com/mchange/c3p0/0.11.1/c3p0-0.11.1.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/apache/groovy/groovy/" + groovyVer + "/" + groovyJar);
        dep.add(LIBS, "https://repo1.maven.org/maven2/jakarta/servlet/jakarta.servlet-api/6.1.0/jakarta.servlet-api-6.1.0.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/apache/logging/log4j/log4j-core/2.22.0/log4j-core-2.22.0.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/apache/logging/log4j/log4j-api/2.22.0/log4j-api-2.22.0.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/apache/logging/log4j/log4j-1.2-api/2.22.0/log4j-1.2-api-2.22.0.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/com/mchange/mchange-commons-java/0.3.2/mchange-commons-java-0.3.2.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/com/microsoft/sqlserver/mssql-jdbc/12.4.2.jre8/mssql-jdbc-12.4.2.jre8.jar");
        // Oracle has removed these files from their public repository
        //dep.add(LIBS, "https://repo1.maven.org/maven2/mysql/mysql-connector-java/9.2.0/mysql-connector-java-9.2.0.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/com/oracle/ojdbc/ojdbc10/19.3.0.0/ojdbc10-19.3.0.0.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/postgresql/postgresql/" + postgresqlVer + "/" + postgresqlJar);
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/slf4j/slf4j-api/1.7.30/slf4j-api-1.7.30.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/slf4j/slf4j-simple/1.7.30/slf4j-simple-1.7.30.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/xerial/sqlite-jdbc/3.47.1.0/sqlite-jdbc-3.47.1.0.jar");
        dep.add("src/main/frontend/lib", "https://code.jquery.com/jquery-3.6.3.min.js");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/apache/pdfbox/pdfbox/2.0.31/pdfbox-2.0.31.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/apache/pdfbox/fontbox/2.0.31/fontbox-2.0.31.jar");
        // ag-grid appears to no longer be available through a CDN.  Therefore, I am simply including it with the Kiss distribution
        //dep.add("src/main/frontend/lib", "https://cdnjs.cloudflare.com/ajax/libs/ag-grid/25.1.0/ag-grid-community.noStyle.min.js");
        //dep.add("src/main/frontend/lib", "https://cdnjs.cloudflare.com/ajax/libs/ag-grid/25.1.0/styles/ag-grid.min.css");
        //dep.add("src/main/frontend/lib", "https://cdnjs.cloudflare.com/ajax/libs/ag-grid/25.1.0/styles/ag-theme-balham.min.css");

        // jUnit
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/junit/jupiter/junit-jupiter/5.11.0/junit-jupiter-5.11.0.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/junit/jupiter/junit-jupiter-params/5.11.0/junit-jupiter-params-5.11.0.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/junit/jupiter/junit-jupiter-api/5.11.0/junit-jupiter-api-5.11.0.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/junit/jupiter/junit-jupiter-engine/5.11.0/junit-jupiter-engine-5.11.0.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/apiguardian/apiguardian-api/1.1.2/apiguardian-api-1.1.2.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/junit/platform/junit-platform-console/1.11.0/junit-platform-console-1.11.0.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/junit/platform/junit-platform-console-standalone/1.11.0/junit-platform-console-standalone-1.11.0.jar");
        return dep;
    }

    /**
     * This specifies the jar files used by the system that are included in the distribution.
     * (All are open-source but exist in other projects.)
     *
     * @return
     */
    private static LocalDependencies buildLocalDependencies() {
        final LocalDependencies dep = new LocalDependencies();
        dep.add(LIBS, "abcl.jar");
        return dep;
    }

}
