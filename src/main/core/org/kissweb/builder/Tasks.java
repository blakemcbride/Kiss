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


package org.kissweb.builder;

import static org.kissweb.builder.BuildUtils.*;

public class Tasks {

    // Things that change semi-often
    final String groovyVer = "4.0.24";
    final String postgresqlVer = "42.7.4";
    final String tomcatVer = "9.0.85";


    final String LIBS = "libs";
    final ForeignDependencies foreignLibs = buildForeignDependencies();
    final LocalDependencies localLibs = buildLocalDependencies();
    final String tomcatTarFile = "apache-tomcat-" + tomcatVer + ".tar.gz";
    final String BUILDDIR = "work";
    final String explodedDir = BUILDDIR + "/" + "exploded";
    final String postgresqlJar = "postgresql-" + postgresqlVer + ".jar";
    final String groovyJar = "groovy-" + groovyVer + ".jar";
    final String debugPort = "9000";

    /**
     * Build the whole system
     * <br><br>
     * 1. download needed jar files<br>
     * 2. build the system into a deployable war file<br>
     * 3. set up a local tomcat server<br>
     * 4. deploy the war file to the local tomcat<br>
     * 5. build JavaDocs
     */
    void all() {
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
    void kisscmd() {
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
    void libs() {
        downloadAll(foreignLibs);
    }

    /**
     * Create Kiss.jar.  This is a JAR file that can be used in other apps as a
     * utility library.
     */
    private void jar(boolean unitTest) {
        libs();
        buildJava("src/main/core", explodedDir + "/WEB-INF/classes", localLibs, foreignLibs, null);
        if (unitTest)
            buildJava("src/test/core", explodedDir + "/WEB-INF/classes", localLibs, foreignLibs, explodedDir + "/WEB-INF/classes");
        rm(explodedDir + "/WEB-INF/lib/javax.servlet-api-4.0.1.jar");
        createJar(explodedDir + "/WEB-INF/classes", BUILDDIR + "/Kiss.jar");
        //println("Kiss.jar has been created in the " + BUILDDIR + " directory");
    }

    /**
     * Build Kiss.jar<br><br>
     * This is a JAR file that can be used in other apps as a utility library.
     */
    void jar() {
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
    void KissGP() {
        final String name = "KissGP";
        final String workDir = BUILDDIR + "/" + name;
        final String jarName = workDir + ".jar";
        jar(false);
        rmTree(workDir);
        rm(jarName);
        unJar(workDir, BUILDDIR + "/Kiss.jar");
        unJar(workDir, "libs/" + postgresqlJar);
        unJar(workDir, "libs/json.jar");
        rm(workDir + "/META-INF/MANIFEST.MF");
        unJar(workDir, "libs/" + groovyJar);
        createJar(workDir, jarName);
        rmTree(workDir);
    }

    /**
     * Build the system for unit testing. (KissUnitTest.jar)
     */
    void UnitTest() {
        final String name = "KissUnitTest";
        final String workDir = BUILDDIR + "/" + name;
        final String jarName = workDir + ".jar";
        jar(true);
        rmTree(workDir);
        rm(jarName);
        unJar(workDir, BUILDDIR + "/Kiss.jar");
        unJar(workDir, "libs/" + postgresqlJar);
        unJar(workDir, "libs/json.jar");

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
    void build() {
        libs();
        copyTree("src/main/frontend", explodedDir);
        writeToFile(explodedDir + "/META-INF/MANIFEST.MF", "Manifest-Version: 1.0\n");
        copyTree("src/main/backend", explodedDir + "/WEB-INF/backend");
        copyTree("libs", explodedDir + "/WEB-INF/lib");
        buildJava("src/main/core", explodedDir + "/WEB-INF/classes", localLibs, foreignLibs, null);
        buildJava("src/test/core", explodedDir + "/WEB-INF/test-classes", localLibs, foreignLibs, explodedDir + "/WEB-INF/classes");
        buildJava("src/main/backend/precompiled", explodedDir + "/WEB-INF/classes", localLibs, foreignLibs, explodedDir + "/WEB-INF/classes");
        rm(explodedDir + "/WEB-INF/lib/javax.servlet-api-4.0.1.jar");
        copyRegex("src/main/core/org/kissweb/lisp", explodedDir + "/WEB-INF/classes/org/kissweb/lisp", ".*\\.lisp", null, false);
        copy("src/main/core/log4j2.xml", explodedDir + "/WEB-INF/classes");
        copyForce("src/main/core/WEB-INF/web-unsafe.xml", explodedDir + "/WEB-INF/web.xml");
    }

    /**
     * Build the system and create the deployable WAR file.
     */
    void war() {
        build();
        copyForce("src/main/core/WEB-INF/web-secure.xml", explodedDir + "/WEB-INF/web.xml");
        createJar(explodedDir, BUILDDIR + "/Kiss.war");
        copyForce("src/main/core/WEB-INF/web-unsafe.xml", explodedDir + "/WEB-INF/web.xml");
        //println("Kiss.war has been created in the " + BUILDDIR + " directory");
    }

    private void deployWar() {
        copy(BUILDDIR + "/Kiss.war", "tomcat/webapps/ROOT.war");
    }

    /**
     * Unpack and install tomcat
     */
    void setupTomcat() {
        if (!exists("tomcat/bin/startup.sh")) {
            download(tomcatTarFile, ".", "https://archive.apache.org/dist/tomcat/tomcat-9/v" + tomcatVer + "/bin/apache-tomcat-" + tomcatVer + ".tar.gz");
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
     * Build and run both the front-end and back-end
     * <br><br>
     * 1. download needed jar files<br>
     * 2. build the system into a deployable war file<br>
     * 3. set up a local tomcat server<br>
     * 4. deploy the war file to the local tomcat<br>
     * 5. build JavaDocs<br>
     * 6. run the local tomcat<br>
     */
    void develop() {
        Process proc;
        build();
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
     * Build and run the back-end only
     * <br><br>
     * 1. download needed jar files<br>
     * 2. build the system into a deployable war file<br>
     * 3. set up a local tomcat server<br>
     * 4. deploy the war file to the local tomcat<br>
     * 5. build JavaDocs<br>
     * 6. run the local tomcat<br>
     */
    void developBackend() {
        Process proc;
        build();
        setupTomcat();
        copyTree(BUILDDIR + "/exploded", "tomcat/webapps/ROOT");
        if (isWindows)
            runWait(true, "tomcat\\bin\\debug.cmd");
        else
            runWait(true, "tomcat/bin/debug");
        println("***** BACKEND IS RUNNING *****");
        println("Server log can be viewed at " + cwd() + "/tomcat/logs/catalina.out or via the view-log command");
        println("You'll need to start the front-end server independently.");
        println("The app can also be debugged at port " + debugPort);
        println("hit any key to stop the backend server");
        readChar();
        println("shutting down tomcat");
        if (isWindows)
            runWait(true, "tomcat\\bin\\stopdebug.cmd");
        else
            runWait(true, "tomcat/bin/shutdown.sh");
    }

    /**
     * build the javdoc files
     */
    void javadoc() {
        libs();
        buildJavadoc("src/main/core", "libs", BUILDDIR + "/javadoc");
    }

    /**
     * Remove:<br>
     * -- all files that were built<br><br>
     * Do not remove:<br>
     * -- the downloaded jar files, tomcat<br>
     * -- the IDE files
     */
    void clean() {
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
    void realclean() {
        clean();
        rmRegex("src/main/frontend/lib", "jquery.*");
        delete(foreignLibs);
        rmTree("tomcat");
        rm(tomcatTarFile);
        rm("manual/Kiss.pdf");

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
    void ideclean() {
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
    private ForeignDependencies buildForeignDependencies() {
        final ForeignDependencies dep = new ForeignDependencies();
        dep.add(LIBS, "https://repo1.maven.org/maven2/com/mchange/c3p0/0.9.5.5/c3p0-0.9.5.5.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/apache/groovy/groovy/" + groovyVer + "/" + groovyJar);
        dep.add(LIBS, "https://repo1.maven.org/maven2/javax/servlet/javax.servlet-api/4.0.1/javax.servlet-api-4.0.1.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/apache/logging/log4j/log4j-core/2.22.0/log4j-core-2.22.0.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/apache/logging/log4j/log4j-api/2.22.0/log4j-api-2.22.0.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/apache/logging/log4j/log4j-1.2-api/2.22.0/log4j-1.2-api-2.22.0.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/com/mchange/mchange-commons-java/0.2.20/mchange-commons-java-0.2.20.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/com/microsoft/sqlserver/mssql-jdbc/12.4.2.jre8/mssql-jdbc-12.4.2.jre8.jar");
        // Oracle has removed these files from their public repository
        //dep.add(LIBS, "https://repo1.maven.org/maven2/mysql/mysql-connector-java/8.0.33/mysql-connector-java-8.0.30.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/com/oracle/ojdbc/ojdbc10/19.3.0.0/ojdbc10-19.3.0.0.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/postgresql/postgresql/" + postgresqlVer + "/" + postgresqlJar);
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/slf4j/slf4j-api/1.7.30/slf4j-api-1.7.30.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/slf4j/slf4j-simple/1.7.30/slf4j-simple-1.7.30.jar");
        dep.add(LIBS, "https://repo1.maven.org/maven2/org/xerial/sqlite-jdbc/3.47.1.0/sqlite-jdbc-3.47.1.0.jar");
        //  https://github.com/dvare/dynamic-loader
        dep.add("dynamic-loader-3.2.jar", LIBS, "https://oss.sonatype.org/service/local/artifact/maven/redirect?r=releases&g=org.dvare&a=dynamic-loader&v=3.2&e=jar");
        // dep.add(LIBS, "https://oss.sonatype.org/service/local/repositories/releases/content/org/dvare/dynamic-loader/3.0/dynamic-loader-3.0.jar");
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
    private LocalDependencies buildLocalDependencies() {
        final LocalDependencies dep = new LocalDependencies();
        dep.add("libs/abcl.jar");
        dep.add("libs/json.jar");
        return dep;
    }

}
