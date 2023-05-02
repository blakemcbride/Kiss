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

    final String LIBS = "libs";
    final ForeignDependencies foreignLibs = buildForeignDependencies();
    final LocalDependencies localLibs = buildLocalDependencies();
    final String tomcatTarFile = "apache-tomcat-9.0.65.tar.gz";
    final String BUILDDIR = "build.work";
    final String explodedDir = BUILDDIR + "/" + "exploded";
    final String postgresqlJar = "postgresql-42.5.1.jar";
    final String groovyJar = "groovy-4.0.11.jar";
    final String debugPort = "9000";

    /**
     * Build the whole system
     *
     * 1. download needed jar files
     * 2. build the system into a deployable war file
     * 3. set up a local tomcat server
     * 4. deploy the war file to the local tomcat
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
        buildJava("src/main/core", targetPath, localLibs, foreignLibs);
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
    void jar() {
        libs();
        buildJava("src/main/core", explodedDir + "/WEB-INF/classes", localLibs, foreignLibs);
        rm(explodedDir + "/WEB-INF/lib/javax.servlet-api-4.0.1.jar");
        createJar(explodedDir + "/WEB-INF/classes", BUILDDIR + "/Kiss.jar");
        //println("Kiss.jar has been created in the " + BUILDDIR + " directory");
    }

    /**
     * Create an executable JAR that includes Kiss, Groovy, and the PostgreSQL driver.
     * It runs an arbitrary groovy file in the context of Kiss and PostgreSQL.
     *
     * All that is needed is KissGP.jar
     *
     * Usage:  java -jar KissGP.jar [groovy-file] [args]...
     *
     * Other databases can be used also.  See the manual.
     */
    void KissGP() {
        final String name = "KissGP";
        final String workDir = BUILDDIR + "/" + name;
        final String jarName = workDir + ".jar";
        jar();
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
     * Build the system into explodedDir
     */
    void build() {
        libs();
        copyTree("src/main/frontend", explodedDir);
        writeToFile(explodedDir + "/META-INF/MANIFEST.MF", "Manifest-Version: 1.0\n");
        copyTree("src/main/backend", explodedDir + "/WEB-INF/backend");
        copyTree("libs", explodedDir + "/WEB-INF/lib");
        buildJava("src/main/core", explodedDir + "/WEB-INF/classes", localLibs, foreignLibs);
        rm(explodedDir + "/WEB-INF/lib/javax.servlet-api-4.0.1.jar");
        copyRegex("src/main/core/org/kissweb/lisp", explodedDir + "/WEB-INF/classes/org/kissweb/lisp", ".*\\.lisp", null, false);
        copy("src/main/core/log4j2.xml", explodedDir + "/WEB-INF/classes");
    }

    /**
     * Build the system and create the deployable WAR file.
     */
    void war() {
        build();
        createJar(explodedDir, BUILDDIR + "/Kiss.war");
        //println("Kiss.war has been created in the " + BUILDDIR + " directory");
    }

    private void deployWar() {
        copy(BUILDDIR + "/Kiss.war", "tomcat/webapps/ROOT.war");
    }

    void setupTomcat() {
        if (!exists("tomcat/bin/startup.sh")) {
            download(tomcatTarFile, ".", "https://archive.apache.org/dist/tomcat/tomcat-9/v9.0.65/bin/apache-tomcat-9.0.65.tar.gz");
            gunzip(tomcatTarFile, "tomcat", 1);
            rmTree("tomcat/webapps/ROOT");
            //run("tar xf apache-tomcat-9.0.31.tar.gz --one-top-level=tomcat --strip-components=1");
        }
        if (isWindows) {
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
    }

    /**
     * Build and run the system
     *
     * 1. download needed jar files
     * 2. build the system into a deployable war file
     * 3. set up a local tomcat server
     * 4. deploy the war file to the local tomcat
     * 5. build JavaDocs
     * 6. run the local tomcat
     */
    void develop() {
        Process proc;
        build();
        war();
        setupTomcat();
        deployWar();
        if (isWindows)
            runWait(true, "tomcat\\bin\\debug.cmd");
        else
            runWait(true, "tomcat/bin/debug");
        proc = runBackground("java -jar SimpleWebServer.jar -d src/main/frontend");
        println("Server log can be viewed at " + cwd() + "/tomcat/logs/catalina.out");
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

    void javadoc() {
        libs();
        buildJavadoc("src/main/core", "libs", BUILDDIR + "/javadoc");
    }

    void clean() {
        rmTree(BUILDDIR);
        rm("manual/Kiss.log");
        rm("manual/Kiss.aux");
        rm("manual/Kiss.toc");
    }

    void realclean() {
        clean();
        rmTree("src/main/frontend/lib");
        delete(foreignLibs);
        rmTree("tomcat");
        rm(tomcatTarFile);
        rm("manual/Kiss.pdf");
        
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

        // libraries that don't have their version number in the file name
        // must be removed from cache
        removeFromCache("ag-grid-community.noStyle.min.js");
        removeFromCache("ag-grid.min.css");
        removeFromCache("ag-theme-balham.min.css");
    }

    private ForeignDependencies buildForeignDependencies() {
        final ForeignDependencies dep = new ForeignDependencies();
        dep.add("c3p0-0.9.5.5.jar", LIBS, "https://repo1.maven.org/maven2/com/mchange/c3p0/0.9.5.5/c3p0-0.9.5.5.jar");
        dep.add(groovyJar, LIBS, "https://repo1.maven.org/maven2/org/apache/groovy/groovy/4.0.11/groovy-4.0.11.jar");
        dep.add("javax.servlet-api-4.0.1.jar", LIBS, "https://repo1.maven.org/maven2/javax/servlet/javax.servlet-api/4.0.1/javax.servlet-api-4.0.1.jar");
        dep.add("log4j-core-2.19.0.jar", LIBS, "https://repo1.maven.org/maven2/org/apache/logging/log4j/log4j-core/2.19.0/log4j-core-2.19.0.jar");
        dep.add("log4j-api-2.19.0.jar", LIBS, "https://repo1.maven.org/maven2/org/apache/logging/log4j/log4j-api/2.19.0/log4j-api-2.19.0.jar");
        dep.add("log4j-1.2-api-2.19.0.jar", LIBS, "https://repo1.maven.org/maven2/org/apache/logging/log4j/log4j-1.2-api/2.19.0/log4j-1.2-api-2.19.0.jar");
        dep.add("mchange-commons-java-0.2.20.jar", LIBS, "https://repo1.maven.org/maven2/com/mchange/mchange-commons-java/0.2.20/mchange-commons-java-0.2.20.jar");
        dep.add("mssql-jdbc-11.2.2.jre8.jar", LIBS, "https://repo1.maven.org/maven2/com/microsoft/sqlserver/mssql-jdbc/11.2.2.jre8/mssql-jdbc-11.2.2.jre8.jar");
        dep.add("mysql-connector-java-8.0.30.jar", LIBS, "https://repo1.maven.org/maven2/mysql/mysql-connector-java/8.0.30/mysql-connector-java-8.0.30.jar");
        dep.add("ojdbc10-19.3.0.0.jar", LIBS, "https://repo1.maven.org/maven2/com/oracle/ojdbc/ojdbc10/19.3.0.0/ojdbc10-19.3.0.0.jar");
        dep.add(postgresqlJar, LIBS, "https://repo1.maven.org/maven2/org/postgresql/postgresql/42.5.1/postgresql-42.5.1.jar");
        dep.add("slf4j-api-1.7.30.jar", LIBS, "https://repo1.maven.org/maven2/org/slf4j/slf4j-api/1.7.30/slf4j-api-1.7.30.jar");
        dep.add("slf4j-simple-1.7.30.jar", LIBS, "https://repo1.maven.org/maven2/org/slf4j/slf4j-simple/1.7.30/slf4j-simple-1.7.30.jar");
        dep.add("sqlite-jdbc-3.40.0.0.jar", LIBS, "https://repo1.maven.org/maven2/org/xerial/sqlite-jdbc/3.40.0.0/sqlite-jdbc-3.40.0.0.jar");
        //  https://github.com/dvare/dynamic-loader
        dep.add("dynamic-loader-3.1.jar", LIBS, "https://oss.sonatype.org/service/local/artifact/maven/redirect?r=releases&g=org.dvare&a=dynamic-loader&v=3.1&e=jar");
        // dep.add("dynamic-loader-3.0.jar", LIBS, "https://oss.sonatype.org/service/local/repositories/releases/content/org/dvare/dynamic-loader/3.0/dynamic-loader-3.0.jar");
        dep.add("jquery-3.6.3.min.js", "src/main/frontend/lib", "https://code.jquery.com/jquery-3.6.3.min.js");
        dep.add("ag-grid-community.noStyle.min.js", "src/main/frontend/lib", "https://cdnjs.cloudflare.com/ajax/libs/ag-grid/25.1.0/ag-grid-community.noStyle.min.js");
        dep.add("ag-grid.min.css", "src/main/frontend/lib", "https://cdnjs.cloudflare.com/ajax/libs/ag-grid/25.1.0/styles/ag-grid.min.css");
        dep.add("ag-theme-balham.min.css", "src/main/frontend/lib", "https://cdnjs.cloudflare.com/ajax/libs/ag-grid/25.1.0/styles/ag-theme-balham.min.css");
        return dep;
    }

    private LocalDependencies buildLocalDependencies() {
        final LocalDependencies dep = new LocalDependencies();
        dep.add("libs/abcl.jar");
        dep.add("libs/json.jar");
        return dep;
    }

}
