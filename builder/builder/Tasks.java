/*
 * Author: Blake McBride
 * Date: 2/16/20
 *
 * I've found that I spend more time messing with build programs (such as Maven, Gradle, and others) than
 * the underlying application I am trying to build.  They all do the normal things very, very easily.
 * But when you try to go off their beaten path it gets real difficult real fast.  Being sick and
 * tired of this, and having easily built a shell script to build what I want, I needed a more portable
 * solution.  The files in this directory are that solution.
 *
 * There are two classes as follows:
 *
 *     BuildUtils -  the generic utilities needed to build
 *     Tasks      -  the application-specific build procedures (or tasks)
 *
 *    Non-private instance methods with no parameters are considered tasks.
 */


package builder;

import static builder.BuildUtils.*;

public class Tasks {

    final String LIBS = "libs";
    final ForeignDependencies foreignLibs = buildForeignDependencies();
    final LocalDependencies localLibs = buildLocalDependencies();
    final String tomcatTarFile = "apache-tomcat-9.0.31.tar.gz";
    final String BUILDDIR = "build.work";
    final String explodedDir = BUILDDIR + "/" + "exploded";

    void all() {
        war();
        setupTomcat();
        deployWar();
        javadoc();
    }

    void libs() {
        mkdir(LIBS);
        downloadAll(foreignLibs);
    }

    void war() {
        libs();
        copyTree("src/main/webapp", explodedDir);
        mkdir(explodedDir + "/META-INF");
        writeToFile(explodedDir + "/META-INF/MANIFEST.MF", "Manifest-Version: 1.0\n");
        copyTree("src/main/application", explodedDir + "/WEB-INF");
        mkdir(explodedDir + "/WEB-INF/lib");
        copyTree("libs", explodedDir + "/WEB-INF/lib");
        mkdir(explodedDir + "/WEB-INF/classes");
        buildJava("src/main/java", explodedDir + "/WEB-INF/classes", localLibs, foreignLibs);
        rm(explodedDir + "/WEB-INF/lib/javax.servlet-api-4.0.1.jar");
        copyRegex("src/main/java/org/kissweb/lisp", explodedDir + "/WEB-INF/classes/org/kissweb/lisp", ".*\\.lisp");
        createJar(explodedDir, BUILDDIR + "/Kiss.war");
        //println("Kiss.war has been created in the " + BUILDDIR + " directory");
    }

    private void deployWar() {
        copy(BUILDDIR + "/Kiss.war", "tomcat/webapps");
    }

    void setupTomcat() {
        if (!exists("tomcat/bin/startup.sh")) {
            download(tomcatTarFile, ".", "http://mirrors.advancedhosters.com/apache/tomcat/tomcat-9/v9.0.31/bin");
            gunzip(tomcatTarFile, "tomcat", 1);
            //run("tar xf apache-tomcat-9.0.31.tar.gz --one-top-level=tomcat --strip-components=1");
        }
        if (isWindows) {
            writeToFile("tomcat\\bin\\debug.cmd", "@echo off\n" +
                    "cd " + getcwd() + "\\tomcat\\bin\n" +
                    "set JAVA_HOME=" + getJavaPathOnWindows() + "\n" +
                    "set CATALINA_HOME=" + getTomcatPath() + "\n" +
                    "set JPDA_ADDRESS=9000\n" +
                    "set JPDA_TRANSPORT=dt_socket\n" +
                    "catalina.bat jpda start\n");
            writeToFile("tomcat\\bin\\stopdebug.cmd", "@echo off\n" +
                    "cd " + getcwd() + "\\tomcat\\bin\n" +
                    "set JAVA_HOME=" + getJavaPathOnWindows() + "\n" +
                    "set CATALINA_HOME=" + getTomcatPath() + "\n" +
                    "shutdown.bat\n");
        } else {
            writeToFile("tomcat/bin/debug", "#\n" +
                    "cd " + getcwd() + "/tomcat/bin\n" +
                    "export JPDA_ADDRESS=9000\n" +
                    "export JPDA_TRANSPORT=dt_socket\n" +
                    "./catalina.sh jpda start\n");
            makeExecutable("tomcat/bin/debug");
        }
    }

    void develop() {
        war();
        setupTomcat();
        deployWar();
        if (isWindows)
            run(true, "tomcat\\bin\\debug.cmd");
        else
            run(true, "tomcat/bin/debug");
	println("Server log can be viewed at " + cwd() + "/tomcat/logs/catalina.out");
        println("You can browse to http://localhost:8080/Kiss");
        println("The app can also be debugged at port 9000");
        println("hit any key to stop tomcat");
        readChar();
        println("shutting down tomcat");
        if (isWindows)
            run(true, "tomcat\\bin\\stopdebug.cmd");
        else
            run(true, "tomcat/bin/shutdown.sh");
    }

    void javadoc() {
        buildJavadoc("src/main/java", BUILDDIR + "/javadoc");
    }

    void clean() {
        rmTree(BUILDDIR);
    }

    void realclean() {
        clean();
	rmTree("src/main/webapp/lib");
        delete(foreignLibs);
        rmTree("tomcat");
        rm(tomcatTarFile);

        rmRegex("builder/builder", ".*\\.class");

        rmTree(".project");
        rmTree(".settings");
        rmTree(".vscode");

	// intelliJ
        rmTree(".idea");
        rmTree("out");
	rmRegex(".", ".*\\.iml");
	
        // NetBeans
        rmTree("dist");
        rmTree("nbproject");
        rmTree("build");
        rm("nbbuild.xml");
    }

    private ForeignDependencies buildForeignDependencies() {
        final ForeignDependencies dep = new ForeignDependencies();
        dep.add("c3p0-0.9.5.5.jar", LIBS, "https://repo1.maven.org/maven2/com/mchange/c3p0/0.9.5.5/c3p0-0.9.5.5.jar");
        dep.add("groovy-all-2.4.18.jar", LIBS, "https://repo1.maven.org/maven2/org/codehaus/groovy/groovy-all/2.4.18/groovy-all-2.4.18.jar");
        dep.add("javax.servlet-api-4.0.1.jar", LIBS, "https://repo1.maven.org/maven2/javax/servlet/javax.servlet-api/4.0.1/javax.servlet-api-4.0.1.jar");
        dep.add("log4j-1.2.17.jar", LIBS, "https://repo1.maven.org/maven2/log4j/log4j/1.2.17/log4j-1.2.17.jar");
        dep.add("mchange-commons-java-0.2.20.jar", LIBS, "https://repo1.maven.org/maven2/com/mchange/mchange-commons-java/0.2.20/mchange-commons-java-0.2.20.jar");
        dep.add("mssql-jdbc-8.2.0.jre8.jar", LIBS, "https://repo1.maven.org/maven2/com/microsoft/sqlserver/mssql-jdbc/8.2.0.jre8/mssql-jdbc-8.2.0.jre8.jar");
        dep.add("mysql-connector-java-8.0.19.jar", LIBS, "https://repo1.maven.org/maven2/mysql/mysql-connector-java/8.0.19/mysql-connector-java-8.0.19.jar");
        dep.add("ojdbc10-19.3.0.0.jar", LIBS, "https://repo1.maven.org/maven2/com/oracle/ojdbc/ojdbc10/19.3.0.0/ojdbc10-19.3.0.0.jar");
        dep.add("postgresql-42.2.10.jar", LIBS, "https://repo1.maven.org/maven2/org/postgresql/postgresql/42.2.10/postgresql-42.2.10.jar");
        dep.add("slf4j-api-1.7.30.jar", LIBS, "https://repo1.maven.org/maven2/org/slf4j/slf4j-api/1.7.30/slf4j-api-1.7.30.jar");
        dep.add("slf4j-simple-1.7.30.jar", LIBS, "https://repo1.maven.org/maven2/org/slf4j/slf4j-simple/1.7.30/slf4j-simple-1.7.30.jar");
        dep.add("sqlite-jdbc-3.30.1.jar", LIBS, "https://repo1.maven.org/maven2/org/xerial/sqlite-jdbc/3.30.1/sqlite-jdbc-3.30.1.jar");
        dep.add("jquery-3.4.1.min.js", "src/main/webapp/lib", "https://ajax.googleapis.com/ajax/libs/jquery/3.4.1/jquery.min.js");
        return dep;
    }

    private LocalDependencies buildLocalDependencies() {
        final LocalDependencies dep = new LocalDependencies();
        dep.add("libs/abcl.jar");
        dep.add("libs/dynamic-loader-1.4-SNAPSHOT.jar");
        return dep;
    }

}
