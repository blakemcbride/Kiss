/*
 * Author: Blake McBride
 * Date: 7/11/26
 *
 * The build system is made up of three classes as follows:
 *
 *     BuildUtils     -  the generic build utilities (usable outside Kiss)
 *     KissBuildUtils -  build procedures common to all Kiss-framework applications
 *     Tasks          -  the application-specific build procedures (or tasks)
 *
 * This class holds the middle layer: procedures every Kiss application needs
 * (Tomcat install/control, development ports, WAR version stamping) but that
 * do not belong in the fully generic BuildUtils, which is also used as part
 * of a generic build system unrelated to the Kiss framework.
 */

package org.kissweb;

import java.io.File;
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URI;
import java.net.URL;

import static org.kissweb.BuildUtils.*;

/**
 * Build procedures common to every Kiss-framework application.
 * These sit between the fully generic {@link BuildUtils} (which is also used
 * outside the Kiss framework) and the application-specific <code>Tasks</code> class.
 */
public class KissBuildUtils {

    /**
     * Private constructor to prevent instantiation of this utility class.
     * All methods in this class are static and should be called directly.
     */
    private KissBuildUtils() {
        // Utility class - not meant to be instantiated
    }

    /**
     * Network ports the embedded Tomcat and the front-end dev server bind
     * to. Normally all four are assigned as one consecutive block by
     * {@link #setPortBase}, called from <code>Tasks.main()</code> with the
     * application's <code>portBase</code> (the initializers below are only
     * the fallback if an application never calls it); each can additionally
     * be overridden on the bld command line so multiple Kiss instances can
     * run side by side without colliding.
     * <ul>
     *   <li><code>-dp PORT</code> / <code>--debug-port=PORT</code>  — JDWP debug (default 9000)</li>
     *   <li><code>-bp PORT</code> / <code>--backend-port=PORT</code>   — Tomcat HTTP (default 8080)</li>
     *   <li><code>-sp PORT</code> / <code>--shutdown-port=PORT</code> — Tomcat shutdown signal (default 8005)</li>
     *   <li><code>-fp PORT</code> / <code>--frontend-port=PORT</code> — the front-end static-file server (default 8000)</li>
     * </ul>
     * The HTTP and shutdown ports are written into
     * <code>tomcat/conf/server.xml</code> on every {@link #installTomcat};
     * the debug port into <code>tomcat/bin/debug</code>; the front-end
     * port into the SimpleWebServer command line. All four are re-applied
     * on every <code>bld</code> invocation, so you can pick different
     * values for each run with no manual cleanup.
     */
    public static String debugPort    = "9000";
    /** Tomcat HTTP port (see {@link #debugPort} for the option syntax). */
    public static String backendPort  = "8080";
    /** Tomcat shutdown port (see {@link #debugPort} for the option syntax). */
    public static String shutdownPort = "8005";
    /** Front-end dev server port (see {@link #debugPort} for the option syntax). */
    public static String frontendPort = "8000";

    /**
     * Assign all four development ports as one consecutive block starting at
     * <code>basePort</code>:
     * <pre>
     *   basePort + 0  — front-end static server (the URL you browse to)
     *   basePort + 1  — Tomcat HTTP (backend)
     *   basePort + 2  — Tomcat shutdown
     *   basePort + 3  — JDWP debug
     * </pre>
     * Called from <code>Tasks.main()</code> (with the application's
     * <code>portBase</code>), <b>before</b> {@link #consumePortOptions}, so each
     * application claims its own non-clashing port block and multiple Kiss
     * applications can run in development mode simultaneously.  Because the
     * command-line port options are parsed afterward, <code>-dp/-bp/-sp/-fp</code>
     * still override the individual ports.
     * <br><br>
     * The front end relies on this convention to find the back end in
     * development: a page served by the front-end static server calls the
     * back end at its own port + 1 (see <code>index.js</code>), so no
     * front-end configuration is needed when the base changes.
     *
     * @param basePort the first port of the block (1 to 65532)
     */
    public static void setPortBase(int basePort) {
        if (basePort <= 0 || basePort > 65532)
            throw new IllegalArgumentException("setPortBase: base port " + basePort + " must be between 1 and 65532");
        frontendPort = Integer.toString(basePort);
        backendPort  = Integer.toString(basePort + 1);
        shutdownPort = Integer.toString(basePort + 2);
        debugPort    = Integer.toString(basePort + 3);
    }

    /**
     * Pull port-override options out of <code>args</code> if present, set
     * the corresponding static fields, and return the remaining arguments
     * for normal task dispatch. Recognized flags (any position):
     * <pre>
     *   -dp PORT, --debug-port=PORT      JDWP   (default 9000)
     *   -bp PORT, --backend-port=PORT    HTTP   (default 8080)
     *   -sp PORT, --shutdown-port=PORT   Tomcat shutdown signal (default 8005)
     *   -fp PORT, --frontend-port=PORT   front-end static server (default 8000)
     * </pre>
     * Non-numeric or out-of-range values are reported on stderr; the
     * default is kept in that case.
     *
     * @param args the raw command-line arguments
     * @return the arguments with the port options removed
     */
    public static String[] consumePortOptions(String[] args) {
        java.util.List<String> out = new java.util.ArrayList<>(args.length);
        for (int i = 0; i < args.length; i++) {
            String a = args[i];
            String[] pair = matchPortOption(a);
            if (pair != null && pair[1] != null) {
                setPort(pair[0], pair[1]);
            } else if (pair != null) {
                if (i + 1 < args.length) {
                    setPort(pair[0], args[i + 1]);
                    i++;
                } else {
                    System.err.println("missing value for " + a + "; ignoring");
                }
            } else {
                out.add(a);
            }
        }
        return out.toArray(new String[0]);
    }

    /**
     * Match one arg against the port-option flags. Returns a 2-element
     * array <code>{which, value}</code> on match, or <code>null</code>
     * otherwise. <code>which</code> is one of "debug", "http", "shutdown",
     * "frontend". <code>value</code> is the part after "=" for long-form
     * options that include one, or <code>null</code> when the value is
     * the next argument.
     */
    private static String[] matchPortOption(String a) {
        if (a.equals("-dp") || a.equals("--debug-port"))     return new String[]{"debug",    null};
        if (a.equals("-bp") || a.equals("--backend-port"))   return new String[]{"http",     null};
        if (a.equals("-sp") || a.equals("--shutdown-port"))  return new String[]{"shutdown", null};
        if (a.equals("-fp") || a.equals("--frontend-port"))  return new String[]{"frontend", null};
        if (a.startsWith("--debug-port="))    return new String[]{"debug",    a.substring("--debug-port=".length())};
        if (a.startsWith("--backend-port="))  return new String[]{"http",     a.substring("--backend-port=".length())};
        if (a.startsWith("--shutdown-port=")) return new String[]{"shutdown", a.substring("--shutdown-port=".length())};
        if (a.startsWith("--frontend-port=")) return new String[]{"frontend", a.substring("--frontend-port=".length())};
        return null;
    }

    /**
     * Print the port-override option lines documented in
     * {@link #consumePortOptions}.  Shared by the <code>help</code> and
     * <code>list-tasks</code> output so the two cannot drift apart.
     * The defaults shown are the live field values, so an application that
     * claims its own block via {@link #setPortBase} displays its real ports.
     */
    public static void printPortOptions() {
        println("  -dp PORT, --debug-port=PORT       JDWP debug port (default " + debugPort + ")");
        println("  -bp PORT, --backend-port=PORT     development back-end port (default " + backendPort + ")");
        println("  -sp PORT, --shutdown-port=PORT    Tomcat shutdown port (default " + shutdownPort + ")");
        println("  -fp PORT, --frontend-port=PORT    development front-end port (default " + frontendPort + ")");
    }

    private static void setPort(String which, String value) {
        int p;
        try {
            p = Integer.parseInt(value.trim());
        } catch (NumberFormatException ignored) {
            System.err.println(which + " port '" + value + "' is not a number; using default");
            return;
        }
        if (p <= 0 || p >= 65536) {
            System.err.println(which + " port '" + value + "' is out of range; using default");
            return;
        }
        String s = Integer.toString(p);
        switch (which) {
            case "debug":    debugPort    = s; break;
            case "http":     backendPort  = s; break;
            case "shutdown": shutdownPort = s; break;
            case "frontend": frontendPort = s; break;
        }
    }

    /**
     * Gets the absolute path to the local Tomcat installation directory.
     *
     * @return the absolute path to the tomcat directory
     */
    public static String getTomcatPath() {
        return (new File("tomcat")).getAbsolutePath();
    }

    /**
     * Download, unpack, and configure the local Tomcat installation.
     * <br><br>
     * On every call (whether or not Tomcat was just downloaded), the
     * configured HTTP and shutdown ports are re-stamped into
     * <code>tomcat/conf/server.xml</code> and the debug helper scripts are
     * regenerated, so port options take effect on each bld invocation.
     *
     * @param tomcatVersion the full Tomcat version, e.g. "11.0.12"
     */
    public static void installTomcat(String tomcatVersion) {
        final String tomcatTarFile = "apache-tomcat-" + tomcatVersion + ".tar.gz";
        final String tomcatMajorVersion = tomcatVersion.substring(0, tomcatVersion.indexOf('.'));
        if (!exists("tomcat/bin/startup.sh")) {
            download(tomcatTarFile, ".", "https://archive.apache.org/dist/tomcat/tomcat-" + tomcatMajorVersion + "/v" + tomcatVersion + "/bin/" + tomcatTarFile);
            gunzip(tomcatTarFile, "tomcat", 1);
            rmTree("tomcat/webapps/ROOT");
        }
        // Always re-stamp server.xml with the current backendPort / shutdownPort
        // so the options take effect on each bld invocation.
        rewriteServerXmlPorts();
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
            // writeToFile is a no-op when the target exists, so explicitly
            // remove the helper scripts first.  Without this, changes to
            // debugPort or to the current working directory
            // would not propagate into the regenerated scripts.
            rm("tomcat\\bin\\debug.cmd");
            rm("tomcat\\bin\\stopdebug.cmd");
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
            // writeToFile is a no-op when the target exists, so explicitly
            // remove the debug helper first.  Without this, changes to
            // debugPort or to the current working directory
            // would not propagate into the regenerated script.
            rm("tomcat/bin/debug");
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
     * Edit <code>tomcat/conf/server.xml</code> in place so the
     * <code>&lt;Server&gt;</code> shutdown port and the HTTP/1.1
     * <code>&lt;Connector&gt;</code> port match the currently configured
     * values. Idempotent — repeated calls converge on the configured
     * ports, regardless of what was in the file previously.
     * <br><br>
     * Called automatically by {@link #installTomcat}.  An application with
     * its own Tomcat setup procedure can call it directly (after assigning
     * {@link #backendPort} / {@link #shutdownPort}) so that a freshly
     * extracted stock <code>server.xml</code> is always re-stamped with the
     * application's ports.
     */
    public static void rewriteServerXmlPorts() {
        java.nio.file.Path p = java.nio.file.Paths.get("tomcat/conf/server.xml");
        if (!java.nio.file.Files.exists(p))
            return;   // tomcat not yet installed; installTomcat runs again later
        try {
            String content = new String(java.nio.file.Files.readAllBytes(p), java.nio.charset.StandardCharsets.UTF_8);
            String updated = content
                    .replaceFirst("<Server port=\"\\d+\"",
                                  "<Server port=\"" + shutdownPort + "\"")
                    .replaceFirst("<Connector port=\"\\d+\" protocol=\"HTTP/1\\.1\"",
                                  "<Connector port=\"" + backendPort + "\" protocol=\"HTTP/1.1\"");
            if (!updated.equals(content))
                java.nio.file.Files.write(p, updated.getBytes(java.nio.charset.StandardCharsets.UTF_8));
        } catch (java.io.IOException e) {
            System.err.println("warning: could not rewrite server.xml ports: " + e.getMessage());
        }
    }

    /**
     * Stop the local Tomcat by sending the shutdown command directly to its
     * shutdown port (exactly what <code>shutdown.sh</code> does, minus the JVM
     * launch).  Unlike the script, this handles a Tomcat that is not running —
     * for example one that crashed or was never started — with a clear
     * one-line message instead of a
     * SEVERE stack trace.  If the port is not listening, it waits briefly and
     * retries once, in case Tomcat is still in the middle of starting.
     */
    public static void shutdownTomcat() {
        int port = Integer.parseInt(shutdownPort);
        for (int attempt = 0; attempt < 2; attempt++) {
            try (java.net.Socket s = new java.net.Socket()) {
                s.connect(new java.net.InetSocketAddress("localhost", port), 2000);
                s.getOutputStream().write("SHUTDOWN".getBytes(java.nio.charset.StandardCharsets.UTF_8));
                s.getOutputStream().flush();
                println("shutting down tomcat");
                return;
            } catch (Exception e) {
                //  Not listening.  On the first miss, give a just-launched Tomcat
                //  a moment to open its shutdown port, then try once more.
                if (attempt == 0) {
                    try {
                        Thread.sleep(2000);
                    } catch (InterruptedException ignored) {
                    }
                }
            }
        }
        println("tomcat is not running (it may have exited after a startup error - see tomcat/logs/catalina.out)");
    }

    /**
     * Stamp production cache-busting values into the WAR's frontend copies.
     * <br><br>
     * Sets a fresh software version (a new UUID) and the current release date, and turns
     * cache control on, so every WAR forces clients to download the new code.  The version
     * and cache-control flag live in <code>index.html</code> (<code>kiss-version</code> /
     * <code>kiss-cache-control</code> meta tags — the only perpetually-fresh file); the
     * release date lives in <code>SystemInfo.js</code>.
     * <br><br>
     * Operates on the staged copies in <code>explodedDir</code> only, so the source files
     * keep their EDIT placeholders.  Modifies only the meta values, not the bootstrap
     * kernel, so the CSP hash is unaffected.
     *
     * @param explodedDir the staged (exploded WAR) directory containing index.html and SystemInfo.js
     */
    public static void stampVersion(String explodedDir) {
        final String uuid = java.util.UUID.randomUUID().toString();
        final String date = java.time.LocalDate.now().toString();
        try {
            final java.nio.file.Path idx = java.nio.file.Paths.get(explodedDir, "index.html");
            String html = java.nio.file.Files.readString(idx);
            html = html.replaceAll("(name=\"kiss-version\"\\s+content=\")[^\"]*(\")",
                    "$1" + java.util.regex.Matcher.quoteReplacement(uuid) + "$2");
            html = html.replaceAll("(name=\"kiss-cache-control\"\\s+content=\")[^\"]*(\")",
                    "$1true$2");
            java.nio.file.Files.writeString(idx, html);

            //  Applications predating SystemInfo.js simply have nothing to stamp there.
            final java.nio.file.Path si = java.nio.file.Paths.get(explodedDir, "SystemInfo.js");
            if (java.nio.file.Files.exists(si)) {
                String js = java.nio.file.Files.readString(si);
                js = js.replaceAll("(SystemInfo\\.releaseDate\\s*=\\s*\")[^\"]*(\")",
                        "$1" + java.util.regex.Matcher.quoteReplacement(date) + "$2");
                java.nio.file.Files.writeString(si, js);
            }

            println("WAR stamped: kiss-version=" + uuid + ", releaseDate=" + date + ", cache-control=true");
        } catch (java.io.IOException e) {
            throw new RuntimeException("Failed to stamp version into WAR frontend files", e);
        }
        //  A WAR is always served by the back end itself.
        stampSameOriginBackend(explodedDir);
    }

    /**
     * Mark a deployed copy of the front end as served by the back end itself:
     * sets <code>SystemInfo.sameOriginBackend = true</code> in the copy's
     * <code>SystemInfo.js</code>.  The front end then calls the back end at
     * the page's own origin instead of applying the development port-block
     * convention.  Applied to every copy Tomcat serves — the production WAR
     * staging (via {@link #stampVersion}) and the development
     * <code>tomcat/webapps/ROOT</code> (from the develop / start-backend
     * tasks).  The source copy, which the front-end dev server serves, is
     * never stamped.
     *
     * @param dir the directory containing the deployed SystemInfo.js
     */
    public static void stampSameOriginBackend(String dir) {
        try {
            final java.nio.file.Path si = java.nio.file.Paths.get(dir, "SystemInfo.js");
            //  Applications without a SystemInfo.js (no front end, or one predating
            //  it) have nothing to stamp.
            if (!java.nio.file.Files.exists(si))
                return;
            String js = java.nio.file.Files.readString(si);
            String updated = js.replaceAll("(SystemInfo\\.sameOriginBackend\\s*=\\s*)[^;]*(;)", "$1true$2");
            if (!updated.equals(js))
                java.nio.file.Files.writeString(si, updated);
        } catch (java.io.IOException e) {
            throw new RuntimeException("Failed to stamp sameOriginBackend into " + dir + "/SystemInfo.js", e);
        }
    }

    /**
     * Send a request to stop the development frontend server.
     * Uses the configured {@link #frontendPort}.
     */
    public static void stopFrontendServer() {
        try {
            URL url = URI.create("http://localhost:" + frontendPort + "/stop-server").toURL();
            HttpURLConnection connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            connection.getInputStream().close();
            connection.disconnect();
        } catch (IOException e) {
            //e.printStackTrace();
        }
    }
}
