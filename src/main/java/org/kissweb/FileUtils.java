package org.kissweb;

import org.kissweb.restServer.MainServlet;

import java.io.File;
import java.io.IOException;

/**
 * Utilities to deal with files on the back-end.  These are usually PDF or CSV files that
 * will be sent to the front-end.
 *
 * @author Blake McBride
 */
public class FileUtils {

    private final static String TempDir = "temporary";
    private final static int DaysOld = 1;

    /**
     * Create a new file in a server accessible temp directory.  The file returned is guaranteed
     * to be new.  The name of the file uses the supplied prefix and suffix but with a random
     * center.  The file is deleted when it is more than a day old.  This provides plenty of time for the user
     * to download a report or data file.
     *
     *
     * @param prefix at least 3 characters long
     * @param suffix
     * @return
     */
    public static File createReportFile(final String prefix, final String suffix)
    {
        File dir;

        try {
            // Needed when Kiss is not used as a server
            Class.forName("javax.servlet.http.HttpServlet");
            dir = new File(MainServlet.getRootPath(), TempDir);
            dir.mkdir();
        } catch (ClassNotFoundException e) {
            dir = null;
        }
        final File f;
        try {
            f = File.createTempFile(prefix, suffix, dir);
            f.deleteOnExit();
        } catch (IOException e) {
            return null;
        }
        if (dir != null)
            deleteOldFiles(dir);
        return f;
    }

    private static void deleteOldFiles(File dir) {
        try {
            long nDaysAgo = new java.util.Date().getTime();

            nDaysAgo -= DaysOld * (24L * 60L * 60L * 1000L);

            final File[] fyles = dir.listFiles();

            if (fyles != null)
                for (final File element : fyles)
                    if (element.isFile() && element.lastModified() < nDaysAgo)
                        element.delete();
        } catch (Exception e) {
        }
    }

    /**
     * Takes a previously obtained File and returns what path the front-end should use to access the file.
     */
    public static String getHTTPPath(File f) {
        String name1 = f.getName();
        f = f.getParentFile();
        if (f == null)
            return name1;
        String name2 = f.getName();
        return name2 + "/" + name1;
    }

    /**
     * Takes a path to a file and returns what path the front-end should use to access the file.
     */
    public static String getHTTPPath(String fn) {
        return getHTTPPath(new File(fn));
    }

}
