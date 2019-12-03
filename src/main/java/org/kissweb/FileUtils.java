package org.kissweb;

import org.kissweb.rest.MainServlet;

import java.io.File;
import java.io.IOException;

/**
 * Utilities to deal with files on the back-end.  These are usually PDF or CSV files that
 * will be sent to the front-end.
 *
 * @author Blake McBride
 */
public class FileUtils {

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
    public static File createTempFile(final String prefix, final String suffix)
    {
        deleteOldFiles(1);
        File dir = new File(MainServlet.getApplicationPath(), "temp");
        dir.mkdir();
        final File f;
        try {
            f = File.createTempFile(prefix, suffix, dir);
            f.deleteOnExit();
        } catch (IOException e) {
            return null;
        }
        return f;
    }

    private static void deleteOldFiles(long daysOld) {
        try {
            long nDaysAgo = new java.util.Date().getTime();

            nDaysAgo -= daysOld * (24L * 60L * 60L * 1000L);

            final File[] fyles = new File(MainServlet.getApplicationPath(), "temp").listFiles();

            if (fyles != null)
                for (final File element : fyles)
                    if (element.isFile() && element.lastModified() < nDaysAgo)
                        element.delete();
        } catch (Exception e) {
        }
    }

}
