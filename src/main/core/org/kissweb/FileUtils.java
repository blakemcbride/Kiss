package org.kissweb;

import org.kissweb.restServer.MainServlet;

import java.io.BufferedInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.nio.file.StandardOpenOption;

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

    /**
     * Write String to file fileName.  File is created if it doesn't exist and truncated otherwise.
     *
     * @param fileName
     * @param str
     * @throws IOException
     */
    public static void write(String fileName, String str) throws IOException {
        Files.write(Paths.get(fileName), str.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING);
    }

    /**
     * Write byte array to file fileName.  File is created if it doesn't exist and truncated otherwise.
     *
     * @param fileName
     * @param vec
     * @throws IOException
     */
    public static void write(String fileName, byte [] vec) throws IOException {
        Files.write(Paths.get(fileName), vec, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING);
    }

    /**
     * Append String to file fileName.  File is created if it doesn't exist and appended otherwise.
     *
     * @param fileName
     * @param str
     * @throws IOException
     */
    public static void append(String fileName, String str) throws IOException {
        Files.write(Paths.get(fileName), str.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
    }

    /**
     * Append byte array to file fileName.  File is created if it doesn't exist and appended otherwise.
     *
     * @param fileName
     * @param vec
     * @throws IOException
     */
    public static void append(String fileName, byte [] vec) throws IOException {
        Files.write(Paths.get(fileName), vec, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
    }

    /**
     * Read the entire contents of a file into a String.
     *
     * @param fileName
     * @return
     * @throws IOException
     */
    public static String readFile(String fileName) throws IOException {
        return new String(readFileBytes(fileName), StandardCharsets.UTF_8);
    }

    /**
     * Read the entire contents of a file into a byte array.
     *
     * @param fileName
     * @return
     * @throws IOException
     */
    public static byte [] readFileBytes(String fileName) throws IOException {
        return Files.readAllBytes(Paths.get(fileName));
    }

    /**
     * Read all bytes from a <code>BufferedInputStream</code> and return a byte array with the data.
     * This is useful when processing an uploaded file.
     *
     * @param bis
     * @return
     * @throws IOException
     */
    public static byte [] readAllBytes(BufferedInputStream bis) throws IOException {
        final ByteArrayOutputStream baos = new ByteArrayOutputStream();
        final byte[] buffer = new byte[1024];
        int len;
        while ((len = bis.read(buffer)) != -1)
            baos.write(buffer, 0, len);
        baos.flush();
        return baos.toByteArray();
    }

    /**
     * Copy file
     *
     * @param source file
     * @param dest  file or directory
     * @throws IOException
     */
    public static void copy(String source, String dest) throws IOException {
        File sfile = new File(source);
        File dfile = new File(dest);
        if (!sfile.exists())
            throw new IOException("copy: " + source + " does not exist");
        if (sfile.isDirectory())
            throw new IOException("copy: " + source + " is a directory");
        if (dfile.exists() && dfile.isDirectory())
            dfile = new File(dfile, sfile.getName());
        dfile.getParentFile().mkdirs();
        Files.copy(sfile.toPath(), dfile.toPath(), StandardCopyOption.REPLACE_EXISTING);
    }
}
