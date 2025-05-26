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
     * @param suffix file extension or suffix for the temporary file
     * @return newly created temporary file or null if creation failed
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
     *
     * @param f the file to get the HTTP path for
     * @return the relative HTTP path to access the file
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
     *
     * @param fn the file path to get the HTTP path for
     * @return the relative HTTP path to access the file
     */
    public static String getHTTPPath(String fn) {
        return getHTTPPath(new File(fn));
    }

    /**
     * Write String to file fileName.  File is created if it doesn't exist and truncated otherwise.
     *
     * @param fileName the name of the file to write to
     * @param str the string to write to the file
     * @throws IOException if an I/O error occurs
     */
    public static void write(String fileName, String str) throws IOException {
        Files.write(Paths.get(fileName), str.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING);
    }

    /**
     * Write byte array to file fileName.  File is created if it doesn't exist and truncated otherwise.
     *
     * @param fileName the name of the file to write to
     * @param vec the byte array to write to the file
     * @throws IOException if an I/O error occurs
     */
    public static void write(String fileName, byte [] vec) throws IOException {
        Files.write(Paths.get(fileName), vec, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING);
    }

        /**
     * Write byte array to file fileName.  File is created if it doesn't exist and truncated otherwise.
     *
     * @param fileName the name of the file to write to
     * @param vec the Byte array to write to the file
     * @throws IOException if an I/O error occurs
     */
    public static void write(String fileName, Byte [] vec) throws IOException {
        Files.write(Paths.get(fileName), ArrayUtils.toPrimitiveByteArray(vec), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING);
    }

    /**
     * Append String to file fileName.  File is created if it doesn't exist and appended otherwise.
     *
     * @param fileName the name of the file to append to
     * @param str the string to append to the file
     * @throws IOException if an I/O error occurs
     */
    public static void append(String fileName, String str) throws IOException {
        Files.write(Paths.get(fileName), str.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
    }

    /**
     * Append byte array to file fileName.  File is created if it doesn't exist and appended otherwise.
     *
     * @param fileName the name of the file to append to
     * @param vec the byte array to append to the file
     * @throws IOException if an I/O error occurs
     */
    public static void append(String fileName, byte [] vec) throws IOException {
        Files.write(Paths.get(fileName), vec, StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
    }

    /**
     * Append byte array to file fileName.  File is created if it doesn't exist and appended otherwise.
     *
     * @param fileName the name of the file to append to
     * @param vec the Byte array to append to the file
     * @throws IOException if an I/O error occurs
     */
    public static void append(String fileName, Byte [] vec) throws IOException {
        Files.write(Paths.get(fileName), ArrayUtils.toPrimitiveByteArray(vec), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
    }

    /**
     * Read the entire contents of a file into a String.
     *
     * @param fileName the name of the file to read
     * @return the contents of the file as a String
     * @throws IOException if an I/O error occurs
     */
    public static String readFile(String fileName) throws IOException {
        return new String(readFileBytes(fileName), StandardCharsets.UTF_8);
    }

    /**
     * Read the entire contents of a file into a byte array.
     *
     * @param fileName the name of the file to read
     * @return the contents of the file as a byte array
     * @throws IOException if an I/O error occurs
     */
    public static byte [] readFileBytes(String fileName) throws IOException {
        return Files.readAllBytes(Paths.get(fileName));
    }

    /**
     * Read all bytes from a <code>BufferedInputStream</code> and return a byte array with the data.
     * This is useful when processing an uploaded file.
     *
     * @param bis the BufferedInputStream to read from
     * @return the data from the stream as a byte array
     * @throws IOException if an I/O error occurs
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
     * @param source source file path
     * @param dest destination file or directory path
     * @throws IOException if an I/O error occurs
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

    /**
	 * Returns the MIME type based on the file name's extension.
	 *
	 * @param  fname  the complete file name or the extension
	 * @return        the MIME type corresponding to the file extension, or "application/octet-stream" if not found
	 */
	public static String getMimeType(String fname) {
		final String dflt = "application/octet-stream";
        String ext;
        if (fname == null || fname.isEmpty())
            return dflt;
        int idx = fname.lastIndexOf('.');
        if (idx == -1)
            ext = fname;  //  only the extension was passed in
        else if (idx == fname.length() - 1)
            return dflt;
        else
            ext = fname.substring(idx+1);
        switch (ext.toLowerCase()) {
            case "aac": return "audio/aac";
            case "abw": return "application/x-abiword";
            case "arc": return "application/x-freearc";
            case "avi": return "video/x-msvideo";
            case "azw": return "application/vnd.amazon.ebook";
            case "bin": return "application/octet-stream";
            case "bmp": return "image/bmp";
            case "bz": return "application/x-bzip";
            case "bz2": return "application/x-bzip2";
            case "cda": return "application/x-cdf";
            case "csh": return "application/x-csh";
            case "css": return "text/css";
            case "csv": return "text/csv";
            case "doc": return "application/msword";
            case "docx": return "application/vnd.openxmlformats-officedocument.wordprocessingml.document";
            case "eot": return "application/vnd.ms-fontobject";
            case "epub": return "application/epub+zip";
            case "gz": return "application/gzip";
            case "gif": return "image/gif";
            case "htm": return "text/html";
            case "html": return "text/html";
            case "ico": return "image/vnd.microsoft.icon";
            case "ics": return "text/calendar";
            case "jar": return "application/java-archive";
            case "jpeg": return "image/jpeg";
            case "jpg": return "image/jpeg";
            case "js": return "text/javascript";
            case "json": return "application/json";
            case "jsonld": return "application/ld+json";
            case "mid": return "audio/x-midi";
            case "midi": return "audio/x-midi";
            case "mjs": return "text/javascript";
            case "mp3": return "audio/mpeg";
            case "mp4": return "video/mp4";
            case "mpeg": return "video/mpeg";
            case "mpg": return "video/mpeg";
            case "mpkg": return "application/vnd.apple.installer+xml";
            case "odp": return "application/vnd.oasis.opendocument.presentation";
            case "ods": return "application/vnd.oasis.opendocument.spreadsheet";
            case "odt": return "application/vnd.oasis.opendocument.text";
            case "oga": return "audio/ogg";
            case "ogv": return "video/ogg";
            case "ogx": return "application/ogg";
            case "opus": return "audio/opus";
            case "otf": return "font/otf";
            case "png": return "image/png";
            case "pdf": return "application/pdf";
            case "php": return "application/x-httpd-php";
            case "ppt": return "application/vnd.ms-powerpoint";
            case "pptx": return "application/vnd.openxmlformats-officedocument.presentationml.presentation";
            case "rar": return "application/vnd.rar";
            case "rtf": return "application/rtf";
            case "sh": return "application/x-sh";
            case "svg": return "image/svg+xml";
            case "swf": return "application/x-shockwave-flash";
            case "tar": return "application/x-tar";
            case "tif": return "image/tiff";
            case "tiff": return "image/tiff";
            case "ts": return "video/mp2t";
            case "ttf": return "font/ttf";
            case "txt": return "text/plain";
            case "vsd": return "application/vnd.visio";
            case "wav": return "audio/wav";
            case "weba": return "audio/webm";
            case "webm": return "video/webm";
            case "webp": return "image/webp";
            case "woff": return "font/woff";
            case "woff2": return "font/woff2";
            case "xhtml": return "application/xhtml+xml";
            case "xls": return "application/vnd.ms-excel";
            case "xlsx": return "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet";
            case "xml": return "application/xml";
            case "xul": return "application/vnd.mozilla.xul+xml";
            case "zip": return "application/zip";
            case "3gp": return "audio/3gpp";
            case "3g2": return "audio/3gpp2";
            case "7z": return "application/x-7z-compressed";
            default: return dflt;
        }
    }

    /**
     * Obtain the file name extension.
     * For example "file.pdf" will return "pdf".
     * "" is returned if no extension is found.
     *
     * @param  fn    the file name
     * @return       the extension portion of the file name without the "."
     */
    public static String getExtension(String fn) {
        int idx;
        if (fn == null  ||  fn.isEmpty()  ||  (idx=fn.lastIndexOf(".")) == -1)
			return "";
        return fn.substring(idx+1);
	}

}
