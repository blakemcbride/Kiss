package org.kissweb;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;

/**
 * Author: Blake McBride
 * Date: 9/18/22
 */
public class KissFile {

    /**
     * Write String to file fileName.  File is created if it doesn't exist and truncated otherwise.
     *
     * @param fileName
     * @param str
     * @throws IOException
     */
    public static void writeString(String fileName, String str) throws IOException {
        Files.write(Paths.get(fileName), str.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.TRUNCATE_EXISTING);
    }

    /**
     * Write String to file fileName.  File is created if it doesn't exist and appended otherwise.
     *
     * @param fileName
     * @param str
     * @throws IOException
     */
    public static void appendString(String fileName, String str) throws IOException {
        Files.write(Paths.get(fileName), str.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
    }

    /**
     * Read the entire contents of a file into a String.
     *
     * @param fileName
     * @return
     * @throws IOException
     */
    public static String readFile(String fileName) throws IOException {
        return new String(Files.readAllBytes(Paths.get(fileName)), StandardCharsets.UTF_8);
    }
}
