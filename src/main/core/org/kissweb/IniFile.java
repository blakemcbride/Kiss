package org.kissweb;

import org.kissweb.restServer.MainServlet;

import java.io.*;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;

/**
 * Class to deal with ini files.  These are text-based property files broken into sections.  Each section may have any number of key/value pairs.  You can also dispense with the sections if you only have one.
 * <br><br>
 * The file can also have blank and comment lines.  Comment lines start with a semicolon, colon, hash, dash, or astrix.  Keys and values may be quoted with either single or double quotes.
 * <br><br>
 * <h2>Thread safety (single JVM)</h2>
 * Every public method that reads or writes shared state is
 * <code>synchronized</code> on the {@link IniFile} instance, so any
 * single call is atomic with respect to any other concurrent call on
 * the same instance.  {@link #getSection(String)} returns a defensive
 * copy of the section's contents, so iterating over the returned map
 * cannot collide with a concurrent mutation through this instance.
 * <br><br>
 * <h3>Compound (multi-call) operations are not atomic by default</h3>
 * The per-call atomicity above does NOT extend across two calls.  A
 * sequence like ``read a value, decide based on it, then write it
 * back'' will race with concurrent writers from other threads unless
 * the caller serializes the sequence explicitly.  Wrap the sequence
 * in a <code>synchronized</code> block on the {@link IniFile}
 * instance --- this acquires the same monitor the per-method locks
 * use, so the entire block is atomic with respect to every other
 * synchronized method:
 * <pre>
 *     synchronized (ini) {
 *         if (ini.get("clients", clientId) == null)
 *             ini.put("clients", clientId, payload);
 *     }
 * </pre>
 * The same pattern is required around any ``mutate then {@link #save()}''
 * sequence where you need the on-disk file to reflect a particular
 * batch of mutations atomically.
 * <br><br>
 * <h3>Limits</h3>
 * <ul>
 *   <li>This class is safe for concurrent access from multiple threads
 *       within one JVM.</li>
 *   <li>It is NOT safe across multiple JVMs / processes, or across two
 *       {@link IniFile} instances pointing at the same file: there is
 *       no file-level lock, only the in-JVM monitor.  Two concurrent
 *       writers to the same path will silently last-writer-win.</li>
 * </ul>
 * <br><br>
 * Files look as follows:
 * <pre>
 *     [section]
 *     key = value
 *     key2 = value2
 * </pre>
 */
public class IniFile {

    private final HashMap<String, HashMap<String, String>> sections = new HashMap<>();
    private String filename;

    /**
     * Create a new ini file in memory.  If it is to be saved, the file name to be used can be specified later.
     */
    public IniFile() { }

    /**
     * Create a new ini file in memory that will be saved to the specified file.
     *
     * @param fname the filename where the ini file will be saved
     */
    public IniFile(String fname) {
        filename = fname;
    }

    /**
     * Load an ini file from a disk file.
     * <br><br>
     * Path resolution is identical to {@link #save(String)} (both delegate
     * to {@link #resolvePath(String)}):
     * <ul>
     *   <li>An <em>absolute</em> path is used verbatim.  Useful for files
     *       that must live outside the deployed web application (e.g.
     *       runtime state that should survive a WAR redeploy).</li>
     *   <li>A <em>relative</em> path is resolved against
     *       {@link MainServlet#getApplicationPath()}, which is the
     *       backend directory (the deployed {@code WEB-INF/backend/} or
     *       its dev-mode equivalent {@code src/main/backend/}).  When the
     *       application path is not set (null or empty), a relative path is
     *       resolved against the current working directory.</li>
     * </ul>
     *
     * @param fname the filename to load from --- absolute or relative
     * @return the loaded IniFile object or null if file doesn't exist
     * @throws IOException if an I/O error occurs while reading the file
     */
    public static IniFile load(String fname) throws IOException {
        fname = resolvePath(fname);
        if (!(new java.io.File(fname)).exists())
            return null;
        IniFile ini = new IniFile();
        ini.filename = fname;
        ini.parse(fname);
        return ini;
    }

    /**
     * Resolve a filename to the location {@link #load(String)} and
     * {@link #save(String)} actually read from and write to, so the two
     * are always consistent:
     * <ul>
     *   <li>an absolute path is returned verbatim;</li>
     *   <li>a relative path is prefixed with
     *       {@link MainServlet#getApplicationPath()}, or with nothing (so
     *       it resolves against the current working directory) when the
     *       application path is null or empty.</li>
     * </ul>
     *
     * @param fname the filename, absolute or relative
     * @return the resolved filename
     */
    private static String resolvePath(String fname) {
        if (new File(fname).isAbsolute())
            return fname;
        final String base = MainServlet.getApplicationPath();
        return (base == null ? "" : base) + fname;
    }

    /**
     * Retrieve the section of the ini file as a map.
     * <br><br>
     * Returns a <em>defensive copy</em> of the section's contents.
     * Mutating the returned map does not affect the {@link IniFile};
     * use {@link #put(String, String, String)} (and its overloads) to
     * change stored values.  Returning a copy also makes the result
     * safe to iterate over while other threads concurrently mutate
     * this instance.
     *
     * @param section the section to retrieve
     * @return a copy of the section's key/value pairs, or null if the
     *         section doesn't exist
     */
    public synchronized HashMap<String,String> getSection(String section) {
        final Map<String,String> s = sections.get(section);
        return s == null ? null : new HashMap<>(s);
    }

    /**
     * Get the names of every section in the ini file.
     * <br><br>
     * The unnamed default section (the area before any <code>[section]</code>
     * header) appears as <code>null</code> if it contains any keys.  The
     * returned set is a snapshot; mutating it does not affect the file.
     *
     * @return a set of section names
     */
    public synchronized Set<String> getSectionNames() {
        return new LinkedHashSet<>(sections.keySet());
    }

    /**
     * Remove an entire section from the ini file.
     * <br><br>
     * Has no effect if the section does not exist.
     *
     * @param section the section to remove
     */
    public synchronized void removeSection(String section) {
        sections.remove(section);
    }

    /**
     * Removes quotes from the start and end of a string.
     *
     * @param s  the string to remove quotes from
     * @return  the string with quotes removed, or null if the input string is null
     */
    private static String unquote(String s) {
        if (s == null)
            return null;
        s = s.trim();
        if (s.isEmpty())
            return null;
        if (s.length() > 1 && (s.startsWith("\"") && s.endsWith("\"") || s.startsWith("'") && s.endsWith("'")))
            return s.substring(1, s.length() - 1);
        return s;
    }

    /**
     * Parse an ini file.
     *
     * @param fname  the filename of the ini file to parse
     */
    private void parse(String fname) throws IOException {
        HashMap<String, String> section = new HashMap<>();
        String sectionName = null;
        try (BufferedReader br = new BufferedReader(new FileReader(fname))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (line.isEmpty() || line.startsWith("#") || line.startsWith(";") || line.startsWith("-") || line.startsWith("*") || line.startsWith(":"))
                    continue;  //  comment or blank
                if (line.startsWith("[")) {
                    sections.put(sectionName, section);
                    sectionName = unquote(line.substring(1, line.length() - 1));
                    section = sections.get(sectionName);
                    if (section == null)
                        section = new HashMap<>();
                    continue;
                }
                String[] parts = line.split("=", 2);
                if (parts.length != 2)
                    continue;
                String key = unquote(parts[0]);
                String value = unquote(parts[1]);
                //System.out.println(key + " = " + value);
                section.put(key, value);
            }
            sections.put(sectionName, section);
        }
    }

    /**
     * Returns the filename of the ini file.
     *
     * @return the filename of the ini file
     */
    public synchronized String getFilename() {
        return filename;
    }

    /**
     * Returns the value of a key from a specified section in the ini file.
     *
     * @param section the section to search for the key
     * @param key     the key to search for
     * @return the value of the key if found, null otherwise
     */
    public synchronized String get(String section, String key) {
        final HashMap<String,String> s = sections.get(section);
        if (s == null)
            return null;
        return s.get(key);
    }

    /**
     * Retrieves the integer value of a key from a specified section in the ini file.
     *
     * @param section the section to search for the key
     * @param key     the key to search for
     * @return the integer value of the key if found, null otherwise
     */
    public Integer getInt(String section, String key) {
        final String s = get(section, key);
        if (s == null)
            return null;
        return Integer.parseInt(s);
    }

    /**
     * Retrieves the first character of a key from a specified section in the ini file.
     *
     * @param section the section to search for the key
     * @param key     the key to search for
     * @return the first character of the key if found, null otherwise
     */
    public Character getChar(String section, String key) {
        final String s = get(section, key);
        if (s == null)
            return null;
        return s.charAt(0);
    }

    /**
     * Retrieves the boolean value of a key from a specified section in the ini file.
     *
     * @param section the section to search for the key
     * @param key     the key to search for
     * @return true if the value starts with 't', '1', or 'y', false otherwise
     */
    public boolean getBoolean(String section, String key) {
        final String s = get(section, key);
        if (s == null || s.isEmpty())
            return false;
        char c = Character.toLowerCase(s.charAt(0));
        return c == 't' ||  c == '1' || c == 'y';
    }

    /**
     * Retrieves the double value of a key from a specified section in the ini file.
     *
     * @param section the section to search for the key
     * @param key     the key to search for
     * @return the double value of the key if found, null otherwise
     */
    public Double getDouble(String section, String key) {
        final String s = get(section, key);
        if (s == null)
            return null;
        return Double.parseDouble(s);
    }

    /**
     * Retrieves the integer value of a date key from a specified section in the ini file.
     *
     * @param section the section to search for the key
     * @param key     the key to search for
     * @return the integer value of the key if found (YYYYMMDD), 0 otherwise
     */
    public int getDateInt(String section, String key) {
        final String s = get(section, key);
        if (s == null)
            return 0;
        return DateUtils.parse(s);
    }

    /**
     * Retrieves the integer value of a time key from a specified section in the ini file.
     *
     * @param section the section to search for the key
     * @param key     the key to search for
     * @return the integer value of the key if found (HHMM), 0 otherwise
     */
    public int getTimeInt(String section, String key) {
        final String s = get(section, key);
        if (s == null)
            return 0;
        return TimeUtils.parse(s);
    }

    /**
     * Puts a key-value pair into a specified section of the ini file.
     *
     * @param section the section to put the key-value pair into
     * @param key     the key to put into the section
     * @param value   the value to put into the section
     */
    public synchronized void put(String section, String key, String value) {
        HashMap<String, String> s = sections.get(section);
        if (s == null)
            s = new HashMap<>();
        s.put(key, value);
        sections.put(section, s);
    }

    /**
     * Puts a key-value pair into a specified section of the ini file.
     *
     * @param section the section to put the key-value pair into
     * @param key     the key to put into the section
     * @param value   the value to put into the section
     */
    public void put(String section, String key, int value) {
        put(section, key, Integer.toString(value));
    }

    /**
     * Puts a key-value pair into a specified section of the ini file.
     *
     * @param section the section to put the key-value pair into
     * @param key     the key to put into the section
     * @param value   the value to put into the section
     */
    public void put(String section, String key, double value) {
        put(section, key, Double.toString(value));
    }

    /**
     * Puts a key-value pair into a specified section of the ini file.
     *
     * @param section the section to put the key-value pair into
     * @param key     the key to put into the section
     * @param value   the value to put into the section
     */
    public void put(String section, String key, boolean value) {
        put(section, key, Boolean.toString(value));
    }

    /**
     * Puts a key-value pair into a specified section of the ini file.
     *
     * @param section the section to put the key-value pair into
     * @param key     the key to put into the section
     * @param value   the value to put into the section
     */
    public void put(String section, String key, char value) {
        put(section, key, Character.toString(value));
    }

    /**
     * Puts a key-value pair into a specified section of the ini file.
     *
     * @param section the section to put the key-value pair into
     * @param key     the key to put into the section
     * @param value   the value to put into the section
     */
    public void put(String section, String key, long value) {
        put(section, key, Long.toString(value));
    }

    /**
     * Puts a key-value pair into a specified section of the ini file.
     *
     * @param section the section to put the key-value pair into
     * @param key     the key to put into the section
     * @param value   the value to put into the section
     */
    public void put(String section, String key, Date value) {
        put(section, key, DateUtils.format4(DateUtils.toInt(value)));
    }

    /**
     * Save the in-memory ini file to the specified file.
     * <br><br>
     * Path resolution is identical to {@link #load(String)} (both delegate
     * to {@link #resolvePath(String)}): an absolute path is used verbatim;
     * a relative path is resolved against
     * {@link MainServlet#getApplicationPath()}, or against the current
     * working directory when the application path is not set.  This means a
     * file written with a given relative name is read back with the same
     * relative name.
     *
     * @param fname the filename to save to --- absolute or relative
     * @throws IOException if an I/O error occurs while writing the file
     */
    public synchronized void save(String fname) throws IOException {
        try (BufferedWriter bw = new BufferedWriter(new FileWriter(resolvePath(fname)))) {
            for (String section : sections.keySet()) {
                if (section == null && sections.get(section).isEmpty())
                    continue;
                bw.write("[" + (section == null ? "" : section) + "]\n\n");
                for (String key : sections.get(section).keySet()) {
                    String val = sections.get(section).get(key);
                    bw.write(key + "=" + (val == null ? "" : val) + "\n");
                }
                bw.write("\n");
            }
        }
    }

    /**
     * Save the in-memory ini file to file it was read from.
     *
     * @throws IOException if an I/O error occurs while writing the file
     */
    public synchronized void save() throws IOException {
        save(filename);
    }

    /**
     * Removes the specified key from the specified section of the ini file.
     *
     * @param section the section to remove the key from
     * @param key     the key to remove
     */
    public synchronized void removeValue(String section, String key) {
        HashMap<String, String> s = sections.get(section);
        if (s == null)
            return;
        s.put(key, null);
    }

    /**
     * Removes the specified key/value pair from the specified section of the ini file.
     *
     * @param section the section to remove the key from
     * @param key     the key to remove
     */
    public synchronized void removeKey(String section, String key) {
        HashMap<String, String> s = sections.get(section);
        if (s == null)
            return;
        s.remove(key);
    }

    /**
     * Retrieves the string value of a key from the ini file from the <code>null</code> section.
     *
     * @param  key     the key to search for
     * @return         the value of the key if found, null otherwise
     */
    public String get(String key) {
        return get(null, key);
    }

    /**
     * Retrieves the Integer value of a key from the ini file from the <code>null</code> section.
     *
     * @param  key     the key to search for
     * @return         the value of the key if found, null otherwise
     */
    public Integer getInt(String key) {
        return getInt(null, key);
    }

    /**
     * Retrieves the Character value of a key from the ini file from the <code>null</code> section.
     *
     * @param  key     the key to search for
     * @return         the value of the key if found, null otherwise
     */
    public Character getChar(String key) {
        return getChar(null, key);
    }

    /**
     * Retrieves the Boolean value of a key from the ini file from the <code>null</code> section.
     *
     * @param  key     the key to search for
     * @return         the value of the key if found, false otherwise
     */
    public boolean getBoolean(String key) {
        return getBoolean(null, key);
    }

    /**
     * Retrieves the Double value of a key from the ini file from the <code>null</code> section.
     *
     * @param  key     the key to search for
     * @return         the value of the key if found, null otherwise
     */
    public Double getDouble(String key) {
        return getDouble(null, key);
    }

    /**
     * Retrieves the integer value representing a date as YYYYMMDD of a key from the ini file from the <code>null</code> section.
     *
     * @param  key     the key to search for
     * @return         the value of the key if found, 0 otherwise
     */
    public int getDateInt(String key) {
        return getDateInt(null, key);
    }

    /**
     * Retrieves the integer value representing a date as HHMM of a key from the ini file from the <code>null</code> section.
     *
     * @param  key     the key to search for
     * @return         the value of the key if found, 0 otherwise
     */
    public int getTimeInt(String key) {
        return getTimeInt(null, key);
    }

    /**
     * Puts a key-value pair into the ini file in the <code>null</code> section.
     *
     * @param  key     the key to put into the section
     * @param  value   the value to put into the section
     */
    public void put(String key, String value) {
        put(null, key, value);
    }

    /**
     * Puts a key-value pair into the ini file in the <code>null</code> section.
     *
     * @param  key     the key to put into the section
     * @param  value   the value to put into the section
     */
    public void put(String key, int value) {
        put(null, key, value);
    }

    /**
     * Puts a key-value pair into the ini file in the <code>null</code> section.
     *
     * @param  key     the key to put into the section
     * @param  value   the value to put into the section
     */
    public void put(String key, double value) {
        put(null, key, value);
    }

    /**
     * Puts a key-value pair into the ini file in the <code>null</code> section.
     *
     * @param  key     the key to put into the section
     * @param  value   the value to put into the section
     */
    public void put(String key, boolean value) {
        put(null, key, value);
    }

    /**
     * Puts a key-value pair into the ini file in the <code>null</code> section.
     *
     * @param  key     the key to put into the section
     * @param  value   the value to put into the section
     */
    public void put(String key, char value) {
        put(null, key, value);
    }

    /**
     * Puts a key-value pair into the ini file in the <code>null</code> section.
     *
     * @param  key     the key to put into the section
     * @param  value   the value to put into the section
     */
    public void put(String key, long value) {
        put(null, key, value);
    }

    /**
     * Puts a key-value pair into the ini file in the <code>null</code> section.
     *
     * @param  key     the key to put into the section
     * @param  value   the value to put into the section
     */
    public void put(String key, Date value) {
        put(null, key, value);
    }

    /**
     * Removes the value associated with the given key from the ini file in the <code>null</code> section.
     *
     * @param  key     the key to remove the value for
     */
    public void removeValue(String key) {
        removeValue(null, key);
    }

    /**
     * Removes the key-value pair associated with the given key from the ini file in the <code>null</code> section.
     *
     * @param  key     the key to remove the value for
     */
    public void removeKey(String key) {
        removeKey(null, key);
    }

    /**
     * This test program can be run from the command-line as follows:<br><br>
     *      <code>java -cp work/Kiss.jar org.kissweb.IniFile</code>
     * @param args command line arguments
     * @throws IOException if an I/O error occurs
     */
    public static void main(String[] args) throws IOException {
        if (args.length != 1) {
            System.out.println("usage: java -cp work/Kiss.jar org.kissweb.IniFile <option number>");
            System.exit(1);
        }
        final String fname = "/tmp/ini.ini";
        IniFile ini;
        switch (args[0]) {
            case "1":  // create a new ini file
                ini = new IniFile(fname);
                ini.put("section1", "key1", "value1");
                ini.put("section1", "key2", "value2");
                ini.save();
                //System.out.println(ini.sections);
                break;
            case "2":  // read ini and display one of the values
                ini = IniFile.load(fname);
                System.out.println(ini.get("section1", "key1"));
                //System.out.println(ini.sections);
                break;
            case "3":  // read and re-write the file
                ini = IniFile.load(fname);
                ini.save();
                break;
            case "4":  // read, change value, and re-write the file
                ini = IniFile.load(fname);
                ini.put("section1", "key1", 56);
                ini.save();
                break;
            case "5":  // read, remove value, and re-write the file
                ini = IniFile.load(fname);
                ini.removeValue("section1", "key1");
                ini.save();
                break;
            case "6":  // read, remove key, and re-write the file
                ini = IniFile.load(fname);
                ini.removeKey("section1", "key1");
                ini.save();
                break;
            case "7":  // read, remove key, and re-write the file
                ini = IniFile.load(fname);
                ini.put("keyAA", "valAA");
                ini.save();
                break;
            default:
                System.out.println("invalid option - " + args[0]);
                System.exit(1);
        }
    }
}
