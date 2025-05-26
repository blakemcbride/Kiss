package org.kissweb;

import java.io.*;
import java.util.Date;
import java.util.HashMap;

/**
 * Class to deal with ini files.  These are text-based property files broken into sections.  Each section may have any number of key/value pairs.  You can also dispense with the sections if you only have one.
 * <br><br>
 * The file can also have blank and comment lines.  Comment lines start with a semicolon, colon, hash, dash, or astrix.  Keys and values may be quoted with either single or double quotes.
 * <br><br>
 * This class is thread-safe.
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
     *
     * @param fname the filename to load from
     * @return the loaded IniFile object or null if file doesn't exist
     * @throws IOException if an I/O error occurs while reading the file
     */
    public static IniFile load(String fname) throws IOException {
        if (!(new java.io.File(fname)).exists()) {
            return null;
        }
        IniFile ini = new IniFile();
        ini.filename = fname;
        ini.parse(fname);
        return ini;
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
    public String getFilename() {
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
     *
     * @param fname the filename to save to
     * @throws IOException if an I/O error occurs while writing the file
     */
    public synchronized void save(String fname) throws IOException {
        try (BufferedWriter bw = new BufferedWriter(new FileWriter(fname))) {
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
    public void save() throws IOException {
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
