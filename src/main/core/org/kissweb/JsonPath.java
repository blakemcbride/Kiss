package org.kissweb;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * This class provides a variety of features on top of the functionality provided by org.json.
 * In addition to being able to parse a string into a json object, the json object can also be obtained
 * from a file.  Once the json object is obtained, embedded elements may be indexed similar to XPath thus
 * providing an easy method of destructuring a complex json object.  Lastly, rather than throw an exception
 * when a data item is missing, these methods return null.
 */
public class JsonPath {

    private static final String DefaultDelimiter = "\\.";

    /**
     * This takes a JSON string or a file name (containing JSON) and
     * returns a JSON object.
     * <br><br>
     * If in a string, the string must start with "{" or space character
     *
     * @param str the json string or file name containing a json string
     * @return the JSONObject parsed from the string or file
     * @throws IOException if file reading fails
     */
    public static JSONObject toJson(String str) throws IOException {
        if (str == null  ||  str.isEmpty())
            return new JSONObject();
        char c = str.charAt(0);
        if (c != '{'  &&  !Character.isWhitespace(c)) {
            // from a file
            str = new String(Files.readAllBytes(Paths.get(str)));
        }
        return new JSONObject(str);
    }

    private static JSONObject getObject(final String [] pa, JSONObject jobj, String path, boolean subtractOne) {
        if (jobj == null)
            return null;
        if (path == null  ||  path.isEmpty())
            return jobj;
        try {
            int n = pa.length;
            if (subtractOne)
                n--;
            for (int i=0 ; i < n ; i++) {
                String s = pa[i];
                if (s.charAt(s.length()-1) == ']') {
                    int b = s.indexOf("[");
                    int idx = Integer.parseInt(s.substring(b+1, s.length()-1));
                    s = s.substring(0, b);
                    jobj = jobj.getJSONArray(s).getJSONObject(idx);
                    if (jobj == null)
                        return null;
                } else {
                    jobj = jobj.getJSONObject(s);
                    if (jobj == null)
                        return null;
                }
            }
            return jobj;
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Traverse an object returning the sub-object referenced by <code>path</code>.
     * The delimiter used in <code>path</code> is a period.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the json object being referenced by path or null if not found
     */
    public static JSONObject getObject(JSONObject jobj, String path) {
        String [] pa = path.split(DefaultDelimiter);
        return getObject(pa, jobj, path, false);
    }

    /**
     * Traverse an object returning the String referenced by <code>path</code>.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the String being referenced by path or null if not found
     */
    public static String getString(JSONObject jobj, String path) {
        try {
            final String [] pa = path.split(DefaultDelimiter);
            final JSONObject obj = getObject(pa, jobj, path, true);
            return obj.getString(pa[pa.length-1]);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Traverse an object returning the Integer referenced by <code>path</code>.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Integer being referenced by path or null if not found
     */
    public static Integer getInteger(JSONObject jobj, String path) {
        try {
            final String [] pa = path.split(DefaultDelimiter);
            final JSONObject obj = getObject(pa, jobj, path, true);
            return obj.getInt(pa[pa.length-1]);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Traverse an object returning the Double referenced by <code>path</code>.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Double being referenced by path or null if not found
     */
    public static Double getDouble(JSONObject jobj, String path) {
        try {
            final String [] pa = path.split(DefaultDelimiter);
            final JSONObject obj = getObject(pa, jobj, path, true);
            return obj.getDouble(pa[pa.length-1]);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Traverse an object returning the JSONArray referenced by <code>path</code>.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the JSONArray being referenced by path or null if not found
     */
    public static JSONArray getJSONArray(JSONObject jobj, String path) {
        try {
            final String [] pa = path.split(DefaultDelimiter);
            final JSONObject obj = getObject(pa, jobj, path, true);
            return obj.getJSONArray(pa[pa.length-1]);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Traverse an object returning the Boolean referenced by <code>path</code>.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Boolean being referenced by path or null if not found
     */
    public static Boolean getBoolean(JSONObject jobj, String path) {
        try {
            final String [] pa = path.split(DefaultDelimiter);
            final JSONObject obj = getObject(pa, jobj, path, true);
            return obj.getBoolean(pa[pa.length-1]);
        } catch (Exception e) {
            return false;
        }
    }

    /**
     * Traverse an object returning the Long referenced by <code>path</code>.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Long being referenced by path or null if not found
     */
    public static Long getLong(JSONObject jobj, String path) {
        try {
            final String [] pa = path.split(DefaultDelimiter);
            final JSONObject obj = getObject(pa, jobj, path, true);
            return obj.getLong(pa[pa.length-1]);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Traverse an object returning the Character referenced by <code>path</code>.
     * <br><br>
     * If the field contains mor than one character, the first is returned.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Character being referenced by path or null if not found
     */
    public static Character getCharacter(JSONObject jobj, String path) {
        try {
            final String [] pa = path.split(DefaultDelimiter);
            final JSONObject obj = getObject(pa, jobj, path, true);
            String str = obj.getString(pa[pa.length-1]);
            return str == null || str.isEmpty() ? null : str.charAt(0);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Traverse an object returning the Integer date referenced by <code>path</code>.
     * <br><br>
     * The Integer date returned is of the form YYYYMMDD.  So, 6/8/2018 would be
     * returned as 20180608.  A null is returned if the data cannot be obtained.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Integer date being referenced by path or null if not found
     */
    public static Integer getDate(JSONObject jobj, String path) {
        try {
            final String [] pa = path.split(DefaultDelimiter);
            final JSONObject obj = getObject(pa, jobj, path, true);
            String str = obj.getString(pa[pa.length-1]);
            if (str == null  ||  str.isEmpty())
                return null;
            int date = DateUtils.parse(str);
            return date == 0 ? null : date;
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Test method for JsonPath functionality.
     *
     * @param argv command line arguments
     * @throws IOException if file operations fail
     */
    public static void main(String [] argv) throws IOException {
        JSONObject jobj = toJson("input.json");
        JSONObject jobj2 = getObject(jobj, "context.System.application");
        String s1 = getString(jobj, "context.System.application.applicationId");
        jobj = null;
    }
}
