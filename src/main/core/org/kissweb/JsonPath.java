package org.kissweb;

import org.json.JSONArray;
import org.json.JSONObject;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Author: Blake McBride<br>
 * Date: 11/8/19
 * <br><br>
 * This class provides a variety of features on top of the functionality provided by org.json.
 * In addition to being able to parse a string into a json object, the json object can also be obtained
 * from a file.  Once the json object is obtained, embedded elements may be indexed similar to XPath thus
 * providing an easy method of destructuring a complex json object.  Lastly, rather than throw an exception
 * when a data item is missing, these methods return null.
 */
public class JsonPath {

    /**
     * This takes a JSON string or a file name (containing JSON) and
     * returns a JSON object.
     * <br><br>
     * If in a string, the string must start with "{" or space character
     *
     * @param str the json string or file name containing a json string
     * @return
     * @throws IOException
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

    /**
     * Traverse an object returning the sub-object referenced by <code>path</code>.
     *
     * @param delimiter the delimiter used in <code>path</code> to delimit the each path sub-part
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the json object being referenced by path or null if not found
     */
    public static JSONObject getObject(String delimiter, JSONObject jobj, String path) {
        if (jobj == null)
            return null;
        if (path == null  ||  path.isEmpty())
            return jobj;
        String [] pa = path.split(delimiter);
        try {
            for (String s : pa) {
                jobj = jobj.getJSONObject(s);
                if (jobj == null)
                    return null;
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
        return getObject("\\.", jobj, path);
    }

    /**
     * Traverse an object returning the String referenced by <code>path</code>.
     *
     * @param delimiter the delimiter used in <code>path</code> to delimit the each path sub-part
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the String being referenced by path or null if not found
     */
    public static String getString(String delimiter, JSONObject jobj, String path) {
        if (jobj == null || path == null  ||  path.isEmpty())
            return null;
        String [] pa = path.split(delimiter);
        try {
            int n = pa.length - 1;
            for (int i=0 ; i < n ; i++) {
                String s = pa[i];
                jobj = jobj.getJSONObject(s);
                if (jobj == null)
                    return null;
            }
            return jobj.getString(pa[n]);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Traverse an object returning the String referenced by <code>path</code>.
     * The delimiter used in <code>path</code> is a period.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the String being referenced by path or null if not found
     */
    public static String getString(JSONObject jobj, String path) {
        return getString("\\.", jobj, path);
    }

    /**
     * Traverse an object returning the Integer referenced by <code>path</code>.
     *
     * @param delimiter the delimiter used in <code>path</code> to delimit the each path sub-part
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Integer being referenced by path or null if not found
     */
    public static Integer getInteger(String delimiter, JSONObject jobj, String path) {
        if (jobj == null || path == null  ||  path.isEmpty())
            return null;
        String [] pa = path.split(delimiter);
        try {
            int n = pa.length - 1;
            for (int i=0 ; i < n ; i++) {
                String s = pa[i];
                jobj = jobj.getJSONObject(s);
                if (jobj == null)
                    return null;
            }
            return jobj.getInt(pa[n]);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Traverse an object returning the Integer referenced by <code>path</code>.
     * The delimiter used in <code>path</code> is a period.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Integer being referenced by path or null if not found
     */
    public static Integer getInteger(JSONObject jobj, String path) {
        return getInteger("\\.", jobj, path);
    }

    /**
     * Traverse an object returning the Double referenced by <code>path</code>.
     *
     * @param delimiter the delimiter used in <code>path</code> to delimit the each path sub-part
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Double being referenced by path or null if not found
     */
    public static Double getDouble(String delimiter, JSONObject jobj, String path) {
        if (jobj == null || path == null  ||  path.isEmpty())
            return null;
        String [] pa = path.split(delimiter);
        try {
            int n = pa.length - 1;
            for (int i=0 ; i < n ; i++) {
                String s = pa[i];
                jobj = jobj.getJSONObject(s);
                if (jobj == null)
                    return null;
            }
            return jobj.getDouble(pa[n]);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Traverse an object returning the Double referenced by <code>path</code>.
     * The delimiter used in <code>path</code> is a period.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Double being referenced by path or null if not found
     */
    public static Double getDouble(JSONObject jobj, String path) {
        return getDouble("\\.", jobj, path);
    }

    /**
     * Traverse an object returning the JSONArray referenced by <code>path</code>.
     *
     * @param delimiter the delimiter used in <code>path</code> to delimit the each path sub-part
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the JSONArray being referenced by path or null if not found
     */
    public static JSONArray getJSONArray(String delimiter, JSONObject jobj, String path) {
        if (jobj == null || path == null  ||  path.isEmpty())
            return null;
        String [] pa = path.split(delimiter);
        try {
            int n = pa.length - 1;
            for (int i=0 ; i < n ; i++) {
                String s = pa[i];
                jobj = jobj.getJSONObject(s);
                if (jobj == null)
                    return null;
            }
            return jobj.getJSONArray(pa[n]);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Traverse an object returning the JSONArray referenced by <code>path</code>.
     * The delimiter used in <code>path</code> is a period.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the JSONArray being referenced by path or null if not found
     */
    public static JSONArray getJSONArray(JSONObject jobj, String path) {
        return getJSONArray("\\.", jobj, path);
    }

    /**
     * Traverse an object returning the Boolean referenced by <code>path</code>.
     *
     * @param delimiter the delimiter used in <code>path</code> to delimit the each path sub-part
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Boolean being referenced by path or null if not found
     */
    public static Boolean getBoolean(String delimiter, JSONObject jobj, String path) {
        if (jobj == null || path == null  ||  path.isEmpty())
            return null;
        String [] pa = path.split(delimiter);
        try {
            int n = pa.length - 1;
            for (int i=0 ; i < n ; i++) {
                String s = pa[i];
                jobj = jobj.getJSONObject(s);
                if (jobj == null)
                    return null;
            }
            return jobj.getBoolean(pa[n]);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Traverse an object returning the Boolean referenced by <code>path</code>.
     * The delimiter used in <code>path</code> is a period.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Boolean being referenced by path or null if not found
     */
    public static Boolean getBoolean(JSONObject jobj, String path) {
        return getBoolean("\\.", jobj, path);
    }

    /**
     * Traverse an object returning the Long referenced by <code>path</code>.
     *
     * @param delimiter the delimiter used in <code>path</code> to delimit the each path sub-part
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Long being referenced by path or null if not found
     */
    public static Long getLong(String delimiter, JSONObject jobj, String path) {
        if (jobj == null || path == null  ||  path.isEmpty())
            return null;
        String [] pa = path.split(delimiter);
        try {
            int n = pa.length - 1;
            for (int i=0 ; i < n ; i++) {
                String s = pa[i];
                jobj = jobj.getJSONObject(s);
                if (jobj == null)
                    return null;
            }
            return jobj.getLong(pa[n]);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Traverse an object returning the Long referenced by <code>path</code>.
     * The delimiter used in <code>path</code> is a period.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Long being referenced by path or null if not found
     */
    public static Long getLong(JSONObject jobj, String path) {
        return getLong("\\.", jobj, path);
    }

    /**
     * Traverse an object returning the Character referenced by <code>path</code>.
     * <br><br>
     * If the field contains mor than one character, the first is returned.
     *
     * @param delimiter the delimiter used in <code>path</code> to delimit the each path sub-part
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Character being referenced by path or null if not found
     */
    public static Character getCharacter(String delimiter, JSONObject jobj, String path) {
        if (jobj == null || path == null  ||  path.isEmpty())
            return null;
        String [] pa = path.split(delimiter);
        try {
            int n = pa.length - 1;
            for (int i=0 ; i < n ; i++) {
                String s = pa[i];
                jobj = jobj.getJSONObject(s);
                if (jobj == null)
                    return null;
            }
            String str = jobj.getString(pa[n]);
            return str == null || str.isEmpty() ? null : str.charAt(0);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Traverse an object returning the Character referenced by <code>path</code>.
     * The delimiter used in <code>path</code> is a period.
     * <br><br>
     * If the field contains mor than one character, the first is returned.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Character being referenced by path or null if not found
     */
    public static Character getCharacter(JSONObject jobj, String path) {
        return getCharacter("\\.", jobj, path);
    }

    /**
     * Traverse an object returning the Integer date referenced by <code>path</code>.
     * <br><br>
     * The Integer date returned is of the form YYYYMMDD.  So, 6/8/2018 would be
     * returned as 20180608.  A null is returned if the data cannot be obtained.
     *
     * @param delimiter the delimiter used in <code>path</code> to delimit the each path sub-part
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Integer date being referenced by path or null if not found
     */
    public static Integer getDate(String delimiter, JSONObject jobj, String path) {
        if (jobj == null || path == null  ||  path.isEmpty())
            return null;
        String [] pa = path.split(delimiter);
        try {
            int n = pa.length - 1;
            for (int i=0 ; i < n ; i++) {
                String s = pa[i];
                jobj = jobj.getJSONObject(s);
                if (jobj == null)
                    return null;
            }
            String str = jobj.getString(pa[n]);
            if (str == null  ||  str.isEmpty())
                return null;
            int date = DateUtils.parse(str);
            return date == 0 ? null : date;
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Traverse an object returning the Integer date referenced by <code>path</code>.
     * The delimiter used in <code>path</code> is a period.
     * <br><br>
     * The Integer date returned is of the form YYYYMMDD.  So, 6/8/2018 would be
     * returned as 20180608.  A null is returned if the data cannot be obtained.
     *
     * @param jobj the initial json object
     * @param path a string indicating the path to the desired element
     * @return the Integer date being referenced by path or null if not found
     */
    public static Integer getDate(JSONObject jobj, String path) {
        return getDate("\\.", jobj, path);
    }

    public static void main(String [] argv) throws IOException {
        JSONObject jobj = toJson("input.json");
        JSONObject jobj2 = getObject(jobj, "context.System.application");
        String s1 = getString(jobj, "context.System.application.applicationId");
        jobj = null;
    }
}
