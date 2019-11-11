package org.kissweb;

import org.json.JSONObject;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import static org.kissweb.StringUtils.drop;

/**
 * Author: Blake McBride
 * Date: 11/9/19
 *
 * This class replaces occurrences of ${NAME} with values obtained from a supplied map.
 */
public class JsonTemplate {

    /**
     * Fill all occurrences of variables in the form ${VAR} with the values given in the map.
     *
     * If <code>jstr</code> is a file name, the json will be taken from the file.
     *
     * @param jstr the json string or file name
     * @param map a map mapping variable name to replacement text.
     * @return
     * @throws IOException
     * 
     * @see #fill(JSONObject, Map)
     */
    public static JSONObject fill(String jstr, Map<String,String> map) throws IOException {
        if (jstr == null  ||  jstr.isEmpty())
            return null;
        if (jstr.charAt(0) != '{')
            jstr = new String(Files.readAllBytes(Paths.get(jstr)));
        Matcher m = Pattern.compile("\\$\\{[^}]+}").matcher(jstr);
        while (m.find()) {
            String pat = m.group();
            String vname = drop(drop(pat, 2), -1);
            String rep = map.get(vname);
            if (rep != null)
                jstr = jstr.replace(pat, rep);
        }
        return new JSONObject(jstr);
    }

    /**
     * Fill all occurrences of variables in the form ${VAR} with the values given in the map.
     *
     * If <code>jstr</code> is a file name, the json will be taken from the file.
     *
     * @param jobj the json object
     * @param map a map mapping variable name to replacement text.
     * @return
     * @throws IOException
     *
     * @see #fill(String, Map)
     */
    public static JSONObject fill(JSONObject jobj, Map<String,String> map) throws IOException {
        return fill(jobj.toString(), map);
    }

    public static void main(String [] argv) throws IOException {
        Map<String,String> map = new HashMap<>();
        map.put("appId", "the actual id");
        JSONObject jobj = fill(JsonPath.toJson("input.json").toString(), map);
    }
}
