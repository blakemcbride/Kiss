package org.kissweb.json;

import java.io.Closeable;
import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.*;
import java.util.Map.Entry;

/**
 * A JSONObject is an unordered collection of name/value pairs. Its external
 * form is a string wrapped in curly braces with colons between the names and
 * values, and commas between the values and names. The internal form is an
 * object having <code>get</code> and <code>opt</code> methods for accessing
 * the values by name, and <code>put</code> methods for adding or replacing
 * values by name. The values can be any of these types: <code>Boolean</code>,
 * <code>JSONArray</code>, <code>JSONObject</code>, <code>Number</code>,
 * <code>String</code>. A
 * JSONObject constructor can be used to convert an external form JSON text
 * into an internal form whose values can be retrieved with the
 * <code>get</code> and <code>opt</code> methods, or to convert values into a
 * JSON text using the <code>put</code> and <code>toString</code> methods. A
 * <code>get</code> method returns a value if one can be found, and throws an
 * exception if one cannot be found. An <code>opt</code> method returns a
 * default value instead of throwing an exception, and so is useful for
 * obtaining optional values.
 * <p>
 * The generic <code>get()</code> and <code>opt()</code> methods return an
 * object, which you can cast or query for type. There are also typed
 * <code>get</code> and <code>opt</code> methods that do type checking and type
 * coercion for you. The opt methods differ from the get methods in that they
 * do not throw. Instead, they return a specified value, such as null.
 * <p>
 * The <code>put</code> methods add or replace values in an object. For
 * example,
 *
 * <pre>
 * myString = new JSONObject()
 *         .put(&quot;JSON&quot;, &quot;Hello, World!&quot;).toString();
 * </pre>
 *
 * produces the string <code>{"JSON": "Hello, World"}</code>.
 * <p>
 * The texts produced by the <code>toString</code> methods strictly conform to
 * the JSON syntax rules. The constructors are more forgiving in the texts they
 * will accept:
 * <ul>
 * <li>An extra <code>,</code>&nbsp;<small>(comma)</small> may appear just
 * before the closing brace.</li>
 * <li>Strings may be quoted with <code>'</code>&nbsp;<small>(single
 * quote)</small>.</li>
 * <li>Strings do not need to be quoted at all if they do not begin with a
 * quote or single quote, and if they do not contain leading or trailing
 * spaces, and if they do not contain any of these characters:
 * <code>{ } [ ] / \ : , #</code> and if they do not look like numbers and
 * if they are not the reserved words <code>true</code>, <code>false</code>,
 * or <code>null</code>.</li>
 * </ul>
 *
 * @author JSON.org
 * @version 2016-08-15
 */
public class JSONObject {

    /**
     * The map where the JSONObject's properties are kept.
     */
    private final Map<String, Object> map;

    /**
     * Construct an empty JSONObject.
     */
    public JSONObject() {
        // HashMap is used on purpose to ensure that elements are unordered by 
        // the specification.
        // JSON tends to be a portable transfer format to allows the container 
        // implementations to rearrange their items for a faster element 
        // retrieval based on associative access.
        // Therefore, an implementation mustn't rely on the order of the item.
        this.map = new HashMap<String, Object>();
    }

    /**
     * Construct a JSONObject from a subset of another JSONObject. An array of
     * strings is used to identify the keys that should be copied. Missing keys
     * are ignored.
     *
     * @param jo A JSONObject.
     * @param names An array of strings.
     */
    public JSONObject(JSONObject jo, String[] names) {
        this(names.length);
        for (String name : names) {
            try {
                this.putOnce(name, jo.opt(name));
            } catch (Exception ignore) {
            }
        }
    }

    /**
     * Construct a JSONObject from a JSONTokener.
     *
     * @param x A JSONTokener object containing the source string.
     * @throws JSONException If there is a syntax error in the source string or a
     *             duplicated key.
     */
    public JSONObject(JSONTokener x) throws JSONException {
        this();
        char c;
        String key;

        if (x.nextClean() != '{') {
            throw x.syntaxError("A JSONObject text must begin with '{'");
        }
        for (;;) {
            c = x.nextClean();
            switch (c) {
            case 0:
                throw x.syntaxError("A JSONObject text must end with '}'");
            case '}':
                return;
            default:
                x.back();
                key = x.nextValue().toString();
            }

            // The key is followed by ':'.

            c = x.nextClean();
            if (c != ':') {
                throw x.syntaxError("Expected a ':' after a key");
            }
            
            // Use syntaxError(..) to include error location
            
            if (key != null) {
                // Check if key exists
                if (this.opt(key) != null) {
                    // key already exists
                    throw x.syntaxError("Duplicate key \"" + key + "\"");
                }
                // Only add value if non-null
                Object value = x.nextValue();
                if (value!=null) {
                    this.put(key, value);
                }
            }

            // Pairs are separated by ','.

            switch (x.nextClean()) {
            case ';':
            case ',':
                if (x.nextClean() == '}') {
                    return;
                }
                x.back();
                break;
            case '}':
                return;
            default:
                throw x.syntaxError("Expected a ',' or '}'");
            }
        }
    }

    /**
     * Construct a JSONObject from a Map.
     *
     * @param m A map object that can be used to initialize the contents of
     *            the JSONObject.
     */
    public JSONObject(Map<?, ?> m) {
        if (m == null) {
            this.map = new HashMap<String, Object>();
        } else {
            this.map = new HashMap<String, Object>(m.size());
        	for (final Entry<?, ?> e : m.entrySet()) {
                final Object value = e.getValue();
                if (value != null) {
                    this.map.put(String.valueOf(e.getKey()), wrap(value));
                }
            }
        }
    }

    /**
     * Construct a JSONObject from an Object using bean getters. It reflects on
     * all of the public methods of the object. For each of the methods with no
     * parameters and a name starting with <code>"get"</code> or
     * <code>"is"</code> followed by an uppercase letter, the method is invoked,
     * and a key and the value returned from the getter method are put into the
     * new JSONObject.
     * <p>
     * The key is formed by removing the <code>"get"</code> or <code>"is"</code>
     * prefix. If the second remaining character is not upper case, then the
     * first character is converted to lower case.
     * <p>
     * For example, if an object has a method named <code>"getName"</code>, and
     * if the result of calling <code>object.getName()</code> is
     * <code>"Larry Fine"</code>, then the JSONObject will contain
     * <code>"name": "Larry Fine"</code>.
     * <p>
     * Methods that return <code>void</code> as well as <code>static</code>
     * methods are ignored.
     * 
     * @param bean An object that has getter methods that should be used to make
     *            a JSONObject.
     */
    public JSONObject(Object bean) {
        this();
        this.populateMap(bean);
    }

    /**
     * Construct a JSONObject from an Object, using reflection to find the
     * public members. The resulting JSONObject's keys will be the strings from
     * the names array, and the values will be the field values associated with
     * those keys in the object. If a key is not found or not visible, then it
     * will not be copied into the new JSONObject.
     *
     * @param object An object that has fields that should be used to make a
     *            JSONObject.
     * @param names An array of strings, the names of the fields to be obtained
     *            from the object.
     */
    public JSONObject(Object object, String[] names) {
        this(names.length);
        Class<?> c = object.getClass();
        for (String name : names) {
            try {
                this.putOpt(name, c.getField(name).get(object));
            } catch (Exception ignore) {
            }
        }
    }

    /**
     * Construct a JSONObject from a source JSON text string. This is the most
     * commonly used JSONObject constructor.
     *
     * @param source A string beginning with <code>{</code>&nbsp;<small>(left
     *            brace)</small> and ending with <code>}</code>
     *            &nbsp;<small>(right brace)</small>.
     * @exception JSONException If there is a syntax error in the source string or a
     *                duplicated key.
     */
    public JSONObject(String source) throws JSONException {
        this(new JSONTokener(source));
    }

    /**
     * Construct a JSONObject from a ResourceBundle.
     *
     * @param baseName The ResourceBundle base name.
     * @param locale The Locale to load the ResourceBundle for.
     * @throws JSONException If any JSONExceptions are detected.
     */
    public JSONObject(String baseName, Locale locale) throws JSONException {
        this();
        ResourceBundle bundle = ResourceBundle.getBundle(baseName, locale,
                Thread.currentThread().getContextClassLoader());

// Iterate through the keys in the bundle.

        Enumeration<String> keys = bundle.getKeys();
        while (keys.hasMoreElements()) {
            String key = keys.nextElement();
            if (key != null) {

// Go through the path, ensuring that there is a nested JSONObject for each
// segment except the last. Add the value using the last segment's name into
// the deepest nested JSONObject.

                String[] path = key.split("\\.");
                int last = path.length - 1;
                JSONObject target = this;
                for (int i = 0; i < last; i += 1) {
                    String segment = path[i];
                    JSONObject nextTarget = target.getJSONObject(segment);
                    if (nextTarget == null) {
                        nextTarget = new JSONObject();
                        target.put(segment, nextTarget);
                    }
                    target = nextTarget;
                }
                target.put(path[last], bundle.getString(key));
            }
        }
    }
    
    /**
     * Constructor to specify an initial capacity of the internal map. Useful for library 
     * internal calls where we know, or at least can best guess, how big this JSONObject
     * will be.
     * 
     * @param initialCapacity initial capacity of the internal map.
     */
    protected JSONObject(int initialCapacity){
        this.map = new HashMap<String, Object>(initialCapacity);
    }

    /**
     * Accumulate values under a key. It is similar to the put method except
     * that if there is already an object stored under the key then a JSONArray
     * is stored under the key to hold all of the accumulated values. If there
     * is already a JSONArray, then the new value is appended to it. In
     * contrast, the put method replaces the previous value.
     *
     * If only one value is accumulated that is not a JSONArray, then the result
     * will be the same as using put. But if multiple values are accumulated,
     * then the result will be like append.
     *
     * @param key A key string.
     * @param value An object to be accumulated under the key.
     * @return this.
     * @throws JSONException If the value is an invalid number or if the key is null.
     */
    public JSONObject accumulate(String key, Object value) throws JSONException {
        testValidity(value);
        Object object = this.opt(key);
        if (object == null) {
            this.put(key,
                    value instanceof JSONArray ? new JSONArray().put(value)
                            : value);
        } else if (object instanceof JSONArray) {
            ((JSONArray) object).put(value);
        } else {
            this.put(key, new JSONArray().put(object).put(value));
        }
        return this;
    }

    /**
     * Append values to the array under a key. If the key does not exist in the
     * JSONObject, then the key is put in the JSONObject with its value being a
     * JSONArray containing the value parameter. If the key was already
     * associated with a JSONArray, then the value parameter is appended to it.
     *
     * @param key A key string.
     * @param value An object to be accumulated under the key.
     * @return this.
     * @throws JSONException If the key is null or if the current value associated with
     *             the key is not a JSONArray.
     */
    public JSONObject append(String key, Object value) throws JSONException {
        testValidity(value);
        Object object = this.opt(key);
        if (object == null) {
            this.put(key, new JSONArray().put(value));
        } else if (object instanceof JSONArray) {
            this.put(key, ((JSONArray) object).put(value));
        } else {
            throw new JSONException("JSONObject[" + key
                    + "] is not a JSONArray.");
        }
        return this;
    }

    /**
     * Produce a string from a double. The string "null" will be returned if the
     * number is not finite.
     *
     * @param d A double.
     * @return A String.
     */
    public static String doubleToString(double d) {
        if (Double.isInfinite(d) || Double.isNaN(d)) {
            return "null";
        }

// Shave off trailing zeros and decimal point, if possible.

        String string = Double.toString(d);
        if (string.indexOf('.') > 0 && string.indexOf('e') < 0
                && string.indexOf('E') < 0) {
            while (string.endsWith("0")) {
                string = string.substring(0, string.length() - 1);
            }
            if (string.endsWith(".")) {
                string = string.substring(0, string.length() - 1);
            }
        }
        return string;
    }

    /**
     * Get the value object associated with a key.
     *
     * @param key A key string.
     * @return The object associated with the key or null if not found.
     * @throws JSONException if the key is null.
     */
    public Object get(String key) throws JSONException {
        if (key == null) {
            throw new JSONException("Null key.");
        }
        return this.opt(key);
    }

    /**
     * Get the enum value associated with a key.
     * 
     * @param <E> The type of enum to retrieve.
     * @param clazz The type of enum to retrieve.
     * @param key A key string.
     * @return The enum value associated with the key or null if not found
     * @throws JSONException if the value cannot be converted to an enum.
     */
    public <E extends Enum<E>> E getEnum(Class<E> clazz, String key) throws JSONException {
        E val = optEnum(clazz, key);
        return val;
    }

    /**
     * Get the Boolean value associated with a key, or return dflt
     * if key isn't present.
     *
     * @param key A key string.
     * @param dflt Default value to return if key is not present.
     * @return The truth value or null if not found.
     * @throws JSONException if the value is not a Boolean or the String "true" or "false".
     */
    public Boolean getBoolean(String key, Boolean dflt) throws JSONException {
        Object object = this.get(key);
        if (object == null)
            return dflt;
        if (object.equals(Boolean.FALSE)
                || (object instanceof String && ((String) object)
                        .equalsIgnoreCase("false"))) {
            return false;
        } else if (object.equals(Boolean.TRUE)
                || (object instanceof String && ((String) object)
                        .equalsIgnoreCase("true"))) {
            return true;
        } else if (object instanceof String) {
            String so = (String) object;
            if (so.equalsIgnoreCase("null") || so.isEmpty())
                return null;
            if (so.equalsIgnoreCase("true"))
                return true;
            if (so.equalsIgnoreCase("false"))
                return false;
        }
        throw new JSONException("JSONObject[" + quote(key) + "] is not a Boolean.");
    }

    /**
     * Get the Boolean value associated with a key, or return null
     * if key isn't present.
     *
     * @param key A key string.
     * @return The truth value or null if not found.
     * @throws JSONException if the value is not a Boolean or the String "true" or "false".
     */
    public Boolean getBoolean(String key) throws JSONException {
        return getBoolean(key, null);
    }

    /**
     * Get the BigInteger value associated with a key.
     *
     * @param key A key string.
     * @return The numeric value or null if not found.
     * @throws JSONException if the value cannot be converted to BigInteger.
     */
    public BigInteger getBigInteger(String key) throws JSONException {
        Object object = this.get(key);
        if (object == null)
            return null;
        try {
            return new BigInteger(object.toString());
        } catch (Exception e) {
            throw new JSONException("JSONObject[" + quote(key) + "] could not be converted to BigInteger.", e);
        }
    }

    /**
     * Get the BigDecimal value associated with a key.
     *
     * @param key A key string.
     * @return The numeric value or null if not found
     * @throws JSONException if the value cannot be converted to BigDecimal.
     */
    public BigDecimal getBigDecimal(String key) throws JSONException {
        Object object = this.get(key);
        if (object == null)
            return null;
        if (object instanceof BigDecimal) {
            return (BigDecimal)object;
        }
        try {
            return new BigDecimal(object.toString());
        } catch (Exception e) {
            throw new JSONException("JSONObject[" + quote(key)
                    + "] could not be converted to BigDecimal.", e);
        }
    }

    /**
     * Get the double value associated with a key.
     *
     * @param key A key string.
     * @param dflt Default value to return if key is not present.
     * @return The numeric value or null if not found.
     * @throws JSONException if the value is not a Number object and cannot be converted to a number.
     */
    public Double getDouble(String key, Double dflt) throws JSONException {
        Object object = this.get(key);
        if (object == null)
            return dflt;
        try {
            if (object instanceof Number)
                return ((Number) object).doubleValue();
            if (object instanceof String) {
                String so = (String) object;
                if ("null".equals(so)  ||  so.isEmpty())
                    return null;
                return Double.parseDouble(so);
            }
            throw new Exception();
        } catch (Exception e) {
            throw new JSONException("JSONObject[" + quote(key) + "] is not a number.", e);
        }
    }

    /**
     * Get the Double value associated with a key, or return null if not found.
     *
     * @param key A key string.
     * @return The numeric value or null if not found
     * @throws JSONException if the value is not a Number object and cannot be converted to a number.
     */
    public Double getDouble(String key) throws JSONException {
        return getDouble(key, null);
    }

    /**
     * Get the Float value associated with a key.
     *
     * @param key A key string.
     * @param dflt The default value.
     * @return The numeric value or dflt if not found.
     * @throws JSONException if the value is not a Number object and cannot be converted to a number.
     */
    public Float getFloat(String key, Float dflt) throws JSONException {
        Object object = this.get(key);
        if (object == null)
            return dflt;
        try {
            if (object instanceof Number)
                return ((Number) object).floatValue();
            if (object instanceof String) {
                String so = (String) object;
                if ("null".equals(so)  ||  so.isEmpty())
                    return null;
                return Float.parseFloat(so);
            }
            throw new Exception();
        } catch (Exception e) {
            throw new JSONException("JSONObject[" + quote(key) + "] is not a number.", e);
        }
    }

    /**
     * Get the Float value associated with a key or return null if not found
     *
     * @param key A key string.
     * @return the Float value or null if not found
     * @throws JSONException if the value is not a Number object and cannot be converted to a number.
     */
    public Float getFloat(String key) throws JSONException {
        return getFloat(key, null);
    }

    /**
     * Get the Number value associated with a key.
     *
     * @param key A key string.
     * @return The numeric value or null if not found
     * @throws JSONException if the value is not a Number object and cannot be converted to a number.
     */
    public Number getNumber(String key) throws JSONException {
        Object object = this.get(key);
        if (object == null)
            return null;
        try {
            if (object instanceof Number) {
                return (Number)object;
            }
            return stringToNumber(object.toString());
        } catch (Exception e) {
            throw new JSONException("JSONObject[" + quote(key)
                    + "] is not a number.", e);
        }
    }

    /**
     * Get the int value associated with a key.
     *
     * @param key A key string.
     * @param dflt The default value
     * @return The integer value or dflt if not found
     * @throws JSONException if the value cannot be converted to an integer.
     */
    public Integer getInt(String key, Integer dflt) throws JSONException {
        Object object = this.get(key);
        if (object == null)
            return dflt;
        try {
            if (object instanceof Number)
                return ((Number) object).intValue();
            if (object instanceof String) {
                String so = (String) object;
                if ("null".equals(so)  ||  so.isEmpty())
                    return null;
                return Integer.parseInt(so);
            }
            throw new Exception();
        } catch (Exception e) {
            throw new JSONException("JSONObject[" + quote(key) + "] is not an int.", e);
        }
    }

    /**
     * Get the Integer value associated with a key.  If the key
     * isn't present, return null.
     *
     * @param key A key string.
     * @return the Integer or null if not found
     * @throws JSONException if the value cannot be converted to an integer.
     */
    public Integer getInt(String key) throws JSONException {
        return getInt(key, null);
    }

    /**
     * Get the JSONArray value associated with a key.
     * If rtnArray is true and the key is not found, an empty JSONArray is returned.
     * If rtnArray is false and the key is not found, null is returned.
     *
     * @param key A key string.
     * @param rtnArray If true, return empty JSONArray when key not found; if false, return null.
     * @return A JSONArray which is the value or null if not found
     * @throws JSONException if the key is not a JSONArray.
     */
    public JSONArray getJSONArray(String key, boolean rtnArray) throws JSONException {
        Object object = this.get(key);
        if (object == null)
            return rtnArray ? new JSONArray() : null;
        if (object instanceof JSONArray) {
            return (JSONArray) object;
        } else if (object instanceof String) {
            String s = (String) object;
            s = s.trim();
            if (s.length() > 1  &&  s.charAt(0) == '[')
                return new JSONArray((String) object);
        }
        throw new JSONException("JSONObject[" + quote(key)
                + "] is not a JSONArray.");
    }

    /**
     *  Get the JSONArray value associated with a key.  If key not found, return null.
     *
     * @param key A key string.
     * @return A JSONArray which is the value or null if not found
     * @throws JSONException if the key is not a JSONArray.
     */
    public JSONArray getJSONArray(String key) throws JSONException {
        return getJSONArray(key, false);
    }

    /**
     * Get the JSONObject value associated with a key.
     * If rtnObj is true and the key is not found, an empty JSONObject is returned.
     *
     * @param key A key string.
     * @param rtnObj If true, return an empty JSONObject when key is not found; if false, return null
     * @return A JSONObject which is the value or null if not found
     * @throws JSONException if the value is not a JSONObject.
     */
    public JSONObject getJSONObject(String key, boolean rtnObj) throws JSONException {
        Object object = this.get(key);
        if (object == null)
            return rtnObj ? new JSONObject() : null;
        if (object instanceof JSONObject) {
            return (JSONObject) object;
        } else if (object instanceof String) {
            String s = (String) object;
            s = s.trim();
            if (s.length() > 1 && s.charAt(0) == '{')
                return new JSONObject((String) object);
        }
        throw new JSONException("JSONObject[" + quote(key)
                + "] is not a JSONObject.");
    }

    /**
     * Get the JSONObject value associated with a key, or return null if not found.
     *
     * @param key A key string.
     * @return A JSONObject which is the value or null if not found
     * @throws JSONException if the value is not a JSONObject.
     */
    public JSONObject getJSONObject(String key) throws JSONException {
        return getJSONObject(key, false);
    }

    /**
     * Get the Long value associated with a key.
     * If key not present, return dflt.
     *
     * @param key A key string.
     * @param dflt The default value.
     * @return The long value or null if not found
     * @throws JSONException if the value cannot be converted to a long.
     */
    public Long getLong(String key, Long dflt) throws JSONException {
        Object object = this.get(key);
        if (object == null)
            return dflt;
        try {
            if (object instanceof Number)
                return ((Number) object).longValue();
            if (object instanceof String) {
                String so = (String) object;
                if ("null".equals(so)  ||  so.isEmpty())
                    return null;
                return Long.parseLong(so);
            }
            throw new Exception();
        } catch (Exception e) {
            throw new JSONException("JSONObject[" + quote(key) + "] is not a long.", e);
        }
    }

    /**
     * Get the Long value associated with a key.
     * If key not present, return null.
     *
     * @param key A key string.
     * @return The long value or null if not found
     * @throws JSONException if the value cannot be converted to a long.
     */
    public Long getLong(String key) throws JSONException {
        return getLong(key, null);
    }

    /**
     * Get the Date value associated with a key.
     * If key not present, return null.
     *
     * @param key A key string.
     * @return The Date value or null if not found
     * @throws JSONException if the value cannot be converted to a long.
     */
    public java.util.Date getDate(String key) throws JSONException {
        Long r = getLong(key, null);
        return r == null ? null : new java.util.Date(r);
    }

    /**
     * Get an array of field names from a JSONObject.
     *
     * @param jo A JSONObject.
     * @return An array of field names, or null if there are no names.
     */
    public static String[] getNames(JSONObject jo) {
        int length = jo.length();
        if (length == 0) {
            return null;
        }
        return jo.keySet().toArray(new String[length]);
    }

    /**
     * Get an array of field names from an Object.
     *
     * @param object An object that has fields.
     * @return An array of field names, or null if there are no names.
     */
    public static String[] getNames(Object object) {
        if (object == null) {
            return null;
        }
        Class<?> klass = object.getClass();
        Field[] fields = klass.getFields();
        int length = fields.length;
        if (length == 0) {
            return null;
        }
        String[] names = new String[length];
        for (int i = 0; i < length; i += 1) {
            names[i] = fields[i].getName();
        }
        return names;
    }

    /**
     * Get the string associated with a key.
     * If value is an integer, convert to string.
     * If key not present, return dflt.
     *
     * @param key A key string.
     * @param dflt The default return value if key not present
     * @return A string which is the value or dflt if not found
     * @throws JSONException if there is no string value for the key.
     */
    public String getString(String key, String dflt) throws JSONException {
        Object object = this.get(key);
        if (object == null)
            return dflt;
        if (object instanceof String)
            return (String) object;
        else if (object instanceof Number)
            return ((Number) object).intValue() + "";  // for backward compatability with old code
        throw new JSONException("JSONObject[" + quote(key) + "] not a string.");
    }

    /**
     * Get the string associated with a key.
     * If key not present, return null.
     *
     * @param key A key string.
     * @return A string which is the value or null if not found
     * @throws JSONException if there is no string value for the key.
     */
    public String getString(String key) throws JSONException {
        return getString(key, null);
    }

    /**
     * Get the character associated with a key.
     * If the item is a string, the first character is returned.
     * If key not present, return null.
     *
     * @param key A key string.
     * @return A Character which is the value or null if not found
     * @throws JSONException if there is no character value for the key.
     */
    public Character getCharacter(String key) throws JSONException {
        String s = getString(key);
        if (s != null && !s.isEmpty())
            return s.charAt(0);
        return null;
    }

    /**
     * Get the character associated with a key.
     * If the item is a string, the first character is returned.
     * If key not present, return dflt.
     *
     * @param key A key string.
     * @param dflt Default value
     * @return A Character which is the value or null if not found
     * @throws JSONException if there is no character value for the key.
     */
    public Character getCharacter(String key, char dflt) throws JSONException {
        String s = getString(key);
        if (s != null && !s.isEmpty())
            return s.charAt(0);
        return dflt;
    }

    /**
     * Determine if the JSONObject contains a specific key.
     *
     * @param key A key string.
     * @return true if the key exists in the JSONObject.
     */
    public boolean has(String key) {
        return this.map.containsKey(key);
    }

    /**
     * Increment a property of a JSONObject. If there is no such property,
     * create one with a value of 1. If there is such a property, and if it is
     * an Integer, Long, Double, or Float, then add one to it.
     *
     * @param key A key string.
     * @return this.
     * @throws JSONException If there is already a property with this name that is not an
     *             Integer, Long, Double, or Float.
     */
    public JSONObject increment(String key) throws JSONException {
        Object value = this.opt(key);
        if (value == null) {
            this.put(key, 1);
        } else if (value instanceof BigInteger) {
            this.put(key, ((BigInteger)value).add(BigInteger.ONE));
        } else if (value instanceof BigDecimal) {
            this.put(key, ((BigDecimal)value).add(BigDecimal.ONE));
        } else if (value instanceof Integer) {
            this.put(key, (Integer) value + 1);
        } else if (value instanceof Long) {
            this.put(key, (Long) value + 1L);
        } else if (value instanceof Double) {
            this.put(key, (Double) value + 1.0d);
        } else if (value instanceof Float) {
            this.put(key, (Float) value + 1.0f);
        } else {
            throw new JSONException("Unable to increment [" + quote(key) + "].");
        }
        return this;
    }

    /**
     * Determine if the value associated with the key is null or if there is no
     * value.
     *
     * @param key A key string.
     * @return true if there is no value associated with the key
     */
    public boolean isNull(String key) {
        return null == this.opt(key);
    }

    /**
     * Get an enumeration of the keys of the JSONObject. Modifying this key Set will also
     * modify the JSONObject. Use with caution.
     *
     * @see Set#iterator()
     * 
     * @return An iterator of the keys.
     */
    public Iterator<String> keys() {
        return this.keySet().iterator();
    }

    /**
     * Get a set of keys of the JSONObject. Modifying this key Set will also modify the
     * JSONObject. Use with caution.
     *
     * @see Map#keySet()
     *
     * @return A keySet.
     */
    public Set<String> keySet() {
        return this.map.keySet();
    }

    /**
     * Get a set of entries of the JSONObject. These are raw values and may not
     * match what is returned by the JSONObject get* and opt* functions. Modifying 
     * the returned EntrySet or the Entry objects contained therein will modify the
     * backing JSONObject. This does not return a clone or a read-only view.
     * 
     * Use with caution.
     *
     * @see Map#entrySet()
     *
     * @return An Entry Set
     */
    protected Set<Entry<String, Object>> entrySet() {
        return this.map.entrySet();
    }

    /**
     * Get the number of keys stored in the JSONObject.
     *
     * @return The number of keys in the JSONObject.
     */
    public int length() {
        return this.map.size();
    }

    /**
     * Produce a JSONArray containing the names of the elements of this
     * JSONObject.
     *
     * @return A JSONArray containing the key strings, or null if the JSONObject
     *         is empty.
     */
    public JSONArray names() {
    	if(this.map.isEmpty()) {
    		return null;
    	}
        return new JSONArray(this.map.keySet());
    }

    /**
     * Produce a string from a Number.
     *
     * @param number A Number
     * @return A String.
     * @throws JSONException If n is a non-finite number.
     */
    public static String numberToString(Number number) throws JSONException {
        if (number == null) {
            throw new JSONException("Null pointer");
        }
        testValidity(number);

        // Shave off trailing zeros and decimal point, if possible.

        String string = number.toString();
        if (string.indexOf('.') > 0 && string.indexOf('e') < 0
                && string.indexOf('E') < 0) {
            while (string.endsWith("0")) {
                string = string.substring(0, string.length() - 1);
            }
            if (string.endsWith(".")) {
                string = string.substring(0, string.length() - 1);
            }
        }
        return string;
    }

    /**
     * Get an optional value associated with a key.
     *
     * @param key A key string.
     * @return An object which is the value, or null if there is no value.
     */
    public Object opt(String key) {
        return key == null ? null : this.map.get(key);
    }

    /**
     * Get the enum value associated with a key.
     * 
     * @param <E> The type of enum to retrieve.
     * @param clazz
     *            The type of enum to retrieve.
     * @param key
     *            A key string.
     * @return The enum value associated with the key or null if not found
     */
    public <E extends Enum<E>> E optEnum(Class<E> clazz, String key) {
        return this.optEnum(clazz, key, null);
    }

    /**
     * Get the enum value associated with a key.
     * 
     * @param <E> The type of enum to retrieve.
     * @param clazz
     *            The type of enum to retrieve.
     * @param key
     *            A key string.
     * @param defaultValue
     *            The default in case the value is not found
     * @return The enum value associated with the key or defaultValue
     *            if the value cannot be assigned to <code>clazz</code>
     */
    public <E extends Enum<E>> E optEnum(Class<E> clazz, String key, E defaultValue) {
        try {
            Object val = this.opt(key);
            if (val == null) {
                return defaultValue;
            }
            if (clazz.isAssignableFrom(val.getClass())) {
                // we just checked it!
                @SuppressWarnings("unchecked")
                E myE = (E) val;
                return myE;
            }
            return Enum.valueOf(clazz, val.toString());
        } catch (IllegalArgumentException | NullPointerException e) {
            return defaultValue;
        }
    }

    /**
     * Populates the internal map of the JSONObject with the bean properties.
     * The bean can not be recursive.
     *
     * @see JSONObject#JSONObject(Object)
     *
     * @param bean
     *            the bean
     */
    private void populateMap(Object bean) {
        Class<?> klass = bean.getClass();

// If klass is a System class then set includeSuperClass to false.

        boolean includeSuperClass = klass.getClassLoader() != null;

        Method[] methods = includeSuperClass ? klass.getMethods() : klass
                .getDeclaredMethods();
        for (final Method method : methods) {
            final int modifiers = method.getModifiers();
            if (Modifier.isPublic(modifiers)
                    && !Modifier.isStatic(modifiers)
                    && method.getParameterTypes().length == 0
                    && !method.isBridge()
                    && method.getReturnType() != Void.TYPE ) {
                final String name = method.getName();
                String key;
                if (name.startsWith("get")) {
                    if ("getClass".equals(name) || "getDeclaringClass".equals(name)) {
                        continue;
                    }
                    key = name.substring(3);
                } else if (name.startsWith("is")) {
                    key = name.substring(2);
                } else {
                    continue;
                }
                if (!key.isEmpty()
                        && Character.isUpperCase(key.charAt(0))) {
                    if (key.length() == 1) {
                        key = key.toLowerCase(Locale.ROOT);
                    } else if (!Character.isUpperCase(key.charAt(1))) {
                        key = key.substring(0, 1).toLowerCase(Locale.ROOT)
                                + key.substring(1);
                    }

                    try {
                        final Object result = method.invoke(bean);
                        if (result != null) {
                            this.map.put(key, wrap(result));
                            // we don't use the result anywhere outside of wrap
                            // if it's a resource we should be sure to close it after calling toString
                            if(result instanceof Closeable) {
                                try {
                                    ((Closeable)result).close();
                                } catch (IOException ignore) {
                                }
                            }
                        }
                    } catch (IllegalAccessException | IllegalArgumentException | InvocationTargetException ignore) {
                    }
                }
            }
        }
    }

    /**
     * Put a key/boolean pair in the JSONObject.
     *
     * @param key A key string.
     * @param value A boolean which is the value.
     * @return this.
     * @throws JSONException If the key is null.
     */
    public JSONObject put(String key, boolean value) throws JSONException {
        this.put(key, value ? Boolean.TRUE : Boolean.FALSE);
        return this;
    }

    /**
     * Put a key/value pair in the JSONObject, where the value will be a
     * JSONArray which is produced from a Collection.
     *
     * @param key A key string.
     * @param value A Collection value.
     * @return this.
     * @throws JSONException If the key is null.
     */
    public JSONObject put(String key, Collection<?> value) throws JSONException {
        this.put(key, new JSONArray(value));
        return this;
    }

    /**
     * Put a key/double pair in the JSONObject.
     *
     * @param key
     *            A key string.
     * @param value
     *            A double which is the value.
     * @return this.
     * @throws JSONException
     *             If the key is null or if the number is invalid.
     */
    public JSONObject put(String key, double value) throws JSONException {
        this.put(key, Double.valueOf(value));
        return this;
    }
    
    /**
     * Put a key/float pair in the JSONObject.
     *
     * @param key
     *            A key string.
     * @param value
     *            A float which is the value.
     * @return this.
     * @throws JSONException
     *             If the key is null or if the number is invalid.
     */
    public JSONObject put(String key, float value) throws JSONException {
        this.put(key, Float.valueOf(value));
        return this;
    }

    /**
     * Put a key/int pair in the JSONObject.
     *
     * @param key
     *            A key string.
     * @param value
     *            An int which is the value.
     * @return this.
     * @throws JSONException
     *             If the key is null.
     */
    public JSONObject put(String key, int value) throws JSONException {
        this.put(key, Integer.valueOf(value));
        return this;
    }

    /**
     * Put a key/long pair in the JSONObject.
     *
     * @param key
     *            A key string.
     * @param value
     *            A long which is the value.
     * @return this.
     * @throws JSONException
     *             If the key is null.
     */
    public JSONObject put(String key, long value) throws JSONException {
        this.put(key, Long.valueOf(value));
        return this;
    }

    /**
     * Put a key/value pair in the JSONObject, where the value will be a
     * JSONObject which is produced from a Map.
     *
     * @param key
     *            A key string.
     * @param value
     *            A Map value.
     * @return this.
     * @throws JSONException If the key is null.
     */
    public JSONObject put(String key, Map<?, ?> value) throws JSONException {
        this.put(key, new JSONObject(value));
        return this;
    }

    /**
     * Put a key/char pair in the JSONObject.
     *
     * @param key
     *            A key string.
     * @param value
     *            A char which is the value.
     * @return this.
     * @throws JSONException
     *             If the key is null.
     */
    public JSONObject put(String key, char value) throws JSONException {
        this.put(key, Character.valueOf(value));
        return this;
    }

    /**
     * Put a key/Date in the JSONObject.
     *
     * @param key
     *            A key string.
     * @param value
     *            A Date object which is the value or null.
     * @return this.
     * @throws JSONException
     *             If the key is null.
     */
    public JSONObject put(String key, java.util.Date value) throws JSONException {
        this.put(key, Long.valueOf(value == null ? 0 : value.getTime()));
        return this;
    }

    /**
     * Put a key/value pair in the JSONObject.
     *
     * @param key
     *            A key string.
     * @param value
     *            An object which is the value. It should be of one of these
     *            types: Boolean, Double, Integer, JSONArray, JSONObject, Long,
     *            String, null, or the JSONObject.
     * @return this.
     * @throws JSONException
     *             If the value is non-finite number or if the key is null.
     */
    public JSONObject put(String key, Object value) throws JSONException {
        if (key == null) {
            throw new NullPointerException("Null key.");
        }
        if (value != null)
            testValidity(value);
        this.map.put(key, value);
        return this;
    }

    /**
     * Put a key/value pair in the JSONObject.
     *
     * @param key A key string.
     * @param value A string value.
     * @return this.
     * @throws JSONException
     *             If the value is non-finite number or if the key is null.
     */
    public JSONObject put(String key, String value) throws JSONException {
        if (key == null) {
            throw new NullPointerException("Null key.");
        }
        if (value != null) {
            value = fixString(value);
            testValidity(value);
        }
        this.map.put(key, value);
        return this;
    }

    /**
     * Remove Unicode BOM from string.
     *
     * @param s
     * @return
     */
    static String fixString(String s) {
        if (s == null || s.isEmpty())
            return s;
        char c = s.charAt(0);
        if (c == '\uFEFF' || c == '\uFFFD' || c == '\uFFFE')
            return fixString(s.substring(1));
        return s;
    }

    /**
     * Put a key/value pair in the JSONObject, but only if the key and the value
     * are both non-null, and only if there is not already a member with that
     * name.
     *
     * @param key string
     * @param value object
     * @return this.
     * @throws JSONException
     *             if the key is a duplicate
     */
    public JSONObject putOnce(String key, Object value) throws JSONException {
        if (key != null && value != null) {
            if (this.opt(key) != null) {
                throw new JSONException("Duplicate key \"" + key + "\"");
            }
            this.put(key, value);
        }
        return this;
    }

    /**
     * Put a key/value pair in the JSONObject, but only if the key and the value
     * are both non-null.
     *
     * @param key
     *            A key string.
     * @param value
     *            An object which is the value. It should be of one of these
     *            types: Boolean, Double, Integer, JSONArray, JSONObject, Long,
     *            or String.
     * @return this.
     * @throws JSONException
     *             If the value is a non-finite number.
     */
    public JSONObject putOpt(String key, Object value) throws JSONException {
        if (key != null && value != null) {
            this.put(key, value);
        }
        return this;
    }

    /**
     * Creates a JSONPointer using an initialization string and tries to 
     * match it to an item within this JSONObject. For example, given a
     * JSONObject initialized with this document:
     * <pre>
     * {
     *     "a":{"b":"c"}
     * }
     * </pre>
     * and this JSONPointer string: 
     * <pre>
     * "/a/b"
     * </pre>
     * Then this method will return the String "c".
     * A JSONPointerException may be thrown from code called by this method.
     *   
     * @param jsonPointer string that can be used to create a JSONPointer
     * @return the item matched by the JSONPointer, otherwise null
     */
    public Object query(String jsonPointer) {
        return query(new JSONPointer(jsonPointer));
    }
    /**
     * Uses a user initialized JSONPointer  and tries to 
     * match it to an item within this JSONObject. For example, given a
     * JSONObject initialized with this document:
     * <pre>
     * {
     *     "a":{"b":"c"}
     * }
     * </pre>
     * and this JSONPointer: 
     * <pre>
     * "/a/b"
     * </pre>
     * Then this method will return the String "c".
     * A JSONPointerException may be thrown from code called by this method.
     *   
     * @param jsonPointer string that can be used to create a JSONPointer
     * @return the item matched by the JSONPointer, otherwise null
     */
    private Object query(JSONPointer jsonPointer) {
        return jsonPointer.queryFrom(this);
    }
    
    /**
     * Queries and returns a value from this object using {@code jsonPointer}, or
     * returns null if the query fails due to a missing key.
     * 
     * @param jsonPointer the string representation of the JSON pointer
     * @return the queried value or {@code null}
     * @throws IllegalArgumentException if {@code jsonPointer} has invalid syntax
     */
    public Object optQuery(String jsonPointer) {
    	return optQuery(new JSONPointer(jsonPointer));
    }
    
    /**
     * Queries and returns a value from this object using {@code jsonPointer}, or
     * returns null if the query fails due to a missing key.
     * 
     * @param jsonPointer The JSON pointer
     * @return the queried value or {@code null}
     * @throws IllegalArgumentException if {@code jsonPointer} has invalid syntax
     */
    private Object optQuery(JSONPointer jsonPointer) {
        try {
            return jsonPointer.queryFrom(this);
        } catch (JSONPointerException e) {
            return null;
        }
    }

    /**
     * Produce a string in double quotes with backslash sequences in all the
     * right places. A backslash will be inserted within </, producing <\/,
     * allowing JSON text to be delivered in HTML. In JSON text, a string cannot
     * contain a control character or an unescaped quote or backslash.
     *
     * @param string
     *            A String
     * @return A String correctly formatted for insertion in a JSON text.
     */
    static String quote(String string) {
        StringWriter sw = new StringWriter();
        synchronized (sw.getBuffer()) {
            try {
                return quote(string, sw).toString();
            } catch (IOException ignored) {
                // will never happen - we are writing to a string writer
                return "";
            }
        }
    }

    private static Writer quote(String string, Writer w) throws IOException {
        if (string == null || string.isEmpty()) {
            w.write("\"\"");
            return w;
        }

        char b;
        char c = 0;
        String hhhh;
        int i;
        int len = string.length();

        w.write('"');
        for (i = 0; i < len; i += 1) {
            b = c;
            c = string.charAt(i);
            switch (c) {
            case '\\':
            case '"':
                w.write('\\');
                w.write(c);
                break;
                /*
            case '/':
                if (b == '<') {
                    w.write('\\');
                }
                w.write(c);
                break;
                 */
            case '\b':
                w.write("\\b");
                break;
            case '\t':
                w.write("\\t");
                break;
            case '\n':
                w.write("\\n");
                break;
            case '\f':
                w.write("\\f");
                break;
            case '\r':
                w.write("\\r");
                break;
            default:
                if (c < ' ' || (c >= '\u0080' && c < '\u00a0')
                        || (c >= '\u2000' && c < '\u2100')) {
                    w.write("\\u");
                    hhhh = Integer.toHexString(c);
                    w.write("0000", 0, 4 - hhhh.length());
                    w.write(hhhh);
                } else {
                    w.write(c);
                }
            }
        }
        w.write('"');
        return w;
    }

    /**
     * Remove a name and its value, if present.
     *
     * @param key The name to be removed.
     * @return The value that was associated with the name, or null if there was
     *         no value.
     */
    public Object remove(String key) {
        return this.map.remove(key);
    }

    /**
     * Determine if two JSONObjects are similar.
     * They must contain the same set of names which must be associated with
     * similar values.
     *
     * @param other The other JSONObject
     * @return true if they are equal
     */
    boolean similar(Object other) {
        try {
            if (!(other instanceof JSONObject)) {
                return false;
            }
            if (!this.keySet().equals(((JSONObject)other).keySet())) {
                return false;
            }
            for (final Entry<String,?> entry : this.entrySet()) {
                String name = entry.getKey();
                Object valueThis = entry.getValue();
                Object valueOther = ((JSONObject)other).get(name);
                if(valueThis == valueOther) {
                	continue;
                }
                if(valueThis == null) {
                	return false;
                }
                if (valueThis instanceof JSONObject) {
                    if (!((JSONObject)valueThis).similar(valueOther)) {
                        return false;
                    }
                } else if (valueThis instanceof JSONArray) {
                    if (!((JSONArray)valueThis).similar(valueOther)) {
                        return false;
                    }
                } else if (!valueThis.equals(valueOther)) {
                    return false;
                }
            }
            return true;
        } catch (Throwable exception) {
            return false;
        }
    }
    
    /**
     * Tests if the value should be tried as a decimal. It makes no test if there are actual digits.
     * 
     * @param val value to test
     * @return true if the string is "-0" or if it contains '.', 'e', or 'E', false otherwise.
     */
    static boolean isDecimalNotation(final String val) {
        return val.indexOf('.') > -1 || val.indexOf('e') > -1
                || val.indexOf('E') > -1 || "-0".equals(val);
    }
    
    /**
     * Converts a string to a number using the narrowest possible type. Possible 
     * returns for this function are BigDecimal, Double, BigInteger, Long, and Integer.
     * When a Double is returned, it should always be a valid Double and not NaN or +-infinity.
     * 
     * @param val value to convert
     * @return Number representation of the value.
     * @throws NumberFormatException thrown if the value is not a valid number. A public
     *      caller should catch this and wrap it in a {@link JSONException} if applicable.
     */
    static Number stringToNumber(final String val) throws NumberFormatException {
        char initial = val.charAt(0);
        if ((initial >= '0' && initial <= '9') || initial == '-') {
            // decimal representation
            if (isDecimalNotation(val)) {
                // quick dirty way to see if we need a BigDecimal instead of a Double
                // this only handles some cases of overflow or underflow
                if (val.length()>14) {
                    return new BigDecimal(val);
                }
                final Double d = Double.valueOf(val);
                if (d.isInfinite() || d.isNaN()) {
                    // if we can't parse it as a double, go up to BigDecimal
                    // this is probably due to underflow like 4.32e-678
                    // or overflow like 4.65e5324. The size of the string is small
                    // but can't be held in a Double.
                    return new BigDecimal(val);
                }
                return d;
            }
            // integer representation.
            // This will narrow any values to the smallest reasonable Object representation
            // (Integer, Long, or BigInteger)
            
            // string version
            // The compare string length method reduces GC,
            // but leads to smaller integers being placed in larger wrappers even though not
            // needed. i.e. 1,000,000,000 -> Long even though it's an Integer
            // 1,000,000,000,000,000,000 -> BigInteger even though it's a Long
            //if(val.length()<=9){
            //    return Integer.valueOf(val);
            //}
            //if(val.length()<=18){
            //    return Long.valueOf(val);
            //}
            //return new BigInteger(val);
            
            // BigInteger version: We use a similar bitLenth compare as
            // BigInteger#intValueExact uses. Increases GC, but objects hold
            // only what they need. i.e. Less runtime overhead if the value is
            // long lived. Which is the better tradeoff? This is closer to what's
            // in stringToValue.
            BigInteger bi = new BigInteger(val);
            if(bi.bitLength()<=31){
                return bi.intValue();
            }
            if(bi.bitLength()<=63){
                return bi.longValue();
            }
            return bi;
        }
        throw new NumberFormatException("val ["+val+"] is not a valid number.");
    }

    /**
     * Try to convert a string into a number, boolean, or null. If the string
     * can't be converted, return the string.
     *
     * @param string
     *            A String.
     * @return A simple JSON value.
     */
    // Changes to this method must be copied to the corresponding method in
    // the XML class to keep full support for Android
    static Object stringToValue(String string) {
        if (string.isEmpty()) {
            return string;
        }
        if (string.equalsIgnoreCase("true")) {
            return Boolean.TRUE;
        }
        if (string.equalsIgnoreCase("false")) {
            return Boolean.FALSE;
        }
        if (string.equalsIgnoreCase("null")) {
            return null;
        }

        /*
         * If it might be a number, try converting it. If a number cannot be
         * produced, then the value will just be a string.
         */

        char initial = string.charAt(0);
        if ((initial >= '0' && initial <= '9') || initial == '-') {
            try {
                // if we want full Big Number support this block can be replaced with:
                // return stringToNumber(string);
                if (isDecimalNotation(string)) {
                    Double d = Double.valueOf(string);
                    if (!d.isInfinite() && !d.isNaN()) {
                        return d;
                    }
                } else {
                    long myLong = Long.parseLong(string);
                    if (string.equals(Long.toString(myLong))) {
                        if (myLong == (int) myLong) {
                            return (int) myLong;
                        }
                        return myLong;
                    }
                }
            } catch (Exception ignore) {
            }
        }
        return string;
    }

    /**
     * Throw an exception if the object is a NaN or infinite number.
     *
     * @param o
     *            The object to test.
     * @throws JSONException
     *             If o is a non-finite number.
     */
    static void testValidity(Object o) throws JSONException {
        if (o != null) {
            if (o instanceof Double) {
                if (((Double) o).isInfinite() || ((Double) o).isNaN()) {
                    throw new JSONException(
                            "JSON does not allow non-finite numbers.");
                }
            } else if (o instanceof Float) {
                if (((Float) o).isInfinite() || ((Float) o).isNaN()) {
                    throw new JSONException(
                            "JSON does not allow non-finite numbers.");
                }
            }
        }
    }

    /**
     * Produce a JSONArray containing the values of the members of this
     * JSONObject.
     *
     * @param names
     *            A JSONArray containing a list of key strings. This determines
     *            the sequence of the values in the result.
     * @return A JSONArray of values.
     * @throws JSONException
     *             If any of the values are non-finite numbers.
     */
    JSONArray toJSONArray(JSONArray names) throws JSONException {
        if (names == null || names.length() == 0) {
            return null;
        }
        JSONArray ja = new JSONArray();
        for (int i = 0; i < names.length(); i += 1) {
            ja.put(this.opt(names.getString(i)));
        }
        return ja;
    }

    /**
     * Make a JSON text of this JSONObject. For compactness, no whitespace is
     * added. If this would not result in a syntactically correct JSON text,
     * then null will be returned instead.
     * <p><b>
     * Warning: This method assumes that the data structure is acyclical.
     * </b>
     * 
     * @return a printable, displayable, portable, transmittable representation
     *         of the object, beginning with <code>{</code>&nbsp;<small>(left
     *         brace)</small> and ending with <code>}</code>&nbsp;<small>(right
     *         brace)</small>.
     */
    @Override
    public String toString() {
        try {
            return this.toString(0);
        } catch (Exception e) {
            return null;
        }
    }

    /**
     * Make a pretty-printed JSON text of this JSONObject.
     * 
     * <p>If <code>indentFactor &gt; 0</code> and the {@link JSONObject}
     * has only one key, then the object will be output on a single line:
     * <pre>{@code {"key": 1}}</pre>
     * 
     * <p>If an object has 2 or more keys, then it will be output across
     * multiple lines: <code>{
     *  "key1": 1,
     *  "key2": "value 2",
     *  "key3": 3
     * }</code>
     * <p><b>
     * Warning: This method assumes that the data structure is acyclical.
     * </b>
     *
     * @param indentFactor
     *            The number of spaces to add to each level of indentation.
     * @return a printable, displayable, portable, transmittable representation
     *         of the object, beginning with <code>{</code>&nbsp;<small>(left
     *         brace)</small> and ending with <code>}</code>&nbsp;<small>(right
     *         brace)</small>.
     * @throws JSONException
     *             If the object contains an invalid number.
     */
    public String toString(int indentFactor) throws JSONException {
        StringWriter w = new StringWriter();
        synchronized (w.getBuffer()) {
            return this.write(w, indentFactor, 0).toString();
        }
    }

    /**
     * Make a JSON text of an Object value. If the object has an
     * value.toJSONString() method, then that method will be used to produce the
     * JSON text. The method is required to produce a strictly conforming text.
     * If the object does not contain a toJSONString method (which is the most
     * common case), then a text will be produced by other means. If the value
     * is an array or Collection, then a JSONArray will be made from it and its
     * toJSONString method will be called. If the value is a MAP, then a
     * JSONObject will be made from it and its toJSONString method will be
     * called. Otherwise, the value's toString method will be called, and the
     * result will be quoted.
     *
     * <p>
     * Warning: This method assumes that the data structure is acyclical.
     *
     * @param value
     *            The value to be serialized.
     * @return a printable, displayable, transmittable representation of the
     *         object, beginning with <code>{</code>&nbsp;<small>(left
     *         brace)</small> and ending with <code>}</code>&nbsp;<small>(right
     *         brace)</small>.
     * @throws JSONException
     *             If the value is or contains an invalid number.
     */
    static String valueToString(Object value) throws JSONException {
    	// moves the implementation to JSONWriter as:
    	// 1. It makes more sense to be part of the writer class
    	// 2. For Android support this method is not available. By implementing it in the Writer
    	//    Android users can use the writer with the built in Android JSONObject implementation.
        return JSONWriter.valueToString(value);
    }

    /**
     * Wrap an object, if necessary.
     * If it is an array or collection, wrap it in a JSONArray. If it is
     * a map, wrap it in a JSONObject. If it is a standard property (Double,
     * String, et al) then it is already wrapped. Otherwise, if it comes from
     * one of the java packages, turn it into a string. And if it doesn't, try
     * to wrap it in a JSONObject. If the wrapping fails, then null is returned.
     *
     * @param object
     *            The object to wrap
     * @return The wrapped value
     */
    static Object wrap(Object object) {
        try {
            if (object == null) {
                return null;
            }
            if (object instanceof JSONObject || object instanceof JSONArray
                    || object instanceof JSONString
                    || object instanceof Byte || object instanceof Character
                    || object instanceof Short || object instanceof Integer
                    || object instanceof Long || object instanceof Boolean
                    || object instanceof Float || object instanceof Double
                    || object instanceof String || object instanceof BigInteger
                    || object instanceof BigDecimal || object instanceof Enum) {
                return object;
            }

            if (object instanceof Collection) {
                Collection<?> coll = (Collection<?>) object;
                return new JSONArray(coll);
            }
            if (object.getClass().isArray()) {
                return new JSONArray(object);
            }
            if (object instanceof Map) {
                Map<?, ?> map = (Map<?, ?>) object;
                return new JSONObject(map);
            }
            Package objectPackage = object.getClass().getPackage();
            String objectPackageName = objectPackage != null ? objectPackage
                    .getName() : "";
            if (true || objectPackageName.startsWith("java.")
                    || objectPackageName.startsWith("javax.")
                    || object.getClass().getClassLoader() == null) {
                return object.toString();
            }
            return new JSONObject(object);
        } catch (Exception exception) {
            return null;
        }
    }

    /**
     * Write the contents of the JSONObject as JSON text to a writer. For
     * compactness, no whitespace is added.
     * <p><b>
     * Warning: This method assumes that the data structure is acyclical.
     * </b>
     * 
     * @param writer The writer to write to.
     * @return The writer.
     * @throws JSONException If there is an error writing to the writer.
     */
    public Writer write(Writer writer) throws JSONException {
        return this.write(writer, 0, 0);
    }

    static Writer writeValue(Writer writer, Object value,
                             int indentFactor, int indent) throws JSONException, IOException {
        if (value == null) {
            writer.write("null");
        } else if (value instanceof JSONString) {
            Object o;
            try {
                o = ((JSONString) value).toJSONString();
            } catch (Exception e) {
                throw new JSONException(e);
            }
            writer.write(o != null ? o.toString() : quote(value.toString()));
        } else if (value instanceof Number) {
            // not all Numbers may match actual JSON Numbers. i.e. fractions or Imaginary
            final String numberAsString = numberToString((Number) value);
            try {
                // Use the BigDecimal constructor for it's parser to validate the format.
                @SuppressWarnings("unused")
                BigDecimal testNum = new BigDecimal(numberAsString);
                // Close enough to a JSON number that we will use it unquoted
                writer.write(numberAsString);
            } catch (NumberFormatException ex){
                // The Number value is not a valid JSON number.
                // Instead we will quote it as a string
                quote(numberAsString, writer);
            }
        } else if (value instanceof Boolean) {
            writer.write(value.toString());
        } else if (value instanceof Enum<?>) {
            writer.write(quote(((Enum<?>)value).name()));
        } else if (value instanceof JSONObject) {
            ((JSONObject) value).write(writer, indentFactor, indent);
        } else if (value instanceof JSONArray) {
            ((JSONArray) value).write(writer, indentFactor, indent);
        } else if (value instanceof Map) {
            Map<?, ?> map = (Map<?, ?>) value;
            new JSONObject(map).write(writer, indentFactor, indent);
        } else if (value instanceof Collection) {
            Collection<?> coll = (Collection<?>) value;
            new JSONArray(coll).write(writer, indentFactor, indent);
        } else if (value.getClass().isArray()) {
            new JSONArray(value).write(writer, indentFactor, indent);
        } else {
            quote(value.toString(), writer);
        }
        return writer;
    }

    static final void indent(Writer writer, int indent) throws IOException {
        for (int i = 0; i < indent; i += 1) {
            writer.write(' ');
        }
    }

    /**
     * Write the contents of the JSONObject as JSON text to a writer.
     * 
     * <p>If <code>indentFactor &gt; 0</code> and the {@link JSONObject}
     * has only one key, then the object will be output on a single line:
     * <pre>{@code {"key": 1}}</pre>
     * 
     * <p>If an object has 2 or more keys, then it will be output across
     * multiple lines: <code><pre>{
     *  "key1": 1,
     *  "key2": "value 2",
     *  "key3": 3
     * }</pre></code>
     * <p><b>
     * Warning: This method assumes that the data structure is acyclical.
     * </b>
     *
     * @param writer
     *            Writes the serialized JSON
     * @param indentFactor
     *            The number of spaces to add to each level of indentation.
     * @param indent
     *            The indentation of the top level.
     * @return The writer.
     * @throws JSONException
     */
    private Writer write(Writer writer, int indentFactor, int indent)
            throws JSONException {
        try {
            boolean commanate = false;
            final int length = this.length();
            writer.write('{');

            if (length == 1) {
            	final Entry<String,?> entry = this.entrySet().iterator().next();
                final String key = entry.getKey();
                writer.write(quote(key));
                writer.write(':');
                if (indentFactor > 0) {
                    writer.write(' ');
                }
                try{
                    writeValue(writer, entry.getValue(), indentFactor, indent);
                } catch (Exception e) {
                    throw new JSONException("Unable to write JSONObject value for key: " + key, e);
                }
            } else if (length != 0) {
                final int newindent = indent + indentFactor;
                for (final Entry<String,?> entry : this.entrySet()) {
                    if (commanate) {
                        writer.write(',');
                    }
                    if (indentFactor > 0) {
                        writer.write('\n');
                    }
                    indent(writer, newindent);
                    final String key = entry.getKey();
                    writer.write(quote(key));
                    writer.write(':');
                    if (indentFactor > 0) {
                        writer.write(' ');
                    }
                    try {
                        writeValue(writer, entry.getValue(), indentFactor, newindent);
                    } catch (Exception e) {
                        throw new JSONException("Unable to write JSONObject value for key: " + key, e);
                    }
                    commanate = true;
                }
                if (indentFactor > 0) {
                    writer.write('\n');
                }
                indent(writer, indent);
            }
            writer.write('}');
            return writer;
        } catch (IOException exception) {
            throw new JSONException(exception);
        }
    }

    /**
     * Returns a java.util.Map containing all of the entries in this object.
     * If an entry in the object is a JSONArray or JSONObject it will also
     * be converted.
     * <p>
     * Warning: This method assumes that the data structure is acyclical.
     *
     * @return a java.util.Map containing the entries of this object
     */
    Map<String, Object> toMap() {
        Map<String, Object> results = new HashMap<String, Object>();
        for (Entry<String, Object> entry : this.entrySet()) {
            Object value;
            if (entry.getValue() == null) {
                value = null;
            } else if (entry.getValue() instanceof JSONObject) {
                value = ((JSONObject) entry.getValue()).toMap();
            } else if (entry.getValue() instanceof JSONArray) {
                value = ((JSONArray) entry.getValue()).toList();
            } else {
                value = entry.getValue();
            }
            results.put(entry.getKey(), value);
        }
        return results;
    }
}
