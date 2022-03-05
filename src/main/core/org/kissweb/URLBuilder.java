package org.kissweb;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;

/**
 * A class used to process URL escape sequences.
 * <br><br>
 * Author: Blake McBride<br>
 * Date: 12/20/21
 */
public class URLBuilder {

    private final String url;
    private String parameters;

    /**
     * Create a URLBuilder passing it the full URL minus any URL parameters.
     *
     * @param url
     */
    public URLBuilder(String url) {
        this.url = url.replaceAll(" ", "%20");
        parameters = "";
    }

    /**
     * Add a parameter to the URL.  This method correctly escapes the arguments.
     *
     * @param param
     * @param value
     * @return
     */
    public URLBuilder addParameter(String param, String value) {
        if (parameters.isEmpty())
            parameters = "?" + encodeURLString(param) + "=" + encodeURLString(value);
        else
            parameters += "&" + encodeURLString(param) + "=" + encodeURLString(value);
        return this;
    }

    /**
     * Return the final usable URL.
     *
     * @return
     */
    public String build() {
        return url + parameters;
    }

    /**
     * URL encode a string.
     *
     * @param s
     * @return
     */
    public static String encodeURLString(String s) {
        try {
            return URLEncoder.encode(s, StandardCharsets.UTF_8.toString());
        } catch (UnsupportedEncodingException ex) {
            throw new RuntimeException(ex.getCause());
        }
    }
}
