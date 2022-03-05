package org.kissweb;

import org.json.JSONArray;
import org.json.JSONObject;
import java.io.IOException;

/**
 * This class gets the distance in miles between two addresses through a Google service.
 * It also gets the estimated travel time in minutes.
 * A Google API is needed for this to work.  (It comes from Google.)
 *
 * Author: Blake McBride
 * Date: 3/4/22
 */
public class GoogleDistance {

    private final static String URL = "https://maps.googleapis.com/maps/api/distancematrix/json";
    private static String API_KEY;  // from Google
    private JSONObject result = null;
    private JSONObject elm0 = null;

    /**
     * This key must be set before you can find any distances or times.
     * The key comes from Google.
     *
     * @param key
     */
    public static void setAPIKey(String key) {
        API_KEY = key;
    }

    /**
     * Performs the actual query through Google.
     * You must set the API_KEY before creating this object.
     * <br><br>
     * Each address may be a normal address such as:  "735 Spring Street, Atlanta, GA 33456"<br>
     * Or, they may be latitude longitude such as:  "38.234567 -85.003456"
     *
     * @param add1
     * @param add2
     */
    public GoogleDistance(String add1, String add2) {
        final URLBuilder url = new URLBuilder(URL);
        url.addParameter("origins", add1);
        url.addParameter("destinations", add2);
        url.addParameter("units", "imperial");
        url.addParameter("key", API_KEY);
        final String surl = url.build();

        RestClient rc = new RestClient();
        try {
            result = rc.jsonCall("GET", surl);
        } catch (IOException ignore) {
        }
    }

    /**
     * Returns the distance in miles between two addresses.
     * <br><br>
     * Returns -1 on error.
     */
    public int miles() {
        try {
            if (elm0 == null) {
                elm0 = getElm0();
                if (elm0 == null)
                    return -1;
            }
            JSONObject dis = elm0.getJSONObject("distance");
            return (int) ((double) dis.getInt("value") / 1606.244);
        } catch (Throwable ex) {
            return -1;
        }
    }

    /**
     * Return the estimated travel time in minutes.<br>
     * -1 is returned on error.
     */
    public int minutes() {
        try {
            if (elm0 == null) {
                elm0 = getElm0();
                if (elm0 == null)
                    return -1;
            }
            JSONObject dur = elm0.getJSONObject("duration");
            return dur.getInt("value") / 60;
        } catch (Throwable ex) {
            return -1;
        }
    }

    private JSONObject getElm0() {
        final JSONArray rows = result.getJSONArray("rows");
        if (rows.length() != 1)
            return null;
        final JSONObject row0 = rows.getJSONObject(0);
        final JSONArray elements = row0.getJSONArray("elements");
        if (elements.length() != 1)
            return null;
        return elements.getJSONObject(0);
    }

    /**
     * Example use of this class.
     */
    private void example() {
        GoogleDistance.setAPIKey("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
        GoogleDistance gm = new GoogleDistance("995 Meridian Blvd, Franklin, TN 37067", "7270 Gary Ave, Miami Beach, FL");
        int miles = gm.miles();
        int minutes = gm.minutes();
    }

}
