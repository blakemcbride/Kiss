package org.kissweb.googleMapsAPI;

import org.json.JSONArray;
import org.json.JSONObject;
import org.kissweb.LRUCache;
import org.kissweb.RestClient;
import org.kissweb.URLBuilder;

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
    private static final LRUCache<String,JSONObject> distanceCache = new LRUCache<>(200L, 0L);
    private static final JSONObject NO_RETURN = new JSONObject();
    private JSONObject elm0;

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
        final String bothAddresses = add1 + "|" + add2;
        elm0 = distanceCache.get(bothAddresses);
        if (elm0 != null)
            return;
        final URLBuilder url = new URLBuilder(URL);
        url.addParameter("origins", add1);
        url.addParameter("destinations", add2);
        url.addParameter("units", "imperial");
        url.addParameter("key", GoogleAPI.getValidAPIKey());
        final String surl = url.build();

        RestClient rc = new RestClient();
        try {
            JSONObject result = rc.jsonCall("GET", surl);
            distanceCache.add(bothAddresses, elm0=getElm0(result));
        } catch (IOException ignore) {
        }
    }

    /**
     * Returns the distance in miles between two addresses.
     * <br><br>
     * Returns -1 on error.
     */
    public int miles() {
        if (elm0 == NO_RETURN)
            return -1;
        JSONObject dis = elm0.getJSONObject("distance");
        return (int) ((double) dis.getInt("value") / 1609.344);
    }

    /**
     * Return the estimated travel distance in meters.
     * <br><br>
     * Returns -1 on error.
     */
    public int meters() {
        if (elm0 == NO_RETURN)
            return -1;
        JSONObject dis = elm0.getJSONObject("distance");
        return dis.getInt("value");
    }

    /**
     * Return the estimated travel time in minutes.<br>
     * -1 is returned on error.
     */
    public int minutes() {
        if (elm0 == NO_RETURN)
            return -1;
        JSONObject dur = elm0.getJSONObject("duration");
        return dur.getInt("value") / 60;
    }

    private JSONObject getElm0(JSONObject result) {
        try {
            final JSONArray rows = result.getJSONArray("rows");
            if (rows.length() != 1)
                return NO_RETURN;
            final JSONObject row0 = rows.getJSONObject(0);
            final JSONArray elements = row0.getJSONArray("elements");
            if (elements.length() != 1)
                return NO_RETURN;
            return elements.getJSONObject(0);
        } catch (Exception e) {
            return NO_RETURN;
        }
    }

    /**
     * Example use of this class.
     */
    private void example() {
        GoogleAPI.setAPIKey("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
        GoogleDistance gm = new GoogleDistance("995 Meridian Blvd, Franklin, TN 37067", "7270 Gary Ave, Miami Beach, FL");
        int miles = gm.miles();
        int minutes = gm.minutes();
    }

}
