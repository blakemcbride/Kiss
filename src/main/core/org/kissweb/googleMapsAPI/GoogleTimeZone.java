package org.kissweb.googleMapsAPI;

import org.json.JSONObject;
import org.kissweb.RestClient;
import org.kissweb.URLBuilder;

import java.io.IOException;

/**
 * This class returns the timezone ID associated with a latitude/longitude.
 * <br><br>
 * Author: Blake McBride<br>
 * Date: 3/5/22
 */
public class GoogleTimeZone {

    private final static String URL = "https://maps.googleapis.com/maps/api/timezone/json";
    private JSONObject result = null;

    /**
     * Perform a timezone query.
     * <br><br>
     * Example:
     * <pre>
     * GoogleMapsAPI gma = GoogleMapsAPI.getTimezone(38.234567, -85.003456);
     * String timezoneID = gma.getTimezone();
     * </pre>
     * @param latitude  latitude
     * @param longitude  longitude
     * @return GoogleMapsAPI object
     * @throws IOException
     */
    public static GoogleTimeZone getTimezone(double latitude, double longitude) throws IOException {
        final GoogleTimeZone gma = new GoogleTimeZone();
        final URLBuilder url = new URLBuilder(URL);
        url.addParameter("location", latitude + "," + longitude);
        url.addParameter("timestamp", ""+(System.currentTimeMillis() / 1000));
        url.addParameter("key", GoogleAPIKey.getAPIKey());
        final String surl = url.build();

        RestClient rc = new RestClient();
        gma.result = rc.jsonCall("GET", surl);
        return gma;
    }

    /**
     * Via the use of <code>getTimezone</code> return the timezone ID associated with the last query.
     * <br><br>
     * Returns null on error.
     * @return timezone ID
     */
    public String getTimezone() {
        try {
            return result.getString("timeZoneId");
        } catch (Throwable ex) {
            return null;
        }
    }

}
