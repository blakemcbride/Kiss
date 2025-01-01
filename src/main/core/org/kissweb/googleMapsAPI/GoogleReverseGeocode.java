package org.kissweb.googleMapsAPI;

import org.json.JSONArray;
import org.json.JSONObject;
import org.kissweb.RestClient;
import org.kissweb.URLBuilder;

import java.io.IOException;

/**
 * This class converts a latitude/longitude into a city and state.
 * <br><br>
 * Author: Blake McBride<br>
 * Date: 3/5/22
 */
public class GoogleReverseGeocode {

    private final static String URL = "https://maps.googleapis.com/maps/api/geocode/json";
    private JSONObject result = null;

    /**
     * Performs the actual query through Google.
     * You must set the API_KEY before creating this object.
     */
    public GoogleReverseGeocode(double latitude, double longitude) {
        final URLBuilder url = new URLBuilder(URL);
        url.addParameter("latlng", latitude + "," + longitude);
        url.addParameter("sensor", "true");
        url.addParameter("key", GoogleAPIKey.getAPIKey());
        final String surl = url.build();

        RestClient rc = new RestClient();
        try {
            result = rc.jsonCall("GET", surl);
        } catch (IOException ignore) {
        }
    }

    public String getCityState() {
        try {
            String city = null;
            String state = null;
            JSONArray results = result.getJSONArray("results");
            JSONObject res = results.getJSONObject(0);
            JSONArray aca = res.getJSONArray("address_components");
            int n = aca.length();
            for (int i=0 ; i < n ; i++) {
                JSONObject ac = aca.getJSONObject(i);
                JSONArray types = ac.getJSONArray("types");
                String type = types.getString(0);
                if (type.equals("locality")) {
                    city = ac.getString("long_name");
                } else if (type.equals("administrative_area_level_1")) {
                    state = ac.getString("short_name");
                }
            }
            String cs = city == null ? "" : city;
            if (city != null && state != null)
                cs += ", ";
            if (state != null)
                cs += state;
            return cs;
        } catch (Throwable t) {
            return null;
        }
    }

    /**
     * Example use of this class.
     */
    private void example() {
        GoogleAPIKey.setAPIKey("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx");
        GoogleReverseGeocode grg = new GoogleReverseGeocode(27.09876, -83.589023);
        String cs = grg.getCityState();
    }

}
