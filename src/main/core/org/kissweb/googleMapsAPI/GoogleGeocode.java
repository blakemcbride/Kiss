package org.kissweb.googleMapsAPI;

import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;
import org.kissweb.LRUCache;
import org.kissweb.NumberUtils;
import org.kissweb.RestClient;
import org.kissweb.URLBuilder;

import java.io.IOException;

/**
 * This class converts a latitude/longitude into a city and state.
 * <br><br>
 * Author: Blake McBride<br>
 * Date: 3/5/22
 */
public class GoogleGeocode extends GoogleAPI {

    private final static String URL = "https://maps.googleapis.com/maps/api/geocode/json";
    private JSONObject result = null;

    private static final LRUCache<String,double[]> addressCache = new LRUCache<>(200L, 0L);

    /**
     * Create a new instance providing the Google API key for this instance.
     * @param apiKey  Google API key
     */
    public GoogleGeocode(String apiKey) {
        super(apiKey);
    }

    /**
     * Performs the actual query through Google.
     * You must set the API_KEY before creating this object.
     *
     * @param latitude the latitude coordinate
     * @param longitude the longitude coordinate
     * @return this
     */
    public GoogleGeocode get(double latitude, double longitude) {
        final URLBuilder url = new URLBuilder(URL);
        url.addParameter("latlng", latitude + "," + longitude);
        url.addParameter("sensor", "true");
        url.addParameter("key", getValidAPIKey());
        final String surl = url.build();

        RestClient rc = new RestClient();
        try {
            result = rc.jsonCall("GET", surl);
        } catch (IOException ignore) {
        }
        return this;
    }

    /**
     * Extracts city and state from the geocoding result.
     *
     * @return the city and state in "city, state" format, or null if not found
     */
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
     * This method uses the Google Geocoding API to get lat/long from an address.
     *
     * @param address the address to geocode
     * @return an array containing [latitude, longitude] or null if not found
     * @throws IOException if the API call fails
     */
    public double[] findGeoLocation(String address) throws IOException {
        double[] res = addressCache.get(address);
        if (res != null && !NumberUtils.doubleEqual(res[0], -1.0, 0.0001) && !NumberUtils.doubleEqual(res[1], -1.0, 0.0001))
            return res;
        URLBuilder url = new URLBuilder(URL);
        url.addParameter("address", address);
        url.addParameter("key", getValidAPIKey());

        RestClient rc = new RestClient();
        JSONObject response = rc.jsonCall("GET", url.build());

        if (!response.has("results") || response.getJSONArray("results").length() == 0) {
            // No results or invalid response
            addressCache.add(address, new double[]{-1,-1});
            return null;
        }

        JSONObject location = response
                .getJSONArray("results")
                .getJSONObject(0)
                .getJSONObject("geometry")
                .getJSONObject("location");

        double lat = location.getDouble("lat");
        double lng = location.getDouble("lng");
        res = new double[]{ lat, lng };
        addressCache.add(address, res);
        return res;
    }

    /**
     * Example use of this class.
     */
    private static void example() {
        GoogleGeocode grg = new GoogleGeocode("xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx").get(27.09876, -83.589023);
        String cs = grg.getCityState();
    }

}
