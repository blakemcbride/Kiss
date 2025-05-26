package org.kissweb.googleMapsAPI;

import org.json.JSONObject;
import org.kissweb.LRUCache;
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

    private static final String TIMEZONE_URL = "https://maps.googleapis.com/maps/api/timezone/json";

    private static final LRUCache<String,String> timezoneCache = new LRUCache<>(500L, 0L);

    /**
     * Perform a timezone query.
     * <br><br>
     * Example:
     * <pre>
     * String timezoneID = GoogleMapsAPI.getTimezone(38.234567, -85.003456);
     * </pre>
     * @param latitude  latitude
     * @param longitude  longitude
     * @return timeZoneId
     * @throws IOException if the API call fails
     */
    public static String getTimezone(double latitude, double longitude) throws IOException {
        String location = GoogleAPI.roundCoordinates(latitude, longitude);
        String tz = timezoneCache.get(location);
        if (tz != null)
            return tz;
        final URLBuilder url = new URLBuilder(TIMEZONE_URL);
        url.addParameter("location", location);
        url.addParameter("timestamp", ""+(System.currentTimeMillis() / 1000));
        url.addParameter("key", GoogleAPI.getValidAPIKey());
        final String surl = url.build();

        RestClient rc = new RestClient();
        JSONObject result = rc.jsonCall("GET", surl);
        try {
            tz = result.getString("timeZoneId");
            timezoneCache.add(location, tz);
            return tz;
        } catch (Throwable ex) {
            return null;
        }
    }

    /**
     * Perform a timezone query based on an address.
     * <br><br>
     * Example:
     * <pre>
     * String timezoneID = GoogleMapsAPI.getTimezone("1600 Amphitheatre Parkway, Mountain View, CA");
     * </pre>
     * @param address  address
     * @return timeZoneId
     * @throws IOException if the API call fails
     */
    public static String getTimezone(String address) throws IOException {
        double[] latLng = GoogleGeocode.findGeoLocation(address);
        if (latLng == null)
            return null;
        return  getTimezone(latLng[0], latLng[1]);
    }

    /**
     * Perform a timezone query based on an address broken into its parts.
     * <br><br>
     * Example:
     * <pre>
     * String timezoneID = GoogleMapsAPI.getTimezone("1600 Amphitheatre Parkway", null, "Mountain View", "CA", "94043");
     * </pre>
     * @param street1  street address 1
     * @param street2  street address 2 (optional)
     * @param city     city
     * @param state    state
     * @param zip      zip code (optional)
     * @return timeZoneId
     * @throws IOException if the API call fails
     */
    public static String getTimezone(String street1, String street2, String city, String state, String zip) throws IOException {
        String address = street1;
        if (street2 != null)
            address += ", " + street2;
        address += ", " + city + ", " + state;
        if (zip != null)
            address += " " + zip;
        return getTimezone(address);
    }

}
