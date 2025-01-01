package org.kissweb.googleMapsAPI;

/**
 * Class used to store the global key for Google API.
 */
public class GoogleAPIKey {

    private static String API_KEY;

    public static void setAPIKey(String key) {
        API_KEY = key;
    }

    public static String getAPIKey() {
        return API_KEY;
    }
}
