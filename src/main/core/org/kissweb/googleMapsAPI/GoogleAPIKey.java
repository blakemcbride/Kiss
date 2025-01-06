package org.kissweb.googleMapsAPI;

/**
 * Class used to store the global key for Google API.
 */
public class GoogleAPIKey {

    private static String apiKey;

    /**
     * Sets the global key for Google API.
     *
     * @param key The key to use for the Google API.
     */
    public static void setAPIKey(String key) {
        apiKey = key;
    }

    /**
     * Retrieves the global key for Google API.
     * <br><br>
     * This will return null if the key has not been set yet.
     *
     * @return the global key for Google API.
     */
    public static String getAPIKey() {
        return apiKey;
    }

    /**
     * Retrieves the global key for Google API.  If the key has not been set
     * yet, this will throw a RuntimeException.
     *
     * @return the global key for Google API.
     * @throws RuntimeException if the key has not been set yet
     */
    public static String getValidAPIKey() {
        if (apiKey == null || apiKey.isEmpty())
            throw new RuntimeException("Google API key is null");
        return apiKey;
    }
}
