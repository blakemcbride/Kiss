package org.kissweb.googleMapsAPI;

/**
 * Class used to store the global key for Google API.
 */
public abstract class GoogleAPI {

    private final static int DEFAULT_DISTANCE_FEET = 100;

    private final String apiKey;

    /**
     * Create a new instance providing the Google API key for this instance.
     * @param apiKey  Google API key
     */
    GoogleAPI(String apiKey) {
        this.apiKey = apiKey;
    }

    /**
     * Retrieves the global key for Google API.  If the key has not been set
     * yet, this will throw a RuntimeException.
     *
     * @return the global key for Google API.
     * @throws RuntimeException if the key has not been set yet
     */
    String getValidAPIKey() {
        if (apiKey == null || apiKey.isEmpty())
            throw new RuntimeException("Google API key is null");
        return apiKey;
    }

    /**
     * Rounds the given latitude and longitude to the nearest interval corresponding
     * to the specified distance in feet (as an integer). Coordinates within the specified
     * distance will round to the same value.
     *
     * @param distanceFeet the distance in feet to round to (as an integer)
     * @param latitude the latitude in decimal degrees
     * @param longitude the longitude in decimal degrees
     * @return a string in the format "roundedLatitude,roundedLongitude"
     */
    public static String roundCoordinates(int distanceFeet, double latitude, double longitude) {
        final double FEET_PER_DEGREE = 364000.0;
        // Calculate the degree interval for latitude.
        double latInterval = distanceFeet / FEET_PER_DEGREE;

        // For longitude, adjust by the cosine of the latitude.
        double cosLat = Math.cos(Math.toRadians(latitude));
        double lonInterval = (Math.abs(cosLat) < 1e-10) ? latInterval : distanceFeet / (FEET_PER_DEGREE * cosLat);

        // Round the coordinates to the nearest interval.
        double roundedLat = Math.round(latitude / latInterval) * latInterval;
        double roundedLon = Math.round(longitude / lonInterval) * lonInterval;

        // Format the result as a string.
        return String.format("%.6f,%.6f", roundedLat, roundedLon);
    }

    /**
     * Rounds the given latitude and longitude to the nearest interval corresponding
     * to the default distance in feet (as an integer). Coordinates within the specified
     * distance will round to the same value.
     *
     * @param latitude the latitude in decimal degrees
     * @param longitude the longitude in decimal degrees
     * @return a string in the format "roundedLatitude,roundedLongitude"
     */
    public static String roundCoordinates(double latitude, double longitude) {
        return roundCoordinates(DEFAULT_DISTANCE_FEET, latitude, longitude);
    }

    /**
     * Returns the default distance in feet to round to.
     *
     * @return the default distance in feet
     */
    public static int getDefaultDistanceFeet() {
        return DEFAULT_DISTANCE_FEET;
    }
}
