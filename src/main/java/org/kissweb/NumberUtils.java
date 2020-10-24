package org.kissweb;

/**
 * Author: Blake McBride
 * Date: 10/24/20
 */
public class NumberUtils {

    /**
     * Correctly compare two doubles.
     *
     * @param d1
     * @param d2
     * @param maxdiff
     * @return true if the two doubles are different by less than maxdiff
     */
    public static boolean doubleEqual(double d1, double d2, double maxdiff) {
        double diff = d1 - d2;
        if (diff < 0.0)
            diff = -diff;
        return diff < maxdiff;
    }

    /**
     * Round a number to the nearest p decimal places.
     *
     * @param n the number to be rounded
     * @param p the number of decimal places
     * @return the rounded number
     */
    public static double round(double n, int p) {
        double r = Math.pow(10.0, (double) p);
        r = Math.floor(0.5 + Math.abs(n * r)) / r;
        return n < 0.0 ? -r : r;
    }

    /**
     * Round a number up to the nearest p decimal places.
     *
     * @param n the number to be rounded
     * @param p the number of decimal places
     * @return the rounded number
     */
    public static double roundUp(double n, int p) {
        double r = Math.pow(10.0, (double) p);
        r = Math.ceil(Math.abs(n * r)) / r;
        return n < 0.0 ? -r : r;
    }

    /**
     * Round a number down to the nearest p decimal places.
     *
     * @param n the number to be rounded
     * @param p the number of decimal places
     * @return the rounded number
     */
    public static double roundDown(double n, int p) {
        double r = Math.pow(10.0, (double) p);
        r = Math.floor(Math.abs(n * r)) / r;
        return n < 0.0 ? -r : r;
    }

}
