package org.kissweb;

import java.text.NumberFormat;

/**
 * Author: Blake McBride
 * Date: 10/24/20
 */
public class NumberUtils {

    /**
     * Private constructor to prevent instantiation of this utility class.
     * All methods in this class are static and should be called directly.
     */
    private NumberUtils() {
        // Utility class - not meant to be instantiated
    }

    /**
     * Correctly compare two doubles.
     *
     * @param d1 the first double value
     * @param d2 the second double value
     * @param maxdiff the maximum allowed difference
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
        double r = Math.pow(10.0, p);
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
        double r = Math.pow(10.0, p);
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
        double r = Math.pow(10.0, p);
        r = Math.floor(Math.abs(n * r)) / r;
        return n < 0.0 ? -r : r;
    }

    /**
     * Truncate the decimal part of a number.
     *
     * @param v the value to truncate
     * @return the truncated value
     */
    public static double trunk(double v) {
        return Math.floor(v);
    }

    /**
     * Parse a String into a double.
     *
     * @param s the string to parse
     * @return the parsed double value or 0 if parsing fails
     */
    public static double parseDouble(String s) {
        try {
            Number n = NumberFormat.getInstance().parse(s);
            if (n instanceof Long) {
                long v = (long) n;
                return (double) v;
            } else {
                return (double) n;
            }
        } catch (Exception e) {
            return 0;
        }
    }

    /**
     * Parse a String into a long.
     *
     * @param s the string to parse
     * @return the parsed long value or 0 if parsing fails
     */
    public static long parseLong(String s) {
        try {
            Number n = NumberFormat.getInstance().parse(s);
            if (n instanceof Double) {
                double v = (double) n;
                return (long) v;
            } else {
                return (long) n;
            }
        } catch (Exception e) {
            return 0;
        }
    }

    /**
     * Is string a valid number?
     *
     * @param s the string to check
     * @return true if the string represents a valid number
     */
    public static boolean isValidNumber(String s) {
        if (s == null)
            return false;
        s = s.trim();
        if (s.isEmpty())
            return false;
        if (!s.replaceAll("[,.0-9]", "").isEmpty())  // bad character check
            return false;
        if (s.replaceAll("[,.]", "").isEmpty()) // has some digits
            return false;
        if (s.replaceAll("[^.]", "").length() > 1) // 0 or 1 decimal place
            return false;
        return true;
    }

    /**
     * Parse a String into an int.
     *
     * @param s the string to parse
     * @return the parsed int value or 0 if parsing fails
     */
    public static int parseInt(String s) {
        return (int) parseLong(s);
    }

    /**
     * Test method for NumberUtils functionality.
     *
     * @param args command line arguments
     */
    public static void main(String [] args) {

        System.out.println(isValidNumber("38") ? "true" : "false");
        System.out.println(isValidNumber("38.17") ? "true" : "false");
        System.out.println(isValidNumber("38.5.") ? "true" : "false");
        System.out.println(isValidNumber("385,345.78") ? "true" : "false");
        System.out.println(isValidNumber("38x") ? "true" : "false");

        int v;
        double d;
        v = parseInt("45");
        v = parseInt("");
        v = parseInt("abc");
        v = parseInt(null);
        v = parseInt("45.99");
        v = parseInt("48 xx");
        v = parseInt("483,345");

        d = parseDouble("45");
        d = parseDouble("");
        d = parseDouble("abc");
        d = parseDouble(null);
        d = parseDouble("45.99");
        d = parseDouble("48 xx");
        d = parseDouble("488,678.98");

        int x = 1;
    }

}
