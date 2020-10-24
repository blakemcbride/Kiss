package org.kissweb;

import java.text.NumberFormat;

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
     * @param v
     * @return
     */
    public static double trunk(double v) {
        return Math.floor(v);
    }

    /**
     * Parse a String into a double.
     *
     * @param s
     * @return
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
     * @param s
     * @return
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
     * Parse a String into an int.
     *
     * @param s
     * @return
     */
    public static int parseInt(String s) {
        return (int) parseLong(s);
    }

    public static void main(String [] args) {
        int v;
        double d;
        v = parseInt("45");
        v = parseInt("");
        v = parseInt("abc");
        v = parseInt(null);
        v = parseInt("45.99");
        v = parseInt("48 xx");

        d = parseDouble("45");
        d = parseDouble("");
        d = parseDouble("abc");
        d = parseDouble(null);
        d = parseDouble("45.99");
        d = parseDouble("48 xx");

        int x = 1;
    }

}
