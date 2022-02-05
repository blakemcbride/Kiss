/* global Utils */

/*
 * Created by Blake McBride on 5/30/2021
 */

'use strict';

/**
 * Utilities to deal with numbers.
 */
class NumberUtils {

    /**
     * Correctly compare two numbers with decimals.
     *
     * @param {number} d1
     * @param {number} d2
     * @param {number} maxdiff
     * @returns {boolean} true if the two doubles are different by less than maxdiff
     */
    static floatEqual(d1, d2, maxdiff) {
        let diff = d1 - d2;
        if (diff < 0.0)
            diff = -diff;
        return diff < maxdiff;
    }

    /**
     * Round a number to a specific number of decimal places.
     *
     * @param {number} num the number to be rounded
     * @param {number} places the number of decimal places
     * @returns {number} the rounded number
     */
    static round(num, places) {
        const mul = Math.pow(10, places);
        return Math.round(num * mul) / mul;
    }

    /**
     * Round a number up to the nearest p decimal places.
     *
     * @param {number} n the number to be rounded
     * @param {number} p the number of decimal places
     * @return {number} the rounded number
     */
    static roundUp(n, p) {
        let r = Math.pow(10, p);
        r = Math.ceil(Math.abs(n * r)) / r;
        return n < 0 ? -r : r;
    }

    /**
     * Round a number down to the nearest p decimal places.
     *
     * @param {number} n the number to be rounded
     * @param {number} p the number of decimal places
     * @return {number} the rounded number
     */
    static roundDown(n, p) {
        let r = Math.pow(10, p);
        r = Math.floor(Math.abs(n * r)) / r;
        return n < 0 ? -r : r;
    }
}