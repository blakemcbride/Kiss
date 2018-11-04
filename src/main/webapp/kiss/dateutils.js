/* global utils */

/**
 * Created by Blake McBride on 3/15/18.
 */

'use strict';

/**
     Function names are meant to tell what the input and output date date types they work on.
     In terms of understanding the data types used to represent dates:

     int          20180322
     str          "3/22/[20]18"
     str2         "3/22/18"
     str4         "3/22/2018"
     SQL          "2018-03-22"
     Date         JavaScript Date object
 */
class dateutils {

    /**
     * Converts a string date "mM/dD/yyYY" to an integer of the form YYYYMMDD.
     * Bad dates return 0
     *
     * @param {string} dateString
     * @returns {number}
     */
    static strToInt(dateString) {
        dateString = dateString.trim();
        dateString = dateString.replace(/-/g, '/');
        dateString = dateString.replace(/\./g, '/');

        if (!/^\d{1,2}\/\d{1,2}\/\d{2,4}$/.test(dateString))
            return 0;

        var parts = dateString.split("/");
        var day = parseInt(parts[1], 10);
        var month = parseInt(parts[0], 10);
        var year = parseInt(parts[2], 10);

        if (year < 100) {
            var currentYear = new Date().getFullYear();
            var y19 = 1900 + year;
            var y20 = 2000 + year;
            year = Math.abs(currentYear - y19) < Math.abs(currentYear - y20) ? y19 : y20;
        }

        if (year < 1000 || year > 3000 || month < 1 || month > 12 || day < 1)
            return 0;

        var monthLength = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

        // Adjust for leap years
        if (year % 400 === 0 || (year % 100 !== 0 && year % 4 === 0))
            monthLength[1] = 29;

        if (day > monthLength[month - 1])
            return 0;

        return year * 10000 + month * 100 + day;
    }

    /**
     * Converts a string date ("YYYY-MM-DD") to an integer of the form YYYYMMDD.
     * Bad dates return 0
     *
     * @param {string} dateString
     * @returns {number}
     */
    static SQLtoInt(dateString) {
        dateString = dateString.trim();
        dateString = dateString.replace(/-/g, '/');
        dateString = dateString.replace(/\./g, '/');

        if (!/^\d{4,4}\/\d{1,2}\/\d{1,2}$/.test(dateString))
            return 0;

        var parts = dateString.split("/");
        var day = parseInt(parts[2], 10);
        var month = parseInt(parts[1], 10);
        var year = parseInt(parts[0], 10);

        if (year < 100) {
            var currentYear = new Date().getFullYear();
            var y19 = 1900 + year;
            var y20 = 2000 + year;
            year = Math.abs(currentYear - y19) < Math.abs(currentYear - y20) ? y19 : y20;
        }

        if (year < 1000 || year > 3000 || month < 1 || month > 12 || day < 1)
            return 0;

        var monthLength = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

        // Adjust for leap years
        if (year % 400 === 0 || (year % 100 !== 0 && year % 4 === 0))
            monthLength[1] = 29;

        if (day > monthLength[month - 1])
            return 0;

        return year * 10000 + month * 100 + day;
    }

    /**
     * Verify string date formatted like mM/dD/yyYY
     *
     * @param {string} val
     * @returns {boolean}
     */
    static isStrDate(val) {
        if (val)
            val = $.trim(val);
        if (!val)
            return false;
        var dt = dateutils.strToInt(val);
        return !!dt;
    }

    /**
     * Verify SQL date formatted like YYYY-MM-DD
     *
     * @param {string} val
     * @returns {boolean}
     */
    static isSQLDate(val) {
        if (val)
            val = $.trim(val);
        if (!val)
            return false;
        var dt = dateutils.SQLtoInt(val);
        return !!dt;
    }

    /**
     * Convert numeric YYYYMMDD to string mm/dd/yyyy
     *
     * @param {number} dt
     * @returns {string}
     */
    static intToStr4(dt) {
        if (!dt)
            return '';
        var y = Math.floor(dt / 10000);
        dt -= y * 10000;
        var m = Math.floor(dt / 100);
        var d = Math.floor(dt - m * 100);
        return utils.take(m.toString(), -2) + '/' + utils.zeroPad(d, 2) + '/' + utils.zeroPad(y, 4);
    }

    /**
     * Convert numeric YYYYMMDD to string yyyy-mm-dd
     *
     * @param {number} dt
     * @returns {string}
     */
    static intToSQL(dt) {
        if (!dt)
            return '';
        var y = Math.floor(dt / 10000);
        dt -= y * 10000;
        var m = Math.floor(dt / 100);
        var d = Math.floor(dt - m * 100);
        return utils.format(y, "Z", 4, 0) + '-' + utils.format(m, "Z", 2, 0) + '-' + utils.format(d, "Z", 2, 0);
    }

    /**
     * Convert numeric YYYYMMDD to string mm/dd/yy
     *
     * @param {number} dt
     * @returns {string}
     */
    static intToStr2(dt) {
        if (!dt)
            return '';
        var y = Math.floor(dt / 10000);
        dt -= y * 10000;
        y %= 100;
        var m = Math.floor(dt / 100);
        var d = Math.floor(dt - m * 100);
        return utils.take(m.toString(), -2) + '/' + utils.zeroPad(d, 2) + '/' + utils.zeroPad(y, 2);
    }

    /**
     * Convert a Date object to YYYYMMDD int
     *
     * @param {Date} dt
     * @returns {number}
     */
    static dateToInt(dt) {
        return dt.getFullYear() * 10000 + (dt.getMonth() + 1) * 100 + dt.getDate();
    }

    /**
     * Convert an int date YYYYMMDD to a JavaScrip Date object
     *
     * @param {number} dt
     * @returns {Date}
     */
    static intToDate(dt) {
        var y = Math.floor(dt / 10000);
        dt -= y * 10000;
        var m = Math.floor(dt / 100);
        var d = Math.floor(dt - m * 100);
        return new Date(y, m - 1, d);
    }

    /**
     * Add days to a Date object
     *
     * @param {Date} dt
     * @param {number} days number of days to add or subtract
     * @returns {Date}
     */
    static dateAddDays(dt, days) {
        var result = new Date(dt);
        result.setDate(result.getDate() + days);
        return result;
//    return new Date(((dt.getTime() / 1000 + (60 * 60 * 24 * days)) * 1000));
    }

    /**
     * Add days to an int date
     *
     * @param {number} dt int date YYYYMMDD
     * @param {number} days number of days to add or subtract
     * @returns {int}
     */
    static intAddDays(dt, days) {
        return dateutils.dateToInt(dateutils.dateAddDays(dateutils.intToDate(dt), days));
    }

}




//# sourceURL=kiss/dateutils.js

