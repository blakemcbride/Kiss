/* global Utils */

/**
 * Created by Blake McBride on 6/18/18.
 */

'use strict';

/**
 * Time utilities.
 *
 * Utilities to deal (mostly) with times stored as a number in the form HHMM since midnight.
 * Also, some handling of Date objects.
 *
 */
class TimeUtils {

    /**
     * Returns the current time in HHMM format.
     *
     * @returns {number} HHMM
     */
    static current() {
        const dt = new Date();
        return dt.getHours() * 100 + dt.getMinutes();
    }

    /**
     * Format time with timezone.
     *
     * @param dt {Date}
     * @returns {string} hh:mm XM XST
     */
    static formatLong(dt) {
        if (typeof dt !== 'object')
            return '';
        const idt = TimeUtils.dateToTime(dt);
        return TimeUtils.format(idt) + ' ' + dt.toLocaleTimeString('en-us',{timeZoneName:'short'}).split(' ')[2];
    }

    /**
     * Format time.
     *
     * @param  {Date|number} val integer HHMM or Date object
     * @param {boolean} zero_fill
     * @returns {string} hh:mm XM
     */
    static format(val, zero_fill=false) {
        if (typeof val === 'string')
            val = Number(val);
        if (val === null || val === undefined || isNaN(val)  ||  val < 0)
            return '';
        if (typeof val === 'object')
            val = DateTimeUtils.dateToIntTime(val);
        const hours = Math.floor(val / 100);
        const minutes = val % 100;
        let width;
        let msk;
        if (zero_fill) {
            width = 2;
            msk = 'Z';
        } else {
            width = 0;
            msk = '';
        }
        if (hours === 0)
            return Utils.format(hours + 12, msk, width, 0) + ':' + Utils.format(minutes, 'Z', 2, 0) + ' AM';
        if (hours >= 13)
            return Utils.format(hours - 12, msk, width, 0) + ':' + Utils.format(minutes, 'Z', 2, 0) + ' PM';
        else
            return Utils.format(hours, msk, width, 0) + ':' + Utils.format(minutes, 'Z', 2, 0) + (hours === 12 ? ' PM' : ' AM');
    }

    /**
     * Convert a string time into an integer time.
     *
     * @param sval {string}  "11:30 AM", "1130", "11:30pm", "11.30", accepts military time too, etc.
     * @returns {null|number}   integer formatted as HHMM or null if not a time
     */
    static strToInt(sval) {
        if (!sval)
            return null;
        sval = sval.trim();
        if (!sval)
            return null;
        const isDigit = function (c) {
            return c >= '0'  &&  c <= '9';
        };
        let buf = '';
        let i = 0;
        let c;

        let ndigits;
        for (ndigits=0 ; ndigits < sval.length && isDigit(sval[ndigits]) ; ndigits++);

        // hours
        let ndh;
        switch (ndigits) {
            case 1:  ndh = 1;  break;
            case 2:  ndh = 2;  break;
            case 3:  ndh = 1;  break;
            default: ndh = 2;  break;
        }
        for (; i < ndh ; i++) {
            c = sval.charAt(i);
            buf += c;
        }
        let hours = buf ? parseInt(buf) : 0;

        // skip : and space
        for (; i < sval.length; i++) {
            c = sval.charAt(i);
            if (isDigit(c)  ||  c === 'a' || c === 'A' || c === 'p' || c === 'P')
                break;
        }

        let minutes = 0;
        if (c !== 'a' && c !== 'A' && c !== 'p' && c !== 'P') {
            if (i < sval.length && isDigit(sval.charAt(i))) {
                buf = '';
                for (let n = 0; i < sval.length; i++, n++) {
                    c = sval.charAt(i);
                    if (!isDigit(c) || n >= 2)
                        break;
                    buf += c;
                }
                minutes = parseInt(buf);
            }

            for (; i < sval.length && sval.charAt(i) === ' '; i++) ;
        }

        let part;
        if (i >= sval.length)
            part = null;
        else {
            c = sval.charAt(i);
            if (c === 'a'  ||  c === 'A')
                part = 'A';
            else if (c === 'p'  ||  c === 'P')
                part = 'P';
            else
                part = null;
        }

        if (!part)
            if (hours >= 6 && hours < 12)
                part = 'A';
            else if (hours < 13)
                part = 'P';

        if (part === 'A'  &&  hours === 12)
            hours -= 12;
        else if (part === 'P' && hours !== 12)
            hours += 12;
        return hours >= 0 && hours < 24 && minutes >= 0 && minutes < 60 ? hours * 100 + minutes : null;
    };

    /**
     * Is argument a valid numeric or string time?
     *
     * Note:  This function should probably not be called if you are going to follow it up with a call to
     * strToInt().  The reason is that this function calls that function.  You would be calling it twice.
     * Just call strToInt() and compare it to null.
     *
     * @param time {number|string} numeric or string time
     * @returns {boolean}
     */
    static isValid(time) {
        if (isNaN(time) || time === null  ||  time === ''  ||  time === undefined)
            return false;
        if (typeof time === 'string')
            return TimeUtils.strToInt(time) !== null;
        if (typeof time !== 'number')
            return false;
        const hours = Math.floor(time / 100);
        const minutes = Math.floor(time - hours * 100);
        if (time !== hours * 100 + minutes)
            return false;
        return hours < 24 && hours >= 0 && minutes < 60 && minutes >= 0;
    }

    /**
     * Returns the hours portion of a time in an integer representation formatted HHMM
     *
     * @param time {number} HHMM
     * @returns {number}
     */
    static hours(time) {
        if (typeof time !== 'number')
            return 0;
        return Math.floor(time / 100);
    }

    /**
     * Returns the minutes portion of a time in an integer representation formatted HHMM
     *
     * @param time {number} HHMM
     * @returns {number}
     */
    static minutes(time) {
        if (typeof time !== 'number')
            return 0;
        return time - Math.floor(time / 100) * 100;
    }

    /**
     * Convert a number of milliseconds since 1970 UTC to an integer time HHMM.
     * This takes into account the local timezone.
     *
     * @param m {number} number of milliseconds since 1970 UTC
     * @returns {number} HHMM
     *
     * @see DateTimeUtils.toMilliseconds(), DateUtils.millsToInt()
     */
    static millsToInt(m) {
        if (!m)
            return 0;
        const dt = new Date(m);
        return TimeUtils.dateToTime(dt);
    }

    /**
     * Returns the total minutes in a time.
     *
     * @param time {number} HHMM
     * @returns {null|number}
     */
    static totalMinutes(time) {
        if (typeof time !== 'number')
            return null;
        return TimeUtils.hours(time) * 60 + TimeUtils.minutes(time);
    }

    /**
     * Returns the total minutes between two times.
     *
     * @param t1 {number} HHMM
     * @param t2 {number} HHMM
     * @returns {null|number}
     */
    static diff(t1, t2) {
        if (typeof t1 !== 'number' || typeof t2 !== 'number')
            return null;
        return TimeUtils.totalMinutes(t1) - TimeUtils.totalMinutes(t2);
    }

    /**
     * Convert a number of minutes into standard time format (HHMM).
     *
     * @param m {number} number of minutes
     * @returns {number} HHMM
     */
    static minutesToTime(m) {
        if (typeof m !== 'number')
            return null;
        const hours = Math.floor(m / 60);
        const minutes = m - hours * 60;
        return hours * 100 + minutes;
    }

    /**
     * Convert a Date object into a numeric time HHMM
     * dropping the date portion.
     *
     * @param dt {Date}
     * @returns {number} HHMM
     */
    static dateToTime(dt) {
        if (typeof dt !== 'object')
            return 0;
        return dt.getHours() * 100 + dt.getMinutes();
    }

}

