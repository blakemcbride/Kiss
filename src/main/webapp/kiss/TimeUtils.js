/* global Utils */

/**
 * Created by Blake McBride on 6/18/18.
 */

'use strict';

/**
 * Time utilities.
 *
 */
class TimeUtils {

    /**
     * Format time.
     *
     * @param  val integer HHMM or Date object
     * @param {boolean} zero_fill
     * @returns {string}
     */
    static format(val, zero_fill) {
        if (val === null || val === undefined || val === ''  ||  val < 0)
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

        // hours
        const ndh = sval.length === 3 ? 1 : 2;
        for (; i < sval.length; i++) {
            let c = sval.charAt(i);
            if (!isDigit(c) || i >= ndh)
                break;
            buf += c;
        }
        let hours = buf ? parseInt(buf) : 0;

        for (; i < sval.length; i++) {
            let c = sval.charAt(i);
            if (isDigit(c))
                break;
        }

        let minutes;
        if (i < sval.length  &&  isDigit(sval.charAt(i))) {
            buf = '';
            for (let n=0 ; i < sval.length; i++, n++) {
                let c = sval.charAt(i);
                if (!isDigit(c) || n >= 2)
                    break;
                buf += c;
            }
            minutes = parseInt(buf);
        } else
            minutes = 0;

        for (; i < sval.length  &&  sval.charAt(i) === ' '; i++) ;

        let part;
        if (i >= sval.length)
            part = null;
        else {
            const c = sval.charAt(i);
            if (c === 'a'  ||  c === 'A')
                part = 'A';
            else if (c === 'p'  ||  c === 'P')
                part = 'P';
            else
                part = null;
        }

        if (part === 'A'  &&  hours === 12)
            hours -= 12;
        else if (part === 'P')
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
     * @param time numeric or string time
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

}

