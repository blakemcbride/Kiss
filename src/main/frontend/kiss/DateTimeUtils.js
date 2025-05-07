/* global DateUtils, TimeUtils */

/**
 * Created by Blake McBride on 6/16/18.
 */

'use strict';


/**
 * Class to deal with dates and times together.
 *
 */
class DateTimeUtils {

    /**
     * Format a Date in a full format.  For example:  Wed Jan 4, 2022 12:31 PM CST
     * <br><br>
     * <code>dt</code> can be a <code>Date</code> object or the number of milliseconds since 1970.
     *
     * @param dt {Date|number}
     * @returns {string}
     */
    static formatDateLong(dt) {
        if (!dt)
            return '';
        if (typeof dt === 'string')
            dt = Number(dt);
        if (typeof dt === 'number')
            dt = new Date(dt);
        if (typeof dt !== 'object')
            return '';
        const idt = DateUtils.dateToInt(dt);
        return Utils.take(DateUtils.dayOfWeekName(idt), 3) + ' ' +
            Utils.take(DateUtils.monthName(idt), 3) + ' ' +
            DateUtils.day(idt) + ', ' +
            DateUtils.year(idt) + ' ' +
            TimeUtils.formatLong(dt);
    }

    /**
     * Format a Date into a standard format useful for data interchange.
     * For example:  2022-06-08 03:27:44 360
     * The 360 is minutes offset from GMT - the timezone
     *
     * @param dt {Date}
     * @returns {string}
     */
    static dateToStd(dt) {
        if (typeof dt !== 'object')
            return '';
        const idt = DateUtils.dateToInt(dt);
        const itm = DateTimeUtils.dateToIntTime(dt);
        const fmt2 = n => Utils.format(n, "Z", 2, 0);
        return DateUtils.year(idt) + '-' + fmt2(DateUtils.month(idt)) + '-' + fmt2(DateUtils.day(idt)) + ' ' +
            fmt2(TimeUtils.hours(itm)) + ':' + fmt2(TimeUtils.minutes(itm)) + ':' + fmt2(dt.getSeconds()) + ' ' +
            dt.getTimezoneOffset();
    }

    /**
     * Parse a standard string date format into a Date object.
     * Expected standard date format looks like this:  2022-06-08 03:27:44 300
     * The 300 is minutes offset from GMT - the timezone
     * This routine parses a date from any timezone and returns a Date object in the local timezone.
     *
     * @param sdt {string}
     * @returns {Date}
     */
    static stdToDate(sdt) {
        if (typeof sdt !== 'string' || !sdt)
            return null;
        if (!/^\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2} -?\d{1,3}$/.test(sdt))
            return null;
        const y = Number(sdt.substr(0, 4));
        const month = Number(sdt.substr(5, 2));
        const d = Number(sdt.substr(8, 2));
        const h = Number(sdt.substr(11, 2));
        const minutes = Number(sdt.substr(14, 2));
        const s = Number(sdt.substr(17, 2));
        const tz1 = Number(Utils.drop(sdt, 20));
        const dt = new Date(y, month-1, d, h, minutes, s);
        const tz2 = dt.getTimezoneOffset();
        return new Date(dt.valueOf() - tz2 * 1000 * 60 + tz1 * 1000 * 60);
    }

    /**
     * Convert in integer date and integer time into a Date object.
     *
     * @param dt YYYYMMDD
     * @param tm  HHMM
     * @returns {Date}
     */
    static createDate(dt, tm) {
        if (!dt)
            return null;
        const y = Math.floor(dt / 10000);
        dt -= y * 10000;
        const m = Math.floor(dt / 100);
        const d = Math.floor(dt - m * 100);
        const h = Math.floor(tm / 100);
        const min = tm - Math.floor(tm / 100) * 100;
        return new Date(y, m - 1, d, h, min);
    }

    /**
     * Format a Date or milliseconds-since-epoch value to a string
     * "mm/dd/yyyy hh:mm AM/PM"  or  "dd/mm/yyyy hh:mm AM/PM"
     *
     * @param {Date|string|number} dt        – Date object, millis, or numeric string
     * @param {string}             [tz]      – IANA zone (e.g. "America/New_York");
     *                                         omit or pass falsy to use the local zone
     * @returns {string}
     */
    static formatDate(dt, tz) {
        // ----- normalise the first argument -----
        if (typeof dt === 'string') {
            dt = dt.trim();
            if (!dt || dt === '0')
                return '';
            dt = Number(dt);
        }
        if (typeof dt === 'number') {            // millis
            if (!dt)
                return '';
            dt = new Date(dt);
        }
        if (!(dt instanceof Date) || isNaN(dt))
            return '';

        // ----- build the parts for the requested (or local) time-zone -----
        const parts = new Intl.DateTimeFormat(
            undefined,                           // keep user’s locale
            {
                timeZone : tz || undefined,      // undefined → local zone
                year     : 'numeric',
                month    : 'numeric',
                day      : 'numeric',
                hour     : 'numeric',            // already 1-12 in hour12 mode
                minute   : '2-digit',
                hour12   : true
            }
        ).formatToParts(dt);

        // quick helper to pull the values we need
        const pick = type => parts.find(p => p.type === type)?.value;

        const month   = pick('month');   // "1" … "12"
        const day     = pick('day');
        const year    = pick('year');
        const hour    = pick('hour');    // "1" … "12"
        const minute  = pick('minute');  // always 2-digit because of '2-digit'
        const ampm    = pick('dayPeriod'); // "AM" / "PM"

        // ----- honour your existing MM/DD vs DD/MM preference -----
        const mdy = DateUtils.detectDateFormat() === 'MM/DD/YYYY';
        const dateStr = mdy
            ? `${month}/${day}/${year}`
            : `${day}/${month}/${year}`;

        return `${dateStr} ${hour}:${minute} ${ampm}`;
    }


    /**
     * Format a date and time into a single string.
     *
     * @param {number} dt    YYYYMMDD
     * @param {number} time  HHMM
     * @returns {string}  mm/dd/yyyy hh:mm AM/PM
     */
    static formatDateTime(dt, time) {
        if (!dt && (time === undefined || time === null || time === ''))
            return '';
        const res = DateUtils.intToStr4(dt);
        const tf = TimeUtils.format(time);
        if (res && tf)
            return res + ' ' + tf;
        else
            return res + tf;
    }

    /**
     * Convert a Date object into in integer time.
     * Ignores / removes the date component.
     *
     * @param {Date} dt
     * @returns {number} HHMM
     */
    static dateToIntTime(dt) {
        const hours = dt.getHours();
        const minutes = dt.getMinutes();
        return hours * 100 + minutes;
    }

    /**
     * Returns the local long timezone text.
     * For example "American/Chicago"
     *
     * @returns {string}
     */
    static getLocalTimezoneLongText() {
        return Intl.DateTimeFormat().resolvedOptions().timeZone;
    }

    /**
     * Returns the local short timezone text.
     * For example "CST"
     *
     * @returns {string}
     */
    static getLocalTimezoneShortText() {
        return new Date().toLocaleTimeString('en-us',{timeZoneName:'short'}).split(' ')[2];
    }

    /**
     * Add hours to a date.
     *
     * @param dt {Date|number|string}
     * @param hours {number}
     * @returns {Date}
     */
    static addHours(dt, hours) {
        if (typeof dt === 'string')
            dt = Number(dt);
        if (typeof dt === 'number')
            dt = new Date(dt);
        return new Date(dt.getTime() + hours * 60 * 60 * 1000);
    }

    /**
     * Add minutes to a date.
     *
     * @param dt {Date|number|string}
     * @param minutes {number}
     * @returns {Date}
     */
    static addMinutes(dt, minutes) {
        if (typeof dt === 'string')
            dt = Number(dt);
        if (typeof dt === 'number')
            dt = new Date(dt);
        return new Date(dt.getTime() + minutes * 60 * 1000);
    }

    /**
     * Combine a date and time into the number of milliseconds since 1970 UTC.
     * This is very valuable when trying to transit a DateTime to a backend without losing timezone info.
     *
     * @param date {number|Date} YYYYMMDD (time portion of a Date is not used)
     * @param time {number|null|undefined} HHMM
     * @returns {number}
     *
     * @see DateUtils.millsToInt()
     * @see TimeUtils.millsToInt()
     */
    static toMilliseconds(date, time) {
        let month = DateUtils.month(date)-1;
        if (month < 0)
            month = 0;
        const dt = new Date(DateUtils.year(date), month, DateUtils.day(date), TimeUtils.hours(time), TimeUtils.minutes(time));
        const n = dt.valueOf();
        return n < 0 ? 0 : n;
    }
}


