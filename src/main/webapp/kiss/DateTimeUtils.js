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
     *
     * @param dt {Date}
     * @returns {string}
     */
    static formatDateLong(dt) {
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
     * Format a Date or number of milliseconds since 1970 UTC to a string representation looking like mm/dd/yyyy hh:mm
     *
     * @param {Date|number} dt
     * @returns {string}
     */
    static formatDate(dt) {
        if (!dt)
            return '';
        if (typeof dt === 'number')
            dt = new Date(dt);
        let hours = dt.getHours();
        let sf;
        if (hours >= 12)
            sf = ' PM';
        else
            sf = ' AM';
        if (hours > 12)
            hours -= 12;
        if (!hours)
            hours = 12;
        let min = dt.getMinutes();
        if (min < 10)
            min = '0' + min.toString();
        else
            min = min.toString();
        return (dt.getMonth() + 1).toString() + '/' + dt.getDate().toString() + '/' + dt.getFullYear().toString() + ' ' + hours.toString() + ':' + min + sf;
    }

    /**
     * Format a date and time into a single string.
     *
     * @param {number} dt    YYYYMMDD
     * @param {number} time  HHMM
     * @returns {string}  mm/dd/yyyy hh:mm
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
     * Convert a Date object into in integer time
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
     * Combine a date and time into the number of milliseconds since 1970 UTC.
     * This is very valuable when trying to transit a DateTime to a backend without losing timezone info.
     *
     * @param date {number|Date} YYYYMMDD (time portion of a Date is not used)
     * @param time {number|null|undefined} HHMM
     * @returns {number}
     *
     * @see DateUtils.millsToInt(), TimeUtils.millsToInt()
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


