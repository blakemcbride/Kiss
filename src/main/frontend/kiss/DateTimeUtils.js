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
     * Format a moment as "Wed Jan 4, 2022 12:31 PM CST".
     *
     * @param {Date|number|string} dt – Date object, epoch-millis, or numeric string
     * @param {string}            [tz] – IANA zone name (e.g. "America/Chicago");
     *                                   omit/falsy → host’s local zone
     * @returns {string}
     */
    static formatDateLong(dt, tz) {
        /* ---------- normalise input ---------- */
        if (typeof dt === 'string') {
            dt = dt.trim();
            if (!dt)
                return '';
            dt = Number(dt);
        }
        if (typeof dt === 'number')
            dt = new Date(dt);
        if (!(dt instanceof Date) || isNaN(dt))
            return '';

        /* ---------- obtain the pieces we need ---------- */
        const opts = {
            timeZone     : tz || undefined,   // undefined ⇒ keep local zone
            weekday      : 'short',           // Wed
            month        : 'short',           // Jan
            day          : 'numeric',         // 4
            year         : 'numeric',         // 2022
            hour         : 'numeric',         // 12  (no leading zero)
            minute       : '2-digit',         // 31
            hour12       : true,              // AM/PM
            timeZoneName : 'short'            // CST
        };

        const parts = new Intl.DateTimeFormat('en-US', opts).formatToParts(dt);
        const get   = type => parts.find(p => p.type === type)?.value || '';

        /* ---------- assemble in the desired order ---------- */
        return `${get('weekday')} ${get('month')} ${get('day')}, ${get('year')} `
            + `${get('hour')}:${get('minute')} ${get('dayPeriod')} ${get('timeZoneName')}`;
    }

    /**
     * Convert a Date into the canonical interchange string
     *   yyyy-MM-dd HH:mm:ss <offset-in-minutes>
     *
     * @param {Date|number|string} dt   – Date object, millis, or numeric string
     * @param {string}            [tz]  – IANA zone name (e.g. "America/New_York");
     *                                    omit/falsy ⇒ use the host’s local zone
     * @returns {string}
     */
    static dateToStd(dt, tz) {
        /* ---------- normalise input ---------- */
        if (typeof dt === 'string') {
            dt = dt.trim();
            if (!dt) return '';
            dt = Number(dt);
        }
        if (typeof dt === 'number') dt = new Date(dt);
        if (!(dt instanceof Date) || isNaN(dt)) return '';

        /* ---------- Y-M-D H:m:s parts in the target zone ---------- */
        const fmt = new Intl.DateTimeFormat('en-CA', {
            timeZone : tz || undefined,   // undefined ⇒ local zone
            year     : 'numeric',
            month    : '2-digit',
            day      : '2-digit',
            hour     : '2-digit',
            minute   : '2-digit',
            second   : '2-digit',
            hour12   : false
        }).formatToParts(dt);

        const pick = t => fmt.find(p => p.type === t).value;
        const YYYY = pick('year'),
            MM   = pick('month'),
            DD   = pick('day'),
            HH   = pick('hour'),
            mm   = pick('minute'),
            ss   = pick('second');

        /* ---------- offset in **integer** minutes ---------- */
        let offsetMinutes;

        if (!tz) {
            offsetMinutes = dt.getTimezoneOffset();           // already an int
        } else {
            const fakeUTC = Date.UTC(
                Number(YYYY), Number(MM) - 1, Number(DD),
                Number(HH),  Number(mm),     Number(ss)
            );
            // Round to ensure we never emit a fraction like 240.0000001
            offsetMinutes = Math.round((dt.getTime() - fakeUTC) / 60000);
        }

        /* ---------- final string ---------- */
        return `${YYYY}-${MM}-${DD} ${HH}:${mm}:${ss} ${offsetMinutes}`;
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
        const y = Number(sdt.slice(0, 4));
        const month = Number(sdt.slice(5, 7));
        const d = Number(sdt.slice(8, 10));
        const h = Number(sdt.slice(11, 13));
        const minutes = Number(sdt.slice(14, 16));
        const s = Number(sdt.slice(17, 19));
        const tz1 = Number(sdt.slice(20)); // replaces Utils.drop(sdt, 20)
        const dt = new Date(y, month - 1, d, h, minutes, s);
        const tz2 = dt.getTimezoneOffset();
        return new Date(dt.valueOf() - tz2 * 60000 + tz1 * 60000);
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
     * Format a date (YYYYMMDD) and a time (HHMM) as
     *   "mm/dd/yyyy hh:mm AM/PM"  or  "dd/mm/yyyy hh:mm AM/PM"
     *
     * @param {number|string} dt     – calendar date, e.g. 20250508
     * @param {number|string} time   – clock time,   e.g. 1145
     * @param {string}        [tz]   – IANA zone name; omit/falsy ⇒ host’s zone
     * @returns {string}
     */
    static formatDateTime(dt, time, tz) {

        /* ---------- trivial short-circuit ---------- */
        if (!dt && (time === undefined || time === null || time === ''))
            return '';

        /* ---------- normalise inputs ---------- */
        if (typeof dt   === 'string') dt   = Number(dt);
        if (typeof time === 'string') time = Number(time);

        if (isNaN(dt)  || dt <= 0)   return '';
        if (isNaN(time) || time < 0) time = 0;

        /* split YYYYMMDD */
        const y  = Math.floor(dt / 10000);
        const m  = Math.floor((dt % 10000) / 100) - 1;   // JS month 0–11
        const d  = dt % 100;

        /* split HHMM */
        const h  = Math.floor(time / 100);
        const mi = time % 100;

        /* ---------- build Date in the **local** zone ---------- */
        const date = new Date(y, m, d, h, mi);

        /* ---------- format in the target (or local) zone ---------- */
        const parts = new Intl.DateTimeFormat(undefined, {
            timeZone : tz || undefined,   // undefined ⇒ keep host’s zone
            year     : 'numeric',
            month    : 'numeric',
            day      : 'numeric',
            hour     : 'numeric',
            minute   : '2-digit',
            hour12   : true
        }).formatToParts(date);

        const get = t => parts.find(p => p.type === t)?.value;
        const MM  = get('month');
        const DD  = get('day');
        const YYYY= get('year');
        const HH  = get('hour');
        const MMm = get('minute');
        const AP  = get('dayPeriod');

        /* honour the user’s MM/DD vs DD/MM preference */
        const mdy  = DateUtils.detectDateFormat?.() === 'MM/DD/YYYY';
        const dateStr = mdy ? `${MM}/${DD}/${YYYY}` : `${DD}/${MM}/${YYYY}`;

        return `${dateStr} ${HH}:${MMm} ${AP}`;
    }

    /**
     * Convert a moment in time to an integer clock value (HHMM).
     * The date portion is ignored.
     *
     * @param {Date|number} dt          – Date instance **or** milliseconds-since-epoch
     * @param {string}      [zoneId]    – optional IANA time-zone (e.g. "America/New_York");
     *                                    omit or falsy → use the host’s local zone
     * @returns {number}                – integer HHMM, or NaN on bad input
     */
    static toIntTime(dt, zoneId) {
        // ---------- normalise the input ----------
        if (typeof dt === 'number') dt = new Date(dt);
        if (!(dt instanceof Date) || isNaN(dt)) return NaN;

        let hours, minutes;

        if (zoneId) {
            // Use Intl to obtain the clock reading in the requested zone
            const parts = new Intl.DateTimeFormat('en-US', {
                timeZone : zoneId,
                hour     : '2-digit',
                minute   : '2-digit',
                hour12   : false          // 00-23
            }).formatToParts(dt);

            hours   = Number(parts.find(p => p.type === 'hour').value);
            minutes = Number(parts.find(p => p.type === 'minute').value);
        } else {
            // Original behaviour: host computer’s local zone
            hours   = dt.getHours();
            minutes = dt.getMinutes();
        }

        return hours * 100 + minutes;     // e.g. 14 h 07 m → 1407
    }

    /**
     * Extract the calendar date as an integer (YYYYMMDD).
     *
     * @param {Date|number} dt        – Date object *or* milliseconds since epoch.
     * @param {string}      [zoneId]  – optional IANA time-zone, e.g. "America/New_York".
     *                                  Omit/falsy ⇒ use the host’s local zone.
     * @returns {number}              – YYYYMMDD, or NaN on invalid input.
     */
    static toIntDate(dt, zoneId) {
        // ---- normalise the first argument ----
        if (typeof dt === 'number') dt = new Date(dt);
        if (!(dt instanceof Date) || isNaN(dt)) return NaN;

        let year, month, day;

        if (zoneId) {
            // Use Intl to get y-m-d as seen in the requested zone
            const parts = new Intl.DateTimeFormat('en-CA', { // fixed ISO-like order
                timeZone : zoneId,
                year  : 'numeric',
                month : '2-digit',
                day   : '2-digit'
            }).formatToParts(dt);

            const pick = t => parts.find(p => p.type === t).value;
            year  = Number(pick('year'));
            month = Number(pick('month'));
            day   = Number(pick('day'));
        } else {
            // Original behaviour: local zone
            year  = dt.getFullYear();
            month = dt.getMonth() + 1;   // JS months are 0-based
            day   = dt.getDate();
        }

        // Convert to YYYYMMDD integer
        return year * 10000 + month * 100 + day;
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


