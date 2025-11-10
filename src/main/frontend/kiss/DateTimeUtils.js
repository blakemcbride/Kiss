/* global DateUtils, TimeUtils */

/**
 * Created by Blake McBride on 6/16/18.
 */

'use strict';


/**
 * Class to deal with dates and times together.
 *
 * Generally, date/times can be represented in any of the following formats:
 *
 * - Date objects
 * - Epoch timestamps (in milliseconds)
 * - Numeric in the form YYYYMMDDHHMM (ymdhm)
 *
 */
class DateTimeUtils {

    /**
     * Creates an int date/time out of an int date and int time.
     *
     * @param dt YYYYMMDD
     * @param tm HHMM
     * @returns {number} YYYYMMDDHHMM
     */
    static create(dt, tm) {
        return dt * 10000 + tm;
    }

    /**
     * Format a moment as "Wed Jan 4, 2022 12:31 PM CST".
     *
     * @param {Date|number|string} dt – Date object, epoch-millis, YYYYMMDDHHMM, or numeric string
     * @param {string}            [tz] – IANA zone name (e.g. "America/Chicago");
     *                                   omit/falsy → host’s local zone
     * @returns {string}
     */
    static formatDateLong(dt, tz) {
        if (!dt)
            return '';
        /* ---------- normalise input ---------- */
        if (typeof dt === 'string') {
            dt = dt.trim();
            if (!dt)
                return '';
            dt = Number(dt);
        }
        if (typeof dt === 'number')
            dt = this.toDate(dt);
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
     * Converts a given epoch or YYYYMMDDHHMM date/time into a Date object.
     *
     * @param {number|Date} dt - the date to convert
     * @returns {Date} - the converted date
     */
    static toDate(dt) {
        if (dt > 250001010000)
            return new Date(dt);

        const year  = Math.floor(dt / 100000000);
        const month = Math.floor(dt / 1000000) % 100;
        const day   = Math.floor(dt / 10000) % 100;
        const hour  = Math.floor(dt / 100) % 100;
        const minute= dt % 100;

        // Note: JavaScript months are 0-based
        return new Date(year, month - 1, day, hour, minute);
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
        if (typeof dt === 'number')
            dt = this.toDate(dt);
        if (!(dt instanceof Date) || isNaN(dt))
            return '';

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
     * Convert an integer date and integer time into a Date object in the local timezone.
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
     * @param {Date|string|number} dt        – Date object, millis, ymdhm,or numeric string
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
            dt = this.toDate(dt);
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
        if (typeof dt   === 'string')
            dt   = Number(dt);
        if (typeof time === 'string')
            time = Number(time);

        if (isNaN(dt)  || dt <= 0)
            return '';
        if (isNaN(time) || time < 0)
            time = 0;

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
     * @param {Date|number} dt          – Date instance or milliseconds-since-epoch or YYYYMMDDHHMM
     * @param {string}      [zoneId]    – optional IANA time-zone (e.g. "America/New_York");
     *                                    omit or falsy → use the host’s local zone
     * @returns {number}                – integer HHMM, or NaN on bad input
     */
    static toIntTime(dt, zoneId) {
        // ---------- normalise the input ----------
        if (typeof dt === 'number') {
            if (dt > 190000000000) {
                // user supplied YYYYMMDDHHMM
                dt = DateTimeUtils.toDate(dt);
            } else
                dt = new Date(dt);
        }
        if (!(dt instanceof Date) || isNaN(dt))
            return NaN;

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
        if (typeof dt === 'number')
            dt = new Date(dt);
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
            dt = this.toDate(dt);
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
            dt = this.toDate(dt);
        return new Date(dt.getTime() + minutes * 60 * 1000);
    }

    /**
     * Combine a date and time into the number of milliseconds since 1970 UTC.
     * This is very valuable when trying to transmit a DateTime to a backend without losing timezone info.
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

    /**
     * Converts a date (YYYYMMDD) and a time (HHMM) into a compact local date-time
     * representation as YYYYMMDDHHMM (number).
     *
     * @param {number} date - date in YYYYMMDD format
     * @param {number} time - time in HHMM format
     * @returns {number} date-time representation in YYYYMMDDHHMM format
     */
    static newYmdhm(date, time) {
        return (Math.trunc(date) * 10000) + Math.trunc(time);
    }

    /**
     * Convert a Date objects or epoch timestamps (in milliseconds or seconds) into a compact local date-time representation as YYYYMMDDHHMM (number).
     *
     * @param {Date|number} input - date-time to convert, either a Date object or an epoch timestamp
     * @param {string} zoneId - IANA time zone name (e.g. "America/New_York")
     * @returns {number} date-time representation in YYYYMMDDHHMM format
     */
    static toYmdhm(input, zoneId) {
        if (!input)
            return 0;
        if (typeof input === 'number' && input < 250001010000)
            input = this.toEpoch(input, zoneId);
        const zone = this.#normZone(zoneId);
        const ms = input instanceof Date ? input.getTime() : input;
        const comps = this.#epochToZonedComponents(ms, zone);
        return this.#compose(comps);
    }

    /**
     * Converts a date-time to an epoch timestamp in milliseconds (UTC).
     *
     * @param {number} dt - date-time in YYYYMMDDHHMM format or a Date object
     * @param {string} zoneId - IANA time zone name (e.g. "America/New_York")
     * @returns {number} epoch timestamp in milliseconds (UTC)
     */
    static toEpoch(dt, zoneId) {
        if (!dt)
            return 0;
        if (dt instanceof Date)
            dt = this.toYmdhm(dt);
        if (dt > 250001010000)
            return dt;  //  already an epoch
        const zone = this.#normZone(zoneId);
        const { year, month, day, hour, minute } = this.#decompose(dt);

        // Basic field validation
        if (
            month < 1 || month > 12 ||
            day < 1 || day > 31 ||
            hour < 0 || hour > 23 ||
            minute < 0 || minute > 59
        ) {
            throw new Error(`Invalid date/time: ${dt}`);
        }
        return this.#zonedTimeToUtc(year, month, day, hour, minute, zone);
    }

    /**
     * Extracts the date in YYYYMMDD format from a date-time in YYYYMMDDHHMM format.
     *
     * @param {number} dt - date-time in YYYYMMDDHHMM format
     * @returns {number} date in YYYYMMDD format
     */
    static getDate(dt) { return Math.trunc(dt / 10000); }

    /**
     * Extracts the time in HHMM format from a date-time in YYYYMMDDHHMM format.
     *
     * @param {number} dt - date-time in YYYYMMDDHHMM format
     * @returns {number} time in HHMM format
     */
    static getTime(dt) { return Math.trunc(dt % 10000); }

    // ---- Adders (timezone/DST-aware) ----

    /**
     * Adds a specified number of years to a date-time in YYYYMMDDHHMM format
     *
     * @param {number} ymdhm - date-time in YYYYMMDDHHMM format
     * @param {number} years - number of years to add
     * @param {string} zoneId - IANA time zone name (e.g. "America/New_York")
     * @returns {number} date-time after adding years in YYYYMMDDHHMM format
     */
    static addYears(ymdhm, years, zoneId) {
        return this.#add(ymdhm, zoneId, { years, months: 0, days: 0 }, { hours: 0, minutes: 0 });
    }

    /**
     * Adds a specified number of months to a date-time in YYYYMMDDHHMM format
     *
     * @param {number} ymdhm - date-time in YYYYMMDDHHMM format
     * @param {number} months - number of months to add
     * @param {string} zoneId - IANA time zone name (e.g. "America/New_York")
     * @returns {number} date-time after adding months in YYYYMMDDHHMM format
     */
    static addMonths(ymdhm, months, zoneId) {
        return this.#add(ymdhm, zoneId, { years: 0, months,  days: 0 }, { hours: 0, minutes: 0 });
    }

    /**
     * Adds a specified number of days to a date-time in YYYYMMDDHHMM format
     *
     * @param {number} ymdhm - date-time in YYYYMMDDHHMM format
     * @param {number} days - number of days to add
     * @param {string} zoneId - IANA time zone name (e.g. "America/New_York")
     * @returns {number} date-time after adding days in YYYYMMDDHHMM format
     */
    static addDays(ymdhm, days, zoneId) {
        return this.#add(ymdhm, zoneId, { years: 0, months: 0, days }, { hours: 0, minutes: 0 });
    }

    /**
     * Adds a specified number of hours to a date-time in YYYYMMDDHHMM format
     *
     * @param {number} ymdhm - date-time in YYYYMMDDHHMM format
     * @param {number} hours - number of hours to add
     * @param {string} zoneId - IANA time zone name (e.g. "America/New_York")
     * @returns {number} date-time after adding hours in YYYYMMDDHHMM format
     */
    static addHours(ymdhm, hours, zoneId) {
        return this.#add(ymdhm, zoneId, { years: 0, months: 0, days: 0 }, { hours, minutes: 0 });
    }

    /**
     * Adds a specified number of minutes to a date-time in YYYYMMDDHHMM format
     *
     * @param {number} ymdhm - date-time in YYYYMMDDHHMM format
     * @param {number} minutes - number of minutes to add
     * @param {string} zoneId - IANA time zone name (e.g. "America/New_York")
     * @returns {number} date-time after adding minutes in YYYYMMDDHHMM format
     */
    static addMinutes(ymdhm, minutes, zoneId) {
        return this.#add(ymdhm, zoneId, { years: 0, months: 0, days: 0 }, { hours: 0, minutes });
    }

    /**
     * Calculate the difference in minutes between two YYYYMMDDHHMM date-time values in the same time zone.
     *
     * @param {number} fromYmdhm - starting date-time in YYYYMMDDHHMM format
     * @param {number} toYmdhm - ending date-time in YYYYMMDDHHMM format
     * @param {string} zoneId - IANA time zone name (e.g. "America/New_York")
     * @returns {number} difference in minutes between the two date-time values
     */
    static diffMinutes(toYmdhm, fromYmdhm, zoneId) {
        const zone = this.#normZone(zoneId);
        const fromMs = this.toEpoch(fromYmdhm, zone);
        const toMs   = this.toEpoch(toYmdhm,   zone);
        return Math.trunc((toMs - fromMs) / 60_000);
    }

    /**
     * Return the local timezone (IANA string) e.g. "America/New_York".
     *
     * @returns {string}
     */
    static localZone() {
        return Intl.DateTimeFormat().resolvedOptions().timeZone;
    }

    /**
     * Compute the timezone offset in milliseconds for a given epoch timestamp and IANA timezone.
     *
     * @param {number} epochMs - timestamp in milliseconds since 1970 UTC
     * @param {string} timeZone - IANA time zone name (e.g. "America/New_York")
     * @returns {number} the timezone offset in milliseconds
     *
     * This function takes into account both DST transitions and the base UTC offset.
     * The returned value is the difference between the given epoch timestamp and the wall clock time in the specified time zone.
     * This value can be used to convert a timestamp in milliseconds since 1970 UTC to a wall clock time in a given time zone.
     */
    static getTimeZoneOffsetMs(epochMs, timeZone) {
        const d = new Date(epochMs);
        const dtf = new Intl.DateTimeFormat('en-US', {
            timeZone,
            hour12: false,
            year: 'numeric',
            month: '2-digit',
            day: '2-digit',
            hour: '2-digit',
            minute: '2-digit',
            second: '2-digit',
        });

        const parts = Object.fromEntries(dtf.formatToParts(d).map(p => [p.type, p.value]));
        const ms = d.getUTCMilliseconds(); // preserve ms exactly

        const wallAsUtc = Date.UTC(
            Number(parts.year),
            Number(parts.month) - 1,
            Number(parts.day),
            Number(parts.hour),
            Number(parts.minute),
            Number(parts.second),
            ms
        );

        // wallAsUtc ≈ epochMs + offset  →  offset = wallAsUtc - epochMs
        return wallAsUtc - epochMs;
    }

    /**
     * Compute the display epoch for a given epoch timestamp and IANA timezone.
     * The result is the epoch timestamp that, when converted to a wall clock time in the given time zone,
     * yields the same wall clock time as the input epoch timestamp when converted to a wall clock time in the local time zone.
     *
     * @param {number} t - timestamp in milliseconds since 1970 UTC
     * @param {string} realTimezone - IANA time zone name (e.g. "America/New_York")
     * @param {string} [displayTimezone] - IANA time zone name; omit/falsy ⇒ use the host’s local zone
     * @returns {number} the display epoch in milliseconds since 1970 UTC
     *
     * This function takes into account both DST transitions and the base UTC offset.
     * The returned value is the epoch timestamp that, when converted to a wall clock time in the specified time zone,
     * yields the same wall clock time as the input epoch timestamp when converted to a wall clock time in the local time zone.
     */
    static epochToDisplayEpoch(t, realTimezone, displayTimezone = this.localZone()) {
        // If target zone is the same as local, identity is exact across DST:
        if (realTimezone === displayTimezone)
            return t;

        // Desired wall-time value A = t + offset_target(t)
        const offTarget = this.getTimeZoneOffsetMs(t, realTimezone);
        const A = t + offTarget;

        // Solve x + off_local(x) = A  via fixed-point iteration.
        // Start with local offset at t (fast convergence in practice).
        let x = A - this.getTimeZoneOffsetMs(t, displayTimezone);

        for (let i = 0; i < 6; i++) {
            const offLocal = this.getTimeZoneOffsetMs(x, displayTimezone);
            const xNext = A - offLocal;

            if (xNext === x)
                break;                      // exact
            if (Math.abs(xNext - x) < 1) {
                x = Math.round(xNext);
                break;
            } // within 1ms
            x = xNext;
        }
        return x;
    }

    /**
     * Compute the epoch timestamp for a given display epoch and IANA timezone.
     * The result is the epoch timestamp that, when converted to a wall clock time in the specified time zone,
     * yields the same wall clock time as the input display epoch when converted to a wall clock time in the local time zone.
     *
     * @param {number} x - display epoch in milliseconds since 1970 UTC
     * @param {string} realTimezone - IANA time zone name (e.g. "America/New_York")
     * @param {string} [displayTimezone] - IANA time zone name; omit/falsy ⇒ use the host’s local zone
     * @returns {number} the epoch timestamp in milliseconds since 1970 UTC
     *
     * This function takes into account both DST transitions and the base UTC offset.
     * The returned value is the epoch timestamp that, when converted to a wall clock time in the specified time zone,
     * yields the same wall clock time as the input display epoch when converted to a wall clock time in the local time zone.
     */
    static displayEpochToEpoch(x, realTimezone, displayTimezone = this.localZone()) {
        // If target zone is the same as local, identity:
        if (realTimezone === displayTimezone)
            return x;

        // B = x + off_local(x)
        const B = x + this.getTimeZoneOffsetMs(x, displayTimezone);

        // Solve t + off_target(t) = B  ⇒  t = B - off_target(t)
        let t = B - this.getTimeZoneOffsetMs(B, realTimezone);

        for (let i = 0; i < 6; i++) {
            const offTarget = this.getTimeZoneOffsetMs(t, realTimezone);
            const tNext = B - offTarget;

            if (tNext === t)
                break;
            if (Math.abs(tNext - t) < 1) {
                t = Math.round(tNext);
                break;
            }
            t = tNext;
        }
        return t;
    }

    /**
     * Tests the round-trip conversion from epoch timestamp to display epoch and back again
     * for several time zones.
     *
     * This function is primarily used for testing and debugging purposes.
     *
     * For each given epoch timestamp, it will print the original local machine’s display
     * followed by the target time zone’s display, and then the recovered local display
     * after converting the epoch timestamp to the target time zone’s display epoch and back again.
     *
     * @private
     */
    static testUSConversions() {
        const zones = [
            'America/New_York',   // Eastern
            'America/Chicago',    // Central (Tennessee)
            'America/Denver',     // Mountain
            'America/Los_Angeles' // Pacific
        ];
        const mine = this.localZone();

        // Pick any epochs you want; these cover standard/DST/transition windows (2025)
        const tests = [
            Date.UTC(2025, 0, 15, 12, 0, 0, 123),
            Date.UTC(2025, 2, 9, 7, 0, 0, 456),
            Date.UTC(2025, 6, 15, 12, 0, 0, 789),
            Date.UTC(2025, 10, 2, 7, 0, 0, 42)
        ];

        const fmtLocal = (ms) => new Intl.DateTimeFormat('en-US', {
            timeZone: mine, hour12: false,
            year: 'numeric', month: '2-digit', day: '2-digit',
            hour: '2-digit', minute: '2-digit', second: '2-digit'
        }).format(new Date(ms));

        const fmtIn = (ms, tz) => new Intl.DateTimeFormat('en-US', {
            timeZone: tz, hour12: false,
            year: 'numeric', month: '2-digit', day: '2-digit',
            hour: '2-digit', minute: '2-digit', second: '2-digit'
        }).format(new Date(ms));

        console.log('Local zone:', mine);

        tests.forEach((t, idx) => {
            console.log(`\n=== Test #${idx + 1}  (UTC: ${new Date(t).toISOString()}) ===`);
            zones.forEach(zone => {
                // 1) Straight display of the original Date (local machine’s zone)
                const straightLocal = fmtLocal(t);

                // 2) How that same instant appears in the specified target time zone
                const targetView = fmtIn(t, zone);

                // 3) Convert to display-epoch for that zone, convert back, and show the recovered local display
                const disp = this.epochToDisplayEpoch(t, zone);
                const back = this.displayEpochToEpoch(disp, zone);
                const recoveredLocal = fmtLocal(back);
                const exact = back === t ? '✅ exact' : `❌ Δ=${back - t}ms`;

                console.log(`Zone: ${zone}`);
                console.log(`  1) Local (original):   ${straightLocal}`);
                console.log(`  2) Target-zone view:   ${targetView}`);
                console.log(`  3) After converting back (local): ${recoveredLocal}   ${exact}`);
            });
        });
    }

    //------------------------------------

    /**
     * Takes an epoch time and returns an epoch time that only has the date portion (midnight local time)
     *
     * @param {number} epochMillis - the epoch time in milliseconds since Jan 1, 1970
     * @returns {number} the date portion of the epoch time in milliseconds since Jan 1, 1970
     */
    static dateOnlyEpoch(epochMillis) {
        const d = new Date(epochMillis);
        d.setHours(0, 0, 0, 0);
        return d.getTime();
    }

    /**
     * Takes an epoch time and returns an epoch time that only represents the time portion of the epoch time
     * (i.e. milliseconds since midnight local time)
     *
     * @param {number} epochMillis - the epoch time in milliseconds since Jan 1, 1970
     * @returns {number} the time portion of the epoch time in milliseconds since midnight local time
     */
    static timeOnlyEpoch(epochMillis) {
        const d = new Date(epochMillis);
        return (d.getHours() * 3600000) +
            (d.getMinutes() * 60000) +
            (d.getSeconds() * 1000) +
            d.getMilliseconds();
    }

    /**
     * Takes a date (YYYYMMDD) and time (HHMM) and returns the epoch time (milliseconds since Jan 1, 1970)
     * dateInt and timeInt are assumed in the local timezone.
     * If timezone is passed, the epoch time is converted to that timezone.
     *
     * @param {number} dateInt - date in YYYYMMDD format
     * @param {number} timeInt - time in HHMM format
     * @param {string} timezone - optional IANA time zone name (e.g. "America/New_York")
     *
     * @returns {number} epoch time in milliseconds since Jan 1, 1970
     */
    static epochFromDateAndTime(dateInt, timeInt, timezone) {
        const year  = Math.floor(dateInt / 10000);
        let month = Math.floor((dateInt % 10000) / 100) - 1; // JS months are 0-based
        if (month < 0)
            month = 0;
        const day   = dateInt % 100;

        const hours = Math.floor(timeInt / 100);
        const mins  = timeInt % 100;

        const d = new Date(year, month, day, hours, mins, 0, 0);
        let et = d.getTime();
        if (timezone)
            et = this.displayEpochToEpoch(et, timezone);
        return et;
    }

    // =================
    // Private helpers
    // =================

    /**
     * Normalizes a provided timezone string.
     *
     * This helper ensures that a valid IANA timezone identifier is always returned.
     * If the given `tz` value is `null`, `undefined`, empty, or only whitespace,
     * the system's default timezone is returned instead.
     *
     * @param tz
     * @returns {string}
     */
    static #normZone(tz) {
        return (tz ?? "").toString().trim() || Intl.DateTimeFormat().resolvedOptions().timeZone;
    }

    static #decompose(ymdhm) {
        // Accept number or string; produce zero-padded 12 chars
        const s = (typeof ymdhm === "number" ? String(Math.trunc(ymdhm)) : String(ymdhm)).padStart(12, "0");
        return {
            year:   Number(s.slice(0, 4)),
            month:  Number(s.slice(4, 6)),
            day:    Number(s.slice(6, 8)),
            hour:   Number(s.slice(8, 10)),
            minute: Number(s.slice(10, 12)),
        };
    }

    static #compose({ year, month, day, hour, minute }) {
        return (year * 100000000) + (month * 1000000) + (day * 10000) + (hour * 100) + minute;
    }

    // Resolve zone offset (ms) for a given UTC Date in a specific IANA timeZone.
    static #getOffsetMs(utcDate, timeZone) {
        const dtf = new Intl.DateTimeFormat("en-US", {
            timeZone, hour12: false,
            year: "numeric", month: "2-digit", day: "2-digit",
            hour: "2-digit", minute: "2-digit", second: "2-digit",
        });
        const parts = dtf.formatToParts(utcDate);
        const map = Object.fromEntries(parts.map(p => [p.type, p.value]));
        const y = Number(map.year);
        const m = Number(map.month);
        const d = Number(map.day);
        const H = Number(map.hour);
        const M = Number(map.minute);
        const S = Number(map.second);
        const asIfUTC = Date.UTC(y, m - 1, d, H, M, S);
        return asIfUTC - utcDate.getTime();
    }

    // Convert a zone-local wall time to epoch ms (UTC), DST-safe.
    static #zonedTimeToUtc(year, month, day, hour, minute, timeZone) {
        // Initial guess: interpret wall time *as if* it were UTC
        const guess = Date.UTC(year, month - 1, day, hour, minute, 0, 0);
        const off1 = this.#getOffsetMs(new Date(guess), timeZone);
        const candidate = guess - off1;
        const off2 = this.#getOffsetMs(new Date(candidate), timeZone);
        // If offset changes after moving to candidate (gap/overlap), adjust once
        return off2 !== off1 ? (guess - off2) : candidate;
    }

    // Convert epoch ms → zone-local components.
    static #epochToZonedComponents(epochMs, timeZone) {
        const dtf = new Intl.DateTimeFormat("en-US", {
            timeZone, hour12: false,
            year: "numeric", month: "2-digit", day: "2-digit",
            hour: "2-digit", minute: "2-digit",
        });
        const parts = dtf.formatToParts(new Date(epochMs));
        const map = Object.fromEntries(parts.map(p => [p.type, p.value]));
        return {
            year: Number(map.year),
            month: Number(map.month),
            day: Number(map.day),
            hour: Number(map.hour),
            minute: Number(map.minute),
        };
    }

    static #daysInMonth(year, month) {
        return new Date(Date.UTC(year, month, 0)).getUTCDate(); // month: 1..12
    }

    // Calendar (years/months/days) + duration (hours/minutes) add, zone-aware/DST-safe.
    static #add(ymdhm, zoneId, dateDelta, timeDelta) {
        const zone = this.#normZone(zoneId);
        let { year, month, day, hour, minute } = this.#decompose(ymdhm);

        // 1) Years + Months (in local calendar)
        const monthsTotal = (year * 12 + (month - 1)) + (dateDelta.years ?? 0) * 12 + (dateDelta.months ?? 0);
        year  = Math.floor(monthsTotal / 12);
        month = (monthsTotal % 12) + 1;

        // Clamp day to end-of-month
        const maxDay = this.#daysInMonth(year, month);
        if (day > maxDay)
            day = maxDay;

        // 2) Days (calendar roll)
        let d = new Date(Date.UTC(year, month - 1, day, hour, minute, 0, 0));
        if (dateDelta.days)
            d.setUTCDate(d.getUTCDate() + (dateDelta.days ?? 0));

        year   = d.getUTCFullYear();
        month  = d.getUTCMonth() + 1;
        day    = d.getUTCDate();
        hour   = d.getUTCHours();
        minute = d.getUTCMinutes();

        // 3) Hours + Minutes as timeline duration across DST
        const addMinTotal = (timeDelta.hours ?? 0) * 60 + (timeDelta.minutes ?? 0);
        if (addMinTotal !== 0) {
            const baseEpoch   = this.#zonedTimeToUtc(year, month, day, hour, minute, zone);
            const resultEpoch = baseEpoch + addMinTotal * 60_000;
            const comps       = this.#epochToZonedComponents(resultEpoch, zone);
            return this.#compose(comps);
        }

        // No duration change: return resulting wall time
        return this.#compose({ year, month, day, hour, minute });
    }
}


