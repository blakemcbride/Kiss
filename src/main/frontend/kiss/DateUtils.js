/* global Utils */

/**
 * Created by Blake McBride on 3/15/18.
 */

'use strict';

/**
 DateUtils manipulates local (without timezones) dates stored a number in the YYYYMMDD form.

 Function names are meant to tell what the input and output date types they work on.
 In terms of understanding the data types used to represent dates:

 int          YYYYMMDD   e.g., 20180322
 str          "3/22/[20]18" (or similar international format)
 str2         "3/22/18" (or similar international format)
 str4         "3/22/2018" (or similar international format)
 SQL          "2018-03-22"
 Date         JavaScript Date object
 */
class DateUtils {

    /**
     * Detects the date format based on the given locale.
     *
     * @param {string} [locale=navigator.language] - optional locale used to determine the date format.
     * @return {string} The detected date format. Possible values are 'MM/DD/YYYY', 'DD/MM/YYYY', or 'Unknown Format'.
     */
    static detectDateFormat(locale = navigator.language) {
        if (!this.intFormat || this.locale !== locale) {
            this.locale = locale;
            const testDate = new Date(2023, 0, 21); // 21st January 2023
            const formattedDate = new Intl.DateTimeFormat(locale, {
                year: 'numeric',
                month: '2-digit',
                day: '2-digit'
            }).format(testDate);

            const parts = formattedDate.match(/(\d{2})/g);
            if (parts.length > 2) {
                if (parts[0] === '01') {
                    this.intFormat = 'MM/DD/YYYY';
                } else if (parts[1] === '01') {
                    this.intFormat = 'DD/MM/YYYY';
                }
            }
            if (!this.intFormat)
                this.intFormat = 'Unknown Format';
        }
        return this.intFormat;
    }

    /**
     * Converts a string in any of the following formats to an int YYYYMMDD:
     *     "mM/dD/yyYY" or "dD/mM/yyYY"  (depending on locale)
     *     "mmddyyyy" or "ddmmyyyy" (depending on locale)
     *     "yyyymmdd"
     *     "YYYY-MM-DD" or "YYYY/MM/DD"
     * Bad dates return 0
     * Takes into account local standard formats.
     *
     * @param {string} dateString
     * @returns {number}
     */
    static strToInt(dateString) {
        const format = this.detectDateFormat();
        dateString = dateString.trim();
        dateString = dateString.replace(/-/g, '/');
        dateString = dateString.replace(/\./g, '/');

        let day;
        let month;
        let year;

        if (format === "MM/DD/YYYY" && /^\d{1,2}\/\d{1,2}\/\d{2,4}/.test(dateString)) {  //  mM/dD/yyYY
            let sp = dateString.indexOf(" ");
            if (sp > 5)
                dateString = dateString.substring(0, sp);
            const parts = dateString.split("/");
            day = parseInt(parts[1], 10);
            month = parseInt(parts[0], 10);
            year = parseInt(parts[2], 10);
        } else if (format === "DD/MM/YYYY" && /^\d{1,2}\/\d{1,2}\/\d{2,4}/.test(dateString)) {  //  dD/mM/yyYY
            let sp = dateString.indexOf(" ");
            if (sp > 5)
                dateString = dateString.substring(0, sp);
            const parts = dateString.split("/");
            month = parseInt(parts[1], 10);
            day = parseInt(parts[0], 10);
            year = parseInt(parts[2], 10);
        } else if (/^\d{4}[-/]\d{2}[-/]\d{2}/.test(dateString)) {  // YYYY-MM-DD or YYYY/MM/DD
            year = parseInt(dateString.substring(0, 4), 10);
            month = parseInt(dateString.substring(5, 7), 10);
            day = parseInt(dateString.substring(8, 10), 10);
        } else if (/^\d{8}$/.test(dateString)) {  // NNNNNNNN
            if (format === "MM/DD/YYYY") {
                // assume MMDDYYYY
                month = parseInt(dateString.substring(0, 2), 10);
                day = parseInt(dateString.substring(2, 4), 10);
                year = parseInt(dateString.substring(4, 8), 10);
            } else {
                // assume DDMMYYYY
                day = parseInt(dateString.substring(0, 2), 10);
                month = parseInt(dateString.substring(2, 4), 10);
                year = parseInt(dateString.substring(4, 8), 10);
            }
            if (month < 1 || month > 12 || day < 1 || day > 31 || year < 1900 || year > 2100) {
                // assume YYYYMMDD
                year = parseInt(dateString.substring(0, 4), 10);
                month = parseInt(dateString.substring(4, 6), 10);
                day = parseInt(dateString.substring(6, 8), 10);
            }
            if (month < 1 || month > 12 || day < 1 || day > 31 || year < 1900 || year > 2100)
                return 0;
        } else
            return 0;

        if (year < 100) {
            let currentYear = new Date().getFullYear();
            let y19 = 1900 + year;
            let y20 = 2000 + year;
            year = Math.abs(currentYear - y19) < Math.abs(currentYear - y20) ? y19 : y20;
        }

        if (year < 1000 || year > 3000 || month < 1 || month > 12 || day < 1)
            return 0;

        const monthLength = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

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

        const parts = dateString.split("/");
        const day = parseInt(parts[2], 10);
        const month = parseInt(parts[1], 10);
        let year = parseInt(parts[0], 10);

        if (year < 100) {
            let currentYear = new Date().getFullYear();
            let y19 = 1900 + year;
            let y20 = 2000 + year;
            year = Math.abs(currentYear - y19) < Math.abs(currentYear - y20) ? y19 : y20;
        }

        if (year < 1000 || year > 3000 || month < 1 || month > 12 || day < 1)
            return 0;

        let monthLength = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];

        // Adjust for leap years
        if (year % 400 === 0 || (year % 100 !== 0 && year % 4 === 0))
            monthLength[1] = 29;

        if (day > monthLength[month - 1])
            return 0;

        return year * 10000 + month * 100 + day;
    }

    /**
     * Convert a JavaScript Date, a string, an integer date in the form of YYYYMMDD, YYYYMMDDHHMM, or the number of milliseconds
     * since the epoch and returns a date formated as an integer in YYYYMMDD format.
     *
     * @param dt number, string, or date
     * @returns {number} YYYYMMDD or zero
     */
    static toInt(dt) {
        if (!dt)
            return 0;
        if (typeof dt === 'string')
            return this.strToInt(dt);
        if (typeof dt === 'number' || dt instanceof Number) {
            if (dt > 250001010000)
                return this.toInt(new Date(dt));
            if (dt > 190001010000)
                return dt / 10000;
            return dt;
        }
        if (dt instanceof Date)
            return dt.getFullYear() * 10000 + (dt.getMonth() + 1) * 100 + dt.getDate();
        return 0;
    }

    /**
     * @deprecated
     *
     * Use DateUtils.toInt()
     *
     * Convert a number of milliseconds since 1970 UTC to an integer date YYYYMMDD.
     * This takes into account the local timezone.
     *
     *
     * @param m {number} number of milliseconds since 1970 UTC
     * @returns {number} YYYYMMDD
     *
     * @see DateUtils.toInt()
     * @see DateTimeUtils.toMilliseconds()
     */
    static millsToInt(m) {
        if (!m)
            return 0;
        const dt = new Date(m);
        return this.toInt(dt);
    }

    /**
     * Is dt a valid date?
     *
     * @param dt number, string, or date
     * @returns {boolean} true if valid date
     */
    static isValid(dt) {
        dt = this.toInt(dt);
        if (!dt)
            return false;
        return dt === this.calendar(this.julian(dt)) && dt >= 19000101 && dt <= 21000101;
    }

    /**
     * Verify SQL date formatted like YYYY-MM-DD
     *
     * @param {string} val
     * @returns {boolean}
     */
    static isSQLDate(val) {
        if (val)
            val = val.trim();
        if (!val)
            return false;
        return !!this.SQLtoInt(val);
    }

    /**
     * Convert a numeric date to a locale-appropriate string
     * (e.g. "05/07/2025" or "07/05/2025" depending on the user’s locale).
     *
     * @param {number|string|Date} dt   – value in YYYYMMDD form *or* milliseconds
     * @param {string}            [tz]  – IANA zone name; omit/falsy ⇒ host’s zone
     * @returns {string}
     */
    static intToStr4(dt, tz) {
        dt = this.toInt?.(dt) ?? dt;         // keeps existing helper logic
        if (!dt)
            return '';

        // If dt is YYYYMMDD (≥ 18000101 and ≤ 99991231) build a Date at local midnight
        // Otherwise assume it’s epoch millis and construct directly
        let date;
        if (typeof dt === 'number' && dt >= 18000101 && dt <= 99991231) {
            const year  = Math.floor(dt / 10000);
            const month = Math.floor((dt % 10000) / 100) - 1;   // 0-based
            const day   = dt % 100;
            date = new Date(year, month, day);
        } else {
            date = new Date(dt);                // handles millis or Date instance
        }
        if (isNaN(date))
            return '';

        const options = {
            year  : 'numeric',
            month : '2-digit',
            day   : '2-digit',
            timeZone : tz || undefined          // undefined → keep local zone
        };

        // `undefined` as the first argument tells Intl to use the browser’s default locale
        return new Intl.DateTimeFormat(undefined, options).format(date);
    }

    /**
     * Convert a date to string yyyy-mm-dd
     *
     * @param dt YYYYMMDD, YYYYMMDDHHMM, Date, milliseconds, or string
     * @returns {string}
     */
    static intToSQL(dt) {
        dt = this.toInt(dt);
        if (!dt)
            return '';
        const y = Math.floor(dt / 10000);
        dt -= y * 10000;
        const m = Math.floor(dt / 100);
        const d = Math.floor(dt - m * 100);
        return Utils.format(y, "Z", 4, 0) + '-' + Utils.format(m, "Z", 2, 0) + '-' + Utils.format(d, "Z", 2, 0);
    }

    /**
     * Convert a date to a locale-appropriate string
     * using 2-digit year: "mm/dd/yy", "dd/mm/yy", etc.
     *
     * @param {number|string|Date} dt  – YYYYMMDD, Date, millis, or numeric string
     * @param {string}            [tz] – IANA zone (e.g. "America/New_York");
     *                                   omit/falsy ⇒ use the host’s zone
     * @returns {string}
     */
    static intToStr2(dt, tz) {
        // Preserve any existing helper behaviour
        dt = this.toInt?.(dt) ?? dt;
        if (!dt) return '';

        /* ---------- build a Date object ---------- */
        let date;

        // If dt is a “YYYYMMDD” integer, turn it into a Date at local midnight
        if (typeof dt === 'number' && dt >= 18000101 && dt <= 99991231) {
            const year  = Math.floor(dt / 10000);
            const month = Math.floor((dt % 10000) / 100) - 1;  // 0-based
            const day   = dt % 100;
            date = new Date(year, month, day);
        } else {
            // Otherwise treat dt as epoch-milliseconds or an actual Date
            date = new Date(dt);
        }
        if (isNaN(date)) return '';

        /* ---------- format in the requested (or local) zone ---------- */
        const options = {
            year     : '2-digit',
            month    : '2-digit',
            day      : '2-digit',
            timeZone : tz || undefined      // undefined ⇒ stay in local zone
        };

        // First arg undefined → browser’s default locale
        return new Intl.DateTimeFormat(undefined, options).format(date);
    }

    /**
     * @deprecated
     *
     * Use DateUtils.toInt()
     *
     * Convert a Date object to YYYYMMDD int
     *
     * @param {Date} dt
     * @returns {number}
     *
     * @see DateUtils.toInt()
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
        const y = Math.floor(dt / 10000);
        dt -= y * 10000;
        const m = Math.floor(dt / 100);
        const d = Math.floor(dt - m * 100);
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
        return this.intToDate(this.intAddDays(this.toInt(dt), days));
    }

    /**
     * Add days to an int date
     *
     * @param {number} dt int date YYYYMMDD
     * @param {number} days number of days to add or subtract
     * @returns {int}
     */
    static intAddDays(dt, days) {
        return this.calendar(this.julian(dt) + days);
    }

    /**
     * Return the number of months between two dates.
     *
     * @param idt1 {number} Date or integer date YYYYMMDD
     * @param idt2 {number} Date or integer date YYYYMMDD
     * @returns {number} number of months between the two dates
     */
    static monthsDifference(idt1, idt2) {
        const dt1 = idt1 instanceof Date ? idt1 : this.intToDate(idt1);
        const dt2 = idt2 instanceof Date ? idt2 : this.intToDate(idt2);
        let months = (dt2.getFullYear() - dt1.getFullYear()) * 12;
        months -= dt1.getMonth() + 1;
        months += dt2.getMonth() + 1;
        if (dt1.getDate() > dt2.getDate())
            months--;
        return months <= 0 ? 0 : months;
    }

    /**
     * Convert an integer date into a julian date.
     *
     * @param {number} dt integer date formatted as YYYYMMDD
     * @returns {number} the julian date
     */
    static julian(dt) {
        /* This can't be done some of the more obvious ways because of changes in daylight savings time.  */
        /* And leap years?  */
        let d, y, m;

        if (dt <= 0)
            return dt;
        y = Math.floor(dt / 10000);
        m = Math.floor((dt % 10000) / 100);
        d = Math.floor(dt % 100);
        d += Math.floor(.5 + (m - 1) * 30.57);
        if (m >	2) {
            d--;
            if (0 !== y % 400 && (0 !== y % 4 || 0 === y % 100))
                d--;
        }
        d += Math.floor(365.25 * --y);
        d += Math.floor(y / 400);
        d -= Math.floor(y / 100);
        return d;
    }

    /**
     * Convert a julian date into an integer date.
     *
     * @param d {number} d the julian date
     * @returns {number} the integer date formatted as YYYYMMDD
     */
    static calendar(d) {
        let	y, m, t;

        if (d <= 0)
            return d;
        y = Math.floor(1.0 + d / 365.2425);
        t = y -	1;
        d -= Math.floor(t * 365.25);
        d -= Math.floor(t / 400);
        d += Math.floor(t / 100);
        if (d >	59  &&	 0 !== y % 400	 &&  (0 !== y %	4  ||  0 === y % 100))
            d++;
        if (d >	60)
            d++;
        m = Math.floor((d + 30) / 30.57);
        d -= Math.floor(.5 + (m - 1) * 30.57);
        if (m === 13)  {
            m = 1;
            ++y;
        }  else if (m === 0)  {
            m = 12;
            --y;
        }
        return 10000 * y + m * 100 + d;
    }

    /**
     * Takes an integer date formatted as YYYYMMDD and returns an integer
     * indicating the day of the week.
     *
     * 0 Sunday
     * 1 Monday
     * 2 Tuesday
     * 3 Wednesday
     * 4 Thursday
     * 5 Friday
     * 6 Saturday
     *
     * @param {number} dt integer date formatted as YYYYMMDD
     * @returns {number} an integer indicating the day of the week
     */
    static dayOfWeek(dt) {
        return this.julian(dt) % 7;
    }

    /**
     * Return the number of days between two dates.
     *
     * @param {number} dt1 date formatted as YYYYMMDD
     * @param {number} dt2 date formatted as YYYYMMDD
     * @returns {number} returns the number of days dt1 - dt2
     */
    static daysDifference(dt1, dt2) {
        return this.julian(dt1) - this.julian(dt2);
    }

    /**
     * Takes an integer date formatted as YYYYMMDD and returns the
     * name of the day of the week as a string such as
     * "Sunday", "Monday", etc.
     *
     * @param {number} dt integer date formatted as YYYYMMDD
     * @returns {string} the name of the day of the week
     */
    static dayOfWeekName(dt) {
        switch (this.dayOfWeek(dt)) {
            case 0:  return "Sunday";
            case 1:  return "Monday";
            case 2:  return "Tuesday";
            case 3:  return "Wednesday";
            case 4:  return "Thursday";
            case 5:  return "Friday";
            case 6:  return "Saturday";
            default: return '';
        }
    }

    /**
     * Takes an integer date formatted as YYYYMMDD and returns the
     * name of the month as a string such as
     * "January", "February", etc.
     *
     * @param {number} dt integer date formatted as YYYYMMDD
     * @returns {string} the name of the day of the week
     */
    static monthName(dt) {
        const y = Math.floor(dt / 10000);
        dt -= y * 10000;
        const m = Math.floor(dt / 100);
        switch (m) {
            case 1: return 'January';
            case 2: return 'February';
            case 3: return 'March';
            case 4: return 'April';
            case 5: return 'May';
            case 6: return 'June';
            case 7: return 'July';
            case 8: return 'August';
            case 9: return 'September';
            case 10: return 'October';
            case 11: return 'November';
            case 12: return 'December';
            default: return '';
        }
    }

    /**
     * Formats a date into a string format as in:  Mon January 12, 2021
     *
     * @param dt {number} integer date formatted as YYYYMMDD
     * @returns {string}
     */
    static longFormat(dt) {
        if (typeof dt === 'string')
            dt = Number(dt);
        if (!dt)
            return '';
        const y = Math.floor(dt / 10000);
        const t = dt - y * 10000;
        const m = Math.floor(t / 100);
        const d = Math.floor(t - m * 100);
        const dayOfWeek = Utils.take(this.dayOfWeekName(dt), 3);
        const monthName = this.monthName(dt);
        return dayOfWeek + " " + monthName + " " + d + ", " + y;
    }

    /**
     * Returns the current date as an int formatted as YYYYMMDD
     *
     * @returns {number}
     */
    static today() {
        return this.toInt(new Date());
    }

    /**
     * Return the year portion of a date.
     *
     * @param dt {number|Date} integer date formatted as YYYYMMDD or a Date object
     * @returns {number}
     */
    static year(dt) {
        if (dt instanceof Date)
            return dt.getFullYear();
        return dt ? Math.floor(dt / 10000) : 0;
    }

    /**
     * Return the month portion of a date.
     *
     * @param dt {number|Date} integer date formatted as YYYYMMDD or a Date object
     * @returns {number}
     */
    static month(dt) {
        if (!dt)
            return 0;
        if (dt instanceof Date)
            return dt.getMonth() + 1;
        const y = Math.floor(dt / 10000);
        dt -= y * 10000;
        return Math.floor(dt / 100);
    }

    /**
     * Return the day portion of a date.
     *
     * @param dt {number|Date} integer date formatted as YYYYMMDD or a Date object
     * @returns {number}
     */
    static day(dt) {
        if (!dt)
            return 0;
        if (dt instanceof Date)
            return dt.getDate();
        const y = Math.floor(dt / 10000);
        dt -= y * 10000;
        const m = Math.floor(dt / 100);
        return Math.floor(dt - m * 100);
    }
}

