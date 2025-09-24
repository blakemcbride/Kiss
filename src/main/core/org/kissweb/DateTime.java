package org.kissweb;

import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

/*
 * Author: Blake McBride
 * Date: 7/28/18
 */

/**
 * Class dealing with dates with times.
 *
 */
public class DateTime {

    private ZonedDateTime zdt;

    /**
     * Create a DateTime object from the current date and time.
     */
    public DateTime() {
        zdt = ZonedDateTime.now();
    }

    /**
     * Create a DateTime object from a ZonedDateTime object
     *
     * @param dt the ZonedDateTime object
     */
    public DateTime(ZonedDateTime dt) {
        zdt = dt;
    }

    /**
     * Create a DateTime object from the number of milliseconds since 1970 (Epoch).
     *
     * @param millisecs the number of milliseconds since epoch
     */
    public DateTime(long millisecs) {
        zdt = ZonedDateTime.ofInstant(Instant.ofEpochMilli(millisecs), ZoneId.systemDefault());
    }

    /**
     * Create a DateTime object from a date (YYYYMMDD) and time (HHMM)
     *
     * @param dt YYYYMMDD
     * @param time HHMM
     */
    public DateTime(int dt, int time) {
        if (dt == 0 && time == 0)
            zdt = null;
        else
            zdt = ZonedDateTime.of(DateUtils.year(dt), DateUtils.month(dt), DateUtils.day(dt), TimeUtils.hour(time), TimeUtils.minutes(time), 0, 0, ZoneId.systemDefault());
    }

    /**
     * Create a DateTime object from a date (YYYYMMDD) and time (HHMM) in the specified time zone.
     *
     * @param dt YYYYMMDD
     * @param time HHMM
     * @param timeZoneId the time zone identifier
     */
    public DateTime(int dt, int time, String timeZoneId) {
        if (dt == 0 && time == 0)
            zdt = null;
        else
            zdt = ZonedDateTime.of(DateUtils.year(dt), DateUtils.month(dt), DateUtils.day(dt), TimeUtils.hour(time), TimeUtils.minutes(time), 0, 0, ZoneId.of(timeZoneId));
    }

    /**
     * Create a DateTime from a Date object.
     *
     * @param dt the Date object
     */
    public DateTime(Date dt) {
        if (dt == null)
            zdt = null;
        else
            zdt = ZonedDateTime.ofInstant(dt.toInstant(), ZoneId.systemDefault());
    }

    /**
     * Create a DateTime from a Date object in the provided zone.
     *
     * @param dt the Date object
     * @param zone the ZoneId
     */
    public DateTime(Date dt, ZoneId zone) {
        if (dt == null)
            zdt = null;
        else
            zdt = ZonedDateTime.ofInstant(dt.toInstant(), zone);
    }

    /**
     * Create a DateTime from a Date object in the provided time zone.
     *
     * @param dt the Date object
     * @param timeZoneId the time zone identifier
     */
    public DateTime(Date dt, String timeZoneId) {
        if (dt == null)
            zdt = null;
        else
            zdt = ZonedDateTime.ofInstant(dt.toInstant(), ZoneId.of(timeZoneId));
    }

    /**
     * Initialize a new DateTime from a GregorianCalendar
     *
     * @param dt the GregorianCalendar object
     */
    public DateTime(GregorianCalendar dt) {
        if (dt == null)
            zdt = null;
        else
            zdt = dt.toZonedDateTime();
    }

    /**
     * Create a new DateTime object that represents the current date/time.
     *
     * @return a new DateTime object representing the current date/time
     */
    public static DateTime now() {
        return new DateTime(ZonedDateTime.now());
    }

    /**
     * Create a new DateTime object that represents the current date/time in the specified
     * time zone.
     *
     * @param timeZoneId
     *            The time zone ID to use.  Examples include "America/New_York",
     *            "America/Chicago", "Europe/London", etc.
     * @return a new DateTime object representing the current date/time in the specified time zone
     */
    public static DateTime now(String timeZoneId) {
        return new DateTime(ZonedDateTime.now(ZoneId.of(timeZoneId)));
    }

    /**
     * Changes the time zone associated with this DateTime object to the specified
     * time zone and returns the modified DateTime object.
     * <br><br>
     * The date and time remain the same, but the time zone is changed to the new
     * one.
     *
     * @param timeZoneId
     *            The ID of the new time zone.  Examples include "America/New_York",
     *            "America/Chicago", "Europe/London", etc.
     * @return This DateTime object, now with the new time zone.
     */
    public DateTime changeTimeZone(String timeZoneId) {
        if (zdt != null)
            zdt = zdt.withZoneSameInstant(ZoneId.of(timeZoneId));
        return this;
    }

    /**
     * Get the year portion of a DateTime
     *
     * @return YYYY
     */
    public int getYear() {
        return zdt == null ? 0 : zdt.getYear();
    }

    /**
     * Return the month portion of a DateTime
     *
     * @return 1-12
     */
    public int getMonth() {
        return zdt == null ? 0 : zdt.getMonthValue();
    }

    /**
     * Return the day of the month
     *
     * @return (1-31)
     */
    public int getDay() {
        return zdt == null ? 0 : zdt.getDayOfMonth();
    }

    /**
     * Return the hour portion of a DateTime
     *
     * @return (0-23)
     */
    public int getHour() {
        return zdt == null ? 0 : zdt.getHour();
    }

    /**
     * Return the minute portion of a DateTime
     *
     * @return (0-59)
     */
    public int getMinute() {
        return zdt == null ? 0 : zdt.getMinute();
    }

    /**
     * Get a Date representation of the DateTime object.
     *
     * @return a Date object representing this DateTime
     */
    public Date getDate() {
        return zdt == null ? null : Date.from(zdt.toInstant());
    }

    /**
     * Get date in an int format YYYYMMDD
     *
     * @return the date as an integer in YYYYMMDD format
     */
    public int getIntDate() {
        return getYear() * 10000 + getMonth() * 100 + getDay();
    }

    /**
     * Return an integer representation of the time portion.
     *
     * @return HHMM
     */
    public int getIntTime() {
        return zdt == null ? 0 : zdt.getHour() * 100 + zdt.getMinute();
    }

    /**
     * Return the ZoneDateTime object
     *
     * @return the ZonedDateTime object
     */
    public ZonedDateTime getZonedDateTime() {
        return zdt;
    }

    /**
     * Return the current date formatted as mm/dd/yyyy hh:mm AM/PM
     *
     * @return the formatted date/time string
     */
    public String format() {
        return DateTime.format(zdt);
    }

    /**
     * Return the current date and time as a string mm/dd/yyyy hh:mm AM/PM
     *
     * @return the formatted current date/time string
     */
    public static String currentDateTimeFormatted() {
        return DateTime.format(new Date());
    }

    /**
     * Return the current date and time as a string mm/dd/yyyy hh:mm AM/PM cST
     *
     * @return the formatted current date/time string with time zone
     */
    public static String currentDateTimeFormattedTZ() {
        return DateTime.formatTZ(new Date());
    }

    /**
     * Format a date passed into a string mm/dd/yyyy hh:mm AM/PM
     * 
     * @param date the date to format
     * @return the formatted date string
     */
    public static String format(Date date) {
        return format(date, "MM/dd/yyyy h:mm a", null);
    }

    /**
     * Format a date passed into a string mm/dd/yyyy hh:mm AM/PM.
     *
     * @param date The date to format.
     * @return The formatted string.
     */
    public static String format(ZonedDateTime date) {
        return format(date, "MM/dd/yyyy h:mm a", null);
    }

    /**
     * Format a date passed into a string according to a format specification.
     *
     * @param date The date to format.
     * @param fmt The format string.
     * @return The formatted string.
     */
    public static String format(ZonedDateTime date, String fmt) {
        return format(date, fmt, null);
    }

    /**
     * Format a date passed into a string according to a format specification.
     *
     * @param date The date to format.
     * @param fmt The format string.
     * @param timeZoneId The ID of the time zone to use when formatting the date.
     * If not null or empty, the time zone is set to that zone before formatting.
     * @return The formatted string.
     */
    public static String format(Date date, String fmt, String timeZoneId) {
        if (date == null)
            return "";
        if (fmt == null)
            fmt = "MM/dd/yyyy h:mm a";
        final SimpleDateFormat df = new SimpleDateFormat(fmt);
        if (timeZoneId != null && !timeZoneId.trim().isEmpty())
            df.setTimeZone(TimeZone.getTimeZone(timeZoneId));
        return df.format(date);
    }

    /**
     * Format a ZonedDateTime into a string using the given format string.
     * If <code>timeZoneId</code> is not null or empty, the time zone is set to that
     * zone before formatting.
     *
     * @param dateTime The ZonedDateTime to format
     * @param fmt The format string
     * @param timeZoneId The time zone ID to use, if not null or empty
     * @return The formatted string
     */
    public static String format(ZonedDateTime dateTime, String fmt, String timeZoneId) {
        if (dateTime == null)
            return "";
        if (fmt == null)
            fmt = "MM/dd/yyyy h:mm a";
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern(fmt);
        if (timeZoneId != null && !timeZoneId.trim().isEmpty())
            dateTime = dateTime.withZoneSameInstant(ZoneId.of(timeZoneId));
        return dateTime.format(formatter);
    }

    /**
     * Format a date passed into a string formatted as determined by <code>fmt</code>
     *
     * @param date the date to format
     * @param fmt the format string
     * @return the formatted date string
     *
     * @see java.text.SimpleDateFormat for information about fmt
     */
    public static String format(Date date, String fmt) {
        return format(date, fmt, null);
    }

    /**
     * Format a date passed into a string mm/dd/yyyy hh:mm AM/PM xST
     *
     * @param date the date to format
     * @return the formatted date string with time zone
     */
    public static String formatTZ(Date date) {
        return format(date, "MM/dd/yyyy h:mm a zzz", null);
    }

    /**
     * Returns a string representing the current date and/or time with a specified format
     * and within a specified timezone.
     *
     * @param dateFormat the date format string
     * @param timeZone the time zone
     * @return the formatted current date/time string
     *
     * @see java.text.SimpleDateFormat for information about dateFormat
     *
     */
    public static String currentDateTimeFormatted(String dateFormat, String timeZone) {
        return format(new Date(), dateFormat, timeZone);
    }

    /**
     * Returns a string representing the current date and/or time with a specified format.
     *
     * @param dateFormat the date format string
     * @return the formatted current date/time string
     *
     * @see java.text.SimpleDateFormat for information about dateFormat
     */
    public static String currentDateTimeFormatted(String dateFormat) {
        return format(new Date(), dateFormat, null);
    }

    /**
     * Returns number of milliseconds since 1970 UTC.
     *
     * @return the number of milliseconds since epoch
     */
    public long getMilliseconds() {
        return zdt == null ? 0L : zdt.toInstant().toEpochMilli();
    }

    /**
     * Returns number of seconds since 1970 UTC.
     *
     * @return the number of seconds since epoch
     */
    public long getSeconds() {
        return zdt == null ? 0L : zdt.toInstant().toEpochMilli() / 1000;
    }
    
    /**
     * Add days to a DateTime object.
     * 
     * @param days the number of days to add - can be negative
     * @return this DateTime object for method chaining 
     */
    public DateTime addDays(int days) {
        if (zdt != null)
            zdt = zdt.plusDays(days);
        return this;
    }

    /**
     * Add hours to a DateTime object.
     *
     * @param hours the number of hours to add - can be negative
     * @return this object
     */
    public DateTime addHours(int hours) {
        if (zdt != null)
            zdt = zdt.plusHours(hours);
        return this;
    }

    /**
     * Add minutes to a DateTime object.
     *
     * @param minutes the number of minutes to add - can be negative
     * @return this object
     */
    public DateTime addMinutes(int minutes) {
        if (zdt != null)
            zdt = zdt.plusMinutes(minutes);
        return this;
    }

    /**
     * Return the date and time combined as a long integer YYYYMMDDHHMM
     * 
     * @return the date and time as a long integer in YYYYMMDDHHMM format
     */
    public long getDTLong() {
        return (long) getIntDate() * 10000L + (long) getIntTime();
    }

    /**
     * Returns the ZoneId object associated with this DateTime object.
     *
     * @return the ZoneId object associated with this DateTime object
     */
    public ZoneId getZoneId() {
        return zdt == null ? null : zdt.getZone();
    }

    /**
     * Get the time zone ID as a string representation.
     * <p>
     * This method returns the string representation of the time zone ID
     * associated with this DateTime object. Common examples include
     * "America/New_York", "America/Chicago", "Europe/London", "UTC", etc.
     * </p>
     *
     * @return the time zone ID as a string, or null if this DateTime is null
     */
    public String getZoneIdString() {
        return zdt == null ? null : zdt.getZone().toString();
    }

    /**
     * Test main method.
     * 
     * @param args command line arguments
     */
    public static void main(String [] args) {
        DateTime dt = new DateTime(20201224, 1130);
        Date d = dt.getDate();
        int x = 1;
    }

}
