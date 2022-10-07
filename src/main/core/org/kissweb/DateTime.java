package org.kissweb;

import java.text.SimpleDateFormat;
import java.time.Instant;
import java.time.ZoneId;
import java.time.ZonedDateTime;
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
     * Create a DateTime object from a ZonedDateTime object
     *
     * @param dt
     */
    public DateTime(ZonedDateTime dt) {
        zdt = dt;
    }

    /**
     * Create a DateTime object from the number of milliseconds since 1970 (Epoch).
     *
     * @param millisecs
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
     * Create a DateTime from a Date object.
     *
     * @param dt
     */
    public DateTime(Date dt) {
        if (dt == null)
            zdt = null;
        else
            zdt = ZonedDateTime.ofInstant(dt.toInstant(), ZoneId.systemDefault());
    }

    /**
     * Initialize a new DateTime from a GregorianCalendar
     *
     * @param dt
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
     * @return
     */
    public static DateTime now() {
        return new DateTime(ZonedDateTime.now());
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
     * @return
     */
    public Date getDate() {
        return zdt == null ? null : Date.from(zdt.toInstant());
    }

    /**
     * Get date in an int format YYYYMMDD
     *
     * @return
     */
    public int getIntDate() {
        return getYear() * 10000 + getMonth() * 100 + getDay();
    }

    /**
     * Return an integer representation of the date portion.
     *
     * @return HHMM
     */
    public int getIntTime() {
        return zdt == null ? 0 : zdt.getHour() * 100 + zdt.getMinute();
    }

    /**
     * Return the ZoneDateTime object
     *
     * @return
     */
    public ZonedDateTime getZonedDateTime() {
        return zdt;
    }

    /**
     * Return the date formatted as mm/dd/yyyy hh:mm AM/PM
     *
     * @return
     */
    public String format() {
        return DateTime.format(getDate());
    }

    /**
     * Return the current date and time as a string mm/dd/yyyy hh:mm AM/PM
     *
     * @return
     */
    public static String currentDateTimeFormatted() {
        return DateTime.format(new Date());
    }

    /**
     * Return the current date and time as a string mm/dd/yyyy hh:mm AM/PM cST
     *
     * @return
     */
    public static String currentDateTimeFormattedTZ() {
        return DateTime.formatTZ(new Date());
    }

    /**
     * Format a date passed into a string mm/dd/yyyy hh:mm AM/PM
     * 
     * @param date
     * @return
     */
    public static String format(Date date) {
        if (date == null)
            return "";
        SimpleDateFormat df = new SimpleDateFormat("MM/dd/yyyy h:mm a");
        return df.format(date);
    }

    /**
     * Format a date passed into a string mm/dd/yyyy hh:mm AM/PM xST
     *
     * @param date
     * @return
     */
    public static String formatTZ(Date date) {
        if (date == null)
            return "";
        SimpleDateFormat df = new SimpleDateFormat("MM/dd/yyyy h:mm a zzz");
        return df.format(date);
    }

    /**
     * Returns a string representing the current date and/or time with a specified format
     * and within a specified timezone.
     *
     * @param dateFormat
     * @param timeZone
     * @return
     */
    public static String currentDateTimeFormatted(String dateFormat, String timeZone) {
        Date date = new Date();
        SimpleDateFormat df = new SimpleDateFormat(dateFormat);
        df.setTimeZone(TimeZone.getTimeZone(timeZone));
        return df.format(date);
    }

    /**
     * Returns a string representing the current date and/or time with a specified format.
     *
     * @param dateFormat
     * @return
     */
    public static String currentDateTimeFormatted(String dateFormat) {
        Date date = new Date();
        SimpleDateFormat df = new SimpleDateFormat(dateFormat);
        return df.format(date);
    }

    /**
     * Returns number of milliseconds since 1970.
     *
     * @return
     */
    public long getMilliseconds() {
        return zdt.toInstant().toEpochMilli();
    }
    
    /**
     * Add days to a DateTime object.
     * 
     * @param days the number of days to add - can be negative
     * @return 
     */
    public DateTime addDays(int days) {
        zdt = zdt.plusDays(days);
        return this;
    } 

    public static void main(String [] args) {
        DateTime dt = new DateTime(20201224, 1130);
        Date d = dt.getDate();
        int x = 1;
    }

}
