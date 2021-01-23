package org.kissweb;

import java.text.SimpleDateFormat;
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
     * Create a DateTime object from a date (YYYYMMDD) and time (HHMM)
     *
     * @param dt YYYYMMDD
     * @param time HHMM
     */
    public DateTime(int dt, int time) {
        zdt = ZonedDateTime.of(DateUtils.year(dt), DateUtils.month(dt), DateUtils.day(dt), TimeUtils.hour(time), TimeUtils.minutes(time), 0, 0, ZoneId.systemDefault());
    }

    /**
     * Create a DateTime from a Date object.
     *
     * @param dt
     */
    public DateTime(Date dt) {
        zdt = ZonedDateTime.ofInstant(dt.toInstant(), ZoneId.systemDefault());
    }

    /**
     * Initialize a new DateTime from a GregorianCalendar
     *
     * @param dt
     */
    public DateTime(GregorianCalendar dt) {
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
        return zdt.getYear();
    }

    /**
     * Return the month portion of a DateTime
     *
     * @return 1-12
     */
    public int getMonth() {
        return zdt.getMonthValue();
    }

    /**
     * Return the day of the month
     *
     * @return (1-31)
     */
    public int getDay() {
        return zdt.getDayOfMonth();
    }

    /**
     * Return the hour portion of a DateTime
     *
     * @return (0-23)
     */
    public int getHour() {
        return zdt.getHour();
    }

    /**
     * Return the minute portion of a DateTime
     *
     * @return (0-59)
     */
    public int getMinute() {
        return zdt.getMinute();
    }

    /**
     * Get a Date representation of the DateTime object.
     *
     * @return
     */
    public Date getDate() {
        return Date.from(zdt.toInstant());
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
        return zdt.getHour() * 100 + zdt.getMinute();
    }

    /**
     * Return the ZoneDateTime object
     *
     * @return
     */
    public ZonedDateTime getZonedDateTime() {
        return zdt;
    }

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
     * Format a date passed in as a string mm/dd/yyyy hh:mm AM/PM
     * 
     * @param date
     * @return
     */
    public static String format(Date date) {
        SimpleDateFormat df = new SimpleDateFormat("MM/dd/yyyy h:mm a");
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

    public static void main(String [] args) {
        DateTime dt = new DateTime(20201224, 1130);
        Date d = dt.getDate();
        int x = 1;
    }

}
