package org.kissweb;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import static java.time.LocalDate.of;

/**
 * This class deals with dates in several formats including int, Date, Calendar, and LocalDate.
 * <code>int</code> dates are formatted as YYYYMMDD.  For example, June 8, 2018 would
 * be represented as 20180608.
 * <br><br>
 * All combinations of converting from one type to another are included.  In order to reduce
 * the number of methods, most methods deal with the <code>int</code> date type.  Given
 * the conversion methods, it is possible to perform any function on any date type.
 *
 * @author Blake McBride
 */
public class DateUtils {

    /**
     * Returns the current date as an integer formatted as YYYYMMDD.
     *
     * @return
     */
    public static int today() {
        Calendar c = Calendar.getInstance();
        int y = c.get(Calendar.YEAR);
        int m = c.get(Calendar.MONTH) + 1;
        int d = c.get(Calendar.DAY_OF_MONTH);
        return y * 10000 + m * 100 + d;
    }

    /**
     * Return today's date as a <code>Date</code> instance.
     *
     * @return
     */
    public static Date todayDate() {
        return new Date();
    }


    /**
     * Return today's date as a <code>Calendar</code> instance.
     *
     * @return
     */
    public static Calendar todayCalendar() {
        return Calendar.getInstance();
    }

    /**
     * Return today's date as a <code>LocalDate</code> instance.
     *
     * @return
     */
    public static LocalDate todayLocalDate() {
        return LocalDate.now();
    }

    private static int guessYear(int y) {
        if (y >= 100)
            return y;
        int currentYear = year(today());
        if (y + 2000 > currentYear + 10)
            return 1900 + y;
        else
            return 2000 + y;
    }

    /**
     * Create an <code>int</code> date from a year, month, and day.
     *
     * @param y
     * @param m
     * @param d
     * @return
     */    public static int toInt(int y, int m, int d) {
        if (m == 0)
            return 0;
        y = guessYear(y);
        return y * 10000 + m * 100 + d;
    }

    /**
     * convert from a <code>Date</code> instance to an <code>int</code> date.
     *
     * @param dat
     * @return
     */
    public static int toInt(Date dat) {
        if (dat == null)
            return 0;
        Calendar cal = Calendar.getInstance();
        cal.setTime(dat);
        return cal.get(Calendar.DAY_OF_MONTH) + ((cal.get(Calendar.MONTH) + 1) * 100) + ((cal.get(Calendar.YEAR)) * 10000);
    }

    /**
     * convert from a <code>Calendar</code> instance to an <code>int</code> date.
     *
     * @param date
     * @return
     */
    public static int toInt(Calendar date) {
        if (date == null)
            return 0;
        return toInt(date.getTime());
    }

    /**
     * convert from a <code>LocalDate</code> instance to an <code>int</code> date.
     *
     * @param dt
     * @return
     */
    public static int toInt(LocalDate dt) {
        if (dt == null)
            return 0;
        return toInt(dt.getYear(), dt.getMonthValue(), dt.getDayOfMonth());
    }

    /**
     * Create a <code>Date</code> from a year, month, and day.
     *
     * @param y
     * @param m
     * @param d
     * @return
     */
    public static Date toDate(int y, int m, int d) {
        if (m == 0)
            return null;
        y = guessYear(y);
        return toDate(toCalendar(y, m, d));
    }

    /**
     * convert from an <code>int</code> date to a <code>Date</code> date.
     *
     * @param dat
     * @return
     */
    public static Date toDate(int dat) {
        if (dat == 0)
            return null;
        int y = year(dat);
        int m = month(dat);
        int d = day(dat);
        return toDate(y, m, d);
    }

    /**
     * convert from an <code>Calendar</code> date to a <code>Date</code> date.
     *
     * @param cal
     * @return
     */
    public static Date toDate(Calendar cal) {
        return cal == null ? null : cal.getTime();
    }

    /**
     * convert from an <code>LocalDate</code> date to a <code>Date</code> date.
     *
     * @param dt
     * @return
     */    public static Date toDate(LocalDate dt) {
        return dt == null ? null : Date.from(dt.atStartOfDay(ZoneId.systemDefault()).toInstant());
    }

    /**
     * Create a <code>Calendar</code> from a year, month, and day.
     *
     * @param y
     * @param m
     * @param d
     * @return
     */
    public static Calendar toCalendar(int y, int m, int d) {
        if (m == 0)
            return null;
        y = guessYear(y);
        return new GregorianCalendar(y, m-1, d);
    }

    /**
     * convert from an <code>int</code> date to a <code>Calendar</code> date.
     *
     * @param dat
     * @return
     */
    public static Calendar toCalendar(int dat) {
        return dat == 0 ? null : new GregorianCalendar(year(dat), month(dat)-1, day(dat));
    }

    /**
     * Convert from a <code>Date</code> date to a <code>Calendar</code> date.
     *
     * @param date
     * @return
     */
    public static Calendar toCalendar(Date date) {
        if (date == null)
            return null;
        Calendar calendar = Calendar.getInstance();
        calendar.setTime(date);
        return calendar;
    }

    /**
     * Convert from a <code>LocalDate</code> date to a <code>Calendar</code> date.
     *
     * @param dt
     * @return
     */
    public static Calendar toCalendar(LocalDate dt) {
        if (dt == null)
            return null;
        return toCalendar(toDate(dt));
    }

    /**
     * Create a <code>LocalDate</code> from a year, month, and day.
     *
     * @param y
     * @param m
     * @param d
     * @return
     */
    public static LocalDate toLocalDate(int y, int m, int d) {
        if (m == 0)
            return null;
        y = guessYear(y);
        return of(y, m, d);
    }

    /**
     * convert from an <code>int</code> date to a <code>LocalDate</code> date.
     *
     * @param dt
     * @return
     */
    public static LocalDate toLocalDate(int dt) {
        if (dt == 0)
            return null;
        int y = year(dt);
        return of(y, month(dt), day(dt));
    }

    /**
     * Convert from a <code>Date</code> date to a <code>LocalDate</code> date.
     *
     * @param date
     * @return
     */
    public static LocalDate toLocalDate(Date date) {
        return date == null ? null : date.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
    }

    /**
     * Convert from a <code>Calendar</code> date to a <code>LocalDate</code>
     *
     * @param dt
     * @return
     *
     */
    public static LocalDate toLocalDate(Calendar dt) {
        return dt == null ? null : toLocalDate(toDate(dt));
    }

    /**
     * Returns year portion of a date.
     *
     * @param dt YYYYMMDD
     * @return  YYYY
     */
    public static int year(int dt) {
        return dt != 0 ? guessYear(dt) / 10000 : 0;
    }

    /**
     * Returns month portion of a date.
     *
     * @param dt YYYYMMDD
     * @return  MM
     */
    public static int month(int dt) {
        return (dt % 10000) / 100;
    }

    /**
     * Returns day portion of a date.
     *
     * @param dt YYYYMMDD
     * @return  DD
     */
    public static int day(int dt) {
        return dt % 100;
    }

    /**
     * Add N days to a date.
     *
     * @param dt
     * @param n
     * @return
     *
     * @see #addMonths(int, int)
     * @see #addYears(int, int)
     */
    public static int addDays(int dt, int n) {
        if (dt == 0)
            return 0;
        Calendar cal = toCalendar(dt);
        cal.add(Calendar.DAY_OF_YEAR, n);
        return toInt(cal);
    }

    /**
     * Add N months to a date.
     *
     * @param dt
     * @param n
     * @return
     *
     * @see #addDays(int, int)
     * @see #addYears(int, int)
     */
    public static int addMonths(int dt, int n) {
        if (dt == 0)
            return 0;
        Calendar cal = toCalendar(dt);
        cal.add(Calendar.MONTH, n);
        return toInt(cal);
    }

    /**
     * Add N years to a date.
     *
     * @param dt
     * @param n
     * @return
     *
     * @see #addDays(int, int)
     * @see #addMonths(int, int)
     */
    public static int addYears(int dt, int n) {
        if (dt == 0)
            return 0;
        int y = year(dt) + n;
        return toInt(y, month(dt), day(dt));
    }

    /**
     * Formats a date to a string according to a format specification.
     *
     * @param fmt format specification
     * @param dt
     * @return
     *
     * @see java.text.SimpleDateFormat
     * @see #format2(int)
     * @see #format4(int)
     * @see #formatSQL(int)
     * @see #formatLong(int)
     */
    public static String format(String fmt, int dt) {
        if (dt == 0)
            return "";
        Date date = toDate(dt);
        SimpleDateFormat df = new SimpleDateFormat(fmt);
        return df.format(date);
    }

    /**
     * Format a date with a two digit year as "m/d/yy"
     *
     * @param dt
     * @return
     */
    public static String format2(int dt) {
        return format("M/d/yy", dt);
    }

    /**
     * Format a date with a four digit year as "m/d/yyyy"
     *
     * @param dt
     * @return
     */
    public static String format4(int dt) {
        return format("M/d/yyyy", dt);
    }

    /**
     * Format a date as "YYYY-MM-DD"
     *
     * @param dt
     * @return
     */
    public static String formatSQL(int dt) {
        return format("yyyy-MM-dd", dt);
    }

    /**
     * Format date as "Jan 3, 2018"
     *
     * @param dt
     * @return
     *
     * @see #format2(int)
     * @see #format4(int)
     */
    public static String formatLong(int dt) {
        return format("MMM d, yyyy", dt);
    }

    /**
     * Return the number of days between two dates.
     * (dat1 - dat2)
     *
     * @param dat1
     * @param dat2
     * @return
     */
    public static long daysBetween(int dat1, int dat2) {
        long dif = toDate(dat1).getTime() - toDate(dat2).getTime();
        return dif / 1000 / 60 / 60 / 24;
    }

    private static String normalizeDate(String date) {
        int length = date.length();
        int year = guessYear(Integer.parseInt(date.substring(length - 2)));
        return date.substring(0, length - 2) + year;
    }

    private static final String DATE_FORMAT_MM_DD_YY = "(0?[1-9]|1[012])/(0?[1-9]|[12][0-9]|3[01])/(\\d\\d)";
    private static final String DATE_FORMAT_MM_DD_YYYY = "(0?[1-9]|1[012])/(0?[1-9]|[12][0-9]|3[01])/(\\d\\d\\d\\d)";
    private static final String DATE_FORMAT_YYYY_MM_DD = "(\\d\\d\\d\\d)-(\\d?\\d)-(\\d?\\d)";


    /**
     * Parses a string containing a date formatted as follows into an integer representation of that date.
     * <br><br>
     * <code>
     *     MM/DD/YY
     *     MM/DD/YYYY
     *     MM.DD.YYYY
     *     YYYY-MM-DD
     * </code>
     * @param date input date
     * @return int date or 0 on error
     */
    public static int parse(String date) {
        if (date == null  ||  date.trim().isEmpty())
            return 0;
        try {
            date = date.replaceAll("\\.", "/");

            if (date.matches(DATE_FORMAT_YYYY_MM_DD))
                return toInt(new SimpleDateFormat("yyyy-MM-dd").parse(date));
            else if (date.matches(DATE_FORMAT_MM_DD_YYYY))
                return toInt(new SimpleDateFormat("MM/dd/yyyy").parse(date));
            else if (date.matches(DATE_FORMAT_MM_DD_YY))
                return toInt(new SimpleDateFormat("MM/dd/yyyy").parse(normalizeDate(date)));
        } catch (ParseException ex) {
            return 0;
        }
        return 0;
    }

    public static void main(String [] argv) {
        System.out.println(parse("6/8/2018"));
    }

}
