package org.kissweb;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.time.LocalDate;
import java.time.ZoneId;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TimeZone;

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
     * @return the current date as an integer in YYYYMMDD format
     */
    public static int today() {
        Calendar c = Calendar.getInstance();
        int y = c.get(Calendar.YEAR);
        int m = c.get(Calendar.MONTH) + 1;
        int d = c.get(Calendar.DAY_OF_MONTH);
        return y * 10000 + m * 100 + d;
    }

    /**
     * Returns the current date in the specified time zone as an integer formatted as YYYYMMDD.
     *
     * @param timeZoneId - the time zone to use when computing the current date
     * @return the current date as an integer formatted as YYYYMMDD in the specified time zone
     */
    public static int today(String timeZoneId) {
        // Get a calendar instance for the specified time zone
        Calendar c = Calendar.getInstance(TimeZone.getTimeZone(timeZoneId));
        int y = c.get(Calendar.YEAR);
        int m = c.get(Calendar.MONTH) + 1; // Months are zero-based in Calendar
        int d = c.get(Calendar.DAY_OF_MONTH);
        return y * 10000 + m * 100 + d;
    }

    /**
     * Return today's date as a <code>Date</code> instance.
     *
     * @return today's date as a Date object
     */
    public static Date todayDate() {
        return new Date();
    }


    /**
     * Return today's date as a <code>Calendar</code> instance.
     *
     * @return today's date as a Calendar object
     */
    public static Calendar todayCalendar() {
        return Calendar.getInstance();
    }

    /**
     * Return today's date as a <code>LocalDate</code> instance.
     *
     * @return today's date as a LocalDate object
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
     * @param y the year (2 or 4 digit)
     * @param m the month (1-12)
     * @param d the day (1-31)
     * @return the date formatted as YYYYMMDD, or 0 if month is 0
     */
    public static int toInt(int y, int m, int d) {
        if (m == 0)
            return 0;
        y = guessYear(y);
        return y * 10000 + m * 100 + d;
    }

    /**
     * convert from a <code>Date</code> instance to an <code>int</code> date.
     *
     * @param dat the Date object to convert
     * @return the date formatted as YYYYMMDD, or 0 if dat is null
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
     * @param date the Calendar object to convert
     * @return the date formatted as YYYYMMDD, or 0 if date is null
     */
    public static int toInt(Calendar date) {
        if (date == null)
            return 0;
        return toInt(date.getTime());
    }

    /**
     * convert from a <code>LocalDate</code> instance to an <code>int</code> date.
     *
     * @param dt the LocalDate object to convert
     * @return the date formatted as YYYYMMDD, or 0 if dt is null
     */
    public static int toInt(LocalDate dt) {
        if (dt == null)
            return 0;
        return toInt(dt.getYear(), dt.getMonthValue(), dt.getDayOfMonth());
    }

    /**
     * Create a <code>Date</code> from a year, month, and day.
     *
     * @param y the year (2 or 4 digit)
     * @param m the month (1-12)
     * @param d the day (1-31)
     * @return the Date object, or null if month is 0
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
     * @param dat a date formatted as YYYYMMDD
     * @return the Date object, or null if dat is 0
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
     * @param cal the Calendar object to convert
     * @return the Date object, or null if cal is null
     */
    public static Date toDate(Calendar cal) {
        return cal == null ? null : cal.getTime();
    }

    /**
     * convert from an <code>LocalDate</code> date to a <code>Date</code> date.
     *
     * @param dt the LocalDate object to convert
     * @return the Date object, or null if dt is null
     */    
    public static Date toDate(LocalDate dt) {
        return dt == null ? null : Date.from(dt.atStartOfDay(ZoneId.systemDefault()).toInstant());
    }

    /**
     * Create a <code>Calendar</code> from a year, month, and day.
     *
     * @param y the year (2 or 4 digit)
     * @param m the month (1-12)
     * @param d the day (1-31)
     * @return the Calendar object, or null if month is 0
     */
    public static Calendar toCalendar(int y, int m, int d) {
        if (m == 0)
            return null;
        y = guessYear(y);
        return new GregorianCalendar(y, m-1, d, 0, 0);
    }

    /**
     * convert from an <code>int</code> date to a <code>Calendar</code> date.
     *
     * @param dat a date formatted as YYYYMMDD
     * @return the Calendar object, or null if dat is 0
     */
    public static Calendar toCalendar(int dat) {
        return dat == 0 ? null : new GregorianCalendar(year(dat), month(dat)-1, day(dat), 0, 0);
    }

    /**
     * Convert from a <code>Date</code> date to a <code>Calendar</code> date.
     *
     * @param date the Date object to convert
     * @return the Calendar object, or null if date is null
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
     * @param dt the LocalDate object to convert
     * @return the Calendar object, or null if dt is null
     */
    public static Calendar toCalendar(LocalDate dt) {
        if (dt == null)
            return null;
        return toCalendar(toDate(dt));
    }

    /**
     * Create a <code>LocalDate</code> from a year, month, and day.
     *
     * @param y the year (2 or 4 digit)
     * @param m the month (1-12)
     * @param d the day (1-31)
     * @return the LocalDate object, or null if month is 0
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
     * @param dt a date formatted as YYYYMMDD
     * @return the LocalDate object, or null if dt is 0
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
     * @param date the Date object to convert
     * @return the LocalDate object, or null if date is null
     */
    public static LocalDate toLocalDate(Date date) {
        return date == null ? null : date.toInstant().atZone(ZoneId.systemDefault()).toLocalDate();
    }

    /**
     * Convert from a <code>Calendar</code> date to a <code>LocalDate</code>
     *
     * @param dt a Calendar object
     * @return the LocalDate object, or null if dt is null
     */
    public static LocalDate toLocalDate(Calendar dt) {
        return dt == null ? null : toLocalDate(toDate(dt));
    }

    /**
     * Returns year portion of a date.
     *
     * @param dt YYYYMMDD or YYMMDD
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
     * @param dt a date formatted as YYYYMMDD
     * @param n the number of days to add, may be negative
     * @return the new date formatted as YYYYMMDD, or 0 if dt is 0
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
     * @param dt a date formatted as YYYYMMDD
     * @param n the number of months to add, may be negative
     * @return the new date formatted as YYYYMMDD, or 0 if dt is 0
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
     * @param dt a date formatted as YYYYMMDD
     * @param n the number of years to add, may be negative
     * @return the new date formatted as YYYYMMDD
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
     * @param dt a date formatted as YYYYMMDD
     * @return the formatted date string, or empty string if dt is 0
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
     * @param dt a date formatted as YYYYMMDD
     * @return the formatted date string as "m/d/yy"
     */
    public static String format2(int dt) {
        return format("M/d/yy", dt);
    }

    /**
     * Format a date with a four digit year as "m/d/yyyy"
     *
     * @param dt a date formatted as YYYYMMDD
     * @return the formatted date string as "m/d/yyyy"
     */
    public static String format4(int dt) {
        return format("M/d/yyyy", dt);
    }

    /**
     * Format a date as "YYYY-MM-DD"
     *
     * @param dt a date formatted as YYYYMMDD
     * @return the formatted date string as "YYYY-MM-DD"
     */
    public static String formatSQL(int dt) {
        return format("yyyy-MM-dd", dt);
    }

    /**
     * Format date as "Jan 3, 2018"
     *
     * @param dt a date formatted as YYYYMMDD
     * @return the formatted date string as "MMM d, yyyy"
     *
     * @see #format2(int)
     * @see #format4(int)
     */
    public static String formatLong(int dt) {
        return format("MMM d, yyyy", dt);
    }

    /**
     * Format date as "Wed, Jan 3, 2018"
     *
     * @param dt a date formatted as YYYYMMDD
     * @return the formatted date string as "DDD, MMM d, yyyy"
     */
    public static String formatLongWithWeekDay(int dt) {
        return StringUtils.take(dayOfWeekName(dt), 3) + ", " + format("MMM d, yyyy", dt);
   }

    /**
     * Return the number of days between two dates.
     * (dat1 - dat2)
     *
     * @param dat1 the first date formatted as YYYYMMDD
     * @param dat2 the second date formatted as YYYYMMDD
     * @return the number of days between the two dates (dat1 - dat2)
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

    /**
     * Convert a date into the number of days since a certain date (julian date)
     *
     * @param dt YYYYMMDD
     * @return number of days since some early start date, or dt if dt &lt;= 0
     */
    public static long julian(int dt) {
        /* This can't be done some of the more obvious ways because of changes in daylight savings time.  */
        long d, y, m;

        if (dt <= 0)
            return dt;
        y = dt / 10000L;
        m = (dt % 10000L) / 100L;
        d = dt % 100L;
        d += (long) (.5 + (m - 1L) * 30.57);
        if (m >	2L) {
            d--;
            if (0L != y % 400L && (0L != y % 4L || 0L == y % 100L))
                d--;
        }
        d += (long) (365.25 * --y);
        d += y / 400L;
        d -= y / 100L;
        return d;
    }

    /**
     * Convert a julian date back into a standard date.
     *
     * @param d the julian date (number of days since epoch)
     * @return YYYYMMDD formatted date, or 0 if d &lt;= 0
     */
    public static int calendar(long d)
    {
        long	y, m, t;

        if (d <= 0L)
            return 0;
        y = (long)(1.0 + d / 365.2425);
        t = y -	1L;
        d -= (long) (t * 365.25);
        d -= t / 400L;
        d += t / 100L;
        if (d >	59L  &&	 0L != y % 400L	 &&  (0L != y %	4  ||  0L == y % 100L))
            d++;
        if (d >	60L)
            d++;
        m = (long)((d + 30L) / 30.57);
        d -= (long) Math.floor(.5 + (m - 1L) * 30.57);
        if (m == 13)  {
            m = 1;
            ++y;
        }  else  if (m == 0L)  {
            m = 12;
            --y;
        }
        return 10000 * (int) y + (int) m * 100 + (int) d;
    }

    /**
     * Returns the day of week number as follows:<br>
     * 0 = Sunday<br>
     * 1 = Monday<br>
     * 2 = Tuesday<br>
     * 3 = Wednesday<br>
     * 4 = Thursday<br>
     * 5 = Friday<br>
     * 6 = Saturday<br>
     * <br>
     * -1 = Invalid date
     *
     * @param dt YYYYMMDD
     * @return the day of week number (0-6), or -1 for invalid date
     */
    public static int dayOfWeek(int dt) {
        if (dt <= 0)
            return -1;
        return (int)(julian(dt) % 7L);
    }


    /**
     * Returns the full name of the week day.
     *
     * @param dt YYYYMMDD
     * @return the full name of the week day, or empty string for invalid date
     */
    public static String dayOfWeekName(int dt) {
        if (dt <= 0)
            return "";
        switch ((int)(julian(dt) % 7L)) {
            case 0:  return "Sunday";
            case 1:  return "Monday";
            case 2:  return "Tuesday";
            case 3:  return "Wednesday";
            case 4:  return "Thursday";
            case 5:  return "Friday";
            case 6:  return "Saturday";
            default:  return "";
        }
    }

    /**
     * Returns the full name of the week day.
     *
     * @param dt the Date object
     * @return the full name of the week day, or empty string for invalid date
     */
    public static String dayOfWeekName(Date dt) {
        return dayOfWeekName(toInt(dt));
    }

    /**
     * Returns the full name of the month of the year.
     *
     * @param dt YYYYMMDD
     * @return the full name of the month, or empty string for invalid date
     */
    public static String monthOfYearName(int dt) {
        if (dt <= 0)
            return "";
        switch (month(dt)) {
            case 1:  return "January";
            case 2:  return "February";
            case 3:  return "March";
            case 4:  return "April";
            case 5:  return "May";
            case 6:  return "June";
            case 7:  return "July";
            case 8:  return "August";
            case 9:  return "September";
            case 10:  return "October";
            case 11:  return "November";
            case 12:  return "December";
            default:  return "";
        }
    }

    /**
     * Main method for testing date utilities.
     *
     * @param argv command line arguments (not used)
     */
    public static void main(String [] argv) {
        int dt = 20230220;
        long jdt = julian(dt);
        int dt2 = calendar(jdt);
        System.out.println(dt);
        System.out.println(jdt);
        System.out.println(dt2);
    }

}
