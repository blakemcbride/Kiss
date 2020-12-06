package org.kissweb;

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.TimeZone;

/*
 * User: Blake McBride
 * Date: 7/28/18
 */

/**
 * Class dealing with dates with times.
 *
 */
public class DateTime {

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

}
