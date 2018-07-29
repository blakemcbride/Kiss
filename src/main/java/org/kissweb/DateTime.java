package org.kissweb;

import java.text.SimpleDateFormat;
import java.util.Date;

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
        Date date = new Date();
        SimpleDateFormat df = new SimpleDateFormat("MM/dd/yyyy h:mm a");
        return df.format(date);
    }
}
