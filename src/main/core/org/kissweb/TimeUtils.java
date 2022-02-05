/*
 * Author: Blake McBride
 * Date: 9/23/20
 */

package org.kissweb;

import java.util.Calendar;

/**
 * TimeUtils class.  Deals with time represented as an int formatted as HHMM.
 */
public class TimeUtils {

    /**
     * Returns the current time of day in the format HHMM since midnight.
     */
    public static int now() {
        final Calendar cal = Calendar.getInstance();
        final int hour = cal.get(Calendar.HOUR_OF_DAY);
        final int min  = cal.get(Calendar.MINUTE);
        return hour * 100 + min;
    }

    /**
     * Format time as hh:mm (hours to 23)
     *
     * @param tm HHMM
     * @return
     */
    public static String formatMilitary(int tm) {
        final int min = tm % 100;
        final int hour = tm / 100;
        return hour + ":" + NumberFormat.Format(min, "Z", 2, 0);
    }

    /**
     * Format time as hh:mm AM/PM
     *
     * @param tm HHMM
     * @return
     */
    public static String formatAMPM(int tm) {
        final int min = tm % 100;
        int hour = tm / 100;
        String side;
        if (hour > 12) {
            hour -= 12;
            side = "PM";
        } else
            side = "AM";
        return hour + ":" + NumberFormat.Format(min, "Z", 2, 0) + ' ' + side;
    }

    /**
     * Return the hour portion of a time HHMM
     *
     * @param time HHMM
     * @return HH
     */
    public static int hour(int time) {
        return time / 100;
    }

    /**
     * Return the minute portion of a time HHMM
     *
     * @param time HHMM
     * @return  MM
     */
    public static int minutes(int time) {
        return time - (time / 100) * 100;
    }

    public static void main(String [] argv) {
        int tm = TimeUtils.now();
        System.out.println(tm);
        System.out.println(TimeUtils.formatMilitary(tm));
        System.out.println(TimeUtils.formatAMPM(tm));
    }
}
