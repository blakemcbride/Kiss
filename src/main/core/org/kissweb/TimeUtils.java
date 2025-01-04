/*
 * Author: Blake McBride
 * Date: 9/23/20
 */

package org.kissweb;

import java.util.Calendar;
import java.util.TimeZone;

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
     * Returns the current time in a given timezone in the format HHMM since midnight.
     * @param timeZoneId the timezone to use
     * @return the current time in the given timezone as an int formatted as HHMM
     */
    public static int now(String timeZoneId) {
        // Get a calendar instance for the specified time zone
        final Calendar cal = Calendar.getInstance(TimeZone.getTimeZone(timeZoneId));
        final int hour = cal.get(Calendar.HOUR_OF_DAY);
        final int min = cal.get(Calendar.MINUTE);
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
        return NumberFormat.Format(hour, "Z", 2, 0) + ":" + NumberFormat.Format(min, "Z", 2, 0);
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
        return (hour == 0 ? 12 : hour) + ":" + NumberFormat.Format(min, "Z", 2, 0) + ' ' + side;
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

    /**
     * Parse a string into an int representing a time in the form HHMM.
     * Intelligently parses the strings, for example:
     * <br><br>
     * "330" -&gt; 330<br>
     * "0330" -&gt; 330<br>
     * "3:30" -&gt; 330<br>
     * "3.30" -&gt; 330<br>
     * "230 pm" -&gt; 1430<br>
     * "14:30" -&gt; 1430<br>
     *
     * @param time
     * @return time as HHMM or -1 on error
     */
    public static int parse(String time) {
        if (time == null ||  time.isEmpty())
            return -1;
        char c;
        int hour;
        int minutes = 0;
        time = time.trim();
        final int len = time.length();
        final StringBuilder sb = new StringBuilder();
        int i = 0;  // index into string
        for ( ; i < len ; i++) {
            c = time.charAt(i);
            if (!Character.isDigit(c))
                break;
            sb.append(c);
        }
        if (i == 0 || i > 4)
            return -1;
        if (i == 3) {
            // one digit hour (HMM)
            sb.setLength(i=1);
        } else if (i == 4) {
            // two digits hour (HHMM)
            sb.setLength(i=2);
        }
        hour = Integer.parseInt(sb.toString());
        if (i >= len)
            return hour * 100;

        sb.setLength(0);
        c = time.charAt(i++);  // digit or [:-.] or [aApP] or space
        if (c == ':' || c == '.' || c == '-' || c == ' ') {
            if (i >= len)
                return -1;
            c = time.charAt(i++); // 1st digit of minutes or [aApP]
        }
        if (Character.isDigit(c)) {
            sb.append(c);
            if (i >= len) {
                minutes = Integer.parseInt(sb.toString());
                return hour * 100 + minutes;
            }
            c = time.charAt(i++);
            if (Character.isDigit(c))
                sb.append(c);
            minutes = Integer.parseInt(sb.toString());
            if (i >= len) {
                minutes = Integer.parseInt(sb.toString());
                return hour * 100 + minutes;
            }
            c = time.charAt(i++);
        }
        if (c == ' ')
            c = time.charAt(i);
        if (c == 'P' || c == 'p') {
            if (hour < 12)
                hour += 12;
        } else if (hour == 12)
            hour = 0;
        return hour * 100 + minutes;
    }

    private static void p(String s) {
        System.out.println("\"" + s + "\"  " + parse(s));
    }

    public static void main(String [] argv) {
        int tm = TimeUtils.now();
        System.out.println(tm);
        System.out.println(TimeUtils.formatMilitary(tm));
        System.out.println(TimeUtils.formatAMPM(tm));

        System.out.println();
        p("330");
        p("330p");
        p("3:30");
        p("3:30 AM");
        p("3:30 PM");
        p("3:30PM");
        p("2134");
        p("1234");
        p("805");
        p("8a");
        p("8p");
    }
}
