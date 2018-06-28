package org.kissweb;

import java.util.Calendar;

/**
 * This class deals with dates in several formats including int, Date, and Calendar.
 * <code>int</code> dates are formatted as YYYYMMDD.  For example, June 8, 2018 would
 * be represented as 20180608.
 *
 * Author: Blake McBride
 * Date: 12/3/17
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
}
