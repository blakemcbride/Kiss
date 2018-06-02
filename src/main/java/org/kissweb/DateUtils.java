package org.kissweb;

import java.util.Calendar;

/**
 * User: Blake McBride
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
