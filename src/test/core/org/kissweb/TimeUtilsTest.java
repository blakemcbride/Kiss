package org.kissweb;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.Calendar;

public class TimeUtilsTest {

    @Test
    public void testNow() {
        int currentHour = Calendar.getInstance().get(Calendar.HOUR_OF_DAY);
        int currentMinute = Calendar.getInstance().get(Calendar.MINUTE);
        int expectedTime = currentHour * 100 + currentMinute;

        // Allow for slight time difference during execution
        int actualTime = TimeUtils.now();
        assertTrue(Math.abs(actualTime - expectedTime) < 2);
    }

    @Test
    public void testFormatMilitary() {
        assertEquals("14:30", TimeUtils.formatMilitary(1430));
        assertEquals("03:05", TimeUtils.formatMilitary(305));
        assertEquals("23:59", TimeUtils.formatMilitary(2359));
    }

    @Test
    public void testFormatAMPM() {
        assertEquals("2:30 PM", TimeUtils.formatAMPM(1430));
        assertEquals("3:05 AM", TimeUtils.formatAMPM(305));
        assertEquals("11:59 PM", TimeUtils.formatAMPM(2359));
        assertEquals("12:00 AM", TimeUtils.formatAMPM(0));
    }

    @Test
    public void testHour() {
        assertEquals(14, TimeUtils.hour(1430));
        assertEquals(3, TimeUtils.hour(305));
        assertEquals(23, TimeUtils.hour(2359));
    }

    @Test
    public void testMinutes() {
        assertEquals(30, TimeUtils.minutes(1430));
        assertEquals(5, TimeUtils.minutes(305));
        assertEquals(59, TimeUtils.minutes(2359));
    }

    @Test
    public void testParseSimpleTimes() {
        assertEquals(330, TimeUtils.parse("330"));
        assertEquals(330, TimeUtils.parse("03:30"));
        assertEquals(805, TimeUtils.parse("805"));
        assertEquals(1430, TimeUtils.parse("2:30 PM"));
        assertEquals(1430, TimeUtils.parse("2:30pm"));
        assertEquals(1430, TimeUtils.parse("230 PM"));
        assertEquals(230, TimeUtils.parse("2:30 AM"));
        assertEquals(0, TimeUtils.parse("12:00 AM"));
        assertEquals(1200, TimeUtils.parse("12:00 PM"));
    }

}

