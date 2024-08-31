package org.kissweb;

import org.junit.jupiter.api.Test;

import java.time.ZoneId;
import java.time.ZonedDateTime;
import java.util.Calendar;
import java.util.Date;
import java.util.GregorianCalendar;

import static org.junit.jupiter.api.Assertions.*;

class DateTimeTest {

    @Test
    void testConstructorWithZonedDateTime() {
        ZonedDateTime zdt = ZonedDateTime.of(2023, 8, 29, 14, 30, 0, 0, ZoneId.systemDefault());
        DateTime dt = new DateTime(zdt);

        assertEquals(2023, dt.getYear());
        assertEquals(8, dt.getMonth());
        assertEquals(29, dt.getDay());
        assertEquals(14, dt.getHour());
        assertEquals(30, dt.getMinute());
    }

    @Test
    void testConstructorWithIntDateAndTime() {
        DateTime dt = new DateTime(20201224, 1130);

        assertEquals(2020, dt.getYear());
        assertEquals(12, dt.getMonth());
        assertEquals(24, dt.getDay());
        assertEquals(11, dt.getHour());
        assertEquals(30, dt.getMinute());
    }

    @Test
    void testConstructorWithDate() {
        Date date = new GregorianCalendar(2021, Calendar.JANUARY, 1, 0, 0).getTime();
        DateTime dt = new DateTime(date);

        assertEquals(2021, dt.getYear());
        assertEquals(1, dt.getMonth());
        assertEquals(1, dt.getDay());
        assertEquals(0, dt.getHour());
        assertEquals(0, dt.getMinute());
    }

    @Test
    void testConstructorWithGregorianCalendar() {
        GregorianCalendar gc = new GregorianCalendar(2020, Calendar.DECEMBER, 24, 11, 30);
        DateTime dt = new DateTime(gc);

        assertEquals(2020, dt.getYear());
        assertEquals(12, dt.getMonth());
        assertEquals(24, dt.getDay());
        assertEquals(11, dt.getHour());
        assertEquals(30, dt.getMinute());
    }

    @Test
    void testNow() {
        DateTime dt = DateTime.now();

        assertNotNull(dt);
        assertEquals(ZonedDateTime.now().getYear(), dt.getYear());
    }

    @Test
    void testGetIntDate() {
        DateTime dt = new DateTime(20201224, 1130);

        assertEquals(20201224, dt.getIntDate());
    }

    @Test
    void testGetIntTime() {
        DateTime dt = new DateTime(20201224, 1130);

        assertEquals(1130, dt.getIntTime());
    }

    @Test
    void testFormat() {
        DateTime dt = new DateTime(20201224, 1130);

        String expected = "12/24/2020 11:30 AM";
        assertEquals(expected, dt.format());
    }

    @Test
    void testAddDays() {
        DateTime dt = new DateTime(20201224, 1130);

        dt.addDays(5);
        assertEquals(20201229, dt.getIntDate());

        dt.addDays(-10);
        assertEquals(20201219, dt.getIntDate());
    }

    @Test
    void testStaticFormatDate() {
        Date date = new GregorianCalendar(2021, Calendar.JANUARY, 1, 0, 0).getTime();
        String expected = "01/01/2021 12:00 AM";

        assertEquals(expected, DateTime.format(date));
    }

    @Test
    void testStaticFormatDateWithFormat() {
        Date date = new GregorianCalendar(2021, Calendar.JANUARY, 1, 0, 0).getTime();
        String expected = "2021-01-01";

        assertEquals(expected, DateTime.format(date, "yyyy-MM-dd"));
    }

    @Test
    void testStaticFormatDateWithTimeZone() {
        Date date = new GregorianCalendar(2021, Calendar.JANUARY, 1, 0, 0).getTime();
        String expected = "01/01/2021 12:00 AM CST";

        assertEquals(expected, DateTime.formatTZ(date));
    }

    @Test
    void testCurrentDateTimeFormatted() {
        String formattedDate = DateTime.currentDateTimeFormatted();

        assertNotNull(formattedDate);
        assertTrue(formattedDate.matches("\\d{2}/\\d{2}/\\d{4} \\d{1,2}:\\d{2} (AM|PM)"));
    }

    @Test
    void testCurrentDateTimeFormattedWithFormat() {
        String dateFormat = "yyyy-MM-dd HH:mm:ss";
        String formattedDate = DateTime.currentDateTimeFormatted(dateFormat);

        assertNotNull(formattedDate);
        assertTrue(formattedDate.matches("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"));
    }

    @Test
    void testCurrentDateTimeFormattedWithFormatAndTimeZone() {
        String dateFormat = "yyyy-MM-dd HH:mm:ss";
        String timeZone = "UTC";
        String formattedDate = DateTime.currentDateTimeFormatted(dateFormat, timeZone);

        assertNotNull(formattedDate);
        assertTrue(formattedDate.matches("\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}"));
    }
}
