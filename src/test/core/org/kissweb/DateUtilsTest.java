package org.kissweb;

import org.junit.jupiter.api.Test;

import java.util.Calendar;
import java.util.Date;
import java.time.LocalDate;

import static org.junit.jupiter.api.Assertions.*;

class DateUtilsTest {

    @Test
    void testToday() {
        int today = DateUtils.today();
        assertNotNull(today);
        // Assuming the format is YYYYMMDD, we can do basic validation
        assertTrue(today > 20200101); // Just a basic check
    }

    @Test
    void testTodayDate() {
        Date todayDate = DateUtils.todayDate();
        assertNotNull(todayDate);
        assertEquals(new Date().toString(), todayDate.toString()); // They should be on the same day
    }

    @Test
    void testTodayCalendar() {
        Calendar todayCalendar = DateUtils.todayCalendar();
        assertNotNull(todayCalendar);
        assertEquals(Calendar.getInstance().get(Calendar.DAY_OF_YEAR), todayCalendar.get(Calendar.DAY_OF_YEAR));
    }

    @Test
    void testTodayLocalDate() {
        LocalDate todayLocalDate = DateUtils.todayLocalDate();
        assertNotNull(todayLocalDate);
        assertEquals(LocalDate.now(), todayLocalDate);
    }

    @Test
    void testToIntFromDate() {
        Date date = new Date();
        int dateInt = DateUtils.toInt(date);
        assertNotEquals(0, dateInt);
    }

    @Test
    void testToIntFromLocalDate() {
        LocalDate localDate = LocalDate.of(2023, 8, 29);
        int dateInt = DateUtils.toInt(localDate);
        assertEquals(20230829, dateInt);
    }

    @Test
    void testToDateFromInt() {
        int dateInt = 20230829;
        Date date = DateUtils.toDate(dateInt);
        assertNotNull(date);
        Calendar cal = Calendar.getInstance();
        cal.setTime(date);
        assertEquals(2023, cal.get(Calendar.YEAR));
        assertEquals(Calendar.AUGUST, cal.get(Calendar.MONTH));
        assertEquals(29, cal.get(Calendar.DAY_OF_MONTH));
    }

    @Test
    void testToCalendarFromInt() {
        int dateInt = 20230829;
        Calendar cal = DateUtils.toCalendar(dateInt);
        assertNotNull(cal);
        assertEquals(2023, cal.get(Calendar.YEAR));
        assertEquals(Calendar.AUGUST, cal.get(Calendar.MONTH));
        assertEquals(29, cal.get(Calendar.DAY_OF_MONTH));
    }

    @Test
    void testToLocalDateFromInt() {
        int dateInt = 20230829;
        LocalDate localDate = DateUtils.toLocalDate(dateInt);
        assertEquals(LocalDate.of(2023, 8, 29), localDate);
    }

    @Test
    void testAddDays() {
        int dateInt = 20230829;
        int newDate = DateUtils.addDays(dateInt, 5);
        assertEquals(20230903, newDate);
    }

    @Test
    void testAddMonths() {
        int dateInt = 20230829;
        int newDate = DateUtils.addMonths(dateInt, 2);
        assertEquals(20231029, newDate);
    }

    @Test
    void testAddYears() {
        int dateInt = 20230829;
        int newDate = DateUtils.addYears(dateInt, 1);
        assertEquals(20240829, newDate);
    }

    @Test
    void testFormat() {
        int dateInt = 20230829;
        String formattedDate = DateUtils.format("yyyy-MM-dd", dateInt);
        assertEquals("2023-08-29", formattedDate);
    }

    @Test
    void testDaysBetween() {
        int dateInt1 = 20230829;
        int dateInt2 = 20230824;
        long daysBetween = DateUtils.daysBetween(dateInt1, dateInt2);
        assertEquals(5, daysBetween);
    }

    @Test
    void testParse() {
        String dateStr = "08/29/2023";
        int dateInt = DateUtils.parse(dateStr);
        assertEquals(20230829, dateInt);
    }

    @Test
    void testJulian() {
        int dateInt = 20230829;
        long julianDate = DateUtils.julian(dateInt);
        assertTrue(julianDate > 0);
    }

    @Test
    void testCalendar() {
        int dateInt = 20230829;
        long julianDate = DateUtils.julian(dateInt);
        int convertedDate = DateUtils.calendar(julianDate);
        assertEquals(dateInt, convertedDate);
    }

    @Test
    void testDayOfWeek() {
        int dateInt = 20230829; // Tuesday
        int dayOfWeek = DateUtils.dayOfWeek(dateInt);
        assertEquals(2, dayOfWeek); // 2 = Tuesday
    }

    @Test
    void testDayOfWeekName() {
        int dateInt = 20230829; // Tuesday
        String dayOfWeekName = DateUtils.dayOfWeekName(dateInt);
        assertEquals("Tuesday", dayOfWeekName);
    }

    @Test
    void testMonthOfYearName() {
        int dateInt = 20230829;
        String monthName = DateUtils.monthOfYearName(dateInt);
        assertEquals("August", monthName);
    }
}
