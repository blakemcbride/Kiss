package org.kissweb;

import java.time.*;
import java.util.Date;

/**
 * This class represents date/times as a single long YYYYMMDDHHMM.
 * There is no timezone attached to it.
 */
public class DateTimeUtils {

    /**
     * Private constructor to prevent instantiation.
     * This is a utility class containing only static methods.
     */
    private DateTimeUtils() {
        // Prevent instantiation
    }

    /**
     * Returns the current date and time as a long in the format YYYYMMDDHHMM.
     *
     * @return the current date and time as a long in the format YYYYMMDDHHMM
     */
    public static long now() {
        return create(new Date());
    }

    /**
     * Creates a compact long date/time from individual date and time components.
     *
     * The returned long is in the format YYYYMMDDHHMM.
     *
     * @param date the date component (YYYYMMDD)
     * @param time the time component (HHMM)
     * @return a compact long date/time
     */
    public static long create(int date, int time) {
        return (long) date * 10000L + (long) time;
    }

    /**
     * Convert an epoch (UTC) + IANA timezone into a compact long YYYYMMDDHHMM
     * in the specified timezone.
     *
     * @param epoch       epoch time; seconds or milliseconds (auto-detected)
     * @param zoneIdStr   IANA timezone, e.g. "America/Chicago", "Europe/Berlin" or null for local
     * @return            long like 202501020305 (YYYYMMDDHHMM)
     * @throws IllegalArgumentException if zoneId is invalid
     */
    public static long create(long epoch, String zoneIdStr) {
        if (epoch == 0)
            return 0;
        final ZoneId zone;
        if (zoneIdStr == null || zoneIdStr.isBlank())
            zoneIdStr = ZoneId.systemDefault().getId();
        try {
            zone = ZoneId.of(zoneIdStr);
        } catch (DateTimeException dte) {
            throw new IllegalArgumentException("Invalid timezone: " + zoneIdStr, dte);
        }

        // Heuristic: if it's smaller than ~2001-09-09 in ms, treat as seconds.
        final Instant instant = (epoch < 1_000_000_000_000L)
                ? Instant.ofEpochSecond(epoch)
                : Instant.ofEpochMilli(epoch);

        final LocalDateTime ldt = LocalDateTime.ofInstant(instant, zone);

        final int year   = ldt.getYear();          // e.g., 2025
        final int month  = ldt.getMonthValue();    // 1..12
        final int day    = ldt.getDayOfMonth();    // 1..31
        final int hour   = ldt.getHour();          // 0..23
        final int minute = ldt.getMinute();        // 0..59

        // Build YYYYMMDDHHMM arithmetically (no formatting/parsing overhead).
        return year * 100000000L
                + month * 1000000L
                + day   * 10000L
                + hour  * 100L
                + minute;
    }

    /**
     * Convert a Date (UTC internally) into YYYYMMDDHHMM
     * in the specified timezone. If the timezone is null,
     * the system default timezone is used.
     *
     * @param date       java.util.Date instance (represents UTC instant)
     * @param zoneIdStr  IANA zone ID, e.g. "America/Chicago" or null for local
     * @return           long like 202501020305 (YYYYMMDDHHMM)
     */
    public static long create(Date date, String zoneIdStr) {
        if (date == null)
            return 0;

        final ZoneId zone;
        if (zoneIdStr == null || zoneIdStr.isBlank()) {
            zone = ZoneId.systemDefault();
        } else {
            try {
                zone = ZoneId.of(zoneIdStr);
            } catch (DateTimeException dte) {
                throw new IllegalArgumentException("Invalid timezone: " + zoneIdStr, dte);
            }
        }

        Instant instant = date.toInstant();
        LocalDateTime ldt = LocalDateTime.ofInstant(instant, zone);

        int year   = ldt.getYear();
        int month  = ldt.getMonthValue();
        int day    = ldt.getDayOfMonth();
        int hour   = ldt.getHour();
        int minute = ldt.getMinute();

        return year * 100000000L
                + month * 1000000L
                + day   * 10000L
                + hour  * 100L
                + minute;
    }

    /**
     * Convert a LocalDateTime into YYYYMMDDHHMM.
     *
     * @param ldt the LocalDateTime to convert
     * @return the YYYYMMDDHHMM representation
     */
    public static long create(LocalDateTime ldt) {
        int year   = ldt.getYear();
        int month  = ldt.getMonthValue();
        int day    = ldt.getDayOfMonth();
        int hour   = ldt.getHour();
        int minute = ldt.getMinute();

        return year * 100000000L
                + month * 1000000L
                + day   * 10000L
                + hour  * 100L
                + minute;
    }

    /**
     * Convert a long of the form YYYYMMDDHHMM (in the given timezone)
     * into an epoch time in milliseconds (UTC).
     *
     * @param ymdhm     date/time as YYYYMMDDHHMM (e.g., 202501020305)
     * @param zoneIdStr IANA timezone (e.g., "America/Chicago") or null for local
     * @return          epoch time in milliseconds since 1970-01-01T00:00:00Z
     */
    public static long toEpoch(long ymdhm, String zoneIdStr) {
        if (ymdhm == 0)
            return 0;
        // Extract components arithmetically (faster and safer than String parsing)
        int minute = (int)(ymdhm % 100);          // last 2 digits
        int hour   = (int)((ymdhm / 100) % 100);  // next 2
        int day    = (int)((ymdhm / 10000) % 100);
        int month  = (int)((ymdhm / 1000000) % 100);
        int year   = (int)(ymdhm / 100000000);

        // Validate range to catch typos early
        if (month < 1 || month > 12 || day < 1 || day > 31 ||
                hour < 0 || hour > 23 || minute < 0 || minute > 59)
            throw new IllegalArgumentException("Invalid date/time: " + ymdhm);

        // Determine the zone
        final ZoneId zone;
        if (zoneIdStr == null || zoneIdStr.isBlank()) {
            zone = ZoneId.systemDefault();
        } else {
            try {
                zone = ZoneId.of(zoneIdStr);
            } catch (DateTimeException dte) {
                throw new IllegalArgumentException("Invalid timezone: " + zoneIdStr, dte);
            }
        }

        // Build a LocalDateTime and convert to epoch millis via ZonedDateTime
        LocalDateTime ldt = LocalDateTime.of(year, month, day, hour, minute);
        ZonedDateTime zdt = ldt.atZone(zone);

        return zdt.toInstant().toEpochMilli();
    }

    /**
     * Convert a long of the form YYYYMMDDHHMM (in the given timezone)
     * into an Instant.
     *
     * @param ymdhm     date/time as YYYYMMDDHHMM (e.g., 202501020305)
     * @param zone IANA timezone (e.g., "America/Chicago") or null for local
     * @return          Instant representing the date/time in the given timezone
     */
    public static Instant toInstant(long ymdhm, ZoneId zone) {
        int minute = (int)(ymdhm % 100);
        int hour   = (int)((ymdhm / 100) % 100);
        int day    = (int)((ymdhm / 10000) % 100);
        int month  = (int)((ymdhm / 1000000) % 100);
        int year   = (int)(ymdhm / 100000000);

        LocalDateTime ldt = LocalDateTime.of(year, month, day, hour, minute);
        ZonedDateTime zdt = ldt.atZone(zone);
        return zdt.toInstant();
    }

    /**
     * Convert a long of the form YYYYMMDDHHMM (in the given timezone)
     * into an Instant.
     *
     * @param ymdhm     date/time as YYYYMMDDHHMM (e.g., 202501020305)
     * @param zoneStr  IANA timezone (e.g., "America/Chicago") or null for local
     * @return          Instant representing the date/time in the given timezone
     */
    public static Instant toInstant(long ymdhm, String zoneStr) {
        if (zoneStr == null || zoneStr.isBlank())
            zoneStr = ZoneId.systemDefault().getId();
        return toInstant(ymdhm, ZoneId.of(zoneStr));
    }

    /**
     * Convert a Date into YYYYMMDDHHMM using the system default timezone.
     *
     * @param date  java.util.Date instance (represents UTC instant)
     * @return      long like 202501020305 (YYYYMMDDHHMM)
     */
    public static long create(Date date) {
        return create(date, null);
    }

    /**
     * Extracts the date part (YYYYMMDD) from a compact long YYYYMMDDHHMM.
     *
     * @param dt the compact long date/time
     * @return the date part (YYYYMMDD)
     */
    public static int getDate(long dt) {
        return (int) (dt / 10000L);
    }

    /**
     * Extracts the time part (HHMM) from a compact long YYYYMMDDHHMM.
     *
     * @param dt the compact long date/time
     * @return the time part (HHMM)
     */
    public static int getTime(long dt) {
        return (int) (dt % 10000L);
    }

    /**
     * Add years to a date/time given in the YYYYMMDDHHMM format.
     * This method takes into account DST and leap years.
     *
     * @param ymdhm the date/time as YYYYMMDDHHMM (e.g., 202501020305)
     * @param years the number of years to add, may be negative
     * @return the resulting date/time as YYYYMMDDHHMM
     */
    public static long addYears(long ymdhm, int years) {
        return compose(normalize(parse(ymdhm).plusYears(years)));
    }

    /**
     * Add months to a date/time given in the YYYYMMDDHHMM format.
     * This method takes into account DST and leap years.
     *
     * @param ymdhm the date/time as YYYYMMDDHHMM (e.g., 202501020305)
     * @param months the number of months to add, may be negative
     * @return the resulting date/time as YYYYMMDDHHMM
     */
    public static long addMonths(long ymdhm, int months) {
        return compose(normalize(parse(ymdhm).plusMonths(months)));
    }

    /**
     * Add days to a date/time given in the YYYYMMDDHHMM format.
     * This method takes into account DST and leap years.
     *
     * @param ymdhm the date/time as YYYYMMDDHHMM (e.g., 202501020305)
     * @param days the number of days to add, may be negative
     * @return the resulting date/time as YYYYMMDDHHMM
     */
    public static long addDays(long ymdhm, int days) {
        return compose(normalize(parse(ymdhm).plusDays(days)));
    }

    /**
     * Add hours to a date/time given in the YYYYMMDDHHMM format.
     * This method takes into account DST and leap years.
     *
     * @param ymdhm the date/time as YYYYMMDDHHMM (e.g., 202501020305)
     * @param hours the number of hours to add, may be negative
     * @return the new date/time as YYYYMMDDHHMM, or the original if ymdhm is 0
     */
    public static long addHours(long ymdhm, int hours) {
        return compose(normalize(parse(ymdhm).plusHours(hours)));
    }

    /**
     * Add minutes to a date/time given in the YYYYMMDDHHMM format.
     * This method takes into account DST and leap years.
     *
     * @param ymdhm the date/time as YYYYMMDDHHMM (e.g., 202501020305)
     * @param minutes the number of minutes to add, may be negative
     * @return the new date/time as YYYYMMDDHHMM, or the original if ymdhm is 0
     */
    public static long addMinutes(long ymdhm, int minutes) {
        return compose(normalize(parse(ymdhm).plusMinutes(minutes)));
    }

    // --- Helpers ---

    // Normalizes through the system zone to handle DST gaps/overlaps correctly
    private static LocalDateTime normalize(LocalDateTime ldt) {
        ZonedDateTime zdt = ldt.atZone(ZoneId.systemDefault()); // handles spring-forward and fall-back transitions
        return zdt.toLocalDateTime();
    }

    private static LocalDateTime parse(long ymdhm) {
        int minute = (int)(ymdhm % 100);
        int hour   = (int)(ymdhm / 100 % 100);
        int day    = (int)(ymdhm / 10000 % 100);
        int month  = (int)(ymdhm / 1000000 % 100);
        int year   = (int)(ymdhm / 100000000);
        return LocalDateTime.of(year, month, day, hour, minute);
    }

    private static long compose(LocalDateTime ldt) {
        return ldt.getYear() * 100000000L
                + ldt.getMonthValue() * 1000000L
                + ldt.getDayOfMonth() * 10000L
                + ldt.getHour() * 100L
                + ldt.getMinute();
    }

    /**
     * Compute the difference in minutes between two dates/times given in the YYYYMMDDHHMM format.
     * The two dates/times are assumed to be in the same timezone, which can be specified
     * using the zoneIdStr parameter.  If zoneIdStr is null or blank, the system default
     * timezone is used.
     *
     * @param fromYmdhm the starting date/time (in YYYYMMDDHHMM format)
     * @param toYmdhm the ending date/time (in YYYYMMDDHHMM format)
     * @param zoneIdStr the timezone ID to use (e.g. "America/Chicago"), or null for local system zone
     * @return the difference in minutes between the two dates/times
     */
    public static long diffMinutes(long fromYmdhm, long toYmdhm, String zoneIdStr) {
        // Choose the zone (null or blank means local system zone)
        ZoneId zone = (zoneIdStr == null || zoneIdStr.isBlank())
                ? ZoneId.systemDefault()
                : ZoneId.of(zoneIdStr);

        // Helper to convert YYYYMMDDHHMM → Instant
        Instant toInstant = toInstant(toYmdhm, zone);
        Instant fromInstant = toInstant(fromYmdhm, zone);

        // Duration between the two instants
        return Duration.between(fromInstant, toInstant).toMinutes();
    }

}
