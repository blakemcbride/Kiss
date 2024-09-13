package org.kissweb;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class NumberUtilsTest {

    @Test
    public void testDoubleEqual() {
        assertTrue(NumberUtils.doubleEqual(1.000001, 1.000002, 0.00001));
        assertFalse(NumberUtils.doubleEqual(1.0001, 1.0002, 0.00001));
    }

    @Test
    public void testRound() {
        assertEquals(123.46, NumberUtils.round(123.456, 2), 0.0001);
        assertEquals(-123.46, NumberUtils.round(-123.456, 2), 0.0001);
    }

    @Test
    public void testRoundUp() {
        assertEquals(123.46, NumberUtils.roundUp(123.456, 2), 0.0001);
        assertEquals(-123.46, NumberUtils.roundUp(-123.456, 2), 0.0001);
    }

    @Test
    public void testRoundDown() {
        assertEquals(123.45, NumberUtils.roundDown(123.456, 2), 0.0001);
        assertEquals(-123.45, NumberUtils.roundDown(-123.456, 2), 0.0001);
    }

    @Test
    public void testTrunk() {
        assertEquals(123, NumberUtils.trunk(123.456), 0.0001);
        assertEquals(-124, NumberUtils.trunk(-123.456), 0.0001);
    }

    @Test
    public void testParseDouble() {
        assertEquals(45.0, NumberUtils.parseDouble("45"), 0.0001);
        assertEquals(0.0, NumberUtils.parseDouble("abc"), 0.0001);
        assertEquals(488678.98, NumberUtils.parseDouble("488,678.98"), 0.0001);
    }

    @Test
    public void testParseLong() {
        assertEquals(45L, NumberUtils.parseLong("45"));
        assertEquals(0L, NumberUtils.parseLong("abc"));
        assertEquals(488678L, NumberUtils.parseLong("488,678.98"));
    }

    @Test
    public void testIsValidNumber() {
        assertTrue(NumberUtils.isValidNumber("38"));
        assertTrue(NumberUtils.isValidNumber("38.17"));
        assertTrue(NumberUtils.isValidNumber("385,345.78"));
        assertFalse(NumberUtils.isValidNumber("38x"));
        assertFalse(NumberUtils.isValidNumber("38.5."));
    }

    @Test
    public void testParseInt() {
        assertEquals(45, NumberUtils.parseInt("45"));
        assertEquals(0, NumberUtils.parseInt("abc"));
        assertEquals(483345, NumberUtils.parseInt("483,345"));
    }
}
