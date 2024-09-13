package org.kissweb;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;
import java.util.Vector;

public class StringUtilsTest {

    @Test
    public void testSubstring() {
        assertEquals("ello", StringUtils.substring("Hello", 1, 4));
        assertEquals("World", StringUtils.substring("Hello World", 6, 5));
    }

    @Test
    public void testTakePositive() {
        assertEquals("Hello", StringUtils.take("Hello", 5));
        assertEquals("Hel  ", StringUtils.take("Hel", 5));
        assertEquals("Hi", StringUtils.take("Hi there", 2));
    }

    @Test
    public void testTakeNegative() {
        assertEquals("lo", StringUtils.take("Hello", -2));
        assertEquals("  Hi", StringUtils.take("Hi", -4));
    }

    @Test
    public void testDrop() {
        assertEquals("lo", StringUtils.drop("Hello", 3));
        assertEquals("Hel", StringUtils.drop("Hello", -2));
        assertEquals("", StringUtils.drop("Hi", 10));
    }

    @Test
    public void testRightStrip() {
        assertEquals("Hello", StringUtils.rightStrip("Hello   "));
        assertEquals("Hello World", StringUtils.rightStrip("Hello World   "));
        assertEquals("NoSpace", StringUtils.rightStrip("NoSpace"));
    }

    @Test
    public void testJoin() {
        String[] input = {"Hello", "World"};
        assertEquals("Hello,World", StringUtils.join(input, ","));
        assertEquals("Hello World", StringUtils.join(input, " "));
    }

    @Test
    public void testIsEmpty() {
        assertTrue(StringUtils.isEmpty(""));
        assertTrue(StringUtils.isEmpty(null));
        assertFalse(StringUtils.isEmpty("Hello"));
    }

    @Test
    public void testStringToCharacter() {
        assertEquals(Character.valueOf('H'), StringUtils.stringToCharacter("Hello"));
        assertNull(StringUtils.stringToCharacter(""));
        assertNull(StringUtils.stringToCharacter(null));
    }

    @Test
    public void testCharacterToString() {
        assertEquals("H", StringUtils.characterToString('H'));
        assertNull(StringUtils.characterToString(null));
    }

    @Test
    public void testSplit() {
        String[] expected = {"Hello", "World"};
        assertArrayEquals(expected, StringUtils.split("Hello World", " "));
        assertArrayEquals(new String[0], StringUtils.split("", " "));
    }

    @Test
    public void testHtmlToText() {
        assertEquals("Hello\nWorld", StringUtils.htmlToText("Hello<br>World"));
        assertEquals(" Test ", StringUtils.htmlToText("<b>Test</b>"));
        assertEquals(" ", StringUtils.htmlToText("&nbsp;"));
    }

    @Test
    public void testFindClosestMatch() {
        Vector<String> vector = new Vector<>();
        vector.add("apple");
        vector.add("banana");
        vector.add("grape");
        String input = "grap";
        assertEquals("grape", StringUtils.findClosestMatch(vector, input));
    }

    @Test
    public void testToAscii() {
        assertEquals("aeiou", StringUtils.toAscii("áéíóú"));
        assertEquals("ASCII test", StringUtils.toAscii("ASCII test"));
        assertEquals("(c) (r)", StringUtils.toAscii("\u00A9 \u00AE"));
    }
}
