package org.kissweb;

import static org.junit.jupiter.api.Assertions.*;
import org.junit.jupiter.api.Test;

public class NumberFormatTest {

    @Test
    public void testFormatBase10WithCommas() {
        // Testing number formatting with commas and 2 decimal places
        String formatted = NumberFormat.Format(12345.146, "C", 10, 2);
        assertEquals(" 12,345.15", formatted);
    }

    @Test
    public void testFormatLeftJustify() {
        // Testing left justification of the number
        String formatted = NumberFormat.Format(125.146, "CL", 10, 2);
        assertEquals("125.15    ", formatted);
    }

    @Test
    public void testFormatLargeNumber() {
        // Testing formatting a large number with commas
        String formatted = NumberFormat.Format(1236354545.146, "C", 0, 2);
        assertEquals("1,236,354,545.15", formatted);
    }

    @Test
    public void testFormatWithParenthesesForNegative() {
        // Testing number with parentheses for negative numbers
        String formatted = NumberFormat.Format(-5.146, "ZP", 10, 2);
        assertEquals("(00005.15)", formatted);
    }

    @Test
    public void testFormatZeroWithBlank() {
        // Testing zero with a blank mask
        String formatted = NumberFormat.Format(0, "CB", -1, -1);
        assertEquals(" ", formatted);
    }

    @Test
    public void testFormatWithDollarSign() {
        // Testing number with a dollar sign mask
        String formatted = NumberFormat.Format(5.146, "CD", 10, 2);
        assertEquals("     $5.15", formatted);
    }
}
