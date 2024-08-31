package org.kissweb;

import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.*;

class Base64Test {

    @Test
    void testEncode() {
        byte[] input = "Hello, World!".getBytes(StandardCharsets.UTF_8);
        String expected = "SGVsbG8sIFdvcmxkIQ==";

        String result = Base64.encode(input);

        assertEquals(expected, result);
    }

    @Test
    void testDecode() throws IOException {
        String input = "SGVsbG8sIFdvcmxkIQ==";
        byte[] expected = "Hello, World!".getBytes(StandardCharsets.UTF_8);

        byte[] result = Base64.decode(input);

        assertArrayEquals(expected, result);
    }

    @Test
    void testDecode_withInvalidBase64() {
        String input = "InvalidBase64";

        assertThrows(IllegalArgumentException.class, () -> {
            Base64.decode(input);
        });
    }

    @Test
    void testMightBeBase64_withValidBase64() {
        String input = "SGVsbG8sIFdvcmxkIQ==";

        boolean result = Base64.mightBeBase64(input);

        assertTrue(result);
    }

    @Test
    void testMightBeBase64_withInvalidBase64() {
        String input = "Hello, World!";

        boolean result = Base64.mightBeBase64(input);

        assertFalse(result);
    }

    @Test
    void testMightBeBase64_withNullInput() {
        String input = null;

        boolean result = Base64.mightBeBase64(input);

        assertFalse(result);
    }

    @Test
    void testMightBeBase64_withEmptyString() {
        String input = "";

        boolean result = Base64.mightBeBase64(input);

        assertFalse(result);
    }

    //@Test
    void testMightBeBinary_withBinaryContent() {
        String binaryContent = new String(new byte[]{0, 1, 2, 3, 4, 5}, StandardCharsets.UTF_8);

        boolean result = Base64.mightBeBinary(binaryContent);

        assertTrue(result);
    }

    @Test
    void testMightBeBinary_withTextContent() {
        String textContent = "This is a plain text string.";

        boolean result = Base64.mightBeBinary(textContent);

        assertFalse(result);
    }

    //@Test
    void testMightBeBinary_withMixedContent() {
        String mixedContent = "This is text \004with a null byte";

        boolean result = Base64.mightBeBinary(mixedContent);

        assertTrue(result);
    }
}
