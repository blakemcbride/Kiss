package org.kissweb;

import java.io.IOException;
import java.util.regex.Pattern;
import java.nio.charset.CharsetDecoder;
import java.nio.charset.CharacterCodingException;
import java.nio.charset.CodingErrorAction;
import java.nio.charset.StandardCharsets;
import java.nio.ByteBuffer;

/**
 * Utility methods to deal with Base64.
 *
 * Author: Blake McBride
 * Date: 11/2/23
 */
public class Base64 {

    private static final java.util.Base64.Decoder base64Decoder = java.util.Base64.getDecoder();
    private static final java.util.Base64.Encoder base64Encoder = java.util.Base64.getEncoder();

    /**
     * Base64 encode
     *
     * @param ba the byte array to be converted to a base64 string
     * @return the base64 string representation of a byte array
     */
    public static String encode(byte [] ba) {
        return base64Encoder.encodeToString(ba);
    }

    /**
     * Base 64 decode
     *
     * @param s the base64 encoded string
     * @return the decoded byte array
     * @throws IOException
     */
    public static byte [] decode(String s) throws IOException {
        return base64Decoder.decode(s);
    }

    private static final Pattern BASE64_PATTERN = Pattern.compile(
        "^([A-Za-z0-9+/]{4})*" +   // Groups of 4 valid Base64 chars
        "([A-Za-z0-9+/]{2}==|" +         // Group of 2 valid Base64 chars followed by '=='
        "[A-Za-z0-9+/]{3}=)?" +          // Group of 3 valid Base64 chars followed by '='
        "$");

    /**
     * Checks if a given string is a valid Base64 string.
     * If <code>true</code> is returned, it does not guarantee that the input string is a base64 string.
     * It only guarantees that it is a valid base64 string.
     * <br><br>
     * For example: <code>isBase64("find") == true</code> because is can be interpreted as a base64
     * but isn't intended to be a base64 string.
     * <br><br>
     * So, this method does not tell you if a string is a base64 string.  It merely tells you if it can be interpreted
     * as a base64 string.
     *
     * @param  value  the string to be checked
     * @return        true if the string is a valid Base64 string, false otherwise
     */
    public static boolean mightBeBase64(String value) {
        if (value == null || value.isEmpty())
            return false;
        if ((value.length() % 4) != 0)
            return false; // Base64 string length should be multiple of 4
        return BASE64_PATTERN.matcher(value).matches();
    }

    /**
     * Determines whether the given content might be binary.
     *
     * @param  content  the content to be checked
     * @return          true if the content might be binary, false if it's likely to be text
     */
    public static boolean mightBeBinary(String content) {
        CharsetDecoder decoder = StandardCharsets.UTF_8.newDecoder();
        decoder.onMalformedInput(CodingErrorAction.REPORT);
        decoder.onUnmappableCharacter(CodingErrorAction.REPORT);

        try {
            // Attempt to decode the content as UTF-8
            decoder.decode(ByteBuffer.wrap(content.getBytes(StandardCharsets.UTF_8)));
        } catch (CharacterCodingException e) {
            // If a decoding error occurs, it is likely to be binary content
            return true;
        }

        // No decoding error, it's likely to be text content
        return false;
    }
}
