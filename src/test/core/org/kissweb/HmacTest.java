package org.kissweb;

import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.HexFormat;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for {@link Hmac}, using RFC 4231 published HMAC-SHA256/384/512 test vectors (Test Cases 1
 * and 2), independently cross-checked against Python's standard-library {@code hmac}/{@code hashlib}
 * before being hardcoded here.
 */
class HmacTest {

    // RFC 4231 Test Case 1: Key = 0x0b * 20, Data = "Hi There"
    private static final byte[] KEY_1 = repeat((byte) 0x0b, 20);
    private static final String DATA_1 = "Hi There";
    private static final String CASE1_SHA256 = "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7";
    private static final String CASE1_SHA384 = "afd03944d84895626b0825f4ab46907f15f9dadbe4101ec682aa034c7cebc59cfaea9ea9076ede7f4af152e8b2fa9cb6";
    private static final String CASE1_SHA512 = "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854";

    // RFC 4231 Test Case 2: Key = "Jefe", Data = "what do ya want for nothing?"
    private static final byte[] KEY_2 = "Jefe".getBytes(StandardCharsets.US_ASCII);
    private static final String DATA_2 = "what do ya want for nothing?";
    private static final String CASE2_SHA256 = "5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843";
    private static final String CASE2_SHA384 = "af45d2e376484031617f78d2b58a6b1b9c7ef464f5a01b47e42ec3736322445e8e2240ca5e69e2c78b3239ecfab21649";
    private static final String CASE2_SHA512 = "164b7a7bfcf819e2e395fbe73b56e0a387bd64222e831fd610270cd7ea2505549758bf75c05a994a6d034f65f8f0e6fdcaeab1a34d4a6b4b636e070a38bce737";

    private static byte[] repeat(byte b, int n) {
        byte[] a = new byte[n];
        java.util.Arrays.fill(a, b);
        return a;
    }

    @Test
    void rfc4231_testCase1_hmacSha256() {
        assertEquals(CASE1_SHA256, Hmac.hmacSha256Hex(KEY_1, DATA_1));
    }

    @Test
    void rfc4231_testCase1_hmacSha384() {
        assertEquals(CASE1_SHA384, Hmac.hmacSha384Hex(KEY_1, DATA_1));
    }

    @Test
    void rfc4231_testCase1_hmacSha512() {
        assertEquals(CASE1_SHA512, Hmac.hmacSha512Hex(KEY_1, DATA_1));
    }

    @Test
    void rfc4231_testCase2_hmacSha256() {
        assertEquals(CASE2_SHA256, Hmac.hmacSha256Hex(KEY_2, DATA_2));
    }

    @Test
    void rfc4231_testCase2_hmacSha384() {
        assertEquals(CASE2_SHA384, Hmac.hmacSha384Hex(KEY_2, DATA_2));
    }

    @Test
    void rfc4231_testCase2_hmacSha512() {
        assertEquals(CASE2_SHA512, Hmac.hmacSha512Hex(KEY_2, DATA_2));
    }

    @Test
    void byteArrayAndStringMessageOverloads_agree() {
        byte[] dataBytes = DATA_1.getBytes(StandardCharsets.UTF_8);
        assertArrayEquals(Hmac.hmacSha256(KEY_1, dataBytes), Hmac.hmacSha256(KEY_1, DATA_1));
        assertArrayEquals(Hmac.hmacSha384(KEY_1, dataBytes), Hmac.hmacSha384(KEY_1, DATA_1));
        assertArrayEquals(Hmac.hmacSha512(KEY_1, dataBytes), Hmac.hmacSha512(KEY_1, DATA_1));
    }

    @Test
    void base64Encoding_decodesBackToSameMacBytes() throws Exception {
        String b64 = Hmac.hmacSha256Base64(KEY_1, DATA_1);
        assertArrayEquals(Hmac.hmacSha256(KEY_1, DATA_1), Base64.decode(b64));
    }

    @Test
    void hexEncoding_isLowercase() {
        String hex = Hmac.hmacSha256Hex(KEY_1, DATA_1);
        assertEquals(hex.toLowerCase(), hex);
    }

    @Test
    void macLengths_areCorrect() {
        assertEquals(32, Hmac.hmacSha256(KEY_1, DATA_1).length);
        assertEquals(48, Hmac.hmacSha384(KEY_1, DATA_1).length);
        assertEquals(64, Hmac.hmacSha512(KEY_1, DATA_1).length);
    }

    // ------------------------------------------------------------------
    // Blind-index pattern: deterministic (same key+message -> same tag), and key- / message-sensitive
    // ------------------------------------------------------------------

    @Test
    void blindIndex_isDeterministic_sameKeyAndMessage() {
        byte[] key = RandomUtil.randomBytes(32);
        String a = Hmac.hmacSha256Hex(key, "123-45-6789");
        String b = Hmac.hmacSha256Hex(key, "123-45-6789");
        assertEquals(a, b);
    }

    @Test
    void blindIndex_differsForDifferentMessage_sameKey() {
        byte[] key = RandomUtil.randomBytes(32);
        String a = Hmac.hmacSha256Hex(key, "123-45-6789");
        String b = Hmac.hmacSha256Hex(key, "123-45-6788");
        assertNotEquals(a, b);
    }

    @Test
    void blindIndex_differsForDifferentKey_sameMessage() {
        byte[] keyA = RandomUtil.randomBytes(32);
        byte[] keyB = RandomUtil.randomBytes(32);
        String a = Hmac.hmacSha256Hex(keyA, "123-45-6789");
        String b = Hmac.hmacSha256Hex(keyB, "123-45-6789");
        assertNotEquals(a, b, "different keys must not collide for the same message (key separation matters)");
    }
}
