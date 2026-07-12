package org.kissweb;

import org.junit.jupiter.api.Test;

import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import java.nio.charset.StandardCharsets;
import java.util.HexFormat;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * Known-answer tests for PBKDF2-HMAC-SHA256 - the key-derivation primitive both {@link Crypto} (see
 * its password-based construction path) and {@link PasswordHash} are built on, via the JDK's
 * {@code SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256")}. Neither class exposes its raw derived
 * key bytes publicly (by design - {@code Crypto} only ever exposes the final AEAD ciphertext, and
 * {@code PasswordHash} only its self-describing stored-hash string), so this test exercises the shared
 * underlying JDK primitive directly.
 * <br><br>
 * Test cases use password {@code "password"} / salt {@code "salt"} at iteration counts 1, 2, and 4096,
 * with a 32-byte (256-bit) derived key length (matching both classes' derived-key size). The expected
 * hex values are the 32-byte PBKDF2-HMAC-SHA256 outputs for those inputs; their first 20 bytes match
 * the independently-published test vectors for the same (password, salt, iteration count) at a 20-byte
 * derived-key length (PBKDF2's output is a simple concatenation of one-block-at-a-time HMAC output, so
 * a longer derived key is always an extension of a shorter one for the same inputs) - cross-checked
 * against Python's standard-library {@code hashlib.pbkdf2_hmac} before being hardcoded here.
 */
class Pbkdf2Test {

    private static final String ALGORITHM = "PBKDF2WithHmacSHA256";
    private static final int KEY_BITS = 256; // 32 bytes, matching Crypto/PasswordHash

    private static byte[] derive(String password, String salt, int iterations) throws Exception {
        PBEKeySpec spec = new PBEKeySpec(password.toCharArray(), salt.getBytes(StandardCharsets.UTF_8), iterations, KEY_BITS);
        try {
            SecretKeyFactory skf = SecretKeyFactory.getInstance(ALGORITHM);
            return skf.generateSecret(spec).getEncoded();
        } finally {
            spec.clearPassword();
        }
    }

    @Test
    void iteration1_matchesKnownAnswer() throws Exception {
        // First 20 bytes independently published (RFC-6070-style PBKDF2-HMAC-SHA256 vector set):
        // 120fb6cffcf8b32c43e7225256c4f837a86548c9
        assertEquals("120fb6cffcf8b32c43e7225256c4f837a86548c92ccc35480805987cb70be17b",
                HexFormat.of().formatHex(derive("password", "salt", 1)));
    }

    @Test
    void iteration2_matchesKnownAnswer() throws Exception {
        // First 20 bytes independently published: ae4d0c95af6b46d32d0adff928f06dd02a303f8e
        assertEquals("ae4d0c95af6b46d32d0adff928f06dd02a303f8ef3c251dfd6e2d85a95474c43",
                HexFormat.of().formatHex(derive("password", "salt", 2)));
    }

    @Test
    void iteration4096_matchesKnownAnswer() throws Exception {
        // First 20 bytes independently published: c5e478d59288c841aa530db6845c4c8d962893a0
        assertEquals("c5e478d59288c841aa530db6845c4c8d962893a001ce4e11a4963873aa98134a",
                HexFormat.of().formatHex(derive("password", "salt", 4096)));
    }

    @Test
    void derivation_isDeterministic_forSameInputs() throws Exception {
        byte[] a = derive("somePassword", "someSalt", 10_000);
        byte[] b = derive("somePassword", "someSalt", 10_000);
        assertEquals(HexFormat.of().formatHex(a), HexFormat.of().formatHex(b));
    }
}
