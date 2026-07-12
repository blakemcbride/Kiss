package org.kissweb;

import java.nio.charset.StandardCharsets;
import java.security.GeneralSecurityException;
import java.util.HexFormat;
import javax.crypto.Mac;
import javax.crypto.spec.SecretKeySpec;

/**
 * Keyed message authentication codes (HMAC-SHA256/384/512), pure JDK ({@code javax.crypto.Mac} -
 * no external dependency needed).
 * <br><br>
 * Unlike {@link Hash}, every digest here is parameterized by a secret key: only someone who
 * knows the key can produce (or verify) a matching tag for a given message. That single
 * property gives two distinct, common uses:
 * <ol>
 *   <li><b>Message authentication</b> - prove a message was produced by (or has not been
 *       altered since) someone holding the key, e.g. signing a webhook payload or a token.</li>
 *   <li><b>The "blind index" pattern</b> - a deterministic, keyed digest computed over a
 *       plaintext value and stored <i>alongside</i> a separately, non-deterministically
 *       encrypted column (see {@link Crypto}), so the column remains searchable by equality
 *       without ever decrypting a row. Reversible authenticated encryption such as
 *       {@link Crypto}'s AES-GCM is deliberately non-deterministic - the same plaintext
 *       encrypts to different ciphertext every time, which is exactly what makes it safe, but
 *       it also means {@code WHERE encrypted_column = ?} cannot work directly. A blind index
 *       restores equality lookup:
 *       <pre>
 *       // write path
 *       ssn_encrypted = Crypto.fromKey(fieldEncryptionKey).encryptWithRandomSalt(ssn);  // reversible, for display/export
 *       ssn_index     = Hmac.hmacSha256Hex(fieldIndexKey, normalize(ssn));              // deterministic, for lookup only
 *
 *       // read path (equality lookup, no decryption of any row)
 *       String candidateIndex = Hmac.hmacSha256Hex(fieldIndexKey, normalize(candidateSsn));
 *       // ... SELECT ... WHERE ssn_index = candidateIndex ...
 *       </pre>
 *       Two things are the caller's responsibility, not this class's, because they are
 *       data-shape decisions specific to the application and the column being indexed:
 *       <ul>
 *         <li><b>Key separation</b> - {@code fieldIndexKey} must be a <i>different</i> key
 *             from {@code fieldEncryptionKey}. Never reuse one secret for two primitives; keep
 *             each as its own entry in {@code application.ini} (see "Secrets and External
 *             Configuration" in the Kiss knowledge base).</li>
 *         <li><b>Normalization</b> - normalize the plaintext before hashing (trim whitespace,
 *             strip punctuation, fix case, etc., as appropriate to the value) so formatting
 *             variance - not a real difference in the underlying value - doesn't defeat
 *             equality lookup. What "normalized" means is inherently application-specific, so
 *             it is not something {@code Hmac} can do on the caller's behalf.</li>
 *       </ul>
 *   </li>
 * </ol>
 * As with {@link Hash}, every algorithm is available as a raw-bytes MAC (from a {@code byte[]}
 * or a UTF-8 {@code String} message) plus lowercase-hex and standard-Base64 encoded
 * convenience forms. To compare an HMAC output against an expected value, use
 * {@link ConstantTime#equals(byte[], byte[])} rather than {@code Arrays.equals} or
 * {@code String.equals}.
 */
public final class Hmac {

    /**
     * Private constructor to prevent instantiation of this utility class.
     * All methods are static and should be accessed directly through the class.
     */
    private Hmac() {
        // Utility class - prevent instantiation
    }

    private static byte[] mac(String algorithm, byte[] key, byte[] message) {
        try {
            final Mac mac = Mac.getInstance(algorithm);
            mac.init(new SecretKeySpec(key, algorithm));
            return mac.doFinal(message);
        } catch (GeneralSecurityException e) {
            // HmacSHA256/384/512 are required by every JRE.
            throw new RuntimeException(algorithm + " is required by every JRE", e);
        }
    }

    // ==================================================================
    // HMAC-SHA256
    // ==================================================================

    /**
     * Compute HMAC-SHA256 over a byte-array message.
     *
     * @param key the secret key (any non-empty length; 32 bytes is typical for SHA-256)
     * @param message the message to authenticate
     * @return the 32-byte MAC
     */
    public static byte[] hmacSha256(byte[] key, byte[] message) {
        return mac("HmacSHA256", key, message);
    }

    /**
     * Compute HMAC-SHA256 over a UTF-8 string message.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the 32-byte MAC
     */
    public static byte[] hmacSha256(byte[] key, String message) {
        return hmacSha256(key, message.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Compute HMAC-SHA256 over a byte-array message, lowercase-hex encoded. Suitable as the
     * deterministic "blind index" value described in this class's documentation.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the lowercase-hex encoded MAC
     */
    public static String hmacSha256Hex(byte[] key, byte[] message) {
        return HexFormat.of().formatHex(hmacSha256(key, message));
    }

    /**
     * Compute HMAC-SHA256 over a UTF-8 string message, lowercase-hex encoded. Suitable as the
     * deterministic "blind index" value described in this class's documentation.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the lowercase-hex encoded MAC
     */
    public static String hmacSha256Hex(byte[] key, String message) {
        return HexFormat.of().formatHex(hmacSha256(key, message));
    }

    /**
     * Compute HMAC-SHA256 over a byte-array message, standard Base64 encoded.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the Base64 encoded MAC
     */
    public static String hmacSha256Base64(byte[] key, byte[] message) {
        return Base64.encode(hmacSha256(key, message));
    }

    /**
     * Compute HMAC-SHA256 over a UTF-8 string message, standard Base64 encoded.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the Base64 encoded MAC
     */
    public static String hmacSha256Base64(byte[] key, String message) {
        return Base64.encode(hmacSha256(key, message));
    }

    // ==================================================================
    // HMAC-SHA384
    // ==================================================================

    /**
     * Compute HMAC-SHA384 over a byte-array message.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the 48-byte MAC
     */
    public static byte[] hmacSha384(byte[] key, byte[] message) {
        return mac("HmacSHA384", key, message);
    }

    /**
     * Compute HMAC-SHA384 over a UTF-8 string message.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the 48-byte MAC
     */
    public static byte[] hmacSha384(byte[] key, String message) {
        return hmacSha384(key, message.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Compute HMAC-SHA384 over a byte-array message, lowercase-hex encoded.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the lowercase-hex encoded MAC
     */
    public static String hmacSha384Hex(byte[] key, byte[] message) {
        return HexFormat.of().formatHex(hmacSha384(key, message));
    }

    /**
     * Compute HMAC-SHA384 over a UTF-8 string message, lowercase-hex encoded.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the lowercase-hex encoded MAC
     */
    public static String hmacSha384Hex(byte[] key, String message) {
        return HexFormat.of().formatHex(hmacSha384(key, message));
    }

    /**
     * Compute HMAC-SHA384 over a byte-array message, standard Base64 encoded.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the Base64 encoded MAC
     */
    public static String hmacSha384Base64(byte[] key, byte[] message) {
        return Base64.encode(hmacSha384(key, message));
    }

    /**
     * Compute HMAC-SHA384 over a UTF-8 string message, standard Base64 encoded.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the Base64 encoded MAC
     */
    public static String hmacSha384Base64(byte[] key, String message) {
        return Base64.encode(hmacSha384(key, message));
    }

    // ==================================================================
    // HMAC-SHA512
    // ==================================================================

    /**
     * Compute HMAC-SHA512 over a byte-array message.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the 64-byte MAC
     */
    public static byte[] hmacSha512(byte[] key, byte[] message) {
        return mac("HmacSHA512", key, message);
    }

    /**
     * Compute HMAC-SHA512 over a UTF-8 string message.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the 64-byte MAC
     */
    public static byte[] hmacSha512(byte[] key, String message) {
        return hmacSha512(key, message.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Compute HMAC-SHA512 over a byte-array message, lowercase-hex encoded.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the lowercase-hex encoded MAC
     */
    public static String hmacSha512Hex(byte[] key, byte[] message) {
        return HexFormat.of().formatHex(hmacSha512(key, message));
    }

    /**
     * Compute HMAC-SHA512 over a UTF-8 string message, lowercase-hex encoded.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the lowercase-hex encoded MAC
     */
    public static String hmacSha512Hex(byte[] key, String message) {
        return HexFormat.of().formatHex(hmacSha512(key, message));
    }

    /**
     * Compute HMAC-SHA512 over a byte-array message, standard Base64 encoded.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the Base64 encoded MAC
     */
    public static String hmacSha512Base64(byte[] key, byte[] message) {
        return Base64.encode(hmacSha512(key, message));
    }

    /**
     * Compute HMAC-SHA512 over a UTF-8 string message, standard Base64 encoded.
     *
     * @param key the secret key
     * @param message the message to authenticate
     * @return the Base64 encoded MAC
     */
    public static String hmacSha512Base64(byte[] key, String message) {
        return Base64.encode(hmacSha512(key, message));
    }

    /**
     * Small self-test / demo entry point, matching the convention used by {@link Crypto#main}.
     * Prints the well-known RFC 4231 Test Case 1 HMAC values and compares them against their
     * published values.
     *
     * @param args command line arguments (unused)
     */
    public static void main(String[] args) {
        final byte[] key = new byte[20];
        java.util.Arrays.fill(key, (byte) 0x0b);
        final String data = "Hi There";

        check("HMAC-SHA256", hmacSha256Hex(key, data), "b0344c61d8db38535ca8afceaf0bf12b881dc200c9833da726e9376c2e32cff7");
        check("HMAC-SHA384", hmacSha384Hex(key, data), "afd03944d84895626b0825f4ab46907f15f9dadbe4101ec682aa034c7cebc59cfaea9ea9076ede7f4af152e8b2fa9cb6");
        check("HMAC-SHA512", hmacSha512Hex(key, data), "87aa7cdea5ef619d4ff0b4241a1d6cb02379f4e2ce4ec2787ad0b30545e17cdedaa833b7d6b8a702038b274eaea3f4e4be9d914eeb61f1702e696c203a126854");
    }

    private static void check(String label, String computed, String expected) {
        final boolean match = computed.equalsIgnoreCase(expected);
        System.out.println(label + "(RFC 4231 Test Case 1) = " + computed + (match ? "  [OK]" : "  [MISMATCH, expected " + expected + "]"));
    }
}
