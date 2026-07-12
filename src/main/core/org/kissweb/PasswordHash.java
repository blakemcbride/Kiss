package org.kissweb;

import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.PBEKeySpec;
import java.security.SecureRandom;
import java.util.Base64;

/**
 * One-way password hashing using PBKDF2-HMAC-SHA256 with a random per-password salt.
 * <br><br>
 * Passwords are <b>hashed, not encrypted</b> — there is intentionally no way to recover the original
 * password.  Authentication verifies a candidate password by re-deriving the hash with the stored salt
 * and comparing in constant time (via {@link ConstantTime#equals(byte[], byte[])}).  (To reversibly
 * encrypt arbitrary data, use {@link Crypto} instead; reversible encryption is the wrong tool for
 * password storage.)
 * <br><br>
 * The stored value is a self-describing string:
 * <pre>
 *     pbkdf2$&lt;iterations&gt;$&lt;Base64(salt)&gt;$&lt;Base64(hash)&gt;
 * </pre>
 * The iteration count and salt are embedded, so the work factor can be raised over time without
 * invalidating existing hashes. {@link #needsRehash(String)} tells the caller when a stored hash was
 * produced with a weaker (older) iteration count than this class's current default, so the standard
 * "verify, then re-hash-and-save if needed" pattern can gradually migrate every stored password up to
 * the current work factor as users log in, with no bulk migration required. The class is structured so
 * that a stronger algorithm could be added as a second recognized prefix in the future (dispatching
 * inside {@link #verify(String, String)}/{@link #isHashed(String)}/{@link #needsRehash(String)} on which
 * prefix a stored value has) without changing any caller-visible signature; today PBKDF2-HMAC-SHA256 is
 * the only supported algorithm.
 * <br><br>
 * <b>Usage Example:</b>
 * <pre>
 * // When setting/changing a password:
 * String stored = PasswordHash.hash(plainTextPassword);   // save 'stored' in the database
 *
 * // When authenticating:
 * boolean ok = PasswordHash.verify(enteredPassword, stored);
 * if (ok &amp;&amp; PasswordHash.needsRehash(stored))
 *     saveNewHash(PasswordHash.hash(enteredPassword));    // opportunistically upgrade the work factor
 * </pre>
 */
public final class PasswordHash {

    private static final String PREFIX = "pbkdf2$";
    private static final String ALGORITHM = "PBKDF2WithHmacSHA256";
    private static final int ITERATIONS = 600_000;   // OWASP (2023) floor for PBKDF2-HMAC-SHA256
    private static final int SALT_BYTES = 16;
    private static final int KEY_BITS = 256;
    private static final SecureRandom RNG = new SecureRandom();

    private PasswordHash() {
        // utility class - prevent instantiation
    }

    /**
     * Hash a plain-text password for storage.
     *
     * @param password the plain-text password (must be non-null)
     * @return the encoded hash string to store in the database
     */
    public static String hash(String password) {
        final byte[] salt = new byte[SALT_BYTES];
        RNG.nextBytes(salt);
        final byte[] dk = pbkdf2(password.toCharArray(), salt, ITERATIONS);
        final Base64.Encoder enc = Base64.getEncoder().withoutPadding();
        return PREFIX + ITERATIONS + "$" + enc.encodeToString(salt) + "$" + enc.encodeToString(dk);
    }

    /**
     * Verify a plain-text password against a stored hash produced by {@link #hash(String)}.
     *
     * @param password the candidate plain-text password
     * @param stored the stored encoded hash
     * @return true if the password matches
     */
    public static boolean verify(String password, String stored) {
        if (password == null || stored == null || !stored.startsWith(PREFIX))
            return false;
        final String[] parts = stored.split("\\$");
        if (parts.length != 4)
            return false;
        try {
            final int iterations = Integer.parseInt(parts[1]);
            final Base64.Decoder dec = Base64.getDecoder();
            final byte[] salt = dec.decode(parts[2]);
            final byte[] expected = dec.decode(parts[3]);
            final byte[] actual = pbkdf2(password.toCharArray(), salt, iterations);
            return ConstantTime.equals(expected, actual);
        } catch (RuntimeException e) {
            return false;
        }
    }

    /**
     * Determine whether a stored value is in this class's hashed format (as opposed to a legacy
     * plain-text or externally-hashed value).
     *
     * @param stored a stored password value
     * @return true if the value is a PasswordHash-format hash
     */
    public static boolean isHashed(String stored) {
        return stored != null && stored.startsWith(PREFIX);
    }

    /**
     * Determine whether a stored hash should be re-hashed and re-saved - typically on next successful
     * login - because it no longer reflects this class's current work-factor default (or, were a second,
     * stronger algorithm ever added, because it is on the older/weaker of the two). Purely advisory: it
     * never invalidates {@code stored} and has no bearing on {@link #verify(String, String)}, which keeps
     * verifying any value it always could. See the class documentation for the standard
     * "verify, then re-hash-and-save if needed" pattern.
     * <br><br>
     * A value that is not even in this class's format at all (fails {@link #isHashed(String)} - null,
     * a legacy plain-text value, or something else entirely) is treated as needing a rehash too, since it
     * certainly does not reflect the current default.
     *
     * @param stored a stored password value (normally one already confirmed valid by {@link #verify})
     * @return true if {@code stored} should be replaced with a fresh {@link #hash(String)} on next
     *         successful verify
     */
    public static boolean needsRehash(String stored) {
        if (!isHashed(stored))
            return true;
        final String[] parts = stored.split("\\$");
        if (parts.length != 4)
            return true;
        try {
            final int iterations = Integer.parseInt(parts[1]);
            return iterations < ITERATIONS;
        } catch (NumberFormatException e) {
            return true;
        }
    }

    private static byte[] pbkdf2(char[] password, byte[] salt, int iterations) {
        final PBEKeySpec spec = new PBEKeySpec(password, salt, iterations, KEY_BITS);
        try {
            final SecretKeyFactory skf = SecretKeyFactory.getInstance(ALGORITHM);
            return skf.generateSecret(spec).getEncoded();
        } catch (Exception e) {
            throw new RuntimeException("PBKDF2 derivation failed", e);
        } finally {
            spec.clearPassword();
        }
    }
}
