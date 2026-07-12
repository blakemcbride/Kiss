package org.kissweb;

import java.security.SecureRandom;

/**
 * Shared, cryptographically strong random-value helper.
 * <br><br>
 * Several places in Kiss each instantiate their own {@link SecureRandom} (e.g.
 * {@link Crypto}, {@link PasswordHash}, OAuth token/PKCE generation). Each of those is already
 * correct on its own, but this class exists so new code has one obvious, shared place to get
 * random key material or tokens from, backed by a single lazily-seeded {@code SecureRandom}
 * instance, rather than every caller writing {@code new SecureRandom()} again.
 */
public final class RandomUtil {

    private static final SecureRandom RNG = new SecureRandom();

    private static final char[] ALPHANUMERIC =
            "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789".toCharArray();

    /**
     * Private constructor to prevent instantiation of this utility class.
     * All methods are static and should be accessed directly through the class.
     */
    private RandomUtil() {
        // Utility class - prevent instantiation
    }

    /**
     * Generate cryptographically strong random bytes, e.g. for a raw encryption key
     * (see {@link Crypto#generateKey()}) or a token.
     *
     * @param n the number of random bytes to generate
     * @return a freshly allocated array of {@code n} random bytes
     */
    public static byte[] randomBytes(int n) {
        final byte[] b = new byte[n];
        RNG.nextBytes(b);
        return b;
    }

    /**
     * Generate a random string of upper/lowercase letters and digits (62-character alphabet),
     * suitable for one-time codes, temporary tokens, or similar human-typeable random values.
     * Not intended as a substitute for {@link #randomBytes(int)} where raw key material is
     * needed - each character carries less than 6 bits of entropy, so use enough characters
     * for the required strength.
     *
     * @param len the number of characters to generate
     * @return a random alphanumeric string of length {@code len}
     */
    public static String randomAlphaNumeric(int len) {
        final StringBuilder sb = new StringBuilder(len);
        for (int i = 0; i < len; i++)
            sb.append(ALPHANUMERIC[RNG.nextInt(ALPHANUMERIC.length)]);
        return sb.toString();
    }
}
