package org.kissweb;

/**
 * Constant-time comparison of byte arrays, for use whenever secret-derived bytes (a MAC/HMAC
 * output, a password hash, a blind-index value, etc.) are compared against an expected value
 * in memory.
 * <br><br>
 * A naive {@code Arrays.equals}-style comparison returns as soon as it finds the first
 * differing byte, so the time taken leaks how many leading bytes matched. For secret
 * comparisons that timing difference can, in principle, be measured by an attacker and used
 * to guess the correct value one byte at a time. {@link #equals(byte[], byte[])} instead
 * always inspects every byte of both arrays (branchless XOR-accumulate, no early return), so
 * its running time depends only on the arrays' length, not their content.
 * <br><br>
 * This is the same comparison {@link PasswordHash} has always used internally (previously a
 * private method there); it is promoted here so any other code needing a constant-time
 * comparison (verifying an {@link Hmac} output against an expected value, comparing a blind
 * index, etc.) reuses one audited implementation instead of every caller re-deriving their
 * own equality loop.
 * <br><br>
 * Note: a real hardware/JIT-level timing side channel is not practical to verify with a unit
 * test; the safeguard here is the implementation itself (no data-dependent branching or early
 * return), not a timing measurement.
 */
public final class ConstantTime {

    /**
     * Private constructor to prevent instantiation of this utility class.
     * All methods are static and should be accessed directly through the class.
     */
    private ConstantTime() {
        // Utility class - prevent instantiation
    }

    /**
     * Compare two byte arrays for equality in constant time (with respect to their content -
     * running time depends only on {@code a.length}, not on where/whether the arrays differ).
     * <br><br>
     * Both arrays must be non-null. A length mismatch is detected and reported as
     * "not equal" immediately - the only information revealed is that the lengths differ,
     * not anything about either array's content, which is the normal and accepted shape for
     * this kind of comparison.
     *
     * @param a the first byte array (must not be null)
     * @param b the second byte array (must not be null)
     * @return true if, and only if, the two arrays have the same length and identical content
     */
    public static boolean equals(byte[] a, byte[] b) {
        if (a.length != b.length)
            return false;
        int result = 0;
        for (int i = 0; i < a.length; i++)
            result |= a[i] ^ b[i];
        return result == 0;
    }
}
