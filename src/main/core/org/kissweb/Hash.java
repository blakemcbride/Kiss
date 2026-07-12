package org.kissweb;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.HexFormat;

/**
 * General-purpose, unkeyed cryptographic hashing (message digests): SHA-256, SHA-384, SHA-512,
 * and their SHA-3 counterparts, all pure JDK ({@link MessageDigest} has supported SHA-3 natively
 * since Java 9 - no external dependency is needed for any algorithm in this class).
 * <br><br>
 * <b>This is not the right tool for passwords or MACs.</b> A plain digest has no secret key and
 * is fast by design, so it must never be used to store a password (use {@link PasswordHash}
 * instead) and, on its own, gives no proof of who produced a value (for that, see the keyed
 * digests in {@link Hmac}). Use {@code Hash} for content fingerprints, dedup/change detection,
 * checksums, cache keys, and similar cases where the input is not secret and speed is a feature,
 * not a liability.
 * <br><br>
 * Every {@code sha*}/{@code sha3_*} algorithm is available as: a raw-bytes digest (from a
 * {@code byte[]} or a UTF-8 {@code String}), a lowercase-hex encoded digest, and a standard
 * Base64 encoded digest - mirroring the encodings already used elsewhere in Kiss
 * ({@link org.kissweb.oauth.BearerTokenValidator} for hex, {@link Crypto}/{@link Base64} for
 * Base64).
 * <br><br>
 * <b>Usage Example:</b>
 * <pre>
 * byte[] digest   = Hash.sha256("some content");
 * String hex      = Hash.sha256Hex("some content");
 * String b64      = Hash.sha256Base64("some content");
 * String fileHash = Hash.hashFileHex(new File("upload.bin"), "SHA-256");
 * </pre>
 */
public final class Hash {

    /**
     * Private constructor to prevent instantiation of this utility class.
     * All methods are static and should be accessed directly through the class.
     */
    private Hash() {
        // Utility class - prevent instantiation
    }

    private static final int STREAM_BUFFER_SIZE = 8192;

    private static byte[] digest(String algorithm, byte[] data) {
        try {
            final MessageDigest md = MessageDigest.getInstance(algorithm);
            return md.digest(data);
        } catch (NoSuchAlgorithmException e) {
            // SHA-256/384/512 and SHA3-256/512 are required by every JRE (Java 9+ for SHA-3).
            throw new RuntimeException(algorithm + " is required by every JRE", e);
        }
    }

    // ==================================================================
    // SHA-256
    // ==================================================================

    /**
     * Compute the SHA-256 digest of a byte array.
     *
     * @param data the data to digest
     * @return the 32-byte SHA-256 digest
     */
    public static byte[] sha256(byte[] data) {
        return digest("SHA-256", data);
    }

    /**
     * Compute the SHA-256 digest of a string, encoded as UTF-8.
     *
     * @param data the string to digest
     * @return the 32-byte SHA-256 digest
     */
    public static byte[] sha256(String data) {
        return sha256(data.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Compute the SHA-256 digest of a byte array, lowercase-hex encoded.
     *
     * @param data the data to digest
     * @return the lowercase-hex encoded SHA-256 digest
     */
    public static String sha256Hex(byte[] data) {
        return HexFormat.of().formatHex(sha256(data));
    }

    /**
     * Compute the SHA-256 digest of a string (UTF-8), lowercase-hex encoded.
     *
     * @param data the string to digest
     * @return the lowercase-hex encoded SHA-256 digest
     */
    public static String sha256Hex(String data) {
        return HexFormat.of().formatHex(sha256(data));
    }

    /**
     * Compute the SHA-256 digest of a byte array, standard Base64 encoded.
     *
     * @param data the data to digest
     * @return the Base64 encoded SHA-256 digest
     */
    public static String sha256Base64(byte[] data) {
        return Base64.encode(sha256(data));
    }

    /**
     * Compute the SHA-256 digest of a string (UTF-8), standard Base64 encoded.
     *
     * @param data the string to digest
     * @return the Base64 encoded SHA-256 digest
     */
    public static String sha256Base64(String data) {
        return Base64.encode(sha256(data));
    }

    // ==================================================================
    // SHA-384
    // ==================================================================

    /**
     * Compute the SHA-384 digest of a byte array.
     *
     * @param data the data to digest
     * @return the 48-byte SHA-384 digest
     */
    public static byte[] sha384(byte[] data) {
        return digest("SHA-384", data);
    }

    /**
     * Compute the SHA-384 digest of a string, encoded as UTF-8.
     *
     * @param data the string to digest
     * @return the 48-byte SHA-384 digest
     */
    public static byte[] sha384(String data) {
        return sha384(data.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Compute the SHA-384 digest of a byte array, lowercase-hex encoded.
     *
     * @param data the data to digest
     * @return the lowercase-hex encoded SHA-384 digest
     */
    public static String sha384Hex(byte[] data) {
        return HexFormat.of().formatHex(sha384(data));
    }

    /**
     * Compute the SHA-384 digest of a string (UTF-8), lowercase-hex encoded.
     *
     * @param data the string to digest
     * @return the lowercase-hex encoded SHA-384 digest
     */
    public static String sha384Hex(String data) {
        return HexFormat.of().formatHex(sha384(data));
    }

    /**
     * Compute the SHA-384 digest of a byte array, standard Base64 encoded.
     *
     * @param data the data to digest
     * @return the Base64 encoded SHA-384 digest
     */
    public static String sha384Base64(byte[] data) {
        return Base64.encode(sha384(data));
    }

    /**
     * Compute the SHA-384 digest of a string (UTF-8), standard Base64 encoded.
     *
     * @param data the string to digest
     * @return the Base64 encoded SHA-384 digest
     */
    public static String sha384Base64(String data) {
        return Base64.encode(sha384(data));
    }

    // ==================================================================
    // SHA-512
    // ==================================================================

    /**
     * Compute the SHA-512 digest of a byte array.
     *
     * @param data the data to digest
     * @return the 64-byte SHA-512 digest
     */
    public static byte[] sha512(byte[] data) {
        return digest("SHA-512", data);
    }

    /**
     * Compute the SHA-512 digest of a string, encoded as UTF-8.
     *
     * @param data the string to digest
     * @return the 64-byte SHA-512 digest
     */
    public static byte[] sha512(String data) {
        return sha512(data.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Compute the SHA-512 digest of a byte array, lowercase-hex encoded.
     *
     * @param data the data to digest
     * @return the lowercase-hex encoded SHA-512 digest
     */
    public static String sha512Hex(byte[] data) {
        return HexFormat.of().formatHex(sha512(data));
    }

    /**
     * Compute the SHA-512 digest of a string (UTF-8), lowercase-hex encoded.
     *
     * @param data the string to digest
     * @return the lowercase-hex encoded SHA-512 digest
     */
    public static String sha512Hex(String data) {
        return HexFormat.of().formatHex(sha512(data));
    }

    /**
     * Compute the SHA-512 digest of a byte array, standard Base64 encoded.
     *
     * @param data the data to digest
     * @return the Base64 encoded SHA-512 digest
     */
    public static String sha512Base64(byte[] data) {
        return Base64.encode(sha512(data));
    }

    /**
     * Compute the SHA-512 digest of a string (UTF-8), standard Base64 encoded.
     *
     * @param data the string to digest
     * @return the Base64 encoded SHA-512 digest
     */
    public static String sha512Base64(String data) {
        return Base64.encode(sha512(data));
    }

    // ==================================================================
    // SHA3-256
    // ==================================================================

    /**
     * Compute the SHA3-256 digest of a byte array.
     *
     * @param data the data to digest
     * @return the 32-byte SHA3-256 digest
     */
    public static byte[] sha3_256(byte[] data) {
        return digest("SHA3-256", data);
    }

    /**
     * Compute the SHA3-256 digest of a string, encoded as UTF-8.
     *
     * @param data the string to digest
     * @return the 32-byte SHA3-256 digest
     */
    public static byte[] sha3_256(String data) {
        return sha3_256(data.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Compute the SHA3-256 digest of a byte array, lowercase-hex encoded.
     *
     * @param data the data to digest
     * @return the lowercase-hex encoded SHA3-256 digest
     */
    public static String sha3_256Hex(byte[] data) {
        return HexFormat.of().formatHex(sha3_256(data));
    }

    /**
     * Compute the SHA3-256 digest of a string (UTF-8), lowercase-hex encoded.
     *
     * @param data the string to digest
     * @return the lowercase-hex encoded SHA3-256 digest
     */
    public static String sha3_256Hex(String data) {
        return HexFormat.of().formatHex(sha3_256(data));
    }

    /**
     * Compute the SHA3-256 digest of a byte array, standard Base64 encoded.
     *
     * @param data the data to digest
     * @return the Base64 encoded SHA3-256 digest
     */
    public static String sha3_256Base64(byte[] data) {
        return Base64.encode(sha3_256(data));
    }

    /**
     * Compute the SHA3-256 digest of a string (UTF-8), standard Base64 encoded.
     *
     * @param data the string to digest
     * @return the Base64 encoded SHA3-256 digest
     */
    public static String sha3_256Base64(String data) {
        return Base64.encode(sha3_256(data));
    }

    // ==================================================================
    // SHA3-512
    // ==================================================================

    /**
     * Compute the SHA3-512 digest of a byte array.
     *
     * @param data the data to digest
     * @return the 64-byte SHA3-512 digest
     */
    public static byte[] sha3_512(byte[] data) {
        return digest("SHA3-512", data);
    }

    /**
     * Compute the SHA3-512 digest of a string, encoded as UTF-8.
     *
     * @param data the string to digest
     * @return the 64-byte SHA3-512 digest
     */
    public static byte[] sha3_512(String data) {
        return sha3_512(data.getBytes(StandardCharsets.UTF_8));
    }

    /**
     * Compute the SHA3-512 digest of a byte array, lowercase-hex encoded.
     *
     * @param data the data to digest
     * @return the lowercase-hex encoded SHA3-512 digest
     */
    public static String sha3_512Hex(byte[] data) {
        return HexFormat.of().formatHex(sha3_512(data));
    }

    /**
     * Compute the SHA3-512 digest of a string (UTF-8), lowercase-hex encoded.
     *
     * @param data the string to digest
     * @return the lowercase-hex encoded SHA3-512 digest
     */
    public static String sha3_512Hex(String data) {
        return HexFormat.of().formatHex(sha3_512(data));
    }

    /**
     * Compute the SHA3-512 digest of a byte array, standard Base64 encoded.
     *
     * @param data the data to digest
     * @return the Base64 encoded SHA3-512 digest
     */
    public static String sha3_512Base64(byte[] data) {
        return Base64.encode(sha3_512(data));
    }

    /**
     * Compute the SHA3-512 digest of a string (UTF-8), standard Base64 encoded.
     *
     * @param data the string to digest
     * @return the Base64 encoded SHA3-512 digest
     */
    public static String sha3_512Base64(String data) {
        return Base64.encode(sha3_512(data));
    }

    // ==================================================================
    // Streaming / file hashing
    // ==================================================================

    /**
     * Digest an {@link InputStream} using the named algorithm, reading it in fixed-size chunks
     * rather than loading the whole content into memory. The stream is read to exhaustion but
     * is <b>not</b> closed by this method - the caller owns the stream's lifecycle.
     *
     * @param in the stream to digest
     * @param algorithm a {@link MessageDigest} algorithm name, e.g. {@code "SHA-256"}
     * @return the raw digest bytes
     * @throws IOException if reading the stream fails, or the algorithm name is not supported
     */
    public static byte[] hash(InputStream in, String algorithm) throws IOException {
        final MessageDigest md;
        try {
            md = MessageDigest.getInstance(algorithm);
        } catch (NoSuchAlgorithmException e) {
            throw new IOException("Unsupported digest algorithm: " + algorithm, e);
        }
        final byte[] buf = new byte[STREAM_BUFFER_SIZE];
        int n;
        while ((n = in.read(buf)) != -1)
            md.update(buf, 0, n);
        return md.digest();
    }

    /**
     * Digest a file's content using the named algorithm, streaming it rather than loading the
     * whole file into memory.
     *
     * @param file the file to digest
     * @param algorithm a {@link MessageDigest} algorithm name, e.g. {@code "SHA-256"}
     * @return the raw digest bytes
     * @throws IOException if the file cannot be read, or the algorithm name is not supported
     */
    public static byte[] hashFile(File file, String algorithm) throws IOException {
        try (InputStream in = new FileInputStream(file)) {
            return hash(in, algorithm);
        }
    }

    /**
     * Digest a file's content using the named algorithm, streaming it rather than loading the
     * whole file into memory, and return the result lowercase-hex encoded.
     *
     * @param file the file to digest
     * @param algorithm a {@link MessageDigest} algorithm name, e.g. {@code "SHA-256"}
     * @return the lowercase-hex encoded digest
     * @throws IOException if the file cannot be read, or the algorithm name is not supported
     */
    public static String hashFileHex(File file, String algorithm) throws IOException {
        return HexFormat.of().formatHex(hashFile(file, algorithm));
    }

    /**
     * Small self-test / demo entry point, matching the convention used by {@link Crypto#main}.
     * Prints the well-known FIPS 180-4 / NIST SHA-3 digests of the ASCII string {@code "abc"}
     * and compares them against their published values.
     *
     * @param args command line arguments (unused)
     */
    public static void main(String[] args) {
        check("SHA-256",   sha256Hex("abc"),   "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad");
        check("SHA-384",   sha384Hex("abc"),   "cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7");
        check("SHA-512",   sha512Hex("abc"),   "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f");
        check("SHA3-256",  sha3_256Hex("abc"), "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532");
        check("SHA3-512",  sha3_512Hex("abc"), "b751850b1a57168a5693cd924b6b096e08f621827444f70d884f5d0240d2712e10e116e9192af3c91a7ec57647e3934057340b4cf408d5a56592f8274eec53f0");
    }

    private static void check(String label, String computed, String expected) {
        final boolean match = computed.equalsIgnoreCase(expected);
        System.out.println(label + "(\"abc\") = " + computed + (match ? "  [OK]" : "  [MISMATCH, expected " + expected + "]"));
    }
}
