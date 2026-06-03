package org.kissweb;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.ByteArrayOutputStream;
import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.security.SecureRandom;
import java.util.Arrays;

/**
 * This class provides the ability to encrypt/decrypt strings and byte arrays with strong (AES) encryption.
 * <br><br>
 * Values are encrypted with AES in GCM mode (authenticated encryption) using a random per-message nonce,
 * and the key is derived from the password (and optional salt) with PBKDF2.  The output is a self-describing,
 * versioned container so that the format can evolve without breaking previously-encrypted data.
 * <br><br>
 * <b>Backward compatibility:</b> Data produced by older versions of this class (unauthenticated AES/ECB)
 * is still readable.  Decryption auto-detects the legacy format and decrypts it with the original algorithm.
 * New encryptions always use the current (GCM) format.  Note that a peer still running the older code cannot
 * decrypt values produced by this version.
 * <br><br>
 * Each encrypt/decrypt operation creates its own {@link Cipher}, so a single instance may be shared across
 * threads.
 * <br><br>
 * <b>Usage Example:</b>
 * <pre>
 * // Create a Crypto instance with a password
 * Crypto crypto = new Crypto("mySecretPassword");
 *
 * // Encrypt a string
 * String encrypted = crypto.encrypt("Hello World");
 *
 * // Decrypt the string
 * String decrypted = crypto.decrypt(encrypted);
 *
 * // With salt
 * String encrypted = crypto.encrypt("mySalt", "Hello World");
 * String decrypted = crypto.decrypt("mySalt", encrypted);
 *
 * // With random salt (salt is embedded in the encrypted value)
 * String encrypted = crypto.encryptWithRandomSalt("Hello World");
 * String decrypted = crypto.decryptWithRandomSalt(encrypted);
 * </pre>
 *
 * Author: Blake McBride<br>
 * Date: 2/1/22
 */
public final class Crypto {

    private static final String KEY_ALGORITHM = "AES";
    // Legacy transform (AES/ECB/PKCS5Padding) — retained ONLY to decrypt data written by older versions.
    private static final String LEGACY_TRANSFORM = "AES";
    private static final String GCM_TRANSFORM = "AES/GCM/NoPadding";

    private static final int GCM_NONCE_BYTES = 12;
    private static final int GCM_TAG_BITS = 128;
    private static final int PBKDF2_ITERATIONS = 65_536;
    private static final int PBKDF2_KEY_BITS = 256;
    private static final int RANDOM_SALT_BYTES = 16;

    // Fixed PBKDF2 salt used when the caller supplies no salt (PBKDF2 requires a non-empty salt).
    private static final byte[] DEFAULT_KDF_SALT =
            {0x4B, 0x69, 0x73, 0x73, 0x43, 0x72, 0x79, 0x70, 0x74, 0x6F, 0x53, 0x61, 0x6C, 0x74, 0x21, 0x00};

    // 8-byte magic prefixing every binary (byte[]) container.  The leading 0x00 cannot begin a legacy
    // random-salt blob (those start with an ASCII Base64 character), making format detection unambiguous.
    private static final byte[] MAGIC = {0x00, 0x4B, 0x43, 0x52, 0x59, 0x50, 0x54, 0x01};
    private static final byte VERSION_1 = 1;

    // Prefix on every String container.  '$' is not in the Base64 alphabet, so it can never begin
    // a legacy String value, making format detection unambiguous.
    private static final String TEXT_PREFIX = "$KC1$";

    private static final SecureRandom secureRandom = new SecureRandom();

    private final String password;

    /**
     * Create a new Crypto instance with the specified password.
     *
     * @param password the password to use for encryption/decryption
     * @throws IllegalArgumentException if password is null or empty
     */
    public Crypto(String password) {
        if (password == null || password.isEmpty()) {
            throw new IllegalArgumentException("Password cannot be null or empty");
        }
        this.password = password;
    }

    /**
     * Encrypt a string utilizing the passed in salt.
     *
     * @param salt the salt or null
     * @param valueToEnc the string value to encrypt
     * @return the encrypted string encoded in Base64
     * @throws Exception if encryption fails
     */
    public String encrypt(String salt, String valueToEnc) throws Exception {
        final byte[] saltBytes = salt == null ? null : salt.getBytes(StandardCharsets.UTF_8);
        final byte[] container = encryptToContainer(saltBytes, valueToEnc.getBytes(StandardCharsets.UTF_8));
        return TEXT_PREFIX + Base64.encode(container);
    }

    /**
     * Encrypt using the instance password and no salt.
     *
     * @param valueToEnc the string value to encrypt
     * @return the encrypted string encoded in Base64
     * @throws Exception if encryption fails
     */
    public String encrypt(String valueToEnc) throws Exception {
        return encrypt(null, valueToEnc);
    }

    /**
     * Encrypt with a random salt.
     *
     * @param valueToEnc the string value to encrypt
     * @return the encrypted string (the salt is embedded), encoded in Base64
     * @throws Exception if encryption fails
     */
    public String encryptWithRandomSalt(String valueToEnc) throws Exception {
        final byte[] salt = new byte[RANDOM_SALT_BYTES];
        secureRandom.nextBytes(salt);
        final byte[] container = encryptToContainer(salt, valueToEnc.getBytes(StandardCharsets.UTF_8));
        return TEXT_PREFIX + Base64.encode(container);
    }

    /**
     * Encrypt a byte array utilizing the passed in salt.
     *
     * @param salt the salt or null
     * @param valueToEnc the byte array to encrypt
     * @return the encrypted byte array
     * @throws Exception if encryption fails
     */
    public byte[] encrypt(String salt, byte[] valueToEnc) throws Exception {
        final byte[] saltBytes = salt == null ? null : salt.getBytes(StandardCharsets.UTF_8);
        return encryptToContainer(saltBytes, valueToEnc);
    }

    /**
     * Encrypt with the instance password and no salt.
     *
     * @param valueToEnc the byte array to encrypt
     * @return the encrypted byte array
     * @throws Exception if encryption fails
     */
    public byte[] encrypt(byte[] valueToEnc) throws Exception {
        return encrypt(null, valueToEnc);
    }

    /**
     * Encrypt with a random salt.
     *
     * @param valueToEnc the byte array to encrypt
     * @return the encrypted byte array (the salt is embedded)
     * @throws Exception if encryption fails
     */
    public byte[] encryptWithRandomSalt(byte[] valueToEnc) throws Exception {
        final byte[] salt = new byte[RANDOM_SALT_BYTES];
        secureRandom.nextBytes(salt);
        return encryptToContainer(salt, valueToEnc);
    }

    /**
     * Decrypt a string utilizing the passed in salt.
     *
     * @param salt the salt or null
     * @param encryptedValue the Base64 encoded encrypted string
     * @return the decrypted string
     * @throws Exception if decryption fails
     */
    public String decrypt(String salt, String encryptedValue) throws Exception {
        if (encryptedValue != null && encryptedValue.startsWith(TEXT_PREFIX)) {
            final byte[] container = Base64.decode(encryptedValue.substring(TEXT_PREFIX.length()));
            return new String(decryptContainer(container), StandardCharsets.UTF_8);
        }
        // Legacy format: Base64(AES/ECB)
        return new String(legacyDecrypt(salt, Base64.decode(encryptedValue)));
    }

    /**
     * Decrypt a string using the instance password and no salt.
     *
     * @param encryptedValue the Base64 encoded encrypted string
     * @return the decrypted string
     * @throws Exception if decryption fails
     */
    public String decrypt(String encryptedValue) throws Exception {
        return decrypt(null, encryptedValue);
    }

    /**
     * Decrypt a string that was encrypted with random salt.
     *
     * @param encryptedValue the encrypted string
     * @return the decrypted string
     * @throws Exception if decryption fails
     */
    public String decryptWithRandomSalt(String encryptedValue) throws Exception {
        if (encryptedValue.startsWith(TEXT_PREFIX)) {
            final byte[] container = Base64.decode(encryptedValue.substring(TEXT_PREFIX.length()));
            return new String(decryptContainer(container), StandardCharsets.UTF_8);
        }
        // Legacy format: 11-character salt prepended to Base64(AES/ECB)
        final String salt = encryptedValue.substring(0, 11);
        final String enc = encryptedValue.substring(11);
        return decrypt(salt, enc);
    }

    /**
     * Decrypt a byte array utilizing the passed in salt.
     *
     * @param salt the salt or null
     * @param encryptedValue the encrypted byte array
     * @return the decrypted byte array
     * @throws Exception if decryption fails
     */
    public byte[] decrypt(String salt, byte[] encryptedValue) throws Exception {
        if (hasMagic(encryptedValue))
            return decryptContainer(encryptedValue);
        // Legacy format: raw AES/ECB ciphertext
        return legacyDecrypt(salt, encryptedValue);
    }

    /**
     * Decrypt a byte array using the instance password and no salt.
     *
     * @param encryptedValue the encrypted byte array
     * @return the decrypted byte array
     * @throws Exception if decryption fails
     */
    public byte[] decrypt(byte[] encryptedValue) throws Exception {
        return decrypt(null, encryptedValue);
    }

    /**
     * Decrypt a byte array that was encrypted with random salt.
     *
     * @param encryptedValue the encrypted byte array
     * @return the decrypted byte array
     * @throws Exception if decryption fails
     */
    public byte[] decryptWithRandomSalt(byte[] encryptedValue) throws Exception {
        if (hasMagic(encryptedValue))
            return decryptContainer(encryptedValue);
        // Legacy format: 11-byte salt prepended to AES/ECB ciphertext
        final String salt = new String(Arrays.copyOfRange(encryptedValue, 0, 11));
        final byte[] enc = Arrays.copyOfRange(encryptedValue, 11, encryptedValue.length);
        return legacyDecrypt(salt, enc);
    }

    // ==================================================================
    // Current (versioned, authenticated) format
    // ==================================================================

    /**
     * Encrypt to a self-describing container:
     * MAGIC(8) | VERSION(1) | saltLen(2, big-endian) | salt | nonce(12) | GCM ciphertext+tag
     */
    private byte[] encryptToContainer(byte[] saltBytes, byte[] plaintext) throws Exception {
        if (saltBytes == null)
            saltBytes = new byte[0];
        if (saltBytes.length > 0xFFFF)
            saltBytes = Arrays.copyOf(saltBytes, 0xFFFF);
        final SecretKey key = deriveKey(saltBytes);
        final byte[] nonce = new byte[GCM_NONCE_BYTES];
        secureRandom.nextBytes(nonce);
        final Cipher c = Cipher.getInstance(GCM_TRANSFORM);
        c.init(Cipher.ENCRYPT_MODE, key, new GCMParameterSpec(GCM_TAG_BITS, nonce));
        final byte[] ct = c.doFinal(plaintext);

        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        out.write(MAGIC);
        out.write(VERSION_1);
        out.write((saltBytes.length >>> 8) & 0xFF);
        out.write(saltBytes.length & 0xFF);
        out.write(saltBytes);
        out.write(nonce);
        out.write(ct);
        return out.toByteArray();
    }

    private byte[] decryptContainer(byte[] container) throws Exception {
        int pos = MAGIC.length;
        final byte version = container[pos++];
        if (version != VERSION_1)
            throw new IllegalArgumentException("Unsupported Crypto format version: " + version);
        final int saltLen = ((container[pos++] & 0xFF) << 8) | (container[pos++] & 0xFF);
        final byte[] saltBytes = Arrays.copyOfRange(container, pos, pos + saltLen);
        pos += saltLen;
        final byte[] nonce = Arrays.copyOfRange(container, pos, pos + GCM_NONCE_BYTES);
        pos += GCM_NONCE_BYTES;
        final byte[] ct = Arrays.copyOfRange(container, pos, container.length);

        final SecretKey key = deriveKey(saltBytes);
        final Cipher c = Cipher.getInstance(GCM_TRANSFORM);
        c.init(Cipher.DECRYPT_MODE, key, new GCMParameterSpec(GCM_TAG_BITS, nonce));
        return c.doFinal(ct);
    }

    /**
     * Derive a 256-bit AES key from the password and salt using PBKDF2.
     */
    private SecretKey deriveKey(byte[] saltBytes) throws Exception {
        final byte[] effectiveSalt = (saltBytes == null || saltBytes.length == 0) ? DEFAULT_KDF_SALT : saltBytes;
        final PBEKeySpec spec = new PBEKeySpec(password.toCharArray(), effectiveSalt, PBKDF2_ITERATIONS, PBKDF2_KEY_BITS);
        try {
            final SecretKeyFactory skf = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
            final byte[] keyBytes = skf.generateSecret(spec).getEncoded();
            return new SecretKeySpec(keyBytes, KEY_ALGORITHM);
        } finally {
            spec.clearPassword();
        }
    }

    private static boolean hasMagic(byte[] data) {
        if (data == null || data.length < MAGIC.length + 1)
            return false;
        for (int i = 0; i < MAGIC.length; i++)
            if (data[i] != MAGIC[i])
                return false;
        return true;
    }

    // ==================================================================
    // Legacy format (decrypt-only) — kept for backward compatibility
    // ==================================================================

    private byte[] legacyDecrypt(String salt, byte[] encryptedBytes) throws Exception {
        final Key key = generateKey(salt, password);
        final Cipher c = Cipher.getInstance(LEGACY_TRANSFORM);
        c.init(Cipher.DECRYPT_MODE, key);
        return c.doFinal(encryptedBytes);
    }

    /**
     * Reproduce the original (legacy) key derivation: the first 32 bytes of salt+password, wrapped.
     * Used only to decrypt data written by older versions of this class.
     */
    private static Key generateKey(String salt, String password) {
        int i = 0;
        final int size = 32;
        final byte[] bytes = new byte[size];
        if (salt != null) {
            final byte[] sb = salt.getBytes();
            int j = 0;
            while (j < sb.length && i < size)
                bytes[i++] = sb[j++];
        }
        if (password != null) {
            final byte[] sb = password.getBytes();
            int j = 0;
            while (j < sb.length && i < size)
                bytes[i++] = sb[j++];
        }
        if (i < size) {
            int j = 0;
            while (i < size) {
                bytes[i++] = bytes[j++];
                if (j >= i - 1)
                    j = 0;
            }
        }
        return new SecretKeySpec(bytes, KEY_ALGORITHM);
    }

    /**
     * Test method demonstrating encryption and decryption.
     *
     * @param args command line arguments
     * @throws Exception if encryption/decryption fails
     */
    public static void main(String[] args) throws Exception {
        final String unencrypted = "Now is the time for all good men to come to the aid of their party.";
        final String password = "ThisIsASecretKey";

        // Create a Crypto instance with the password
        Crypto crypto = new Crypto(password);

        /*
          The following example is interesting because we are encrypting the same text twice with the same password
          yet getting totally different encryption texts.  Yet they both decrypt fine!
         */
        String encrypted1 = crypto.encryptWithRandomSalt(unencrypted);
        String encrypted2 = crypto.encryptWithRandomSalt(unencrypted);
        String decrypted1 = crypto.decryptWithRandomSalt(encrypted1);
        String decrypted2 = crypto.decryptWithRandomSalt(encrypted2);
        System.out.println("\n" + unencrypted + " (" + unencrypted.length() + ")");
        System.out.println(encrypted1 + " (" + encrypted1.length() + ")");
        System.out.println(encrypted2 + " (" + encrypted2.length() + ")");
        System.out.println(decrypted1 + " (" + decrypted1.length() + ")");
        System.out.println(decrypted2 + " (" + decrypted2.length() + ")");

        /*
         * Same thing with byte array.
         */
        System.out.println();
        byte[] e1 = crypto.encryptWithRandomSalt(unencrypted.getBytes());
        byte[] e2 = crypto.encryptWithRandomSalt(unencrypted.getBytes());
        byte[] d1 = crypto.decryptWithRandomSalt(e1);
        byte[] d2 = crypto.decryptWithRandomSalt(e2);
        System.out.println("\n" + unencrypted + " (" + unencrypted.length() + ")");
        System.out.println(Base64.encode(e1) + " (" + e1.length + ")");
        System.out.println(Base64.encode(e2) + " (" + e2.length + ")");
        System.out.println(new String(d1) + " (" + d1.length + ")");
        System.out.println(new String(d2) + " (" + d2.length + ")");
    }
}
