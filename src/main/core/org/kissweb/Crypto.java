package org.kissweb;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.crypto.Cipher;
import javax.crypto.SecretKey;
import javax.crypto.SecretKeyFactory;
import javax.crypto.spec.GCMParameterSpec;
import javax.crypto.spec.PBEKeySpec;
import javax.crypto.spec.SecretKeySpec;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.security.Key;
import java.security.SecureRandom;
import java.util.Arrays;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicBoolean;

/**
 * This class provides the ability to encrypt/decrypt strings and byte arrays with strong (AES) encryption.
 * <br><br>
 * Values are encrypted with AES in GCM mode (authenticated encryption) using a random per-message nonce.
 * The output is a self-describing, versioned container so that the format can evolve without breaking
 * previously-encrypted data.
 * <br><br>
 * <b>Two independent ways to supply the key, for two different situations:</b>
 * <ul>
 *   <li><b>Password-based ({@link #Crypto(String)}, {@link #deriveOnce(String, String)}):</b> the AES key is
 *       derived from a human-choosable password (and optional salt) with PBKDF2 (65,536 iterations, 256-bit
 *       derived key) - container format {@code VERSION_1}. PBKDF2 exists to slow down brute-forcing a
 *       low-entropy secret, so this path makes sense when the input really is a password. It does not make
 *       sense - and only adds latency - when the "password" is actually a high-entropy, machine-generated
 *       key (e.g. 256 bits from a secret store): there is nothing to brute-force, so pay for {@link #fromKey}
 *       instead.</li>
 *   <li><b>Raw key ({@link #fromKey(byte[])}, {@link #fromKeyBase64(String)}):</b> the caller supplies an
 *       already-high-entropy 256-bit key directly - no PBKDF2, no salt, no per-call KDF cost - container
 *       format {@code VERSION_2}. This is the recommended path for bulk/high-volume field encryption (e.g. a
 *       master key sourced once from {@code application.ini} and reused for many rows); see
 *       {@link #generateKey()} to provision a fresh key.</li>
 * </ul>
 * Regardless of path, per-value non-determinism (the same plaintext encrypting to different ciphertext each
 * time) comes solely from the random 12-byte GCM nonce generated on every encrypt call - never from the key
 * derivation - so no security property differs between the two paths; only the KDF cost does.
 * <br><br>
 * <b>Key derivation is cached, not repeated per call.</b> For password-based instances, the PBKDF2-derived
 * key for a given (password, salt) pair is computed at most once per instance and reused for every subsequent
 * encrypt/decrypt call on that instance with the same salt - no construction path re-derives the key on every
 * call. {@link #deriveOnce(String, String)} additionally computes that derivation immediately (eagerly, at
 * construction) rather than lazily on first use, which is useful when a caller wants the one-time PBKDF2 cost
 * to happen at a predictable point (e.g. once at the start of a batch) instead of on whichever row happens to
 * encrypt first. A caller who instead wants to skip PBKDF2 altogether should use {@link #fromKey}.
 * <br><br>
 * <b>{@code DEFAULT_KDF_SALT}:</b> when a password-based encrypt/decrypt call supplies no salt, a fixed,
 * built-in salt is used so PBKDF2 (which requires a non-empty salt) still has one. This remains fully
 * supported for decrypting data encrypted this way, and is unaffected by anything above - but it is a
 * compatibility fallback for pre-existing callers only, not a recommended pattern for new code. New code
 * should supply an explicit salt, or (better) a per-value random salt via {@link #encryptWithRandomSalt}, or
 * (better still) use {@link #fromKey}/{@link #fromKeyBase64} where salt/KDF do not apply at all. The first
 * time any {@code Crypto} instance in this JVM falls back to {@code DEFAULT_KDF_SALT}, a one-time {@code WARN}
 * is logged as a nudge to migrate - it does not throw and does not change what gets decrypted.
 * <br><br>
 * <b>Backward compatibility:</b> Data produced by older versions of this class (unauthenticated AES/ECB)
 * is still readable.  Decryption auto-detects the legacy format and decrypts it with the original algorithm.
 * New encryptions always use one of the current (GCM) formats.  Note that a peer still running older code
 * (code that predates {@code VERSION_2}) cannot decrypt values produced via {@link #fromKey}/
 * {@link #fromKeyBase64} on this version, though it can still decrypt everything it always could.
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
 *
 * // Raw 256-bit key, no PBKDF2 - recommended for bulk/high-volume field encryption
 * String key = Crypto.generateKeyBase64();          // provision once, store in application.ini
 * Crypto fast = Crypto.fromKeyBase64(key);
 * String encrypted2 = fast.encryptWithRandomSalt("Hello World");
 * String decrypted2 = fast.decryptWithRandomSalt(encrypted2);
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
    // VERSION_2: key used directly (fromKey/fromKeyBase64), no PBKDF2 involved at all.  saltLen is
    // always 0 (kept in the container purely for structural/parsing consistency with VERSION_1).
    private static final byte VERSION_2 = 2;
    private static final int RAW_KEY_BYTES = 32; // 256 bits

    // Prefix on every String container.  '$' is not in the Base64 alphabet, so it can never begin
    // a legacy String value, making format detection unambiguous.
    private static final String TEXT_PREFIX = "$KC1$";

    private static final SecureRandom secureRandom = new SecureRandom();

    private static final Logger logger = LogManager.getLogger(Crypto.class);
    // Ensures the DEFAULT_KDF_SALT fallback is only ever logged once per JVM, no matter how many
    // Crypto instances/calls fall back to it.
    private static final AtomicBoolean defaultSaltWarned = new AtomicBoolean(false);

    // Password-based mode: non-null.  Raw-key mode (fromKey/fromKeyBase64): null (directKey is used instead).
    private final String password;
    // Raw-key mode (fromKey/fromKeyBase64): non-null, used as-is, no KDF.  Password-based mode: null.
    private final SecretKey directKey;
    // Password-based mode only: PBKDF2-derived keys, memoized per effective salt so that no construction
    // path re-derives the key on every encrypt/decrypt call - only the first call for a given salt on a
    // given instance pays the PBKDF2 cost.  A ConcurrentHashMap keeps this safe for an instance shared
    // across threads, consistent with the existing "single instance may be shared across threads" contract.
    private final ConcurrentHashMap<String, SecretKey> keyCache = new ConcurrentHashMap<>();

    /**
     * Create a new Crypto instance with the specified password.
     * <br><br>
     * The AES key is derived from this password via PBKDF2 (see class documentation).  For a
     * high-entropy, machine-generated key (no human password involved), prefer {@link #fromKey}/
     * {@link #fromKeyBase64} instead, which skip PBKDF2 entirely.
     *
     * @param password the password to use for encryption/decryption
     * @throws IllegalArgumentException if password is null or empty
     */
    public Crypto(String password) {
        if (password == null || password.isEmpty()) {
            throw new IllegalArgumentException("Password cannot be null or empty");
        }
        this.password = password;
        this.directKey = null;
    }

    /**
     * Private constructor for the raw-key (no KDF) construction path.  Use {@link #fromKey(byte[])} or
     * {@link #fromKeyBase64(String)} to obtain an instance built this way.
     */
    private Crypto(SecretKey directKey) {
        this.password = null;
        this.directKey = directKey;
    }

    /**
     * Construct a {@code Crypto} instance directly from a raw 256-bit (32-byte) key, with no PBKDF2
     * involved at all.  The key is used as-is, wrapped as an AES {@link SecretKey}, for every
     * encrypt/decrypt call on the returned instance - construct once, cache/reuse the instance (or at
     * least the key material) rather than re-deriving anything per value.
     * <br><br>
     * This is the recommended construction path for bulk/high-volume field encryption, where the key
     * already comes from a high-entropy source (e.g. {@code application.ini} or a secret store) and so
     * has nothing for PBKDF2 to usefully slow down. Output uses container format {@code VERSION_2}; only
     * a {@code Crypto} instance built via {@code fromKey}/{@code fromKeyBase64} can decrypt it (a
     * password-based instance cannot, since there is no password/derivation involved).
     * <br><br>
     * Use {@link #generateKey()} to provision a fresh random key.
     *
     * @param key256 exactly 32 bytes (256 bits) of key material
     * @return a Crypto instance bound to this raw key
     * @throws IllegalArgumentException if key256 is null or not exactly 32 bytes
     */
    public static Crypto fromKey(byte[] key256) {
        if (key256 == null || key256.length != RAW_KEY_BYTES)
            throw new IllegalArgumentException("Key must be exactly " + RAW_KEY_BYTES + " bytes (256 bits)");
        return new Crypto(new SecretKeySpec(key256.clone(), KEY_ALGORITHM));
    }

    /**
     * Construct a {@code Crypto} instance from a Base64-encoded raw 256-bit key - a convenience for the
     * common case of storing the key as text (e.g. in {@code application.ini}).  See {@link #fromKey(byte[])}
     * for full details.
     *
     * @param base64Key256 a 256-bit (32-byte) key, standard Base64 encoded
     * @return a Crypto instance bound to this raw key
     * @throws IllegalArgumentException if base64Key256 is null, not valid Base64, or does not decode to
     *                                   exactly 32 bytes
     */
    public static Crypto fromKeyBase64(String base64Key256) {
        if (base64Key256 == null)
            throw new IllegalArgumentException("base64Key256 cannot be null");
        final byte[] key;
        try {
            key = Base64.decode(base64Key256);
        } catch (IOException e) {
            throw new IllegalArgumentException("base64Key256 is not valid Base64", e);
        }
        return fromKey(key);
    }

    /**
     * Generate a fresh, cryptographically random 256-bit key suitable for {@link #fromKey(byte[])} -
     * typically used once, at provisioning/setup time, to create a new field-encryption master key.
     *
     * @return 32 random bytes from {@link RandomUtil}
     */
    public static byte[] generateKey() {
        return RandomUtil.randomBytes(RAW_KEY_BYTES);
    }

    /**
     * Generate a fresh, cryptographically random 256-bit key, Base64 encoded for convenient storage
     * (e.g. as a single {@code application.ini} value). See {@link #generateKey()}.
     *
     * @return a Base64-encoded 256-bit random key
     */
    public static String generateKeyBase64() {
        return Base64.encode(generateKey());
    }

    /**
     * Construct a {@code Crypto} instance exactly like {@link #Crypto(String)}, except that the
     * PBKDF2-derived key for the given salt is computed immediately, at construction, rather than lazily
     * on the first encrypt/decrypt call. Output remains container format {@code VERSION_1}, byte-for-byte
     * compatible with (and decryptable by) a plain {@code new Crypto(password)} instance - this method
     * changes only <i>when</i> the PBKDF2 cost is paid, not the format or the key itself.
     * <br><br>
     * This exists for password-based callers who reuse one {@code Crypto} instance across many
     * encrypt/decrypt calls with the same salt (e.g. one instance per batch or per session, mirroring how
     * this instance's key derivation is already cached per salt - see class documentation) but want that
     * cost paid at a predictable point (e.g. once at the start of the batch) instead of on whichever call
     * happens to run first. A caller who can instead source a genuinely high-entropy key should prefer
     * {@link #fromKey}, which removes the PBKDF2 cost entirely rather than just relocating it.
     *
     * @param password the password to use for encryption/decryption
     * @param salt the salt to derive with, or null/empty to use the same {@code DEFAULT_KDF_SALT}
     *             fallback {@link #Crypto(String)} would use for a saltless call
     * @return a Crypto instance with its derived key already computed and cached
     * @throws Exception if the key derivation fails
     */
    public static Crypto deriveOnce(String password, String salt) throws Exception {
        final Crypto crypto = new Crypto(password);
        final byte[] saltBytes = salt == null ? null : salt.getBytes(StandardCharsets.UTF_8);
        crypto.deriveKey(saltBytes); // computed now; cached for every subsequent call on this instance
        return crypto;
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
     * <br><br>
     * Password-based instances (VERSION_1): saltBytes is used, deriving (or reusing a cached derivation
     * of) the AES key via PBKDF2, exactly as before this method learned about raw-key instances.
     * Raw-key instances (VERSION_2, {@link #fromKey}/{@link #fromKeyBase64}): saltBytes is ignored (no
     * KDF applies), the key is used directly, and saltLen is written as 0.
     */
    private byte[] encryptToContainer(byte[] saltBytes, byte[] plaintext) throws Exception {
        final SecretKey key;
        final byte version;
        final byte[] saltOut;
        if (directKey != null) {
            key = directKey;
            version = VERSION_2;
            saltOut = new byte[0];
        } else {
            if (saltBytes == null)
                saltBytes = new byte[0];
            if (saltBytes.length > 0xFFFF)
                saltBytes = Arrays.copyOf(saltBytes, 0xFFFF);
            key = deriveKey(saltBytes);
            version = VERSION_1;
            saltOut = saltBytes;
        }
        final byte[] nonce = new byte[GCM_NONCE_BYTES];
        secureRandom.nextBytes(nonce);
        final Cipher c = Cipher.getInstance(GCM_TRANSFORM);
        c.init(Cipher.ENCRYPT_MODE, key, new GCMParameterSpec(GCM_TAG_BITS, nonce));
        final byte[] ct = c.doFinal(plaintext);

        final ByteArrayOutputStream out = new ByteArrayOutputStream();
        out.write(MAGIC);
        out.write(version);
        out.write((saltOut.length >>> 8) & 0xFF);
        out.write(saltOut.length & 0xFF);
        out.write(saltOut);
        out.write(nonce);
        out.write(ct);
        return out.toByteArray();
    }

    private byte[] decryptContainer(byte[] container) throws Exception {
        int pos = MAGIC.length;
        final byte version = container[pos++];
        if (version != VERSION_1 && version != VERSION_2)
            throw new IllegalArgumentException("Unsupported Crypto format version: " + version);
        final int saltLen = ((container[pos++] & 0xFF) << 8) | (container[pos++] & 0xFF);
        final byte[] saltBytes = Arrays.copyOfRange(container, pos, pos + saltLen);
        pos += saltLen;
        final byte[] nonce = Arrays.copyOfRange(container, pos, pos + GCM_NONCE_BYTES);
        pos += GCM_NONCE_BYTES;
        final byte[] ct = Arrays.copyOfRange(container, pos, container.length);

        final SecretKey key;
        if (version == VERSION_2) {
            if (directKey == null)
                throw new IllegalStateException("VERSION_2 (raw-key) data requires a Crypto instance constructed via fromKey()/fromKeyBase64()");
            key = directKey;
        } else {
            key = deriveKey(saltBytes);
        }
        final Cipher c = Cipher.getInstance(GCM_TRANSFORM);
        c.init(Cipher.DECRYPT_MODE, key, new GCMParameterSpec(GCM_TAG_BITS, nonce));
        return c.doFinal(ct);
    }

    /**
     * Derive a 256-bit AES key from the password and salt using PBKDF2 - or return the already-derived
     * key for this exact (instance, effective salt) pairing if a previous call already computed it (see
     * class documentation on key-derivation caching). Only called for password-based instances.
     */
    private SecretKey deriveKey(byte[] saltBytes) throws Exception {
        final boolean usingDefaultSalt = saltBytes == null || saltBytes.length == 0;
        final byte[] effectiveSalt = usingDefaultSalt ? DEFAULT_KDF_SALT : saltBytes;
        if (usingDefaultSalt && defaultSaltWarned.compareAndSet(false, true))
            logger.warn("Crypto: no salt supplied for password-based encryption/decryption; falling back to " +
                    "the built-in DEFAULT_KDF_SALT. This is a compatibility path for pre-existing callers " +
                    "only - new code should supply an explicit salt (or a per-value random salt via " +
                    "encryptWithRandomSalt), or, better, use Crypto.fromKey()/fromKeyBase64() where salt/KDF " +
                    "do not apply at all. This warning is logged only once per JVM.");
        final String cacheKey = Base64.encode(effectiveSalt);
        final SecretKey cached = keyCache.get(cacheKey);
        if (cached != null)
            return cached;
        final PBEKeySpec spec = new PBEKeySpec(password.toCharArray(), effectiveSalt, PBKDF2_ITERATIONS, PBKDF2_KEY_BITS);
        try {
            final SecretKeyFactory skf = SecretKeyFactory.getInstance("PBKDF2WithHmacSHA256");
            final byte[] keyBytes = skf.generateSecret(spec).getEncoded();
            final SecretKey key = new SecretKeySpec(keyBytes, KEY_ALGORITHM);
            keyCache.put(cacheKey, key);
            return key;
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
