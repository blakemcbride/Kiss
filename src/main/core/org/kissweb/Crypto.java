package org.kissweb;

import javax.crypto.Cipher;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.spec.SecretKeySpec;
import java.security.Key;
import java.security.NoSuchAlgorithmException;
import java.util.Arrays;
import java.util.Random;

/**
 * This class provides the ability to encrypt/decrypt strings and byte arrays with very strong (AES) encryption.
 * <br><br>
 * Each instance of this class maintains its own Cipher, making it thread-safe when each thread uses
 * its own instance. For concurrent operations, create a separate Crypto instance for each operation.
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
 * // With random salt (salt is prepended to encrypted value)
 * String encrypted = crypto.encryptWithRandomSalt("Hello World");
 * String decrypted = crypto.decryptWithRandomSalt(encrypted);
 * </pre>
 *
 * Author: Blake McBride<br>
 * Date: 2/1/22
 */
public final class Crypto {

    private static final String ALGORITHM = "AES";
    private static final Random random = new Random();

    private final Cipher cipher;
    private final String password;

    /**
     * Create a new Crypto instance with the specified password.
     * <br><br>
     * The password can be any size greater than 0 but only a max of the first 32 bytes will be used.
     *
     * @param password the password to use for encryption/decryption
     * @throws IllegalArgumentException if password is null or empty
     */
    public Crypto(String password) {
        if (password == null || password.isEmpty()) {
            throw new IllegalArgumentException("Password cannot be null or empty");
        }
        this.password = password;
        try {
            this.cipher = Cipher.getInstance(ALGORITHM);
        } catch (NoSuchAlgorithmException | NoSuchPaddingException e) {
            throw new RuntimeException("Failed to initialize cipher", e);
        }
    }

    /**
     * Encrypt a string utilizing the passed in salt.
     * <br><br>
     * While <code>salt</code> and <code>password</code> can be any size, they are added together and only the first
     * 32 bytes are used.
     *
     * @param salt the salt or null
     * @param valueToEnc the string value to encrypt
     * @return the encrypted string encoded in Base64
     * @throws Exception if encryption fails
     */
    public String encrypt(String salt, String valueToEnc) throws Exception {
        final Key key = generateKey(salt, password);
        cipher.init(Cipher.ENCRYPT_MODE, key);
        final byte[] encValue = cipher.doFinal(valueToEnc.getBytes());
        return Base64.encode(encValue);
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
     * @return the salt prepended to the encrypted string encoded in Base64
     * @throws Exception if encryption fails
     */
    public String encryptWithRandomSalt(String valueToEnc) throws Exception {
        final String salt = createSalt();
        return salt + encrypt(salt, valueToEnc);
    }

    /**
     * Encrypt a byte array utilizing the passed in salt.
     * <br><br>
     * While <code>salt</code> and <code>password</code> can be any size, they are added together and only the first
     * 32 bytes are used.
     *
     * @param salt the salt or null
     * @param valueToEnc the byte array to encrypt
     * @return the encrypted byte array
     * @throws Exception if encryption fails
     */
    public byte[] encrypt(String salt, byte[] valueToEnc) throws Exception {
        final Key key = generateKey(salt, password);
        cipher.init(Cipher.ENCRYPT_MODE, key);
        return cipher.doFinal(valueToEnc);
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
     * @return the salt prepended to the encrypted byte array
     * @throws Exception if encryption fails
     */
    public byte[] encryptWithRandomSalt(byte[] valueToEnc) throws Exception {
        final String salt = createSalt();
        final byte[] ba = encrypt(salt, valueToEnc);
        final byte[] salta = salt.getBytes();
        final byte[] na = Arrays.copyOf(salta, salta.length + ba.length);
        System.arraycopy(ba, 0, na, salta.length, ba.length);
        return na;
    }

    /**
     * Decrypt a string utilizing the passed in salt.
     * <br><br>
     * While <code>salt</code> and <code>password</code> can be any size, they are added together and only the first
     * 32 bytes are used.
     *
     * @param salt the salt or null
     * @param encryptedValue the Base64 encoded encrypted string
     * @return the decrypted string
     * @throws Exception if decryption fails
     */
    public String decrypt(String salt, String encryptedValue) throws Exception {
        final Key key = generateKey(salt, password);
        cipher.init(Cipher.DECRYPT_MODE, key);
        final byte[] decodedValue = Base64.decode(encryptedValue);
        final byte[] decValue = cipher.doFinal(decodedValue);
        return new String(decValue);
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
     * @param encryptedValue the salt prepended encrypted string
     * @return the decrypted string
     * @throws Exception if decryption fails
     */
    public String decryptWithRandomSalt(String encryptedValue) throws Exception {
        final String salt = encryptedValue.substring(0, 11);
        encryptedValue = encryptedValue.substring(11);
        return decrypt(salt, encryptedValue);
    }

    /**
     * Decrypt a byte array utilizing the passed in salt.
     * <br><br>
     * While <code>salt</code> and <code>password</code> can be any size, they are added together and only the first
     * 32 bytes are used.
     *
     * @param salt the salt or null
     * @param encryptedValue the encrypted byte array
     * @return the decrypted byte array
     * @throws Exception if decryption fails
     */
    public byte[] decrypt(String salt, byte[] encryptedValue) throws Exception {
        final Key key = generateKey(salt, password);
        cipher.init(Cipher.DECRYPT_MODE, key);
        return cipher.doFinal(encryptedValue);
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
     * @param encryptedValue the salt prepended encrypted byte array
     * @return the decrypted byte array
     * @throws Exception if decryption fails
     */
    public byte[] decryptWithRandomSalt(byte[] encryptedValue) throws Exception {
        final String salt = new String(Arrays.copyOfRange(encryptedValue, 0, 11));
        encryptedValue = Arrays.copyOfRange(encryptedValue, 11, encryptedValue.length);
        return decrypt(salt, encryptedValue);
    }

    /**
     * Take a salt and password of any size and produce an encryption key.
     * <br><br>
     * While <code>salt</code> and <code>password</code> can be any size, they are added together and only the first
     * 32 bytes are used.
     *
     * @param salt null or something unique to the item being encrypted
     * @param password the password (can be any length but max of the first 32 characters are used)
     * @return the encryption key
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
        return new SecretKeySpec(bytes, ALGORITHM);
    }

    /**
     * Convert a long value to byte array.
     *
     * @param data the long value to convert
     * @return the byte array representation
     */
    private static byte[] longtoBytes(long data) {
        return new byte[]{
                (byte) ((data >> 56) & 0xff),
                (byte) ((data >> 48) & 0xff),
                (byte) ((data >> 40) & 0xff),
                (byte) ((data >> 32) & 0xff),
                (byte) ((data >> 24) & 0xff),
                (byte) ((data >> 16) & 0xff),
                (byte) ((data >> 8) & 0xff),
                (byte) (data & 0xff),
        };
    }

    /**
     * Create a random salt.
     * It is always 11 bytes long.
     *
     * @return a random salt string
     */
    private static String createSalt() {
        final long rl = random.nextLong();
        final byte[] ba = longtoBytes(rl);
        return Base64.encode(ba);
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
