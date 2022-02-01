package org.kissweb;

import sun.misc.BASE64Decoder;
import sun.misc.BASE64Encoder;

import javax.crypto.Cipher;
import javax.crypto.spec.SecretKeySpec;
import java.security.Key;

/**
 * Author: Blake McBride
 * Date: 2/1/22
 *
 * This class provides the ability to encrypt/decrypt strings and byte arrays with very strong (AES) encryption.
 */
public class Crypto {

    private static final String ALGORITHM = "AES";
    private static String defaultPassword;

    /**
     * Encrypt a string utilizing the passed in salt and password.
     *
     * While <code>salt</code> and <code>password</code> can be any size, they are added together and only the first
     * 32 bytes are used.
     *
     * @param salt the salt or null
     * @param password
     * @param valueToEnc
     * @return
     * @throws Exception
     */
    public static String encrypt(String salt, String password, String valueToEnc) throws Exception {
        final Key key = generateKey(salt, password);
        final Cipher c = Cipher.getInstance(ALGORITHM);
        c.init(Cipher.ENCRYPT_MODE, key);
        final byte[] encValue = c.doFinal(valueToEnc.getBytes());
        return new BASE64Encoder().encode(encValue);
    }

    /**
     * Encrypt a string utilizing the passed in salt and the default password.
     *
     * @param salt the salt or null
     * @param valueToEnc
     * @return
     * @throws Exception
     */
    public static String encrypt(String salt, String valueToEnc) throws Exception {
        return encrypt(salt, defaultPassword, valueToEnc);
    }

    /**
     * Encrypt a byte array utilizing the passed in salt and password.
     *
     * While <code>salt</code> and <code>password</code> can be any size, they are added together and only the first
     * 32 bytes are used.
     *
     * @param salt the salt or null
     * @param password
     * @param valueToEnc
     * @return
     * @throws Exception
     */
    public static byte [] encrypt(String salt, String password, byte [] valueToEnc) throws Exception {
        final Key key = generateKey(salt, password);
        final Cipher c = Cipher.getInstance(ALGORITHM);
        c.init(Cipher.ENCRYPT_MODE, key);
        return c.doFinal(valueToEnc);
    }

    /**
     * Encrypt a byte array utilizing a salt and the default password.
     *
     *
     * @param salt the salt or null
     * @param valueToEnc
     * @return
     * @throws Exception
     */
    public static byte [] encrypt(String salt, byte [] valueToEnc) throws Exception {
        return encrypt(salt, defaultPassword, valueToEnc);
    }

    /**
     * Decrypt a string utilizing the passed in salt and password.
     *
     * While <code>salt</code> and <code>password</code> can be any size, they are added together and only the first
     * 32 bytes are used.
     *
     * @param salt the salt or null
     * @param password
     * @param encryptedValue
     * @return
     * @throws Exception
     */
    public static String decrypt(String salt, String password, String encryptedValue) throws Exception {
        final Key key = generateKey(salt, password);
        final Cipher c = Cipher.getInstance(ALGORITHM);
        c.init(Cipher.DECRYPT_MODE, key);
        final byte[] decordedValue = new BASE64Decoder().decodeBuffer(encryptedValue);
        final byte[] decValue = c.doFinal(decordedValue);
        return new String(decValue);
    }

    /**
     * Decrypt a string utilizing the passed in salt and the default password.
     *
     * @param salt the salt or null
     * @param encryptedValue
     * @return
     * @throws Exception
     */
    public static String decrypt(String salt, String encryptedValue) throws Exception {
        return decrypt(salt, defaultPassword, encryptedValue);
    }

    /**
     * Decrypt a byte array utilizing the passed in salt and password.
     *
     * While <code>salt</code> and <code>password</code> can be any size, they are added together and only the first
     * 32 bytes are used.
     *
     * @param salt the salt or null
     * @param password
     * @param encryptedValue
     * @return
     * @throws Exception
     */
    public static byte [] decrypt(String salt, String password, byte [] encryptedValue) throws Exception {
        final Key key = generateKey(salt, password);
        final Cipher c = Cipher.getInstance(ALGORITHM);
        c.init(Cipher.DECRYPT_MODE, key);
        return c.doFinal(encryptedValue);
    }

    /**
     * Decrypt a byte array utilizing a slat and the default password.
     *
     * @param salt the salt or null
     * @param encryptedValue
     * @return
     * @throws Exception
     */
    public static byte [] decrypt(String salt, byte [] encryptedValue) throws Exception {
        return decrypt(salt, defaultPassword, encryptedValue);
    }

    /**
     * Set the default password.  This remains until it is changed.
     *
     * The password can be any size > 0 but only a max of the first 32 bytes will be used.
     *
     * @param password the password
     */
    public static void setDefaultPassword(String password) {
        defaultPassword = password;
    }

    /**
     * Take a salt and password of any size and convert produce an encryption key.
     *
     * While <code>salt</code> and <code>password</code> can be any size, they are added together and only the first
     * 32 bytes are used.
     *
     * @param salt null or something unique to the item being encrypted
     * @param password the password (can be any length but max of the first 32 characters are used)
     * @return
     * @throws Exception
     */
    private static Key generateKey(String salt, String password) throws Exception {
        int i = 0;
        final int size = 32;
        final byte [] bytes = new byte[size];
        if (salt != null) {
            final byte [] sb = salt.getBytes();
            int j = 0;
            while (j < sb.length  &&  i < size)
                bytes[i++] = sb[j];
        }
        if (password != null) {
            final byte [] sb = password.getBytes();
            int j = 0;
            while (j < sb.length && i < size)
                bytes[i++] = sb[j++];
        }
        if (i < size) {
            int j = 0;
            while (i < size) {
                bytes[i++] = bytes[j++];
                if (j > size)
                    j = 0;
            }
        }
        return new SecretKeySpec(bytes, ALGORITHM);
    }

    public static void main(String [] args) throws Exception {
        String unencrypted = "Now is the time for all good men to come to the aid of their party.";
        final String password = "ThisIsASecretKey";
        String salt1 = "ab";
        String salt2 = "vr";
        String encrypted1 = encrypt(salt1, password, unencrypted);
        String encrypted2 = encrypt(salt2, password, unencrypted);
        String decrypted1 = decrypt(salt1, password, encrypted1);
        String decrypted2 = decrypt(salt2, password, encrypted2);
        System.out.println("unencrypted  = \"" + unencrypted + "\" (" + unencrypted.length() + ")");
        System.out.println("encrypted1   = \"" + encrypted1 + "\" (" + encrypted1.length() + ")");
        System.out.println("encrypted2   = \"" + encrypted2 + "\" (" + encrypted2.length() + ")");
        System.out.println("decrypted1   = \"" + decrypted1 + "\" (" + decrypted1.length() + ")");
        System.out.println("decrypted2   = \"" + decrypted2 + "\" (" + decrypted2.length() + ")");
    }
}
