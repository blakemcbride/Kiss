package org.kissweb;

import org.junit.jupiter.api.Test;

import java.nio.charset.StandardCharsets;
import java.util.Arrays;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for {@link Crypto}.
 * <br><br>
 * The "FIXTURE_*" constants below were captured by running the ORIGINAL, UNMODIFIED {@code Crypto} and
 * {@code PasswordHash} classes - before {@code fromKey}/{@code fromKeyBase64}/{@code generateKey}/
 * {@code generateKeyBase64}/{@code deriveOnce} were added to {@code Crypto}, and before {@code Hash},
 * {@code Hmac}, {@code ConstantTime}, and {@code RandomUtil} existed at all. They are the actual
 * backward-compatibility regression test (not just an assertion in prose): every one of these fixed
 * values MUST continue to decrypt/verify correctly forever, no matter what else changes in this class.
 * Do not regenerate these values from the current code - they only prove anything by having been
 * produced independently, by code that predates this change.
 */
class CryptoTest {

    // ------------------------------------------------------------------
    // STEP 0 backward-compatibility fixtures (see class javadoc)
    // ------------------------------------------------------------------

    private static final String FIXTURE_EXPLICIT_SALT_PASSWORD = "FixturePassword123!";
    private static final String FIXTURE_EXPLICIT_SALT_SALT = "FixtureSalt-Alpha";
    private static final String FIXTURE_EXPLICIT_SALT_PLAINTEXT = "The quick brown fox jumps over the lazy dog. Fixture payload one.";
    private static final String FIXTURE_EXPLICIT_SALT_CIPHERTEXT =
            "$KC1$AEtDUllQVAEBABFGaXh0dXJlU2FsdC1BbHBoYVFNXC6wG9qNbwUEOvtzls/YFxcHU7wbRc34M2wMrHykbq+fHeJ9LYzne3kk4fGe8SM0RsYVldCYIbMSN5xipThqYAVeI1BeLkbXtBfMrNwO6/gLMfckK4uYEcWAEg==";

    private static final String FIXTURE_RANDOM_SALT_PASSWORD = "FixturePassword456!";
    private static final String FIXTURE_RANDOM_SALT_PLAINTEXT = "Pack my box with five dozen liquor jugs. Fixture payload two.";
    private static final String FIXTURE_RANDOM_SALT_CIPHERTEXT =
            "$KC1$AEtDUllQVAEBABA3GccTLeLPPJEffPVSrprMftDQQovCyKWN4QykJI73eIdyJUEWdZeMzhu+34oxUliMbyZ/5aYee+WvHZ76Q10pDSaKTftvW5xlByVoJeHz1bqI3Q1VIpVoISIRAUhcB0ePU7zaEbMPMts=";

    private static final String FIXTURE_DEFAULT_SALT_PASSWORD = "FixturePassword789!";
    private static final String FIXTURE_DEFAULT_SALT_PLAINTEXT = "How vexingly quick daft zebras jump! Fixture payload three.";
    private static final String FIXTURE_DEFAULT_SALT_CIPHERTEXT =
            "$KC1$AEtDUllQVAEBAAAmZOISCQFv5lblwkgAW2UdxFeTn+eRBNI9LxQBmJXPDpKeF957DBw+rtw9efM0nylO8xpjFEtc6pgKNPOyNs0fdFRNDmRDNoIN4x6r6ljJRYfgZsxRihg=";

    private static final String FIXTURE_LEGACY_EXPLICIT_PASSWORD = "LegacyFixturePassword";
    private static final String FIXTURE_LEGACY_EXPLICIT_SALT = "LegacySaltXYZ";
    private static final String FIXTURE_LEGACY_EXPLICIT_PLAINTEXT = "Legacy fixture payload one - AES ECB pre-GCM data at rest.";
    private static final String FIXTURE_LEGACY_EXPLICIT_CIPHERTEXT_B64 =
            "EyZmtNvcgUYCAfBi9GkK1fQPncu/3fsPq6BKijZ/4GFVqq2ooGQHbpMB752yq5XgW6H81t8QOSN+HvG18xOzfw==";

    private static final String FIXTURE_LEGACY_RANDOMSTYLE_PASSWORD = "LegacyFixturePassword2";
    private static final String FIXTURE_LEGACY_RANDOMSTYLE_PLAINTEXT = "Legacy fixture payload two - AES ECB pre-GCM data at rest.";
    private static final String FIXTURE_LEGACY_RANDOMSTYLE_COMBINED =
            "KISSFIXTUREg37URYmGIiZWDRlctLaD5X+vb93jnMPy7MszzPaltnLToHWpUWpoi582Eq3IZ5v3bvMTepTqg19dzqWkU/zclg==";

    @Test
    void fixture_explicitSaltVersion1_stillDecrypts() throws Exception {
        String dec = new Crypto(FIXTURE_EXPLICIT_SALT_PASSWORD).decrypt(FIXTURE_EXPLICIT_SALT_SALT, FIXTURE_EXPLICIT_SALT_CIPHERTEXT);
        assertEquals(FIXTURE_EXPLICIT_SALT_PLAINTEXT, dec);
    }

    @Test
    void fixture_randomSaltVersion1_stillDecrypts() throws Exception {
        String dec = new Crypto(FIXTURE_RANDOM_SALT_PASSWORD).decryptWithRandomSalt(FIXTURE_RANDOM_SALT_CIPHERTEXT);
        assertEquals(FIXTURE_RANDOM_SALT_PLAINTEXT, dec);
    }

    @Test
    void fixture_defaultKdfSaltVersion1_stillDecrypts() throws Exception {
        // No salt supplied at all -> exercises the DEFAULT_KDF_SALT fallback path.
        String dec = new Crypto(FIXTURE_DEFAULT_SALT_PASSWORD).decrypt(FIXTURE_DEFAULT_SALT_CIPHERTEXT);
        assertEquals(FIXTURE_DEFAULT_SALT_PLAINTEXT, dec);
    }

    @Test
    void fixture_legacyAesEcbExplicitSaltStyle_stillDecrypts() throws Exception {
        String dec = new Crypto(FIXTURE_LEGACY_EXPLICIT_PASSWORD).decrypt(FIXTURE_LEGACY_EXPLICIT_SALT, FIXTURE_LEGACY_EXPLICIT_CIPHERTEXT_B64);
        assertEquals(FIXTURE_LEGACY_EXPLICIT_PLAINTEXT, dec);
    }

    @Test
    void fixture_legacyAesEcbRandomSaltStyle_stillDecrypts() throws Exception {
        String dec = new Crypto(FIXTURE_LEGACY_RANDOMSTYLE_PASSWORD).decryptWithRandomSalt(FIXTURE_LEGACY_RANDOMSTYLE_COMBINED);
        assertEquals(FIXTURE_LEGACY_RANDOMSTYLE_PLAINTEXT, dec);
    }

    // ------------------------------------------------------------------
    // Constructor validation (pre-existing behavior)
    // ------------------------------------------------------------------

    @Test
    void constructor_nullPassword_throws() {
        assertThrows(IllegalArgumentException.class, () -> new Crypto(null));
    }

    @Test
    void constructor_emptyPassword_throws() {
        assertThrows(IllegalArgumentException.class, () -> new Crypto(""));
    }

    // ------------------------------------------------------------------
    // Round trips: password-based construction (VERSION_1), every method family
    // ------------------------------------------------------------------

    @Test
    void passwordBased_string_noSalt_roundTrips() throws Exception {
        Crypto c = new Crypto("regressionPassword1");
        String plaintext = "round trip payload, no salt";
        String enc = c.encrypt(plaintext);
        assertTrue(enc.startsWith("$KC1$"));
        assertEquals(plaintext, c.decrypt(enc));
    }

    @Test
    void passwordBased_string_explicitSalt_roundTrips() throws Exception {
        Crypto c = new Crypto("regressionPassword2");
        String plaintext = "round trip payload, explicit salt";
        String enc = c.encrypt("someSalt", plaintext);
        assertEquals(plaintext, c.decrypt("someSalt", enc));
    }

    @Test
    void passwordBased_string_randomSalt_roundTrips_andDiffersEachCall() throws Exception {
        Crypto c = new Crypto("regressionPassword3");
        String plaintext = "round trip payload, random salt";
        String enc1 = c.encryptWithRandomSalt(plaintext);
        String enc2 = c.encryptWithRandomSalt(plaintext);
        assertNotEquals(enc1, enc2, "same plaintext should produce different ciphertext due to random nonce/salt");
        assertEquals(plaintext, c.decryptWithRandomSalt(enc1));
        assertEquals(plaintext, c.decryptWithRandomSalt(enc2));
    }

    @Test
    void passwordBased_bytes_noSalt_roundTrips() throws Exception {
        Crypto c = new Crypto("regressionPassword4");
        byte[] plaintext = "round trip byte payload, no salt".getBytes(StandardCharsets.UTF_8);
        byte[] enc = c.encrypt(plaintext);
        assertArrayEquals(plaintext, c.decrypt(enc));
    }

    @Test
    void passwordBased_bytes_explicitSalt_roundTrips() throws Exception {
        Crypto c = new Crypto("regressionPassword5");
        byte[] plaintext = "round trip byte payload, explicit salt".getBytes(StandardCharsets.UTF_8);
        byte[] enc = c.encrypt("saltyBytes", plaintext);
        assertArrayEquals(plaintext, c.decrypt("saltyBytes", enc));
    }

    @Test
    void passwordBased_bytes_randomSalt_roundTrips() throws Exception {
        Crypto c = new Crypto("regressionPassword6");
        byte[] plaintext = "round trip byte payload, random salt".getBytes(StandardCharsets.UTF_8);
        byte[] enc = c.encryptWithRandomSalt(plaintext);
        assertArrayEquals(plaintext, c.decryptWithRandomSalt(enc));
    }

    // ------------------------------------------------------------------
    // Round trips: fromKey / fromKeyBase64 construction (VERSION_2), every method family
    // ------------------------------------------------------------------

    @Test
    void fromKey_string_noSalt_roundTrips() throws Exception {
        Crypto c = Crypto.fromKey(Crypto.generateKey());
        String plaintext = "raw-key payload, no salt";
        String enc = c.encrypt(plaintext);
        assertEquals(plaintext, c.decrypt(enc));
    }

    @Test
    void fromKey_string_randomSalt_roundTrips() throws Exception {
        Crypto c = Crypto.fromKey(Crypto.generateKey());
        String plaintext = "raw-key payload, random salt";
        String enc = c.encryptWithRandomSalt(plaintext);
        assertEquals(plaintext, c.decryptWithRandomSalt(enc));
    }

    @Test
    void fromKey_bytes_noSalt_roundTrips() throws Exception {
        Crypto c = Crypto.fromKey(Crypto.generateKey());
        byte[] plaintext = "raw-key byte payload, no salt".getBytes(StandardCharsets.UTF_8);
        byte[] enc = c.encrypt(plaintext);
        assertArrayEquals(plaintext, c.decrypt(enc));
    }

    @Test
    void fromKey_bytes_randomSalt_roundTrips() throws Exception {
        Crypto c = Crypto.fromKey(Crypto.generateKey());
        byte[] plaintext = "raw-key byte payload, random salt".getBytes(StandardCharsets.UTF_8);
        byte[] enc = c.encryptWithRandomSalt(plaintext);
        assertArrayEquals(plaintext, c.decryptWithRandomSalt(enc));
    }

    @Test
    void fromKeyBase64_roundTrips() throws Exception {
        String key = Crypto.generateKeyBase64();
        Crypto c = Crypto.fromKeyBase64(key);
        String plaintext = "raw-key payload via base64 key";
        String enc = c.encryptWithRandomSalt(plaintext);
        assertEquals(plaintext, c.decryptWithRandomSalt(enc));
    }

    @Test
    void fromKey_saltParameterIsIgnored_bothEncryptAndDecryptStillRoundTrip() throws Exception {
        Crypto c = Crypto.fromKey(Crypto.generateKey());
        String plaintext = "raw-key payload, salt argument is meaningless here";
        // Passing a "salt" has no effect for a raw-key instance (no KDF applies at all) - encrypting
        // with one salt value and decrypting while passing a completely different one still works.
        String enc = c.encrypt("someSaltThatIsIgnored", plaintext);
        assertEquals(plaintext, c.decrypt("aDifferentIgnoredSalt", enc));
    }

    @Test
    void generateKey_is32Bytes_andDiffersEachCall() {
        byte[] k1 = Crypto.generateKey();
        byte[] k2 = Crypto.generateKey();
        assertEquals(32, k1.length);
        assertEquals(32, k2.length);
        assertFalse(Arrays.equals(k1, k2), "two generated keys should not collide");
    }

    @Test
    void generateKeyBase64_decodesTo32Bytes() throws Exception {
        String b64 = Crypto.generateKeyBase64();
        byte[] decoded = Base64.decode(b64);
        assertEquals(32, decoded.length);
        // and is directly usable
        Crypto c = Crypto.fromKeyBase64(b64);
        assertNotNull(c);
    }

    @Test
    void fromKey_nullKey_throws() {
        assertThrows(IllegalArgumentException.class, () -> Crypto.fromKey(null));
    }

    @Test
    void fromKey_wrongLengthKey_throws() {
        assertThrows(IllegalArgumentException.class, () -> Crypto.fromKey(new byte[16]));
        assertThrows(IllegalArgumentException.class, () -> Crypto.fromKey(new byte[64]));
    }

    @Test
    void fromKeyBase64_invalidBase64_throws() {
        assertThrows(IllegalArgumentException.class, () -> Crypto.fromKeyBase64("not valid base64!!"));
    }

    @Test
    void fromKeyBase64_nullKey_throws() {
        assertThrows(IllegalArgumentException.class, () -> Crypto.fromKeyBase64(null));
    }

    // ------------------------------------------------------------------
    // Cross-version behavior: VERSION_2 data requires a fromKey-constructed instance to decrypt
    // ------------------------------------------------------------------

    @Test
    void version2Data_cannotBeDecryptedByPasswordBasedInstance() throws Exception {
        Crypto raw = Crypto.fromKey(Crypto.generateKey());
        String enc = raw.encryptWithRandomSalt("this was encrypted with a raw key");
        Crypto pw = new Crypto("someUnrelatedPassword");
        assertThrows(IllegalStateException.class, () -> pw.decryptWithRandomSalt(enc));
    }

    // ------------------------------------------------------------------
    // deriveOnce: VERSION_1-compatible output, cross-decryptable by a plain new Crypto(password)
    // ------------------------------------------------------------------

    @Test
    void deriveOnce_explicitSalt_roundTrips_andIsCrossCompatibleWithPlainConstructor() throws Exception {
        Crypto once = Crypto.deriveOnce("batchPassword", "fixedBatchSalt");
        String p1 = "batch payload one";
        String p2 = "batch payload two";
        String e1 = once.encrypt("fixedBatchSalt", p1);
        String e2 = once.encrypt("fixedBatchSalt", p2);
        // Round-trips on the same (deriveOnce) instance.
        assertEquals(p1, once.decrypt("fixedBatchSalt", e1));
        assertEquals(p2, once.decrypt("fixedBatchSalt", e2));
        // And decrypts fine on a fresh, ordinary password-based instance - proving deriveOnce only
        // changes *when* PBKDF2 runs, not the resulting VERSION_1 format/key.
        Crypto plain = new Crypto("batchPassword");
        assertEquals(p1, plain.decrypt("fixedBatchSalt", e1));
        assertEquals(p2, plain.decrypt("fixedBatchSalt", e2));
    }

    @Test
    void deriveOnce_defaultSalt_roundTrips_andIsCrossCompatibleWithPlainConstructor() throws Exception {
        Crypto once = Crypto.deriveOnce("batchPassword2", null);
        String plaintext = "batch payload, default salt";
        String enc = once.encrypt(plaintext);
        assertEquals(plaintext, once.decrypt(enc));
        Crypto plain = new Crypto("batchPassword2");
        assertEquals(plaintext, plain.decrypt(enc));
    }

    @Test
    void deriveOnce_emptySalt_behavesSameAsNullSalt() throws Exception {
        Crypto once = Crypto.deriveOnce("batchPassword3", "");
        String plaintext = "batch payload, empty salt";
        String enc = once.encrypt(plaintext);
        assertEquals(plaintext, once.decrypt(enc));
    }

    // ------------------------------------------------------------------
    // Key-derivation caching: same (instance, salt) reuses the cached key without changing results
    // ------------------------------------------------------------------

    @Test
    void repeatedCallsWithSameSalt_onSameInstance_produceIndependentlyDecryptableCiphertexts() throws Exception {
        // Not a timing test (not practical/reliable in a unit test) - just confirms that relying on the
        // per-instance key cache does not corrupt results across many calls with the same salt.
        Crypto c = new Crypto("cachingRegressionPassword");
        for (int i = 0; i < 25; i++) {
            String plaintext = "payload #" + i;
            String enc = c.encrypt("stableSalt", plaintext);
            assertEquals(plaintext, c.decrypt("stableSalt", enc));
        }
    }
}
