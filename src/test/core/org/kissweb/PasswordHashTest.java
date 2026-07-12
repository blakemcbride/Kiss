package org.kissweb;

import org.junit.jupiter.api.Test;

import java.util.Base64;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Tests for {@link PasswordHash}.
 * <br><br>
 * {@code FIXTURE_PWHASH_STORED} was produced by the ORIGINAL, UNMODIFIED {@code PasswordHash} class
 * (before {@code needsRehash} existed and before its internal constant-time compare was promoted to
 * {@link ConstantTime}). It must continue to verify correctly forever.
 */
class PasswordHashTest {

    private static final String FIXTURE_PWHASH_PASSWORD = "FixtureUserPassword!42";
    private static final String FIXTURE_PWHASH_STORED =
            "pbkdf2$600000$qbO1cHfxh96Y79JjOJzANg$Z8rdO5ZENmJLMHdgL1u4FkIfCEJIRtGMg+bsQvFbbZI";

    @Test
    void fixture_preChangeHash_stillVerifies() {
        assertTrue(PasswordHash.verify(FIXTURE_PWHASH_PASSWORD, FIXTURE_PWHASH_STORED));
    }

    @Test
    void fixture_preChangeHash_wrongPasswordFails() {
        assertFalse(PasswordHash.verify("not the right password", FIXTURE_PWHASH_STORED));
    }

    @Test
    void fixture_preChangeHash_isRecognizedAsHashed() {
        assertTrue(PasswordHash.isHashed(FIXTURE_PWHASH_STORED));
    }

    @Test
    void fixture_preChangeHash_atCurrentIterationCount_needsNoRehash() {
        // The fixture was produced at 600,000 iterations, which is still PasswordHash's current default,
        // so it should NOT be flagged for rehashing.
        assertFalse(PasswordHash.needsRehash(FIXTURE_PWHASH_STORED));
    }

    // ------------------------------------------------------------------
    // hash()/verify()/isHashed() round trip and basic behavior
    // ------------------------------------------------------------------

    @Test
    void hash_thenVerify_succeeds() {
        String stored = PasswordHash.hash("correctHorseBatteryStaple");
        assertTrue(PasswordHash.verify("correctHorseBatteryStaple", stored));
    }

    @Test
    void hash_thenVerify_wrongPassword_fails() {
        String stored = PasswordHash.hash("correctHorseBatteryStaple");
        assertFalse(PasswordHash.verify("wrongPassword", stored));
    }

    @Test
    void hash_producesDifferentSaltEachTime() {
        String h1 = PasswordHash.hash("samePassword");
        String h2 = PasswordHash.hash("samePassword");
        assertNotEquals(h1, h2, "each hash should use a fresh random salt");
        assertTrue(PasswordHash.verify("samePassword", h1));
        assertTrue(PasswordHash.verify("samePassword", h2));
    }

    @Test
    void hash_isRecognizedAsHashed() {
        assertTrue(PasswordHash.isHashed(PasswordHash.hash("anyPassword")));
    }

    @Test
    void isHashed_plainTextValue_isFalse() {
        assertFalse(PasswordHash.isHashed("plainTextPassword"));
        assertFalse(PasswordHash.isHashed(null));
    }

    @Test
    void verify_nullOrMalformedInputs_returnFalseNotThrow() {
        assertFalse(PasswordHash.verify(null, "pbkdf2$600000$abc$def"));
        assertFalse(PasswordHash.verify("password", null));
        assertFalse(PasswordHash.verify("password", "not-even-close-to-valid"));
        assertFalse(PasswordHash.verify("password", "pbkdf2$notanumber$abc$def"));
    }

    // ------------------------------------------------------------------
    // needsRehash
    // ------------------------------------------------------------------

    @Test
    void needsRehash_freshHash_isFalse() {
        String stored = PasswordHash.hash("freshPassword");
        assertFalse(PasswordHash.needsRehash(stored));
    }

    @Test
    void needsRehash_lowIterationCount_isTrue() {
        // Hand-craft a stored value with an iteration count below the current default (600,000) but
        // otherwise well-formed, to exercise the "weaker work factor" path.
        String salt = Base64.getEncoder().withoutPadding().encodeToString(new byte[16]);
        String hash = Base64.getEncoder().withoutPadding().encodeToString(new byte[32]);
        String weak = "pbkdf2$1000$" + salt + "$" + hash;
        assertTrue(PasswordHash.needsRehash(weak));
    }

    @Test
    void needsRehash_nonHashedValue_isTrue() {
        assertTrue(PasswordHash.needsRehash("plainTextLegacyPassword"));
        assertTrue(PasswordHash.needsRehash(null));
    }

    @Test
    void needsRehash_malformedHashedValue_isTrue() {
        assertTrue(PasswordHash.needsRehash("pbkdf2$notanumber$abc$def"));
        assertTrue(PasswordHash.needsRehash("pbkdf2$600000$onlyThreeParts"));
    }
}
