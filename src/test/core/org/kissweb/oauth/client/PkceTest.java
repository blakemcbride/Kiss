package org.kissweb.oauth.client;

import org.junit.jupiter.api.Test;
import org.kissweb.oauth.as.PkceValidator;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Verifies the client-side PKCE generator produces verifiers and
 * challenges that the authorization-server-side {@link PkceValidator}
 * accepts --- the two halves must agree on the S256 computation.
 */
class PkceTest {

    @Test
    void generatesVerifierWithinRfcLength() {
        final Pkce p = Pkce.generate();
        final int len = p.getCodeVerifier().length();
        assertTrue(len >= 43 && len <= 128, "verifier length out of RFC 7636 range: " + len);
        assertEquals(Pkce.METHOD_S256, p.getMethod());
    }

    @Test
    void challengeVerifiesAgainstAuthServerValidator() {
        for (int i = 0; i < 50; i++) {
            final Pkce p = Pkce.generate();
            assertTrue(PkceValidator.verify(Pkce.METHOD_S256, p.getCodeVerifier(), p.getCodeChallenge()),
                    "AS validator rejected a challenge the client generated");
        }
    }

    @Test
    void distinctVerifiersEachCall() {
        final Pkce a = Pkce.generate();
        final Pkce b = Pkce.generate();
        assertNotEquals(a.getCodeVerifier(), b.getCodeVerifier());
        assertNotEquals(a.getCodeChallenge(), b.getCodeChallenge());
    }

    @Test
    void wrongVerifierIsRejected() {
        final Pkce p = Pkce.generate();
        final Pkce other = Pkce.generate();
        assertFalse(PkceValidator.verify(Pkce.METHOD_S256, other.getCodeVerifier(), p.getCodeChallenge()));
    }
}
