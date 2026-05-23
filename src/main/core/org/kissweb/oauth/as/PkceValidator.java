package org.kissweb.oauth.as;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;

/**
 * PKCE (Proof Key for Code Exchange, RFC 7636) verifier.
 * <br><br>
 * OAuth 2.1 makes PKCE mandatory and forbids the {@code plain} method,
 * so this class supports {@code S256} only:
 * <pre>
 *   code_challenge = base64url(SHA-256(code_verifier))
 * </pre>
 * The {@code /authorize} endpoint records the supplied
 * {@code code_challenge}; the {@code /token} endpoint recomputes from
 * the {@code code_verifier} the client supplies and compares.
 */
public final class PkceValidator {

    /** The only PKCE method OAuth 2.1 permits. */
    public static final String METHOD_S256 = "S256";

    private PkceValidator() { }

    /**
     * Verify that the supplied code verifier produces the supplied code
     * challenge under the named method.
     *
     * @param method        the PKCE method, must be {@code S256}
     * @param codeVerifier  the verifier the client supplied at token exchange
     * @param codeChallenge the challenge the client supplied at /authorize
     * @return true if the verifier matches the challenge under the method
     */
    public static boolean verify(String method, String codeVerifier, String codeChallenge) {
        if (!METHOD_S256.equals(method))
            return false;
        if (codeVerifier == null || codeChallenge == null)
            return false;
        if (codeVerifier.length() < 43 || codeVerifier.length() > 128)
            return false;
        final String computed = sha256Base64Url(codeVerifier);
        // Constant-time compare to avoid leaking partial matches.
        return constantTimeEquals(computed, codeChallenge);
    }

    /**
     * Compute the {@code S256} challenge for a given verifier.  Exposed
     * for tests --- production code should call {@link #verify}.
     *
     * @param codeVerifier the verifier
     * @return the base64url-encoded SHA-256 hash
     */
    public static String sha256Base64Url(String codeVerifier) {
        try {
            final MessageDigest md = MessageDigest.getInstance("SHA-256");
            final byte[] digest = md.digest(codeVerifier.getBytes(StandardCharsets.US_ASCII));
            return Base64.getUrlEncoder().withoutPadding().encodeToString(digest);
        } catch (NoSuchAlgorithmException e) {
            // SHA-256 is required by every JRE.
            throw new IllegalStateException(e);
        }
    }

    private static boolean constantTimeEquals(String a, String b) {
        if (a.length() != b.length())
            return false;
        int diff = 0;
        for (int i = 0; i < a.length(); i++)
            diff |= a.charAt(i) ^ b.charAt(i);
        return diff == 0;
    }
}
