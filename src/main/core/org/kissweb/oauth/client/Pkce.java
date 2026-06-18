package org.kissweb.oauth.client;

import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import java.util.Base64;

/**
 * Client-side PKCE (Proof Key for Code Exchange, RFC 7636) pair: a
 * randomly generated {@code code_verifier} and its derived
 * {@code code_challenge}.
 * <br><br>
 * OAuth 2.1 makes PKCE mandatory and permits only the {@code S256}
 * method:
 * <pre>
 *   code_challenge = base64url(SHA-256(code_verifier))
 * </pre>
 * The verifier is sent at the token exchange; the challenge is sent at
 * {@code /authorize}.  This is the client-side counterpart of the
 * resource/authorization-server {@code PkceValidator}; the S256
 * computation is duplicated here (a few lines) deliberately, so the
 * client role carries no dependency on the authorization-server package.
 */
public final class Pkce {

    /** The only PKCE method OAuth 2.1 permits. */
    public static final String METHOD_S256 = "S256";

    private static final SecureRandom RANDOM = new SecureRandom();

    private final String codeVerifier;
    private final String codeChallenge;

    private Pkce(String codeVerifier, String codeChallenge) {
        this.codeVerifier  = codeVerifier;
        this.codeChallenge = codeChallenge;
    }

    /**
     * Generate a fresh PKCE pair with a 32-byte (256-bit) verifier, which
     * base64url-encodes to 43 characters --- within the RFC 7636 range of
     * 43 to 128 characters.
     *
     * @return a new PKCE pair
     */
    public static Pkce generate() {
        final byte[] bytes = new byte[32];
        RANDOM.nextBytes(bytes);
        final String verifier = Base64.getUrlEncoder().withoutPadding().encodeToString(bytes);
        return new Pkce(verifier, sha256Base64Url(verifier));
    }

    /**
     * Get the code verifier (sent at the token exchange).
     * @return the code verifier
     */
    public String getCodeVerifier() {
        return codeVerifier;
    }

    /**
     * Get the code challenge (sent at {@code /authorize}).
     * @return the code challenge
     */
    public String getCodeChallenge() {
        return codeChallenge;
    }

    /**
     * Get the PKCE method, always {@code S256}.
     * @return the method name
     */
    public String getMethod() {
        return METHOD_S256;
    }

    /**
     * Compute the {@code S256} challenge for a given verifier:
     * {@code base64url(SHA-256(verifier))} with no padding.
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
}
