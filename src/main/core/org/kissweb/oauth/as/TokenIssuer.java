package org.kissweb.oauth.as;

import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;

import java.nio.charset.StandardCharsets;
import java.security.NoSuchAlgorithmException;
import java.security.Signature;
import java.util.Base64;
import java.util.Set;
import java.util.UUID;

/**
 * Builds and signs access tokens (RS256 JWTs).
 * <br><br>
 * The signed claim set follows the conventions in RFC 9068 (\emph{JSON
 * Web Token (JWT) Profile for OAuth 2.0 Access Tokens}):
 * <ul>
 *   <li>{@code iss} --- the AS issuer URL ({@code OAuthAsIssuer})</li>
 *   <li>{@code sub} --- the user's subject identifier</li>
 *   <li>{@code aud} --- the resource the token is bound to (RFC 8707
 *       resource indicator), or, if none was requested, the AS issuer
 *       (the token is effectively unbound and any RS that trusts this
 *       AS may accept it)</li>
 *   <li>{@code exp}, {@code iat}, {@code nbf} --- standard time claims</li>
 *   <li>{@code jti} --- unique token id (UUID)</li>
 *   <li>{@code scope} --- space-separated string of granted scopes</li>
 *   <li>{@code client_id} --- the client the token was issued to</li>
 *   <li>any extra claims supplied via {@link AuthenticatedUser#getExtraClaims}
 *       (email, groups, custom application claims)</li>
 * </ul>
 * Signed with the {@link KeyManager}'s current RSA private key using
 * {@code SHA256withRSA}.  The {@code kid} header matches the JWKS the
 * {@link org.kissweb.oauth.JwksCache} on the resource-server side will
 * fetch from this AS.
 */
public final class TokenIssuer {

    private TokenIssuer() { }

    /**
     * Build and sign an access token.
     *
     * @param user      the authenticated user the token represents
     * @param clientId  the client the token is being issued to
     * @param scopes    the granted scopes
     * @param audience  the resource indicator (RFC 8707), or null to
     *                  bind the token to the AS issuer itself
     * @return the JWT compact serialization {@code header.payload.signature}
     */
    public static String issueAccessToken(AuthenticatedUser user,
                                          String clientId,
                                          Set<String> scopes,
                                          String audience) {
        final AuthorizationServerConfig cfg = AuthorizationServerConfig.get();
        final KeyManager km = KeyManager.get();
        final long now = System.currentTimeMillis() / 1000L;

        final JSONObject claims = new JSONObject();
        // Copy any user-provided extras first so they cannot overwrite
        // protocol-mandated claims.
        final JSONObject extras = user.getExtraClaims();
        for (String k : extras.keySet())
            claims.put(k, extras.get(k));

        claims.put("iss",       cfg.getIssuer());
        claims.put("sub",       user.getSubject());
        claims.put("aud",       audience != null && !audience.isEmpty() ? audience : cfg.getIssuer());
        claims.put("exp",       now + cfg.getAccessTokenTtlSeconds());
        claims.put("iat",       now);
        claims.put("nbf",       now);
        claims.put("jti",       UUID.randomUUID().toString());
        claims.put("scope",     String.join(" ", scopes));
        claims.put("client_id", clientId);
        // RFC 9068 type --- tells generic JWT consumers this is an OAuth
        // access token and not an ID token / something else.
        claims.put("typ",       "at+jwt");

        final JSONObject header = new JSONObject();
        header.put("alg", "RS256");
        header.put("kid", km.getKid());
        header.put("typ", "at+jwt");

        return sign(header, claims);
    }

    /**
     * Generate a fresh refresh-token JTI.  Refresh tokens are not
     * structured JWTs --- they are opaque random identifiers issued
     * directly to the client.  Their state lives in
     * {@link RefreshTokenStore}.
     *
     * @return a random refresh token value
     */
    public static String issueRefreshTokenValue() {
        return randomToken();
    }

    /**
     * Generate a fresh authorization-code value.
     *
     * @return a random authorization-code value
     */
    public static String issueAuthorizationCodeValue() {
        return randomToken();
    }

    /**
     * Build the {@code scope} value for an OAuth token response from a
     * set of scopes.  Convenience for token-endpoint code.
     *
     * @param scopes the granted scopes
     * @return space-separated scope string
     */
    public static String scopesAsString(Set<String> scopes) {
        return String.join(" ", scopes);
    }

    /**
     * Build a JSON array representation of the supplied scopes.
     * Convenience for metadata documents.
     *
     * @param scopes the scopes
     * @return a JSONArray containing each scope as a string
     */
    public static JSONArray scopesAsArray(Set<String> scopes) {
        final JSONArray arr = new JSONArray();
        for (String s : scopes)
            arr.put(s);
        return arr;
    }

    // ------------------------------------------------------------------

    private static String sign(JSONObject header, JSONObject claims) {
        final String headerB64  = b64u(header.toString());
        final String payloadB64 = b64u(claims.toString());
        final String signingInput = headerB64 + "." + payloadB64;
        try {
            final Signature sig = Signature.getInstance("SHA256withRSA");
            sig.initSign(KeyManager.get().getPrivateKey());
            sig.update(signingInput.getBytes(StandardCharsets.US_ASCII));
            final byte[] signature = sig.sign();
            return signingInput + "." + Base64.getUrlEncoder().withoutPadding().encodeToString(signature);
        } catch (NoSuchAlgorithmException | java.security.SignatureException | java.security.InvalidKeyException e) {
            throw new IllegalStateException("Failed to sign access token", e);
        }
    }

    private static String b64u(String s) {
        return Base64.getUrlEncoder().withoutPadding().encodeToString(s.getBytes(StandardCharsets.UTF_8));
    }

    private static String randomToken() {
        // 256 bits of entropy, base64url-encoded.  ~43 chars, URL-safe,
        // no padding.
        final byte[] bytes = new byte[32];
        new java.security.SecureRandom().nextBytes(bytes);
        return Base64.getUrlEncoder().withoutPadding().encodeToString(bytes);
    }
}
