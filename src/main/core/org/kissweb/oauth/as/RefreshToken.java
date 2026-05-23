package org.kissweb.oauth.as;

import org.kissweb.json.JSONObject;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * Immutable representation of a refresh token.  Persisted to
 * {@code oauth.ini} as a {@code [refresh.<jti>]} section.
 * <br><br>
 * Refresh tokens are issued in \emph{families}: when a refresh token is
 * exchanged for a new access token, a new refresh token is issued in the
 * same family ({@link #getFamilyId}) and the old one is marked as
 * rotated.  If a rotated (consumed) refresh token is ever presented
 * again, the entire family is revoked --- this is the OAuth 2.1
 * detection-of-stolen-refresh-tokens mechanism described in RFC 6749bis
 * \S{}6.1.  See {@link RefreshTokenStore}.
 */
public final class RefreshToken {

    private final String      jti;
    private final String      familyId;
    private final String      clientId;
    private final String      userSubject;
    private final JSONObject  userExtraClaims;
    private final Set<String> scopes;
    private final String      audience;
    private final long        createdAtEpochSeconds;
    private final long        expiresAtEpochSeconds;
    /** Null for a freshly-issued token; set to the previous JTI in the family when this one is rotated out. */
    private final String      rotatedToJti;

    /**
     * Construct a refresh token record.
     *
     * @param jti                   the unique token identifier --- also the secret value handed to the client
     * @param familyId              groups successive rotations of the same logical session
     * @param clientId              the client the token was issued to
     * @param userSubject           the user the token represents
     * @param userExtraClaims       extra claims to ride along on access tokens minted from this refresh token; may be empty, never null
     * @param scopes                scopes carried on tokens minted from this refresh token
     * @param audience              the resource indicator the access tokens will carry, or null
     * @param createdAtEpochSeconds when the token was created
     * @param expiresAtEpochSeconds when the token expires
     * @param rotatedToJti          null for a current token; for a rotated one, the JTI of the successor token in this family
     */
    public RefreshToken(String jti,
                        String familyId,
                        String clientId,
                        String userSubject,
                        JSONObject userExtraClaims,
                        Set<String> scopes,
                        String audience,
                        long createdAtEpochSeconds,
                        long expiresAtEpochSeconds,
                        String rotatedToJti) {
        this.jti                   = jti;
        this.familyId              = familyId;
        this.clientId              = clientId;
        this.userSubject           = userSubject;
        this.userExtraClaims       = userExtraClaims != null ? userExtraClaims : new JSONObject();
        this.scopes                = Collections.unmodifiableSet(new LinkedHashSet<>(scopes));
        this.audience              = audience;
        this.createdAtEpochSeconds = createdAtEpochSeconds;
        this.expiresAtEpochSeconds = expiresAtEpochSeconds;
        this.rotatedToJti          = rotatedToJti;
    }

    /** @return extra claims to ride along on access tokens minted from this refresh token */
    public JSONObject getUserExtraClaims() { return userExtraClaims; }

    /** @return the {@link AuthenticatedUser} reconstituted from {@code userSubject} + {@code userExtraClaims} */
    public AuthenticatedUser getUser() { return new AuthenticatedUser(userSubject, userExtraClaims); }

    /** @return the token's unique id (also the secret value sent to the client) */
    public String getJti() { return jti; }

    /** @return the family id (constant across all rotations of the same logical session) */
    public String getFamilyId() { return familyId; }

    /** @return the client the token was issued to */
    public String getClientId() { return clientId; }

    /** @return the user the token represents */
    public String getUserSubject() { return userSubject; }

    /** @return scopes carried on access tokens minted from this refresh token */
    public Set<String> getScopes() { return scopes; }

    /** @return the resource indicator access tokens will carry, or null */
    public String getAudience() { return audience; }

    /** @return when the token was created (Unix epoch seconds) */
    public long getCreatedAtEpochSeconds() { return createdAtEpochSeconds; }

    /** @return when the token expires (Unix epoch seconds) */
    public long getExpiresAtEpochSeconds() { return expiresAtEpochSeconds; }

    /** @return null for a current token; the successor JTI for a rotated one */
    public String getRotatedToJti() { return rotatedToJti; }

    /** @return true if this token has been rotated out (i.e. consumed by a successful refresh) */
    public boolean isRotated() { return rotatedToJti != null; }

    /** @return true if the token's expiry is in the past */
    public boolean isExpired() {
        return System.currentTimeMillis() / 1000L >= expiresAtEpochSeconds;
    }

    /**
     * Produce a copy marked as rotated, pointing at the given successor.
     *
     * @param successorJti the JTI of the new refresh token replacing this one
     * @return a new RefreshToken with {@code rotatedToJti} set
     */
    public RefreshToken withRotation(String successorJti) {
        return new RefreshToken(jti, familyId, clientId, userSubject, userExtraClaims, scopes,
                audience, createdAtEpochSeconds, expiresAtEpochSeconds, successorJti);
    }
}
