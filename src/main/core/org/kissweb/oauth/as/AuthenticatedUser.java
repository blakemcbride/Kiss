package org.kissweb.oauth.as;

import org.kissweb.json.JSONObject;

/**
 * The result of a successful credential check by a
 * {@link UserAuthenticator}.  Holds the user's subject identifier
 * (which goes into the {@code sub} claim of every issued token) and an
 * optional bag of additional claims that the
 * {@link TokenIssuer} will copy onto access tokens.
 * <br><br>
 * Typical extra claims: {@code email}, {@code preferred_username},
 * {@code groups}, custom application-specific claims.  Whatever the
 * application puts here, the resource server (and any other consumer of
 * the access token) can read via {@link org.kissweb.oauth.ValidatedToken#getClaims}.
 */
public final class AuthenticatedUser {

    private final String     subject;
    private final JSONObject extraClaims;

    /**
     * Construct an authenticated user with just a subject identifier.
     *
     * @param subject the user's identifier (becomes the {@code sub} claim)
     */
    public AuthenticatedUser(String subject) {
        this(subject, new JSONObject());
    }

    /**
     * Construct an authenticated user with extra claims to be included
     * on every access token issued for this user.
     *
     * @param subject     the user's identifier (becomes the {@code sub} claim)
     * @param extraClaims additional claims; copied onto issued tokens
     */
    public AuthenticatedUser(String subject, JSONObject extraClaims) {
        if (subject == null || subject.isEmpty())
            throw new IllegalArgumentException("AuthenticatedUser subject is required");
        this.subject     = subject;
        this.extraClaims = extraClaims != null ? extraClaims : new JSONObject();
    }

    /** @return the user's subject identifier */
    public String getSubject() {
        return subject;
    }

    /** @return additional claims to embed in issued tokens; may be empty, never null */
    public JSONObject getExtraClaims() {
        return extraClaims;
    }
}
