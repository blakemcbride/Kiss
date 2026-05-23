package org.kissweb.oauth.as;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * Immutable representation of an authorization code minted by the
 * {@code /authorize} endpoint and consumed by the {@code /token}
 * endpoint.
 * <br><br>
 * Codes are short-lived (default 60 seconds) and single-use.  They are
 * not persisted to disk --- the cost of losing one on restart is the
 * user clicking ``Allow'' again.
 */
public final class AuthorizationCode {

    private final String            code;
    private final String            clientId;
    private final AuthenticatedUser user;
    private final String            redirectUri;
    private final Set<String>       scopes;
    private final String            audience;
    private final String            codeChallenge;
    /** Only "S256" is allowed under OAuth 2.1; recorded here for completeness. */
    private final String            codeChallengeMethod;
    private final String            nonce;
    private final long              expiresAtEpochSeconds;

    /**
     * Construct an authorization code record.
     *
     * @param code                  the opaque code value
     * @param clientId              the client the code was issued to
     * @param user                  the authenticated user, including any extra claims to ride along on issued tokens
     * @param redirectUri           the {@code redirect_uri} the client supplied --- the same value must be sent again at token exchange
     * @param scopes                approved scopes
     * @param audience              the resource indicator (RFC 8707) the client supplied, or null
     * @param codeChallenge         the PKCE code_challenge the client supplied
     * @param codeChallengeMethod   PKCE method --- must be "S256"
     * @param nonce                 OIDC nonce, or null
     * @param expiresAtEpochSeconds when the code expires (Unix epoch seconds)
     */
    public AuthorizationCode(String code,
                             String clientId,
                             AuthenticatedUser user,
                             String redirectUri,
                             Set<String> scopes,
                             String audience,
                             String codeChallenge,
                             String codeChallengeMethod,
                             String nonce,
                             long expiresAtEpochSeconds) {
        this.code                  = code;
        this.clientId              = clientId;
        this.user                  = user;
        this.redirectUri           = redirectUri;
        this.scopes                = Collections.unmodifiableSet(new LinkedHashSet<>(scopes));
        this.audience              = audience;
        this.codeChallenge         = codeChallenge;
        this.codeChallengeMethod   = codeChallengeMethod;
        this.nonce                 = nonce;
        this.expiresAtEpochSeconds = expiresAtEpochSeconds;
    }

    /** @return the opaque code value */
    public String getCode() { return code; }

    /** @return the client the code was issued to */
    public String getClientId() { return clientId; }

    /** @return the authenticated user (subject + extra claims) */
    public AuthenticatedUser getUser() { return user; }

    /** @return the redirect URI the client supplied */
    public String getRedirectUri() { return redirectUri; }

    /** @return the approved scopes */
    public Set<String> getScopes() { return scopes; }

    /** @return the resource indicator (RFC 8707), or null */
    public String getAudience() { return audience; }

    /** @return the PKCE code_challenge */
    public String getCodeChallenge() { return codeChallenge; }

    /** @return the PKCE code_challenge_method (must be "S256") */
    public String getCodeChallengeMethod() { return codeChallengeMethod; }

    /** @return the OIDC nonce, or null */
    public String getNonce() { return nonce; }

    /** @return expiry as Unix epoch seconds */
    public long getExpiresAtEpochSeconds() { return expiresAtEpochSeconds; }

    /**
     * Check whether the code has expired as of now.
     *
     * @return true if {@code expiresAt} is in the past
     */
    public boolean isExpired() {
        return System.currentTimeMillis() / 1000L >= expiresAtEpochSeconds;
    }
}
