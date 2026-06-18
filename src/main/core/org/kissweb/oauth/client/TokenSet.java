package org.kissweb.oauth.client;

/**
 * Immutable snapshot of the tokens currently held for a provider: the
 * access token (with its absolute expiry), the optional refresh token,
 * the token type, and the granted scope.
 * <br><br>
 * Persisted and reloaded by {@link OAuthClientStore}.  {@link OAuthClient}
 * consults {@link #isExpired(int)} to decide whether the access token can
 * be used directly or must first be refreshed.
 */
public final class TokenSet {

    private final String accessToken;
    private final String refreshToken;
    private final String tokenType;
    private final String scope;
    private final long   expiresAtEpochSeconds;

    /**
     * Construct a token set.
     *
     * @param accessToken           the access token (required)
     * @param refreshToken          the refresh token, or null if none was issued
     * @param tokenType             the token type (typically {@code Bearer})
     * @param scope                 the granted scope string, or null
     * @param expiresAtEpochSeconds absolute access-token expiry, in epoch
     *                              seconds (0 means unknown / non-expiring)
     */
    public TokenSet(String accessToken, String refreshToken, String tokenType,
                    String scope, long expiresAtEpochSeconds) {
        this.accessToken           = accessToken;
        this.refreshToken          = refreshToken;
        this.tokenType             = tokenType;
        this.scope                 = scope;
        this.expiresAtEpochSeconds = expiresAtEpochSeconds;
    }

    /**
     * Get the access token.
     * @return the access token
     */
    public String getAccessToken() {
        return accessToken;
    }

    /**
     * Get the refresh token, if any.
     * @return the refresh token, or null
     */
    public String getRefreshToken() {
        return refreshToken;
    }

    /**
     * Get the token type.
     * @return the token type (typically {@code Bearer})
     */
    public String getTokenType() {
        return tokenType;
    }

    /**
     * Get the granted scope string.
     * @return the scope, or null
     */
    public String getScope() {
        return scope;
    }

    /**
     * Get the absolute access-token expiry in epoch seconds.
     * @return expiry epoch seconds, or 0 if unknown / non-expiring
     */
    public long getExpiresAtEpochSeconds() {
        return expiresAtEpochSeconds;
    }

    /**
     * Whether a refresh token is available.
     * @return true if a non-empty refresh token is held
     */
    public boolean hasRefreshToken() {
        return refreshToken != null && !refreshToken.isEmpty();
    }

    /**
     * Whether the access token is expired (or about to expire) given a
     * safety margin.  A {@code expiresAtEpochSeconds} of 0 is treated as
     * non-expiring and never reports expired.
     *
     * @param skewSeconds seconds of safety margin to treat the token as
     *                    expired before its actual expiry
     * @return true if the token should be considered expired
     */
    public boolean isExpired(int skewSeconds) {
        if (expiresAtEpochSeconds <= 0)
            return false;
        final long now = System.currentTimeMillis() / 1000L;
        return now >= (expiresAtEpochSeconds - skewSeconds);
    }
}
