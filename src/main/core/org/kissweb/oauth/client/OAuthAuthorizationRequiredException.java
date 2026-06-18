package org.kissweb.oauth.client;

/**
 * Signals that no usable token is available for a provider and the user
 * must complete an interactive authorization (the browser redirect flow)
 * before an access token can be obtained.
 * <br><br>
 * This is a normal control-flow signal, not an error: a caller of
 * {@link OAuthClient#getAccessToken()} that catches this should kick off
 * the login flow (see {@link OAuthClient#beginAuthorization(String)}),
 * not treat it as a failure.  It carries the provider name so the caller
 * knows which provider needs authorization.
 */
public class OAuthAuthorizationRequiredException extends RuntimeException {

    private final String provider;

    /**
     * Create the exception for the named provider.
     *
     * @param provider the provider that requires interactive authorization
     */
    public OAuthAuthorizationRequiredException(String provider) {
        super("Interactive authorization is required for OAuth client provider '" + provider + "'");
        this.provider = provider;
    }

    /**
     * Get the name of the provider that requires authorization.
     *
     * @return the provider name
     */
    public String getProvider() {
        return provider;
    }
}
