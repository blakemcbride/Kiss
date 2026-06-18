package org.kissweb.oauth.client;

/**
 * Immutable result of OAuth 2.1 authorization-server discovery: the
 * endpoints the client needs to drive the authorization-code flow,
 * resolved from the remote server's well-known metadata documents by
 * {@link OAuthMetadataDiscovery} and cached by {@link OAuthClientStore}.
 */
public final class DiscoveryMetadata {

    private final String issuer;
    private final String authorizationEndpoint;
    private final String tokenEndpoint;
    private final String registrationEndpoint;

    /**
     * Construct a discovery result.
     *
     * @param issuer                the authorization server's issuer identifier
     * @param authorizationEndpoint the {@code authorization_endpoint} URL
     * @param tokenEndpoint         the {@code token_endpoint} URL
     * @param registrationEndpoint  the {@code registration_endpoint} URL, or
     *                              null if the server does not advertise DCR
     */
    public DiscoveryMetadata(String issuer, String authorizationEndpoint,
                             String tokenEndpoint, String registrationEndpoint) {
        this.issuer                = issuer;
        this.authorizationEndpoint = authorizationEndpoint;
        this.tokenEndpoint         = tokenEndpoint;
        this.registrationEndpoint  = registrationEndpoint;
    }

    /**
     * Get the issuer identifier.
     * @return the issuer
     */
    public String getIssuer() {
        return issuer;
    }

    /**
     * Get the authorization endpoint URL.
     * @return the authorization endpoint
     */
    public String getAuthorizationEndpoint() {
        return authorizationEndpoint;
    }

    /**
     * Get the token endpoint URL.
     * @return the token endpoint
     */
    public String getTokenEndpoint() {
        return tokenEndpoint;
    }

    /**
     * Get the dynamic client registration endpoint URL, if advertised.
     * @return the registration endpoint, or null if the server has no DCR
     */
    public String getRegistrationEndpoint() {
        return registrationEndpoint;
    }
}
