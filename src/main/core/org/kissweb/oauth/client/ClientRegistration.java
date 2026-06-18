package org.kissweb.oauth.client;

/**
 * Immutable record of this client's identity with a remote authorization
 * server: the {@code client_id} (and, for a confidential client, the
 * {@code client_secret}) together with the {@code redirect_uri} the
 * registration is bound to.
 * <br><br>
 * Produced either by {@link DynamicClientRegistration} (RFC 7591) or from
 * a pre-registered {@code ClientId}/{@code ClientSecret} configured in
 * {@code application.ini}, and cached by {@link OAuthClientStore} so the
 * client registers at most once per provider.
 */
public final class ClientRegistration {

    private final String clientId;
    private final String clientSecret;
    private final String redirectUri;

    /**
     * Construct a client registration record.
     *
     * @param clientId     the client id issued by (or configured for) the server
     * @param clientSecret the client secret for a confidential client, or
     *                     null for a public (PKCE-only) client
     * @param redirectUri  the redirect URI this registration is bound to
     */
    public ClientRegistration(String clientId, String clientSecret, String redirectUri) {
        this.clientId     = clientId;
        this.clientSecret = clientSecret;
        this.redirectUri  = redirectUri;
    }

    /**
     * Get the client id.
     * @return the client id
     */
    public String getClientId() {
        return clientId;
    }

    /**
     * Get the client secret, if any.
     * @return the client secret, or null for a public client
     */
    public String getClientSecret() {
        return clientSecret;
    }

    /**
     * Get the redirect URI this registration is bound to.
     * @return the redirect URI
     */
    public String getRedirectUri() {
        return redirectUri;
    }

    /**
     * Whether this is a confidential client (has a secret).
     * @return true if a client secret is present
     */
    public boolean isConfidential() {
        return clientSecret != null && !clientSecret.isEmpty();
    }
}
