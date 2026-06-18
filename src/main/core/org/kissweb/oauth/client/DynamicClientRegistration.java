package org.kissweb.oauth.client;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.RestClient;
import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;

import java.io.IOException;
import java.time.Duration;

/**
 * Registers this application as a client with a remote authorization
 * server using RFC 7591 Dynamic Client Registration.
 * <br><br>
 * Registers a <em>public</em> client ({@code token_endpoint_auth_method=none})
 * whose security rests on PKCE --- the right shape for an app that cannot
 * keep a client secret confidential and the kind MCP servers expect.  The
 * resulting {@link ClientRegistration} is cached by {@link OAuthClientStore}
 * so registration happens at most once per provider.
 * <br><br>
 * Used only when the provider has no pre-registered {@code ClientId} in
 * {@code application.ini} and the discovered metadata advertises a
 * {@code registration_endpoint}.  If neither is true, the caller surfaces
 * a configuration error.
 */
public final class DynamicClientRegistration {

    private static final Logger logger = LogManager.getLogger(DynamicClientRegistration.class);

    private DynamicClientRegistration() { }

    /**
     * Register a public client and return its credentials.
     *
     * @param provider    the provider being registered with
     * @param discovery   the discovered metadata (must have a registration endpoint)
     * @param redirectUri the redirect URI to register
     * @return the resulting client registration
     * @throws OAuthClientException if the server has no DCR endpoint or the
     *                              registration request fails
     */
    public static ClientRegistration register(OAuthClientProvider provider,
                                              DiscoveryMetadata discovery,
                                              String redirectUri) {
        final String endpoint = discovery.getRegistrationEndpoint();
        if (endpoint == null || endpoint.isEmpty())
            throw new OAuthClientException("Provider '" + provider.getName()
                    + "' has no pre-registered ClientId and its authorization server does not"
                    + " support Dynamic Client Registration; set ClientId in the"
                    + " [OAuthClient " + provider.getName() + "] section of application.ini");

        final JSONObject body = new JSONObject();
        body.put("client_name", "Kiss OAuth Client (" + provider.getName() + ")");
        body.put("redirect_uris", new JSONArray().put(redirectUri));
        body.put("grant_types", new JSONArray().put("authorization_code").put("refresh_token"));
        body.put("response_types", new JSONArray().put("code"));
        body.put("token_endpoint_auth_method", "none");
        if (!provider.getScopeString().isEmpty())
            body.put("scope", provider.getScopeString());

        final RestClient rc = new RestClient().setTimeouts(Duration.ofSeconds(15), Duration.ofSeconds(15));
        final JSONObject headers = new JSONObject();
        headers.put("Content-Type", "application/json");
        headers.put("Accept", "application/json");

        final JSONObject resp;
        try {
            resp = rc.jsonCall("POST", endpoint, body.toString(), headers);
        } catch (IOException e) {
            throw new OAuthClientException("Dynamic client registration request to " + endpoint
                    + " failed for provider '" + provider.getName() + "'", e);
        }
        if (resp == null)
            throw new OAuthClientException("Dynamic client registration at " + endpoint
                    + " returned HTTP " + rc.getResponseCode() + ": " + rc.getResponseString());

        final String clientId = resp.getString("client_id", null);
        if (clientId == null || clientId.isEmpty())
            throw new OAuthClientException("Dynamic client registration response had no client_id: "
                    + resp);
        // Most public-client registrations return no secret; honor one if present.
        final String clientSecret = resp.getString("client_secret", null);

        logger.info("Dynamically registered client_id '" + clientId + "' for provider '"
                + provider.getName() + "'");
        return new ClientRegistration(clientId, clientSecret, redirectUri);
    }
}
