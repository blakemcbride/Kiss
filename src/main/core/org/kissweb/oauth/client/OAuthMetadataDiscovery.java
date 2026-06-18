package org.kissweb.oauth.client;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.RestClient;
import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;

import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;
import java.time.Duration;

/**
 * Discovers a remote authorization server's endpoints, following the
 * same well-known ladder the resource-server side of this package uses:
 * <ol>
 *   <li>Fetch the resource's RFC 9728 protected-resource metadata at
 *       {@code <origin>/.well-known/oauth-protected-resource} to learn
 *       which authorization server protects it.</li>
 *   <li>Fetch that authorization server's RFC 8414 metadata at
 *       {@code <issuer>/.well-known/oauth-authorization-server},
 *       falling back to the OIDC document at
 *       {@code <issuer>/.well-known/openid-configuration}.</li>
 * </ol>
 * If step 1 yields nothing (the resource publishes no protected-resource
 * metadata), the provider URL's own origin is treated as the issuer and
 * step 2 is attempted directly --- so a server that is both resource and
 * authorization server still resolves.
 * <br><br>
 * The result is a {@link DiscoveryMetadata}; callers cache it via
 * {@link OAuthClientStore}.  All network I/O uses {@link RestClient}.
 */
public final class OAuthMetadataDiscovery {

    private static final Logger logger = LogManager.getLogger(OAuthMetadataDiscovery.class);

    private OAuthMetadataDiscovery() { }

    /**
     * Discover the endpoints for a provider.
     *
     * @param provider the configured provider
     * @return the resolved discovery metadata
     * @throws OAuthClientException if discovery fails or the document is
     *                              missing the required endpoints
     */
    public static DiscoveryMetadata discover(OAuthClientProvider provider) {
        final RestClient rc = newClient();
        final String resourceOrigin = origin(provider.getUrl());

        String issuer = resolveIssuer(rc, resourceOrigin);
        if (issuer == null)
            issuer = resourceOrigin;   // server is its own AS, or publishes no PRM

        final JSONObject asMeta = fetchAsMetadata(rc, issuer);
        if (asMeta == null)
            throw new OAuthClientException("Could not fetch authorization-server metadata for issuer '"
                    + issuer + "' (provider '" + provider.getName() + "')");

        final String authorizationEndpoint = asMeta.getString("authorization_endpoint", null);
        final String tokenEndpoint         = asMeta.getString("token_endpoint", null);
        final String registrationEndpoint  = asMeta.getString("registration_endpoint", null);
        final String resolvedIssuer        = asMeta.getString("issuer", issuer);

        if (authorizationEndpoint == null || authorizationEndpoint.isEmpty())
            throw new OAuthClientException("Authorization-server metadata for '" + issuer
                    + "' has no authorization_endpoint");
        if (tokenEndpoint == null || tokenEndpoint.isEmpty())
            throw new OAuthClientException("Authorization-server metadata for '" + issuer
                    + "' has no token_endpoint");

        logger.info("Discovered OAuth endpoints for provider '" + provider.getName()
                + "': issuer=" + resolvedIssuer + ", token=" + tokenEndpoint
                + ", registration=" + (registrationEndpoint == null ? "<none>" : registrationEndpoint));

        return new DiscoveryMetadata(resolvedIssuer, authorizationEndpoint, tokenEndpoint, registrationEndpoint);
    }

    // ------------------------------------------------------------------

    /**
     * Try to read the protected-resource metadata and return the first
     * advertised authorization server, or null if none is published.
     */
    private static String resolveIssuer(RestClient rc, String resourceOrigin) {
        final JSONObject prm = getJson(rc, resourceOrigin + "/.well-known/oauth-protected-resource");
        if (prm == null)
            return null;
        if (!prm.has("authorization_servers"))
            return null;
        final JSONArray servers = prm.getJSONArray("authorization_servers");
        if (servers == null || servers.length() == 0)
            return null;
        final String issuer = servers.optString(0, null);
        if (issuer == null || issuer.isEmpty())
            return null;
        return trimTrailingSlash(issuer);
    }

    private static JSONObject fetchAsMetadata(RestClient rc, String issuer) {
        JSONObject meta = getJson(rc, issuer + "/.well-known/oauth-authorization-server");
        if (meta == null)
            meta = getJson(rc, issuer + "/.well-known/openid-configuration");
        return meta;
    }

    private static JSONObject getJson(RestClient rc, String url) {
        try {
            final JSONObject headers = new JSONObject();
            headers.put("Accept", "application/json");
            return rc.jsonCall("GET", url, (String) null, headers);
        } catch (IOException e) {
            logger.debug("OAuth discovery GET failed for " + url + ": " + e.getMessage());
            return null;
        }
    }

    private static RestClient newClient() {
        return new RestClient().setTimeouts(Duration.ofSeconds(15), Duration.ofSeconds(15));
    }

    /**
     * Return the {@code scheme://host[:port]} origin of a URL.
     *
     * @param url the URL
     * @return the origin
     * @throws OAuthClientException if the URL is malformed
     */
    static String origin(String url) {
        try {
            final URI u = new URI(url);
            if (u.getScheme() == null || u.getHost() == null)
                throw new OAuthClientException("Provider Url is not an absolute URL: " + url);
            final StringBuilder sb = new StringBuilder();
            sb.append(u.getScheme()).append("://").append(u.getHost());
            if (u.getPort() != -1)
                sb.append(':').append(u.getPort());
            return sb.toString();
        } catch (URISyntaxException e) {
            throw new OAuthClientException("Provider Url is malformed: " + url, e);
        }
    }

    private static String trimTrailingSlash(String s) {
        while (s.endsWith("/"))
            s = s.substring(0, s.length() - 1);
        return s;
    }
}
