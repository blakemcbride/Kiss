package org.kissweb.oauth.client;

import java.util.Collections;
import java.util.List;

/**
 * Immutable description of one remote OAuth 2.1-protected server this
 * application connects to as a client, parsed from an
 * {@code [OAuthClient <name>]} section of {@code application.ini} by
 * {@link OAuthClientConfig}.
 * <br><br>
 * The {@code name} is the operator-chosen label used both in code
 * ({@link OAuthClient#forProvider(String)}) and as the key under which
 * this provider's discovery document, client registration, and tokens
 * are persisted.
 */
public final class OAuthClientProvider {

    private final String       name;
    private final String       url;
    private final List<String> scopes;
    private final String       clientId;
    private final String       clientSecret;

    /**
     * Construct a provider description.
     *
     * @param name         the operator-chosen label (the part after
     *                     {@code OAuthClient } in the section header)
     * @param url          the remote resource/server URL (required)
     * @param scopes       the scopes to request; may be empty
     * @param clientId     a pre-registered client id, or null/empty to
     *                     use Dynamic Client Registration
     * @param clientSecret a pre-registered client secret for a
     *                     confidential client, or null/empty for a public
     *                     (PKCE-only) client
     */
    public OAuthClientProvider(String name, String url, List<String> scopes,
                               String clientId, String clientSecret) {
        this.name         = name;
        this.url          = url;
        this.scopes       = scopes == null ? Collections.emptyList()
                : Collections.unmodifiableList(scopes);
        this.clientId     = emptyToNull(clientId);
        this.clientSecret = emptyToNull(clientSecret);
    }

    /**
     * Get the provider name (label).
     * @return the provider name
     */
    public String getName() {
        return name;
    }

    /**
     * Get the remote resource/server URL.
     * @return the remote URL
     */
    public String getUrl() {
        return url;
    }

    /**
     * Get the requested scopes.
     * @return the scopes; empty list means none requested
     */
    public List<String> getScopes() {
        return scopes;
    }

    /**
     * Get the requested scopes as a single space-separated string.
     * @return the scope string, or empty string if none
     */
    public String getScopeString() {
        return String.join(" ", scopes);
    }

    /**
     * Get the pre-registered client id, if any.
     * @return the configured client id, or null to use Dynamic Client Registration
     */
    public String getClientId() {
        return clientId;
    }

    /**
     * Get the pre-registered client secret, if any.
     * @return the configured client secret, or null for a public client
     */
    public String getClientSecret() {
        return clientSecret;
    }

    /**
     * Whether this provider was configured with a pre-registered client id.
     * @return true if a client id was configured (so DCR is not needed)
     */
    public boolean hasConfiguredClient() {
        return clientId != null;
    }

    private static String emptyToNull(String s) {
        if (s == null)
            return null;
        s = s.trim();
        return s.isEmpty() ? null : s;
    }
}
