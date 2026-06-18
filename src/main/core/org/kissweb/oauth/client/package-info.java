/**
 * OAuth 2.1 <em>client</em> (relying-party) support for Kiss applications.
 * <br><br>
 * This package lets a Kiss application act as a client of a remote,
 * OAuth 2.1-protected server: it obtains access tokens via the
 * authorization-code + PKCE flow, refreshes them, and makes them
 * available for outbound calls (typically as an
 * {@code Authorization: Bearer ...} header).  It is the third OAuth role
 * in {@code org.kissweb.oauth}, complementing the existing
 * <em>resource-server</em> role (top-level package, validates incoming
 * tokens) and <em>authorization-server</em> role
 * ({@code org.kissweb.oauth.as}, issues tokens).
 * <br><br>
 * The package is generic and application-neutral; it has no knowledge of
 * what the remote server does (MCP, a plain REST API, anything else).
 * <br><br>
 * <h2>Inertness</h2>
 * The package does nothing unless at least one provider is configured.
 * With no {@code [OAuthClient *]} section in {@code application.ini},
 * {@link org.kissweb.oauth.client.OAuthClientConfig#isEnabled()} is false,
 * no SQLite state file is created, the
 * {@link org.kissweb.oauth.client.OAuthCallbackServlet} returns
 * {@code 404}, and no code path reaches the network --- exactly the
 * "applications that don't use OAuth pay no runtime cost" guarantee the
 * resource-server package makes.
 * <br><br>
 * <h2>Configuration</h2>
 * One section per remote provider, read directly from the ini file (the
 * framework's flat environment only carries {@code [main]}):
 * <pre>
 * [OAuthClient ownsona]
 * Url          = https://example.com/mcp   ; remote resource/server URL (required)
 * Scopes       = mcp:read mcp:write        ; space- or comma-separated (optional)
 * ClientId     =                           ; blank =&gt; Dynamic Client Registration
 * ClientSecret =                           ; only if pre-registered + confidential
 * </pre>
 * Optional {@code [main]} keys: {@code OAuthClientRedirectBaseUrl} (base
 * for the {@code redirect_uri}; otherwise derived from the request) and
 * {@code OAuthClientRefreshSkewSeconds} (default 60).  See
 * {@link org.kissweb.oauth.client.OAuthClientConfig} for the full list.
 * <br><br>
 * Cached registrations and tokens are persisted in the same SQLite
 * database as the authorization-server side ({@code oauth.db} by default,
 * configured via {@code OAuthAsSqliteFile}) --- there is no separate
 * client database.
 * <br><br>
 * <h2>Usage</h2>
 * <pre>
 * OAuthClient client = OAuthClient.forProvider("ownsona");
 * if (!client.isAuthorized()) {
 *     // Send the browser to this URL to log in / consent:
 *     String authorizeUrl = client.beginAuthorization("http://localhost:8080");
 *     ...
 * }
 * // Later, on every outbound call:
 * String token = client.getAccessToken();   // discovers, registers, and refreshes as needed
 * </pre>
 * The browser returns to {@link org.kissweb.oauth.client.OAuthCallbackServlet}
 * at {@code /oauth/client/callback}, which exchanges the code for tokens.
 * When no usable token can be produced without a fresh login,
 * {@link org.kissweb.oauth.client.OAuthClient#getAccessToken()} throws
 * {@link org.kissweb.oauth.client.OAuthAuthorizationRequiredException}.
 * <br><br>
 * <h2>What it does for you</h2>
 * <ul>
 *   <li>Authorization-server discovery via the RFC 9728 protected-resource
 *       document then RFC 8414 / OIDC metadata
 *       ({@link org.kissweb.oauth.client.OAuthMetadataDiscovery}), cached.</li>
 *   <li>RFC 7591 Dynamic Client Registration when no {@code ClientId} is
 *       configured ({@link org.kissweb.oauth.client.DynamicClientRegistration}),
 *       cached.</li>
 *   <li>PKCE (S256) generation
 *       ({@link org.kissweb.oauth.client.Pkce}).</li>
 *   <li>Access-token refresh with refresh-token rotation and single-flight
 *       per provider.</li>
 * </ul>
 */
package org.kissweb.oauth.client;
