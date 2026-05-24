/**
 * OAuth 2.1 authorization-server implementation.
 * <br><br>
 * This package issues tokens: it implements the authorization endpoint
 * (auth code flow with PKCE), the token endpoint (code exchange and
 * refresh-token rotation), dynamic client registration (RFC 7591), the
 * RFC 8414 metadata endpoint, and the JWKS publishing endpoint.
 * <br><br>
 * The companion package {@link org.kissweb.oauth} is the resource-server
 * side that validates tokens issued here.  An MCP server built on Kiss
 * typically runs both --- the AS issues access tokens to MCP clients
 * after a user logs in and grants consent, and the RS validates those
 * tokens on incoming MCP method calls.
 * <br><br>
 * <h2>Persistence</h2>
 * All long-lived state (signing keys, registered clients, refresh
 * tokens) is persisted to a single INI file.  The default location is
 * <code>oauth.ini</code> in the application root (the deployed
 * <code>WEB-INF/backend/</code> directory, or its dev-mode equivalent
 * <code>src/main/backend/</code>).  An application that wants the file
 * to survive WAR redeploys can set <code>OAuthAsIniFile</code> to an
 * absolute path outside the webapp tree --- see
 * {@link AuthorizationServerConfig}.  Auth codes live only in memory
 * --- their TTL is 60 seconds and the cost of losing them on restart
 * is one user re-clicking ``Allow.''  Saves are atomic (write to
 * <code>oauth.ini.tmp</code>, then rename).
 * <br><br>
 * <h2>Two app-provided interfaces</h2>
 * Everything in this package is generic, but two pieces are necessarily
 * application-specific:
 * <ul>
 *   <li>{@link UserAuthenticator} --- verifies a username/password and
 *       returns the authenticated user's subject identifier.
 *       Implementations live under <code>src/main/backend/</code> or
 *       <code>src/main/precompiled/</code> and typically wrap the
 *       application's existing user store (Kiss's <code>Login.groovy</code>,
 *       LDAP, an external IdP, etc.).</li>
 *   <li>{@link ConsentProvider} --- supplies human-readable descriptions
 *       of scopes so the consent page can show what the requesting
 *       client is asking for.  A default implementation returns the
 *       scope name unchanged; apps typically override it.</li>
 * </ul>
 * Both are registered at startup via {@link AsExtensions}.
 * <br><br>
 * <h2>Configuration</h2>
 * See {@link AuthorizationServerConfig} for the recognized
 * {@code application.ini} keys.
 */
package org.kissweb.oauth.as;
