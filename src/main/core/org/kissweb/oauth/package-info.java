/**
 * OAuth 2.1 resource-server support for Kiss applications.
 * <br><br>
 * This package validates incoming bearer tokens against an external OAuth 2.1
 * authorization server (Auth0, Okta, Keycloak, Google, etc.) and exposes the
 * RFC 9728 protected-resource discovery endpoint that MCP clients and other
 * OAuth-aware clients use to bootstrap authentication.
 * <br><br>
 * Roles implemented: <em>resource server</em> only.  Token issuance, dynamic
 * client registration, and consent UI (the authorization-server role) are
 * out of scope --- delegate those to a dedicated authorization server.
 * <br><br>
 * <h2>Configuration</h2>
 * All settings live in {@code application.ini} and are read via
 * {@link org.kissweb.restServer.MainServlet#getEnvironment(String)}:
 * <pre>
 * OAuthAuthorizationServer = https://auth.example.com
 * OAuthResourceIdentifier  = https://mcp.myapp.com
 * OAuthRequiredScopes      = mcp:read,mcp:write
 * OAuthJwksUri             = https://auth.example.com/.well-known/jwks.json   # optional
 * OAuthJwksCacheSeconds    = 3600                                              # optional
 * OAuthAllowedAlgorithms   = RS256                                             # optional
 * OAuthClockSkewSeconds    = 60                                                # optional
 * </pre>
 * If {@code OAuthAuthorizationServer} is not set, the package is inert and
 * the discovery endpoint returns 404 --- applications that don't use OAuth
 * pay no runtime cost.
 * <br><br>
 * <h2>Usage from an MCP server</h2>
 * No code change is required.  As soon as {@code OAuthAuthorizationServer} is
 * set in {@code application.ini}, every {@link org.kissweb.MCPServerBase}
 * subclass automatically validates the {@code Authorization: Bearer ...} header
 * on each incoming request and rejects requests that lack a valid token with
 * the appropriate 401 / 403 challenge.
 * <br><br>
 * Tool methods can read the validated token:
 * <pre>
 * ValidatedToken tok = BearerTokenValidator.currentToken();
 * String userId = tok.getSubject();
 * if (!tok.hasScope("mcp:write"))
 *     return toolError("This tool requires the mcp:write scope.");
 * </pre>
 * For non-MCP servlets (or to layer extra checks on top of the OAuth default),
 * call {@link BearerTokenValidator#requireBearerToken} explicitly from an
 * {@code authenticate} override or a request filter.
 * <h2>Discovery endpoint</h2>
 * {@link ProtectedResourceMetadataServlet} is auto-registered at
 * {@code /.well-known/oauth-protected-resource} via {@code @WebServlet}
 * annotation scanning and serves the RFC 9728 metadata document.
 */
package org.kissweb.oauth;
