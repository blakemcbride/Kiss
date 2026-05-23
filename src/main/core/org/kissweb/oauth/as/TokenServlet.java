package org.kissweb.oauth.as;

import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.json.JSONObject;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.util.Base64;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;
import java.util.UUID;

/**
 * The OAuth 2.1 token endpoint at {@code /oauth/token}.  Exchanges
 * authorization codes for access tokens and rotates refresh tokens.
 * <br><br>
 * <h2>Supported grants</h2>
 * <ul>
 *   <li>{@code grant_type=authorization_code} --- the client redeems the
 *       code it received from {@code /authorize} for an access token
 *       (and, by default, a refresh token).  Validates PKCE, ties the
 *       request to the redirect_uri originally supplied, and consumes
 *       the code so it cannot be reused.</li>
 *   <li>{@code grant_type=refresh_token} --- the client exchanges a
 *       valid refresh token for a fresh access token \emph{and} a new
 *       refresh token (rotation).  If a rotated (already-consumed)
 *       refresh token is replayed, the framework interprets that as
 *       theft and {@link RefreshTokenStore#revokeFamily revokes the
 *       entire token family}, forcing the legitimate user to log in
 *       again.</li>
 * </ul>
 * <h2>Client authentication</h2>
 * Public clients (no secret) authenticate via PKCE alone.  Confidential
 * clients send their {@code client_secret} either in the HTTP Basic
 * authorization header ({@code client_secret_basic}) or in the body
 * ({@code client_secret_post}); both are accepted.
 * <br><br>
 * <h2>Response shape</h2>
 * Per RFC 6749 \S{}5.1:
 * <pre>
 * {
 *   "access_token":  "...",
 *   "token_type":    "Bearer",
 *   "expires_in":    3600,
 *   "refresh_token": "...",
 *   "scope":         "mcp:read mcp:write"
 * }
 * </pre>
 * All token responses include {@code Cache-Control: no-store}.
 */
@WebServlet(urlPatterns = "/oauth/token")
public class TokenServlet extends HttpServlet {

    private static final Logger logger = LogManager.getLogger(TokenServlet.class);

    /** Default constructor for servlet container instantiation. */
    public TokenServlet() { }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        AuthorizationServlet.prune();   // opportunistic prune (package-private)
        if (!AuthorizationServerConfig.get().isEnabled()) {
            response.sendError(HttpServletResponse.SC_NOT_FOUND);
            return;
        }

        final String grantType = nullToEmpty(request.getParameter("grant_type"));
        switch (grantType) {
            case "authorization_code":
                handleAuthorizationCode(request, response);
                return;
            case "refresh_token":
                handleRefreshToken(request, response);
                return;
            case "":
                error(response, 400, "invalid_request", "grant_type is required");
                return;
            default:
                error(response, 400, "unsupported_grant_type", "Unsupported grant_type: " + grantType);
        }
    }

    // ==================================================================
    // authorization_code grant
    // ==================================================================

    private void handleAuthorizationCode(HttpServletRequest request,
                                         HttpServletResponse response) throws IOException {
        final ClientAuth auth = authenticateClient(request);
        if (auth.error != null) {
            error(response, 401, "invalid_client", auth.error);
            return;
        }
        final RegisteredClient client = auth.client;

        final String codeValue   = request.getParameter("code");
        final String redirectUri = request.getParameter("redirect_uri");
        final String verifier    = request.getParameter("code_verifier");

        if (codeValue == null || codeValue.isEmpty()) {
            error(response, 400, "invalid_request", "code is required");
            return;
        }
        final AuthorizationCode authCode = AuthorizationCodeStore.get().consume(codeValue);
        if (authCode == null) {
            error(response, 400, "invalid_grant", "Authorization code is invalid or expired");
            return;
        }
        if (!client.getClientId().equals(authCode.getClientId())) {
            error(response, 400, "invalid_grant", "Authorization code was issued to a different client");
            return;
        }
        if (redirectUri == null || !redirectUri.equals(authCode.getRedirectUri())) {
            error(response, 400, "invalid_grant", "redirect_uri does not match the value supplied at /authorize");
            return;
        }
        if (verifier == null || verifier.isEmpty()) {
            error(response, 400, "invalid_request", "code_verifier is required (PKCE)");
            return;
        }
        if (!PkceValidator.verify(authCode.getCodeChallengeMethod(), verifier, authCode.getCodeChallenge())) {
            error(response, 400, "invalid_grant", "PKCE code_verifier does not match code_challenge");
            return;
        }

        // All checks passed --- mint tokens.
        issueTokens(response, client, authCode.getUser(), authCode.getScopes(),
                authCode.getAudience(), /*familyId=*/UUID.randomUUID().toString());
    }

    // ==================================================================
    // refresh_token grant
    // ==================================================================

    private void handleRefreshToken(HttpServletRequest request,
                                    HttpServletResponse response) throws IOException {
        final ClientAuth auth = authenticateClient(request);
        if (auth.error != null) {
            error(response, 401, "invalid_client", auth.error);
            return;
        }
        final RegisteredClient client = auth.client;

        final String refreshValue = request.getParameter("refresh_token");
        if (refreshValue == null || refreshValue.isEmpty()) {
            error(response, 400, "invalid_request", "refresh_token is required");
            return;
        }
        final RefreshTokenStore store = RefreshTokenStore.get();
        final RefreshToken existing = store.get(refreshValue);
        if (existing == null) {
            error(response, 400, "invalid_grant", "Refresh token is unknown");
            return;
        }
        if (existing.isExpired()) {
            error(response, 400, "invalid_grant", "Refresh token has expired");
            return;
        }
        if (existing.isRotated()) {
            // The legitimate client should never see a rotated token again.
            // Treat this as evidence of theft: revoke the entire family and
            // force re-authentication.
            try {
                store.revokeFamily(existing.getFamilyId());
            } catch (IOException e) {
                logger.error("Failed to revoke refresh-token family on reuse detection", e);
            }
            error(response, 400, "invalid_grant", "Refresh token has been rotated; family revoked");
            return;
        }
        if (!client.getClientId().equals(existing.getClientId())) {
            error(response, 400, "invalid_grant", "Refresh token was issued to a different client");
            return;
        }

        // Optional scope narrowing.
        Set<String> scopes = existing.getScopes();
        final String requestedScope = request.getParameter("scope");
        if (requestedScope != null && !requestedScope.isEmpty()) {
            final Set<String> requested = parseScopes(requestedScope);
            for (String s : requested) {
                if (!existing.getScopes().contains(s)) {
                    error(response, 400, "invalid_scope",
                            "Requested scope '" + s + "' was not in the original grant");
                    return;
                }
            }
            scopes = requested;
        }

        // Optional audience override (RFC 8707).  Must equal what was originally requested.
        String audience = existing.getAudience();
        final String requestedAudience = request.getParameter("resource");
        if (requestedAudience != null && !requestedAudience.isEmpty()) {
            if (audience != null && !audience.equals(requestedAudience)) {
                error(response, 400, "invalid_target",
                        "Requested resource does not match the original token's audience");
                return;
            }
            audience = requestedAudience;
        }

        // Mint a new access token + new refresh token, rotating the old one.
        final long now = System.currentTimeMillis() / 1000L;
        final String newRefreshValue = TokenIssuer.issueRefreshTokenValue();
        final RefreshToken successor = new RefreshToken(
                newRefreshValue, existing.getFamilyId(), client.getClientId(),
                existing.getUserSubject(), existing.getUserExtraClaims(),
                scopes, audience, now,
                now + AuthorizationServerConfig.get().getRefreshTokenTtlSeconds(),
                null);
        try {
            store.rotate(existing.getJti(), successor);
        } catch (IOException e) {
            logger.error("Failed to rotate refresh token", e);
            error(response, 500, "server_error", "Could not persist refresh token rotation");
            return;
        }

        final String accessToken = TokenIssuer.issueAccessToken(existing.getUser(),
                client.getClientId(), scopes, audience);
        writeSuccess(response, accessToken, newRefreshValue, scopes);
    }

    // ==================================================================
    // Token-response writing
    // ==================================================================

    private static void issueTokens(HttpServletResponse response,
                                    RegisteredClient client,
                                    AuthenticatedUser user,
                                    Set<String> scopes,
                                    String audience,
                                    String familyId) throws IOException {
        final long now = System.currentTimeMillis() / 1000L;
        final boolean offerRefresh = client.getAllowedGrantTypes().contains("refresh_token");

        final String refreshValue;
        if (offerRefresh) {
            refreshValue = TokenIssuer.issueRefreshTokenValue();
            final RefreshToken rt = new RefreshToken(
                    refreshValue, familyId, client.getClientId(), user.getSubject(),
                    user.getExtraClaims(), scopes, audience, now,
                    now + AuthorizationServerConfig.get().getRefreshTokenTtlSeconds(),
                    null);
            try {
                RefreshTokenStore.get().store(rt);
            } catch (IOException e) {
                logger.error("Failed to persist refresh token", e);
                error(response, 500, "server_error", "Could not persist refresh token");
                return;
            }
        } else {
            refreshValue = null;
        }

        final String accessToken = TokenIssuer.issueAccessToken(user, client.getClientId(), scopes, audience);
        writeSuccess(response, accessToken, refreshValue, scopes);
    }

    private static void writeSuccess(HttpServletResponse response,
                                     String accessToken,
                                     String refreshTokenOrNull,
                                     Set<String> scopes) throws IOException {
        final JSONObject body = new JSONObject();
        body.put("access_token", accessToken);
        body.put("token_type",   "Bearer");
        body.put("expires_in",   AuthorizationServerConfig.get().getAccessTokenTtlSeconds());
        if (refreshTokenOrNull != null)
            body.put("refresh_token", refreshTokenOrNull);
        if (!scopes.isEmpty())
            body.put("scope", String.join(" ", scopes));

        response.setStatus(HttpServletResponse.SC_OK);
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        response.setHeader("Cache-Control", "no-store");
        response.setHeader("Pragma", "no-cache");
        try (PrintWriter w = response.getWriter()) {
            w.print(body.toString(2));
        }
    }

    private static void error(HttpServletResponse response, int status, String code, String description)
            throws IOException {
        response.setStatus(status);
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        response.setHeader("Cache-Control", "no-store");
        response.setHeader("Pragma", "no-cache");
        final JSONObject err = new JSONObject();
        err.put("error", code);
        if (description != null)
            err.put("error_description", description);
        try (PrintWriter w = response.getWriter()) {
            w.print(err.toString());
        }
    }

    // ==================================================================
    // Client authentication
    // ==================================================================

    /** Resolved client + optional error string from {@link #authenticateClient}. */
    private static final class ClientAuth {
        final RegisteredClient client;
        final String           error;
        ClientAuth(RegisteredClient client, String error) {
            this.client = client;
            this.error  = error;
        }
    }

    /**
     * Resolve and authenticate the client making this request.  Accepts
     * credentials via HTTP Basic auth header
     * ({@code client_secret_basic}) or form body
     * ({@code client_secret_post}).  Public clients (no secret) need
     * only supply {@code client_id}; their authentication is PKCE.
     */
    private static ClientAuth authenticateClient(HttpServletRequest request) {
        String clientId     = null;
        String clientSecret = null;

        // 1. HTTP Basic header has precedence per RFC 6749 §2.3.1.
        final String authHeader = request.getHeader("Authorization");
        if (authHeader != null && authHeader.regionMatches(true, 0, "Basic ", 0, 6)) {
            try {
                final String decoded = new String(
                        Base64.getDecoder().decode(authHeader.substring(6).trim()),
                        StandardCharsets.UTF_8);
                final int colon = decoded.indexOf(':');
                if (colon < 0)
                    return new ClientAuth(null, "Malformed Basic credentials");
                clientId     = decoded.substring(0, colon);
                clientSecret = decoded.substring(colon + 1);
            } catch (IllegalArgumentException e) {
                return new ClientAuth(null, "Basic credentials are not valid base64");
            }
        } else {
            // 2. Otherwise read from the form body.
            clientId     = request.getParameter("client_id");
            clientSecret = request.getParameter("client_secret");
        }

        if (clientId == null || clientId.isEmpty())
            return new ClientAuth(null, "client_id is required");
        final RegisteredClient client = ClientStore.get().get(clientId);
        if (client == null)
            return new ClientAuth(null, "Unknown client: " + clientId);

        if (client.isPublicClient()) {
            if (clientSecret != null && !clientSecret.isEmpty())
                return new ClientAuth(null, "Public client must not send a client_secret");
            return new ClientAuth(client, null);
        }

        // Confidential client --- verify the secret.
        if (clientSecret == null || clientSecret.isEmpty())
            return new ClientAuth(null, "client_secret is required for confidential clients");
        final String expectedHash = client.getClientSecretHash();
        if (expectedHash == null)
            return new ClientAuth(null, "Client is missing stored secret hash");
        if (!constantTimeEquals(expectedHash, "sha256:" + sha256Hex(clientSecret)))
            return new ClientAuth(null, "Invalid client_secret");
        return new ClientAuth(client, null);
    }

    // ==================================================================
    // Helpers
    // ==================================================================

    private static Set<String> parseScopes(String scope) {
        if (scope == null || scope.isEmpty())
            return Collections.emptySet();
        final Set<String> out = new LinkedHashSet<>();
        for (String s : scope.split("\\s+"))
            if (!s.isEmpty())
                out.add(s);
        return out;
    }

    private static String nullToEmpty(String s) {
        return s == null ? "" : s;
    }

    private static String sha256Hex(String s) {
        try {
            final MessageDigest md = MessageDigest.getInstance("SHA-256");
            final byte[] digest = md.digest(s.getBytes(StandardCharsets.UTF_8));
            final StringBuilder sb = new StringBuilder(digest.length * 2);
            for (byte b : digest)
                sb.append(String.format("%02x", b));
            return sb.toString();
        } catch (NoSuchAlgorithmException e) {
            throw new IllegalStateException(e);
        }
    }

    private static boolean constantTimeEquals(String a, String b) {
        if (a.length() != b.length()) return false;
        int diff = 0;
        for (int i = 0; i < a.length(); i++)
            diff |= a.charAt(i) ^ b.charAt(i);
        return diff == 0;
    }
}
