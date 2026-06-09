package org.kissweb.oauth;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.LRUCache;
import org.kissweb.json.JSONObject;

import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.PublicKey;
import java.security.Signature;
import java.security.SignatureException;
import java.util.Base64;
import java.util.HexFormat;
import java.util.List;

/**
 * Validates OAuth 2.1 bearer tokens (JWTs) presented in the
 * <code>Authorization: Bearer ...</code> header against the JWKS published
 * by the configured authorization server, and surfaces the validated
 * subject and scopes to the rest of the request.
 * <br><br>
 * <h2>Typical use</h2>
 * From an {@link org.kissweb.MCPServerBase} subclass:
 * <pre>
 * &#64;Override
 * protected boolean authenticate(HttpServletRequest req, HttpServletResponse resp) throws IOException {
 *     return BearerTokenValidator.requireBearerToken(req, resp) != null;
 * }
 * </pre>
 * Tool methods can then call {@link #currentToken()} to read the subject
 * and scopes that were validated for this request.
 * <br><br>
 * <h2>What is validated</h2>
 * <ul>
 *   <li>JWS signature using the JWKS key whose {@code kid} matches the JWT header</li>
 *   <li>{@code alg} is in {@code OAuthAllowedAlgorithms} (default: RS256)</li>
 *   <li>{@code iss} equals the configured authorization-server URL</li>
 *   <li>{@code aud} contains the configured resource identifier</li>
 *   <li>{@code exp} is in the future (within configured clock skew)</li>
 *   <li>{@code nbf}, if present, is not in the future (within clock skew)</li>
 *   <li>Every scope listed in {@code OAuthRequiredScopes} is present in
 *       the token's {@code scope} or {@code scp} claim</li>
 * </ul>
 * Currently supports RSA signing keys (RS256).  Other algorithms can be
 * added by extending the algorithm dispatch in {@link #verifySignature}.
 * <br><br>
 * <h2>Caching</h2>
 * To avoid re-validating the same token on every JSON-RPC call from the
 * same MCP session, validated tokens are cached for 60 seconds keyed by
 * their SHA-256 hash.  The cache is small and bounded.
 */
public final class BearerTokenValidator {

    private static final Logger logger = LogManager.getLogger(BearerTokenValidator.class);

    private static final ThreadLocal<ValidatedToken> CURRENT = new ThreadLocal<>();

    /** Brief positive-result cache so a chatty MCP session doesn't re-verify the same JWT on every call. */
    private static final LRUCache<String, ValidatedToken> TOKEN_CACHE = new LRUCache<>(1024, 60);

    private BearerTokenValidator() {
    }

    /**
     * Validate the bearer token on the request.  On success the token is
     * placed in a thread-local so {@link #currentToken()} can return it
     * during the rest of the request, and it is also returned to the
     * caller.  On failure a {@code 401 Unauthorized} (or
     * {@code 403 Forbidden} for an insufficient-scope token) is written
     * to the response, complete with the RFC 6750 / RFC 9728
     * {@code WWW-Authenticate} challenge, and the method returns null.
     *
     * @param request the incoming request
     * @param response the response to write a challenge to on failure
     * @return the validated token, or null if validation failed (in
     *         which case a response has already been written)
     * @throws IOException if writing the response fails
     */
    public static ValidatedToken requireBearerToken(HttpServletRequest request,
                                                    HttpServletResponse response) throws IOException {
        final OAuthConfig cfg = OAuthConfig.get();
        if (!cfg.isEnabled()) {
            writeChallenge(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                    null, "OAuth resource-server is not configured (set OAuthAuthorizationServer in application.ini)");
            return null;
        }

        final String header = request.getHeader("Authorization");
        if (header == null || !header.regionMatches(true, 0, "Bearer ", 0, 7)) {
            // RFC 6750 §3.1: when no credentials are presented, omit
            // error parameters --- just emit the bare challenge.
            writeChallenge(response, HttpServletResponse.SC_UNAUTHORIZED, null, null);
            return null;
        }
        final String token = header.substring(7).trim();
        if (token.isEmpty()) {
            writeChallenge(response, HttpServletResponse.SC_UNAUTHORIZED,
                    "invalid_token", "Empty bearer token");
            return null;
        }

        try {
            final ValidatedToken validated = validate(token, cfg);
            CURRENT.set(validated);
            return validated;
        } catch (InsufficientScopeException e) {
            writeChallenge(response, HttpServletResponse.SC_FORBIDDEN,
                    "insufficient_scope", e.getMessage());
            return null;
        } catch (TokenValidationException e) {
            logger.debug("Bearer token rejected: " + e.getMessage());
            writeChallenge(response, HttpServletResponse.SC_UNAUTHORIZED,
                    "invalid_token", e.getMessage());
            return null;
        } catch (IOException e) {
            // JWKS fetch failed --- this is a server problem, not the
            // client's fault, but for the client a 401 keeps us from
            // leaking infrastructure details.
            logger.error("JWKS fetch failed during token validation", e);
            writeChallenge(response, HttpServletResponse.SC_UNAUTHORIZED,
                    "invalid_token", "Unable to verify token");
            return null;
        }
    }

    /**
     * Validate a token string directly.  Use this from non-MCP contexts
     * where you need access to the validated claims without the 401
     * response-writing machinery.  Throws on any validation failure.
     *
     * @param token the raw JWT (no {@code Bearer } prefix)
     * @return the validated token
     * @throws TokenValidationException if the token is malformed,
     *         expired, signed by an unknown key, or fails claim checks
     * @throws InsufficientScopeException if signature and claims are
     *         valid but the token lacks one or more required scopes
     * @throws IOException if the JWKS fetch fails
     */
    public static ValidatedToken validate(String token) throws TokenValidationException, InsufficientScopeException, IOException {
        return validate(token, OAuthConfig.get());
    }

    /**
     * Get the token validated earlier in this request --- intended to
     * be called from tool methods after {@link #requireBearerToken} has
     * already succeeded.
     *
     * @return the validated token, or null if none was validated (the
     *         caller is not inside an authenticated request, or the
     *         auth path was not taken)
     */
    public static ValidatedToken currentToken() {
        return CURRENT.get();
    }

    /**
     * Clear the thread-local set by {@link #requireBearerToken}.  Called
     * automatically by {@link org.kissweb.MCPServerBase} after each
     * request; other servlets that use {@link #requireBearerToken}
     * directly should invoke this in a {@code finally} block to prevent
     * the validated token from leaking onto the next request scheduled
     * on the same servlet-container thread.
     */
    public static void clearCurrentToken() {
        CURRENT.remove();
    }

    // ==================================================================
    // Internal: validation pipeline
    // ==================================================================

    private static ValidatedToken validate(String token, OAuthConfig cfg)
            throws TokenValidationException, InsufficientScopeException, IOException {

        final String cacheKey = sha256Hex(token);
        ValidatedToken cached = TOKEN_CACHE.get(cacheKey);
        if (cached != null) {
            // Re-check expiry from the cached claims; the cache TTL is
            // short but tokens can still expire within it.
            checkTimeClaims(cached.getClaims(), cfg);
            checkScopes(cached.getScopes(), cfg);
            return cached;
        }

        final String[] parts = token.split("\\.", -1);
        if (parts.length != 3)
            throw new TokenValidationException("Malformed JWT: expected three dot-separated segments");

        final JSONObject header  = decodeSegment(parts[0], "header");
        final JSONObject payload = decodeSegment(parts[1], "payload");
        final byte[] signature   = decodeSignature(parts[2]);

        final String alg = header.getString("alg", null);
        if (alg == null || !cfg.getAllowedAlgorithms().contains(alg))
            throw new TokenValidationException("JWT alg '" + alg + "' is not in OAuthAllowedAlgorithms");

        final String kid = header.getString("kid", null);
        if (kid == null || kid.isEmpty())
            throw new TokenValidationException("JWT header is missing 'kid'");

        final PublicKey key = JwksCache.get().getKey(kid);
        if (key == null)
            throw new TokenValidationException("No JWKS key found for kid=" + kid);

        final byte[] signingInput = (parts[0] + "." + parts[1]).getBytes(StandardCharsets.US_ASCII);
        if (!verifySignature(alg, key, signingInput, signature))
            throw new TokenValidationException("JWT signature verification failed");

        checkIssuer(payload, cfg);
        checkAudience(payload, cfg);
        checkTimeClaims(payload, cfg);

        final ValidatedToken result = new ValidatedToken(payload);
        checkScopes(result.getScopes(), cfg);

        TOKEN_CACHE.add(cacheKey, result);
        return result;
    }

    private static JSONObject decodeSegment(String segment, String what) throws TokenValidationException {
        try {
            final byte[] bytes = Base64.getUrlDecoder().decode(segment);
            return new JSONObject(new String(bytes, StandardCharsets.UTF_8));
        } catch (IllegalArgumentException e) {
            throw new TokenValidationException("JWT " + what + " is not valid base64url");
        } catch (RuntimeException e) {
            throw new TokenValidationException("JWT " + what + " is not valid JSON: " + e.getMessage());
        }
    }

    private static byte[] decodeSignature(String segment) throws TokenValidationException {
        try {
            return Base64.getUrlDecoder().decode(segment);
        } catch (IllegalArgumentException e) {
            throw new TokenValidationException("JWT signature is not valid base64url");
        }
    }

    private static boolean verifySignature(String alg, PublicKey key, byte[] signingInput, byte[] signature)
            throws TokenValidationException {
        final String javaAlg;
        switch (alg) {
            case "RS256": javaAlg = "SHA256withRSA"; break;
            case "RS384": javaAlg = "SHA384withRSA"; break;
            case "RS512": javaAlg = "SHA512withRSA"; break;
            default:
                throw new TokenValidationException("Unsupported JWS algorithm: " + alg);
        }
        try {
            final Signature s = Signature.getInstance(javaAlg);
            s.initVerify(key);
            s.update(signingInput);
            return s.verify(signature);
        } catch (NoSuchAlgorithmException | SignatureException | java.security.InvalidKeyException e) {
            throw new TokenValidationException("Signature verification error: " + e.getMessage());
        }
    }

    private static void checkIssuer(JSONObject payload, OAuthConfig cfg) throws TokenValidationException {
        final String iss = payload.getString("iss", null);
        if (iss == null)
            throw new TokenValidationException("Token is missing 'iss' claim");
        final String expected = cfg.getAuthorizationServer();
        if (!iss.equals(expected) && !trimTrailingSlash(iss).equals(expected))
            throw new TokenValidationException("Token iss '" + iss + "' does not match expected '" + expected + "'");
    }

    private static void checkAudience(JSONObject payload, OAuthConfig cfg) throws TokenValidationException {
        final ValidatedToken probe = new ValidatedToken(payload);
        // Compare audiences with a trailing slash trimmed on both sides, the
        // same normalization checkIssuer() applies.  RFC 3986 canonicalizes a
        // URI whose authority has an empty path to a single "/" path, so a
        // client that treats the resource indicator as a URI (e.g. it
        // discovers "https://host" from the protected-resource metadata and
        // normalizes it) will present aud="https://host/", while a client
        // that sends the value verbatim presents aud="https://host".  Both
        // denote the same resource; an exact match would reject one of them.
        final String expected = trimTrailingSlash(cfg.getResourceIdentifier());
        for (String aud : probe.getAudience()) {
            if (java.util.Objects.equals(expected, trimTrailingSlash(aud)))
                return;
        }
        throw new TokenValidationException("Token aud does not include resource identifier '"
                + cfg.getResourceIdentifier() + "'");
    }

    private static void checkTimeClaims(JSONObject payload, OAuthConfig cfg) throws TokenValidationException {
        final long now = System.currentTimeMillis() / 1000L;
        final int skew = cfg.getClockSkewSeconds();

        final long exp = payload.getLong("exp", 0L);
        if (exp <= 0L)
            throw new TokenValidationException("Token is missing 'exp' claim");
        if (now - skew > exp)
            throw new TokenValidationException("Token has expired");

        if (payload.has("nbf")) {
            final long nbf = payload.getLong("nbf", 0L);
            if (nbf > now + skew)
                throw new TokenValidationException("Token is not yet valid (nbf in the future)");
        }
    }

    private static void checkScopes(java.util.Set<String> tokenScopes, OAuthConfig cfg) throws InsufficientScopeException {
        final List<String> required = cfg.getRequiredScopes();
        if (required.isEmpty())
            return;
        for (String s : required) {
            if (!tokenScopes.contains(s))
                throw new InsufficientScopeException("Token is missing required scope: " + s);
        }
    }

    // ==================================================================
    // Internal: response writing
    // ==================================================================

    private static void writeChallenge(HttpServletResponse response, int status, String error, String description)
            throws IOException {
        final OAuthConfig cfg = OAuthConfig.get();
        final StringBuilder header = new StringBuilder("Bearer");
        boolean first = true;
        if (cfg.isEnabled()) {
            header.append(first ? " " : ", ");
            first = false;
            header.append("realm=\"").append(quote(cfg.getResourceIdentifier())).append("\"");
        }
        if (error != null) {
            header.append(first ? " " : ", ");
            first = false;
            header.append("error=\"").append(quote(error)).append("\"");
        }
        if (description != null) {
            header.append(first ? " " : ", ");
            first = false;
            header.append("error_description=\"").append(quote(description)).append("\"");
        }
        if (cfg.isEnabled()) {
            header.append(first ? " " : ", ");
            header.append("resource_metadata=\"")
                  .append(quote(cfg.getResourceIdentifier() + "/.well-known/oauth-protected-resource"))
                  .append("\"");
        }
        response.setStatus(status);
        response.setHeader("WWW-Authenticate", header.toString());
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        try (PrintWriter out = response.getWriter()) {
            final JSONObject body = new JSONObject();
            if (error != null)
                body.put("error", error);
            if (description != null)
                body.put("error_description", description);
            out.print(body.toString());
        }
    }

    private static String quote(String s) {
        return s == null ? "" : s.replace("\\", "\\\\").replace("\"", "\\\"");
    }

    private static String trimTrailingSlash(String s) {
        if (s == null)
            return null;
        while (s.endsWith("/"))
            s = s.substring(0, s.length() - 1);
        return s;
    }

    private static String sha256Hex(String s) {
        try {
            final MessageDigest md = MessageDigest.getInstance("SHA-256");
            final byte[] digest = md.digest(s.getBytes(StandardCharsets.UTF_8));
            return HexFormat.of().formatHex(digest);
        } catch (NoSuchAlgorithmException e) {
            // SHA-256 is required by every JRE
            throw new IllegalStateException(e);
        }
    }

    // ==================================================================
    // Exceptions
    // ==================================================================

    /** Thrown when a token is malformed, has a bad signature, or fails a claim check. */
    public static class TokenValidationException extends Exception {
        /**
         * Construct the exception.
         *
         * @param message human-readable diagnostic
         */
        public TokenValidationException(String message) {
            super(message);
        }
    }

    /** Thrown when a token is otherwise valid but lacks a required scope. */
    public static class InsufficientScopeException extends Exception {
        /**
         * Construct the exception.
         *
         * @param message human-readable diagnostic
         */
        public InsufficientScopeException(String message) {
            super(message);
        }
    }
}
