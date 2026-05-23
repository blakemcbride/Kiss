package org.kissweb.oauth.as;

import jakarta.servlet.ServletContext;
import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.Cookie;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.io.InputStream;
import java.io.PrintWriter;
import java.net.URI;
import java.net.URISyntaxException;
import java.net.URLEncoder;
import java.nio.charset.StandardCharsets;
import java.security.SecureRandom;
import java.util.Base64;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * The OAuth 2.1 authorization endpoint.  Handles the authorization-code
 * flow with mandatory PKCE.
 * <br><br>
 * <h2>Flow</h2>
 * <ol>
 *   <li>GET {@code /oauth/authorize?response_type=code&client_id=...} ---
 *       the AS validates the request, picks a flow id, then renders the
 *       login page (if the user has no session) or the consent page (if
 *       they do).</li>
 *   <li>POST {@code /oauth/authorize} with {@code stage=login&flow=...&username=...&password=...}
 *       --- the AS calls {@link UserAuthenticator}.  On success it sets a
 *       session cookie and renders the consent page; on failure it
 *       re-renders the login page with an error.</li>
 *   <li>POST {@code /oauth/authorize} with {@code stage=consent&flow=...&decision=allow|deny}
 *       --- the AS either generates an authorization code and redirects
 *       to the client's {@code redirect_uri} carrying the code and the
 *       {@code state} parameter, or redirects with
 *       {@code error=access_denied}.</li>
 * </ol>
 * <h2>Templates</h2>
 * The login and consent pages have minimal built-in HTML.  Applications
 * override them by placing replacement files at
 * {@code src/main/frontend/oauth/login.html} and
 * {@code .../consent.html}.  Placeholders are {@code &#123;name&#125;}
 * markers; see the default templates in the classpath at
 * {@code org/kissweb/oauth/as/templates/} for the supported names.
 * <h2>Sessions</h2>
 * After a successful login, the AS sets a session cookie
 * ({@code oauth_session}) scoped to {@code /oauth/} with a TTL from
 * {@code OAuthSessionTtlSeconds}.  Subsequent {@code /authorize} calls
 * from the same browser skip the login step.  Session state is held in
 * memory only --- restarts simply force everyone to log in again.
 */
@WebServlet(urlPatterns = "/oauth/authorize")
public class AuthorizationServlet extends HttpServlet {

    private static final Logger logger = LogManager.getLogger(AuthorizationServlet.class);

    private static final String SESSION_COOKIE = "oauth_session";
    private static final int FLOW_TTL_SECONDS = 600;   // 10 min for the user to log in + click Allow

    private static final ConcurrentHashMap<String, PendingAuthorization> flows    = new ConcurrentHashMap<>();
    private static final ConcurrentHashMap<String, String>               sessions = new ConcurrentHashMap<>(); // session_id -> user_sub
    private static final ConcurrentHashMap<String, Long>                 sessionExpiries = new ConcurrentHashMap<>();

    /** Default constructor for servlet container instantiation. */
    public AuthorizationServlet() { }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        prune();
        if (!AuthorizationServerConfig.get().isEnabled()) {
            response.sendError(HttpServletResponse.SC_NOT_FOUND);
            return;
        }
        if (AsExtensions.getUserAuthenticator() == null) {
            renderInfo(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR,
                    "OAuth AS is not fully configured",
                    "No UserAuthenticator has been registered.  Add one in KissInit.groovy "
                            + "via AsExtensions.setUserAuthenticator(...).");
            return;
        }

        // 1. Validate the request.
        final String responseType = nullToEmpty(request.getParameter("response_type"));
        final String clientId     = request.getParameter("client_id");
        final String redirectUri  = request.getParameter("redirect_uri");
        final String state        = nullToEmpty(request.getParameter("state"));
        final String scope        = nullToEmpty(request.getParameter("scope"));
        final String challenge    = request.getParameter("code_challenge");
        final String challengeMth = nullToEmpty(request.getParameter("code_challenge_method"));
        final String audience     = request.getParameter("resource");           // RFC 8707
        final String nonce        = request.getParameter("nonce");

        if (!"code".equals(responseType)) {
            redirectError(response, redirectUri, state, "unsupported_response_type",
                    "Only response_type=code is supported");
            return;
        }
        if (clientId == null || clientId.isEmpty()) {
            renderInfo(response, HttpServletResponse.SC_BAD_REQUEST, "Bad Request",
                    "client_id is required");
            return;
        }
        final RegisteredClient client = ClientStore.get().get(clientId);
        if (client == null) {
            renderInfo(response, HttpServletResponse.SC_BAD_REQUEST, "Unknown client",
                    "No client is registered with id '" + escapeHtml(clientId) + "'");
            return;
        }
        if (redirectUri == null || !client.hasRedirectUri(redirectUri)) {
            renderInfo(response, HttpServletResponse.SC_BAD_REQUEST, "Invalid redirect_uri",
                    "The supplied redirect_uri is not registered for this client.");
            return;
        }
        // From here on, errors can be reported by redirecting back to the client.
        if (!client.getAllowedGrantTypes().contains("authorization_code")) {
            redirectError(response, redirectUri, state, "unauthorized_client",
                    "Client is not allowed to use authorization_code grant");
            return;
        }
        if (challenge == null || challenge.isEmpty()) {
            redirectError(response, redirectUri, state, "invalid_request",
                    "code_challenge is required (PKCE)");
            return;
        }
        if (!PkceValidator.METHOD_S256.equals(challengeMth)) {
            redirectError(response, redirectUri, state, "invalid_request",
                    "code_challenge_method must be S256");
            return;
        }
        final Set<String> requested = parseScopes(scope);
        for (String s : requested) {
            if (!client.getAllowedScopes().contains(s)) {
                redirectError(response, redirectUri, state, "invalid_scope",
                        "Client is not permitted to request scope: " + s);
                return;
            }
        }

        // 2. Create a flow record.
        final String flowId = newRandom(16);
        final PendingAuthorization pending = new PendingAuthorization(
                flowId, clientId, redirectUri, requested, audience, state, challenge, challengeMth, nonce);
        flows.put(flowId, pending);

        // 3. If user already has a session, jump to consent; else show login.
        final AuthenticatedUser existing = getAuthenticatedUserForRequest(request);
        if (existing != null) {
            pending.authenticatedUser = existing;
            renderConsent(request, response, pending, client, null);
        } else {
            renderLogin(request, response, pending, client, null);
        }
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        prune();
        if (!AuthorizationServerConfig.get().isEnabled()) {
            response.sendError(HttpServletResponse.SC_NOT_FOUND);
            return;
        }

        final String flowId = request.getParameter("flow");
        final PendingAuthorization pending = flowId == null ? null : flows.get(flowId);
        if (pending == null) {
            renderInfo(response, HttpServletResponse.SC_BAD_REQUEST, "Session expired",
                    "Your authorization session has expired.  Please return to your application and try again.");
            return;
        }
        final RegisteredClient client = ClientStore.get().get(pending.clientId);
        if (client == null) {
            renderInfo(response, HttpServletResponse.SC_BAD_REQUEST, "Unknown client",
                    "The client for this authorization is no longer registered.");
            flows.remove(flowId);
            return;
        }

        final String stage = nullToEmpty(request.getParameter("stage"));
        switch (stage) {
            case "login":   handleLoginSubmit(request, response, pending, client);   return;
            case "consent": handleConsentSubmit(request, response, pending, client); return;
            default:
                renderInfo(response, HttpServletResponse.SC_BAD_REQUEST, "Bad Request",
                        "Missing or invalid 'stage' parameter.");
        }
    }

    // ==================================================================
    // Stage handlers
    // ==================================================================

    private void handleLoginSubmit(HttpServletRequest request,
                                   HttpServletResponse response,
                                   PendingAuthorization pending,
                                   RegisteredClient client) throws IOException {
        final String username = nullToEmpty(request.getParameter("username"));
        final String password = nullToEmpty(request.getParameter("password"));

        final UserAuthenticator authenticator = AsExtensions.getUserAuthenticator();
        AuthenticatedUser user = null;
        try {
            user = authenticator.authenticate(username, password);
        } catch (RuntimeException e) {
            logger.error("UserAuthenticator threw during login", e);
        }
        if (user == null) {
            renderLogin(request, response, pending, client, "Invalid username or password.");
            return;
        }

        // Successful login.  Set session cookie and render consent.
        final String sessionId = newRandom(24);
        sessions.put(sessionId, user.getSubject());
        sessionExpiries.put(sessionId,
                System.currentTimeMillis() / 1000L + AuthorizationServerConfig.get().getSessionTtlSeconds());
        // Also keep a per-session copy of the extra claims we want to ride along.
        userClaimsBySession.put(sessionId, user);

        final Cookie cookie = new Cookie(SESSION_COOKIE, sessionId);
        cookie.setPath("/oauth/");
        cookie.setHttpOnly(true);
        cookie.setSecure(request.isSecure());
        cookie.setMaxAge(AuthorizationServerConfig.get().getSessionTtlSeconds());
        response.addCookie(cookie);

        pending.authenticatedUser = user;
        renderConsent(request, response, pending, client, null);
    }

    private void handleConsentSubmit(HttpServletRequest request,
                                     HttpServletResponse response,
                                     PendingAuthorization pending,
                                     RegisteredClient client) throws IOException {
        if (pending.authenticatedUser == null) {
            // No user logged in for this flow --- shouldn't happen via the
            // normal UI, but treat defensively.
            renderLogin(request, response, pending, client, "Please sign in first.");
            return;
        }
        final String decision = nullToEmpty(request.getParameter("decision"));
        if (!"allow".equals(decision)) {
            // User clicked Deny (or anything other than Allow).
            flows.remove(pending.flowId);
            redirectError(response, pending.redirectUri, pending.state, "access_denied",
                    "The user denied the authorization request.");
            return;
        }

        // Generate and store the authorization code, then redirect back to
        // the client with code + state.
        final String code = TokenIssuer.issueAuthorizationCodeValue();
        final long expiresAt = System.currentTimeMillis() / 1000L
                + AuthorizationServerConfig.get().getAuthCodeTtlSeconds();
        final AuthorizationCode authCode = new AuthorizationCode(
                code, pending.clientId, pending.authenticatedUser, pending.redirectUri,
                pending.requestedScopes, pending.audience, pending.codeChallenge,
                pending.codeChallengeMethod, pending.nonce, expiresAt);
        AuthorizationCodeStore.get().store(authCode);
        flows.remove(pending.flowId);

        final StringBuilder url = new StringBuilder(pending.redirectUri);
        url.append(pending.redirectUri.contains("?") ? '&' : '?');
        url.append("code=").append(urlEnc(code));
        if (!pending.state.isEmpty())
            url.append("&state=").append(urlEnc(pending.state));
        response.sendRedirect(url.toString());
    }

    // ==================================================================
    // Rendering
    // ==================================================================

    private void renderLogin(HttpServletRequest request,
                             HttpServletResponse response,
                             PendingAuthorization pending,
                             RegisteredClient client,
                             String errorMessage) throws IOException {
        final Map<String, String> vars = new LinkedHashMap<>();
        vars.put("asName",       escapeHtml(AsExtensions.getConsentProvider().getDisplayName()));
        vars.put("clientName",   escapeHtml(orFallback(client.getClientName(), client.getClientId())));
        vars.put("flowId",       escapeHtml(pending.flowId));
        vars.put("errorBlock",   errorMessage == null ? "" :
                "<div class=\"error\">" + escapeHtml(errorMessage) + "</div>");
        writeHtml(response, render(loadTemplate(request, "login.html", DEFAULT_LOGIN_HTML), vars));
    }

    private void renderConsent(HttpServletRequest request,
                               HttpServletResponse response,
                               PendingAuthorization pending,
                               RegisteredClient client,
                               String errorMessage) throws IOException {
        final ConsentProvider provider = AsExtensions.getConsentProvider();
        final StringBuilder scopeList = new StringBuilder();
        for (String s : pending.requestedScopes) {
            scopeList.append("<li><strong>").append(escapeHtml(s)).append("</strong> &mdash; ")
                     .append(escapeHtml(provider.describeScope(s))).append("</li>\n");
        }
        if (scopeList.length() == 0)
            scopeList.append("<li>(no specific scopes requested)</li>\n");

        final Map<String, String> vars = new LinkedHashMap<>();
        vars.put("asName",       escapeHtml(provider.getDisplayName()));
        vars.put("clientName",   escapeHtml(orFallback(client.getClientName(), client.getClientId())));
        vars.put("userSubject",  escapeHtml(pending.authenticatedUser.getSubject()));
        vars.put("flowId",       escapeHtml(pending.flowId));
        vars.put("scopeList",    scopeList.toString());
        vars.put("errorBlock",   errorMessage == null ? "" :
                "<div class=\"error\">" + escapeHtml(errorMessage) + "</div>");
        writeHtml(response, render(loadTemplate(request, "consent.html", DEFAULT_CONSENT_HTML), vars));
    }

    private static void renderInfo(HttpServletResponse response, int status, String heading, String detail)
            throws IOException {
        response.setStatus(status);
        response.setContentType("text/html");
        response.setCharacterEncoding("UTF-8");
        try (PrintWriter w = response.getWriter()) {
            w.print("<!DOCTYPE html><html><head><title>");
            w.print(escapeHtml(heading));
            w.print("</title><style>body{font-family:sans-serif;max-width:560px;margin:60px auto;padding:20px;}h1{font-size:18px;color:#444;}</style></head><body><h1>");
            w.print(escapeHtml(heading));
            w.print("</h1><p>");
            w.print(escapeHtml(detail));
            w.print("</p></body></html>");
        }
    }

    private static void redirectError(HttpServletResponse response, String redirectUri, String state,
                                      String error, String description) throws IOException {
        if (redirectUri == null) {
            renderInfo(response, HttpServletResponse.SC_BAD_REQUEST, "Authorization error",
                    error + ": " + description);
            return;
        }
        final StringBuilder url = new StringBuilder(redirectUri);
        url.append(redirectUri.contains("?") ? '&' : '?');
        url.append("error=").append(urlEnc(error));
        if (description != null)
            url.append("&error_description=").append(urlEnc(description));
        if (state != null && !state.isEmpty())
            url.append("&state=").append(urlEnc(state));
        response.sendRedirect(url.toString());
    }

    // ==================================================================
    // Templates
    // ==================================================================

    /** Per-(template-name) cached resolved template content. */
    private static final ConcurrentHashMap<String, String> templateCache = new ConcurrentHashMap<>();

    /**
     * Load a template, preferring an app override at
     * {@code /oauth/<name>} (i.e. {@code src/main/frontend/oauth/<name>})
     * over the built-in default bundled in the framework JAR.
     */
    private String loadTemplate(HttpServletRequest request, String name, String fallback) {
        return templateCache.computeIfAbsent(name, n -> {
            // App-side override under the deployed web root?
            final ServletContext ctx = getServletContext();
            try (InputStream in = ctx.getResourceAsStream("/oauth/" + n)) {
                if (in != null) {
                    final String s = new Scanner(in, StandardCharsets.UTF_8).useDelimiter("\\A").next();
                    logger.info("Using app-provided " + n + " template from /oauth/" + n);
                    return s;
                }
            } catch (Exception e) {
                logger.warn("Failed to read app-provided " + n + " template: " + e.getMessage());
            }
            return fallback;
        });
    }

    private static String render(String template, Map<String, String> vars) {
        String out = template;
        for (Map.Entry<String, String> e : vars.entrySet())
            out = out.replace("{" + e.getKey() + "}", e.getValue());
        return out;
    }

    /** Cached per-session AuthenticatedUser so consent submit knows the extras to embed. */
    static final ConcurrentHashMap<String, AuthenticatedUser> userClaimsBySession = new ConcurrentHashMap<>();

    /**
     * Look up the {@link AuthenticatedUser} associated with the
     * session cookie on this request, if any.  Returns null if no
     * cookie, the cookie is unknown, or the session has expired
     * (in which case the session is cleaned up as a side effect).
     *
     * @param request the request
     * @return the cached user, or null if no valid session
     */
    static AuthenticatedUser getAuthenticatedUserForRequest(HttpServletRequest request) {
        final Cookie[] cookies = request.getCookies();
        if (cookies == null) return null;
        for (Cookie c : cookies) {
            if (!SESSION_COOKIE.equals(c.getName())) continue;
            final String id = c.getValue();
            final Long expires = sessionExpiries.get(id);
            if (expires == null || expires < System.currentTimeMillis() / 1000L) {
                sessions.remove(id);
                sessionExpiries.remove(id);
                userClaimsBySession.remove(id);
                return null;
            }
            return userClaimsBySession.get(id);
        }
        return null;
    }

    // ==================================================================
    // House-keeping
    // ==================================================================

    static void prune() {
        final OAuthIniStore store = OAuthIniStore.get();
        if (!store.tryStartPrune())
            return;
        try {
            AuthorizationCodeStore.get().pruneExpired();
            RefreshTokenStore.get().pruneExpired();
        } catch (IOException e) {
            logger.warn("Prune sweep failed: " + e.getMessage());
        }
        // In-process flow + session expiries
        final long now = System.currentTimeMillis() / 1000L;
        flows.entrySet().removeIf(e -> now - e.getValue().createdAtEpochSeconds > FLOW_TTL_SECONDS);
        sessionExpiries.entrySet().removeIf(e -> e.getValue() < now);
        for (String id : new java.util.ArrayList<>(sessions.keySet()))
            if (!sessionExpiries.containsKey(id)) {
                sessions.remove(id);
                userClaimsBySession.remove(id);
            }
    }

    // ==================================================================
    // Helpers
    // ==================================================================

    private static String urlEnc(String s) {
        return URLEncoder.encode(s == null ? "" : s, StandardCharsets.UTF_8);
    }

    private static String nullToEmpty(String s) {
        return s == null ? "" : s;
    }

    private static String orFallback(String s, String fallback) {
        return (s == null || s.isEmpty()) ? fallback : s;
    }

    private static String escapeHtml(String s) {
        if (s == null) return "";
        return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")
                .replace("\"", "&quot;").replace("'", "&#39;");
    }

    private static Set<String> parseScopes(String scope) {
        if (scope == null || scope.isEmpty())
            return Collections.emptySet();
        final Set<String> out = new LinkedHashSet<>();
        for (String s : scope.split("\\s+"))
            if (!s.isEmpty())
                out.add(s);
        return out;
    }

    private static String newRandom(int bytes) {
        final byte[] buf = new byte[bytes];
        new SecureRandom().nextBytes(buf);
        return Base64.getUrlEncoder().withoutPadding().encodeToString(buf);
    }

    private static void writeHtml(HttpServletResponse response, String html) throws IOException {
        response.setStatus(HttpServletResponse.SC_OK);
        response.setContentType("text/html");
        response.setCharacterEncoding("UTF-8");
        try (PrintWriter w = response.getWriter()) {
            w.print(html);
        }
    }

    private static String resolveOrigin(HttpServletRequest request) {
        try {
            final URI uri = new URI(request.getRequestURL().toString());
            return uri.getScheme() + "://" + uri.getAuthority();
        } catch (URISyntaxException e) {
            return "";
        }
    }

    // ==================================================================
    // Default templates (used when src/main/frontend/oauth/<name> absent)
    // ==================================================================

    private static final String DEFAULT_LOGIN_HTML =
            "<!DOCTYPE html>\n" +
            "<html lang=\"en\">\n" +
            "<head>\n" +
            "  <meta charset=\"UTF-8\">\n" +
            "  <title>Sign in</title>\n" +
            "  <style>\n" +
            "    body { font-family: -apple-system, system-ui, sans-serif; max-width: 420px; margin: 60px auto; padding: 28px; color: #222; }\n" +
            "    h1 { font-size: 20px; margin-top: 0; }\n" +
            "    .sub { color: #666; margin-bottom: 24px; }\n" +
            "    label { display: block; margin-bottom: 4px; font-size: 13px; color: #444; }\n" +
            "    input[type=text], input[type=password] { width: 100%; padding: 8px 10px; font-size: 14px; margin-bottom: 14px; border: 1px solid #ccc; border-radius: 4px; box-sizing: border-box; }\n" +
            "    button { padding: 10px 22px; font-size: 14px; cursor: pointer; background: #2563eb; color: #fff; border: none; border-radius: 4px; }\n" +
            "    button:hover { background: #1d4ed8; }\n" +
            "    .error { background: #fef2f2; border: 1px solid #fecaca; color: #991b1b; padding: 10px; border-radius: 4px; margin-bottom: 16px; font-size: 13px; }\n" +
            "  </style>\n" +
            "</head>\n" +
            "<body>\n" +
            "  <h1>Sign in to {asName}</h1>\n" +
            "  <p class=\"sub\"><strong>{clientName}</strong> is requesting access.</p>\n" +
            "  {errorBlock}\n" +
            "  <form method=\"POST\" action=\"/oauth/authorize\" autocomplete=\"on\">\n" +
            "    <input type=\"hidden\" name=\"stage\" value=\"login\">\n" +
            "    <input type=\"hidden\" name=\"flow\" value=\"{flowId}\">\n" +
            "    <label for=\"u\">Username</label>\n" +
            "    <input id=\"u\" type=\"text\" name=\"username\" autocomplete=\"username\" autofocus required>\n" +
            "    <label for=\"p\">Password</label>\n" +
            "    <input id=\"p\" type=\"password\" name=\"password\" autocomplete=\"current-password\" required>\n" +
            "    <button type=\"submit\">Sign in</button>\n" +
            "  </form>\n" +
            "</body>\n" +
            "</html>\n";

    private static final String DEFAULT_CONSENT_HTML =
            "<!DOCTYPE html>\n" +
            "<html lang=\"en\">\n" +
            "<head>\n" +
            "  <meta charset=\"UTF-8\">\n" +
            "  <title>Authorize</title>\n" +
            "  <style>\n" +
            "    body { font-family: -apple-system, system-ui, sans-serif; max-width: 480px; margin: 60px auto; padding: 28px; color: #222; }\n" +
            "    h1 { font-size: 20px; margin-top: 0; }\n" +
            "    .sub { color: #666; margin-bottom: 18px; }\n" +
            "    ul.scopes { padding-left: 0; list-style: none; margin: 18px 0 24px 0; }\n" +
            "    ul.scopes li { padding: 10px 12px; border: 1px solid #e5e7eb; border-radius: 4px; margin-bottom: 6px; font-size: 14px; }\n" +
            "    .who { font-size: 12px; color: #888; margin-bottom: 20px; }\n" +
            "    button { padding: 10px 22px; font-size: 14px; cursor: pointer; border: none; border-radius: 4px; margin-right: 8px; }\n" +
            "    .allow { background: #16a34a; color: #fff; } .allow:hover { background: #15803d; }\n" +
            "    .deny { background: #e5e7eb; color: #111; } .deny:hover { background: #d1d5db; }\n" +
            "    .error { background: #fef2f2; border: 1px solid #fecaca; color: #991b1b; padding: 10px; border-radius: 4px; margin-bottom: 16px; font-size: 13px; }\n" +
            "  </style>\n" +
            "</head>\n" +
            "<body>\n" +
            "  <h1>Authorize {clientName}</h1>\n" +
            "  <p class=\"sub\"><strong>{clientName}</strong> is requesting permission to:</p>\n" +
            "  {errorBlock}\n" +
            "  <ul class=\"scopes\">\n" +
            "    {scopeList}\n" +
            "  </ul>\n" +
            "  <p class=\"who\">You are signed in as <strong>{userSubject}</strong>.</p>\n" +
            "  <form method=\"POST\" action=\"/oauth/authorize\">\n" +
            "    <input type=\"hidden\" name=\"stage\" value=\"consent\">\n" +
            "    <input type=\"hidden\" name=\"flow\" value=\"{flowId}\">\n" +
            "    <button type=\"submit\" name=\"decision\" value=\"allow\" class=\"allow\">Allow</button>\n" +
            "    <button type=\"submit\" name=\"decision\" value=\"deny\" class=\"deny\">Deny</button>\n" +
            "  </form>\n" +
            "</body>\n" +
            "</html>\n";
}
