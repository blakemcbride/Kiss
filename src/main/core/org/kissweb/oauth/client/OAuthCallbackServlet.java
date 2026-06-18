package org.kissweb.oauth.client;

import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import java.io.IOException;
import java.io.PrintWriter;

/**
 * Redirect target for the OAuth 2.1 authorization-code flow, mounted at
 * {@code /oauth/client/callback}.  The remote authorization server sends
 * the browser here with a {@code code} and {@code state}; this servlet
 * validates the {@code state} against the in-memory
 * {@link PendingAuthorization} registry (CSRF protection) and hands the
 * code to {@link OAuthClient#completeAuthorization} for exchange.
 * <br><br>
 * Returns {@code 404 Not Found} when no client provider is configured, so
 * the endpoint is inert in applications that do not use the OAuth client
 * --- the same convention the resource-server discovery endpoint follows.
 * <br><br>
 * This is a framework OAuth endpoint (like the authorization-server
 * servlets under {@code org.kissweb.oauth.as}), not an application
 * JSON-RPC service, so it legitimately renders a small HTML completion
 * page; the "backend never generates HTML" rule governs application
 * business endpoints, not OAuth protocol endpoints.
 */
@WebServlet(urlPatterns = OAuthClientConfig.CALLBACK_PATH)
public class OAuthCallbackServlet extends HttpServlet {

    private static final Logger logger = LogManager.getLogger(OAuthCallbackServlet.class);

    /** Default constructor for servlet container instantiation. */
    public OAuthCallbackServlet() { }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        if (!OAuthClientConfig.get().isEnabled()) {
            response.sendError(HttpServletResponse.SC_NOT_FOUND);
            return;
        }

        // The server reported an error instead of returning a code.
        final String error = request.getParameter("error");
        if (error != null && !error.isEmpty()) {
            final String desc = request.getParameter("error_description");
            renderError(response, HttpServletResponse.SC_BAD_REQUEST,
                    "Authorization failed: " + error + (desc == null ? "" : " --- " + desc));
            return;
        }

        final String state = request.getParameter("state");
        final PendingAuthorization pending = PendingAuthorization.consume(state);
        if (pending == null) {
            renderError(response, HttpServletResponse.SC_BAD_REQUEST,
                    "Invalid or expired authorization state. Please start the login again.");
            return;
        }

        final String code = request.getParameter("code");
        if (code == null || code.isEmpty()) {
            renderError(response, HttpServletResponse.SC_BAD_REQUEST,
                    "Authorization response was missing the authorization code.");
            return;
        }

        if (OAuthClientConfig.get().getProvider(pending.getProvider()) == null) {
            renderError(response, HttpServletResponse.SC_BAD_REQUEST,
                    "Provider '" + pending.getProvider() + "' is no longer configured.");
            return;
        }

        try {
            OAuthClient.forProvider(pending.getProvider()).completeAuthorization(pending, code);
        } catch (OAuthAuthorizationRequiredException e) {
            renderError(response, HttpServletResponse.SC_BAD_REQUEST,
                    "The authorization code was rejected. Please start the login again.");
            return;
        } catch (OAuthClientException e) {
            logger.error("OAuth code exchange failed for provider '" + pending.getProvider() + "'", e);
            renderError(response, HttpServletResponse.SC_BAD_GATEWAY,
                    "Could not complete authorization: " + e.getMessage());
            return;
        }

        renderSuccess(response, pending.getProvider());
    }

    // ------------------------------------------------------------------

    private static void renderSuccess(HttpServletResponse response, String provider) throws IOException {
        response.setStatus(HttpServletResponse.SC_OK);
        response.setContentType("text/html");
        response.setCharacterEncoding("UTF-8");
        try (PrintWriter w = response.getWriter()) {
            w.print("<!DOCTYPE html><html><head><meta charset=\"utf-8\">"
                    + "<title>Authorization complete</title></head><body>"
                    + "<h2>Authorization complete</h2>"
                    + "<p>Connected to <b>" + escape(provider) + "</b>. You may close this window.</p>"
                    + "<script>setTimeout(function(){window.close();},1500);</script>"
                    + "</body></html>");
        }
    }

    private static void renderError(HttpServletResponse response, int status, String message) throws IOException {
        response.setStatus(status);
        response.setContentType("text/html");
        response.setCharacterEncoding("UTF-8");
        try (PrintWriter w = response.getWriter()) {
            w.print("<!DOCTYPE html><html><head><meta charset=\"utf-8\">"
                    + "<title>Authorization error</title></head><body>"
                    + "<h2>Authorization error</h2>"
                    + "<p>" + escape(message) + "</p>"
                    + "</body></html>");
        }
    }

    /** Minimal HTML-escape for the small amount of dynamic text rendered here. */
    private static String escape(String s) {
        if (s == null)
            return "";
        return s.replace("&", "&amp;")
                .replace("<", "&lt;")
                .replace(">", "&gt;")
                .replace("\"", "&quot;");
    }
}
