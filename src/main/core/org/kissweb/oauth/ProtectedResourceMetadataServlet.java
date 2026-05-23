package org.kissweb.oauth;

import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;

import java.io.IOException;
import java.io.PrintWriter;

/**
 * Serves the OAuth 2.0 Protected Resource Metadata document
 * (<a href="https://datatracker.ietf.org/doc/rfc9728/">RFC 9728</a>) that
 * MCP clients and other OAuth-aware clients fetch to discover which
 * authorization server protects this resource.
 * <br><br>
 * The endpoint is auto-registered at
 * {@code /.well-known/oauth-protected-resource} via {@code @WebServlet}
 * annotation scanning.  If OAuth is not configured (no
 * {@code OAuthAuthorizationServer} in {@code application.ini}), the
 * endpoint returns {@code 404 Not Found} --- the same response a client
 * would get from any non-OAuth resource server, which tells the client
 * that the resource isn't protected by OAuth.
 * <br><br>
 * The {@code WWW-Authenticate} challenge written by
 * {@link BearerTokenValidator} points clients at this URL.
 */
@WebServlet(urlPatterns = "/.well-known/oauth-protected-resource")
public class ProtectedResourceMetadataServlet extends HttpServlet {

    /**
     * Default constructor for servlet container instantiation.
     */
    public ProtectedResourceMetadataServlet() {
    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        final OAuthConfig cfg = OAuthConfig.get();
        if (!cfg.isEnabled()) {
            response.sendError(HttpServletResponse.SC_NOT_FOUND);
            return;
        }

        final JSONObject doc = new JSONObject();
        doc.put("resource", cfg.getResourceIdentifier());

        final JSONArray authServers = new JSONArray();
        authServers.put(cfg.getAuthorizationServer());
        doc.put("authorization_servers", authServers);

        final JSONArray methods = new JSONArray();
        methods.put("header");
        doc.put("bearer_methods_supported", methods);

        if (!cfg.getRequiredScopes().isEmpty()) {
            final JSONArray scopes = new JSONArray();
            for (String s : cfg.getRequiredScopes())
                scopes.put(s);
            doc.put("scopes_supported", scopes);
        }

        final JSONArray algs = new JSONArray();
        for (String a : cfg.getAllowedAlgorithms())
            algs.put(a);
        doc.put("resource_signing_alg_values_supported", algs);

        response.setStatus(HttpServletResponse.SC_OK);
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        // Allow clients to cache the metadata for an hour; it changes rarely.
        response.setHeader("Cache-Control", "public, max-age=3600");
        try (PrintWriter out = response.getWriter()) {
            out.print(doc.toString(2));
        }
    }
}
