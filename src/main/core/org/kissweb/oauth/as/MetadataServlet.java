package org.kissweb.oauth.as;

import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;

import java.io.IOException;
import java.io.PrintWriter;

/**
 * Serves the OAuth 2.0 Authorization Server Metadata document
 * (<a href="https://datatracker.ietf.org/doc/rfc8414/">RFC 8414</a>) at
 * {@code /.well-known/oauth-authorization-server}.
 * <br><br>
 * MCP clients fetch this after discovering the AS issuer URL from the
 * resource server's {@code .well-known/oauth-protected-resource}
 * document; from here they learn the location of the authorization,
 * token, registration, and JWKS endpoints, along with the supported
 * grant types and PKCE methods.
 * <br><br>
 * Returns {@code 404 Not Found} when the AS is not enabled
 * ({@code OAuthAsEnabled = false}) --- the same response a server with
 * no AS would give, signaling to clients that the resource is not
 * paired with this AS.
 */
@WebServlet(urlPatterns = "/.well-known/oauth-authorization-server")
public class MetadataServlet extends HttpServlet {

    /** Default constructor for servlet container instantiation. */
    public MetadataServlet() { }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        final AuthorizationServerConfig cfg = AuthorizationServerConfig.get();
        if (!cfg.isEnabled()) {
            response.sendError(HttpServletResponse.SC_NOT_FOUND);
            return;
        }

        final String issuer = cfg.getIssuer();
        final JSONObject doc = new JSONObject();
        doc.put("issuer",                                issuer);
        doc.put("authorization_endpoint",                issuer + "/oauth/authorize");
        doc.put("token_endpoint",                        issuer + "/oauth/token");
        doc.put("jwks_uri",                              issuer + "/oauth/jwks");
        if (cfg.isAllowDynamicRegistration())
            doc.put("registration_endpoint",             issuer + "/oauth/register");

        doc.put("response_types_supported",              jsonArr("code"));
        doc.put("grant_types_supported",                 jsonArr("authorization_code", "refresh_token"));
        doc.put("code_challenge_methods_supported",      jsonArr("S256"));
        doc.put("token_endpoint_auth_methods_supported", jsonArr("client_secret_basic", "client_secret_post", "none"));
        doc.put("id_token_signing_alg_values_supported", jsonArr("RS256"));
        doc.put("scopes_supported",                      new JSONArray());   // populated dynamically per client at /authorize
        doc.put("subject_types_supported",               jsonArr("public"));

        response.setStatus(HttpServletResponse.SC_OK);
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        response.setHeader("Cache-Control", "public, max-age=3600");
        try (PrintWriter out = response.getWriter()) {
            out.print(doc.toString(2));
        }
    }

    private static JSONArray jsonArr(String... values) {
        final JSONArray a = new JSONArray();
        for (String v : values)
            a.put(v);
        return a;
    }
}
