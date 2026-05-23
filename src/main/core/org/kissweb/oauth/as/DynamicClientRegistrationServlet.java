package org.kissweb.oauth.as;

import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.PrintWriter;
import java.nio.charset.StandardCharsets;
import java.security.MessageDigest;
import java.security.SecureRandom;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Base64;
import java.util.HashSet;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;

/**
 * RFC 7591 Dynamic Client Registration endpoint.
 * <br><br>
 * POST a JSON body containing the client's metadata
 * ({@code redirect_uris}, {@code client_name}, {@code scope},
 * {@code token_endpoint_auth_method}, etc.); the AS generates a
 * {@code client_id} (and, for confidential clients, a
 * {@code client_secret}), persists the registration, and returns the
 * full client record.
 * <br><br>
 * Required for MCP --- the spec mandates that MCP servers support DCR
 * so a freshly-installed MCP client can register itself without
 * out-of-band setup.
 * <br><br>
 * Two client kinds are supported:
 * <ul>
 *   <li>{@code token_endpoint_auth_method=none} --- public client (no
 *       secret); typical for native / desktop MCP clients.
 *       Authentication relies on PKCE.</li>
 *   <li>{@code token_endpoint_auth_method=client_secret_basic} or
 *       {@code client_secret_post} (the default) --- confidential
 *       client; the AS generates a secret and returns it in the
 *       response.  Only the SHA-256 hash of the secret is stored.</li>
 * </ul>
 * Returns {@code 404 Not Found} when the AS is disabled or DCR is
 * turned off via {@code OAuthAllowDynamicRegistration=false}.
 */
@WebServlet(urlPatterns = "/oauth/register")
public class DynamicClientRegistrationServlet extends HttpServlet {

    private static final Logger logger = LogManager.getLogger(DynamicClientRegistrationServlet.class);

    private static final Set<String> ALLOWED_AUTH_METHODS = new HashSet<>(Arrays.asList(
            "none", "client_secret_basic", "client_secret_post"));

    /** Default constructor for servlet container instantiation. */
    public DynamicClientRegistrationServlet() { }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        final AuthorizationServerConfig cfg = AuthorizationServerConfig.get();
        if (!cfg.isEnabled() || !cfg.isAllowDynamicRegistration()) {
            response.sendError(HttpServletResponse.SC_NOT_FOUND);
            return;
        }

        final JSONObject in;
        try {
            in = readJsonBody(request);
        } catch (RuntimeException e) {
            error(response, HttpServletResponse.SC_BAD_REQUEST, "invalid_client_metadata",
                    "Request body is not valid JSON");
            return;
        }

        final List<String> redirectUris = readStringArray(in, "redirect_uris");
        if (redirectUris.isEmpty()) {
            error(response, HttpServletResponse.SC_BAD_REQUEST, "invalid_redirect_uri",
                    "At least one redirect_uri is required");
            return;
        }
        // OAuth 2.1 forbids open redirectors and wildcard matching: every
        // URI must be an absolute, opaque string with no fragment.
        for (String uri : redirectUris) {
            if (uri.contains("#")) {
                error(response, HttpServletResponse.SC_BAD_REQUEST, "invalid_redirect_uri",
                        "redirect_uri must not contain a fragment: " + uri);
                return;
            }
        }

        final String authMethod = in.getString("token_endpoint_auth_method", "client_secret_basic");
        if (!ALLOWED_AUTH_METHODS.contains(authMethod)) {
            error(response, HttpServletResponse.SC_BAD_REQUEST, "invalid_client_metadata",
                    "Unsupported token_endpoint_auth_method: " + authMethod);
            return;
        }

        final Set<String> scopes = parseScopes(in.getString("scope", null));
        final Set<String> grantTypes = readStringSet(in, "grant_types");
        if (grantTypes.isEmpty()) {
            grantTypes.add("authorization_code");
            grantTypes.add("refresh_token");
        }
        // OAuth 2.1: only authorization_code and refresh_token are allowed for interactive clients.
        for (String g : grantTypes) {
            if (!g.equals("authorization_code") && !g.equals("refresh_token")) {
                error(response, HttpServletResponse.SC_BAD_REQUEST, "invalid_client_metadata",
                        "Unsupported grant_type: " + g);
                return;
            }
        }

        final String clientName = in.getString("client_name", "");
        final String clientId   = newClientId();

        final String clientSecret;
        final String clientSecretHash;
        if ("none".equals(authMethod)) {
            clientSecret     = null;
            clientSecretHash = null;
        } else {
            clientSecret     = newClientSecret();
            clientSecretHash = "sha256:" + sha256Hex(clientSecret);
        }

        final RegisteredClient client = new RegisteredClient(
                clientId, clientSecretHash, clientName, redirectUris,
                scopes, grantTypes, System.currentTimeMillis() / 1000L);
        try {
            ClientStore.get().register(client);
        } catch (IOException e) {
            logger.error("Failed to persist DCR client", e);
            error(response, HttpServletResponse.SC_INTERNAL_SERVER_ERROR, "server_error",
                    "Unable to persist new client registration");
            return;
        }

        final JSONObject out = new JSONObject();
        out.put("client_id",                  clientId);
        out.put("client_id_issued_at",        client.getCreatedAtEpochSeconds());
        if (clientSecret != null) {
            out.put("client_secret",          clientSecret);
            out.put("client_secret_expires_at", 0);     // 0 = never
        }
        out.put("client_name",                clientName);
        out.put("redirect_uris",              asJsonArray(redirectUris));
        out.put("grant_types",                asJsonArray(grantTypes));
        out.put("response_types",             asJsonArray(java.util.Collections.singleton("code")));
        out.put("token_endpoint_auth_method", authMethod);
        if (!scopes.isEmpty())
            out.put("scope", String.join(" ", scopes));

        response.setStatus(HttpServletResponse.SC_CREATED);
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        try (PrintWriter w = response.getWriter()) {
            w.print(out.toString(2));
        }
    }

    // ------------------------------------------------------------------

    private static JSONObject readJsonBody(HttpServletRequest request) throws IOException {
        final StringBuilder sb = new StringBuilder();
        try (BufferedReader r = request.getReader()) {
            final char[] buf = new char[1024];
            int n;
            while ((n = r.read(buf)) != -1)
                sb.append(buf, 0, n);
        }
        return new JSONObject(sb.toString());
    }

    private static List<String> readStringArray(JSONObject in, String key) {
        if (!in.has(key))
            return java.util.Collections.emptyList();
        final Object v = in.opt(key);
        final List<String> out = new ArrayList<>();
        if (v instanceof JSONArray) {
            final JSONArray a = (JSONArray) v;
            for (int i = 0; i < a.length(); i++) {
                String s = a.optString(i, null);
                if (s != null && !s.isEmpty())
                    out.add(s);
            }
        }
        return out;
    }

    private static Set<String> readStringSet(JSONObject in, String key) {
        return new LinkedHashSet<>(readStringArray(in, key));
    }

    private static Set<String> parseScopes(String scope) {
        if (scope == null || scope.isEmpty())
            return new LinkedHashSet<>();
        final Set<String> out = new LinkedHashSet<>();
        for (String s : scope.split("\\s+"))
            if (!s.isEmpty())
                out.add(s);
        return out;
    }

    private static JSONArray asJsonArray(Iterable<String> items) {
        final JSONArray a = new JSONArray();
        for (String s : items)
            a.put(s);
        return a;
    }

    private static String newClientId() {
        // 96 bits of entropy --- short enough to be readable in URLs,
        // long enough to make collisions implausible.
        final byte[] bytes = new byte[12];
        new SecureRandom().nextBytes(bytes);
        return Base64.getUrlEncoder().withoutPadding().encodeToString(bytes);
    }

    private static String newClientSecret() {
        // 256 bits of entropy for the actual secret.
        final byte[] bytes = new byte[32];
        new SecureRandom().nextBytes(bytes);
        return Base64.getUrlEncoder().withoutPadding().encodeToString(bytes);
    }

    private static String sha256Hex(String s) {
        try {
            final MessageDigest md = MessageDigest.getInstance("SHA-256");
            final byte[] digest = md.digest(s.getBytes(StandardCharsets.UTF_8));
            final StringBuilder sb = new StringBuilder(digest.length * 2);
            for (byte b : digest)
                sb.append(String.format("%02x", b));
            return sb.toString();
        } catch (Exception e) {
            throw new IllegalStateException(e);
        }
    }

    static void error(HttpServletResponse response, int status, String error, String description) throws IOException {
        response.setStatus(status);
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        final JSONObject err = new JSONObject();
        err.put("error", error);
        if (description != null)
            err.put("error_description", description);
        try (PrintWriter w = response.getWriter()) {
            w.print(err.toString());
        }
    }
}
