package org.kissweb.oauth.as;

import jakarta.servlet.annotation.WebServlet;
import jakarta.servlet.http.HttpServlet;
import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;

import java.io.IOException;
import java.io.PrintWriter;

/**
 * Publishes the authorization server's JWKS at {@code /oauth/jwks}.
 * <br><br>
 * Resource servers (including the one in this same JVM, via
 * {@link org.kissweb.oauth.JwksCache}) fetch this to verify signatures
 * on access tokens issued by this AS.  The endpoint serves a single
 * key entry today --- the current RSA-2048 public key from
 * {@link KeyManager} --- but the response is shaped as a JSON array so
 * future key-rotation support can publish the outgoing previous key
 * alongside the incoming new key without changing the wire format.
 * <br><br>
 * Returns {@code 404 Not Found} when the AS is not enabled.
 */
@WebServlet(urlPatterns = "/oauth/jwks")
public class JwksPublishServlet extends HttpServlet {

    /** Default constructor for servlet container instantiation. */
    public JwksPublishServlet() { }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        if (!AuthorizationServerConfig.get().isEnabled()) {
            response.sendError(HttpServletResponse.SC_NOT_FOUND);
            return;
        }

        response.setStatus(HttpServletResponse.SC_OK);
        response.setContentType("application/jwk-set+json");
        response.setCharacterEncoding("UTF-8");
        response.setHeader("Cache-Control", "public, max-age=300");
        try (PrintWriter out = response.getWriter()) {
            out.print(KeyManager.get().buildJwks().toString(2));
        }
    }
}
