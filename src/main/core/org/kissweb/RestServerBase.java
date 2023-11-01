package org.kissweb;

import org.apache.log4j.Logger;
import org.json.JSONObject;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

/**
 * Author: Blake McBride <br>
 * Date: 11/1/23
 * <br><br>
 * This class provides the base functionality for creating non-Kiss REST servers.
 * See <code>org.kissweb.RestServer</code> for an example.
 */
public abstract class RestServerBase extends HttpServlet {

    private static final Logger logger = Logger.getLogger(RestServerBase.class);

    /**
     * Support for Basic Authentication.
     *
     * @param request
     * @param response
     * @param relm        whatever the client may require or null
     * @param userName
     * @param password
     * @return <code>true</code> means they passed authentication and <code>false</code> if they didn't
     */
    protected static boolean basicAuthenticate(HttpServletRequest request, HttpServletResponse response, String relm, String userName, String password) {
        final String authHeader = request.getHeader("Authorization");
        if (authHeader == null || !authHeader.startsWith("Basic ")) {
            logger.error("Unauthorized access");
            return false;
        }

        // Remove the "Basic " prefix
        final String base64Credentials = authHeader.substring("Basic ".length()).trim();

        // Decode the Base64 encoded header value
        final byte[] credDecoded = Base64.getDecoder().decode(base64Credentials);
        final String credentials = new String(credDecoded, StandardCharsets.UTF_8);
        if (credentials.isEmpty())
            return false;

        // credentials = username:password
        final String[] values = credentials.split(":", 2);
        if (values.length != 2)
            return false;
        final String user = values[0];
        final String passwd = values[1];
        if (userName == null || userName.isEmpty()) {
            logger.error("POSTMARK_USERNAME is not set");
            return false;
        }
        if (password == null || password.isEmpty()) {
            logger.error("POSTMARK_PASSWORD is not set");
            return false;
        }
         if (user.equals(userName) && passwd.equals(password))
             return true;
        // bad password or username
        response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
        if (relm != null && !relm.isEmpty())
            response.setHeader("WWW-Authenticate", "Basic realm=\"" + relm + "\"");
        return false;
    }

    /**
     * Get the body of the request as a string.
     *
     * @param request
     * @return
     * @throws IOException
     */
    protected static String getBodyString(HttpServletRequest request) throws IOException {
        final StringBuilder sb = new StringBuilder();
        final BufferedReader br = request.getReader();
        final char[] buf = new char[1024];
        while (true) {
            int n = br.read(buf);
            if (n == -1)
                break;
            sb.append(buf, 0, n);
        }
        br.close();
        return sb.toString();
    }

    /**
     * Get the body of the request as a JSON object.  If it's not JSON, return NULL.
     *
     * @param request
     * @return
     * @throws IOException
     */
    protected static JSONObject getBodyJson(HttpServletRequest request) throws IOException {
        if ("application/json".equals(request.getContentType()))
            return new JSONObject(getBodyString(request));
        else return null;
    }

    /**
     *  Get the body of the request as a binary array.
     *
     * @param request
     * @return
     * @throws IOException
     */
    protected static byte[] getBodyBinary(HttpServletRequest request) throws IOException {
        try (ByteArrayOutputStream buffer = new ByteArrayOutputStream()) {
            try (InputStream inputStream = request.getInputStream()) {
                byte[] tempBuffer = new byte[1024];
                int bytesRead;

                while ((bytesRead = inputStream.read(tempBuffer)) != -1)
                    buffer.write(tempBuffer, 0, bytesRead);
            }
            return buffer.toByteArray();
        }
    }

    /**
     * Set the response to a JSON object.
     *
     * @param response
     * @param json
     * @throws IOException
     */
    protected static void setResponse(HttpServletResponse response, JSONObject json) throws IOException {
        response.setContentType("application/json");
        response.setCharacterEncoding("UTF-8");
        PrintWriter out = response.getWriter();
        out.print(json.toString(2));
        out.flush();
    }

    /**
     * Set the response to a string.
     *
     * @param response
     * @param responseType like "text/plain", "text/html", "application/xml", etc.
     * @param resp
     * @throws IOException
     */
    protected static void setResponse(HttpServletResponse response, String responseType, String resp) throws IOException {
        response.setContentType(responseType);
        response.setCharacterEncoding("UTF-8");
        PrintWriter out = response.getWriter();
        out.print(resp);
        out.flush();
    }

        /**
     * Set the response to a string.
     *
     * @param response
     * @param responseType like "application/octet-stream", "image/jpeg", "audio/mpeg", "video/mp4", etc.
     * @param resp
     * @throws IOException
     */
    protected static void setResponse(HttpServletResponse response, String responseType, byte [] resp) throws IOException {
        response.setContentType(responseType);
        try (OutputStream out = response.getOutputStream()) {
            out.write(resp);
        }
    }
}
