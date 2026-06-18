package com.mycompany;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.MCPClientBase;
import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;
import org.kissweb.restServer.MainServlet;


/**
 * Author: Blake McBride <br>
 * Date: 2026-06-18
 * <br><br>
 * Sample MCP (Model Context Protocol) client.
 * <br><br>
 * MCP is the open protocol used by AI assistants to talk to external tools and data sources.  This
 * class is the mirror image of {@link MCPServerExample}: instead of <em>serving</em> tools, it
 * <em>consumes</em> the tools of a remote MCP server (which may itself be an {@link MCPServerExample}
 * running elsewhere, or any other MCP server).
 * <br><br>
 * All of the JSON-RPC 2.0 protocol boilerplate --- the <code>initialize</code> handshake,
 * <code>notifications/initialized</code>, <code>ping</code>, and the <code>tools/list</code> /
 * <code>tools/call</code> requests --- is handled by {@link MCPClientBase}.  This class only has to
 * say which server to talk to (and, optionally, how to authenticate to it).
 * <br><br>
 * (1) This class is essentially a template.  Copy it and edit it for your particular needs.
 * <br><br>
 * (2) Unlike the server side, an MCP client is <em>not</em> a servlet --- there is no
 * <code>@WebServlet</code> annotation and no URL pattern.  You instantiate this class (or call its
 * static {@link #demo()} helper) from a backend service.  It lives under <code>precompiled</code>
 * here only so the template ships alongside {@link MCPServerExample}; a real client could just as
 * well be a backend service class.
 * <br><br>
 * (3) Per Kiss convention the remote URL (and any credentials) come from
 * <code>application.ini</code>, never hard-coded.  Set:
 * <pre>
 *   RemoteMcpUrl = https://some-host.example.com/mcp
 * </pre>
 * <br>
 * (4) The handshake runs automatically on the first request, so {@link #demo()} can call
 * {@link MCPClientBase#listTools()} directly.
 * <br><br>
 * (5) Since this is Java code under <code>precompiled</code>, after editing run
 * <code>./bld -v build</code> and restart the server.  Hot reload does not apply here.
 */
public class MCPClientExample extends MCPClientBase {

    private static final Logger logger = LogManager.getLogger(MCPClientExample.class);

    /**
     * The remote MCP endpoint to connect to.  Read from <code>application.ini</code> rather than
     * hard-coded, per Kiss convention.
     */
    @Override
    protected String getServerUrl() {
        return (String) MainServlet.getEnvironment("RemoteMcpUrl");
    }

    @Override
    protected String getClientName() {
        return "kiss-mcp-client-example";
    }

    @Override
    protected String getClientVersion() {
        return "1.0.0";
    }

    /**
     * Drive the example: connect to the remote server, list its tools, and call the
     * <code>echo</code> tool exposed by {@link MCPServerExample}.  Returns the echoed text.
     * <br><br>
     * Call this from a backend service, e.g.:
     * <pre>
     *   String reply = com.mycompany.MCPClientExample.demo();
     * </pre>
     *
     * @return the text returned by the remote <code>echo</code> tool
     */
    public static String demo() {
        final MCPClientExample client = new MCPClientExample();

        // Discover what the remote server offers (runs the initialize handshake on first use).
        final JSONArray tools = client.listTools();
        logger.info("Remote MCP server exposes " + tools.length() + " tool(s)");

        // Call the "echo" tool with a single "message" argument.
        final JSONObject args = new JSONObject();
        args.put("message", "Hello from the Kiss MCP client");
        final String reply = client.callToolText("echo", args);
        logger.info("echo tool returned: " + reply);
        return reply;
    }

    // ====================================================================================
    // Authentication
    // ====================================================================================
    //
    // If the remote MCP server requires an OAuth 2.1 bearer token, declare a provider in
    // application.ini:
    //
    //   [OAuthClient myprovider]
    //   Url    = https://some-host.example.com/mcp     ; the remote MCP server (the resource)
    //   Scopes = mcp:read mcp:write                    ; optional
    //
    // then point this client at it by overriding getOAuthProviderName():
    //
    //     @Override
    //     protected String getOAuthProviderName() {
    //         return "myprovider";
    //     }
    //
    // Every request will then carry a transparently-refreshed "Authorization: Bearer ..." header.
    // The first time no token is held, getAccessToken() (inside applyAuthentication) throws
    // OAuthAuthorizationRequiredException; catch it and send the browser through the login flow:
    //
    //     try {
    //         reply = client.callToolText("echo", args);
    //     } catch (OAuthAuthorizationRequiredException e) {
    //         String url = OAuthClient.forProvider("myprovider").beginAuthorization("http://localhost:8080");
    //         // ... redirect the browser to 'url'; retry after the callback completes the login.
    //     }
    //
    // For any other scheme (HTTP basic auth, a static API token, a custom header, etc.) override
    // applyAuthentication(JSONObject headers) instead:
    //
    //     @Override
    //     protected void applyAuthentication(JSONObject headers) {
    //         headers.put("Authorization", "Bearer " + (String) MainServlet.getEnvironment("RemoteMcpToken"));
    //     }
}
