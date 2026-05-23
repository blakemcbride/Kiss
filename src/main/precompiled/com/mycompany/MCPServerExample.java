package com.mycompany;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.MCPServerBase;
import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;
// Uncomment if a tool method needs to read the validated OAuth token (see bottom of file).
//import org.kissweb.oauth.BearerTokenValidator;
//import org.kissweb.oauth.ValidatedToken;

import jakarta.servlet.annotation.WebServlet;


/**
 * Author: Blake McBride <br>
 * Date: 2026-05-02
 * <br><br>
 * Sample MCP (Model Context Protocol) server.
 * <br><br>
 * MCP is the open protocol used by AI assistants (Claude Desktop, Claude Code, etc.) to talk to
 * external tools and data sources.  This class exposes an MCP endpoint over HTTP.  When the AI
 * assistant calls one of the advertised tools, control lands in {@link #callTool}.
 * <br><br>
 * All of the JSON-RPC 2.0 protocol boilerplate --- envelope parsing, the <code>initialize</code>
 * handshake, <code>ping</code>, notifications, and error replies --- is handled by
 * {@link MCPServerBase}.  This class only has to declare the four hooks.
 * <br><br>
 * (1) This class is essentially a template.  Copy it and edit it for your particular needs.
 * It must reside under the <code>precompiled</code> directory.
 * <br><br>
 * (2) Be sure to set the <code>urlPatterns</code> on the {@code @WebServlet} annotation.  The MCP
 * client will be configured with the full URL ending in this path.
 * <br><br>
 * (3) Add or remove tools by editing {@link #listTools()} (the catalog the client sees) and
 * {@link #callTool(String, JSONObject)} (the dispatcher that runs them).  The two must stay in sync.
 * <br><br>
 * (4) Authentication is automatic when configured.  If
 * <code>OAuthAuthorizationServer</code> is set in <code>application.ini</code>,
 * every request to this endpoint is validated as an OAuth 2.1 bearer token
 * with no code change required.  To use a different scheme (HTTP basic auth,
 * a custom token, mixed schemes, or no auth at all) override
 * {@link MCPServerBase#authenticate(jakarta.servlet.http.HttpServletRequest, jakarta.servlet.http.HttpServletResponse)}.
 * See the comment at the bottom of this file for accessing the validated token
 * from inside a tool method.
 * <br><br>
 * (5) Since this is Java code under <code>precompiled</code>, after editing run
 * <code>./bld -v build</code> and restart the server.  Hot reload does not apply here.
 */
@WebServlet(urlPatterns="/mcp-example")
public class MCPServerExample extends MCPServerBase {

    private static final Logger logger = LogManager.getLogger(MCPServerExample.class);

    @Override
    protected String getServerName() {
        return "kiss-mcp-example";
    }

    @Override
    protected String getServerVersion() {
        return "1.0.0";
    }

    /**
     * Advertise the tools the AI client may call.  Each tool needs a name, a human-readable
     * description, and a JSON Schema describing its arguments.  The model uses the description and
     * schema to decide when and how to call the tool.
     */
    @Override
    protected JSONArray listTools() {
        final JSONArray tools = new JSONArray();

        // ---- Tool: echo ----
        final JSONObject echo = new JSONObject();
        echo.put("name", "echo");
        echo.put("description", "Echo the supplied message back to the caller.");
        echo.put("inputSchema", buildSchema(
                "Echo input",
                new String[]{"message"},                       // required
                new String[]{"message"}, new String[]{"string"},
                new String[]{"Message to echo back"}));
        tools.put(echo);

        // ---- Tool: add ----
        final JSONObject add = new JSONObject();
        add.put("name", "add");
        add.put("description", "Add two numbers and return the sum.");
        add.put("inputSchema", buildSchema(
                "Add input",
                new String[]{"a", "b"},
                new String[]{"a", "b"}, new String[]{"number", "number"},
                new String[]{"First addend", "Second addend"}));
        tools.put(add);

        return tools;
    }

    /**
     * Execute a tool the AI client requested.  Return a result built with {@link #toolResult} for
     * success or {@link #toolError} for tool-specific failures.  Throwing causes the framework to
     * emit a JSON-RPC internal error, which the model does not see --- so prefer
     * <code>toolError</code> for anything you want the model to recover from.
     */
    @Override
    protected JSONObject callTool(String name, JSONObject args) {
        switch (name) {
            case "echo":
                return toolResult(args.getString("message", ""));
            case "add": {
                final double a = args.getDouble("a", 0.0);
                final double b = args.getDouble("b", 0.0);
                return toolResult(String.valueOf(a + b));
            }
            default:
                return toolError("Unknown tool: " + name);
        }
    }

    // ====================================================================================
    // OAuth 2.1 bearer-token authentication
    // ====================================================================================
    //
    // To require an OAuth 2.1 access token on every call to this MCP server, set in
    // application.ini:
    //
    //   OAuthAuthorizationServer = https://your-auth-server.example.com
    //   OAuthResourceIdentifier  = https://your-mcp-server.example.com
    //   OAuthRequiredScopes      = mcp:read mcp:write     # optional
    //
    // That's it --- the framework automatically validates the bearer token on every
    // request to this servlet, and serves the RFC 9728 discovery document at
    // /.well-known/oauth-protected-resource so MCP clients can bootstrap their auth.
    // No code change to this file is required.
    //
    // From inside any tool method you can then read the validated token (uncomment
    // the imports at the top of this file first):
    //
    //     ValidatedToken tok = BearerTokenValidator.currentToken();
    //     String userId = tok.getSubject();
    //     if (!tok.hasScope("mcp:write"))
    //         return toolError("This tool requires the mcp:write scope.");
    //
    // To use a different auth scheme (HTTP basic, custom token, mixed schemes, or
    // none even when OAuth is configured globally), override authenticate():
    //
    //     @Override
    //     protected boolean authenticate(HttpServletRequest req, HttpServletResponse resp) throws IOException {
    //         return basicAuthenticate(req, resp, "MyRealm", "user", "pass");
    //     }
}
