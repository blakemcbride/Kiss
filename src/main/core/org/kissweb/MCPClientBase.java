package org.kissweb;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;
import org.kissweb.oauth.client.OAuthAuthorizationRequiredException;
import org.kissweb.oauth.client.OAuthClient;

import java.io.IOException;
import java.time.Duration;
import java.util.concurrent.atomic.AtomicLong;

/**
 * Base class for MCP (Model Context Protocol) clients.
 * <br><br>
 * MCP is the open protocol AI assistants use to talk to external tools and data.  This class is the
 * mirror image of {@link MCPServerBase}: where that class implements the server side of the wire
 * protocol, this one implements the <em>client</em> side --- the JSON-RPC 2.0 envelope, the
 * <code>initialize</code> handshake, the <code>notifications/initialized</code> follow-up,
 * <code>ping</code>, and the <code>tools/list</code> / <code>tools/call</code> requests --- so a
 * subclass only has to say which remote server to talk to and how to authenticate to it.
 * <br><br>
 * The transport is MCP "Streamable HTTP" in its simplest form: each request is a single JSON-RPC
 * message POSTed to the server URL, and the reply is read back as one <code>application/json</code>
 * document.  This is exactly what {@link MCPServerBase} produces, so the two interoperate directly.
 * HTTP itself is done through {@link RestClient}.
 * <br><br>
 * A subclass implements one required hook:
 * <ul>
 *   <li>{@link #getServerUrl()} --- the full URL of the remote MCP endpoint</li>
 * </ul>
 * Optional hooks:
 * <ul>
 *   <li>{@link #getClientName()} / {@link #getClientVersion()} --- the <code>clientInfo</code>
 *       reported during initialize (defaults provided)</li>
 *   <li>{@link #getOAuthProviderName()} --- name of a configured {@code [OAuthClient <name>]}
 *       provider to obtain bearer tokens from (default: none)</li>
 *   <li>{@link #applyAuthentication(JSONObject)} --- attach auth headers to every request
 *       (default: OAuth bearer token when a provider name is given, otherwise nothing)</li>
 *   <li>{@link #getClientCapabilities()} --- advertise client feature areas (default: none)</li>
 * </ul>
 * <h2>Typical use</h2>
 * <pre>
 * class MyMcpClient extends MCPClientBase {
 *     protected String getServerUrl()        { return MainServlet.getEnvironment("RemoteMcpUrl"); }
 *     protected String getOAuthProviderName() { return "myprovider"; }   // optional
 * }
 *
 * MyMcpClient client = new MyMcpClient();
 * JSONArray tools = client.listTools();                 // auto-runs initialize on first call
 * JSONObject args = new JSONObject().put("message", "hi");
 * String reply = client.callToolText("echo", args);
 * </pre>
 * The handshake runs lazily on the first request, so callers do not have to invoke
 * {@link #initialize()} explicitly (they may, to inspect the server's capabilities up front).
 * <br><br>
 * <h2>Authentication</h2>
 * When {@link #getOAuthProviderName()} returns a configured provider name, every request carries an
 * <code>Authorization: Bearer ...</code> header whose token is obtained (and transparently
 * refreshed) via {@link OAuthClient}.  If no usable token is held and an interactive login is
 * required, {@link OAuthAuthorizationRequiredException} propagates out of the call --- the caller
 * should kick off the browser redirect flow (see {@link OAuthClient#beginAuthorization(String)})
 * rather than treat it as an error.  Override {@link #applyAuthentication(JSONObject)} for any other
 * scheme (HTTP basic, a static API token, etc.).
 * <br><br>
 * Unlike {@link MCPServerBase}, this class is application-neutral framework code under
 * <code>src/main/core/</code> and is used by instantiating (or subclassing) it from a backend
 * service; there is no servlet registration involved.
 * <br><br>
 * What this class does <em>not</em> implement (mirroring the server base; each can be added
 * incrementally): resources, prompts, sampling, server-initiated requests, SSE (text/event-stream)
 * streaming responses, and session management via the <code>Mcp-Session-Id</code> header.
 *
 * @see MCPServerBase
 */
public abstract class MCPClientBase {

    private static final Logger logger = LogManager.getLogger(MCPClientBase.class);

    /** The MCP protocol version this base class implements (matches {@link MCPServerBase#PROTOCOL_VERSION}). */
    public static final String PROTOCOL_VERSION = "2025-06-18";

    /** JSON-RPC 2.0 error code: invalid JSON was received by the server. */
    public static final int ERR_PARSE           = -32700;
    /** JSON-RPC 2.0 error code: the JSON sent is not a valid Request object. */
    public static final int ERR_INVALID_REQUEST = -32600;
    /** JSON-RPC 2.0 error code: the method does not exist or is not available. */
    public static final int ERR_METHOD_NOT_FOUND = -32601;
    /** JSON-RPC 2.0 error code: invalid method parameter(s). */
    public static final int ERR_INVALID_PARAMS  = -32602;
    /** JSON-RPC 2.0 error code: internal JSON-RPC error. */
    public static final int ERR_INTERNAL        = -32603;

    private final AtomicLong nextId = new AtomicLong(1);
    private final Object initLock = new Object();

    private volatile boolean initialized;
    private JSONObject serverInfo;
    private JSONObject serverCapabilities;
    private String     negotiatedProtocolVersion = PROTOCOL_VERSION;

    private Duration connectTimeout = Duration.ofSeconds(30);
    private Duration requestTimeout = Duration.ofSeconds(60);

    /**
     * Protected constructor, intended to be called by subclasses.
     */
    protected MCPClientBase() {
    }

    // ====================================================================================
    // Required hooks
    // ====================================================================================

    /**
     * The full URL of the remote MCP endpoint to connect to.
     * <br><br>
     * Per Kiss conventions this should come from <code>application.ini</code> (via
     * <code>MainServlet.getEnvironment(...)</code>) rather than be hard-coded.
     *
     * @return the remote MCP server URL
     */
    protected abstract String getServerUrl();

    // ====================================================================================
    // Optional hooks
    // ====================================================================================

    /**
     * Client name reported in the <code>clientInfo</code> of the <code>initialize</code> request.
     *
     * @return a short identifier for this client
     */
    protected String getClientName() {
        return "kiss-mcp-client";
    }

    /**
     * Client version reported in the <code>clientInfo</code> of the <code>initialize</code> request.
     *
     * @return a version string, e.g. "1.0.0"
     */
    protected String getClientVersion() {
        return "1.0.0";
    }

    /**
     * Name of the configured OAuth client provider (an <code>[OAuthClient &lt;name&gt;]</code>
     * section in <code>application.ini</code>) to obtain bearer tokens from.  When non-null, the
     * default {@link #applyAuthentication(JSONObject)} attaches a bearer token to every request.
     *
     * @return the provider name, or null for no OAuth (the default)
     */
    protected String getOAuthProviderName() {
        return null;
    }

    /**
     * Attach authentication headers to an outbound request.
     * <br><br>
     * Default behavior: if {@link #getOAuthProviderName()} returns a provider name, obtain a
     * (transparently refreshed) access token via {@link OAuthClient#getAccessToken()} and set the
     * <code>Authorization: Bearer ...</code> header.  If no provider is configured, no header is
     * added.
     * <br><br>
     * Override to use a different scheme (HTTP basic auth, a static API token, a custom header,
     * etc.).  Overrides that want OAuth plus extra headers can call
     * {@code super.applyAuthentication(headers)} first.
     *
     * @param headers the header object to populate (never null)
     * @throws OAuthAuthorizationRequiredException if OAuth is configured but no usable token is
     *         held and an interactive login is required
     */
    protected void applyAuthentication(JSONObject headers) {
        final String provider = getOAuthProviderName();
        if (provider == null || provider.isEmpty())
            return;
        final String token = OAuthClient.forProvider(provider).getAccessToken();
        headers.put("Authorization", "Bearer " + token);
    }

    /**
     * Build the capabilities object sent in the <code>initialize</code> request.  The default
     * advertises none.  Override to declare client features such as <code>roots</code> or
     * <code>sampling</code>.
     *
     * @return a capabilities JSON object
     */
    protected JSONObject getClientCapabilities() {
        return new JSONObject();
    }

    /**
     * Configure connection and request (read) timeouts.  Defaults are 30 s connect / 60 s request.
     *
     * @param connect the maximum time allowed for establishing a connection (null leaves it unchanged)
     * @param request the maximum time allowed until the response is fully received (null leaves it unchanged)
     * @return this client, for chaining
     */
    public MCPClientBase setTimeouts(Duration connect, Duration request) {
        if (connect != null)
            this.connectTimeout = connect;
        if (request != null)
            this.requestTimeout = request;
        return this;
    }

    // ====================================================================================
    // Public protocol operations
    // ====================================================================================

    /**
     * Perform the MCP <code>initialize</code> handshake and send the follow-up
     * <code>notifications/initialized</code>.  Safe to call more than once: the handshake runs only
     * the first time.  It is also run automatically the first time any other operation is invoked,
     * so calling this explicitly is optional --- do so when you want to inspect the server's
     * capabilities before making any tool calls.
     *
     * @return the full <code>initialize</code> result object returned by the server
     */
    public JSONObject initialize() {
        ensureInitialized();
        final JSONObject result = new JSONObject();
        result.put("protocolVersion", negotiatedProtocolVersion);
        result.put("capabilities", serverCapabilities == null ? new JSONObject() : serverCapabilities);
        result.put("serverInfo", serverInfo == null ? new JSONObject() : serverInfo);
        return result;
    }

    /**
     * Fetch the catalog of tools the remote server exposes.  Pagination (the
     * <code>nextCursor</code> field) is followed automatically, so the returned array contains every
     * tool.  Each element is a tool descriptor with <code>name</code>, <code>description</code>, and
     * <code>inputSchema</code> fields.
     *
     * @return a JSON array of tool descriptors
     */
    public JSONArray listTools() {
        ensureInitialized();
        final JSONArray all = new JSONArray();
        String cursor = null;
        do {
            final JSONObject params = new JSONObject();
            if (cursor != null)
                params.put("cursor", cursor);
            final JSONObject result = rpc("tools/list", params);
            final JSONArray tools = result.getJSONArray("tools", true);
            for (int i = 0; i < tools.length(); i++)
                all.put(tools.get(i));
            cursor = result.getString("nextCursor", null);
        } while (cursor != null && !cursor.isEmpty());
        return all;
    }

    /**
     * Call a tool on the remote server and return the raw <code>tools/call</code> result.
     * <br><br>
     * The result has a <code>content</code> array and an optional <code>isError</code> flag.  A tool
     * that fails in a way the model is meant to see reports it via <code>isError: true</code> rather
     * than a JSON-RPC error; this method returns that result normally.  Use {@link #isError} and
     * {@link #textOf} to inspect it, or {@link #callToolText} for the common "give me the text,
     * throw on error" case.
     *
     * @param name the tool name
     * @param arguments the arguments object to pass (may be null for a no-argument tool)
     * @return the tool-call result object
     * @throws MCPClientException if the server returns a JSON-RPC error or the transport fails
     */
    public JSONObject callTool(String name, JSONObject arguments) {
        ensureInitialized();
        final JSONObject params = new JSONObject();
        params.put("name", name);
        params.put("arguments", arguments == null ? new JSONObject() : arguments);
        return rpc("tools/call", params);
    }

    /**
     * Call a tool and return its text content as a single string (text blocks concatenated with
     * newlines).  Convenience wrapper around {@link #callTool}.
     *
     * @param name the tool name
     * @param arguments the arguments object to pass (may be null)
     * @return the concatenated text content of the result
     * @throws MCPClientException if the tool reported <code>isError: true</code>, the server
     *         returned a JSON-RPC error, or the transport failed
     */
    public String callToolText(String name, JSONObject arguments) {
        final JSONObject result = callTool(name, arguments);
        final String text = textOf(result);
        if (isError(result))
            throw new MCPClientException("Tool '" + name + "' returned an error: " + text);
        return text;
    }

    /**
     * Send a <code>ping</code> to verify the connection is alive.
     *
     * @throws MCPClientException if the server returns an error or the transport fails
     */
    public void ping() {
        ensureInitialized();
        rpc("ping", new JSONObject());
    }

    /**
     * Make an arbitrary JSON-RPC request and return its <code>result</code>.  Use this for MCP
     * methods not covered by the convenience wrappers (e.g. <code>resources/list</code>,
     * <code>prompts/get</code>).  The handshake is run first if needed.
     *
     * @param method the JSON-RPC method name
     * @param params the params object (may be null)
     * @return the <code>result</code> object from the response
     * @throws MCPClientException if the server returns a JSON-RPC error or the transport fails
     */
    public JSONObject call(String method, JSONObject params) {
        ensureInitialized();
        return rpc(method, params == null ? new JSONObject() : params);
    }

    /**
     * Send a JSON-RPC notification (a request with no <code>id</code>, expecting no response).  The
     * handshake is run first if needed.
     *
     * @param method the JSON-RPC method name
     * @param params the params object (may be null)
     * @throws MCPClientException if the transport fails
     */
    public void notify(String method, JSONObject params) {
        ensureInitialized();
        rpcNotify(method, params == null ? new JSONObject() : params);
    }

    // ====================================================================================
    // Accessors populated by the handshake
    // ====================================================================================

    /**
     * The <code>serverInfo</code> reported by the remote server during initialize.
     *
     * @return the server info object, or null if the handshake has not run
     */
    public JSONObject getServerInfo() {
        return serverInfo;
    }

    /**
     * The capabilities object the remote server advertised during initialize.
     *
     * @return the capabilities object, or null if the handshake has not run
     */
    public JSONObject getServerCapabilities() {
        return serverCapabilities;
    }

    /**
     * The protocol version negotiated with the server (the version it returned from initialize).
     *
     * @return the negotiated protocol version
     */
    public String getNegotiatedProtocolVersion() {
        return negotiatedProtocolVersion;
    }

    // ====================================================================================
    // Static helpers for reading tool results
    // ====================================================================================

    /**
     * Whether a <code>tools/call</code> result is flagged as an error
     * (<code>isError: true</code>).
     *
     * @param toolResult a result returned by {@link #callTool}
     * @return true if the tool reported an error
     */
    public static boolean isError(JSONObject toolResult) {
        return toolResult != null && toolResult.getBoolean("isError", false);
    }

    /**
     * Extract the text from a <code>tools/call</code> result --- the <code>text</code> field of
     * every <code>type: "text"</code> content block, joined with newlines.  Non-text blocks (images,
     * etc.) are skipped.
     *
     * @param toolResult a result returned by {@link #callTool}
     * @return the concatenated text content (empty string if there is none)
     */
    public static String textOf(JSONObject toolResult) {
        if (toolResult == null)
            return "";
        final JSONArray content = toolResult.getJSONArray("content", true);
        final StringBuilder sb = new StringBuilder();
        for (int i = 0; i < content.length(); i++) {
            final JSONObject block = content.getJSONObject(i);
            if (block == null)
                continue;
            if ("text".equals(block.getString("type", null))) {
                if (sb.length() > 0)
                    sb.append('\n');
                sb.append(block.getString("text", ""));
            }
        }
        return sb.toString();
    }

    // ====================================================================================
    // Internals
    // ====================================================================================

    /** Run the initialize handshake exactly once, the first time it is needed. */
    private void ensureInitialized() {
        if (initialized)
            return;
        synchronized (initLock) {
            if (initialized)
                return;

            final JSONObject params = new JSONObject();
            params.put("protocolVersion", PROTOCOL_VERSION);
            params.put("capabilities", getClientCapabilities());
            final JSONObject clientInfo = new JSONObject();
            clientInfo.put("name", getClientName());
            clientInfo.put("version", getClientVersion());
            params.put("clientInfo", clientInfo);

            final JSONObject result = rpc("initialize", params);
            serverInfo                = result.getJSONObject("serverInfo", true);
            serverCapabilities        = result.getJSONObject("capabilities", true);
            negotiatedProtocolVersion = result.getString("protocolVersion", PROTOCOL_VERSION);

            // Mark initialized before the notification so rpcNotify() does not recurse.
            initialized = true;
            rpcNotify("notifications/initialized", new JSONObject());
            logger.info("MCP client initialized against " + getServerUrl()
                    + " (server protocol " + negotiatedProtocolVersion + ")");
        }
    }

    /**
     * Perform one id'd JSON-RPC request and return its <code>result</code> object.  Throws
     * {@link MCPClientException} on a JSON-RPC error, a non-2xx HTTP status, an unparseable body, or
     * a transport failure.
     */
    private JSONObject rpc(String method, JSONObject params) {
        final long id = nextId.getAndIncrement();
        final JSONObject request = new JSONObject();
        request.put("jsonrpc", "2.0");
        request.put("id", id);
        request.put("method", method);
        request.put("params", params);

        final String responseBody = send(request.toString());
        if (responseBody == null || responseBody.isEmpty())
            throw new MCPClientException("Empty response from MCP server for method '" + method + "'");

        final JSONObject response;
        try {
            response = new JSONObject(responseBody);
        } catch (RuntimeException e) {
            throw new MCPClientException("Unparseable response from MCP server for method '"
                    + method + "': " + responseBody, e);
        }

        final JSONObject error = response.getJSONObject("error", false);
        if (error != null) {
            final int code = error.getInt("code", ERR_INTERNAL);
            final String message = error.getString("message", "(no message)");
            throw new MCPClientException(code, "MCP server error on '" + method + "': " + message);
        }
        return response.getJSONObject("result", true);
    }

    /** Send a JSON-RPC notification (no id, no response expected). */
    private void rpcNotify(String method, JSONObject params) {
        final JSONObject request = new JSONObject();
        request.put("jsonrpc", "2.0");
        request.put("method", method);
        request.put("params", params);
        send(request.toString());
    }

    /**
     * POST a JSON-RPC message to the server URL and return the response body.  Adds the protocol,
     * accept, and authentication headers.
     */
    private String send(String body) {
        final String url = getServerUrl();
        if (url == null || url.isEmpty())
            throw new MCPClientException("No MCP server URL configured (getServerUrl() returned "
                    + (url == null ? "null" : "an empty string") + ")");

        final JSONObject headers = new JSONObject();
        headers.put("Content-Type", "application/json");
        headers.put("Accept", "application/json");
        // Per MCP 2025-06-18, clients send the negotiated protocol version after initialize.
        headers.put("MCP-Protocol-Version", negotiatedProtocolVersion);
        applyAuthentication(headers);

        final RestClient rc = new RestClient().setTimeouts(connectTimeout, requestTimeout);
        final int status;
        try {
            status = rc.performService("POST", url, body, headers);
        } catch (IOException e) {
            throw new MCPClientException("MCP request to " + url + " failed", e);
        }
        final String responseBody = rc.getResponseString();

        if (status < 200 || status >= 300)
            throw new MCPClientException("MCP server " + url + " returned HTTP " + status
                    + (responseBody == null || responseBody.isEmpty() ? "" : ": " + responseBody));
        return responseBody;
    }

    // ====================================================================================
    // Exception
    // ====================================================================================

    /**
     * Thrown when an MCP request fails --- a JSON-RPC error from the server, a non-2xx HTTP status,
     * an unparseable response, or a transport-level failure.  When the failure carries a JSON-RPC
     * error code it is available via {@link #getCode()} (otherwise {@code 0}).
     * <br><br>
     * Note that a <em>tool</em> failure the model is meant to see is normally reported by the server
     * with <code>isError: true</code> in the result, not as one of these exceptions; see
     * {@link #callTool} and {@link #isError}.
     */
    public static class MCPClientException extends RuntimeException {

        /** The JSON-RPC error code associated with this failure, or {@code 0} if none. */
        private final int code;

        /**
         * Create an exception with no JSON-RPC error code.
         *
         * @param message the detail message
         */
        public MCPClientException(String message) {
            super(message);
            this.code = 0;
        }

        /**
         * Create an exception with no JSON-RPC error code, wrapping a cause.
         *
         * @param message the detail message
         * @param cause the underlying cause
         */
        public MCPClientException(String message, Throwable cause) {
            super(message, cause);
            this.code = 0;
        }

        /**
         * Create an exception carrying a JSON-RPC error code.
         *
         * @param code the JSON-RPC error code returned by the server
         * @param message the detail message
         */
        public MCPClientException(int code, String message) {
            super(message);
            this.code = code;
        }

        /**
         * The JSON-RPC error code, or {@code 0} if the failure was not a JSON-RPC error.
         *
         * @return the error code
         */
        public int getCode() {
            return code;
        }
    }
}
