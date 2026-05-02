package org.kissweb;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;

import jakarta.servlet.http.HttpServletRequest;
import jakarta.servlet.http.HttpServletResponse;
import java.io.IOException;

/**
 * Base class for MCP (Model Context Protocol) servers.
 * <br><br>
 * MCP is the open protocol AI assistants use to talk to external tools and data.  This class
 * implements the wire-protocol boilerplate --- the JSON-RPC 2.0 envelope, the
 * <code>initialize</code> handshake, <code>ping</code>, and dispatch to <code>tools/list</code>
 * and <code>tools/call</code> --- so a subclass only has to declare its tools and run them.
 * <br><br>
 * It extends {@link RestServerBase} and reuses its body-reading and response-writing helpers, so
 * an MCP server is registered exactly the same way a plain REST endpoint is: with a
 * {@code @WebServlet} annotation on the subclass, no <code>web.xml</code> entries needed.
 * <br><br>
 * A subclass implements four hooks:
 * <ul>
 *   <li>{@link #getServerName()} --- name reported during initialize</li>
 *   <li>{@link #getServerVersion()} --- version reported during initialize</li>
 *   <li>{@link #listTools()} --- the tool catalog (a {@link JSONArray} of tool descriptors)</li>
 *   <li>{@link #callTool(String, JSONObject)} --- execute a tool, return its result</li>
 * </ul>
 * Optional hooks:
 * <ul>
 *   <li>{@link #authenticate(HttpServletRequest, HttpServletResponse)} --- gate every request
 *       (default: allow)</li>
 *   <li>{@link #getCapabilities()} --- advertise additional MCP feature areas (default: tools only)</li>
 *   <li>{@link #dispatchExtension(String, JSONObject)} --- handle methods beyond the built-in set
 *       (default: return method-not-found)</li>
 * </ul>
 * Static helpers ({@link #textBlock}, {@link #toolResult}, {@link #toolError}, {@link #buildSchema})
 * cover the common cases of building tool descriptors and tool-call results.
 * <br><br>
 * Like {@link RestServerBase}, classes extending this one must reside under
 * <code>src/main/precompiled/</code> --- the servlet container discovers them via annotation
 * scanning at startup, so they have to be compiled into the WAR ahead of time.  Hot reload does
 * not apply.  See <code>com.mycompany.MCPServerExample</code> for a working template.
 * <br><br>
 * What this class does <em>not</em> implement (each can be added incrementally):
 * resources, prompts, sampling, server-initiated requests, SSE streaming responses, and session
 * management via the <code>Mcp-Session-Id</code> header.
 */
public abstract class MCPServerBase extends RestServerBase {

    private static final Logger logger = LogManager.getLogger(MCPServerBase.class);

    /** The MCP protocol version this base class implements. */
    public static final String PROTOCOL_VERSION = "2025-06-18";

    /** JSON-RPC 2.0 error code: invalid JSON was received by the server. */
    public static final int ERR_PARSE          = -32700;
    /** JSON-RPC 2.0 error code: the JSON sent is not a valid Request object. */
    public static final int ERR_INVALID_REQUEST = -32600;
    /** JSON-RPC 2.0 error code: the method does not exist or is not available. */
    public static final int ERR_METHOD_NOT_FOUND = -32601;
    /** JSON-RPC 2.0 error code: invalid method parameter(s). */
    public static final int ERR_INVALID_PARAMS = -32602;
    /** JSON-RPC 2.0 error code: internal JSON-RPC error. */
    public static final int ERR_INTERNAL       = -32603;

    /**
     * Protected constructor, intended to be called by subclasses.
     */
    protected MCPServerBase() {
    }

    // ====================================================================================
    // Required hooks
    // ====================================================================================

    /**
     * Server name reported in the <code>initialize</code> response.
     *
     * @return a short identifier for this server
     */
    protected abstract String getServerName();

    /**
     * Server version reported in the <code>initialize</code> response.
     *
     * @return a version string, e.g. "1.0.0"
     */
    protected abstract String getServerVersion();

    /**
     * The catalog of tools this server exposes.  Each element must be a {@link JSONObject} with
     * <code>name</code>, <code>description</code>, and <code>inputSchema</code> fields.  Use
     * {@link #buildSchema} to construct the schema concisely.
     *
     * @return a JSON array of tool descriptors
     */
    protected abstract JSONArray listTools();

    /**
     * Run a tool the AI client requested.  The returned object is the value of the JSON-RPC
     * <code>result</code> field for a <code>tools/call</code> response --- typically built with
     * {@link #toolResult(String)} or {@link #toolError(String)}.
     * <br><br>
     * Tool-specific failures should be returned with <code>isError: true</code> (see
     * {@link #toolError}) rather than thrown.  That way the model sees the failure and can react.
     * Throwing causes the framework to emit a JSON-RPC <code>-32603 Internal error</code>, which
     * the model does not see.
     *
     * @param name the tool name
     * @param arguments the arguments object the client supplied (may be empty, never null)
     * @return the tool-call result object
     * @throws Exception unexpected errors; converted to a JSON-RPC internal error
     */
    protected abstract JSONObject callTool(String name, JSONObject arguments) throws Exception;

    // ====================================================================================
    // Optional hooks
    // ====================================================================================

    /**
     * Authenticate the request before any MCP method is dispatched.  The default implementation
     * allows every request.  Override to require basic auth, a bearer token, etc.; if the request
     * is rejected, the override should set an appropriate response status and return false.
     *
     * @param request the incoming request
     * @param response the response (use to set 401, etc., on rejection)
     * @return true to proceed, false to abort handling
     * @throws IOException if writing to the response fails
     */
    protected boolean authenticate(HttpServletRequest request, HttpServletResponse response) throws IOException {
        return true;
    }

    /**
     * Build the capabilities object returned during <code>initialize</code>.  The default declares
     * the <code>tools</code> capability with <code>listChanged: false</code>.  Override to add
     * <code>resources</code>, <code>prompts</code>, etc.
     *
     * @return a capabilities JSON object
     */
    protected JSONObject getCapabilities() {
        final JSONObject caps = new JSONObject();
        final JSONObject toolsCap = new JSONObject();
        toolsCap.put("listChanged", false);
        caps.put("tools", toolsCap);
        return caps;
    }

    /**
     * Handle a method that isn't one of the built-in MCP methods.  The default returns
     * <code>null</code>, which the dispatcher converts to a JSON-RPC method-not-found error.
     * Override to implement <code>resources/list</code>, <code>prompts/get</code>, etc.
     * <br><br>
     * Returning <code>null</code> from the override means "I do not handle this method" --- the
     * framework will produce the appropriate error response.
     *
     * @param method the JSON-RPC method name
     * @param params the params object (may be empty, never null)
     * @return the result object, or null if the method is not handled
     * @throws Exception unexpected errors; converted to a JSON-RPC internal error
     */
    protected JSONObject dispatchExtension(String method, JSONObject params) throws Exception {
        return null;
    }

    // ====================================================================================
    // The dispatch loop
    // ====================================================================================

    @Override
    protected final void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        if (!authenticate(request, response))
            return;

        final JSONObject req = getBodyJson(request);
        if (req == null) {
            sendError(response, null, ERR_PARSE, "Parse error: expected application/json body");
            return;
        }

        // JSON-RPC 2.0 envelope: {"jsonrpc":"2.0","id":..,"method":"..","params":{..}}
        // A request that omits "id" is a notification: it expects no response.
        final boolean isNotification = !req.has("id");
        final Object id = req.opt("id");
        final String method = req.getString("method", null);
        final JSONObject params = req.getJSONObject("params", true);

        if (method == null) {
            sendError(response, id, ERR_INVALID_REQUEST, "Invalid Request: missing method");
            return;
        }

        try {
            final JSONObject result = handleMethod(method, params);

            if (result == null) {
                // dispatchExtension declined --- unknown method.
                if (isNotification) {
                    response.setStatus(HttpServletResponse.SC_ACCEPTED);
                    return;
                }
                sendError(response, id, ERR_METHOD_NOT_FOUND, "Method not found: " + method);
                return;
            }

            if (result == NOTIFICATION_HANDLED) {
                response.setStatus(HttpServletResponse.SC_ACCEPTED);
                return;
            }

            if (isNotification) {
                response.setStatus(HttpServletResponse.SC_ACCEPTED);
                return;
            }

            final JSONObject reply = new JSONObject();
            reply.put("jsonrpc", "2.0");
            reply.put("id", id);
            reply.put("result", result);
            setResponse(response, reply);
        } catch (Exception e) {
            logger.error("MCP error handling " + method, e);
            sendError(response, id, ERR_INTERNAL, "Internal error: " + e.getMessage());
        }
    }

    /** Sentinel used by {@link #handleMethod} to signal "this was a notification, no response". */
    private static final JSONObject NOTIFICATION_HANDLED = new JSONObject();

    /**
     * Dispatch a single MCP method.  Returns the result object, {@link #NOTIFICATION_HANDLED} for
     * a notification that needs no response, or null if the method is unknown.
     */
    private JSONObject handleMethod(String method, JSONObject params) throws Exception {
        switch (method) {
            case "initialize":
                return handleInitialize(params);
            case "tools/list": {
                final JSONObject result = new JSONObject();
                result.put("tools", listTools());
                return result;
            }
            case "tools/call":
                return callTool(params.getString("name", ""), params.getJSONObject("arguments", true));
            case "ping":
                return new JSONObject();
            default:
                if (method.startsWith("notifications/"))
                    return NOTIFICATION_HANDLED;
                return dispatchExtension(method, params);
        }
    }

    private JSONObject handleInitialize(JSONObject params) {
        final JSONObject result = new JSONObject();
        result.put("protocolVersion", PROTOCOL_VERSION);
        result.put("capabilities", getCapabilities());

        final JSONObject serverInfo = new JSONObject();
        serverInfo.put("name", getServerName());
        serverInfo.put("version", getServerVersion());
        result.put("serverInfo", serverInfo);

        return result;
    }

    // ====================================================================================
    // Helpers for building tool descriptors and results
    // ====================================================================================

    /**
     * Build a single MCP text content block: <code>{"type":"text","text":...}</code>.
     *
     * @param text the text content
     * @return a content-block JSON object
     */
    public static JSONObject textBlock(String text) {
        final JSONObject block = new JSONObject();
        block.put("type", "text");
        block.put("text", text);
        return block;
    }

    /**
     * Build a tools/call result containing a single text block.
     *
     * @param text the text to return
     * @return a tools/call result object
     */
    public static JSONObject toolResult(String text) {
        final JSONObject result = new JSONObject();
        final JSONArray content = new JSONArray();
        content.put(textBlock(text));
        result.put("content", content);
        return result;
    }

    /**
     * Build a tools/call error result --- a single text block plus <code>isError: true</code>.
     * Use this for tool-specific failures the model should see and react to.
     *
     * @param message the error message for the model
     * @return a tools/call error result object
     */
    public static JSONObject toolError(String message) {
        final JSONObject result = toolResult(message);
        result.put("isError", true);
        return result;
    }

    /**
     * Build a minimal JSON Schema object for a tool's <code>inputSchema</code>.  All properties
     * named in <code>names</code> are described; the <code>required</code> list names the subset
     * the model must supply.
     *
     * @param title human-readable schema title
     * @param required names of required properties
     * @param names property names (parallel arrays with <code>types</code> and <code>descriptions</code>)
     * @param types JSON Schema types ("string", "number", "boolean", "object", "array")
     * @param descriptions per-property description strings
     * @return a JSON Schema object
     */
    public static JSONObject buildSchema(String title,
                                         String[] required,
                                         String[] names,
                                         String[] types,
                                         String[] descriptions) {
        final JSONObject schema = new JSONObject();
        schema.put("type", "object");
        schema.put("title", title);

        final JSONObject properties = new JSONObject();
        for (int i = 0; i < names.length; i++) {
            final JSONObject prop = new JSONObject();
            prop.put("type", types[i]);
            prop.put("description", descriptions[i]);
            properties.put(names[i], prop);
        }
        schema.put("properties", properties);

        final JSONArray req = new JSONArray();
        for (String r : required)
            req.put(r);
        schema.put("required", req);

        return schema;
    }

    /**
     * Send a JSON-RPC error response.  Visible to subclasses so they can fail fast from inside
     * an extension handler if needed.
     *
     * @param response the response to write to
     * @param id the request id (may be null per JSON-RPC 2.0)
     * @param code the JSON-RPC error code
     * @param message the error message
     * @throws IOException if writing fails
     */
    protected void sendError(HttpServletResponse response, Object id, int code, String message) throws IOException {
        final JSONObject reply = new JSONObject();
        reply.put("jsonrpc", "2.0");
        reply.put("id", id);
        final JSONObject err = new JSONObject();
        err.put("code", code);
        err.put("message", message);
        reply.put("error", err);
        setResponse(response, reply);
    }
}
