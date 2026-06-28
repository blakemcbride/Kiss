package org.kissweb.llm;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;
import org.kissweb.RestClient;

import java.io.*;
import java.net.URI;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.regex.Pattern;
import java.util.stream.Stream;

/**
 * Provides an interface to the Ollama AI server.
 *
 * @see <a href="https://ollama.com">Ollama project</a>
 */
public class Ollama {

    private static final Logger logger = LogManager.getLogger(Ollama.class);
    private static final Pattern thinkPattern = Pattern.compile("(?s).*<think>(.*?)</think>(.*)"); // for integration with ollama
    private String URL = "http://localhost:11434/api/";
    private String model = null;
    private RestClient restClient;

    /**
     * Create a new Ollama client using the default local server.
     */
    public Ollama() {}

    /**
     * Create a new Ollama client with a specified Ollama server.
     *
     * @param urlOrModel Either the URL of the Ollama server, like "http://localhost:11434" or the name of a model, like "llama-2-7b"
     */
    public Ollama(String urlOrModel) {
        if (urlOrModel.startsWith("http"))
            this.URL = (urlOrModel.endsWith("/") ? urlOrModel : urlOrModel + "/") + "api/";
        else
            this.model = urlOrModel;
    }

    /**
     * Create a new Ollama client with a specified Ollama server and model.
     *
     * @param url The URL of the Ollama server, like "http://localhost:11434"
     * @param model The name of the model, like "llama-2-7b"
     */
    public Ollama(String url, String model) {
        this.URL = (url.endsWith("/") ? url : url + "/") + "api/";
        this.model = model;
    }

    /**
     * Check if the Ollama server is up and running.
     *
     * @return true if the server is up and running, false otherwise.
     */
    public boolean isOllamaUp() {
        try {
            restClient = new RestClient();
            restClient.strCall("GET", URL + "version");
            if (restClient.getResponseCode() == 200)
                return true;
        } catch (Exception e) {
            // ignore
        }
        return false;
    }

    /**
     * Returns a list of available Ollama models.
     *
     * @return List of model names (as Strings). If the server is unreachable or an error occurs, an empty list is returned.
     * @throws IOException if connection to Ollama server fails
     */
    public List<String> getAvailableModels() throws IOException {
        List<String> models = new ArrayList<>();
        restClient = new RestClient();
        JSONObject json = restClient.jsonCall("GET", URL + "tags");
        JSONArray jsonModels = json.getJSONArray("models");
        for (int i=0 ; i < jsonModels.length() ; i++)
            models.add(jsonModels.getJSONObject(i).getString("model"));
        return models;
    }

    /**
     * Send a prompt to the Ollama server and receive a response using the previously selected model.
     *
     * @param prompt the text prompt to send
     * @return the response from the model
     * @throws Exception if model is not set or communication fails
     */
    public String send(String prompt) throws Exception {
        if (model == null || model.isEmpty())
            throw new Exception("Model not set");
        restClient = new RestClient();
        //  Request a non-streaming response so Ollama returns a single JSON object.  Without
        //  "stream": false, /api/generate streams newline-delimited JSON ({...}\n{...}\n...),
        //  which is not a parseable single document.
        String str = restClient.strCall("POST", URL + "generate",
                new JSONObject().put("model", model).put("prompt", prompt).put("stream", false).toString());
        if (str == null)
            return "";
        JSONObject responseJson = new JSONObject(str);
        return responseJson.getString("response", "");
    }

    /**
     * Streaming form of {@link #send(String)}: POST to <code>/api/generate</code> with
     * <code>stream:true</code> and hand each response chunk to <code>onToken</code> as the model
     * produces it.
     * <br><br>
     * The complete text is also assembled and returned, so a caller that wants both the live
     * stream and the final string needs only this one call.  For a simple, non-streaming call use
     * {@link #send(String)}.
     *
     * @param prompt the text prompt to send
     * @param onToken receives each text chunk, in order, as it arrives; may be <code>null</code> to
     *                only assemble and return the result
     * @return the complete response text
     * @throws Exception if model is not set or communication fails
     */
    public String send(String prompt, Consumer<String> onToken) throws Exception {
        if (model == null || model.isEmpty())
            throw new Exception("Model not set");
        JSONObject body = new JSONObject();
        body.put("model", model);
        body.put("prompt", prompt);
        body.put("stream", true);
        return streamRequest("generate", body, o -> o.getString("response", ""), onToken);
    }

    /**
     * Send a multi-turn conversation to the Ollama server and receive the assistant's reply,
     * using the previously selected model.
     * <br><br>
     * This posts to Ollama's <code>/api/chat</code> endpoint with a structured
     * <code>messages</code> array, rather than to <code>/api/generate</code> with a hand-built
     * prompt string (as {@link #send(String)} does).  The chat endpoint is the better design:
     * <ul>
     *   <li><b>Correct chat templating.</b> Each instruct model ships its own chat template with
     *       special tokens (Llama&nbsp;3's <code>&lt;|start_header_id|&gt;</code>, Mistral's
     *       <code>[INST]…[/INST]</code>, etc.).  <code>/api/chat</code> applies the model's
     *       registered template automatically, so role separation and instruction-following are
     *       not degraded by emitting a format the model was not trained on.</li>
     *   <li><b>Clean role separation.</b> System / user / assistant become explicit fields rather
     *       than text the model must parse out of one blob.</li>
     *   <li><b>Tool-calling foundation.</b> <code>/api/chat</code> is also where Ollama exposes the
     *       <code>tools</code> parameter for function calling (see
     *       {@link #chat(List, JSONArray)}), which an agentic tool-calling loop requires.</li>
     * </ul>
     *
     * @param messages the conversation, in order.  Each element is a map describing one message;
     *                 it must contain a <code>"role"</code> (typically <code>"system"</code>,
     *                 <code>"user"</code>, or <code>"assistant"</code>) and <code>"content"</code>.
     *                 Any additional keys are passed through to Ollama unchanged.
     * @return the assistant message's content, or an empty string if the response carried no message
     * @throws Exception if model is not set or communication fails
     */
    public String chat(List<Map<String, String>> messages) throws Exception {
        return chat(messages, (JSONArray) null);
    }

    /**
     * Send a multi-turn conversation to the Ollama server, optionally advertising tools the model
     * may call, and receive the assistant's reply using the previously selected model.
     * <br><br>
     * This is the tool-calling form of {@link #chat(List)}.  When <code>tools</code> is non-null and
     * non-empty it is included as the <code>tools</code> parameter of the <code>/api/chat</code>
     * request, enabling function calling.  When the model elects to call a tool, the assistant
     * message's <code>content</code> is typically empty and the tool calls appear under the
     * message's <code>tool_calls</code> field; use {@link #getResponseString()} to inspect the full
     * raw response in that case.
     *
     * @param messages the conversation, in order (see {@link #chat(List)} for the map format)
     * @param tools a JSON array of tool/function descriptors to advertise to the model, or null for none
     * @return the assistant message's content, or an empty string if the response carried no message content
     * @throws Exception if model is not set or communication fails
     */
    public String chat(List<Map<String, String>> messages, JSONArray tools) throws Exception {
        if (model == null || model.isEmpty())
            throw new Exception("Model not set");
        restClient = new RestClient();
        JSONObject body = new JSONObject();
        body.put("model", model);
        body.put("messages", toMessageArray(messages));
        body.put("stream", false);
        if (tools != null && tools.length() > 0)
            body.put("tools", tools);
        String str = restClient.strCall("POST", URL + "chat", body.toString());
        if (str == null)
            return null;
        JSONObject responseJson = new JSONObject(str);
        JSONObject message = responseJson.getJSONObject("message");
        if (message == null)
            return "";
        return message.getString("content", "");
    }

    /**
     * Streaming form of {@link #chat(List)}: POST to <code>/api/chat</code> with
     * <code>stream:true</code> and hand each assistant content chunk to <code>onToken</code> as it
     * arrives, returning the full assembled reply.
     * <br><br>
     * Like {@link #send(String, Consumer)}, the complete reply is returned as well, so one call
     * serves both the live token stream and the final text.  For a non-streaming call use
     * {@link #chat(List)}.
     *
     * @param messages the conversation, in order (see {@link #chat(List)} for the map format)
     * @param onToken receives each content chunk, in order, as it arrives; may be <code>null</code>
     * @return the complete assistant reply text
     * @throws Exception if model is not set or communication fails
     */
    public String chat(List<Map<String, String>> messages, Consumer<String> onToken) throws Exception {
        if (model == null || model.isEmpty())
            throw new Exception("Model not set");
        JSONObject body = new JSONObject();
        body.put("model", model);
        body.put("messages", toMessageArray(messages));
        body.put("stream", true);
        return streamRequest("chat", body, o -> {
            JSONObject m = o.getJSONObject("message", false);
            return m == null ? "" : m.getString("content", "");
        }, onToken);
    }

    /**
     * Build the <code>/api/chat</code> messages array from the caller's list of role/content maps.
     * Any keys beyond role and content are passed through to Ollama unchanged.
     *
     * @param messages the conversation, in order (may be null → empty array)
     * @return the JSON messages array
     */
    private static JSONArray toMessageArray(List<Map<String, String>> messages) {
        JSONArray messageArray = new JSONArray();
        if (messages != null) {
            for (Map<String, String> message : messages) {
                JSONObject msg = new JSONObject();
                for (Map.Entry<String, String> entry : message.entrySet())
                    msg.put(entry.getKey(), entry.getValue());
                messageArray.put(msg);
            }
        }
        return messageArray;
    }

    /**
     * Shared engine for the streaming (<code>stream:true</code>) endpoints.  Opens the request,
     * reads Ollama's newline-delimited JSON response one object per line, hands each non-empty text
     * chunk to <code>onToken</code> as it arrives, and returns the full concatenated text once the
     * <code>"done"</code> marker is seen.  Non-JSON keep-alive lines are skipped.
     *
     * @param endpoint the API endpoint name, e.g. "generate" or "chat"
     * @param body the request body (must already contain <code>"stream": true</code>)
     * @param chunk extracts the incremental text from one streamed object (returns "" when none)
     * @param onToken receives each non-empty chunk in arrival order; may be <code>null</code>
     * @return the complete assembled text
     * @throws IOException if the communication fails, or the server reports an error
     */
    private String streamRequest(String endpoint, JSONObject body, Function<JSONObject, String> chunk,
                                 Consumer<String> onToken) throws IOException {
        final HttpRequest req = HttpRequest.newBuilder()
                .uri(URI.create(URL + endpoint))
                .header("Content-Type", "application/json")
                .POST(HttpRequest.BodyPublishers.ofString(body.toString()))
                .build();
        restClient = new RestClient();
        final HttpResponse<Stream<String>> resp = restClient.streamCall(req, HttpResponse.BodyHandlers.ofLines());
        final StringBuilder full = new StringBuilder();
        final Iterator<String> it = resp.body().iterator();
        while (it.hasNext()) {
            final String line = it.next();
            if (line.isEmpty())
                continue;
            final JSONObject o;
            try {
                o = new JSONObject(line);
            } catch (RuntimeException e) {
                continue;   // skip any non-JSON keep-alive line
            }
            if (o.has("error"))
                throw new IOException("Ollama error: " + o.getString("error", ""));
            final String c = chunk.apply(o);
            if (c != null && !c.isEmpty()) {
                full.append(c);
                if (onToken != null)
                    onToken.accept(c);
            }
            if (o.getBoolean("done", false))
                break;
        }
        return full.toString();
    }

    /**
     * Set the model to use for generating text.
     *
     * @param model the model name to use
     */
    public void selectModel(String model) {
        this.model = model;
    }

    /**
     * Convert a string returned from a model to HTML.
     *
     * @param a the string to convert
     * @return the HTML formatted string
     */
    public static String toHtml(String a) {
        if (a == null)
            return "";
        return a.replaceAll("\\\\boxed\\{(.*?)}", "$1")
                .replaceAll("\\\\", "")
                .replaceAll("\n", "<br>")
                .replaceAll("\\*{2}(.*?)\\*{2}", "<b>$1</b>");
    }

    /**
     * Gets the embeddings for the given text by calling the Ollama REST API.
     * Embedding are generally model specific.
     *
     * @param model The model to use for generating embeddings.
     * @param text The input text for which embeddings are requested.
     * @return A vector representing the embeddings, or an empty vector if an error occurs.
     * @throws Exception if model is not set or communication fails
     */
    public double[] getEmbeddings(String model, String text) throws Exception {
        if (model == null || model.isEmpty())
            throw new Exception("Model not set");
        restClient = new RestClient();
        String str = restClient.strCall("POST", URL + "embed", new JSONObject().put("model", model).put("input", text).toString());
        if (str == null)
            return new double[0];
        JSONObject responseJson = new JSONObject(str);
        JSONArray embeddingsOuter = responseJson.getJSONArray("embeddings");
        if (embeddingsOuter == null) {
            // "embeddings" not present or not an array
            return new double[0];
        }

        // Convert the 2D JSON array into a 2D double array.
        double[][] embeddings = new double[embeddingsOuter.length()][];
        for (int i = 0; i < embeddingsOuter.length(); i++) {
            JSONArray innerArray = embeddingsOuter.getJSONArray(i);
            double[] row = new double[innerArray.length()];
            for (int j = 0; j < innerArray.length(); j++)
                row[j] = innerArray.getDouble(j);
            embeddings[i] = row;
        }
        return embeddings[0];
    }

    /**
     * Gets the embeddings for the given text by calling the Ollama REST API using the default model.
     * Embedding are generally model specific.
     *
     * @param prompt The input text for which embeddings are requested.
     * @return A vector representing the embeddings, or an empty vector if an error occurs.
     * @throws Exception if model is not set or communication fails
     */
    public double [] getEmbeddings(String prompt) throws Exception {
        if (model == null || model.isEmpty())
            throw new Exception("Model not set");
        return getEmbeddings(model, prompt);
    }

    /**
     * The HTTP response code for the last call
     *
     * @return the HTTP response code
     */
    public int getResponseCode() {
        return restClient == null ? 0 : restClient.getResponseCode();
    }

    /**
     * Returns the response string for the last call.
     *
     * @return the response string for the last call
     */
    public String getResponseString() {
        return restClient == null ? null : restClient.getResponseString();
    }
}
