package org.kissweb.llm;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;
import org.kissweb.RestClient;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

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
        String str = restClient.strCall("POST", URL + "generate", new JSONObject().put("model", model).put("prompt", prompt).toString());
        str = str.replaceAll("\\}\\{", "},{");
        JSONArray ja = new JSONArray("[" + str + "]");
        StringBuilder sb = new StringBuilder();
        final int len = ja.length();
        for (int i = 0; i < len ; i++)
            sb.append(ja.getJSONObject(i).getString("response"));
        return sb.toString();
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
        return chat(messages, null);
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
        JSONArray messageArray = new JSONArray();
        if (messages != null) {
            for (Map<String, String> message : messages) {
                JSONObject msg = new JSONObject();
                for (Map.Entry<String, String> entry : message.entrySet())
                    msg.put(entry.getKey(), entry.getValue());
                messageArray.put(msg);
            }
        }
        JSONObject body = new JSONObject();
        body.put("model", model);
        body.put("messages", messageArray);
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
