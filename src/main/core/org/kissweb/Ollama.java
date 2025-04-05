package org.kissweb;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * Provides an interface to the Ollama AI server.
 *
 * @see <a href="https://ollama.com">Ollama project</a>
 */
public class Ollama {

    private static final Logger logger = Logger.getLogger(Ollama.class);
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
     * @param prompt
     * @return
     * @throws Exception
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
     * Set the model to use for generating text.
     *
     * @param model
     */
    public void selectModel(String model) {
        this.model = model;
    }

    /**
     * Convert a string returned from a model to HTML.
     *
     * @param a
     * @return
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
     */
    public double[] getEmbeddings(String model, String text) throws Exception {
        if (model == null || model.isEmpty())
            throw new Exception("Model not set");
        restClient = new RestClient();
        String str = restClient.strCall("POST", URL + "embed", new JSONObject().put("model", model).put("input", text).toString());
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
     */
    public double [] getEmbeddings(String prompt) throws Exception {
        if (model == null || model.isEmpty())
            throw new Exception("Model not set");
        return getEmbeddings(model, prompt);
    }

    /**
     * The HTTP response code for the last call
     *
     * @return
     */
    public int getResponseCode() {
        return restClient == null ? 0 : restClient.getResponseCode();
    }

    /**
     * Returns the response string for the last call.
     *
     * @return
     */
    public String getResponseString() {
        return restClient == null ? null : restClient.getResponseString();
    }
}
