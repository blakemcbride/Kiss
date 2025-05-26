package org.kissweb.llm;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;
import org.kissweb.RestClient;

/**
 * This class provides an interface to the OpenAI chat API.
 */
public class OpenAI {

    private static final Logger logger = Logger.getLogger(OpenAI.class);
    private static final String OpenAIURL = "https://api.openai.com/v1/chat/completions";

    private final String apiKey;  // your API key
    private final String model;
    private float temperature = 0.7f;
    private JSONObject lastResponse;
    private RestClient restClient;

    /**
     * Initialize an interface to OpenAI.
     *
     * @param apiKey from OpenAI
     * @param model "gpt-4-turbo" or "gpt-3.5-turbo", "gpt-4o", "o1", etc.
     */
    public OpenAI(String apiKey, String model) {
        this.apiKey = apiKey;
        this.model = model;
    }

    /**
     * The temperature is a value between 0 and 2 that controls the randomness of the response.<br>
     * For example:<br>
     * 0.0	Deterministic: Always picks the most likely next word;	Math, logic, programming, technical writing<br>
     * 0.5	Balanced: Some variation, still coherent;	General conversation, articles<br>
     * 0.7	Default: More creative and varied;	Brainstorming, casual writing<br>
     * 1.0+	Highly creative or random;	Poetry, fiction, jokes, character voices<br>
     * 1.5–2.0	Often incoherent or silly;	Experimental or absurdist tasks<br>
     *
     *
     * @param temperature the temperature value (0.0 to 2.0) that controls randomness in the response
     */
    public void setTemperature(float temperature) {
        this.temperature = temperature;
    }

    /**
     * Send a query to OpenAI and receive a response.
     *
     * @param query the user query to send to OpenAI
     * @return the response content from OpenAI
     * @throws Exception if model is not set or communication fails
     */
    public String send(String query) throws Exception {
        if (model == null || model.isEmpty())
            throw new Exception("Model not set");
        restClient = new RestClient();
        JSONObject request = new JSONObject();
        request.put("model", model);
        request.put("temperature", temperature);
        JSONArray messages = new JSONArray();

        JSONObject message = new JSONObject();
        message.put("role", "system");
        message.put("content", "You are a helpful assistant.");
        messages.put(message);

        message = new JSONObject();
        message.put("role", "user");
        message.put("content", query);
        messages.put(message);

        request.put("messages", messages);

        JSONObject headers = new JSONObject();
        headers.put("Content-Type", "application/json");
        headers.put("Authorization", "Bearer " + apiKey);
        lastResponse = restClient.jsonCall("POST", OpenAIURL, request.toString(), headers);
        return lastResponse.getJSONArray("choices").getJSONObject(0).getJSONObject("message").getString("content");
    }

    /**
     * Get the embeddings for the given text by calling the OpenAI REST API.
     * Embedding are generally model specific.
     *
     * @param text The input text for which embeddings are requested.
     * @return A vector representing the embeddings, or an empty vector if an error occurs.
     * @throws Exception if model is not set or communication fails
     */
    public double [] getEmbeddings(String text) throws Exception {
        if (model == null || model.isEmpty())
            throw new Exception("Model not set");
        restClient = new RestClient();
        JSONObject request = new JSONObject();
        request.put("input", text);
        request.put("model", model);
        JSONObject headers = new JSONObject();
        headers.put("Content-Type", "application/json");
        headers.put("Authorization", "Bearer " + apiKey);
        lastResponse = restClient.jsonCall("POST", "https://api.openai.com/v1/embeddings", request.toString(), headers);
        JSONArray arry = lastResponse.getJSONArray("data").getJSONObject(0).getJSONArray("embedding");
        double [] result = new double[arry.length()];
        for (int i = 0; i < arry.length(); i++)
            result[i] = arry.getDouble(i);
        return result;
    }

    /**
     * Get the full JSON response from the last OpenAI request.
     *
     * @return the full JSON response from the last request
     */
    public JSONObject getLastFullResponse() {
        return lastResponse;
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
