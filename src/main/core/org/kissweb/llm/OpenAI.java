package org.kissweb.llm;

import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;
import org.kissweb.RestClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.URI;
import java.time.Duration;
import java.util.stream.Stream;
import java.util.function.Consumer;

/**
 * This class provides an interface to the OpenAI chat API.
 */
public class OpenAI {

    private static final String OpenAIURL = "https://api.openai.com/v1/chat/completions";

    private final String apiKey;  // your API key
    private final String model;
    private final boolean isReasoning;
    private float temperature = 0.7f;
    private float top_p = 0.7f;
    private String reasoningEffort = "medium";
    private JSONObject lastResponse;
    private RestClient restClient;

    /**
     * Initialize an interface to OpenAI.
     *
     * @param apiKey from OpenAI
     * @param model "gpt-4-turbo" or "gpt-3.5-turbo", "gpt-4o", "o1", etc.
     * @param reasoningModel if true, the selected model reasoning model
     */
    public OpenAI(String apiKey, String model, boolean reasoningModel) {
        this.apiKey = apiKey;
        this.model = model;
        this.isReasoning = reasoningModel;
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
     * Controls the sampling algorithm used by the model to generate text.<br>
     * The algorithm is as follows:<br>
     * 0.0	Selects the most likely next token;	Math, logic, programming, technical writing<br>
     * 0.5	Sampling from the top k tokens;	General conversation, articles<br>
     * 0.7	Default: Samples from the top k tokens;	Brainstorming, casual writing<br>
     * 1.0+	Selects from all tokens;	Poetry, fiction, jokes, character voices<br>
     * <p>
     * This value is used to compute the number of tokens to sample from when generating text.<br>
     * The number of tokens to sample is {@code top_p * tokens.length()}.
     *
     * @param top_p the fraction of tokens to sample from
     */
    public void setSampling(float top_p) {
        this.top_p = top_p;
    }

    /**
     * Set the reasoning effort string.
     * <p>
     * The "reasoning effort" string is a special input that can be passed to the model to give it extra
     * information about the context of the question. For example, if the question is a math problem, the
     * reasoning effort might be "Show me how to solve this math problem". The model will then include the
     * solution in the output.
     *
     * @param effort The reasoning effort string.
     */
    public void setReasoningEffort(String effort) {
        this.reasoningEffort = effort;
    }

    /**
     * Stream a response from the OpenAI model.
     *
     * @param query   the user query
     * @param onToken callback invoked for every partial token
     * @param onDone  callback invoked when the stream ends
     * @throws Exception   if the model is not set or if communication fails
     */
    public void stream(String query,
                       Consumer<String> onToken,
                       Runnable onDone) throws Exception {
        JSONObject body = buildChatBody(query);

        HttpRequest req = HttpRequest.newBuilder(URI.create(OpenAIURL))
                .header("Authorization", "Bearer " + apiKey)
                .header("Content-Type", "application/json")
                .timeout(Duration.ofMinutes(10))
                .POST(HttpRequest.BodyPublishers.ofString(body.toString()))
                .build();

        RestClient client = new RestClient();
        client.setTimeouts(Duration.ofSeconds(60), Duration.ofMinutes(5));

        HttpResponse<Stream<String>> resp = client.streamCall(req, HttpResponse.BodyHandlers.ofLines());

        resp.body()
            .filter(l -> l.startsWith("data: "))
            .map(l -> l.substring(6).trim())
            .forEach(payload -> {
                if ("[DONE]".equals(payload)) { onDone.run(); return; }
                if (payload.startsWith("{")) {
                    JSONObject delta = new JSONObject(payload)
                            .getJSONArray("choices").getJSONObject(0)
                            .getJSONObject("delta");
                    if (delta.has("content"))
                        onToken.accept(delta.getString("content"));
                }
            });
    }

    /**
     * Send a query to OpenAI and receive a response.
     *
     * @param query the user query to send to OpenAI
     * @return the response content from OpenAI
     * @throws Exception if model is not set or communication fails
     */
    public String send(String query) throws Exception {
        StringBuilder answer = new StringBuilder();
        stream(query, answer::append, () -> {});
        // No single "full" JSON in streaming mode
        lastResponse = null;
        restClient   = null;
        return answer.toString();
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

    /**
     * Builds the common request body for chat completions used by both {@link #send} and {@link #stream}.
     *
     * @param query       the user prompt
     * @return a fully populated {@link JSONObject} ready for the HTTP request
     */
    private JSONObject buildChatBody(String query) {
        JSONObject body = new JSONObject();
        body.put("model", model);
        body.put("stream", true);

        if (isReasoning) {
            body.put("reasoning_effort", this.reasoningEffort);
        } else {
            body.put("temperature", temperature);
            body.put("top_p", top_p);
        }

        JSONArray messages = new JSONArray()
                .put(new JSONObject()
                        .put("role", "system")
                        .put("content", "You are a helpful assistant."))
                .put(new JSONObject()
                        .put("role", "user")
                        .put("content", query));

        body.put("messages", messages);
        return body;
    }
}
