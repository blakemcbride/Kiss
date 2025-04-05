package org.kissweb;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

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
     * @param temperature
     */
    public void setTemperature(float temperature) {
        this.temperature = temperature;
    }

    /**
     * Send a query to OpenAI and receive a response.
     *
     * @param query
     * @return
     */
    public String send(String query) throws Exception {
        if (model == null || model.isEmpty())
            throw new Exception("Model not set");
        RestClient rc = new RestClient();
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
        lastResponse = rc.jsonCall("POST", OpenAIURL, request.toString(), headers);
        return lastResponse.getJSONArray("choices").getJSONObject(0).getJSONObject("message").getString("content");
    }

    /**
     * Get the full JSON response from the last OpenAI request.
     *
     * @return
     */
    public JSONObject getLastFullResponse() {
        return lastResponse;
    }

}
