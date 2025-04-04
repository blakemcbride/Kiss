package org.kissweb;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONObject;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;

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
    public String send(String query) {
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

        try {
            URL url = new URL(OpenAIURL);
            HttpURLConnection conn = (HttpURLConnection) url.openConnection();

            conn.setRequestMethod("POST");
            conn.setDoOutput(true);
            conn.setRequestProperty("Content-Type", "application/json");
            conn.setRequestProperty("Authorization", "Bearer " + apiKey);

            try (OutputStream os = conn.getOutputStream()) {
                os.write(request.toString().getBytes(StandardCharsets.UTF_8));
            }

            //int responseCode = conn.getResponseCode();

            StringBuilder response = new StringBuilder();
            try (BufferedReader in = new BufferedReader(new InputStreamReader(conn.getInputStream()))) {
                String inputLine;
                while ((inputLine = in.readLine()) != null)
                    response.append(inputLine).append("\n");
            }
            lastResponse = new JSONObject(response.toString());
            return lastResponse.getJSONArray("choices").getJSONObject(0).getJSONObject("message").getString("content");
        } catch (Exception e) {
            logger.error(e);
            lastResponse = null;
            return null;
        }
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
