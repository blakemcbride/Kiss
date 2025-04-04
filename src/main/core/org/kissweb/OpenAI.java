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

public class OpenAI {

    private static final Logger logger = Logger.getLogger(OpenAI.class);
    private static final String OpenAIURL = "https://api.openai.com/v1/chat/completions";

    private final String apiKey;  // your API key
    private final String model;
    private float temperature = 0.7f;
    private JSONObject lastResponse;

    public OpenAI(String apiKey, String model) {
        this.apiKey = apiKey;  // from OpenAI
        this.model = model;    //  "gpt-4-turbo" r gpt-3.5-turbo, gpt-4o, o1, etc.
    }

    public void setTemperature(float temperature) {
        this.temperature = temperature;
    }

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

    public JSONObject getLastFullResponse() {
        return lastResponse;
    }

}
