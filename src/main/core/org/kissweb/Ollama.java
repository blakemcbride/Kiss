package org.kissweb;

import org.apache.log4j.Logger;
import org.json.JSONArray;
import org.json.JSONException;
import org.json.JSONObject;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import java.io.*;
import java.net.HttpURLConnection;
import java.net.InetSocketAddress;
import java.net.Proxy;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Matcher;
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

    /**
     * Create a new Ollama client using the default local server.
     */
    public Ollama() {}

    /**
     * Create a new Ollama client with a specified Ollama server.
     *
     * @param url like "http://localhost:11434"
     */
    public Ollama(String url) {
        this.URL = (url.endsWith("/") ? url : url + "/") + "api/";
    }

    /**
     * Check if the Ollama server is up and running.
     *
     * @return true if the server is up and running, false otherwise.
     */
    public boolean isOllamaUp() {
        try {
            URL url = new URL(URL + "version");
            HttpURLConnection con = (HttpURLConnection) url.openConnection();
            con.setConnectTimeout(3000);
            con.setReadTimeout(3000);
            con.setRequestMethod("GET");

            if (con.getResponseCode() == 200) {
                // Optionally read and process the version response
                try (BufferedReader in = new BufferedReader(new InputStreamReader(con.getInputStream()))) {
                    StringBuilder response = new StringBuilder();
                    String line;
                    while ((line = in.readLine()) != null)
                        response.append(line);
                    // System.out.println("Ollama version: " + response.toString());
                }
                return true;
            }
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
    public List<String> getAvailableModels() {
        List<String> models = new ArrayList<>();
        String urlStr = URL + "tags";
        HttpURLConnection connection = null;

        try {
            URL url = new URL(urlStr);
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("GET");
            connection.setConnectTimeout(5000); // 5 seconds timeout
            connection.setReadTimeout(5000);    // 5 seconds read timeout

            int responseCode = connection.getResponseCode();
            if (responseCode == HttpURLConnection.HTTP_OK) {
                // Read the response from the server
                BufferedReader in = new BufferedReader(new InputStreamReader(connection.getInputStream()));
                StringBuilder response = new StringBuilder();
                String inputLine;
                while ((inputLine = in.readLine()) != null)
                    response.append(inputLine);
                in.close();

                // Parse the JSON response.
                // Assuming the response is structured like: { "tags": ["model1", "model2", ...] }
                JSONObject json = new JSONObject(response.toString());
                JSONArray jsonModels = json.getJSONArray("models");
                for (int i=0 ; i < jsonModels.length() ; i++)
                    models.add(jsonModels.getJSONObject(i).getString("model"));
            } else {
                //System.err.println("Received non-OK response: " + responseCode);
                return null;
            }
        } catch (Exception e) {
            logger.error(e);
            return null;
        } finally {
            if (connection != null)
                connection.disconnect();
        }
        return models;
    }

    /**
     * Generate text from a prompt using a specified model.
     *
     * @param model The model to use for generating text.
     * @param prompt The prompt to generate text from.
     * @return A JSONObject containing the generated text.
     * @throws IOException
     */
    public JSONObject send(String model, String prompt) throws IOException {
        final String urlStr = URL + "generate";
        HttpURLConnection con = null;
        StringBuilder res = new StringBuilder();
        JSONObject injson = new JSONObject();

        // Add mandatory fields.
        injson.put("model", model);
        injson.put("prompt", prompt);
        String outStr = injson.toString();

        String proxyServerURL = null;
        int proxyServerPort = 0;
        SSLContext context = null;

        try {
            URL url = new URL(urlStr);

            // Proxy server support
            if (proxyServerURL != null && !proxyServerURL.isEmpty()) {
                Proxy proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(proxyServerURL, proxyServerPort));
                con = (HttpURLConnection) url.openConnection(proxy);
            } else
                con = (HttpURLConnection) url.openConnection();

            //  SSL support
            if (context != null) {
                SSLSocketFactory sockFact = context.getSocketFactory();
                ((HttpsURLConnection) con).setSSLSocketFactory(sockFact);
            }

            con.setConnectTimeout(30000);
            con.setReadTimeout(30000);
            con.setUseCaches(false);

            con.setRequestMethod("POST");
            con.setDoInput(true);
            if (outStr != null && !outStr.isEmpty()) {
                con.setDoOutput(true);
                try (OutputStreamWriter out = new OutputStreamWriter(con.getOutputStream())) {
                    out.write(outStr);
                }
            }

            int responseCode = con.getResponseCode();
            InputStream inputStream;

            if (responseCode >= 200 && responseCode < 300)
                inputStream = con.getInputStream();
            else
                inputStream = con.getErrorStream();

            if (inputStream != null) {
                try (BufferedReader in = new BufferedReader(new InputStreamReader(inputStream))) {
                    String line;
                    while ((line = in.readLine()) != null) {
                        // Attempt to parse each chunk as JSON and extract the "response" text.
                        try {
                            JSONObject jsonChunk = new JSONObject(line);
                            res.append(jsonChunk.getString("response"));
                            if (jsonChunk.has("done") && jsonChunk.getBoolean("done"))
                                break;  // Stop when final chunk is received.
                        } catch (JSONException e) {
                            // If the chunk isn’t valid JSON, ignore and continue.
                        }
                    }
                }
            }
        } catch (Exception e) {
            logger.error(e);
        } finally {
            if (con != null)
                con.disconnect();
        }

        String resStr = res.toString();
        Matcher matcher = thinkPattern.matcher(resStr);

        String thinkContent = null;
        String afterThink = resStr; // Default: the entire string if no match is found

        if (matcher.matches()) {
            thinkContent = matcher.group(1).trim();
            afterThink = matcher.group(2).trim();
        }

        JSONObject outjson = new JSONObject();
        outjson.put("think", thinkContent);
        outjson.put("response", afterThink);
        return outjson;
    }

    /**
     * Send a prompt to the Ollama server and receive a response using the previously selected model.
     *
     * @param prompt
     * @return
     * @throws Exception
     */
    public JSONObject send(String prompt) throws Exception {
        if (model == null || model.isEmpty())
            throw new Exception("Model not set");
        return send(model, prompt);
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
     * @return A double array representing the embeddings, or an empty array if an error occurs.
     */
    public double[][] getEmbeddings(String model, String text) {
        final String urlStr = URL + "embed";
        HttpURLConnection connection = null;
        StringBuilder responseBuilder = new StringBuilder();

        try {
            URL url = new URL(urlStr);
            connection = (HttpURLConnection) url.openConnection();
            connection.setRequestMethod("POST");
            connection.setConnectTimeout(5000); // 5 seconds timeout
            connection.setReadTimeout(5000);    // 5 seconds timeout
            connection.setDoOutput(true);
            connection.setRequestProperty("Content-Type", "application/json");

            // Create the JSON request payload
            JSONObject requestJson = new JSONObject();
            requestJson.put("input", text);
            // Optionally, include the model if needed:
            requestJson.put("model", model);
            String requestBody = requestJson.toString();

            // Write the JSON payload to the request body
            try (OutputStream os = connection.getOutputStream()) {
                byte[] inputBytes = requestBody.getBytes(StandardCharsets.UTF_8);
                os.write(inputBytes, 0, inputBytes.length);
            }

            int responseCode = connection.getResponseCode();
            InputStream inputStream;
            if (responseCode >= 200 && responseCode < 300)
                inputStream = connection.getInputStream();
            else
                inputStream = connection.getErrorStream();

            try (BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream, StandardCharsets.UTF_8))) {
                String line;
                while ((line = reader.readLine()) != null)
                    responseBuilder.append(line);
            }

            // Parse the JSON response. We expect a top-level "embeddings" key holding an array of arrays.
            JSONObject responseJson = new JSONObject(responseBuilder.toString());
            JSONArray embeddingsOuter = responseJson.getJSONArray("embeddings");
            if (embeddingsOuter == null) {
                // "embeddings" not present or not an array
                return new double[0][];
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

            return embeddings;
        } catch (Exception e) {
            logger.error(e);
            // Return an empty 2D array if there's any error.
            return new double[0][];

        } finally {
            if (connection != null)
                connection.disconnect();
        }
    }

    /**
     * Gets the embeddings for the given text by calling the Ollama REST API using the default model.
     * Embedding are generally model specific.
     *
     * @param prompt The input text for which embeddings are requested.
     * @return A double array representing the embeddings, or an empty array if an error occurs.
     */
    public double [][] getEmbeddings(String prompt) throws Exception {
        if (model == null || model.isEmpty())
            throw new Exception("Model not set");
        return getEmbeddings(model, prompt);
    }
}
