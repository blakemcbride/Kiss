package org.kissweb.llm;

import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;
import org.kissweb.RestClient;

import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.URI;
import java.time.Duration;
import java.util.Base64;
import java.util.function.Consumer;
import java.util.stream.Stream;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Client for interacting with Anthropic's Claude API.
 *
 * <p>This class provides a simple interface for sending chat completion requests to Anthropic's
 * Messages API, supporting both text and image inputs as well as streaming responses.
 * It handles authentication, request construction, and response parsing.</p>
 *
 * <h2>Features</h2>
 * <ul>
 *   <li>Text and multimodal (text + image) chat completions</li>
 *   <li>Streaming and non-streaming response modes</li>
 *   <li>Configurable generation parameters (temperature, top-p sampling, max tokens)</li>
 *   <li>Built-in retry logic and timeout handling</li>
 * </ul>
 *
 * <h2>Basic Usage Example</h2>
 * <pre>{@code
 * // Initialize the client
 * Anthropic ai = new Anthropic("your-api-key", "claude-sonnet-4-20250514");
 *
 * // Simple text query
 * String response = ai.send("What is the capital of France?");
 * System.out.println(response);
 *
 * // Query with image
 * String answer = ai.send("What's in this image?", "/path/to/image.jpg");
 *
 * // Streaming response
 * ai.stream("Tell me a story",
 *     token -> System.out.print(token),  // Handle each token
 *     () -> System.out.println("\nDone!") // Called when complete
 * );
 * }</pre>
 *
 * <h2>Advanced Configuration</h2>
 * <pre>{@code
 * Anthropic ai = new Anthropic(apiKey, "claude-haiku-4-20250514");
 * ai.setTemperature(0.9f);  // More creative responses
 * ai.setSampling(0.95f);    // Broader vocabulary selection
 * ai.setMaxTokens(8192);    // Longer responses
 * }</pre>
 *
 * @author Kiss Web Development Framework
 * @see RestClient
 */
public class Anthropic {

    private static String ANTHROPIC_URL = "https://api.anthropic.com/v1/messages";
    private static String ANTHROPIC_VERSION = "2023-06-01";

    private final String apiKey; // Your Anthropic API key
    private final String model;  // e.g. "claude-sonnet-4-20250514"

    private float temperature = 0.7f; // Generation randomness
    private float topP = 0.7f;        // Nucleus sampling
    private int maxTokens = 4096;     // Required by Anthropic API

    private JSONObject lastResponse;  // Full JSON of last non-stream call
    private final RestClient restClient; // Re-used HTTP helper

    /**
     * Overrides the Anthropic Messages API endpoint URL used by this class.
     *
     * <p>This is a global (static) setting: changing the URL affects all current and future
     * {@link Anthropic} instances created in this JVM.</p>
     *
     * <p>This is primarily intended for testing, proxies, gateways, or Anthropic-compatible
     * endpoints.</p>
     *
     * @param url the full URL to use for Messages API requests (for example,
     *            {@code https://api.anthropic.com/v1/messages})
     * @return this {@link Anthropic} client to allow fluent call chaining
     */
    public static Anthropic setUrl(String url) {
        ANTHROPIC_URL = url;
        return this;
    }

    /**
     * Overrides the Anthropic API version header value sent with requests.
     *
     * <p>This is a global (static) setting: changing the version affects all current and future
     * {@link Anthropic} instances created in this JVM.</p>
     *
     * <p>The version is sent as the {@code anthropic-version} request header. Use this if you need
     * to pin to a specific API contract or test against a different version.</p>
     *
     * @param version the API version string to send (for example, {@code 2023-06-01})
     * @return this {@link Anthropic} client to allow fluent call chaining
     */
    public static Anthropic setVersion(String version) {
        ANTHROPIC_VERSION = version;
        return this;
    }

    /**
     * Creates a new Anthropic client instance.
     *
     * @param apiKey Your Anthropic API key for authentication
     * @param model  The model identifier (e.g., "claude-sonnet-4-20250514", "claude-haiku-4-20250514")
     * @throws NullPointerException if apiKey or model is null
     */
    public Anthropic(String apiKey, String model) {
        this.apiKey = apiKey;
        this.model = model;

// Configure a reusable RestClient
        this.restClient = new RestClient();
        this.restClient.setTimeouts(Duration.ofSeconds(60), Duration.ofMinutes(5));
        this.restClient.setRetryPolicy(3, 500);
    }

    /* ----------------------------------------------------------------------
     * Configuration setters
     * ---------------------------------------------------------------------- */

    /**
     * Sets the temperature parameter for response generation.
     *
     * <p>Temperature controls the randomness of the model's output. Lower values make
     * the output more focused and deterministic, while higher values make it more
     * diverse and creative.</p>
     *
     * @param temperature Value between 0.0 and 1.0 (default: 0.7)
     *                    <ul>
     *                      <li>0.0-0.3: Very focused, deterministic responses</li>
     *                      <li>0.4-0.7: Balanced creativity and coherence</li>
     *                      <li>0.8-1.0: More creative and varied responses</li>
     *                    </ul>
     */
    public void setTemperature(float temperature) {
        this.temperature = temperature;
    }

    /**
     * Sets the top-p (nucleus) sampling parameter.
     *
     * <p>Top-p sampling considers the smallest set of tokens whose cumulative
     * probability exceeds the specified value. This provides an alternative
     * to temperature for controlling randomness.</p>
     *
     * @param topP Value between 0.0 and 1.0 (default: 0.7)
     *             <ul>
     *               <li>0.1: Only very likely tokens</li>
     *               <li>0.5: Moderately likely tokens</li>
     *               <li>0.9: Wide range of tokens</li>
     *               <li>1.0: Consider all tokens</li>
     *             </ul>
     */
    public void setSampling(float topP) {
        this.topP = topP;
    }

    /**
     * Sets the maximum number of tokens to generate in the response.
     *
     * <p>Anthropic's API requires an explicit max_tokens value. Setting this higher
     * allows for longer responses but may increase latency and cost.</p>
     *
     * @param maxTokens The maximum tokens to generate (default: 4096)
     */
    public void setMaxTokens(int maxTokens) {
        this.maxTokens = maxTokens;
    }

    /* ----------------------------------------------------------------------
     * Public helpers – send and stream
     * ---------------------------------------------------------------------- */

    /**
     * Sends a synchronous chat completion request with optional image input.
     *
     * <p>This method blocks until the complete response is received. For long responses
     * or real-time feedback, consider using {@link #stream} instead.</p>
     *
     * <b>Example with image:</b>
     * <pre>{@code
     * String response = ai.send("Describe this chart", "/path/to/chart.png");
     * }</pre>
     *
     * @param query     The text prompt or question to send to the model
     * @param imagePath Path to an image file (JPEG, PNG, GIF, or WebP) to include in the request,
     *                  or {@code null} for text-only requests
     * @return The complete response text from the model
     * @throws Exception if the API request fails, the image cannot be read, or the response
     *                   cannot be parsed
     * @throws java.io.IOException if the image file cannot be read
     * @see #send(String) for text-only requests
     * @see #stream for streaming responses
     */
    public String send(String query, String imagePath) throws Exception {
        StringBuilder answer = new StringBuilder();
        stream(query, imagePath, answer::append, () -> {});
        lastResponse = null; // Streaming mode does not return full JSON in one chunk
        return answer.toString();
    }

    /**
     * Sends a synchronous text-only chat completion request.
     *
     * <p>Convenience method for text-only queries without image input.</p>
     *
     * <b>Example:</b>
     * <pre>{@code
     * String response = ai.send("Explain quantum computing in simple terms");
     * System.out.println(response);
     * }</pre>
     *
     * @param query The text prompt or question to send to the model
     * @return The complete response text from the model
     * @throws Exception if the API request fails or the response cannot be parsed
     * @see #send(String, String) for multimodal requests
     * @see #stream(String, Consumer, Runnable) for streaming responses
     */
    public String send(String query) throws Exception {
        return send(query, null);
    }

    /**
     * Sends a streaming chat completion request with optional image input.
     *
     * <p>This method returns immediately and invokes callbacks as response tokens arrive.
     * Ideal for long responses where you want to show progress to users in real-time.</p>
     *
     * <b>Example with streaming:</b>
     * <pre>{@code
     * StringBuilder response = new StringBuilder();
     * ai.stream("Write a short story", "/path/to/inspiration.jpg",
     *     token -> {
     *         System.out.print(token);        // Display as it arrives
     *         response.append(token);         // Collect full response
     *     },
     *     () -> System.out.println("\n\nStory complete!")
     * );
     * }</pre>
     *
     * @param query     The text prompt or question to send to the model
     * @param imagePath Path to an image file to include, or {@code null} for text-only
     * @param onToken   Callback invoked for each response token as it arrives.
     *                  Tokens may be partial words or punctuation.
     * @param onDone    Callback invoked when the response stream is complete
     * @throws Exception if the API request fails, the image cannot be read, or streaming fails
     * @throws NullPointerException if query, onToken, or onDone is null
     * @see #stream(String, Consumer, Runnable) for text-only streaming
     */
    public void stream(String query,
                       String imagePath,
                       Consumer<String> onToken,
                       Runnable onDone) throws Exception {

        JSONObject body = buildMessagesBody(query, imagePath);

        HttpRequest req = HttpRequest.newBuilder(URI.create(ANTHROPIC_URL))
                .header("x-api-key", apiKey)
                .header("anthropic-version", ANTHROPIC_VERSION)
                .header("Content-Type", "application/json")
                .timeout(Duration.ofMinutes(10))
                .POST(HttpRequest.BodyPublishers.ofString(body.toString()))
                .build();

        HttpResponse<Stream<String>> resp =
                restClient.streamCall(req, HttpResponse.BodyHandlers.ofLines());

        final boolean[] doneFired = {false};

        resp.body()
                .filter(line -> line.startsWith("data: "))
                .map(line -> line.substring(6).trim())
                .forEach(payload -> {
                    if (payload.startsWith("{")) {
                        JSONObject event = new JSONObject(payload);
                        String type = event.has("type") ? event.getString("type") : "";
                        if ("content_block_delta".equals(type)) {
                            JSONObject delta = event.has("delta") ? event.getJSONObject("delta") : null;
                            if (delta != null && "text_delta".equals(delta.has("type") ? delta.getString("type") : "")) {
                                String text = delta.has("text") ? delta.getString("text") : "";
                                if (!text.isEmpty()) onToken.accept(text);
                            }
                        } else if ("message_stop".equals(type)) {
                            if (!doneFired[0]) {
                                doneFired[0] = true;
                                onDone.run();
                            }
                        }
                    }
                });

// Ensure onDone is always called even if message_stop event was not received
        if (!doneFired[0]) {
            onDone.run();
        }
    }

    /**
     * Sends a streaming text-only chat completion request.
     *
     * <p>Convenience method for streaming text-only queries without image input.</p>
     *
     * <b>Example:</b>
     * <pre>{@code
     * ai.stream("Explain the history of computing",
     *     token -> System.out.print(token),
     *     () -> System.out.println("\nExplanation complete.")
     * );
     * }</pre>
     *
     * @param query   The text prompt or question to send to the model
     * @param onToken Callback invoked for each response token as it arrives
     * @param onDone  Callback invoked when the response is complete
     * @throws Exception if the API request fails or streaming fails
     * @throws NullPointerException if any parameter is null
     * @see #stream(String, String, Consumer, Runnable) for multimodal streaming
     */
    public void stream(String query,
                       Consumer<String> onToken,
                       Runnable onDone) throws Exception {
        stream(query, null, onToken, onDone);
    }

    /* ----------------------------------------------------------------------
     * Diagnostics helpers
     * ---------------------------------------------------------------------- */

    /**
     * Returns the complete JSON response from the last non-streaming API call.
     *
     * <p>Useful for debugging or accessing additional metadata such as token usage.
     * Returns {@code null} for streaming calls or if no non-streaming call has been
     * made yet.</p>
     *
     * <b>Response Structure Example:</b>
     * <pre>{@code
     * {
     *   "id": "msg_...",
     *   "type": "message",
     *   "role": "assistant",
     *   "model": "claude-sonnet-4-20250514",
     *   "usage": {
     *     "input_tokens": 25,
     *     "output_tokens": 50
     *   },
     *   "content": [{"type": "text", "text": "..."}]
     * }
     * }</pre>
     *
     * @return The full JSON response object, or {@code null} if not available
     * @see #getResponseCode() for HTTP status code
     * @see #getResponseString() for raw response text
     */
    public JSONObject getLastFullResponse() {
        return lastResponse;
    }

    /**
     * Returns the HTTP response code from the last API call.
     *
     * <p>Common response codes:</p>
     * <ul>
     *   <li>200 - Success</li>
     *   <li>400 - Bad request (invalid parameters)</li>
     *   <li>401 - Invalid API key</li>
     *   <li>429 - Rate limit exceeded</li>
     *   <li>500 - Server error</li>
     *   <li>529 - Overloaded</li>
     * </ul>
     *
     * @return The HTTP status code from the most recent API call, or 0 if no call has been made
     */
    public int getResponseCode() {
        return restClient.getResponseCode();
    }

    /**
     * Returns the raw response string from the last API call.
     *
     * <p>Provides access to the complete HTTP response body as received from the server.
     * Useful for debugging when response parsing fails or for accessing error details.</p>
     *
     * @return The raw response text, or {@code null} if no response is available
     * @see #getLastFullResponse() for parsed JSON response
     */
    public String getResponseString() {
        return restClient.getResponseString();
    }

    /* ----------------------------------------------------------------------
     * Private helper to build the payload
     * ---------------------------------------------------------------------- */

    /**
     * Builds the JSON request body for the Anthropic Messages API call.
     *
     * <p>Constructs a properly formatted request including model selection,
     * generation parameters, and message content with optional image data.
     * Always sets {@code "stream": true} since the send methods delegate to stream.</p>
     *
     * @param query     The user's text prompt
     * @param imagePath Optional path to an image file to include
     * @return A JSONObject containing the complete request body
     * @throws Exception if image file cannot be read or encoded
     */
    private JSONObject buildMessagesBody(String query, String imagePath) throws Exception {
        JSONObject body = new JSONObject()
                .put("model", model)
                .put("max_tokens", maxTokens)
                .put("temperature", temperature)
                .put("top_p", topP)
                .put("stream", true);

// Assemble user message content array
        JSONArray contentArray = new JSONArray()
                .put(new JSONObject()
                        .put("type", "text")
                        .put("text", query));

        if (imagePath != null) {
            byte[] imageBytes = Files.readAllBytes(Paths.get(imagePath));
            String base64Data = Base64.getEncoder().encodeToString(imageBytes);
            String mediaType = detectMediaType(imagePath);

            contentArray.put(new JSONObject()
                    .put("type", "image")
                    .put("source", new JSONObject()
                            .put("type", "base64")
                            .put("media_type", mediaType)
                            .put("data", base64Data)));
        }

        JSONArray messages = new JSONArray()
                .put(new JSONObject()
                        .put("role", "user")
                        .put("content", contentArray));

        body.put("messages", messages);
        return body;
    }

    /**
     * Detects the MIME media type from an image file path based on its extension.
     *
     * @param imagePath The file path of the image
     * @return The media type string (e.g., "image/jpeg"), defaulting to "image/jpeg" if unknown
     */
    private static String detectMediaType(String imagePath) {
        String lower = imagePath.toLowerCase();
        if (lower.endsWith(".png"))
            return "image/png";
        if (lower.endsWith(".gif"))
            return "image/gif";
        if (lower.endsWith(".webp"))
            return "image/webp";
        // Default covers .jpg and .jpeg
        return "image/jpeg";
    }
}
