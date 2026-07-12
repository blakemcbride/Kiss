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
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Client for interacting with the OpenRouter API.
 *
 * <p>OpenRouter (<a href="https://openrouter.ai">openrouter.ai</a>) is a cloud gateway that
 * provides access to models from many providers (OpenAI, Anthropic, Google, Meta, Mistral,
 * and others) through a single, OpenAI-compatible chat completions API and a single API key.
 * Models are identified with a {@code provider/model} string (for example,
 * {@code "openai/gpt-4o"}, {@code "anthropic/claude-sonnet-4"},
 * {@code "meta-llama/llama-3.1-70b-instruct"}).</p>
 *
 * <p>This class provides a simple interface for sending chat completion requests through
 * OpenRouter, supporting both text and image inputs as well as streaming responses.
 * It handles authentication, request construction, and response parsing.</p>
 *
 * <h2>Features</h2>
 * <ul>
 *   <li>Access to many model providers through one API key</li>
 *   <li>Text and multimodal (text + image) chat completions</li>
 *   <li>Streaming and non-streaming response modes</li>
 *   <li>Model fallback routing (alternate models tried automatically if the primary fails)</li>
 *   <li>Unified reasoning-effort control across providers that support reasoning</li>
 *   <li>Configurable generation parameters (temperature, top-p sampling)</li>
 *   <li>Built-in retry logic and timeout handling</li>
 * </ul>
 *
 * <h2>Basic Usage Example</h2>
 * <pre>{@code
 * // Initialize the client
 * OpenRouter ai = new OpenRouter("your-api-key", "openai/gpt-4o");
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
 * OpenRouter ai = new OpenRouter(apiKey, "anthropic/claude-sonnet-4");
 * ai.setTemperature(0.9f);   // More creative responses
 * ai.setSampling(0.95f);     // Broader vocabulary selection
 * ai.setReasoningEffort("high");  // Enable reasoning on models that support it
 * ai.setFallbackModels("openai/gpt-4o", "meta-llama/llama-3.1-70b-instruct");
 * ai.setSiteUrl("https://example.com");  // Optional attribution headers
 * ai.setSiteName("My Application");
 * }</pre>
 *
 * @author Kiss Web Development Framework
 * @see RestClient
 */
public class OpenRouter {

    private static String OPENROUTER_URL = "https://openrouter.ai/api/v1/chat/completions";

    private final String apiKey; // Your OpenRouter API key
    private final String model;  // e.g. "openai/gpt-4o"

    private float temperature = 0.7f; // Generation randomness
    private float topP = 0.7f;        // Nucleus sampling
    private String reasoningEffort;   // When set, request reasoning ("low", "medium", "high")
    private String imageDetail = "auto"; // "low", "high", "auto"
    private String siteUrl;           // Optional HTTP-Referer attribution header
    private String siteName;          // Optional X-Title attribution header
    private String[] fallbackModels;  // Optional alternate models tried in order

    private JSONObject lastResponse;  // Full JSON of last non-stream call
    private int lastHttpStatus;       // HTTP status of the last call
    private String lastErrorBody;     // Raw error body of the last failed call, else null
    private final RestClient restClient; // Re-used HTTP helper

    /**
     * Overrides the OpenRouter Chat Completions endpoint URL used by this class.
     *
     * <p>This is a global setting: changing the URL affects all current and future
     * {@link OpenRouter} instances created in this JVM.</p>
     *
     * <p>This is primarily intended for testing, proxies, or gateways.</p>
     *
     * @param url the full URL to use for chat completion requests (for example,
     *            {@code https://openrouter.ai/api/v1/chat/completions})
     */
    public static void setUrl(String url) {
        OPENROUTER_URL = url;
    }

    /**
     * Creates a new OpenRouter client instance.
     *
     * @param apiKey Your OpenRouter API key for authentication
     * @param model  The model identifier in {@code provider/model} form
     *               (e.g., "openai/gpt-4o", "anthropic/claude-sonnet-4",
     *               "meta-llama/llama-3.1-70b-instruct")
     * @throws NullPointerException if apiKey or model is null
     */
    public OpenRouter(String apiKey, String model) {
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
     * @param temperature Value between 0.0 and 2.0 (default: 0.7)
     *                    <ul>
     *                      <li>0.0-0.3: Very focused, deterministic responses</li>
     *                      <li>0.4-0.7: Balanced creativity and coherence</li>
     *                      <li>0.8-1.0: More creative and varied responses</li>
     *                      <li>1.1-2.0: Highly creative but potentially less coherent</li>
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
     * Sets the reasoning effort level for models that support reasoning.
     *
     * <p>OpenRouter normalizes reasoning control across providers: when an effort level
     * is set, it is sent as OpenRouter's unified {@code reasoning} parameter and translated
     * to each provider's native mechanism. It is ignored by models that do not support
     * reasoning. When not set (the default), no reasoning parameter is sent.</p>
     *
     * @param effort The reasoning effort level, or {@code null} to disable:
     *               <ul>
     *                 <li>"low" - Quick reasoning, faster responses</li>
     *                 <li>"medium" - Balanced reasoning and speed</li>
     *                 <li>"high" - Thorough reasoning, slower but more accurate</li>
     *               </ul>
     */
    public void setReasoningEffort(String effort) {
        this.reasoningEffort = effort;
    }

    /**
     * Sets the detail level for image analysis.
     *
     * <p>Controls how the model processes images. Higher detail levels provide
     * better accuracy but consume more tokens and may be slower.</p>
     *
     * @param detail The image processing detail level:
     *               <ul>
     *                 <li>"low" - Faster processing, lower token usage</li>
     *                 <li>"high" - Better accuracy, higher token usage</li>
     *                 <li>"auto" - Model automatically chooses based on image (default)</li>
     *               </ul>
     */
    public void setImageDetail(String detail) {
        this.imageDetail = detail;
    }

    /**
     * Sets alternate models for OpenRouter's fallback routing.
     *
     * <p>When set, the list is sent as OpenRouter's {@code models} parameter: if the
     * primary model (given in the constructor) is unavailable or the request to it
     * fails, OpenRouter automatically retries with each alternate model in order.
     * When not set (the default), only the primary model is used.</p>
     *
     * @param models Alternate model identifiers to try in order (e.g., "openai/gpt-4o",
     *               "meta-llama/llama-3.1-70b-instruct"); pass no arguments to clear
     */
    public void setFallbackModels(String... models) {
        this.fallbackModels = (models == null || models.length == 0) ? null : models;
    }

    /**
     * Sets the URL of your application, sent as the optional {@code HTTP-Referer} header.
     *
     * <p>OpenRouter uses this header (together with {@link #setSiteName}) to attribute
     * requests to your application in its usage statistics and rankings. It is entirely
     * optional and has no effect on request processing.</p>
     *
     * @param url Your application's URL, or {@code null} to omit the header (default)
     */
    public void setSiteUrl(String url) {
        this.siteUrl = url;
    }

    /**
     * Sets the name of your application, sent as the optional {@code X-Title} header.
     *
     * <p>OpenRouter uses this header (together with {@link #setSiteUrl}) to attribute
     * requests to your application in its usage statistics and rankings. It is entirely
     * optional and has no effect on request processing.</p>
     *
     * @param name Your application's name, or {@code null} to omit the header (default)
     */
    public void setSiteName(String name) {
        this.siteName = name;
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

        JSONObject body = buildChatBody(query, imagePath);

        HttpRequest.Builder builder = HttpRequest.newBuilder(URI.create(OPENROUTER_URL))
                .header("Authorization", "Bearer " + apiKey)
                .header("Content-Type", "application/json")
                .timeout(Duration.ofMinutes(10))
                .POST(HttpRequest.BodyPublishers.ofString(body.toString()));
        if (siteUrl != null && !siteUrl.isEmpty())
            builder.header("HTTP-Referer", siteUrl);
        if (siteName != null && !siteName.isEmpty())
            builder.header("X-Title", siteName);
        HttpRequest req = builder.build();

        lastHttpStatus = 0;
        lastErrorBody = null;

        HttpResponse<Stream<String>> resp =
                restClient.streamCall(req, HttpResponse.BodyHandlers.ofLines());

        lastHttpStatus = resp.statusCode();
        if (lastHttpStatus / 100 != 2) {
            // Errors arrive as a plain JSON body, not an SSE stream
            lastErrorBody = resp.body().collect(Collectors.joining("\n"));
            throw new Exception("OpenRouter request failed with HTTP " + lastHttpStatus + ": " + lastErrorBody);
        }

        final boolean[] doneFired = {false};

// OpenRouter interleaves SSE comment lines (": OPENROUTER PROCESSING") as keep-alives;
// the "data: " filter skips them.
        resp.body()
                .filter(line -> line.startsWith("data: "))
                .map(line -> line.substring(6).trim())
                .forEach(payload -> {
                    if ("[DONE]".equals(payload)) {
                        if (!doneFired[0]) {
                            doneFired[0] = true;
                            onDone.run();
                        }
                        return;
                    }
                    if (payload.startsWith("{")) {
                        JSONObject chunk = new JSONObject(payload);
                        if (chunk.has("error")) {
                            // A failure can also be reported mid-stream as an SSE error event
                            lastErrorBody = chunk.getJSONObject("error").toString();
                            throw new RuntimeException("OpenRouter returned an error: " + lastErrorBody);
                        }
                        JSONArray choices = chunk.has("choices") ? chunk.getJSONArray("choices") : null;
                        if (choices == null || choices.length() == 0)
                            return; // e.g. a final usage-only chunk
                        JSONObject delta = choices.getJSONObject(0).getJSONObject("delta");
                        if (delta.has("content") && !delta.isNull("content")) {
                            String text = delta.getString("content");
                            if (!text.isEmpty())
                                onToken.accept(text);
                        }
                    }
                });

// Ensure onDone is always called even if the [DONE] sentinel was not received
        if (!doneFired[0])
            onDone.run();
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
     * <p>Useful for debugging or accessing additional metadata not included in the
     * simplified response. Returns {@code null} for streaming calls or if no
     * non-streaming call has been made yet.</p>
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
     *   <li>402 - Insufficient credits</li>
     *   <li>429 - Rate limit exceeded</li>
     *   <li>502 - The selected model/provider is down or returned an invalid response</li>
     *   <li>503 - No available provider meets the request's requirements</li>
     * </ul>
     *
     * @return The HTTP status code from the most recent API call, or 0 if no call has been made
     */
    public int getResponseCode() {
        return lastHttpStatus != 0 ? lastHttpStatus : restClient.getResponseCode();
    }

    /**
     * Returns the raw error body from the last failed API call.
     *
     * <p>When a call fails (non-2xx HTTP status, or an error event received mid-stream),
     * the raw error text returned by OpenRouter is retained here for debugging.
     * The same text is also included in the thrown exception's message.</p>
     *
     * @return The raw error text from the last failed call, or {@code null} if the
     *         last call succeeded or no call has been made
     * @see #getResponseCode() for HTTP status code
     */
    public String getResponseString() {
        return lastErrorBody != null ? lastErrorBody : restClient.getResponseString();
    }

    /* ----------------------------------------------------------------------
     * Private helper to build the payload
     * ---------------------------------------------------------------------- */

    /**
     * Builds the JSON request body for chat completion API calls.
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
    private JSONObject buildChatBody(String query,
                                     String imagePath) throws Exception {
        JSONObject body = new JSONObject()
                .put("model", model)
                .put("stream", true)
                .put("temperature", temperature)
                .put("top_p", topP);

        if (reasoningEffort != null && !reasoningEffort.isEmpty())
            body.put("reasoning", new JSONObject().put("effort", reasoningEffort));

        if (fallbackModels != null) {
            JSONArray models = new JSONArray();
            for (String m : fallbackModels)
                models.put(m);
            body.put("models", models);
        }

// Assemble user message content
        JSONArray contentArray = new JSONArray()
                .put(new JSONObject()
                        .put("type", "text")
                        .put("text", query));

        if (imagePath != null) {
            byte[] imageBytes = Files.readAllBytes(Paths.get(imagePath));
            String base64Image = Base64.getEncoder().encodeToString(imageBytes);
            String imageDataUrl = "data:" + detectMediaType(imagePath) + ";base64," + base64Image;

            JSONObject imageObj = new JSONObject().put("url", imageDataUrl);
            if (imageDetail != null && !imageDetail.isEmpty())
                imageObj.put("detail", imageDetail); // Only include if caller supplied

            contentArray.put(new JSONObject()
                    .put("type", "image_url")
                    .put("image_url", imageObj));
        }

        JSONArray messages = new JSONArray()
                .put(new JSONObject()
                        .put("role", "system")
                        .put("content", "You are a helpful assistant."))
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
