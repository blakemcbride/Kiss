package org.kissweb.llm;

import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;
import org.kissweb.RestClient;

import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.URI;
import java.time.Duration;
import java.util.Base64;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.function.Consumer;
import java.util.stream.Collectors;
import java.util.stream.Stream;
import java.nio.file.Files;
import java.nio.file.Paths;

/**
 * Client for interacting with OpenAI's Chat and Embeddings APIs.
 * 
 * <p>This class provides a simple interface for sending chat completions requests to OpenAI's API,
 * supporting both text and image inputs, streaming responses, and embeddings generation.
 * It handles authentication, request construction, and response parsing.</p>
 * 
 * <h2>Features</h2>
 * <ul>
 *   <li>Text and multimodal (text + image) chat completions</li>
 *   <li>Streaming and non-streaming response modes</li>
 *   <li>Support for reasoning models (o1, o3, etc.) with reasoning effort control</li>
 *   <li>Text embeddings generation</li>
 *   <li>Configurable generation parameters (temperature, top-p sampling)</li>
 *   <li>Built-in retry logic and timeout handling</li>
 * </ul>
 * 
 * <h2>Basic Usage Example</h2>
 * <pre>{@code
 * // Initialize the client
 * OpenAI ai = new OpenAI("your-api-key", "gpt-4o", false);
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
 * OpenAI ai = new OpenAI(apiKey, "gpt-4o-mini", false);
 * ai.setTemperature(0.9f);  // More creative responses
 * ai.setSampling(0.95f);    // Broader vocabulary selection
 * ai.setImageDetail("high"); // Higher quality image analysis
 * }</pre>
 * 
 * <h2>Reasoning Models</h2>
 * <pre>{@code
 * // For o1, o3, and other reasoning models
 * OpenAI reasoning = new OpenAI(apiKey, "o1-preview", true);
 * reasoning.setReasoningEffort("high"); // More thorough reasoning
 * String result = reasoning.send("Solve this complex problem...");
 * }</pre>
 *
 * <h2>Chat Completions vs. Responses</h2>
 * <p>The provider serves models through two different endpoints: the long-standing
 * chat completions endpoint, and the newer responses endpoint.  Some models are
 * available only on the latter, and answer a chat completions request with an HTTP 404
 * saying so.  By default this class handles that automatically ({@link Api#AUTO}): a
 * request goes to chat completions, and if the provider reports that the model requires
 * the responses endpoint, the same request is transparently re-sent there and the
 * choice is remembered for that model for the life of the JVM.  Callers that know which
 * endpoint they want can pin it with {@link #setApi(Api)}.</p>
 *
 * @author Kiss Web Development Framework
 * @see RestClient
 */
public class OpenAI {

    private static String OPENAI_URL = "https://api.openai.com/v1/chat/completions";
    private static String OPENAI_RESPONSES_URL = "https://api.openai.com/v1/responses";

    /**
     * Which of the provider's two APIs a request is sent to.
     *
     * @see #setApi(Api)
     */
    public enum Api {
        /** Use chat completions, falling back to responses when the model requires it (default). */
        AUTO,
        /** Always use the chat completions endpoint. */
        CHAT_COMPLETIONS,
        /** Always use the responses endpoint. */
        RESPONSES
    }

    /** Models the provider has told us are served only by the responses endpoint. */
    private static final Set<String> RESPONSES_ONLY_MODELS = ConcurrentHashMap.newKeySet();

    private final String apiKey; // Your API key
    private final String model; // e.g. "gpt-4o"
    private final boolean reasoningModel; // true if model supports the "reasoning_effort" field

    private float temperature = 0.7f; // Generation randomness
    private float topP = 0.7f; // Nucleus sampling
    private String reasoningEffort = "medium"; // Extra signal for reasoning models
    private String imageDetail = "auto"; // "low", "high", "auto"
    private Api api = Api.AUTO; // Which endpoint to use

    private JSONObject lastResponse; // Full JSON of last non-stream call
    private int lastHttpStatus;      // HTTP status of the last streaming call
    private String lastErrorBody;    // Raw error body of the last failed call, else null
    private final RestClient restClient; // Re‑used HTTP helper



    /**
     * Overrides the OpenAI Chat Completions endpoint URL used by this class.
     *
     * <p>This is a global setting: changing the URL affects all current and future
     * {@link OpenAI} instances created in this JVM.</p>
     *
     * <p>This is primarily intended for testing, proxies, gateways, or OpenAI-compatible
     * endpoints.</p>
     *
     * @param url the full URL to use for chat completion requests (for example,
     *            {@code https://api.openai.com/v1/chat/completions})
     */
    public static void setUrl(String url) {
        OPENAI_URL = url;
    }

    /**
     * Overrides the Responses endpoint URL used by this class.
     *
     * <p>This is a global setting: changing the URL affects all current and future
     * {@link OpenAI} instances created in this JVM.</p>
     *
     * <p>This is primarily intended for testing, proxies, gateways, or compatible
     * endpoints.</p>
     *
     * @param url the full URL to use for responses requests (for example,
     *            {@code https://api.openai.com/v1/responses})
     */
    public static void setResponsesUrl(String url) {
        OPENAI_RESPONSES_URL = url;
    }

    /**
     * Creates a new OpenAI client instance.
     * 
     * @param apiKey         Your OpenAI API key for authentication
     * @param model          The model identifier (e.g., "gpt-4o", "gpt-4o-mini", "o1-preview")
     * @param reasoningModel {@code true} if the model supports reasoning_effort parameter
     *                       (e.g., o1, o3 models), {@code false} for standard models
     * @throws NullPointerException if apiKey or model is null
     */
    public OpenAI(String apiKey, String model, boolean reasoningModel) {
        this.apiKey = apiKey;
        this.model = model;
        this.reasoningModel = reasoningModel;

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
     * @throws IllegalArgumentException if temperature is negative
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
     * Sets the reasoning effort level for reasoning models.
     * 
     * <p>This parameter is only used by reasoning models (o1, o3, etc.) to control
     * how much computational effort should be spent on reasoning through the problem.</p>
     * 
     * @param effort The reasoning effort level:
     *               <ul>
     *                 <li>"low" - Quick reasoning, faster responses</li>
     *                 <li>"medium" - Balanced reasoning and speed (default)</li>
     *                 <li>"high" - Thorough reasoning, slower but more accurate</li>
     *               </ul>
     * @throws IllegalArgumentException if effort is not "low", "medium", or "high"
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
     *                 <li>"low" - Faster processing, lower token usage, 512x512px</li>
     *                 <li>"high" - Better accuracy, higher token usage, up to 2048x2048px</li>
     *                 <li>"auto" - Model automatically chooses based on image (default)</li>
     *               </ul>
     */
    public void setImageDetail(String detail) {
        this.imageDetail = detail;
    }

    /**
     * Selects which of the provider's two APIs this instance sends requests to.
     *
     * <p>The default, {@link Api#AUTO}, needs no configuration: requests go to the chat
     * completions endpoint, and when the provider answers that the model is served only
     * by the responses endpoint, the request is transparently re-sent there and that
     * model is remembered so later requests go straight to it.  Pin the value only when
     * a particular endpoint is required (for example, when pointing this class at a
     * gateway that implements just one of them).</p>
     *
     * @param api the API to use; {@code null} is treated as {@link Api#AUTO}
     */
    public void setApi(Api api) {
        this.api = api == null ? Api.AUTO : api;
    }

    /* ----------------------------------------------------------------------
     * Public helpers – ONE send, ONE stream
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
     * @throws java.lang.IllegalArgumentException if the image format is not supported
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
     * @param onDone    Callback invoked when the response is complete
     * @throws Exception if the API request fails, the image cannot be read, or streaming fails
     * @throws NullPointerException if query, onToken, or onDone is null
     * @see #stream(String, Consumer, Runnable) for text-only streaming
     * @see #setApi(Api) for choosing the endpoint explicitly
     */
    public void stream(String query,
                       String imagePath,
                       Consumer<String> onToken,
                       Runnable onDone) throws Exception {

        Api use = api;
        if (use == Api.AUTO)
            use = RESPONSES_ONLY_MODELS.contains(model) ? Api.RESPONSES : Api.CHAT_COMPLETIONS;

        if (use == Api.RESPONSES) {
            streamResponses(query, imagePath, onToken, onDone);
            return;
        }

        try {
            streamChatCompletions(query, imagePath, onToken, onDone);
        } catch (Exception e) {
            if (api != Api.AUTO  ||  !modelRequiresResponsesApi())
                throw e;
            // The model exists but is served only by the responses endpoint.  Nothing has
            // been delivered to the caller yet (the failure is the HTTP status of the
            // request itself), so the call can simply be re-sent to the other endpoint.
            RESPONSES_ONLY_MODELS.add(model);
            streamResponses(query, imagePath, onToken, onDone);
        }
    }

    /**
     * Whether the last failed request failed because the model is served only by the
     * responses endpoint (the provider reports this as a 404 naming that endpoint).
     */
    private boolean modelRequiresResponsesApi() {
        return lastHttpStatus == 404  &&  lastErrorBody != null  &&  lastErrorBody.contains("/responses");
    }

    /**
     * Streams a request through the chat completions endpoint.
     *
     * @param query     the text prompt
     * @param imagePath optional path to an image file to include, or {@code null}
     * @param onToken   callback invoked for each response token
     * @param onDone    callback invoked when the response is complete
     * @throws Exception if the request fails, the image cannot be read, or streaming fails
     */
    private void streamChatCompletions(String query,
                                       String imagePath,
                                       Consumer<String> onToken,
                                       Runnable onDone) throws Exception {

        JSONObject body = buildChatBody(query, imagePath);

        HttpRequest req = HttpRequest.newBuilder(URI.create(OPENAI_URL))
                .header("Authorization", "Bearer " + apiKey)
                .header("Content-Type", "application/json")
                .timeout(Duration.ofMinutes(10))
                .POST(HttpRequest.BodyPublishers.ofString(body.toString()))
                .build();

        lastHttpStatus = 0;
        lastErrorBody = null;

        HttpResponse<Stream<String>> resp =
                restClient.streamCall(req, HttpResponse.BodyHandlers.ofLines());

        lastHttpStatus = resp.statusCode();
        if (lastHttpStatus / 100 != 2) {
            // Errors arrive as a plain JSON body, not an SSE stream
            lastErrorBody = resp.body().collect(Collectors.joining("\n"));
            throw new Exception("OpenAI request failed with HTTP " + lastHttpStatus + ": " + lastErrorBody);
        }

        final boolean[] doneFired = {false};

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
                            throw new RuntimeException("OpenAI returned an error: " + lastErrorBody);
                        }
                        JSONArray choices = chunk.has("choices") ? chunk.getJSONArray("choices") : null;
                        if (choices == null || choices.length() == 0)
                            return; // e.g. a final usage-only chunk
                        JSONObject delta = choices.getJSONObject(0).getJSONObject("delta");
                        if (delta.has("content") && !delta.isNull("content"))
                            onToken.accept(delta.getString("content"));
                    }
                });

// Ensure onDone is always called even if the [DONE] sentinel was not received
        if (!doneFired[0])
            onDone.run();
    }

    /**
     * Streams a request through the responses endpoint.
     *
     * <p>The responses endpoint reports its progress as a sequence of typed events rather
     * than as message deltas; only the output-text deltas are passed to {@code onToken},
     * so callers see the same token stream either endpoint is used.</p>
     *
     * @param query     the text prompt
     * @param imagePath optional path to an image file to include, or {@code null}
     * @param onToken   callback invoked for each response token
     * @param onDone    callback invoked when the response is complete
     * @throws Exception if the request fails, the image cannot be read, or streaming fails
     */
    private void streamResponses(String query,
                                 String imagePath,
                                 Consumer<String> onToken,
                                 Runnable onDone) throws Exception {

        JSONObject body = buildResponsesBody(query, imagePath);

        HttpRequest req = HttpRequest.newBuilder(URI.create(OPENAI_RESPONSES_URL))
                .header("Authorization", "Bearer " + apiKey)
                .header("Content-Type", "application/json")
                .timeout(Duration.ofMinutes(10))
                .POST(HttpRequest.BodyPublishers.ofString(body.toString()))
                .build();

        lastHttpStatus = 0;
        lastErrorBody = null;

        HttpResponse<Stream<String>> resp =
                restClient.streamCall(req, HttpResponse.BodyHandlers.ofLines());

        lastHttpStatus = resp.statusCode();
        if (lastHttpStatus / 100 != 2) {
            // Errors arrive as a plain JSON body, not an SSE stream
            lastErrorBody = resp.body().collect(Collectors.joining("\n"));
            throw new Exception("OpenAI request failed with HTTP " + lastHttpStatus + ": " + lastErrorBody);
        }

        final boolean[] doneFired = {false};

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
                    if (!payload.startsWith("{"))
                        return;
                    JSONObject event = new JSONObject(payload);
                    String type = event.getString("type", "");
                    switch (type) {
                        case "response.output_text.delta":
                            if (event.has("delta") && !event.isNull("delta"))
                                onToken.accept(event.getString("delta"));
                            break;
                        case "response.completed":
                        case "response.incomplete":
                            // Incomplete means the answer was cut short (for example by a
                            // token limit); whatever was produced has already been delivered.
                            if (!doneFired[0]) {
                                doneFired[0] = true;
                                onDone.run();
                            }
                            break;
                        case "response.failed":
                        case "error":
                            // A failure can also be reported mid-stream as an SSE error event
                            lastErrorBody = payload;
                            throw new RuntimeException("OpenAI returned an error: " + lastErrorBody);
                        default:
                            break; // progress events carrying nothing the caller needs
                    }
                });

// Ensure onDone is always called even if no completion event was received
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
     * Embeddings API
     * ---------------------------------------------------------------------- */

    /**
     * Generates embedding vectors for the given text.
     * 
     * <p>Embeddings are numerical representations of text that capture semantic meaning.
     * They are useful for similarity comparisons, clustering, search, and classification tasks.</p>
     * 
     * <b>Common Use Cases:</b>
     * <ul>
     *   <li>Semantic search - Find similar documents or passages</li>
     *   <li>Clustering - Group related texts together</li>
     *   <li>Recommendations - Find similar items based on descriptions</li>
     *   <li>Anomaly detection - Identify outliers in text data</li>
     * </ul>
     * 
     * <b>Example Usage:</b>
     * <pre>{@code
     * OpenAI ai = new OpenAI(apiKey, "text-embedding-3-small", false);
     * 
     * // Get embeddings for two texts
     * double[] embedding1 = ai.getEmbeddings("The weather is sunny today");
     * double[] embedding2 = ai.getEmbeddings("Today is a bright and clear day");
     * 
     * // Calculate cosine similarity
     * double similarity = cosineSimilarity(embedding1, embedding2);
     * System.out.println("Similarity: " + similarity); // High similarity expected
     * }</pre>
     * 
     * @param text The input text to generate embeddings for. Maximum length varies by model
     *             (8,191 tokens for text-embedding-3-small/large).
     * @return An array of doubles representing the embedding vector. The dimensionality
     *         depends on the model (1,536 for text-embedding-3-small, 3,072 for text-embedding-3-large)
     * @throws Exception if the API request fails, text is too long, or response cannot be parsed
     * @throws IllegalArgumentException if text is null or empty
     * @throws RuntimeException if the embeddings array is empty or malformed
     */
    public double[] getEmbeddings(String text) throws Exception {
        lastHttpStatus = 0;
        lastErrorBody = null;

        JSONObject request = new JSONObject()
                .put("input", text)
                .put("model", model);

        JSONObject headers = new JSONObject()
                .put("Content-Type", "application/json")
                .put("Authorization", "Bearer " + apiKey);

        lastResponse = restClient.jsonCall("POST", "https://api.openai.com/v1/embeddings", request.toString(), headers);

        JSONArray arr = lastResponse.getJSONArray("data").getJSONObject(0).getJSONArray("embedding");
        double[] result = new double[arr.length()];
        for (int i = 0; i < arr.length(); i++) result[i] = arr.getDouble(i);
        return result;
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
     * <b>Response Structure Example:</b>
     * <pre>{@code
     * {
     *   "id": "chatcmpl-...",
     *   "object": "chat.completion",
     *   "created": 1234567890,
     *   "model": "gpt-4o",
     *   "usage": {
     *     "prompt_tokens": 25,
     *     "completion_tokens": 50,
     *     "total_tokens": 75
     *   },
     *   "choices": [...]
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
     *   <li>503 - Service temporarily unavailable</li>
     * </ul>
     * 
     * @return The HTTP status code from the most recent API call, or 0 if no call has been made
     */
    public int getResponseCode() {
        return lastHttpStatus != 0 ? lastHttpStatus : restClient.getResponseCode();
    }

    /**
     * Returns the raw response string from the last API call.
     *
     * <p>Provides access to the complete HTTP response body as received from the server.
     * When a call fails (non-2xx HTTP status, or an error event received mid-stream),
     * the raw error text is retained here; the same text is also included in the
     * thrown exception's message.</p>
     *
     * @return The raw response text, or {@code null} if no response is available
     * @see #getLastFullResponse() for parsed JSON response
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
     * generation parameters, and message content with optional image data.</p>
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
                .put("stream", true);

        if (reasoningModel) {
            body.put("reasoning_effort", reasoningEffort);
        } else {
            body.put("temperature", temperature)
                    .put("top_p", topP);
        }

// Assemble user message content
        JSONArray contentArray = new JSONArray()
                .put(new JSONObject()
                        .put("type", "text")
                        .put("text", query));

        if (imagePath != null) {
            JSONObject imageObj = new JSONObject().put("url", imageDataUrl(imagePath));
            if (imageDetail != null && !imageDetail.isEmpty()) {
                imageObj.put("detail", imageDetail); // Only include if caller supplied
            }

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
     * Builds the JSON request body for responses endpoint calls.
     *
     * <p>Carries the same content as {@link #buildChatBody} in the shape that endpoint
     * expects: the system message becomes the top-level instructions, the prompt becomes
     * typed input content, and the reasoning effort moves into its own object.</p>
     *
     * @param query     The user's text prompt
     * @param imagePath Optional path to an image file to include
     * @return A JSONObject containing the complete request body
     * @throws Exception if image file cannot be read or encoded
     */
    private JSONObject buildResponsesBody(String query,
                                          String imagePath) throws Exception {
        JSONObject body = new JSONObject()
                .put("model", model)
                .put("stream", true)
                .put("instructions", "You are a helpful assistant.");

        if (reasoningModel) {
            body.put("reasoning", new JSONObject().put("effort", reasoningEffort));
        } else {
            body.put("temperature", temperature)
                    .put("top_p", topP);
        }

// Assemble user message content
        JSONArray contentArray = new JSONArray()
                .put(new JSONObject()
                        .put("type", "input_text")
                        .put("text", query));

        if (imagePath != null) {
            JSONObject imageObj = new JSONObject()
                    .put("type", "input_image")
                    .put("image_url", imageDataUrl(imagePath));
            if (imageDetail != null && !imageDetail.isEmpty()) {
                imageObj.put("detail", imageDetail); // Only include if caller supplied
            }
            contentArray.put(imageObj);
        }

        body.put("input", new JSONArray()
                .put(new JSONObject()
                        .put("role", "user")
                        .put("content", contentArray)));
        return body;
    }

    /**
     * Reads an image file and encodes it as a data URL for inclusion in a request.
     *
     * @param imagePath path to the image file
     * @return the image as a {@code data:} URL
     * @throws Exception if the file cannot be read
     */
    private String imageDataUrl(String imagePath) throws Exception {
        byte[] imageBytes = Files.readAllBytes(Paths.get(imagePath));
        return "data:image/jpeg;base64," + Base64.getEncoder().encodeToString(imageBytes);
    }
}
