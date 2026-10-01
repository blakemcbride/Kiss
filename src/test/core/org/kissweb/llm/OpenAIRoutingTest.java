package org.kissweb.llm;

import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.Test;

import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.*;

/**
 * No-network tests for {@link OpenAI}'s endpoint routing: the "-pro" model predicate and the
 * eligibility check that decides whether a failed chat completions call is re-sent to the
 * responses endpoint.
 */
class OpenAIRoutingTest {

    private static final String OLD_BODY =
            "{\"error\":{\"message\":\"This model is only supported in v1/responses and not in v1/chat/completions. "
            + "Use https://api.openai.com/v1/responses\",\"type\":\"invalid_request_error\",\"param\":\"model\",\"code\":null}}";
    private static final String OLD_BODY_NO_PARAM =
            "{\"error\":{\"message\":\"Use /v1/responses for this model\",\"type\":\"invalid_request_error\"}}";
    private static final String NEW_BODY =
            "{\"error\":{\"message\":\"This is not a chat model and thus not supported in the v1/chat/completions "
            + "endpoint. Did you mean to use v1/completions?\",\"type\":\"invalid_request_error\",\"param\":\"model\",\"code\":null}}";
    private static final String PARAM_MODEL_ONLY =
            "{\"error\":{\"message\":\"Something about the model\",\"type\":\"invalid_request_error\",\"param\":\"model\"}}";
    private static final String UNKNOWN_MODEL =
            "{\"error\":{\"message\":\"The model `nope` does not exist or you do not have access to it.\","
            + "\"type\":\"invalid_request_error\",\"param\":null,\"code\":\"model_not_found\"}}";
    private static final String UNKNOWN_MODEL_WITH_PARAM =
            "{\"error\":{\"message\":\"The model `nope` does not exist\",\"type\":\"invalid_request_error\","
            + "\"param\":\"model\",\"code\":\"model_not_found\"}}";

    @Test
    void oldResponsesBodyIsEligible() {
        assertTrue(OpenAI.isResponsesRequiredError(404, OLD_BODY));
        assertTrue(OpenAI.isResponsesRequiredError(404, OLD_BODY_NO_PARAM));
    }

    @Test
    void notAChatModelBodyIsEligible() {
        assertTrue(OpenAI.isResponsesRequiredError(404, NEW_BODY));
        assertTrue(OpenAI.isResponsesRequiredError(404, PARAM_MODEL_ONLY));
        assertTrue(OpenAI.isResponsesRequiredError(404, "plain text: This is not a chat model"));
    }

    @Test
    void ordinaryFailuresAreNotEligible() {
        assertFalse(OpenAI.isResponsesRequiredError(404, "Not Found"));
        assertFalse(OpenAI.isResponsesRequiredError(404, "{\"error\":{\"message\":\"nope\"}}"));
        assertFalse(OpenAI.isResponsesRequiredError(404, UNKNOWN_MODEL));
        assertFalse(OpenAI.isResponsesRequiredError(404, UNKNOWN_MODEL_WITH_PARAM));
        assertFalse(OpenAI.isResponsesRequiredError(404, null));
        assertFalse(OpenAI.isResponsesRequiredError(404, ""));
        assertFalse(OpenAI.isResponsesRequiredError(400, NEW_BODY));
        assertFalse(OpenAI.isResponsesRequiredError(500, OLD_BODY));
        assertFalse(OpenAI.isResponsesRequiredError(0, NEW_BODY));
    }

    @Test
    void proSuffixIsResponsesOnly() {
        assertTrue(OpenAI.isResponsesOnlyModel("o1-pro"));
        assertTrue(OpenAI.isResponsesOnlyModel("gpt-5-pro"));
        assertTrue(OpenAI.isResponsesOnlyModel("gpt-5.4-pro"));
        assertTrue(OpenAI.isResponsesOnlyModel("GPT-6.1-PRO"));
        assertFalse(OpenAI.isResponsesOnlyModel("gpt-4o"));
        assertFalse(OpenAI.isResponsesOnlyModel("gpt-5.4"));
        assertFalse(OpenAI.isResponsesOnlyModel("gpt-pro-mini"));
        assertFalse(OpenAI.isResponsesOnlyModel("pro"));
        assertFalse(OpenAI.isResponsesOnlyModel(null));
    }

    /**
     * End to end against a local server: chat completions answers the "not a chat model" 404;
     * when the responses endpoint also answers 404 the call throws after exactly one retry
     * and the model is not remembered; when it succeeds the model is remembered, so the next
     * call skips chat completions.
     */
    @Test
    void fallbackRetriesOnceAndRemembersOnlyOnSuccess() throws Exception {
        AtomicInteger chatCalls = new AtomicInteger();
        AtomicInteger respCalls = new AtomicInteger();
        boolean[] respOk = {false};
        HttpServer server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
        server.createContext("/chat", ex -> {
            chatCalls.incrementAndGet();
            reply(ex, 404, NEW_BODY);
        });
        server.createContext("/resp", ex -> {
            respCalls.incrementAndGet();
            if (respOk[0])
                reply(ex, 200, "data: {\"type\":\"response.output_text.delta\",\"delta\":\"hi\"}\n\n"
                        + "data: {\"type\":\"response.completed\"}\n\n");
            else
                reply(ex, 404, UNKNOWN_MODEL);
        });
        server.start();
        try {
            String base = "http://127.0.0.1:" + server.getAddress().getPort();
            OpenAI.setUrl(base + "/chat");
            OpenAI.setResponsesUrl(base + "/resp");
            String model = "routing-test-model-x";

            Exception e = assertThrows(Exception.class, () -> new OpenAI("k", model, false).send("q"));
            assertTrue(e.getMessage().contains("404"));
            assertEquals(1, chatCalls.get());
            assertEquals(1, respCalls.get());
            assertFalse(OpenAI.isResponsesOnlyModel(model));

            respOk[0] = true;
            assertEquals("hi", new OpenAI("k", model, false).send("q"));
            assertEquals(2, chatCalls.get());
            assertEquals(2, respCalls.get());
            assertTrue(OpenAI.isResponsesOnlyModel(model));

            assertEquals("hi", new OpenAI("k", model, false).send("q"));
            assertEquals(2, chatCalls.get());   // went straight to responses
            assertEquals(3, respCalls.get());
        } finally {
            server.stop(0);
            OpenAI.setUrl("https://api.openai.com/v1/chat/completions");
            OpenAI.setResponsesUrl("https://api.openai.com/v1/responses");
        }
    }

    private static void reply(com.sun.net.httpserver.HttpExchange ex, int status, String body) throws java.io.IOException {
        byte[] b = body.getBytes(StandardCharsets.UTF_8);
        ex.sendResponseHeaders(status, b.length);
        ex.getResponseBody().write(b);
        ex.close();
    }
}
