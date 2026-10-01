package org.kissweb.llm;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.Test;
import org.kissweb.json.JSONObject;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.*;

/**
 * No-network tests for the opt-in web search of {@link Anthropic}, {@link OpenAI} and
 * {@link OpenRouter}: the outbound request body carries the provider's search field only
 * when enabled, and streamed responses that interleave search activity with text parse to
 * the concatenated text.
 */
class WebSearchTest {

    /** A local server that records the last request path and body and replies with a fixed SSE body. */
    private static final class Fake {
        final HttpServer server;
        volatile String body;
        volatile String path;

        Fake(String sse) throws IOException {
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            server.createContext("/", ex -> {
                path = ex.getRequestURI().getPath();
                body = new String(ex.getRequestBody().readAllBytes(), StandardCharsets.UTF_8);
                reply(ex, sse);
            });
            server.start();
        }

        String url(String p) {
            return "http://127.0.0.1:" + server.getAddress().getPort() + p;
        }

        JSONObject json() {
            return new JSONObject(body);
        }
    }

    private static void reply(HttpExchange ex, String body) throws IOException {
        byte[] b = body.getBytes(StandardCharsets.UTF_8);
        ex.sendResponseHeaders(200, b.length);
        ex.getResponseBody().write(b);
        ex.close();
    }

    /* ------------------------------ Anthropic ------------------------------ */

    private static final String ANTHROPIC_SSE =
            "event: message_start\ndata: {\"type\":\"message_start\"}\n\n"
            + "data: {\"type\":\"content_block_start\",\"index\":0,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
            + "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"I'll search. \"}}\n\n"
            + "data: {\"type\":\"content_block_start\",\"index\":1,\"content_block\":{\"type\":\"server_tool_use\",\"id\":\"srvtoolu_1\",\"name\":\"web_search\"}}\n\n"
            + "data: {\"type\":\"content_block_delta\",\"index\":1,\"delta\":{\"type\":\"input_json_delta\",\"partial_json\":\"{\\\"query\\\":\\\"x\\\"}\"}}\n\n"
            + "data: {\"type\":\"content_block_start\",\"index\":2,\"content_block\":{\"type\":\"web_search_tool_result\",\"tool_use_id\":\"srvtoolu_1\",\"content\":[]}}\n\n"
            + "data: {\"type\":\"content_block_start\",\"index\":3,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
            + "data: {\"type\":\"content_block_delta\",\"index\":3,\"delta\":{\"type\":\"text_delta\",\"text\":\"Based on \"}}\n\n"
            + "data: {\"type\":\"content_block_delta\",\"index\":3,\"delta\":{\"type\":\"citations_delta\",\"citation\":{\"type\":\"web_search_result_location\"}}}\n\n"
            + "data: {\"type\":\"content_block_start\",\"index\":4,\"content_block\":{\"type\":\"text\",\"text\":\"\"}}\n\n"
            + "data: {\"type\":\"content_block_delta\",\"index\":4,\"delta\":{\"type\":\"text_delta\",\"text\":\"the results.\"}}\n\n"
            + "data: {\"type\":\"message_stop\"}\n\n";

    @Test
    void anthropicWebSearchOnAndOff() throws Exception {
        Fake f = new Fake(ANTHROPIC_SSE);
        try {
            Anthropic.setUrl(f.url("/v1/messages"));
            Anthropic off = new Anthropic("k", "claude-x");
            assertEquals("I'll search. Based on the results.", off.send("q"));
            assertFalse(f.json().has("tools"));

            Anthropic on = new Anthropic("k", "claude-x");
            on.setWebSearch(true);
            assertEquals("I'll search. Based on the results.", on.send("q"));
            JSONObject tool = f.json().getJSONArray("tools").getJSONObject(0);
            assertEquals("web_search_20250305", tool.getString("type"));
            assertEquals("web_search", tool.getString("name"));
            assertEquals(1, f.json().getJSONArray("tools").length());
        } finally {
            f.server.stop(0);
            Anthropic.setUrl("https://api.anthropic.com/v1/messages");
        }
    }

    /* ------------------------------- OpenAI -------------------------------- */

    private static final String OPENAI_RESPONSES_SSE =
            "data: {\"type\":\"response.created\"}\n\n"
            + "data: {\"type\":\"response.output_item.added\",\"item\":{\"type\":\"web_search_call\",\"id\":\"ws_1\",\"status\":\"in_progress\"}}\n\n"
            + "data: {\"type\":\"response.web_search_call.searching\",\"item_id\":\"ws_1\"}\n\n"
            + "data: {\"type\":\"response.web_search_call.completed\",\"item_id\":\"ws_1\"}\n\n"
            + "data: {\"type\":\"response.output_text.delta\",\"delta\":\"Acme \"}\n\n"
            + "data: {\"type\":\"response.output_text.annotation.added\",\"annotation\":{\"type\":\"url_citation\"}}\n\n"
            + "data: {\"type\":\"response.output_text.delta\",\"delta\":\"sells widgets.\"}\n\n"
            + "data: {\"type\":\"response.completed\"}\n\n";

    @Test
    void openAiWebSearchRoutesToResponses() throws Exception {
        Fake f = new Fake(OPENAI_RESPONSES_SSE);
        try {
            OpenAI.setUrl(f.url("/chat"));
            OpenAI.setResponsesUrl(f.url("/resp"));

            OpenAI on = new OpenAI("k", "gpt-4o", false);
            on.setWebSearch(true);
            assertEquals("Acme sells widgets.", on.send("q"));
            assertEquals("/resp", f.path);   // plain chat model, still the responses endpoint
            JSONObject tool = f.json().getJSONArray("tools").getJSONObject(0);
            assertEquals("web_search", tool.getString("type"));

            // pinned to chat completions: web search still wins
            on.setApi(OpenAI.Api.CHAT_COMPLETIONS);
            on.send("q");
            assertEquals("/resp", f.path);
        } finally {
            f.server.stop(0);
            OpenAI.setUrl("https://api.openai.com/v1/chat/completions");
            OpenAI.setResponsesUrl("https://api.openai.com/v1/responses");
        }
    }

    @Test
    void openAiWebSearchOffIsUnchanged() throws Exception {
        Fake f = new Fake("data: {\"choices\":[{\"delta\":{\"content\":\"hi\"}}]}\n\ndata: [DONE]\n\n");
        try {
            OpenAI.setUrl(f.url("/chat"));
            OpenAI.setResponsesUrl(f.url("/resp"));
            assertEquals("hi", new OpenAI("k", "gpt-4o", false).send("q"));
            assertEquals("/chat", f.path);
            assertFalse(f.json().has("tools"));
        } finally {
            f.server.stop(0);
            OpenAI.setUrl("https://api.openai.com/v1/chat/completions");
            OpenAI.setResponsesUrl("https://api.openai.com/v1/responses");
        }
    }

    @Test
    void openAiResponsesWithoutWebSearchHasNoTools() throws Exception {
        Fake f = new Fake(OPENAI_RESPONSES_SSE);
        try {
            OpenAI.setResponsesUrl(f.url("/resp"));
            assertEquals("Acme sells widgets.", new OpenAI("k", "gpt-5-pro", false).send("q"));
            assertFalse(f.json().has("tools"));
        } finally {
            f.server.stop(0);
            OpenAI.setResponsesUrl("https://api.openai.com/v1/responses");
        }
    }

    /* ----------------------------- OpenRouter ------------------------------ */

    private static final String OPENROUTER_SSE =
            ": OPENROUTER PROCESSING\n\n"
            + "data: {\"choices\":[{\"delta\":{\"content\":\"Found \"}}]}\n\n"
            + "data: {\"choices\":[{\"delta\":{\"content\":\"it.\",\"annotations\":[{\"type\":\"url_citation\","
            + "\"url_citation\":{\"url\":\"https://example.com\"}}]}}]}\n\n"
            + "data: [DONE]\n\n";

    @Test
    void openRouterWebSearchOnAndOff() throws Exception {
        Fake f = new Fake(OPENROUTER_SSE);
        try {
            OpenRouter.setUrl(f.url("/chat"));
            OpenRouter off = new OpenRouter("k", "openai/gpt-4o");
            assertEquals("Found it.", off.send("q"));
            assertFalse(f.json().has("plugins"));

            OpenRouter on = new OpenRouter("k", "openai/gpt-4o");
            on.setWebSearch(true);
            assertEquals("Found it.", on.send("q"));
            JSONObject b = f.json();
            assertEquals("web", b.getJSONArray("plugins").getJSONObject(0).getString("id"));
            assertEquals("openai/gpt-4o", b.getString("model")); // id untouched, no :online
        } finally {
            f.server.stop(0);
            OpenRouter.setUrl("https://openrouter.ai/api/v1/chat/completions");
        }
    }
}
