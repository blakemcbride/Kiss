package org.kissweb.llm;

import com.sun.net.httpserver.HttpExchange;
import com.sun.net.httpserver.HttpServer;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.charset.StandardCharsets;
import java.util.concurrent.Callable;
import java.util.concurrent.atomic.AtomicInteger;

import static org.junit.jupiter.api.Assertions.assertEquals;

/**
 * No-network tests for the per-instance endpoint URL accepted by the {@link Anthropic},
 * {@link OpenAI} and {@link OpenRouter} constructors: two instances route to their own servers,
 * a null/blank URL falls back to the static default, and constructing an instance with a URL
 * never changes the static default seen by another instance.
 */
class PerInstanceUrlTest {

    private static final String ANTHROPIC_DEFAULT = "https://api.anthropic.com/v1/messages";
    private static final String OPENAI_DEFAULT = "https://api.openai.com/v1/chat/completions";
    private static final String OPENROUTER_DEFAULT = "https://openrouter.ai/api/v1/chat/completions";

    private static final String ANTHROPIC_SSE =
            "data: {\"type\":\"content_block_delta\",\"index\":0,\"delta\":{\"type\":\"text_delta\",\"text\":\"ok\"}}\n\n"
            + "data: {\"type\":\"message_stop\"}\n\n";
    private static final String CHAT_SSE =
            "data: {\"choices\":[{\"delta\":{\"content\":\"ok\"}}]}\n\ndata: [DONE]\n\n";

    /** A local server that counts the requests it receives and replies with a fixed stream. */
    private static final class Srv {
        final HttpServer server;
        final AtomicInteger hits = new AtomicInteger();

        Srv(String body) throws IOException {
            server = HttpServer.create(new InetSocketAddress("127.0.0.1", 0), 0);
            server.createContext("/", ex -> {
                hits.incrementAndGet();
                reply(ex, body);
            });
            server.start();
        }

        String url() {
            return "http://127.0.0.1:" + server.getAddress().getPort() + "/x";
        }

        void stop() {
            server.stop(0);
        }
    }

    private static void reply(HttpExchange ex, String body) throws IOException {
        byte[] b = body.getBytes(StandardCharsets.UTF_8);
        ex.sendResponseHeaders(200, b.length);
        ex.getResponseBody().write(b);
        ex.close();
    }

    /**
     * Runs the three scenarios against one provider: per-instance routing, static fallback, no leakage.
     *
     * @param sse        the response stream the fake servers return
     */
    private interface Send {
        String send(String url) throws Exception;
    }

    private static void exercise(String sse, java.util.function.Consumer<String> setStatic, String restore,
                                 Send withUrl, Callable<String> legacy) throws Exception {
        Srv a = new Srv(sse), b = new Srv(sse), def = new Srv(sse);
        try {
            setStatic.accept(def.url());

            // (1) two instances with different URLs each reach their own server
            assertEquals("ok", withUrl.send(a.url()));
            assertEquals("ok", withUrl.send(b.url()));
            assertEquals("ok", withUrl.send(a.url()));
            assertEquals(2, a.hits.get());
            assertEquals(1, b.hits.get());
            assertEquals(0, def.hits.get());

            // (3) constructing instances with URLs did not change the static default
            assertEquals("ok", legacy.call());
            assertEquals(1, def.hits.get());
            assertEquals(2, a.hits.get());
            assertEquals(1, b.hits.get());

            // (2) null and blank URLs use the static default
            assertEquals("ok", withUrl.send(null));
            assertEquals("ok", withUrl.send("   "));
            assertEquals(3, def.hits.get());
            assertEquals(2, a.hits.get());
            assertEquals(1, b.hits.get());
        } finally {
            setStatic.accept(restore);
            a.stop();
            b.stop();
            def.stop();
        }
    }

    @Test
    void anthropic() throws Exception {
        exercise(ANTHROPIC_SSE, Anthropic::setUrl, ANTHROPIC_DEFAULT,
                u -> new Anthropic("k", "m", u).send("q"),
                () -> new Anthropic("k", "m").send("q"));
    }

    @Test
    void openAI() throws Exception {
        exercise(CHAT_SSE, OpenAI::setUrl, OPENAI_DEFAULT,
                u -> new OpenAI("k", "gpt-4o", false, u).send("q"),
                () -> new OpenAI("k", "gpt-4o", false).send("q"));
    }

    @Test
    void openRouter() throws Exception {
        exercise(CHAT_SSE, OpenRouter::setUrl, OPENROUTER_DEFAULT,
                u -> new OpenRouter("k", "openai/gpt-4o", u).send("q"),
                () -> new OpenRouter("k", "openai/gpt-4o").send("q"));
    }
}
