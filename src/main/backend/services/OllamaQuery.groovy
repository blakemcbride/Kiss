package services

import org.kissweb.json.JSONObject
import org.kissweb.llm.Ollama
import org.kissweb.database.*
import org.kissweb.restServer.ProcessServlet
import org.kissweb.restServer.MainServlet

class OllamaQuery {

    void ask(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        // An LLM generation can take far longer than the container's default async timeout (~30s),
        // so lift the timeout for this request; the Ollama client's own request timeout still bounds it.
        servlet.setTimeout(0)
        String model = injson.getString("model")
        String prompt = injson.getString("prompt")
        Ollama server = new Ollama()
        // Optional: tune the Ollama request timeout (seconds) from application.ini without recompiling.
        String timeout = MainServlet.getEnvironment("OllamaTimeoutSeconds")
        if (timeout != null && timeout.trim().isInteger())
            server.setTimeoutSeconds(timeout.trim().toInteger())
        server.selectModel(model)
        String response = server.send(prompt)
        outjson.put("textResponse", response)
        outjson.put("htmlResponse", Ollama.toHtml(response))
    }

    void isOllamaUp(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        outjson.put("isOllamaUp", new Ollama().isOllamaUp())
    }

    void listModels(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        outjson.put("models", new Ollama().getAvailableModels())
    }
}
