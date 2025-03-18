package services

import org.json.JSONObject
import org.kissweb.Ollama
import org.kissweb.database.*
import org.kissweb.restServer.ProcessServlet

class OllamaQuery {

    void ask(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        String model = injson.getString("model")
        String prompt = injson.getString("prompt")
        Ollama server = new Ollama()
        JSONObject res = server.send(model, prompt)
        String response = res.getString("response")
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
