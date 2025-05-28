package org.kissweb.llm;

import java.net.*;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.UUID;

import org.kissweb.json.*;

/**
 * Interface to the Qdrant vector database used to store embeddings for RAG.
 */
public class QdrantClient {

    private static String QDRANT_URL = "http://localhost:6333";
    private final String collection;

    /**
     * Open an existing collection.
     *
     * @param collection the name of the collection
     */
    public QdrantClient(String collection) {
        this.collection = collection;
    }

    /**
     * Change the default (local) URL for the Qdrant server.
     *
     * @param url the URL of the Qdrant server
     */
    public static void setUrl(String url) {
        QDRANT_URL = url;
    }

    /**
     * Create a new collection.
     *
     * @param collection the name of the collection
     * @param vectorSize the size of the embedding vector
     * @throws Exception if an error occurs during collection creation
     */
    public static void createCollection(String collection, int vectorSize) throws Exception {
        JSONObject body = new JSONObject();
        JSONObject vectors = new JSONObject();
        vectors.put("size", vectorSize);
        vectors.put("distance", "Cosine");
        body.put("vectors", vectors);

        sendRequest("PUT", "/collections/" + collection, body.toString());
    }

    /**
     * List the names of existing collections.
     *
     * @return an array of collection names
     * @throws Exception if an error occurs during the request
     */
    public static String[] listCollections() throws Exception {
        String response = sendRequest("GET", "/collections", null);
        JSONObject obj = new JSONObject(response);
        JSONArray cls = obj.getJSONObject("result").getJSONArray("collections");
        int size = cls.length();
        String[] result = new String[size];
        for (int i = 0; i < size; i++)
            result[i] = cls.getJSONObject(i).getString("name");
        return result;
    }

    /**
     * Delete the named collection.
     *
     * @param collection the name of the collection to delete
     * @throws Exception if an error occurs during collection deletion
     */
    public static void deleteCollection(String collection) throws Exception {
        sendRequest("DELETE", "/collections/" + collection, null);
    }

    /**
     * Insert a new record or update an existing record.
     *
     * @param chunkId The ID of the chunk to which the record belongs or null if you want it auto-generated for a new record.
     * @param documentId The ID of the document to which the chunk is associated.
     * @param sequenceNumber The sequence number of the text within the document.
     * @param vector The embedding vector to be stored. Its length must exactly match the vector size defined when the collection was created (e.g., 384, 768, etc.).
     * @param text A piece of associated plain text to be stored in the record's payload under the key "text". This can be used for display or filtering purposes.
     * @param otherInfo Optional additional metadata (key-value pairs) to store with the record in the payload object. This can contain fields like "author", "title", "timestamp", etc. It may be null.
     * @return The chunkId of the inserted or updated record
     * @throws Exception if an error occurs during the insert/update operation
     */
    public String insertOrUpdate(String chunkId, String documentId, int sequenceNumber, double[] vector, String text, JSONObject otherInfo) throws Exception {
        JSONObject body = new JSONObject();
        JSONArray points = new JSONArray();
        JSONObject point = new JSONObject();

        if (chunkId == null)
            chunkId = UUID.randomUUID().toString();
        point.put("id", chunkId);

        JSONArray vectorArray = new JSONArray();
        for (double v : vector)
            vectorArray.put(v);
        point.put("vector", vectorArray);

        JSONObject payload = new JSONObject();
        payload.put("text", text);
        payload.put("document_id", documentId);
        payload.put("sequence_number", sequenceNumber);
        if (otherInfo != null)
            for (String key : JSONObject.getNames(otherInfo))
                payload.put(key, otherInfo.get(key));

        point.put("payload", payload);
        points.put(point);
        body.put("points", points);

        sendRequest("PUT", "/collections/" + collection + "/points", body.toString());
        return chunkId;
    }

    /**
     * Search for the <code>limit</code> closest vectors to the provided <code>vector</code>.
     *
     * @param vector the query vector to search for
     * @param limit the maximum number of results to return
     * @return a JSONArray of search results
     * @throws Exception if an error occurs during the search
     */
    public JSONArray search(double[] vector, int limit) throws Exception {
        JSONObject body = new JSONObject();
        JSONArray vectorArray = new JSONArray();
        for (double v : vector) vectorArray.put(v);
        body.put("vector", vectorArray);
        body.put("limit", limit);

        String response = sendRequest("POST", "/collections/" + collection + "/points/search", body.toString());
        JSONObject obj = new JSONObject(response);
        return obj.getJSONArray("result");
    }

    /**
     * Get a specific record along with all the data associated with it.
     *
     * @param id the ID of the record to retrieve
     * @return the record data as a JSONObject
     * @throws Exception if an error occurs during the retrieval
     */
    public JSONObject getRecord(String id) throws Exception {
        String response = sendRequest("GET", "/collections/" + collection + "/points/" + id, null);
        JSONObject obj = new JSONObject(response);
        return obj.getJSONObject("result");
    }

    /**
     * Delete a specified record.
     *
     * @param id the ID of the record to delete
     * @throws Exception if an error occurs during the deletion
     */
    public void deleteRecord(String id) throws Exception {
        JSONObject body = new JSONObject();
        JSONArray ids = new JSONArray();
        ids.put(id);
        body.put("points", ids);

        sendRequest("POST", "/collections/" + collection + "/points/delete", body.toString());
    }

    /**
     * Utility method that does the actual HTTP request.
     *
     * @param method
     * @param path
     * @param jsonBody
     * @return
     * @throws Exception
     */
    private static String sendRequest(String method, String path, String jsonBody) throws Exception {
        URL url = new URL(QDRANT_URL + path);
        HttpURLConnection conn = (HttpURLConnection) url.openConnection();
        conn.setRequestMethod(method);
        conn.setRequestProperty("Content-Type", "application/json");

        if (jsonBody != null && !jsonBody.isEmpty()) {
            conn.setDoOutput(true);
            try (OutputStream os = conn.getOutputStream()) {
                os.write(jsonBody.getBytes(StandardCharsets.UTF_8));
            }
        }

        int code = conn.getResponseCode();
        InputStream is = (code >= 200 && code < 400) ? conn.getInputStream() : conn.getErrorStream();

        BufferedReader reader = new BufferedReader(new InputStreamReader(is));
        StringBuilder response = new StringBuilder();
        String line;
        while ((line = reader.readLine()) != null)
            response.append(line);
        reader.close();
        return response.toString();
    }

}
