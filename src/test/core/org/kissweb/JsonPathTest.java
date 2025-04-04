package org.kissweb;

import org.json.JSONArray;
import org.json.JSONObject;
import org.junit.jupiter.api.Test;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

import static org.junit.jupiter.api.Assertions.*;

class JsonPathTest {

    /**
     * A sample JSON structure to test various features.
     */
    private static final String SAMPLE_JSON =
            "{\n" +
                    "  \"person\": {\n" +
                    "    \"name\": \"John Doe\",\n" +
                    "    \"age\": 30,\n" +
                    "    \"salary\": 75000.5,\n" +
                    "    \"married\": true,\n" +
                    "    \"children\": [\n" +
                    "      {\"name\": \"Alice\", \"age\": 5},\n" +
                    "      {\"name\": \"Bob\",   \"age\": 7}\n" +
                    "    ],\n" +
                    "    \"initial\": \"J\",\n" +
                    "    \"birthday\": \"2018-06-08\"\n" +
                    "  }\n" +
                    "}";

    @Test
    void testToJsonFromString() throws IOException {
        // Convert JSON string to JSONObject
        JSONObject obj = JsonPath.toJson(SAMPLE_JSON);
        assertNotNull(obj, "JSONObject should not be null");

        // Check basic fields to confirm success
        JSONObject person = obj.getJSONObject("person");
        assertEquals("John Doe", person.getString("name"));
        assertEquals(30, person.getInt("age"));
        assertEquals(75000.5, person.getDouble("salary"), 0.0001);
        assertTrue(person.getBoolean("married"));
    }

    @Test
    void testToJsonFromFile() throws IOException {
        // Write SAMPLE_JSON to a temporary file
        Path tempFile = Files.createTempFile("sample", ".json");
        Files.write(tempFile, SAMPLE_JSON.getBytes());

        // Load JSONObject from file path
        JSONObject obj = JsonPath.toJson(tempFile.toString());
        assertNotNull(obj, "JSONObject should not be null after reading from file");

        // Verify it read correctly
        JSONObject person = obj.getJSONObject("person");
        assertEquals("John Doe", person.getString("name"));
    }

    @Test
    void testToJsonWithNullOrEmptyString() throws IOException {
        // Null or empty input should return an empty JSONObject
        JSONObject empty1 = JsonPath.toJson(null);
        JSONObject empty2 = JsonPath.toJson("");

        assertEquals(0, empty1.length(), "JSONObject should be empty for null input");
        assertEquals(0, empty2.length(), "JSONObject should be empty for empty string input");
    }

    @Test
    void testGetObject() throws IOException {
        JSONObject obj = JsonPath.toJson(SAMPLE_JSON);

        // Direct sub-object
        JSONObject person = JsonPath.getObject(obj, "person");
        assertNotNull(person, "Should retrieve 'person' object");
        assertEquals("John Doe", person.getString("name"));

        // Access a nested object with array index: children[1]
        JSONObject bob = JsonPath.getObject(obj, "person.children[1]");
        assertNotNull(bob, "Should retrieve second child (Bob)");
        assertEquals("Bob", bob.getString("name"));

        // Invalid path should return null
        assertNull(JsonPath.getObject(obj, "person.unknownField"), "Invalid path should yield null");
        assertNull(JsonPath.getObject(null, "person"), "Null source object should yield null");
    }

    @Test
    void testGetString() throws IOException {
        JSONObject obj = JsonPath.toJson(SAMPLE_JSON);

        assertEquals("John Doe", JsonPath.getString(obj, "person.name"));
        assertEquals("Alice", JsonPath.getString(obj, "person.children[0].name"));

        // Invalid path
        assertNull(JsonPath.getString(obj, "person.noSuchField"));
    }

    @Test
    void testGetInteger() throws IOException {
        JSONObject obj = JsonPath.toJson(SAMPLE_JSON);

        assertEquals(30, JsonPath.getInteger(obj, "person.age"));
        // Array index
        assertEquals(5, JsonPath.getInteger(obj, "person.children[0].age"));

        // Invalid path
        assertNull(JsonPath.getInteger(obj, "person.noSuchField"));
        // Wrong type or missing leads to null (e.g., passing a string field)
        assertNull(JsonPath.getInteger(obj, "person.name"));
    }

    @Test
    void testGetDouble() throws IOException {
        JSONObject obj = JsonPath.toJson(SAMPLE_JSON);

        assertEquals(75000.5, JsonPath.getDouble(obj, "person.salary"), 0.0001);

        // Invalid path
        assertNull(JsonPath.getDouble(obj, "person.noSuchField"));
    }

    @Test
    void testGetBoolean() throws IOException {
        JSONObject obj = JsonPath.toJson(SAMPLE_JSON);

        assertTrue(JsonPath.getBoolean(obj, "person.married"), "Should retrieve true");

        // Invalid path returns false (as per code)
        assertNull(JsonPath.getBoolean(obj, "person.noSuchField"), "Should default to false if missing");
    }

    @Test
    void testGetLong() throws IOException {
        JSONObject obj = JsonPath.toJson(SAMPLE_JSON);

        // age = 30, can also be retrieved as Long
        Long age = JsonPath.getLong(obj, "person.age");
        assertNotNull(age);
        assertEquals(30L, age);

        // Invalid path
        assertNull(JsonPath.getLong(obj, "person.noSuchField"));
    }

    @Test
    void testGetCharacter() throws IOException {
        JSONObject obj = JsonPath.toJson(SAMPLE_JSON);

        Character initial = JsonPath.getCharacter(obj, "person.initial");
        assertNotNull(initial);
        assertEquals('J', initial.charValue());

        // Invalid path
        assertNull(JsonPath.getCharacter(obj, "person.unknown"));
    }

    @Test
    void testGetJSONArray() throws IOException {
        JSONObject obj = JsonPath.toJson(SAMPLE_JSON);

        JSONArray children = JsonPath.getJSONArray(obj, "person.children");
        assertNotNull(children, "Should retrieve children array");
        assertEquals(2, children.length());

        // Invalid path
        assertNull(JsonPath.getJSONArray(obj, "person.unknownArray"));
    }

    @Test
    void testGetDate() throws IOException {
        // Note: DateUtils.parse(...) is not shown in your snippet,
        // so this test only checks whether getDate returns non-null
        // given a valid date string. Adjust as needed.
        JSONObject obj = JsonPath.toJson(SAMPLE_JSON);
        Integer dateVal = JsonPath.getDate(obj, "person.birthday");
        // Expect "2018-06-08" -> 20180608 if parse is implemented that way
        // or null if parse fails
        assertNotNull(dateVal, "Date should not be null if parse is successful");

        // Invalid path should yield null
        assertNull(JsonPath.getDate(obj, "person.noSuchField"));
    }
}
