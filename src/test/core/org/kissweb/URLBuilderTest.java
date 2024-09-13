package org.kissweb;

import org.junit.jupiter.api.Test;
import static org.junit.jupiter.api.Assertions.*;

public class URLBuilderTest {

    @Test
    public void testBuildURLWithoutParameters() {
        URLBuilder builder = new URLBuilder("http://example.com");
        assertEquals("http://example.com", builder.build());
    }

    @Test
    public void testBuildURLWithSingleParameter() {
        URLBuilder builder = new URLBuilder("http://example.com");
        builder.addParameter("key", "value");
        assertEquals("http://example.com?key=value", builder.build());
    }

    @Test
    public void testBuildURLWithMultipleParameters() {
        URLBuilder builder = new URLBuilder("http://example.com");
        builder.addParameter("key1", "value1")
               .addParameter("key2", "value2");
        assertEquals("http://example.com?key1=value1&key2=value2", builder.build());
    }

    @Test
    public void testBuildURLWithSpaceInURL() {
        URLBuilder builder = new URLBuilder("http://example.com/search query");
        assertEquals("http://example.com/search%20query", builder.build());
    }

    @Test
    public void testAddParameterWithSpace() {
        URLBuilder builder = new URLBuilder("http://example.com");
        builder.addParameter("search", "a b c");
        assertEquals("http://example.com?search=a+b+c", builder.build());
    }

    @Test
    public void testAddParameterWithSpecialCharacters() {
        URLBuilder builder = new URLBuilder("http://example.com");
        builder.addParameter("name", "John Doe & Sons");
        assertEquals("http://example.com?name=John+Doe+%26+Sons", builder.build());
    }

    @Test
    public void testEncodeURLString() {
        assertEquals("Hello+World", URLBuilder.encodeURLString("Hello World"));
        assertEquals("a%2Fb%2Fc", URLBuilder.encodeURLString("a/b/c"));
        assertEquals("%40%23%24%25", URLBuilder.encodeURLString("@#$%"));
    }

}

