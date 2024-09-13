package org.kissweb;

import org.junit.jupiter.api.Test;
import org.w3c.dom.Document;
import static org.junit.jupiter.api.Assertions.*;

public class XMLTest {

    @Test
    public void testParseValidXML() {
        String xml = "<root><child>Content</child></root>";
        Document doc = XML.parse(xml);
        assertNotNull(doc);
        assertEquals("root", doc.getDocumentElement().getNodeName());
    }

    @Test
    public void testParseInvalidXMLThrowsException() {
        String invalidXml = "<root><child>Content</child>";
        Exception exception = assertThrows(RuntimeException.class, () -> {
            XML.parse(invalidXml);
        });
        assertNotNull(exception);
    }

    @Test
    public void testFormatXMLDocument() {
        String xml = "<root><child>Content</child></root>";
        Document doc = XML.parse(xml);
        String formattedXML = XML.format(doc);

        String expectedFormattedXML = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
                                    + "<root>\n"
                                    + "  <child>Content</child>\n"
                                    + "</root>\n";
        assertEquals(expectedFormattedXML.trim(), formattedXML.trim());
    }

    @Test
    public void testFormatXMLString() {
        String unformattedXML = "<root><child>Content</child></root>";
        String formattedXML = XML.format(unformattedXML);

        String expectedFormattedXML = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
                                    + "<root>\n"
                                    + "  <child>Content</child>\n"
                                    + "</root>\n";
        assertEquals(expectedFormattedXML.trim(), formattedXML.trim());
    }

    @Test
    public void testFormatComplexXMLString() {
        String unformattedXML = "<root><child1>Content1</child1><child2>Content2</child2></root>";
        String formattedXML = XML.format(unformattedXML);

        String expectedFormattedXML = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n"
                                    + "<root>\n"
                                    + "  <child1>Content1</child1>\n"
                                    + "  <child2>Content2</child2>\n"
                                    + "</root>\n";
        assertEquals(expectedFormattedXML.trim(), formattedXML.trim());
    }
}

