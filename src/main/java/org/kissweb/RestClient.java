package org.kissweb;

import org.json.JSONObject;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.*;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;

/**
 * Author: Blake McBride
 * Date: 2/21/21
 */
public class RestClient {

    /**
     * Call a REST service sending a JSON object and returning a JSON object.
     *
     * @param method POST / GET / etc.
     * @param urlStr URL endpoint
     * @param out what is sent to the service
     * @param headers representing all of the headers
     * @param debugFileName file name to send debug output to or null
     * @return what is returned from the service
     * @throws IOException if error
     */
    public static JSONObject jsonCall(String method, String urlStr, JSONObject out, JSONObject headers, String debugFileName) throws IOException {
        return jsonCall(method, urlStr, out.toString(), headers, debugFileName);
    }

    /**
     * Call a REST service sending a String but returning an XML document.
     *
     * @param method POST / GET / etc.
     * @param urlStr URL endpoint
     * @param outStr what is sent to the service
     * @param headers representing all of the headers
     * @param debugFileName file name to send debug output to or null
     * @return what is returned from the service
     * @throws IOException if error
     * @throws ParserConfigurationException if error
     * @throws SAXException if error
     */
    public static Document xmlCall(String method, String urlStr, String outStr, JSONObject headers, String debugFileName) throws IOException, ParserConfigurationException, SAXException {
        String res = performService(method, urlStr, outStr, headers);
        if (debugFileName != null)
            Files.write(Paths.get(debugFileName), res.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
        return parseXML(res);
    }

    /**
     * Call a REST service sending a string but returning a JSON object.
     *
     * 'headers' is a JSON object.  Each element is a header / value combination.  These are sent as the header of
     *  the REST call.
     *
     * @param method POST / GET / etc.
     * @param urlStr URL endpoint
     * @param outStr what is sent to the service
     * @param headers representing all of the headers
     * @param debugFileName file name to send debug output to or null
     * @return the JSON return from the call
     * @throws IOException if the communication fail, an exception is thrown
     */
    public static JSONObject jsonCall(String method, String urlStr, String outStr, JSONObject headers, String debugFileName) throws IOException {
        if (debugFileName != null)
            Files.write(Paths.get("S-" + debugFileName), outStr.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
        String res = performService(method, urlStr, outStr, headers);
        if (debugFileName != null)
            Files.write(Paths.get("R-" + debugFileName), res.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
        return new JSONObject(res);
    }

    /**
     * Performs the web service call.  Sends and returns raw strings.
     *
     * 'headers' is a JSON object.  Each element is a header / value combination.  These are sent as the header of
     * the REST call.
     *
     * @param method POST / GET / etc.
     * @param urlStr the URL endpoint
     * @param outStr what is sent to the service
     * @param headers representing all of the headers
     * @return the string returned from the call
     * @throws IOException if the communication fail, an exception is thrown
     */
    public static String performService(String method, String urlStr, String outStr, JSONObject headers) throws IOException {
        HttpURLConnection con = null;
        OutputStreamWriter out = null;
        BufferedReader in = null;
        StringBuilder res = new StringBuilder();

        try {
            URL url = new URL(urlStr);
            con = (HttpURLConnection) url.openConnection();
            con.setRequestMethod(method);
            for (String header : headers.keySet())
                con.setRequestProperty(header, headers.getString(header));
            con.setDoInput(true);
            if (outStr != null &&  outStr.length() > 0) {
                con.setDoOutput(true);
                out = new OutputStreamWriter(con.getOutputStream());
                out.write(outStr);
                out.flush();
            }
            in = new BufferedReader(new InputStreamReader(con.getInputStream()));
            String line;
            while ((line = in.readLine()) != null)
                res.append(line);
        } finally {
            if (out != null)
                try {
                    out.close();
                } catch (Exception e) {
                    // ignore
                }
            if (in != null)
                try {
                    in.close();
                } catch (Exception e) {
                    // ignore
                }
            if (con != null)
                con.disconnect();
        }
        //       Files.write(Paths.get("abc.xml"), res.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
        return res.toString();
    }

    /**
     * Convert a string into an XML Document.
     */
    private static Document parseXML(String str) throws ParserConfigurationException, IOException, SAXException {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder builder = factory.newDocumentBuilder();
        return builder.parse(new InputSource(new StringReader(str)));
    }

}
