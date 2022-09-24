package org.kissweb;

import org.json.JSONObject;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
import javax.net.ssl.SSLSocketFactory;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import java.io.*;
import java.net.*;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.nio.file.StandardOpenOption;
import java.security.*;
import java.security.cert.CertificateException;
import java.util.Base64;
import org.json.JSONException;

/**
 * Provides the ability to act as a client to an external REST server.
 * Supports text, XML, and JSON communications.
 * <br><br>
 * Author: Blake McBride<br>
 * Date: 2/21/21
 */
public class RestClient {

    private String proxyServerURL = null;
    private int proxyServerPort;
    private SSLContext context = null;
    private int responseCode;
    private String responseString;
    private String debugFileName;

    public RestClient setProxy(String proxyServerURL, int proxyServerPort) {
        this.proxyServerURL = proxyServerURL;
        this.proxyServerPort = proxyServerPort;
        return this;
    }

    /**
     * Set a TLS key.
     *
     * @param clientKeystoreType
     * @param clientKeystorePath
     * @param clientKeystorePassword
     * @return
     * @throws KeyStoreException
     * @throws IOException
     * @throws CertificateException
     * @throws NoSuchAlgorithmException
     * @throws UnrecoverableKeyException
     * @throws KeyManagementException
     */
    public RestClient setTLSKey(String clientKeystoreType, String clientKeystorePath, String clientKeystorePassword) throws KeyStoreException, IOException, CertificateException, NoSuchAlgorithmException, UnrecoverableKeyException, KeyManagementException {
        File pKeyFile = new File(clientKeystorePath);
        KeyManagerFactory keyManagerFactory = KeyManagerFactory.getInstance("SunX509");
        KeyStore keyStore = KeyStore.getInstance(clientKeystoreType);
        InputStream keyInput = new FileInputStream(pKeyFile);
        keyStore.load(keyInput, clientKeystorePassword.toCharArray());
        keyInput.close();
        keyManagerFactory.init(keyStore, clientKeystorePassword.toCharArray());
        context = SSLContext.getInstance("TLS");
        context.init(keyManagerFactory.getKeyManagers(), null, new SecureRandom());
        return this;
    }

    /**
     * Call a REST service sending a JSON object and returning a JSON object.
     *
     * @param method POST / GET / etc.
     * @param urlStr URL endpoint
     * @param out what is sent to the service
     * @param headers representing all of the headers
     * @return what is returned from the service
     * @throws IOException if error
     */
    public JSONObject jsonCall(String method, String urlStr, JSONObject out, JSONObject headers) throws IOException {
        return jsonCall(method, urlStr, out == null ? null : out.toString(), headers);
    }

    /**
     * Call a REST service sending nothing and returning a JSON object.
     *
     * @param method
     * @param urlStr
     * @return
     * @throws IOException
     */
    public JSONObject jsonCall(String method, String urlStr) throws IOException {
        return jsonCall(method, urlStr, (String) null, null);
    }

    /**
     * Call a REST service sending a String but returning an XML document.
     *
     * @param method POST / GET / etc.
     * @param urlStr URL endpoint
     * @param outStr what is sent to the service
     * @param headers representing all of the headers
     * @return what is returned from the service
     * @throws IOException if error
     * @throws ParserConfigurationException if error
     * @throws SAXException if error
     */
    public Document xmlCall(String method, String urlStr, String outStr, JSONObject headers) throws IOException, ParserConfigurationException, SAXException {
        int ires = performService(method, urlStr, outStr, headers);
        if (ires == HttpURLConnection.HTTP_OK) {
            if (debugFileName != null)
                Files.write(Paths.get(debugFileName), responseString.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
            return parseXML(responseString);
        }
        return null;
    }

    /**
     * Call a REST service sending nothing and returning an XML document.
     *
     * @param method
     * @param urlStr
     * @return
     * @throws IOException
     * @throws ParserConfigurationException
     * @throws SAXException
     */
    public Document xmlCall(String method, String urlStr) throws IOException, ParserConfigurationException, SAXException {
        return xmlCall(method, urlStr, null, null);
    }

    /**
     * Call a REST service sending a string and returning a JSON object.
     * <br><br>
     * 'headers' is a JSON object.  Each element is a header / value combination.  These are sent as the header of
     *  the REST call.
     *
     * @param method POST / GET / etc.
     * @param urlStr URL endpoint
     * @param outStr what is sent to the service
     * @param headers representing all of the headers
     * @return the JSON return from the call
     * @throws IOException if the communication fail, an exception is thrown
     */
    public JSONObject jsonCall(String method, String urlStr, String outStr, JSONObject headers) throws IOException {
        if (debugFileName != null)
            Files.write(Paths.get("S-" + debugFileName), outStr == null ? "".getBytes() : outStr.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
        int ires = performService(method, urlStr, outStr, headers);
        if (ires == HttpURLConnection.HTTP_OK) {
            if (debugFileName != null)
                Files.write(Paths.get("R-" + debugFileName), responseString.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
            try {
                return new JSONObject(responseString);
            } catch (JSONException e) {
                return null;
            }
        }
        return null;
    }

    /**
     * Performs the web service call.  Sends and returns raw strings.
     * <br><br>
     * 'headers' is a JSON object.  Each element is a header / value combination.  These are sent as the header of
     * the REST call.
     *
     * @param method POST / GET / etc.
     * @param urlStr the URL endpoint
     * @param outStr what is sent to the service
     * @param headers representing all of the headers
     * @return the HTTP return code
     * @throws IOException if the communication fail, an exception is thrown
     */
    public int performService(String method, String urlStr, String outStr, JSONObject headers) throws IOException {
        HttpURLConnection con = null;
        StringBuilder res = new StringBuilder();

        responseString = null;
        try {
            URL url = new URL(urlStr);

            // Proxy server support
            if (proxyServerURL != null && !proxyServerURL.isEmpty()) {
                Proxy proxy = new Proxy(Proxy.Type.HTTP, new InetSocketAddress(proxyServerURL, proxyServerPort));
                con = (HttpURLConnection) url.openConnection(proxy);
            } else
                con = (HttpURLConnection) url.openConnection();

            //  SSL support
            if (context != null) {
                SSLSocketFactory sockFact = context.getSocketFactory();
                ((HttpsURLConnection) con).setSSLSocketFactory(sockFact);
            }

            con.setConnectTimeout(30000);
            con.setReadTimeout(30000);
            con.setUseCaches(false);

            con.setRequestMethod(method);
            if (headers != null)
                for (String header : headers.keySet())
                    con.setRequestProperty(header, headers.getString(header));
            con.setDoInput(true);
            if (outStr != null && outStr.length() > 0) {
                con.setDoOutput(true);
                try (OutputStreamWriter out = new OutputStreamWriter(con.getOutputStream())) {
                    out.write(outStr);
                }
            }

            responseCode = con.getResponseCode();
            InputStream inputStream;

            if (responseCode >= 200  &&  responseCode < 300)
                inputStream = con.getInputStream();
            else
                inputStream = con.getErrorStream();
            if (inputStream != null) {
                try (BufferedReader in = new BufferedReader(new InputStreamReader(inputStream))) {
                    String line;
                    while ((line = in.readLine()) != null)
                        res.append(line);
                }
            }
        } finally {
            if (con != null)
                con.disconnect();
        }
        responseString = res.toString();
        return responseCode;
    }

    /**
     * The following builds a JSON header that implements HTTP basic authentication.
     * Additional items may be added to the returned header object.
     *
     * @param user
     * @param pw
     * @return a new JSON header object containing the basic authentication
     */
    public static JSONObject basicAuthenticationHeader(String user, String pw) {
        final JSONObject header = new JSONObject();
        final String valueToEncode = user + ":" + pw;
        header.put("Authorization", "Basic " + Base64.getEncoder().encodeToString(valueToEncode.getBytes()));
        return header;
    }

    /**
     * The HTTP response code
     *
     * @return
     */
    public int getResponseCode() {
        return responseCode;
    }

    /**
     * Returns the response string.
     *
     * @return
     */
    public String getResponseString() {
        return responseString;
    }

    /**
     * Set a file for debug output.
     *
     * @param fname
     * @return
     */
    public String setDebugFileName(String fname) {
        String pn = debugFileName;
        debugFileName = fname;
        return pn;
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
