package org.kissweb;

import org.kissweb.json.JSONArray;
import org.kissweb.json.JSONObject;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import javax.net.ssl.KeyManagerFactory;
import javax.net.ssl.SSLContext;
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
import org.kissweb.json.JSONException;

import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.net.http.HttpTimeoutException;
import java.time.Duration;
import java.net.ProxySelector;
import java.net.ConnectException;
import java.util.Locale;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.CompletionException;

/**
 * Provides the ability to act as a client to an external REST server.
 * Supports text, XML, and JSON communications.
 * <br><br>
 * Author: Blake McBride<br>
 * Date: 2/21/21
 */
public class RestClient {

    /**
     * Constructs a new RestClient instance.
     */
    public RestClient() {
    }

    private String proxyServerURL = null;
    private int proxyServerPort;
    private SSLContext context = null;
    private int responseCode;
    private String responseString;
    private String debugFileName;

    // --- New HTTP/2‑capable client and retry policy configuration ---
    private volatile HttpClient httpClient = null;
    private int maxRetries = 1;
    private long retryBackoffMillis = 500;

    // --- Configurable timeouts (defaults are 60 s) ---
    private Duration connectTimeout = Duration.ofSeconds(60);
    private Duration requestTimeout = Duration.ofSeconds(60);

    /**
     * Set proxy server configuration for HTTP requests.
     *
     * @param proxyServerURL the URL of the proxy server
     * @param proxyServerPort the port number of the proxy server
     * @return this RestClient instance for method chaining
     */
    public RestClient setProxy(String proxyServerURL, int proxyServerPort) {
        this.proxyServerURL = proxyServerURL;
        this.proxyServerPort = proxyServerPort;
        this.httpClient = null;
        return this;
    }

    /**
     * Set a TLS key.
     *
     * @param clientKeystoreType the type of the keystore (e.g., "JKS", "PKCS12")
     * @param clientKeystorePath the file path to the keystore
     * @param clientKeystorePassword the password for the keystore
     * @return this RestClient instance for method chaining
     * @throws KeyStoreException if there is an error with the keystore
     * @throws IOException if there is an I/O error reading the keystore
     * @throws CertificateException if there is an error with certificates
     * @throws NoSuchAlgorithmException if the specified algorithm is not available
     * @throws UnrecoverableKeyException if the key cannot be recovered
     * @throws KeyManagementException if there is an error managing keys
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
        this.httpClient = null;
        return this;
    }

    /**
     * Call a REST service sending a JSON object and returning a JSON object.
     *
     * @param method POST / GET / etc.
     * @param urlStr URL endpoint
     * @param out what is sent to the service
     * @param headers representing all the headers
     * @return what is returned from the service
     * @throws IOException if error
     */
    public JSONObject jsonCall(String method, String urlStr, JSONObject out, JSONObject headers) throws IOException {
        return jsonCall(method, urlStr, out == null ? null : out.toString(), headers);
    }

    /**
     * Call a REST service sending a JSON object and returning a JSON object.
     *
     * @param method the HTTP method (POST, GET, etc.)
     * @param urlStr the URL endpoint
     * @param out the JSON object to send to the service
     * @return the JSON object returned from the service
     * @throws IOException if an I/O error occurs
     */
    public JSONObject jsonCall(String method, String urlStr, JSONObject out) throws IOException {
        return jsonCall(method, urlStr, out, null);
    }

    /**
     * Call a REST service sending a JSON array and returning a JSON object.
     *
     * @param method POST / GET / etc.
     * @param urlStr URL endpoint
     * @param out what is sent to the service
     * @param headers representing all the headers
     * @return what is returned from the service
     * @throws IOException if error
     */
    public JSONObject jsonCall(String method, String urlStr, JSONArray out, JSONObject headers) throws IOException {
        return jsonCall(method, urlStr, out == null ? null : out.toString(), headers);
    }

    /**
     * Call a REST service sending nothing and returning a JSON object.
     *
     * @param method the HTTP method (POST, GET, etc.)
     * @param urlStr the URL endpoint
     * @return the JSON object returned from the service
     * @throws IOException if an I/O error occurs
     */
    public JSONObject jsonCall(String method, String urlStr) throws IOException {
        return jsonCall(method, urlStr, (String) null, null);
    }

    /**
     * Call a REST service sending a String but returning an XML document.
     *
     * @param method POST / GET / etc.
     * @param urlStr URL endpoint
     * @param outStr what is sent to the service or null
     * @param headers representing all the headers or null
     * @return what is returned from the service
     * @throws IOException if error
     * @throws ParserConfigurationException if error
     * @throws SAXException if error
     */
    public Document xmlCall(String method, String urlStr, String outStr, JSONObject headers) throws IOException, ParserConfigurationException, SAXException {
        int ires = performService(method, urlStr, outStr, headers);
        if (ires >= 200  &&  ires < 300) {
            if (debugFileName != null)
                Files.write(Paths.get(debugFileName), responseString.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
            return parseXML(responseString);
        }
        return null;
    }

    /**
     * Call a REST service sending nothing and returning an XML document.
     *
     * @param method the HTTP method (POST, GET, etc.)
     * @param urlStr the URL endpoint
     * @return the XML document returned from the service
     * @throws IOException if an I/O error occurs
     * @throws ParserConfigurationException if there is a parser configuration error
     * @throws SAXException if there is a SAX parsing error
     */
    public Document xmlCall(String method, String urlStr) throws IOException, ParserConfigurationException, SAXException {
        return xmlCall(method, urlStr, null, null);
    }

    /**
     * Call a REST service sending a String and returning a String.
     * <br><br>
     * 'headers' is a JSON object.  Each element is a header / value combination.  These are sent as the header of
     *  the REST call.
     *
     * @param method POST / GET / etc.
     * @param urlStr URL endpoint
     * @param outStr what is sent to the service or null
     * @param headers representing all the headers or null
     * @return the String return from the call
     * @throws IOException if the communication fail, an exception is thrown
     */
    public String strCall(String method, String urlStr, String outStr, JSONObject headers) throws IOException {
        if (debugFileName != null)
            Files.write(Paths.get("S-" + debugFileName), outStr == null ? "".getBytes() : outStr.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
        int ires = performService(method, urlStr, outStr, headers);
        if (ires >= 200  &&  ires < 300) {
            if (debugFileName != null)
                Files.write(Paths.get("R-" + debugFileName), responseString.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
            try {
                return responseString;
            } catch (JSONException e) {
                return null;
            }
        }
        return null;
    }

    /**
     * Call a REST service sending a String and returning a String.
     *
     * @param method POST / GET / etc.
     * @param urlStr URL endpoint
     * @param outStr what is sent to the service or null
     * @return the String return from the call
     * @throws IOException if the communication fail, an exception is thrown
     */
    public String strCall(String method, String urlStr, String outStr) throws IOException {
        return strCall(method, urlStr, outStr, null);
    }

    /**
     * Call a REST service returning a String.
     *
     * @param method POST / GET / etc.
     * @param urlStr URL endpoint
     * @return the String return from the call
     * @throws IOException if the communication fail, an exception is thrown
     */
    public String strCall(String method, String urlStr) throws IOException {
        return strCall(method, urlStr, null, null);
    }

    /**
     * Call a REST service sending a string and returning a JSON object.
     * <br><br>
     * 'headers' is a JSON object.  Each element is a header / value combination.  These are sent as the header of
     *  the REST call.
     *
     * @param method POST / GET / etc.
     * @param urlStr URL endpoint
     * @param outStr what is sent to the service or null
     * @param headers representing all the headers or null
     * @return the JSON return from the call
     * @throws IOException if the communication fail, an exception is thrown
     */
    public JSONObject jsonCall(String method, String urlStr, String outStr, JSONObject headers) throws IOException {
        if (debugFileName != null)
            Files.write(Paths.get("S-" + debugFileName), outStr == null ? "".getBytes() : outStr.getBytes(), StandardOpenOption.CREATE, StandardOpenOption.WRITE, StandardOpenOption.APPEND);
        int ires = performService(method, urlStr, outStr, headers);
        if (ires >= 200  &&  ires < 300) {
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
     * Call a REST service sending a string and returning a JSON object.
     * <br><br>
     * 'headers' is a JSON object.  Each element is a header / value combination.  These are sent as the header of
     *  the REST call.
     *
     * @param method POST / GET / etc.
     * @param urlStr URL endpoint
     * @param outStr what is sent to the service
     * @return the JSON return from the call
     * @throws IOException if the communication fail, an exception is thrown
     */
    public JSONObject jsonCall(String method, String urlStr, String outStr) throws IOException {
        return jsonCall(method, urlStr, outStr, null);
    }

    private static final Set<String> RESTRICTED_HEADERS =
            Set.of("connection", "content-length", "expect", "host", "upgrade");

    /**
     * Performs the web service call.  Sends and returns raw strings.
     * <br><br>
     * 'headers' is a JSON object.  Each element is a header / value combination.  These are sent as the header of
     * the REST call.
     *
     * @param method POST / GET / etc.
     * @param urlStr the URL endpoint
     * @param outStr what is sent to the service or null
     * @param headers representing all the headers or null
     * @return the HTTP return code
     * @throws IOException if the communication fail, an exception is thrown
     */
    public int performService(String method,
                              String urlStr,
                              String outStr,
                              JSONObject headers) throws IOException {

        // Reset previous response state
        responseString = null;

        int  attempts = 0;
        long delay    = retryBackoffMillis;   // initial back-off

        while (true) {
            attempts++;
            try {
                HttpRequest.Builder builder = HttpRequest.newBuilder()
                        .uri(URI.create(urlStr))
                        .timeout(requestTimeout)
                        .method(method.toUpperCase(Locale.ROOT),
                                (outStr == null || outStr.isEmpty())
                                        ? HttpRequest.BodyPublishers.noBody()
                                        : HttpRequest.BodyPublishers.ofString(outStr));

                // Add caller-supplied headers, skipping the forbidden ones
                if (headers != null) {
                    for (String name : headers.keySet()) {
                        if (!RESTRICTED_HEADERS.contains(name.toLowerCase(Locale.ROOT))) {
                            builder.header(name, headers.getString(name));
                        }
                    }
                }

                HttpResponse<String> resp =
                        ensureHttpClient().send(builder.build(),
                                HttpResponse.BodyHandlers.ofString());

                responseCode   = resp.statusCode();
                responseString = resp.body() == null ? "" : resp.body();
                return responseCode;                 // success ─> leave the loop

            } catch (IOException | InterruptedException e) {
                boolean retriable =
                        e instanceof ConnectException ||
                                e instanceof java.net.SocketTimeoutException ||
                                e instanceof HttpTimeoutException ||
                                (e instanceof IOException && e.getMessage() != null && e.getMessage().contains("GOAWAY"));

                if (!retriable || attempts >= maxRetries) {
                    if (e instanceof InterruptedException) {
                        Thread.currentThread().interrupt();
                    }
                    throw new IOException(
                            "HTTP request failed after " + attempts + " attempt(s): "
                                    + e.getMessage(), e);
                }

                // Exponential back-off before the next retry
                try {
                    TimeUnit.MILLISECONDS.sleep(delay);
                } catch (InterruptedException ie) {
                    Thread.currentThread().interrupt();
                    throw new IOException("Retry interrupted", ie);
                }
                delay *= 2;   // double the wait for the next loop
            }
        }
    }

    /**
     * Lazily build or rebuild the underlying HttpClient, taking into account
     * proxy and TLS context settings for this RestClient instance.
     */
    private HttpClient ensureHttpClient() {
        if (httpClient == null) {
            synchronized (this) {
                if (httpClient == null) {
                    HttpClient.Builder b = HttpClient.newBuilder()
                            .version(HttpClient.Version.HTTP_2)
                            .connectTimeout(connectTimeout);
                    if (context != null)
                        b.sslContext(context);
                    if (proxyServerURL != null && !proxyServerURL.isEmpty())
                        b.proxy(ProxySelector.of(new InetSocketAddress(proxyServerURL, proxyServerPort)));
                    httpClient = b.build();
                }
            }
        }
        return httpClient;
    }

    /**
     * Optional: customise simple retry behaviour.
     *
     * @param retries       maximum number of attempts (≥1)
     * @param backoffMillis initial back‑off in milliseconds
     * @return this RestClient instance
     */
    public RestClient setRetryPolicy(int retries, long backoffMillis) {
        if (retries < 1)
            throw new IllegalArgumentException("retries must be ≥ 1");
        this.maxRetries = retries;
        this.retryBackoffMillis = backoffMillis;
        return this;
    }

    /**
     * Configure connection and request (read) timeouts.
     *
     * @param connect the maximum time allowed for establishing a TCP/TLS connection
     * @param request the maximum time allowed from sending the request until the response body is fully received
     * @return this RestClient instance for method chaining
     */
    public RestClient setTimeouts(Duration connect, Duration request) {
        if (connect != null)
            this.connectTimeout = connect;
        if (request != null)
            this.requestTimeout = request;
        this.httpClient = null;   // force rebuild with new connect timeout
        return this;
    }

    /**
     * The following builds a JSON header that implements HTTP basic authentication.
     * Additional items may be added to the returned header object.
     *
     * @param user the username for authentication
     * @param pw the password for authentication
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
     * @return the HTTP response code from the last request
     */
    public int getResponseCode() {
        return responseCode;
    }

    /**
     * Returns the response string.
     *
     * @return the response string from the last request
     */
    public String getResponseString() {
        return responseString;
    }

    /**
     * Set a file for debug output.
     *
     * @param fname the filename for debug output
     * @return the previous debug filename
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

    /**
     * Call a REST service with streaming response support.
     * This method is specifically designed for handling Server-Sent Events (SSE) streams.
     *
     * @param <T> the type of the response
     * @param req the HTTP request to send
     * @param h the handler for the response body
     * @return the response body handler
     * @throws IOException if the communication fails
     */
    public <T> HttpResponse<T> streamCall(HttpRequest req, HttpResponse.BodyHandler<T> h) throws IOException {
        try {
            return ensureHttpClient().sendAsync(req, h).join();
        } catch (CompletionException ce) {
            throw (ce.getCause() instanceof IOException io) ? io : new IOException("Async stream failed", ce.getCause());
        }
    }

}
