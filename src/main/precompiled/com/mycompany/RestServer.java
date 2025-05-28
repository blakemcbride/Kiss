package com.mycompany;

import org.apache.log4j.Logger;
import org.kissweb.json.JSONObject;
import org.kissweb.RestServerBase;

import javax.servlet.annotation.MultipartConfig;
import javax.servlet.annotation.WebServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;


/**
 * Author: Blake McBride <br>
 * Date: 11/1/23
 * <br><br>
 * Sample REST server
 * <br><br>
 * Occasionally, it is necessary to have an external service call into your application. That's what this class handles.
 * Kiss itself uses more advanced methodology with build in authentication.  See <code>org.kissweb.restServer</code>
 * <br><br>
 * This class handles a more straight forward / common methodology that would be expected by an external server calling
 * into your application.  You must create your own version of this file for each service.
 * <br><br>
 * (1) This class is essentially a template.  You would copy this class and edit it for your particular needs.
 * It must reside under the <code>precompiled</code> directory.
 * <br><br>
 * (2) Be sure to set the <code>urlPattern</code>.
 * <br><br>
 * (3) If basic authentication is being used, enable it by setting <code></code> and set the <code>username</code> and <code>password</code>.
 * <br><br>
 * (4) Since this will be Java code, Kiss will have to be rebuilt.
 * <br><br>
 * REST servers must exist in the <code>precompiled</code> directory because they must be compiled
 * into the system rather than compiled at runtime because they are called by external
 * systems and must always be available.
 */
@WebServlet(urlPatterns="/myservice")
@MultipartConfig
public class RestServer extends RestServerBase {

    private static final Logger logger = Logger.getLogger(RestServer.class);

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws IOException {
        // If basic authentication is being used, do:
        if (!basicAuthenticate(request, response, null, "username", "password"))
            return;

        String someHeader = request.getHeader("SomeHeader");
        String contentType = request.getContentType();
        String val = request.getParameter("SomeURLArgument");

        // ************************************************************************************************
        // get the data they are sending you
        String body = getBodyString(request);
        //     or
        JSONObject jsonBody = getBodyJson(request);
        //     or
        byte [] binaryBody = getBodyBinary(request);
        // ************************************************************************************************


        // now you have all the data in body or bodyJson.  Do what you need with it here.


        // ************************************************************************************************
        // If you need to reply with data do something like this:
        /*
        setResponse(response, jsonObject);
             or
        setResponse(response, "text/html", htmlText);
             or
        setResponse(response, "image/jpeg", jpehBinary);
         */
        // ************************************************************************************************

    }

    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException {
        // respond to a GET request (like above example)
    }

    @Override
    protected void doPut(HttpServletRequest request, HttpServletResponse response) throws IOException {
        // respond to a PUT request (like above example)
    }

    @Override
    protected void doDelete(HttpServletRequest request, HttpServletResponse response) throws IOException {
        // respond to a DELETE request (like above example)
    }

}
