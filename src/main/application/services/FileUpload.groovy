package services

import org.json.JSONObject
import org.kissweb.database.Connection
import org.kissweb.restServer.ProcessServlet

/**
 * Author: Blake McBride
 * Date: 7/24/20
 */
class FileUpload {

    void upload(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        int numberOfFiles = servlet.getUploadFileCount()
        String fileName = servlet.getUploadFileName(0)  // name of first file
        BufferedInputStream is = servlet.getUploadBufferedInputStream(0)  // get the first one
        // do what you want with the uploaded file
        is.close()
    }
}
