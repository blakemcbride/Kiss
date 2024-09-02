package services

import org.json.JSONObject
import org.kissweb.database.Connection
import org.kissweb.restServer.ProcessServlet

/**
 * <code>FileUpload</code> is the name of the web service.  This is all you need.
 * There is no build process or configuration.  It can be added, changed, or deleted on a running system.
 * It gets auto-compiled and loaded whenever it changes.  It always runs at full compiled speed.
 */
class FileUpload {

    /**
     * <code>upload</code> is a complete web method.  When called by the front-end, the system authenticates the
     * call before this point is reached.  So you know all calls are authenticated.
     *
     * This method looks a little different because it is opening a stream between the front-end and back-end
     * to receive the file being sent by the front-end.
     *
     * @param injson json data from the front-end
     * @param outjson json data to be sent back to the front-end
     * @param db connection to the SQL database
     * @param servlet information specific to this particular call
     */
    void upload(JSONObject injson, JSONObject outjson, Connection db, ProcessServlet servlet) {
        int numberOfFiles = servlet.getUploadFileCount()
        String fileName = servlet.getUploadFileName(0)  // name of first file
        BufferedInputStream is = servlet.getUploadBufferedInputStream(0)  // get the first one
        // do what you want with the input stream (like save the file)
        is.close()
    }
}
