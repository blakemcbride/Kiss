/**
 * Created by Blake McBride on 9/24/16.
 */


'use strict';

/**
 * This class provides the facilities used to communicate with the back-end.
 */
class Server {

    /**
     * Set the URL of the back-end.
     *
     * @param {string} url
     */
    static setURL(url) {
        Server.url = url;
    }

    // internal
    static setUUID(uuid) {
        Server.uuid = uuid;
    }

    /**
     *  Removes the user association between the back-end and front-end.
     */
    static logout() {
        Server.uuid = '';
    }

    /**
     * Evoke a back-end REST service.
     *
     * This function is typically called with an await and returns the resule of the call.
     *
     * @param {string} cls the web service to be called
     * @param {string} meth  the web method
     * @param {object} injson data to be passed to the back-end
     *
     * @returns {Promise<any>} data returned from the back-end
     */
    static call(cls, meth, injson) {

        const doCall = function (cls, meth, injson, pass, resolve, reject) {
            const path = "rest";  // path to servlet
            if (!injson)
                injson = {};
            injson._uuid = Server.uuid;
            injson._method = meth;
            injson._class = cls;

            jQuery.ajax({
                type: 'POST',
                url: Server.url + '/' + path,
                data: JSON.stringify(injson),
                contentType: 'application/json',
                dataType: 'json',
                success: function (data, status, hdr) {
                    if (!data._Success)
                        utils.showMessage('Error', data._ErrorMessage);
                    resolve(data);
                },
                error: function (error, status) {
                    if (pass < 3) {
                        doCall(cls, meth, injson, pass + 1, resolve, reject);
                        return;
                    }
                    const msg = 'Error communicating with the server.';
                    utils.showMessage('Error', msg);
                    resolve({_Success: false, _ErrorMessage: msg});
                    //reject(error, status);
                }
            });
        };

        return new Promise(function (resolve, reject) {
            doCall(cls, meth, injson, 1, resolve, reject);
        });
    }

    /**
     * Send the file upload to the server.
     * This method displays a wait message and a final status message.
     *
     * @param {string} cls
     * @param {string} meth
     * @param {FormData} data
     *
     * @see utils.getFileUploadCount
     * @see utils.getFileUploadFormData
     */
    static fileUploadSend(cls, meth, data) {
        return new Promise(function (resolve, reject) {
            data.append('_class', cls);
            data.append('_method', meth);
            utils.waitMessage("File upload in progress.");
            $.ajax({
                url: Server.url + '/rest',
                type: 'POST',
                processData: false,
                contentType: false,
                data: data,
                dataType: 'json',  // what is coming back
                cache: false,
                success: function (res, status, hdr) {
                    utils.waitMessageEnd();
                    if (res._Success)
                        utils.showMessage("Information", "Upload successful.");
                    else
                        utils.showMessage("Error", res._ErrorMessage);
                    resolve(res);
                },
                error: function (hdr, status, error) {
                    const msg = 'Error communicating with the server.';
                    utils.waitMessageEnd();
                    utils.showMessage("Error", msg);
                    resolve({_Success: false, _ErrorMessage: msg});
                }
            });
        });
    }

}

Server.contextCreated = false;



//# sourceURL=kiss/Server.js
