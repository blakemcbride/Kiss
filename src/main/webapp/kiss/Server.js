/* global Utils */

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
     * This function is typically called with an await and returns the result of the call.
     *
     * @param {string} cls the web service to be called
     * @param {string} meth  the web method
     * @param {object} injson data to be passed to the back-end
     *
     * @returns data returned from the back-end
     */
    static async call(cls, meth, injson) {

        const path = "rest";  // path to servlet
        if (!injson)
            injson = {};
        injson._uuid = Server.uuid;
        injson._method = meth;
        injson._class = cls;

        const doCall = async function (cls, meth, injson, pass) {
            let response;
            try {
                response = await fetch(Server.url + '/' + path, {
                    method: 'POST',
                    body: JSON.stringify(injson),
                    headers: {
                        'Content-Type': 'application/json'
                    }
                });
            } catch (err) {
                return await processError(cls, meth, injson, 1, err);
            }
            let a = 1;
            try {
                let b = 1;
                let c = await response.json();
                let d = 1;
                return c;
            } catch (err) {
                return await processError(cls, meth, injson, 1, err);
            }
        };

        const processError = async function(cls, meth, injson, pass, err) {
            if (pass < 3)
                return await doCall(cls, meth, injson, pass + 1);
            const msg = 'Error communicating with the server.';
            Utils.showMessage('Error', msg);
            return {_Success: false, _ErrorMessage: msg};
        };

        let r = await doCall(cls, meth, injson, 1);
        let g = 1;
        return r;


        //////////////

        const doCall_old = function (cls, meth, injson, pass, resolve, reject) {
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
                        Utils.showMessage('Error', data._ErrorMessage);
                    resolve(data);
                },
                error: function (error, status) {
                    if (pass < 3) {
                        doCall_old(cls, meth, injson, pass + 1, resolve, reject);
                        return;
                    }
                    const msg = 'Error communicating with the server.';
                    Utils.showMessage('Error', msg);
                    resolve({_Success: false, _ErrorMessage: msg});
                    //reject(error, status);
                }
            });
        };

        return new Promise(function (resolve, reject) {
            doCall_old(cls, meth, injson, 1, resolve, reject);
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
     * @see Utils.getFileUploadCount
     * @see Utils.getFileUploadFormData
     */
    static fileUploadSend(cls, meth, data) {
        return new Promise(function (resolve, reject) {
            data.append('_class', cls);
            data.append('_method', meth);
            Utils.waitMessage("File upload in progress.");
            $.ajax({
                url: Server.url + '/rest',
                type: 'POST',
                processData: false,
                contentType: false,
                data: data,
                dataType: 'json',  // what is coming back
                cache: false,
                success: function (res, status, hdr) {
                    Utils.waitMessageEnd();
                    if (res._Success)
                        Utils.showMessage("Information", "Upload successful.");
                    else
                        Utils.showMessage("Error", res._ErrorMessage);
                    resolve(res);
                },
                error: function (hdr, status, error) {
                    const msg = 'Error communicating with the server.';
                    Utils.waitMessageEnd();
                    Utils.showMessage("Error", msg);
                    resolve({_Success: false, _ErrorMessage: msg});
                }
            });
        });
    }

}

Server.contextCreated = false;



//# sourceURL=kiss/Server.js
