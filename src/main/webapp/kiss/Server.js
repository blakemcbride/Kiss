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
    static async call(cls, meth, injson=null) {

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
                return await processError(cls, meth, injson, pass, err);
            }
            try {
                let res =  await response.json();
                if (!res._Success)
                    await Utils.showMessage('Error', res._ErrorMessage);
                return res;
            } catch (err) {
                return await processError(cls, meth, injson, pass, err);
            }
        };

        const processError = async function(cls, meth, injson, pass, err) {
            if (pass < 3)
                return await doCall(cls, meth, injson, pass + 1);
            const msg = 'Error communicating with the server.';
            await Utils.showMessage('Error', msg);
            return {_Success: false, _ErrorMessage: msg};
        };

        return await doCall(cls, meth, injson, 1);
    }

    /**
     * Send the file upload to the server.
     * This method displays a wait message and a final status message.
     *
     * @param {string} cls
     * @param {string} meth
     * @param {FormData} fd
     * @param {object} injson
     *
     * @see Utils.getFileUploadCount
     * @see Utils.getFileUploadFormData
     */
    static fileUploadSend(cls, meth, fd, injson=null) {
        return new Promise(function (resolve, reject) {
            fd.append('_class', cls);
            fd.append('_method', meth);
            if (injson)
                for (let key in injson)
                    fd.append(key, injson[key]);
            Utils.waitMessage("File upload in progress.");
            $.ajax({
                url: Server.url + '/rest',
                type: 'POST',
                processData: false,
                contentType: false,
                data: fd,
                dataType: 'json',  // what is coming back
                cache: false,
                success: async function (res, status, hdr) {
                    Utils.waitMessageEnd();
                    if (res._Success)
                        await Utils.showMessage("Information", "Upload successful.");
                    else
                        await Utils.showMessage("Error", res._ErrorMessage);
                    resolve(res);
                },
                error: async function (hdr, status, error) {
                    const msg = 'Error communicating with the server.';
                    Utils.waitMessageEnd();
                    await Utils.showMessage("Error", msg);
                    resolve({_Success: false, _ErrorMessage: msg});
                }
            });
        });
    }

}

Server.contextCreated = false;



