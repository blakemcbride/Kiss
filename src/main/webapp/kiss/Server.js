/**
 * Created by Blake McBride on 9/24/16.
 */


'use strict';

/**
 * This class provides the facilities used to communicate with the back-end.
 */
class Server {

    static contextCreated = false;

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
     * @param {string} pkg the web service to be called
     * @param {string} cls  the web method
     * @param {object} injson data to be passed to the back-end
     *
     * @returns {Promise<any>} data returned from the back-end
     */
    static call(pkg, cls, injson) {

        const doCall = function (pkg, cls, injson, pass, resolve, reject) {
            const path = "rest";  // path to servlet
            if (!injson)
                injson = {};
            injson._uuid = Server.uuid;
            injson._method = "main";
            injson._package = pkg;
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
                        doCall(pkg, cls, injson, pass + 1, resolve, reject);
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
            doCall(pkg, cls, injson, 1, resolve, reject);
        });
    }
}

//# sourceURL=kiss/Server.js
