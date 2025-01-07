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
     * Reloads the top/main page from the server and clears all context.
     * <br><br>
     * This is a deep reload, meaning it will bypass any cached page content.
     * It is a full page reload, so it will also destroy any context information.
     * It also effectively works as a logout.
     *
     */
    static logout() {
        Utils.suspendDepth = 0;
        document.body.style.cursor = 'default';
        Utils.cleanup();  //  clean up any context information
        Server.uuid = '';
        window.onbeforeunload = null;  //  allow logout
        location.reload();
    }

    /**
     * Evoke a back-end REST service.
     * <br><br>
     * This function is typically called with an <code>await</code> or a <code>then</code> in order to process the result.
     *
     * @param {string} cls the web service to be called
     * @param {string} meth  the web method
     * @param {object} injson data to be passed to the back-end
     *
     * @returns data returned from the back-end
     */
    static async call(cls, meth, injson=null) {
        Server.checkTime();
        const path = "rest";  // path to servlet
        if (!injson)
            injson = {};
        else
            injson = { ...injson };  // shallow copy
        injson._uuid = Server.uuid;
        injson._method = meth;
        injson._class = cls;

        const doCall = async function (cls, meth, injson, pass, resolve, reject) {
            let response;
            if (pass === 1)
                Server.incCount();
            try {
                response = await fetch(Server.url + '/' + path, {
                    method: 'POST',
                    body: JSON.stringify(injson),
                    headers: {
                        'Content-Type': 'application/json'
                    }
                });
            } catch (err) {
                if (pass < 3)
                    return doCall(cls, meth, injson, pass + 1, resolve, reject);
                console.log("Server communication error (1): " + cls + "." + meth + "(): " + err.message);
                Server.decCount();
                await Utils.showMessage('Error', Server.errorMessage);
                resolve({_Success: false, _ErrorMessage: Server.errorMessage});
                return;
            }
            try {
                const res = await response.json();
                Server.decCount();
                if (!res._Success)
                    if (res._ErrorCode === 2) {
                        await Utils.showMessage('Error', res._ErrorMessage);
                        Server.logout();
                    } else
                        await Utils.showMessage('Error', res._ErrorMessage);
                resolve(res);
            } catch (err) {
                if (pass < 3)
                    return doCall(cls, meth, injson, pass + 1, resolve, reject);
                console.log("Server communication error (2): " + cls + "." + meth + "(): " + err.message);
                Server.decCount();
                await Utils.showMessage('Error', Server.errorMessage);
                resolve({_Success: false, _ErrorMessage: Server.errorMessage});
            }
        };

        return new Promise(function (resolve, reject) {
            doCall(cls, meth, injson, 1, resolve, reject);
        });

    }

    /**
     * Perform a binary call.  JSON is sent and JSON is returned.
     * However, a new element will be in the returned json called '_data'.
     * _data will contain the binary data.
     * This method is often used to retrieve images.
     * The back-end service should call <code>servlet.returnBinary()</code>
     *
     * @param cls
     * @param meth
     * @param injson
     * @returns {Promise<unknown>}
     *
     * @see Utils.toBase64
     */
    static async binaryCall(cls, meth, injson=null) {
        Server.checkTime();
        const path = "rest";  // path to servlet
        if (!injson)
            injson = {};
        else
            injson = { ...injson };  // shallow copy
        injson._uuid = Server.uuid;
        injson._method = meth;
        injson._class = cls;

        const doCall = async function (cls, meth, injson, pass, resolve, reject) {
            let response;
            if (pass === 1)
                Server.incCount();
            try {
                response = await fetch(Server.url + '/' + path, {
                    method: 'POST',
                    body: JSON.stringify(injson),
                    headers: {
                        'Content-Type': 'application/json'
                    }
                });
            } catch (err) {
                if (pass < 3)
                    return doCall(cls, meth, injson, pass + 1, resolve, reject);
                console.log("Server communication error (3): " + cls + "." + meth + "(): " + err.message);
                Server.decCount();
                await Utils.showMessage('Error', Server.errorMessage);
                resolve({_Success: false, _ErrorMessage: Server.errorMessage});
                return;
            }
            try {
                const res = await response.arrayBuffer();
                Server.decCount();
                if (!res) {
                    await Utils.showMessage('Error', Server.errorMessage);
                    resolve({_Success: false, _ErrorMessage: Server.errorMessage});
                }
                //               let str = String.fromCharCode.apply(null, new Uint8Array(res));    sometimes causes stack overflow
                const bytes = new Uint8Array(res);
                let json = '';
                let i = 0;
                let c = ' ';
                const m = bytes.length;
                while (c !== '\x03' && i < m) {
                    c = String.fromCharCode(bytes[i++]);
                    if (c !== '\x03')
                        json += c;
                }
                const ret = JSON.parse(json);
                if (!ret._Success)
                    if (ret._ErrorCode === 2) {
                        await Utils.showMessage('Error', ret._ErrorMessage);
                        Server.logout();
                    } else
                        await Utils.showMessage('Error', ret._ErrorMessage);
                ret._data = bytes.slice(i, bytes.length);
                resolve(ret);
            } catch (err) {
                if (pass < 3)
                    return doCall(cls, meth, injson, pass + 1, resolve, reject);
                Server.decCount();
                await Utils.showMessage('Error', Server.errorMessage);
                resolve({_Success: false, _ErrorMessage: Server.errorMessage});
            }
        };

        return new Promise(function (resolve, reject) {
            doCall(cls, meth, injson, 1, resolve, reject);
        });

    }

    static incCount() {
        if (++Utils.suspendDepth === 1)
            document.body.style.cursor = 'wait';
    }

    static decCount() {
        if (--Utils.suspendDepth === 0)
            document.body.style.cursor = 'default';
    }

    /**
     * Send the file upload to the server.
     * This method displays a wait message and a final status message.
     * <br><br>
     * <code>fd</code> can either be form data or it can be the ID of the file upload control.
     *
     * @param {string} cls
     * @param {string} meth
     * @param {FormData|string} fd ctl-id, FormData, FileList, or array of FileList
     * @param {object} injson
     * @param {string} waitMsg  optional wait message
     * @param {string} successMessage optional success message
     *
     * @see Utils.getFileUploadCount
     * @see Utils.getFileUploadFormData
     */
    static fileUploadSend(cls, meth, fd, injson=null, waitMsg, successMessage) {
        Server.checkTime();
        return new Promise(function (resolve, reject) {
            if (typeof fd === 'string')
                fd = $$(fd).getFormData();
            else if (Array.isArray(fd)) {
                const flst = fd;
                fd = new FormData();
                let i = 0;
                for (let j=0 ; j < flst.length ; j++) {
                    const files = flst[j];
                    if (Array.isArray(files))
                        for ( ; i < files.length ; i++)
                            fd.append('_file-' + i, 'S' + files[i]);
                    else
                        fd.append('_file-' + i++, 'S' + files);
                }
            } else if (fd instanceof FileList) {
                const files = fd;
                fd = new FormData();
                for (let i=0 ; i < files.length ; i++)
                    fd.append('_file-' + i, 'S' + files[i]);
            }
            fd.append('_class', cls);
            fd.append('_method', meth);
            fd.append("_uuid", Server.uuid);
            if (injson)
                for (let key in injson) {
                    let val = injson[key];
                    if (typeof val === 'object' && val !== null)
                        val = JSON.stringify(val);
                    else if (typeof val === 'string')
                        val = 'S' + val;
                    fd.append(key, val);
                }
            Utils.waitMessage(waitMsg ? waitMsg : "File upload in progress.");
            Server.incCount();
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
                    Server.decCount();
                    if (res._Success) {
                        if (successMessage)
                            await Utils.showMessage("Information", successMessage);
                    } else if (res._ErrorCode === 2) {
                        await Utils.showMessage("Error", res._ErrorMessage);
                        Server.logout();
                    } else
                        await Utils.showMessage("Error", res._ErrorMessage);
                    resolve(res);
                },
                error: async function (hdr, status, error) {
                    Utils.waitMessageEnd();
                    Server.decCount();
                    await Utils.showMessage("Error", Server.errorMessage);
                    resolve({_Success: false, _ErrorMessage: Server.errorMessage});
                }
            });
        });
    }

    /**
     * Used to call a number of simultaneous web services and wait till they're all done
     * before processing any of their results.
     * <br><br>
     * This function takes a variable number of arguments.
     * <br><br>
     * The first argument is an array of the Promises from each web service call.
     * <br><br>
     * Each remaining argument is a function that gets the result from the positionally corresponding
     * promise in the first argument.  If any are null there is no function executed for that returned promise.
     * Each function that gets executed gets passed the return value of the associated web service.
     * <br><br>
     * You can wait for this function to complete asynchronously by calling it with an await.
     * <br><br>
     * The return value is <code>false</code> if all the web services complete and <code>true</code> if there is an error.
     */
    static callAll(pa /*, ... each subsequent arg is a function to handle the result of the next promise in pa */) {
        Server.checkTime();
        const args = arguments;
        return new Promise(function (resolve, reject) {
            Promise.all(pa).then(function (ret) {
                for (let i = 0; i < ret.length; i++)
                    if (!ret[i]._Success) {
                        if (ret[i]._ErrorCode === 2)
                            Server.logout();
                        resolve(true);  //  error
                        return;
                    }
                for (let i=1 ; i < args.length  &&  i <= ret.length ; i++) {
                    let fun = args[i];
                    if (fun)
                        fun(ret[i-1]);
                }
                resolve(false);  //  success
            });
        });
    }

    /**
     * Set the maximum number of seconds between calls or zero for no max.
     * If the maximum number of seconds is exceeded, the user will be logged out
     * on their next attempt to make a service call.
     *
     * @param seconds
     */
    static setMaxInactivitySeconds(seconds) {
        Server.maxInactiveSeconds = seconds;
        Server.timeLastCall = (new Date()).getTime() / 1000; // seconds since 1970
    }

    /**
     * Set the maximum number of minutes between calls or zero for no max.
     * If the maximum number of minutes is exceeded, the user will be logged out
     * on their next attempt to make a service call.
     *
     * @param minutes
     */
    static setMaxInactivityMinutes(minutes) {
        Server.setMaxInactivitySeconds(minutes * 60);
    }

    /**
     * Set the maximum number of hours between calls or zero for no max.
     * If the maximum number of hours is exceeded, the user will be logged out
     * on their next attempt to make a service call.
     *
     * @param hours
     */
    static setMaxInactivityHours(hours) {
        Server.setMaxInactivitySeconds(hours * 60 * 60);
    }

    static async checkTime() {
        if (!Server.maxInactiveSeconds)
            return;
        const now = (new Date()).getTime() / 1000;
        if (now - Server.timeLastCall > Server.maxInactiveSeconds) {
            await Utils.showMessage("Warning", "Auto logout due to inactivity.  Please re-login.");
            Server.logout();
        } else
            Server.timeLastCall = now;
    }
}

    // class variables
Server.errorMessage = 'Error communicating with the server.';
Server.timeLastCall;
Server.maxInactiveSeconds = 0;  // max number of seconds between calls or zero for no max (or auto logout)




