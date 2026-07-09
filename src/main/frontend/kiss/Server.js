/* global Utils, AppState, Router, DOMUtils */

/**
 * Created by Blake McBride on 9/24/16.
 */


'use strict';

/**
 * This class provides the facilities used to communicate with the back-end.
 */
class Server {

    static #numberOfRetries = 1;

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
        AppState.set('_uuid', uuid);
        if (uuid) {
            Server.logoutInProgress = false;
            Server.sessionExpiredPromise = null;
        }
    }

    /**
     * The current session token (UUID).  Persisted through AppState so it
     * survives page navigation/reload according to SystemInfo.stateStore.
     *
     * @returns {string}
     */
    static get uuid() {
        return AppState.get('_uuid') || '';
    }

    // internal: the server boot id seen when this session was established
    static setBootId(id) {
        AppState.set('_bootId', id);
    }

    static getBootId() {
        return AppState.get('_bootId');
    }

    /**
     * Drop the persisted session so a fresh login is required.  Called by the login
     * screen, so that returning to it (e.g. via the browser Back button) does not leave
     * the user silently logged in — a real re-login is required to proceed.
     * <br><br>
     * Synchronous and non-navigating (the login screen is already showing).  Clearing the
     * client token is what forces re-authentication; the back-end session is left to
     * expire on its own (it is unusable once the token is gone).
     */
    static clearSession() {
        AppState.clear();
    }

    /**
     * Detect a back-end restart at startup.  If a session is persisted but the server's
     * boot id has changed since that session was established, the back end was rebooted —
     * drop the stale session so the user is forced to re-login instead of resuming onto a
     * dead session.  Call once at startup, before routing begins.
     */
    static async verifyServerInstance() {
        if (!Server.uuid)
            return;   // no persisted session to protect
        const stored = Server.getBootId();
        let res;
        try {
            res = await Server.call('', 'LoginRequired', {});   // unauthenticated; returns _BootId
        } catch (err) {
            return;   // transient error — let the normal flow handle it
        }
        const current = res ? res._BootId : null;
        if (!current)
            return;
        if (stored && stored !== current) {
            AppState.clear();   // stale session from a previous server instance
            await Utils.showMessage('Notice', 'The server was restarted.  Please log in again.');
        } else if (!stored) {
            Server.setBootId(current);
        }
    }

    /**
     * Logs out the current user and reloads the page.
     * <br><br>
     * This calls the backend Logout service to properly terminate the session,
     * then performs a full page reload to clear all context and return to the login screen.
     *
     */
    static async logout(captureReturn = false, skipBackend = false) {
        if (Server.logoutInProgress)
            return;
        Server.logoutInProgress = true;
        Utils.suspendDepth = 0;
        document.body.style.cursor = '';
        Utils.cleanup();  //  clean up any context information
        DOMUtils.preventNavigation(false);  //  disable back button protection

        // Call backend Logout service if we have a valid UUID.  This intentionally
        // avoids Server.call(), because logout may run while the session is already
        // expired and should never show another framework error popup.
        if (!skipBackend && Server.uuid) {
            try {
                await fetch(Server.url + '/rest', {
                    method: 'POST',
                    cache: 'no-store',
                    body: JSON.stringify({
                        _uuid: Server.uuid,
                        _method: 'Logout',
                        _class: ''
                    }),
                    headers: {
                        'Content-Type': 'application/json'
                    }
                });
            } catch (err) {
                // Ignore errors - we're logging out anyway
                console.log('Logout service call failed:', err);
            }
        }

        AppState.clear();
        //  Return to the login screen.  Use the router when active (no full reload);
        //  fall back to a reload for apps that don't use the router.
        if (typeof Router !== 'undefined' && Router.isStarted())
            Router.gotoLogin(captureReturn);
        else
            location.reload();
    }

    static async handleSessionExpired(message, title = 'Error') {
        if (Server.sessionExpiredPromise)
            return Server.sessionExpiredPromise;
        Server.sessionExpiredPromise = (async () => {
            DOMUtils.preventNavigation(false);
            await Utils.showMessage(title, message || 'You have been logged out. Please log in again.');
            await Server.logout(true, true);
        })();
        return Server.sessionExpiredPromise;
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
        if (!await Server.checkTime())
            return {_Success: false, _ErrorCode: 2, _ErrorMessage: Server.sessionExpiredMessage};
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
                    cache: 'no-store',
                    body: JSON.stringify(injson),
                    headers: {
                        'Content-Type': 'application/json'
                    }
                });
            } catch (err) {
                if (pass < Server.#numberOfRetries)
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
                        await Server.handleSessionExpired(res._ErrorMessage);
                    } else
                        await Utils.showMessage('Error', res._ErrorMessage);
                resolve(res);
            } catch (err) {
                if (pass < Server.#numberOfRetries)
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
     * Evoke a back-end REST service without changing the global busy count.
     * Use this for lightweight background searches where the UI owns any loading
     * indication and the whole app should not look blocked.  Unlike {@link Server.call},
     * transport/parse failures do not raise the framework error popup — the caller gets
     * a <code>{_Success:false}</code> result to handle quietly.
     *
     * @param cls
     * @param meth
     * @param injson
     * @returns {Promise<*>}
     *
     * @see Server.call
     */
    static async callQuiet(cls, meth, injson=null) {
        if (!await Server.checkTime())
            return {_Success: false, _ErrorCode: 2, _ErrorMessage: Server.sessionExpiredMessage};
        const path = "rest";
        if (!injson)
            injson = {};
        else
            injson = { ...injson };
        injson._uuid = Server.uuid;
        injson._method = meth;
        injson._class = cls;

        try {
            const response = await fetch(Server.url + '/' + path, {
                method: 'POST',
                cache: 'no-store',
                body: JSON.stringify(injson),
                headers: {
                    'Content-Type': 'application/json'
                }
            });
            const res = await response.json();
            if (!res._Success && res._ErrorCode === 2) {
                await Server.handleSessionExpired(res._ErrorMessage);
            }
            return res;
        } catch (err) {
            console.log("Server communication error (quiet): " + cls + "." + meth + "(): " + err.message);
            return {_Success: false, _ErrorMessage: Server.errorMessage};
        }
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
        if (!await Server.checkTime())
            return {_Success: false, _ErrorCode: 2, _ErrorMessage: Server.sessionExpiredMessage};
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
                    cache: 'no-store',
                    body: JSON.stringify(injson),
                    headers: {
                        'Content-Type': 'application/json'
                    }
                });
            } catch (err) {
                if (pass < Server.#numberOfRetries)
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
                        await Server.handleSessionExpired(ret._ErrorMessage);
                    } else
                        await Utils.showMessage('Error', ret._ErrorMessage);
                ret._data = bytes.slice(i, bytes.length);
                resolve(ret);
            } catch (err) {
                if (pass < Server.#numberOfRetries)
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
        Utils.suspendDepth++;
        Server.clearBusyCursor();
    }

    static decCount() {
        if (Utils.suspendDepth > 0)
            Utils.suspendDepth--;
        else
            Utils.suspendDepth = 0;
        Server.clearBusyCursor();
    }

    static clearBusyCursor() {
        if (document.body && (document.body.style.cursor === 'wait' || document.body.style.cursor === 'progress'))
            document.body.style.cursor = '';
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
    static async fileUploadSend(cls, meth, fd, injson=null, waitMsg, successMessage) {
        if (!await Server.checkTime())
            return {_Success: false, _ErrorCode: 2, _ErrorMessage: Server.sessionExpiredMessage};
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

            (async () => {
                try {
                    const response = await fetch(Server.url + '/rest', {
                        method: 'POST',
                        cache: 'no-store',
                        body: fd  // FormData - browser sets Content-Type automatically
                    });
                    const res = await response.json();
                    Utils.waitMessageEnd();
                    Server.decCount();
                    if (res._Success) {
                        if (successMessage)
                            await Utils.showMessage("Information", successMessage);
                    } else if (res._ErrorCode === 2) {
                        await Server.handleSessionExpired(res._ErrorMessage);
                    } else
                        await Utils.showMessage("Error", res._ErrorMessage);
                    resolve(res);
                } catch (err) {
                    Utils.waitMessageEnd();
                    Server.decCount();
                    await Utils.showMessage("Error", Server.errorMessage);
                    resolve({_Success: false, _ErrorMessage: Server.errorMessage});
                }
            })();
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
    static async callAll(pa /*, ... each subsequent arg is a function to handle the result of the next promise in pa */) {
        if (!await Server.checkTime())
            return true;
        const args = arguments;
        return new Promise(function (resolve, reject) {
            Promise.all(pa).then(function (ret) {
                for (let i = 0; i < ret.length; i++)
                    if (!ret[i]._Success) {
                        if (ret[i]._ErrorCode === 2)
                            Server.handleSessionExpired(ret[i]._ErrorMessage);
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
        if (Server.logoutInProgress || !Server.maxInactiveSeconds)
            return true;
        const now = (new Date()).getTime() / 1000;
        if (now - Server.timeLastCall > Server.maxInactiveSeconds) {
            await Server.handleSessionExpired(Server.sessionExpiredMessage, 'Warning');
            return false;
        }
        Server.timeLastCall = now;
        return true;
    }
}

    // class variables
Server.errorMessage = 'Error communicating with the server.';
Server.sessionExpiredMessage = 'You have been logged out due to inactivity. Please log in again.';
Server.timeLastCall;
Server.maxInactiveSeconds = 0;  // max number of seconds between calls or zero for no max (or auto logout)
Server.logoutInProgress = false;
Server.sessionExpiredPromise = null;
