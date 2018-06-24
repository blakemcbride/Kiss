/**
 * Created by Blake McBride on 9/24/16.
 */


'use strict';

var Server = function () {};

Server.contextCreated = false;

Server.setURL = function (url) {
    Server.url = url;
};

Server.setUUID = function (uuid) {
    Server.uuid = uuid;
};

Server.logout = function () {
    Server.uuid = '';
};

Server.call = function (pkg, cls, injson) {

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
};

//# sourceURL=kiss/Server.js
