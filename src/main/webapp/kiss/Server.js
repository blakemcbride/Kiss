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

Server.call = function (pkg, cls, injson) {
    var path = "rest";  // path to servlet
    if (!injson)
        injson = {};
    injson._uuid = Server.uuid;
    injson._method = "main";
    injson._package = pkg;
    injson._class = cls;

    var dfd = jQuery.Deferred();
    jQuery.ajax({
        type: 'POST',
        url: path,
        data: JSON.stringify(injson),
        contentType: 'application/json',
        dataType: 'json',
        success:  function (data, status, hdr) {
            if (!data._Success)
                utils.showMessage('Error', data._ErrorMessage);
            dfd.resolve(data);
        },
        error: function (error, status) {
            var msg = 'Error communicating with the server.';
            utils.showMessage('Error', msg);
            dfd.resolve({ _Success: false, _ErrorMessage: msg });
            //dfd.reject(error, status);
        }
    });
    return dfd.promise();
};

//# sourceURL=kiss/Server.js
