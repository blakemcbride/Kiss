
/* global $$, Utils, Server */

'use strict';

(async function () {

    $$('rest-services').onclick(function () {
        Utils.cleanup();  //  clean up any context information
        Utils.loadPage('screens/RestServices/RestServices', 'app-screen-area', 'input-1');
    });

    $$('controls').onclick(function () {
        Utils.cleanup();  //  clean up any context information
        Utils.loadPage('screens/Controls/Controls', 'app-screen-area', 'ctl-text');
    });

    $$('crud').onclick(function () {
        Utils.cleanup();  //  clean up any context information
        Utils.loadPage('screens/CRUD/CRUD', 'app-screen-area');
    });

    $$('file-upload').onclick(function () {
        Utils.cleanup();  //  clean up any context information
        Utils.loadPage('screens/FileUpload/FileUpload', 'app-screen-area');
    });


    $$('logout').onclick(function () {
        Utils.cleanup();  //  clean up any context information
        Server.logout();
        window.onbeforeunload = null;
        history.go(0);
    });

})();
