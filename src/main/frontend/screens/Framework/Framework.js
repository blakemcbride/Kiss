
/* global $$, Utils, Server */

'use strict';

(async function () {

    const screenArea = 'app-screen-area';

    $$('rest-services').onclick(function () {
        Utils.cleanup();  //  clean up any context information
        Utils.loadPage('screens/RestServices/RestServices', screenArea, 'input-1');
    });

    $$('controls').onclick(function () {
        Utils.cleanup();  //  clean up any context information
        Utils.loadPage('screens/Controls/Controls', screenArea, 'ctl-text');
    });

    $$('crud').onclick(function () {
        Utils.cleanup();  //  clean up any context information
        Utils.loadPage('screens/CRUD/CRUD', screenArea);
    });

    $$('users').onclick(function () {
        Utils.cleanup();  //  clean up any context information
        Utils.loadPage('screens/Users/Users', screenArea);
    });

    $$('file-upload').onclick(function () {
        Utils.cleanup();  //  clean up any context information
        Utils.loadPage('screens/FileUpload/FileUpload', screenArea);
    });

    $$('ai').onclick(function () {
        Utils.cleanup();  //  clean up any context information
        Utils.loadPage('screens/Ollama/Ollama', screenArea);
    });

    $$('logout').onclick(function () {
        Server.logout();
    });

    // show the startup message
    Utils.popup_open('intro-popup');
    $$('close-intro').onclick(function () {
        Utils.popup_close();
    });


})();
