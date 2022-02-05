
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

    $$('file-upload').onclick(function () {
        Utils.cleanup();  //  clean up any context information
        Utils.loadPage('screens/FileUpload/FileUpload', screenArea);
    });


    $$('logout').onclick(function () {
        Server.logout();
    });

})();
