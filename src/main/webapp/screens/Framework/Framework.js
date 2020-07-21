
/* global $$, Utils, Server */

'use strict';

(async function () {

    $$('rest-services').onclick(function () {
        Utils.loadPage('screens/RestServices/RestServices', 'app-screen-area', 'input-1');
    });

    $$('controls').onclick(function () {
        Utils.loadPage('screens/Controls/Controls', 'app-screen-area', 'ctl-text');
    });

    $$('crud').onclick(function () {
        Utils.loadPage('screens/CRUD/CRUD', 'app-screen-area');
    });

    $$('export').onclick(function () {
        Utils.loadPage('screens/Export/Export', 'app-screen-area');
    });

    $$('file-upload').onclick(function () {
        Utils.loadPage('screens/FileUpload/FileUpload', 'app-screen-area');
    });


    $$('logout').onclick(function () {
        Server.logout();
        window.onbeforeunload = null;
        history.go(0);
    });

})();
