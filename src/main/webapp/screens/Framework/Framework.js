
/* global $$, Utils, Server */

'use strict';

(async function () {

    $$('rest-services').onclick(function () {
        AGGrid.popAllGridContexts();
        Utils.loadPage('screens/RestServices/RestServices', 'app-screen-area', 'input-1');
    });

    $$('controls').onclick(function () {
        AGGrid.popAllGridContexts();
        Utils.loadPage('screens/Controls/Controls', 'app-screen-area', 'ctl-text');
    });

    $$('crud').onclick(function () {
        AGGrid.popAllGridContexts();
        Utils.loadPage('screens/CRUD/CRUD', 'app-screen-area');
    });

    $$('file-upload').onclick(function () {
        AGGrid.popAllGridContexts();
        Utils.loadPage('screens/FileUpload/FileUpload', 'app-screen-area');
    });


    $$('logout').onclick(function () {
        AGGrid.popAllGridContexts();
        Server.logout();
        window.onbeforeunload = null;
        history.go(0);
    });

})();
