
/* global $$, Utils, Server */

'use strict';

(function () {

    $$('rest-services').onclick(function () {
        Utils.loadPage('screens/RestServices/RestServices', 'app-screen-area');
    });

    $$('controls').onclick(function () {
        Utils.loadPage('screens/Controls/Controls', 'app-screen-area');
    });

    $$('sql-access').onclick(function () {
        Utils.loadPage('screens/SQLAccess/SQLAccess', 'app-screen-area');
    });

    $$('report').onclick(function () {
        Utils.loadPage('screens/Report/Report', 'app-screen-area');
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
