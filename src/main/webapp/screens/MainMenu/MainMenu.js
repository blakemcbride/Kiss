
/* global $$, Utils, Server */

'use strict';

(function () {

    $$('rest-services').onclick(function () {
        Utils.loadPage('screens/RestServices/RestServices', 'app-screen-area');
    });

    $$('controls').onclick(function () {
        Utils.loadPage('screens/Controls/Controls', 'app-screen-area');
    });


    $$('logout').onclick(function () {
        Server.logout();
        window.onbeforeunload = null;
        history.go(0);
    });

})();
