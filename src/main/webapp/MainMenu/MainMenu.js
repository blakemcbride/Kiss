
/* global $$, Utils, Server */

'use strict';



$$('rest-services').onclick(function () {
    Utils.loadPage('RestServices/RestServices', 'app-screen-area');
});

$$('controls').onclick(function () {
    Utils.loadPage('Controls/Controls', 'app-screen-area');
});


$$('logout').onclick(function () {
    Server.setUUID('');
    window.onbeforeunload = null;
    history.go(0);
});


