
'use strict';



$$('rest-services').onclick(function () {
    utils.loadPage('RestServices/RestServices', 'app-screen-area');
});

$$('controls').onclick(function () {
    utils.loadPage('Controls/Controls', 'app-screen-area');
});


$$('logout').onclick(function () {
    Server.setUUID('');
    history.go(0);
});


//# sourceURL=MainMenu/MainMenu.js
