
/* global $$, Server, Utils */

'use strict';

(function () {

    async function login() {
        if ($$('username').isError('Username'))
            return;
        if ($$('password').isError('Password'))
            return;

        let data = {
            username: $$('username').getValue().toLowerCase(),
            password: $$('password').getValue()
        };
        let res = await Server.call('', 'Login', data);
        if (res._Success) {
            Server.setUUID(res.uuid);
            // prevent accidental browser back button
            window.onbeforeunload = function() {
                return "Back button hit.";
            };
            Utils.loadPage('screens/MainMenu/MainMenu');
        } else {
            $$('password').clear().focus();
        }
    }

    $$('login').onclick(login);

    $$('username').onEnter(function () {
        $$('password').focus();
    });

    $$('password').onEnter(function () {
        login();
    });

    $$('username').focus();

})();
