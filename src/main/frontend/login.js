
/* global $$, Server, Router */

'use strict';

(function () {

    //  Reaching the login screen ends any existing session so a fresh login is required
    //  (e.g. when the user backs into it from inside the app).
    Server.clearSession();

    async function login() {
        if ($$('username').isError('Username'))
            return;
        if ($$('password').isError('Password'))
            return;

        const data = {
            username: $$('username').getValue().toLowerCase(),
            password: $$('password').getValue()
        };
        const res = await Server.call('', 'Login', data);
        if (res._Success) {
            Server.setUUID(res.uuid);
            Server.setBootId(res._BootId);   //  record the server instance this session belongs to
            //  Go where the user was originally headed (deep link), else the home shell.
            //  go() (push) so the browser Back button returns to the login screen.
            Router.go(Router.returnTarget());
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
