
/* global $$, Server, Utils */

'use strict';

$$('login').onclick(async function () {

    let error = $$('username').isError('Username');
    if (!error)
        error = $$('password').isError('Password');

    if (!error) {
        let data = {
            username: $$('username').getValue(),
            password: $$('password').getValue()
        };
        let res = await Server.call('', 'Login', data);
        if (res._Success) {
            Server.setUUID(res.uuid);
            Utils.loadPage('MainMenu/MainMenu');
        }
    }

});

$$('username').focus();


//# sourceURL=login.js