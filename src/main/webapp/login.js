
/* global $$, Server, Utils */

'use strict';

$$('login').onclick(async function () {

    if ($$('username').isError('Username'))
        return;
    if ($$('password').isError('Password'))
        return;

    let data = {
        username: $$('username').getValue(),
        password: $$('password').getValue()
    };
    let res = await Server.call('', 'Login', data);
    if (res._Success) {
        Server.setUUID(res.uuid);
        Utils.loadPage('screens/MainMenu/MainMenu');
    }

});

$$('username').focus();

