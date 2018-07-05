
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
            utils.loadPage('page1/page1');
        }
    }

});


//# sourceURL=login.js