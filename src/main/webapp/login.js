
'use strict';

$$('login').onclick(function () {

    let error = $$('username').isError('Username');
    if (!error)
        error = $$('password').isError('Password');

    if (!error) {
        let data = {
            username: $$('username').getValue(),
            password: $$('password').getValue()
        };
        Server.call('', 'Login', data).then(function (res) {
            if (res._Success) {
                Server.setUUID(res.uuid);
                utils.loadPage('page1/page1');
            }
        });
    }

});


//# sourceURL=login.js