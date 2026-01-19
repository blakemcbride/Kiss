
/* global $$, Server, Utils */

'use strict';

(function () {

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
            // prevent accidental browser back button using History API (works reliably on mobile)
            Server.isLoggedIn = true;
            history.pushState(null, document.title, location.href);
            window.addEventListener('popstate', function(event) {
                if (Server.isLoggedIn) {
                    history.pushState(null, document.title, location.href);
                    Utils.yesNo('Confirm', 'Are you sure you want to logout?', function() {
                        Server.logout();
                    }, function() {
                        // User chose to stay, do nothing (already pushed state back)
                    });
                }
            });
            // Warn on page reload/close
            window.addEventListener('beforeunload', function(e) {
                if (Server.isLoggedIn) {
                    e.preventDefault();
                    e.returnValue = '';
                    return '';
                }
            });
            Utils.loadPage('screens/Framework/Framework');
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
