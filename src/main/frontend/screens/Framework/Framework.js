
/* global $$, Utils, Server, Router */

'use strict';

(async function () {

    //  Navigation goes through the router so each screen has its own URL (deep-linkable,
    //  Back/Forward navigate between them).  loadPage cleanup happens inside the router.
    $$('rest-services').onclick(function () {
        Router.go('/rest-services');
    });

    $$('controls').onclick(function () {
        Router.go('/controls');
    });

    $$('crud').onclick(function () {
        Router.go('/crud');
    });

    $$('users').onclick(function () {
        Router.go('/users');
    });

    $$('file-upload').onclick(function () {
        Router.go('/file-upload');
    });

    $$('ai').onclick(function () {
        Router.go('/ai');
    });

    $$('logout').onclick(function () {
        Server.logout();
    });

    // show the startup message once per browser session (the shell reloads when you
    // return to the home route, and we don't want the intro popping up every time)
    if (!window.__kissIntroShown) {
        window.__kissIntroShown = true;
        Utils.popup_open('intro-popup');
    }
    $$('close-intro').onclick(function () {
        Utils.popup_close();
    });


})();
