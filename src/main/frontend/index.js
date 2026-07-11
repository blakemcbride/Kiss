
/* global Utils, Server, Router, SystemInfo */

'use strict';

Utils.afterComponentsLoaded(async function () {
    if (SystemInfo.backendUrl) {
        // explicit backend URL
        Server.setURL(SystemInfo.backendUrl);
    } else if (SystemInfo.sameOriginBackend) {
        //  This copy was stamped by the build system because it is served by
        //  the back end itself (dev Tomcat ROOT or production WAR) - same origin
        let url = Utils.getAppUrl();
        Server.setURL(url);
    } else if (window.location.protocol === "file:") {
        //  Electron desktop frontend.  The back end is one above the port
        //  block base in Tasks.java (default base 8000 -> back end 8001);
        //  set SystemInfo.backendUrl instead if this application differs.
        Server.setURL('http://localhost:8001');
    } else if (window.location.protocol === "http:" && window.location.port >= 8000) {
        //  Development environment: this page came from the front-end static
        //  server.  Under the Kiss port-block convention (see setPortBase in
        //  Tasks.java) the back end runs one port above the front end.
        Server.setURL('http://' + window.location.hostname + ':' + (Number(window.location.port) + 1));
    } else {
        //  Production environment with front-end & back-end as one unit
        let url = Utils.getAppUrl();
        Server.setURL(url);
    }

    Utils.forceASCII = false;  // Force all text entry to ASCII (see Utils.forceASCII)

    //  If the back end was restarted since this browser's session was established,
    //  drop the stale persisted session so the user is forced to re-login (rather than
    //  resuming onto a dead session).  Must complete before routing/resume begins.
    //  Guarded so a hiccup here can never block the app from starting.
    try {
        await Server.verifyServerInstance();
    } catch (e) {
        console.log('verifyServerInstance failed:', e);
    }

    //  Begin hash-based routing.  Routes are declared in routes.js; the current
    //  hash is dispatched now (deep-linking and, when a session token persists in
    //  AppState, session resume).  Unauthenticated routes redirect to /login.
    Router.start();
});


(function () {
    Utils.useComponent('Popup');
    Utils.useComponent('CheckBox');
    Utils.useComponent('ComboBox');
    Utils.useComponent('DateInput');
    Utils.useComponent('DropDown');
    Utils.useComponent('DurationInput');
    Utils.useComponent('ListBox');
    Utils.useComponent('NumericInput');
    Utils.useComponent('PushButton');
    Utils.useComponent('RadioButton');
    Utils.useComponent('TextboxInput');
    Utils.useComponent('TextInput');
    Utils.useComponent('TextLabel');
    Utils.useComponent('TimeInput');
    Utils.useComponent('FileUpload');
    Utils.useComponent('NativeDateInput');
    Utils.useComponent('Picture');
})();

