/* global Router */

'use strict';

/**
 * Application route table.
 *
 * This file is owned by the application — add a route here for each addressable
 * screen.  See kiss/Router.js for the route-definition format.  Routes are
 * registered at load; index.js calls Router.start() once components are ready.
 */

(function () {

    //  Device-aware page selection (mirrors the old screen-size logic in index.js).
    function loginPage() {
        return (screen.height * screen.width < 600000) ? 'mobile/login' : 'login';
    }
    function homePage() {
        return (screen.height * screen.width < 600000) ? 'mobile/page1' : 'screens/Framework/Framework';
    }

    //  Full-body screens
    Router.add('/login', { page: loginPage, auth: false });
    Router.add('/',      { page: homePage });

    //  Sub-screens loaded into the home shell's content region (shell defaults to '/')
    Router.add('/rest-services', { page: 'screens/RestServices/RestServices',         tag: 'app-screen-area', focus: 'input-1' });
    Router.add('/controls',      { page: 'screens/Controls/Controls',                 tag: 'app-screen-area', focus: 'ctl-text' });
    Router.add('/crud',          { page: 'screens/CRUD/CRUD',                         tag: 'app-screen-area' });
    Router.add('/users',         { page: 'screens/Users/Users',                       tag: 'app-screen-area' });
    Router.add('/file-upload',   { page: 'screens/FileUploadScreen/FileUploadScreen', tag: 'app-screen-area' });
    Router.add('/ai',            { page: 'screens/Ollama/Ollama',                     tag: 'app-screen-area' });

    //  File-based fallback: any URL that matches none of the routes above is loaded as a
    //  screen under 'screens', into the home shell's content region (the same place the
    //  routes above target).  So #/Reports/Daily loads screens/Reports/Daily without needing
    //  its own Router.add().  A ?tag= on the URL (or Router.go(path, tag)) overrides the region.
    Router.setScreenRoot('screens', { shell: '/', tag: 'app-screen-area' });

})();
