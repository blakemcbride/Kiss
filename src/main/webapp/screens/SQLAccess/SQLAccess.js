
/* global $$, Server */

'use strict';

(async function () {


    let res = await Server.call('services/MyGroovyService', 'hasDatabase', {});
    if (res._Success) {

    }

})();
