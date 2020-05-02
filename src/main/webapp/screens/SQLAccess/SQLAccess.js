
/* global $$, Server */

'use strict';

(async function () {

    let res = await Server.call('services/MyGroovyService', 'hasDatabase', {});
    if (res._Success) {
        if (res.hasDatabase)
            $('#has-database').prop('hidden', false);
        else {
            $('#no-database').prop('hidden', false);
            return;
        }
    } else {
        $('#no-database').prop('hidden', false);
        return;
    }

    

})();
