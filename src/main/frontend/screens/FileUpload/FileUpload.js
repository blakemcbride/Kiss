
/* global $$, Server */

'use strict';

(function () {

    $$('upload').onclick(async () => {
        if ($$('the-file').numberOfUploadFiles() < 1) {
            Utils.showMessage('Error', 'You must first select a file to upload.');
            return;
        }
        const fd = $$('the-file').getFormData();
        const data = {
            var1: 22,    // just some random data we want to send to the back-end
            var2: 33
        }
        const r = await Server.fileUploadSend('services/FileUpload', 'upload', fd, data);
    });


})();
