/*
      Author: Blake McBride
      Date:  4/26/19
 */

/* global Utils, Component */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nstyle;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';
        let nattrs = '';
        let id;
        let required = false;
        for (let prop in attr) {
            switch (prop) {

                // new attributes
                case 'required':
                    required = true;
                    break;

                // preexisting attributes

                case 'style':
                    break;  // already dealing with this
                case 'id':
                    id = Utils.removeQuotes(attr[prop]);
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        const newElm = Utils.replaceHTML(id, elm, '<input id="{id}" type="file" style="{style}" {attr}>', {
            style: nstyle,
            attr: nattrs
        });
        if (!newElm)
            return;
        const jqObj = newElm.jqObj;

        newElm.numberOfUploadFiles = function () {
            return jqObj[0].files.length;
        };

        newElm.uploadFilename = function (idx) {
            return jqObj[0].files[idx].name;
        };

        newElm.uploadFileExtension = function (idx) {
            const name = jqObj[0].files[idx].name;
            const i = name.lastIndexOf(".");
            return i === -1 ? '' : name.substr(i+1);
        };

        newElm.uploadFile = function (idx) {
            return jqObj[0].files[idx];
        };

        newElm.getFormData = function () {
            const fd = new FormData();
            const n = jqObj[0].files.length;
            for (let i=0 ; i < n ; i++)
                fd.append('_file-' + i, jqObj[0].files[i]);
            return fd;
        };

        newElm.click = function () {
            jqObj.off('click').click();
        };

        newElm.isDirty = function () {
            return !!jqObj[0].files.length;
        };

        newElm.clear = function () {
            return jqObj.val(null);
        };

        newElm.disable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.prop('disabled', flg);
            return this;
        };

        newElm.enable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.prop('disabled', !flg);
            return this;
        };

        newElm.isDisabled = function () {
            return !!jqObj.attr('disabled');
        };

        //--

        newElm.hide = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                jqObj.hide();
            else
                jqObj.show();
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                jqObj.show();
            else
                jqObj.hide();
            return this;
        };

        newElm.isHidden = function () {
            return jqObj.is(':hidden');
        };

        newElm.isVisible = function () {
            return jqObj.is(':visible');
        };

        newElm.onChange = function (fun) {
            jqObj.off('change').change(fun);
            return this;
        };

        newElm.focus = function () {
            jqObj.focus();
            return this;
        };

        newElm.isError = function (desc) {
            if (required && !jqObj[0].files.length) {
                Utils.showMessage('Error', desc + " file selection is required.").then(function () {
                    jqObj.focus();
                });
                return true;
            }
            return false;
        };
    };

    const componentInfo = {
        name: 'FileUpload',
        tag: 'file-upload',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();
