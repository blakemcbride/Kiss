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
        let accept = '';
        let custom = false;  // if true, hide the file input and display a push-button instead
        for (let prop in attr) {
            switch (prop) {

                // new attributes
                case 'required':
                    required = true;
                    break;
                case 'custom':
                    custom = true;
                    break;

                // preexisting attributes

                case 'accept':
                    accept = Utils.removeQuotes(attr[prop]);
                    break;
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

        let newElm;
        if (custom) {
            newElm = Utils.replaceHTML(id, elm, '<div style="{style}"><input id="{id}" type="file"  accept="{accept}" {attr} style="display: none;"><input type="button" id="{btnid}" value="{content}"></div>', {
                style: nstyle,
                attr: nattrs,
                accept: accept,
                content: content ? content.trim() : '',
                btnid: id + '-btn'
            });
        } else {
            newElm = Utils.replaceHTML(id, elm, '<input id="{id}" type="file" style="{style}" accept="{accept}" {attr}>', {
                style: nstyle,
                attr: nattrs,
                accept: accept
            });
        }

        if (!newElm)
            return;
        const jqObj = newElm.jqObj;

        if (custom) {
            $('#' + id + '-btn').on('click', function () {
                jqObj.click();
            });
        }

        newElm.numberOfUploadFiles = function () {
            return jqObj[0].files.length;
        };

        newElm.uploadFilename = function (idx) {
            return jqObj[0].files[idx].name;
        };

        newElm.uploadFileExtension = function (idx) {
            const name = jqObj[0].files[idx].name;
            const i = name.lastIndexOf(".");
            return i === -1 ? '' : name.substring(i+1);
        };

        newElm.uploadFile = function (idx) {
            return typeof idx === 'undefined' ? jqObj[0].files : jqObj[0].files[idx];
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
                jqObj.show().css('visibility', 'visible');
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                jqObj.show().css('visibility', 'visible');
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
