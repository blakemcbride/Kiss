/*
      Author: Blake McBride
      Date:  4/26/19
*/

/* global Utils, Component, Kiss */

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
            newElm = Utils.replaceHTML(id, elm, '<div style="{style}; display: inline-block;"><input id="{id}" type="file"  accept="{accept}" {attr} style="display: none;"><input type="button" id="{btnid}" value="{content}"></div>', {
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
        const el = newElm.element;

        if (custom) {
            const btnEl = document.getElementById(id + '-btn');
            if (btnEl) {
                btnEl.addEventListener('click', function () {
                    el.click();
                });
            }
        }

        newElm.numberOfUploadFiles = function () {
            return el.files.length;
        };

        newElm.uploadFilename = function (idx) {
            return el.files[idx].name;
        };

        newElm.uploadFileExtension = function (idx) {
            const name = el.files[idx].name;
            const i = name.lastIndexOf(".");
            return i === -1 ? '' : name.substring(i+1);
        };

        newElm.uploadFile = function (idx) {
            return typeof idx === 'undefined' ? el.files : el.files[idx];
        };

        newElm.getFormData = function () {
            const fd = new FormData();
            const n = el.files.length;
            for (let i=0 ; i < n ; i++)
                fd.append('_file-' + i, el.files[i]);
            return fd;
        };

        newElm.click = function () {
            // Remove any existing click handlers by cloning the element
            const newEl = el.cloneNode(true);
            el.parentNode.replaceChild(newEl, el);
            // Update the reference
            newElm.element = newEl;
            newEl.click();
        };

        newElm.isDirty = function () {
            return !!el.files.length;
        };

        newElm.clear = function () {
            el.value = '';
            return this;
        };

        newElm.disable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.disabled = flg;
            return this;
        };

        newElm.enable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.disabled = !flg;
            return this;
        };

        newElm.isDisabled = function () {
            return !!el.disabled;
        };

        //--

        newElm.hide = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                Kiss.hide(el);
            else {
                Kiss.show(el);
                el.style.visibility = 'visible';
            }
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg) {
                Kiss.show(el);
                el.style.visibility = 'visible';
            } else
                Kiss.hide(el);
            return this;
        };

        newElm.isHidden = function () {
            return Kiss.isHidden(el);
        };

        newElm.isVisible = function () {
            return !Kiss.isHidden(el);
        };

        newElm.onChange = function (fun) {
            // Remove existing change event listeners by cloning
            const newEl = el.cloneNode(true);
            el.parentNode.replaceChild(newEl, el);
            newElm.element = newEl;
            // Add new listener
            newEl.addEventListener('change', fun);
            return this;
        };

        newElm.focus = function () {
            el.focus();
            return this;
        };

        newElm.isError = function (desc) {
            if (required && !el.files.length) {
                Utils.showMessage('Error', desc + " file selection is required.").then(function () {
                    el.focus();
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
