/*
      Author: Blake McBride
      Date:  4/24/18
 */

/* global Utils */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nStyle, originalValue;
        if (attr.style)
            nStyle = 'display: inline-flex; ' + attr.style;
        else
            nStyle = 'margin-left: 8px; display: inline-flex;';
        let cls = '';
        let processChanges = true;

        let nAttrs = '';
        let id;
        for (let prop in attr) {
            switch (prop) {

                // new attributes


                // pre-existing attributes

                case 'style':
                    break;  // already dealing with this
                case 'class':
                    cls = attr[prop];
                    break;
                case 'id':
                    id = Utils.removeQuotes(attr[prop]);
                    break;
                default:
                    nAttrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        const newElm = Utils.replaceHTML(id, elm, '<div style="{style}" class="{class}"><input type="checkbox" {attr} id="{id}"><label for="{id}" style="margin-left: 4px;">{content}</label></div>', {
            style: nStyle,
            class: cls,
            attr: nAttrs,
            content: content ? content.trim() : ''
        });
        const jqObj = newElm.jqObj;

        jqObj.change(function () {
            if (processChanges)
                Utils.someControlValueChanged();
        });

        newElm.getValue = function () {
            return jqObj.prop('checked');
        };

        newElm.setValue = function (val) {
            jqObj.prop('checked', !!val);
            originalValue = newElm.getValue();
            return this;
        };

        newElm.clear = function () {
            jqObj.prop('checked', false);
            originalValue = newElm.getValue();
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getValue();
        };

        newElm.processChanges = function (v) {
            let p = processChanges;
            processChanges = v;
            return p;
        }

        //--

        newElm.readOnly = function () {
            jqObj.attr('readonly', true);
            return this;
        };

        newElm.readWrite = function () {
            jqObj.attr('readonly', false);
            return this;
        };

        newElm.isReadOnly = function () {
            return !!jqObj.attr('readonly');
        };

        //--

        newElm.disable = function () {
            jqObj.prop('disabled', true);
            return this;
        };

        newElm.enable = function () {
            jqObj.prop('disabled', false);
            return this;
        };

        newElm.isDisabled = function () {
            return !!jqObj.attr('disabled');
        };

        newElm.hide = function () {
            jqObj.hide();
            return this;
        };

        newElm.show = function () {
            jqObj.show();
            return this;
        };

        newElm.isHidden = function () {
            return jqObj.is(':hidden');
        };

        newElm.isVisible = function () {
            return jqObj.is(':visible');
        };

        newElm.onChange = function (fun) {
            jqObj.change(fun);
            return this;
        };

        newElm.focus = function () {
            jqObj.focus();
            return this;
        };
    };

    const componentInfo = {
        name: 'CheckBox',
        tag: 'check-box',
        processor: processor
    };

    Utils.newComponent(componentInfo);
})();


