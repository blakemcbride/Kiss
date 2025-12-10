/*
      Author: Blake McBride
      Date:  4/24/18
*/

/* global Utils, DOMUtils */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nStyle, originalValue;
        if (attr.style) {
            if ((new RegExp("^display:|[^-]display:")).test(attr.style))
                nStyle = attr.style;
            else
                nStyle = 'display: inline-flex; ' + attr.style;
        } else
            nStyle = 'margin-left: 8px; display: inline-flex;';
        let cls = '';
        let processChanges = true;

        let nAttrs = '';
        let id;
        for (let prop in attr) {
            switch (prop) {

                // new attributes


                // preexisting attributes

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
        if (!newElm)
            return;
        const el = newElm.element;

        let changeHandler = function () {
            if (processChanges)
                Utils.someControlValueChanged();
        };
        el.addEventListener('change', changeHandler);

        newElm.getValue = function () {
            return el.checked;
        };

        newElm.setValue = function (val) {
            if (typeof val === 'string')
                val = val === 'true';
            el.checked = !!val;
            originalValue = newElm.getValue();
            return this;
        };

        newElm.clear = function () {
            el.checked = false;
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

        newElm.readOnly = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                el.setAttribute('readonly', 'readonly');
            else
                el.removeAttribute('readonly');
            return this;
        };

        newElm.readWrite = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                el.removeAttribute('readonly');
            else
                el.setAttribute('readonly', 'readonly');
            return this;
        };

        newElm.isReadOnly = function () {
            return el.hasAttribute('readonly');
        };

        //--

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
            return el.disabled;
        };

        newElm.hide = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                DOMUtils.hide(el);
            else {
                DOMUtils.show(el);
            }
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                DOMUtils.show(el);
            else
                DOMUtils.hide(el);
            return this;
        };

        newElm.isHidden = function () {
            return DOMUtils.isHidden(el);
        };

        newElm.isVisible = function () {
            return DOMUtils.isVisible(el);
        };

        newElm.onChange = function (fun) {
            el.removeEventListener('change', changeHandler);
            changeHandler = () => {
                if (fun)
                    fun(el.checked);
                if (processChanges)
                    Utils.someControlValueChanged();
            };
            el.addEventListener('change', changeHandler);
            return this;
        };

        newElm.focus = function () {
            el.focus();
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


