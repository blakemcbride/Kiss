/*
      Author: Blake McBride
 */

/* global Utils, Component */

'use strict';

(function () {
    const processor = (elm, attr, content) => {
        let nstyle;
        let hasFor = false;

        if (attr.style)
            nstyle = 'cursor: default; ' + attr.style;
        else
            nstyle = 'cursor: default;';

        let nattrs = '';
        let id;

        for (let prop in attr) {
            switch (prop) {
                case 'style':
                    break;  // already dealing with this
                case 'id':
                    id = Utils.removeQuotes(attr[prop]);
                    break;
                case 'for':
                    hasFor = true;
                // no break
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        let newElm;
        if (hasFor)
            newElm = Utils.replaceHTML(id, elm, `<label style="{style}" {attr} id="{id}">${content ? content.trim() : ''}</label>`, {
                style: nstyle,
                attr: nattrs
            });
        else
            newElm = Utils.replaceHTML(id, elm, `<span style="{style}" {attr} id="{id}">${content ? content.trim() : ''}</span>`, {
                style: nstyle,
                attr: nattrs
            });
        if (!newElm)
            return;
        const jqObj = newElm.jqObj;

        newElm.getValue = function () {
            let sval = jqObj.text();
            return sval ? sval : '';
        };

        newElm.setValue = function (val) {
            if (val !== 0  &&  !val) {
                jqObj.text('');
                return this;
            }
            jqObj.text(val);
            return this;
        };

        newElm.clear = function () {
            jqObj.text('');
            return this;
        };

        newElm.onclick = function (fun) {
            // the off() is used to assure that multiple calls to this method doesn't cause the function to execute multiple times
            // but it also limits to a single callback function
            jqObj.off('click');
            if (fun)
                jqObj.css('cursor', 'pointer').click(fun);
            else
                jqObj.css('cursor', 'default');
            return this;
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

        newElm.setColor = function (color) {
            jqObj.css('color', color);
            return this;
        };
    };

    const componentInfo = {
        name: 'TextLabel',
        tag: 'text-label',
        processor: processor
    };
    Utils.newComponent(componentInfo);

    Component.TextLabel.$textlabel = function (elm) {
        return elm.value.replace(/^\s+/, "");
    };
})();
