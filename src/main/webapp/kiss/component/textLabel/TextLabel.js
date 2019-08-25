/*
      Author: Blake McBride
 */

/* global Utils, Component */

'use strict';

(function () {
    let processor = (elm, attr, content) => {
        let nstyle;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';

        let nattrs = '';
        let id;

        for (let prop in attr) {
            switch (prop) {
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

        var newElm = Utils.replaceHTML(id, elm, `<label style="{style}" {attr} id="{id}">${content ? content.trim() : ''}</label>`, {
            style: nstyle,
            attr: nattrs
        });

        var jqObj = newElm.jqObj;

        newElm.getValue = function () {
            var sval = jqObj.val();
            return sval ? sval : '';
        };

        newElm.setValue = function (val) {
            if (val !== 0  &&  !val) {
                jqObj.val('');
                return this;
            }
            jqObj.val(val);
            return this;
        };

        newElm.clear = function () {
            jqObj.val('');
            return this;
        };

        //--

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
    };

    var componentInfo = {
        name: 'TextLabel',
        tag: 'text-label',
        processor: processor
    };
    Utils.newComponent(componentInfo);

    Component.TextLabel.$textlabel = function (elm) {
        var val = elm.value.replace(/^\s+/, "");
        return val;
    };
})();
