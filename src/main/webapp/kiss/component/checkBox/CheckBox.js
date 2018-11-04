/*
      Author: Blake McBride
      Date:  4/24/18
 */

/* global utils */

'use strict';

(function () {

    var processor = function (elm, attr, content) {
        var nstyle;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';

        var nattrs = '';
        var id;
        for (var prop in attr) {
            switch (prop) {

                // new attributes


                // pre-existing attributes

                case 'style':
                    break;  // already dealing with this
                case 'id':
                    id = utils.removeQuotes(attr[prop]);
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        var newElm = utils.replaceHTML(id, elm, '<div style="display: inline-block;"><input type="checkbox" style="{style}" {attr} id="{id}">{content}</div>', {
            style: nstyle,
            attr: nattrs,
            content: content ? content : ''
        });
        var jqObj = newElm.jqObj;

        newElm.getValue = function () {
            return jqObj.prop('checked');
        };

        newElm.setValue = function (val) {
            jqObj.prop('checked', !!val);
            return this;
        };

        newElm.clear = function () {
            jqObj.prop('checked', false);
            return this;
        };

        newElm.onChange = function (fun) {
            jqObj.change(fun);
            return this;
        };

        newElm.disable = function () {
            jqObj.prop('disabled', true);
            return this;
        };

        newElm.enable = function () {
            jqObj.prop('disabled', false);
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

    };

    var componentInfo = {
        name: 'CheckBox',
        tag: 'check-box',
        processor: processor
    };
    utils.newComponent(componentInfo);

})();




//# sourceURL=kiss/component/checkBox/CheckBox.js
