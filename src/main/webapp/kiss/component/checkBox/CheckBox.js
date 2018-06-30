/*
      Author: Blake McBride
      Date:  4/24/18
 */

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

        var newElm = utils.replaceHTML(id, elm, '<input type="checkbox" style="{style}" {attr} id="{id}">{content}', {
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
        };

        newElm.clear = function () {
            jqObj.prop('checked', false);
        };

        newElm.onChange = function (fun) {
            jqObj.change(fun);
        };

        newElm.disable = function () {
            jqObj.prop('disabled', true);
        };

        newElm.enable = function () {
            jqObj.prop('disabled', false);
        };

        newElm.hide = function () {
            jqObj.hide();
        };

        newElm.show = function () {
            jqObj.show();
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
