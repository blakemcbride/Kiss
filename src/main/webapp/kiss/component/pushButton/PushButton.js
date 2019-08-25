/*
      Author: Blake McBride
      Date:  4/25/18
 */

/* global Utils */

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
                    id = Utils.removeQuotes(attr[prop]);
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        var newElm = Utils.replaceHTML(id, elm, '<input type="button" style="{style}" {attr} value="{value}" id="{id}">', {
            style: nstyle,
            attr: nattrs,
            value: content ? content.trim() : ''
        });
        var jqObj = newElm.jqObj;

        newElm.onclick = function (fun) {
            // the unbind is used to assure that multiple calls to this method doesn't cause the function to execute multiple times
            // but it also limits to a single callback function
            jqObj.unbind('click').click(fun);
            return this;
        };

        newElm.click = function () {
            jqObj.click();
            return this;
        };

        newElm.getValue = function () {
            return jqObj.val();
        };

        newElm.setValue = function (val) {
            jqObj.val(val);
            return this;
        };

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

        //--

        newElm.focus = function () {
            jqObj.focus();
            return this;
        }
    };

    var componentInfo = {
        name: 'PushButton',
        tag: 'push-button',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();




//# sourceURL=kiss/component/pushButton/PushButton.js
