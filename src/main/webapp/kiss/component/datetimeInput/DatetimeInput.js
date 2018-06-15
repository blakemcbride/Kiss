/*
      Author: Blake McBride
      Date:  4/22/18
 */

'use strict';


(function () {

    var processor = function (elm, attr, content) {

        var nstyle;
        var min = null;
        var max = null;
        var required = false;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';

        var nattrs = '';
        var id;
        for (var prop in attr) {
            switch (prop) {

                // new attributes

                case 'required':
                    required = true;
                    break;

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

        var newElm = utils.replaceHTML(id, elm, '<input type="datetime-local" style="{style}" {attr} placeholder="{placeholder}" id="{id}">', {
            style: nstyle,
            attr: nattrs,
            placeholder: content
        });
        var jqObj = newElm.jqObj;

        newElm.getSQLValue = function () {
            return jqObj.val();
        };

        newElm.getDateValue = function () {
            var val = jqObj.val();
            if (!val  ||  val.length < 16)
                return null;
            return new Date(parseInt(val.substr(0, 4), 10), parseInt(val.substr(5, 2), 10)-1, parseInt(val.substr(8, 2), 10), parseInt(val.substr(11, 2), 10), parseInt(val.substr(14, 2), 10));
        };

        newElm.setValue = function (val) {
            if (!val)
                jqObj.val('');
            else if (typeof val === 'string')
                jqObj.val(val);
        };

        newElm.clear = function () {
            jqObj.val('');
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

        newElm.isError = function (desc) {
            var val = newElm.getSQLValue();
            if (required && !val) {
                utils.showMessage('Error', desc + ' is required.', function () {
                    jqObj.focus();
                });
                return true;
            }
            return false;
        };
    };

    var componentInfo = {
        name: 'DatetimeInput',
        tag: 'datetime-input',
        processor: processor
    };
    utils.newComponent(componentInfo);

})();



//# sourceURL=kiss/component/datetimeInput/DatetimeInput.js
