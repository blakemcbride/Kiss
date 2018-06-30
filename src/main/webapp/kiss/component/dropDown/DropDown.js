/*
      Author: Blake McBride
      Date:  4/22/18
 */

'use strict';

(function () {

    var processor = function (elm, attr, content) {
        var nstyle;
        var required = false;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';
        var nattrs = '';
        var id;
        var default_option;
        for (var prop in attr) {
            switch (prop) {

                // new attributes

                case 'required':
                    required = true;
                    break;
                case 'default-option':
                    default_option = attr[prop];
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

        if (!content  &&  default_option)
            content = '<option value="">' + default_option + '</option>';

        var newElm = utils.replaceHTML(id, elm, '<select style="{style}" {attr} id="{id}">{content}</select>', {
            style: nstyle,
            attr: nattrs,
            content: content
        });
        var jqObj = newElm.jqObj;
        var dataStore = {};

        newElm.clear = function () {
            jqObj.empty();
            if (default_option)
                newElm.add('', default_option);
            dataStore = {};
        };

        newElm.add = function (val, label, data) {
            jqObj.append($('<option></option>').attr('value', val).text(label));
            if (data)
                dataStore[val] = data;
        };

        newElm.getValue = function () {
            return jqObj.val();
        };

        newElm.setValue = function (val) {
            jqObj.val(val).change();
        };

        newElm.getLabel = function () {
            return jqObj.find('option:selected').text();
        };

        newElm.getData = function () {
            return dataStore[jqObj.val()];
        };

        newElm.onchange = function (func) {
            jqObj.on('change', function () {
                // func gets passed the selected value, label
                func(jqObj.val(), jqObj.find('option:selected').text(), dataStore[jqObj.val()]);
            });
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

        newElm.size = function () {
            return jqObj.children('option').length;
        };

        newElm.isError = function (desc) {
            if (!required)
                return false;
            var val = newElm.getValue();
            if (!val) {
                utils.showMessage('Error', desc + ' selection is required.', function () {
                    jqObj.focus();
                });
                return true;
            }
            return false;
        };
    };

    var componentInfo = {
        name: 'DropDown',
        tag: 'drop-down',
        processor: processor
    };
    utils.newComponent(componentInfo);

})();



//# sourceURL=kiss/component/dropDown/DropDown.js
