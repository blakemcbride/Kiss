/*
 * Author: Blake McBride
 * Date:  4/22/18
 */

/* global Utils */

'use strict';

(function () {

    var processor = function (elm, attr, content) {
        var nStyle, originalValue = null;
        var required = false;
        if (attr.style)
            nStyle = attr.style;
        else
            nStyle = '';
        var nAttrs = '';
        var id;
        var default_option;
        for (var prop of Object.keys(attr)) {
            switch (prop) {

                // New attributes.

                case 'required':
                    required = true;
                    break;
                case 'default-option':
                    default_option = attr[prop];
                    break;

                // Pre-existing attributes

                case 'style':
                    break;  // Already dealing with this.
                case 'id':
                    id = Utils.removeQuotes(attr[prop]);
                    break;
                default:
                    nAttrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        if (!content  &&  default_option)
            content = '<option value="">' + default_option + '</option>';

        var newElm = Utils.replaceHTML(id, elm, '<select style="{style}" {attr} id="{id}">{content}</select>', {
            style: nStyle,
            attr: nAttrs,
            content: content
        });

        var jqObj = newElm.jqObj;
        var dataStore = {};

        //--

        newElm.clear = function () {
            jqObj.empty();
            if (default_option)
                newElm.add('', default_option);
            dataStore = {};
            originalValue = null;
            return this;
        };

        newElm.add = function (val, label, data) {
            jqObj.append($('<option></option>').attr('value', val).text(label));
            if (data)
                dataStore[val] = data;
            originalValue = jqObj.val();
            return this;
        };

        newElm.size = function () {
            return jqObj.children('option').length;
        };

        //--

        newElm.getValue = function () {
            return jqObj.val();
        };

        newElm.setValue = function (val) {
            jqObj.val(val).change();
            originalValue = jqObj.val();
            return this;
        };

        newElm.getLabel = function () {
            return jqObj.find('option:selected').text();
        };

        newElm.getData = function () {
            return dataStore[jqObj.val()];
        };

        newElm.isDirty = function () {
            return originalValue !== jqObj.val();
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

        newElm.onChange = function (func) {
            jqObj.on('change', function () {
                // func gets passed the selected value, label
                func(jqObj.val(), jqObj.find('option:selected').text(), dataStore[jqObj.val()]);
            });
            return this;
        };

        newElm.focus = function () {
            jqObj.focus();
            return this;
        };

        newElm.isError = function (desc) {
            if (!required)
                return false;
            var val = newElm.getValue();
            if (!val) {
                Utils.showMessage('Error', desc + ' selection is required.', function () {
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

    Utils.newComponent(componentInfo);
})();

//# sourceURL=kiss/component/dropDown/DropDown.js
