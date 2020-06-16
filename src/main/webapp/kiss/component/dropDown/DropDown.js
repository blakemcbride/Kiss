/*
 * Author: Blake McBride
 * Date:  4/22/18
 */

/* global Utils */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nStyle, originalValue = null;
        let required = false;
        if (attr.style)
            nStyle = attr.style;
        else
            nStyle = '';
        let nAttrs = '';
        let id;
        let default_option;
        for (let prop of Object.keys(attr)) {
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

        const newElm = Utils.replaceHTML(id, elm, '<select style="{style}" {attr} id="{id}">{content}</select>', {
            style: nStyle,
            attr: nAttrs,
            content: content
        });

        const jqObj = newElm.jqObj;
        let dataStore = {};

        jqObj.on('change', function () {
            Utils.someControlValueChanged();
        });

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

        newElm.addItems = function (items, valField, labelField, dataField) {
            items = Utils.assureArray(items);
            const len = items.length;
            for (let i=0 ; i < len ; i++) {
                jqObj.append($('<option></option>').attr('value', items[i][valField]).text(items[i][labelField]));
                if (dataField)
                    dataStore[items[i][valField]] = items[i][dataField];
            }
            originalValue = jqObj.val();
            return this;
        };

        newElm.size = function () {
            return jqObj.children('option').length;
        };

        newElm.getValue = function () {
            return jqObj.val();
        };

        newElm.getIntValue = function () {
            let val = jqObj.val();
            return val ? Number(val) : 0;
        };

        newElm.setValue = function (val) {
            jqObj.val(val);
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

        newElm.onChange = function (func) {
            jqObj.off('change').on('change', function () {
                Utils.someControlValueChanged();
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
            let val = newElm.getValue();
            if (!val) {
                Utils.showMessage('Error', desc + ' selection is required.').then(function () {
                    jqObj.focus();
                });
                return true;
            }
            return false;
        };
    };

    const componentInfo = {
        name: 'DropDown',
        tag: 'drop-down',
        processor: processor
    };

    Utils.newComponent(componentInfo);
})();

