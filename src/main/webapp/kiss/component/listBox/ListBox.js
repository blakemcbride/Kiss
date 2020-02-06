/*
      Author: Blake McBride
      Date:  5/25/18
 */

/* global Utils */

'use strict';

(function () {

    var processor = function (elm, attr, content) {
        var nstyle, originalValue = null;
        var required = false;
        var size = null;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';
        nstyle = 'overflow-y: auto; ' + nstyle;  // cause vertical scrollbar only when necessary

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
                    id = Utils.removeQuotes(attr[prop]);
                    break;
                case 'size':
                    size = attr[prop];
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }
        if (!size)
            size = '2';  // make sure it isn't a dropdown

        if (!content  &&  default_option)
            content = '<option value="">' + default_option + '</option>';

        var newElm = Utils.replaceHTML(id, elm, '<select style="{style}" {attr} id="{id}" size="{size}">{content}</select>', {
            style: nstyle,
            attr: nattrs,
            content: content,
            size: size
        });
        var jqObj = newElm.jqObj;
        var dataStore = {};

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

        newElm.addItems = function (items, valField, labelField) {
            items = Utils.assureArray(items);
            const len = items.length;
            for (let i=0 ; i < len ; i++)
                jqObj.append($('<option></option>').attr('value', items[i][valField]).text(items[i][labelField]));
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

        newElm.onChange = function (func) {
            jqObj.on('change', function () {
                // func gets passed the selected value, label
                func(jqObj.val(), jqObj.find('option:selected').text(), dataStore[jqObj.val()]);
            });
            return this;
        };

        var timeout;

        newElm.onClick = function (fun) {
            jqObj.click(function () {
                if (jqObj.val()) {
                    timeout = setTimeout(function () {
                        if (timeout)
                            fun(jqObj.val(), jqObj.find('option:selected').text(), dataStore[jqObj.val()]);
                    }, 300);
                }
            });
            return this;
        };

        newElm.onDblClick = function (fun) {
            jqObj.dblclick(function () {
                if (jqObj.val()) {
                    if (timeout) {
                        clearTimeout(timeout);
                        timeout = null;
                    }
                    fun(jqObj.val(), jqObj.find('option:selected').text(), dataStore[jqObj.val()]);
                }
            });
            return this;
        };

        newElm.selectedIndex = function () {
            return jqObj.prop('selectedIndex');
        };

        newElm.removeByIndex = function (idx) {
            if (idx < jqObj.children('option').length)
                jqObj.find('option').eq(idx).remove();
            originalValue = jqObj.val();
            return this;
        };

    };

    var componentInfo = {
        name: 'ListBox',
        tag: 'list-box',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();


