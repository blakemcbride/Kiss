/*
      Author: Blake McBride
      Date:  5/25/18
 */

/* global Utils */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nstyle, originalValue = null;
        let required = false;
        let size = null;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';
        nstyle = 'overflow-y: auto; ' + nstyle;  // cause vertical scrollbar only when necessary

        let nattrs = '';
        let id;
        let default_option;
        for (let prop in attr) {
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

        // code to correct bug in Chrome
        let addHeight = true;
        if (attr.style) {
            let tstyle = attr.style.split(';');
            for (let i = 0; i < tstyle.length; i++) {
                let s = tstyle[i].split(':');
                if (s.length) {
                    let a = s[0].trim();
                    if (a === 'height') {
                        addHeight = false;
                        break;
                    }
                }
            }
        }
        if (addHeight)
            nstyle = 'height: auto; ' + nstyle;

        if (!content  &&  default_option)
            content = '<option value="">' + default_option + '</option>';

        const newElm = Utils.replaceHTML(id, elm, '<select style="{style}" {attr} id="{id}" size="{size}">{content}</select>', {
            style: nstyle,
            attr: nattrs,
            content: content,
            size: size
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
                else
                    dataStore[items[i][valField]] = items[i]; // store the whole thing
            }
            originalValue = jqObj.val();
            return this;
        };

        newElm.size = function () {
            return jqObj.children('option').length;
        };

        //--

        newElm.getValue = function (row) {
            if (row !== 0 && !row)
                return jqObj.val();
            return jqObj.find('option')[row].value;
        };

        newElm.getIntValue = function (row) {
            const val = newElm.getValue(row);
            return val ? Number(val) : 0;
        };

        newElm.setValue = function (val, row) {
            if (row !== 0 && !row) {
                jqObj.val(val);
                originalValue = jqObj.val();
            } else {
                const origVal = newElm.getValue(row);
                jqObj.find('option')[row].value = val;
                if (origVal) {
                    const data = dataStore[origVal];
                    delete dataStore[origVal];
                    dataStore[val] = data;
                }
            }
            return this;
        };

        newElm.getLabel = function (row) {
            if (row !== 0 && !row)
                return jqObj.find('option:selected').text();
            return jqObj.find('option')[row].text;
        };

        newElm.getAllLabels = function () {
            const r = [];
            jqObj.find('option').each((idx, option) => {
                r.push(option.text);
            });
            return r;
        };

        newElm.setLabel = function (lbl, row) {
            if (row !== 0 && !row)
                jqObj.find('option:selected').text(lbl);
            else
                jqObj.find('option')[row].text = lbl;
            return this;
        };

        newElm.getData = function (row) {
            return dataStore[newElm.getValue(row)];
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
            let val = newElm.getValue();
            if (!val) {
                Utils.showMessage('Error', desc + ' selection is required.').then(function () {
                    jqObj.focus();
                });
                return true;
            }
            return false;
        };

        newElm.onChange = function (func) {
            jqObj.off('change').on('change', function () {
                Utils.someControlValueChanged();
                // func gets passed the selected value, label
                func(jqObj.val(), jqObj.find('option:selected').text(), dataStore[jqObj.val()]);
            });
            return this;
        };

        let timeout;

        newElm.onClick = function (fun) {
            jqObj.off('click').click(function () {
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
            jqObj.off('dblclick').dblclick(function () {
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
            const val = jqObj.val();
            if (idx < jqObj.children('option').length)
                jqObj.find('option').eq(idx).remove();
            if (val)
                delete dataStore[val];
            originalValue = jqObj.val();
            return this;
        };

    };

    const componentInfo = {
        name: 'ListBox',
        tag: 'list-box',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();


