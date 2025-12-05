/*
      Author: Blake McBride
      Date:  5/25/18
*/

/* global Utils, Kiss */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nstyle, originalValue = null;
        let required = false;
        let size = null;
        let multiple = false;
        let keyIsNumber = false;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';
        nstyle = 'overflow-y: auto; ' + nstyle;  // cause vertical scrollbar only when necessary

        let nattrs = '';
        let id;
        let default_option;
        let triggerGlobalChange = true;

        for (let prop in attr) {
            switch (prop) {

                // new attributes

                case 'required':
                    required = true;
                    break;
                case 'default-option':
                    default_option = attr[prop];
                    break;

                // preexisting attributes

                case 'multiple':
                    multiple = true;
                    nattrs += ' ' + prop;
                    break;
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
        if (!newElm)
            return;
        const el = newElm.element;
        let dataStore = {};
        let changeHandler;

        changeHandler = function () {
            if (triggerGlobalChange)
                Utils.someControlValueChanged();
        };
        el.addEventListener('change', changeHandler);

        //--

        newElm.clear = function () {
            el.innerHTML = '';
            if (default_option)
                newElm.add('', default_option);
            dataStore = {};
            originalValue = null;
            return this;
        };

        newElm.add = function (val, label, data) {
            if (typeof val === "number")
                keyIsNumber = true;
            const option = document.createElement('option');
            option.value = val;
            option.textContent = label;
            el.appendChild(option);
            if (data)
                dataStore[val] = data;
            originalValue = el.value;
            return this;
        };

        newElm.addItems = function (items, valField, labelField, dataField) {
            items = Utils.assureArray(items);
            const len = items.length;
            for (let i=0 ; i < len ; i++) {
                let item = items[i];
                let lbl = typeof labelField === 'function' ? labelField(item) : item[labelField];
                if (typeof item[valField] === 'number')
                    keyIsNumber = true;
                const option = document.createElement('option');
                option.value = item[valField];
                option.textContent = lbl;
                el.appendChild(option);
                dataStore[item[valField]] = dataField ? item[dataField] : item;
            }
            originalValue = el.value;
            return this;
        };

        newElm.fill = function (selectedItem, items, valField, labelField, dataField) {
            newElm.clear();
            if (!selectedItem)
                newElm.add('', '(choose)');
            newElm.addItems(items, valField, labelField, dataField);
            if (selectedItem)
                newElm.setValue(selectedItem);
            return this;
        }

        newElm.size = function () {
            return el.options.length;
        };

        //--

        newElm.getValue = function (row) {
            if (row !== 0 && !row)
                return keyIsNumber ? Number(el.value) : el.value;
            const v = el.options[row].value;
            return keyIsNumber ? Number(v) : v;
        };

        /*
            This function has been deprecated and replaced with getValue() above.
            It is left here for backward compatability.
        */
        newElm.getIntValue = function (row) {
            console.log("ListBox.js getIntValue called");
            const val = newElm.getValue(row);
            return val ? Number(val) : 0;
        };

        newElm.setValue = function (val, row) {
            if (row !== 0 && !row) {
                el.value = val;
                originalValue = el.value;
            } else {
                const origVal = newElm.getValue(row);
                el.options[row].value = val;
                if (origVal) {
                    const data = dataStore[origVal];
                    delete dataStore[origVal];
                    dataStore[val] = data;
                }
            }
            return this;
        };

        newElm.getLabel = function (row) {
            if (row !== 0 && !row) {
                const selectedOption = el.selectedOptions[0];
                return selectedOption ? selectedOption.textContent : '';
            }
            return el.options[row].textContent;
        };

        newElm.getAllLabels = function () {
            const r = [];
            Array.from(el.options).forEach((option) => {
                r.push(option.textContent);
            });
            return r;
        };

        newElm.setLabel = function (lbl, row) {
            if (row !== 0 && !row) {
                const selectedOption = el.selectedOptions[0];
                if (selectedOption)
                    selectedOption.textContent = lbl;
            } else {
                el.options[row].textContent = lbl;
            }
            return this;
        };

        newElm.getData = function (row) {
            return dataStore[newElm.getValue(row)];
        };

        newElm.getAllData = function () {
            return dataStore;
        };

        newElm.isDirty = function () {
            return originalValue !== el.value;
        };

        //--

        newElm.readOnly = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                el.setAttribute('readonly', 'readonly');
            else
                el.removeAttribute('readonly');
            return this;
        };

        newElm.readWrite = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (!flg)
                el.setAttribute('readonly', 'readonly');
            else
                el.removeAttribute('readonly');
            return this;
        };

        newElm.isReadOnly = function () {
            return el.hasAttribute('readonly');
        };

        //--

        newElm.disable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.disabled = flg;
            return this;
        };

        newElm.enable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.disabled = !flg;
            return this;
        };

        newElm.isDisabled = function () {
            return el.disabled;
        };

        //--

        newElm.hide = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                Kiss.hide(el);
            else {
                Kiss.show(el);
                el.style.visibility = 'visible';
            }
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg) {
                Kiss.show(el);
                el.style.visibility = 'visible';
            } else {
                Kiss.hide(el);
            }
            return this;
        };

        newElm.isHidden = function () {
            return Kiss.isHidden(el);
        };

        newElm.isVisible = function () {
            return Kiss.isVisible(el);
        };

        //--

        newElm.focus = function () {
            el.focus();
            return this;
        };

        newElm.isError = function (desc) {
            if (!required)
                return false;
            let val = newElm.getValue();
            if (!val) {
                Utils.showMessage('Error', desc + ' selection is required.').then(function () {
                    el.focus();
                });
                return true;
            }
            return false;
        };

        newElm.onChange = function (func) {
            el.removeEventListener('change', changeHandler);
            changeHandler = function () {
                if (triggerGlobalChange)
                    Utils.someControlValueChanged();
                // func gets passed the selected value, label
                if (func) {
                    const selectedOption = el.selectedOptions[0];
                    const val = el.value;
                    const label = selectedOption ? selectedOption.textContent : '';
                    func(val, label, dataStore[val]);
                }
            };
            el.addEventListener('change', changeHandler);
            return this;
        };

        newElm.triggerGlobalChange = function (flg) {
            triggerGlobalChange = flg;
        }

        let timeout;
        let clickHandler;

        newElm.onClick = function (fun) {
            if (clickHandler)
                el.removeEventListener('click', clickHandler);
            clickHandler = function () {
                timeout = setTimeout(function () {
                    if (fun) {
                        const val = el.value;
                        if (val) {
                            const selectedOption = el.selectedOptions[0];
                            const label = selectedOption ? selectedOption.textContent : '';
                            fun(val, label, dataStore[val]);
                        } else {
                            fun(null, null, null);
                        }
                    }
                }, 300);
            };
            el.addEventListener('click', clickHandler);
            return this;
        };

        // double-click is not recognised on mobile devices
        let dblclickHandler;

        newElm.onDblClick = function (fun) {
            if (dblclickHandler)
                el.removeEventListener('dblclick', dblclickHandler);
            dblclickHandler = function () {
                if (timeout) {
                    clearTimeout(timeout);
                    timeout = null;
                }
                if (fun) {
                    const val = el.value;
                    if (val) {
                        const selectedOption = el.selectedOptions[0];
                        const label = selectedOption ? selectedOption.textContent : '';
                        fun(val, label, dataStore[val]);
                    } else {
                        fun(null, null, null);
                    }
                }
            };
            el.addEventListener('dblclick', dblclickHandler);
            return this;
        };

        newElm.selectedIndex = function () {
            return el.selectedIndex;
        };

        newElm.selectIndex = function (idx) {
            if (idx >= 0 && idx < el.options.length)
                el.options[idx].selected = true;
            return this;
        };

        newElm.removeByIndex = function (idx) {
            const val = el.value;
            if (idx < el.options.length)
                el.options[idx].remove();
            if (val)
                delete dataStore[val];
            originalValue = el.value;
            return this;
        };

        newElm.clearSelection = function () {
            el.selectedIndex = -1;
        };

    };

    const componentInfo = {
        name: 'ListBox',
        tag: 'list-box',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();


