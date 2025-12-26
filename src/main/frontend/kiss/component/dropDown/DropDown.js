/*
 * Author: Blake McBride
 * Date:  4/22/18
 */

/* global Utils, DOMUtils */

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
        let keyIsNumber = false;
        let triggerGlobalChange = true;

        for (let prop of Object.keys(attr)) {
            switch (prop) {

                // New attributes.

                case 'required':
                    required = true;
                    break;
                case 'default-option':
                    default_option = attr[prop];
                    break;

                // Preexisting attributes

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
        if (!newElm)
            return;
        const el = newElm.element;
        let dataStore = {};
        let changeHandler = null;

        changeHandler = function () {
            if (triggerGlobalChange)
                Utils.someControlValueChanged();
        };
        DOMUtils.on(el, 'change', changeHandler);

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
        };

        newElm.size = function () {
            return el.options.length;
        };

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
            console.log("DropDown.js getIntValue called");
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
            if (row !== 0 && !row)
                return el.options[el.selectedIndex].text;
            return el.options[row].text;
        };

        newElm.getAllLabels = function () {
            const r = [];
            for (let i = 0; i < el.options.length; i++)
                r.push(el.options[i].text);
            return r;
        };

        newElm.setLabel = function (lbl, row) {
            if (row !== 0 && !row)
                el.options[el.selectedIndex].text = lbl;
            else
                el.options[row].text = lbl;
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
            if (flg)
                el.removeAttribute('readonly');
            else
                el.setAttribute('readonly', 'readonly');
            return this;
        };

        newElm.isReadOnly = function () {
            return el.hasAttribute('readonly');
        };

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

        newElm.hide = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                DOMUtils.hide(el);
            else
                DOMUtils.show(el);
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                DOMUtils.show(el);
            else
                DOMUtils.hide(el);
            return this;
        };

        newElm.isHidden = function () {
            return DOMUtils.isHidden(el);
        };

        newElm.isVisible = function () {
            return !DOMUtils.isHidden(el);
        };

        newElm.onChange = function (func) {
            if (changeHandler)
                DOMUtils.off(el, 'change', changeHandler);
            changeHandler = function () {
                if (triggerGlobalChange)
                    Utils.someControlValueChanged();
                // func gets passed the selected value, label
                if (func)
                    func(el.value, el.options[el.selectedIndex].text, dataStore[el.value]);
            };
            DOMUtils.on(el, 'change', changeHandler);
            return this;
        };

        newElm.triggerGlobalChange = function (flg) {
            triggerGlobalChange = flg;
        };

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

        newElm.selectedIndex = function () {
            return el.selectedIndex;
        };

        newElm.selectIndex = function (idx) {
            if (idx >= 0 && idx < el.options.length)
                el.selectedIndex = idx;
            return this;
        };

        newElm.removeByIndex = function (idx) {
            const val = el.value;
            if (idx < el.options.length)
                el.remove(idx);
            if (val)
                delete dataStore[val];
            originalValue = el.value;
            return this;
        };

    };

    const componentInfo = {
        name: 'DropDown',
        tag: 'drop-down',
        processor: processor
    };

    Utils.newComponent(componentInfo);
})();
