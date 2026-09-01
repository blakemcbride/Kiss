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

        //--  Type-to-filter
        //
        //  While the control has focus, typed characters accumulate in filterText and the list
        //  is narrowed to the options whose label contains that text (case-insensitive, anywhere
        //  in the label).  Non-matching options are detached from the select and re-attached, in
        //  their original order, as the filter is relaxed or ended.  While a filter is active,
        //  allOptions holds every option in its original order and is what the index-based API
        //  works against, so the filter never changes what the API reports; the only thing it
        //  changes is the selected value.

        let filterText = '';
        let allOptions = null;      // every option, original order; non-null only while a filter is active
        let preFilterValue = null;  // the value when typing began (Escape returns to it)
        let lastValue = null;       // the last value the control actually had (used while the narrowed list is empty)

        const opts = function () {
            return allOptions ? allOptions : el.options;
        };

        // The logical current value.  When the narrowed list is empty nothing is selected,
        // so report the last value the control actually had.
        const currentValue = function () {
            return allOptions && el.selectedIndex < 0 ? lastValue : el.value;
        };

        const logicalSelectedIndex = function () {
            if (!allOptions)
                return el.selectedIndex;
            if (el.selectedIndex >= 0)
                return allOptions.indexOf(el.options[el.selectedIndex]);
            for (let i = 0; i < allOptions.length; i++)
                if (allOptions[i].value === lastValue)
                    return i;
            return -1;
        };

        const restoreAllOptions = function () {
            el.innerHTML = '';
            for (let i = 0; i < allOptions.length; i++)
                el.appendChild(allOptions[i]);
        };

        // Erase the typed sequence and restore the full list, keeping the current selection.
        const endFilter = function () {
            filterText = '';
            if (!allOptions)
                return;
            const val = currentValue();
            restoreAllOptions();
            el.value = val;
            allOptions = null;
            preFilterValue = null;
            lastValue = null;
        };

        // Escape:  erase the typed sequence, restore the full list, and return to the
        // selection that was current before typing began.
        const cancelFilter = function () {
            filterText = '';
            if (!allOptions)
                return;
            const before = currentValue();
            restoreAllOptions();
            el.value = preFilterValue;
            allOptions = null;
            preFilterValue = null;
            lastValue = null;
            if (el.value !== before)
                DOMUtils.trigger(el, 'change');
        };

        const applyFilter = function () {
            if (!filterText) {
                endFilter();
                return;
            }
            if (!allOptions) {
                allOptions = Array.from(el.options);
                preFilterValue = el.value;
                lastValue = el.value;
            }
            const before = currentValue();
            const needle = filterText.toLowerCase();
            el.innerHTML = '';
            for (let i = 0; i < allOptions.length; i++)
                if (allOptions[i].text.toLowerCase().indexOf(needle) !== -1)
                    el.appendChild(allOptions[i]);
            if (!el.options.length)
                return;                   // nothing matches; lastValue still holds the last real selection
            el.value = before;            // keep the current selection if it survived the filter ...
            if (el.selectedIndex < 0)
                el.selectedIndex = 0;     // ... otherwise select the first match
            lastValue = el.value;
            if (el.value !== before)
                DOMUtils.trigger(el, 'change');
        };

        DOMUtils.on(el, 'keydown', function (e) {
            if (e.ctrlKey || e.altKey || e.metaKey || e.isComposing || el.multiple)
                return;
            if (e.key === 'Escape') {
                if (filterText) {
                    e.preventDefault();
                    e.stopPropagation();
                    cancelFilter();
                }
                return;
            }
            if (e.key === 'Backspace') {
                if (filterText) {
                    e.preventDefault();
                    filterText = filterText.slice(0, -1);
                    applyFilter();
                }
                return;
            }
            if (e.key.length !== 1)
                return;                   // arrows, Tab, Enter, function keys, etc.
            if (e.key === ' ' && !filterText)
                return;                   // a leading space keeps its native meaning (opens the list)
            if (!opts().length)
                return;
            e.preventDefault();           // suppress the browser's own first-letter type-ahead
            filterText += e.key;
            applyFilter();
        });

        DOMUtils.on(el, 'blur', function () {
            endFilter();
        });

        //--

        newElm.clear = function () {
            endFilter();
            el.innerHTML = '';
            if (default_option)
                newElm.add('', default_option);
            dataStore = {};
            originalValue = null;
            return this;
        };

        newElm.add = function (val, label, data) {
            endFilter();
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
            endFilter();
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
            return opts().length;
        };

        newElm.getValue = function (row) {
            if (row !== 0 && !row) {
                const v = currentValue();
                return keyIsNumber ? Number(v) : v;
            }
            const v = opts()[row].value;
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
            endFilter();
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
                const option = opts()[logicalSelectedIndex()];
                return option ? option.text : '';
            }
            return opts()[row].text;
        };

        newElm.getAllLabels = function () {
            const r = [];
            const o = opts();
            for (let i = 0; i < o.length; i++)
                r.push(o[i].text);
            return r;
        };

        newElm.setLabel = function (lbl, row) {
            endFilter();
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
            return originalValue !== currentValue();
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

        // Disabling or hiding a focused control does not reliably blur it, so end any
        // filter here rather than leave the list narrowed until the next blur.

        newElm.disable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            endFilter();
            el.disabled = flg;
            return this;
        };

        newElm.enable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            endFilter();
            el.disabled = !flg;
            return this;
        };

        newElm.isDisabled = function () {
            return el.disabled;
        };

        newElm.hide = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            endFilter();
            if (flg)
                DOMUtils.hide(el);
            else
                DOMUtils.show(el);
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            endFilter();
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
            return logicalSelectedIndex();
        };

        newElm.selectIndex = function (idx) {
            endFilter();
            if (idx >= 0 && idx < el.options.length)
                el.selectedIndex = idx;
            return this;
        };

        newElm.removeByIndex = function (idx) {
            endFilter();
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
