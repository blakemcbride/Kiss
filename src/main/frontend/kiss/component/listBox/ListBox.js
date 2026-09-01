/*
      Author: Blake McBride
      Date:  5/25/18
*/

/* global Utils, DOMUtils */

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
        DOMUtils.on(el, 'change', changeHandler);

        //--  Type-to-filter
        //
        //  While the control has focus, typed characters accumulate in filterText and the list
        //  is narrowed to the options whose label contains that text (case-insensitive, anywhere
        //  in the label).  Non-matching options are detached from the select and re-attached, in
        //  their original order, as the filter is relaxed or ended.  While a filter is active,
        //  allOptions holds every option in its original order and is what the index-based API
        //  works against, so the filter never changes what the API reports; the only thing it
        //  changes is the selection.  A single-select list keeps its selection when it still
        //  matches and otherwise selects the first match (as native type-ahead does); a
        //  multiple-select list only narrows what is shown and leaves every selection flag alone.

        let filterText = '';
        let allOptions = null;         // every option, original order; non-null only while a filter is active
        let preFilterSelected = null;  // when typing began:  single = the selected option (or null); multiple = one flag per option
        let lastSelected = null;       // single:  the option last actually selected (used while the narrowed list is empty)

        const opts = function () {
            return allOptions ? allOptions : el.options;
        };

        // Single-select:  the logically selected option.  When the narrowed list is empty
        // nothing is selected, so report the option that was last actually selected.
        const currentOption = function () {
            const o = el.selectedOptions[0] || null;
            return allOptions && !o ? lastSelected : o;
        };

        // The logically selected option (the first one, for a multiple-select list).
        const firstSelected = function () {
            if (!multiple)
                return currentOption();
            const o = opts();
            for (let i = 0; i < o.length; i++)
                if (o[i].selected)
                    return o[i];
            return null;
        };

        const selectedFlags = function () {
            const o = opts(), r = [];
            for (let i = 0; i < o.length; i++)
                r.push(o[i].selected);
            return r;
        };

        const sameFlags = function (a, b) {
            for (let i = 0; i < a.length; i++)
                if (a[i] !== b[i])
                    return false;
            return true;
        };

        // Single-select:  make option (or nothing, when null) the selection.
        const selectOnly = function (option) {
            el.selectedIndex = -1;
            if (option)
                option.selected = true;
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
            const option = multiple ? null : currentOption();
            restoreAllOptions();
            if (!multiple)
                selectOnly(option);
            allOptions = null;
            preFilterSelected = null;
            lastSelected = null;
        };

        // Escape:  erase the typed sequence, restore the full list, and return to the
        // selection that was current before typing began.
        const cancelFilter = function () {
            filterText = '';
            if (!allOptions)
                return;
            let changed;
            if (multiple) {
                const before = selectedFlags();
                restoreAllOptions();
                for (let i = 0; i < allOptions.length; i++)
                    allOptions[i].selected = preFilterSelected[i];
                changed = !sameFlags(before, preFilterSelected);
            } else {
                const before = currentOption();
                restoreAllOptions();
                selectOnly(preFilterSelected);
                changed = preFilterSelected !== before;
            }
            allOptions = null;
            preFilterSelected = null;
            lastSelected = null;
            if (changed)
                DOMUtils.trigger(el, 'change');
        };

        const applyFilter = function () {
            if (!filterText) {
                endFilter();
                return;
            }
            if (!allOptions) {
                allOptions = Array.from(el.options);
                preFilterSelected = multiple ? selectedFlags() : (el.selectedOptions[0] || null);
                lastSelected = multiple ? null : (el.selectedOptions[0] || null);
            }
            const before = multiple ? null : currentOption();
            const needle = filterText.toLowerCase();
            el.innerHTML = '';
            for (let i = 0; i < allOptions.length; i++)
                if (allOptions[i].text.toLowerCase().indexOf(needle) !== -1)
                    el.appendChild(allOptions[i]);
            if (multiple || !el.options.length)
                return;               // multiple:  selection flags are left alone.  Empty:  lastSelected still holds the last real selection
            // keep the current selection if it survived the filter, otherwise select the first match
            const option = before && before.parentNode === el ? before : el.options[0];
            selectOnly(option);
            lastSelected = option;
            if (option !== before)
                DOMUtils.trigger(el, 'change');
        };

        DOMUtils.on(el, 'keydown', function (e) {
            if (e.ctrlKey || e.altKey || e.metaKey || e.isComposing)
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
                return;               // arrows, Tab, Enter, function keys, etc.
            if (e.key === ' ' && !filterText)
                return;               // a leading space keeps its native meaning
            if (!opts().length)
                return;
            e.preventDefault();       // suppress the browser's own first-letter type-ahead
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
        }

        newElm.size = function () {
            return opts().length;
        };

        //--

        newElm.getValue = function (row) {
            if (row !== 0 && !row) {
                // For multi-select with no specific row requested, return an array of all selected values.
                if (multiple) {
                    const r = [];
                    const o = opts();
                    for (let i = 0; i < o.length; i++)
                        if (o[i].selected)
                            r.push(keyIsNumber ? Number(o[i].value) : o[i].value);
                    return r;
                }
                const option = currentOption();
                const v = option ? option.value : '';
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
            console.log("ListBox.js getIntValue called");
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
                const selectedOption = firstSelected();
                return selectedOption ? selectedOption.textContent : '';
            }
            return opts()[row].textContent;
        };

        newElm.getAllLabels = function () {
            const r = [];
            const o = opts();
            for (let i = 0; i < o.length; i++)
                r.push(o[i].textContent);
            return r;
        };

        newElm.setLabel = function (lbl, row) {
            endFilter();
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
            const selectedOption = firstSelected();
            return originalValue !== (selectedOption ? selectedOption.value : '');
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

        //--

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
            return DOMUtils.isVisible(el);
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
            DOMUtils.off(el, 'change', changeHandler);
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
            DOMUtils.on(el, 'change', changeHandler);
            return this;
        };

        newElm.triggerGlobalChange = function (flg) {
            triggerGlobalChange = flg;
        }

        let timeout;
        let clickHandler;

        newElm.onClick = function (fun) {
            if (clickHandler)
                DOMUtils.off(el, 'click', clickHandler);
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
            DOMUtils.on(el, 'click', clickHandler);
            return this;
        };

        // double-click is not recognised on mobile devices
        let dblclickHandler;

        newElm.onDblClick = function (fun) {
            if (dblclickHandler)
                DOMUtils.off(el, 'dblclick', dblclickHandler);
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
            DOMUtils.on(el, 'dblclick', dblclickHandler);
            return this;
        };

        newElm.selectedIndex = function () {
            const selectedOption = firstSelected();
            return selectedOption ? Array.prototype.indexOf.call(opts(), selectedOption) : -1;
        };

        newElm.selectIndex = function (idx) {
            endFilter();
            if (idx >= 0 && idx < el.options.length)
                el.options[idx].selected = true;
            return this;
        };

        newElm.removeByIndex = function (idx) {
            endFilter();
            const val = el.value;
            if (idx < el.options.length)
                el.options[idx].remove();
            if (val)
                delete dataStore[val];
            originalValue = el.value;
            return this;
        };

        newElm.clearSelection = function () {
            endFilter();
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

