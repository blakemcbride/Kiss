/*
 * Author: Blake McBride
 * Date:  4/22/18
 */

/* global Utils, DOMUtils */

'use strict';

(function () {

    const MAX_HEIGHT = 300;         // tallest the open list gets (px); it scrolls beyond that
    const MIN_HEIGHT = 60;          // never squeezed below this, even in a cramped viewport

    let listEl = null;              // the one open-list element shared by every drop-down, made on first use
    let openDropDown = null;        // closes the drop-down whose list is showing, if any

    const sharedList = function () {
        if (!listEl || !DOMUtils.contains(document.body, listEl)) {
            listEl = DOMUtils.create('div', {id: 'kiss-dropdown-list', class: 'kiss-dropdown-list', role: 'listbox', tabindex: '-1', hidden: ''});
            DOMUtils.appendChild(document.body, listEl);
            openDropDown = null;   // whatever control thought it owned the old (now-gone) list no longer does
        }
        return listEl;
    };

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

        //--  Type-to-filter and the open list
        //
        //  While the control has focus, typed characters accumulate in filterText and the list
        //  is narrowed to the options whose label contains that text (case-insensitive, anywhere
        //  in the label).  Non-matching options are detached from the select and re-attached, in
        //  their original order, as the filter is relaxed or ended.  While a filter is active,
        //  allOptions holds every option in its original order and is what the index-based API
        //  works against, so the filter never changes what the API reports; the only thing it
        //  changes is the selected value.
        //
        //  The browser's own pop-up list is not used for mouse or keyboard opens.  While that
        //  pop-up is open the browser delivers every keystroke to it and none to the page, so
        //  typing to filter could not work there.  Instead the control draws its own list — one
        //  <div> shared by every drop-down, appended to the body and placed under (or above)
        //  whichever drop-down is open — and the <select> itself keeps focus and receives the
        //  keys, so typing narrows the list whether it is open or closed.  The select's value is
        //  always the highlighted item:  arrow keys move it (firing change, as they do natively in
        //  a closed select), Enter or a click closes the list, and Escape returns to the value
        //  the interaction began with.  Touch input keeps the browser's native picker:  a touch
        //  device has no keyboard to filter with, and its picker is the better control there.

        let filterText = '';
        let allOptions = null;      // every option, original order; non-null only while a filter is active
        let anchorValue = null;     // the value when the interaction (typing or opening) began; Escape returns to it
        let lastValue = null;       // the last value the control actually had (used while the narrowed list is empty)
        let isOpen = false;         // the shared list is showing for this control
        let itemEls = [];           // while open: list item element per el.options index (null for hidden options)
        let lastTouch = 0;          // time of the last touch pointerdown (its mousedown keeps the native picker)

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

        const isInteractive = function () {
            return !el.disabled && !el.hasAttribute('readonly') && !el.multiple && el.size <= 1;
        };

        const beginInteraction = function () {
            if (anchorValue === null)
                anchorValue = currentValue();
        };

        const restoreAllOptions = function () {
            el.innerHTML = '';
            for (let i = 0; i < allOptions.length; i++)
                el.appendChild(allOptions[i]);
        };

        // Erase the typed sequence and restore the full list, keeping the current selection.
        const clearFilter = function () {
            filterText = '';
            if (!allOptions)
                return;
            const val = currentValue();
            restoreAllOptions();
            el.value = val;
            allOptions = null;
            lastValue = null;
        };

        // Commit:  end the filter, close the list, keep whatever is selected.
        const finishInteraction = function () {
            clearFilter();
            closeList();
            anchorValue = null;
        };

        // Escape with a typed sequence:  erase it, restore the full list, and return to the
        // selection that was current before the interaction began.  An open list stays open.
        const cancelFilter = function () {
            const before = currentValue();
            clearFilter();
            el.value = anchorValue;
            if (isOpen)
                renderList();
            else
                anchorValue = null;
            if (el.value !== before)
                DOMUtils.trigger(el, 'change');
        };

        // Escape with the list open and nothing typed:  close it and return to the value
        // the interaction began with, as the native pop-up does.
        const cancelInteraction = function () {
            const before = currentValue();
            clearFilter();
            closeList();
            el.value = anchorValue;
            anchorValue = null;
            if (el.value !== before)
                DOMUtils.trigger(el, 'change');
        };

        const applyFilter = function () {
            if (!filterText) {
                clearFilter();
                if (isOpen)
                    renderList();
                else
                    anchorValue = null;
                return;
            }
            if (!allOptions) {
                beginInteraction();
                allOptions = Array.from(el.options);
                lastValue = el.value;
            }
            const before = currentValue();
            const needle = filterText.toLowerCase();
            el.innerHTML = '';
            for (let i = 0; i < allOptions.length; i++)
                if (allOptions[i].text.toLowerCase().indexOf(needle) !== -1)
                    el.appendChild(allOptions[i]);
            if (el.options.length) {
                el.value = before;            // keep the current selection if it survived the filter ...
                if (el.selectedIndex < 0)
                    el.selectedIndex = 0;     // ... otherwise select the first match
                lastValue = el.value;
            }
            // else nothing matches; lastValue still holds the last real selection
            if (isOpen)
                renderList();
            if (el.options.length && el.value !== before)
                DOMUtils.trigger(el, 'change');
        };

        // Called on every change event (the control's own and the browser's) so the filter
        // state and the open list follow the selection however it was moved.
        const syncSelection = function () {
            if (allOptions && el.selectedIndex >= 0)
                lastValue = el.value;
            if (isOpen) {
                renderHighlight();
                scrollSelectedIntoView();
            }
        };

        //--  the open list

        const listMouseDown = function (e) {
            // An item click must not move focus off the select (the click is handled below).
            // A press on the list's scrollbar or padding is left alone so the scrollbar
            // still works; the list then takes focus briefly and hands it straight back.
            if (DOMUtils.closest(e.target, '.kiss-dropdown-item'))
                e.preventDefault();
        };

        const listClick = function (e) {
            const item = DOMUtils.closest(e.target, '.kiss-dropdown-item');
            if (!item || DOMUtils.hasClass(item, 'is-disabled'))
                return;
            const idx = Number(DOMUtils.attr(item, 'data-index'));
            const before = el.value;
            el.selectedIndex = idx;
            lastValue = el.value;
            finishInteraction();
            if (el.value !== before)
                DOMUtils.trigger(el, 'change');
            el.focus({preventScroll: true});
        };

        const listFocus = function () {
            el.focus({preventScroll: true});
        };

        //  These three are raw listeners, added per open and removed on close, because they
        //  are on the document/window, which DOMUtils' single-handler-per-event tracking
        //  would share between every drop-down.
        const outsideHandler = function (e) {
            if (e.target === el || DOMUtils.contains(sharedList(), e.target))
                return;
            finishInteraction();
        };

        const scrollHandler = function (e) {
            if (e.target === sharedList())
                return;
            finishInteraction();          // the list is positioned in the viewport; it would no longer line up
        };

        const resizeHandler = function () {
            finishInteraction();
        };

        const openList = function () {
            if (isOpen || !isInteractive() || !opts().length)
                return;
            if (openDropDown)
                openDropDown();           // only one list is ever open
            beginInteraction();
            isOpen = true;
            openDropDown = finishInteraction;
            const list = sharedList();
            const style = getComputedStyle(el);
            DOMUtils.css(list, 'fontFamily', style.fontFamily);
            DOMUtils.css(list, 'fontSize', style.fontSize);
            DOMUtils.on(list, 'mousedown', listMouseDown);
            DOMUtils.on(list, 'click', listClick);
            DOMUtils.on(list, 'focus', listFocus);
            document.addEventListener('mousedown', outsideHandler, true);
            window.addEventListener('scroll', scrollHandler, true);
            window.addEventListener('resize', resizeHandler);
            DOMUtils.removeAttr(list, 'hidden');
            DOMUtils.attr(el, 'aria-expanded', 'true');
            DOMUtils.attr(el, 'aria-controls', list.id);
            renderList();
        };

        const closeList = function () {
            if (!isOpen)
                return;
            isOpen = false;
            if (openDropDown === finishInteraction)
                openDropDown = null;
            const list = sharedList();
            DOMUtils.attr(list, 'hidden', '');
            DOMUtils.empty(list);
            itemEls = [];
            document.removeEventListener('mousedown', outsideHandler, true);
            window.removeEventListener('scroll', scrollHandler, true);
            window.removeEventListener('resize', resizeHandler);
            DOMUtils.attr(el, 'aria-expanded', 'false');
        };

        const toggleList = function () {
            if (isOpen)
                finishInteraction();
            else
                openList();
        };

        // Draw the list from the select's current (possibly narrowed) options.
        const renderList = function () {
            const list = sharedList();
            DOMUtils.empty(list);
            itemEls = [];
            if (filterText) {
                const f = DOMUtils.create('div', {class: 'kiss-dropdown-filter'});
                DOMUtils.text(f, 'Filter: ' + filterText);
                DOMUtils.appendChild(list, f);
            }
            const options = el.options;
            let lastGroup = null;
            let shown = 0;
            for (let i = 0; i < options.length; i++) {
                const o = options[i];
                if (o.hidden) {
                    itemEls.push(null);
                    continue;
                }
                const group = o.parentNode && o.parentNode.tagName === 'OPTGROUP' ? o.parentNode : null;
                if (group && group !== lastGroup) {
                    const g = DOMUtils.create('div', {class: 'kiss-dropdown-group'});
                    DOMUtils.text(g, group.label);
                    DOMUtils.appendChild(list, g);
                }
                lastGroup = group;
                const item = DOMUtils.create('div', {class: 'kiss-dropdown-item', role: 'option', 'data-index': i});
                DOMUtils.text(item, o.text);
                if (o.disabled)
                    DOMUtils.addClass(item, 'is-disabled');
                DOMUtils.appendChild(list, item);
                itemEls.push(item);
                shown++;
            }
            if (!shown) {
                const none = DOMUtils.create('div', {class: 'kiss-dropdown-empty'});
                DOMUtils.text(none, '(no matches)');
                DOMUtils.appendChild(list, none);
            }
            renderHighlight();
            positionList();
            scrollSelectedIntoView();
        };

        const renderHighlight = function () {
            for (let i = 0; i < itemEls.length; i++) {
                if (!itemEls[i])
                    continue;
                if (i === el.selectedIndex) {
                    DOMUtils.addClass(itemEls[i], 'is-selected');
                    DOMUtils.attr(itemEls[i], 'aria-selected', 'true');
                } else {
                    DOMUtils.removeClass(itemEls[i], 'is-selected');
                    DOMUtils.attr(itemEls[i], 'aria-selected', 'false');
                }
            }
        };

        const scrollSelectedIntoView = function () {
            const item = itemEls[el.selectedIndex];
            if (item && item.scrollIntoView)
                item.scrollIntoView({block: 'nearest'});
        };

        // Place the list under the select, or above it when there is more room there,
        // never taller than MAX_HEIGHT and never off the edge of the viewport.
        const positionList = function () {
            const list = sharedList();
            const r = DOMUtils.getRect(el);
            const gap = 3;
            const margin = 8;
            DOMUtils.css(list, 'minWidth', r.width + 'px');
            DOMUtils.css(list, 'maxHeight', '');
            const natural = list.offsetHeight;
            const below = window.innerHeight - r.bottom - gap - margin;
            const above = r.top - gap - margin;
            let height = Math.min(natural, MAX_HEIGHT);
            let top;
            if (height <= below || below >= above) {
                height = Math.min(height, Math.max(below, MIN_HEIGHT));
                DOMUtils.css(list, 'maxHeight', height + 'px');
                top = r.bottom + gap;
            } else {
                height = Math.min(height, Math.max(above, MIN_HEIGHT));
                DOMUtils.css(list, 'maxHeight', height + 'px');
                top = Math.max(0, r.top - gap - list.offsetHeight);
            }
            let left = r.left;
            const width = list.offsetWidth;
            if (left + width > window.innerWidth - margin)
                left = Math.max(margin, window.innerWidth - margin - width);
            DOMUtils.css(list, 'left', left + 'px');
            DOMUtils.css(list, 'top', top + 'px');
        };

        // Move the selection by one visible, enabled option (the open list's arrow keys).
        const moveSelection = function (dir) {
            const options = el.options;
            const n = options.length;
            if (!n)
                return;
            let i = el.selectedIndex;
            if (i < 0)
                i = dir > 0 ? -1 : n;
            for (i += dir; i >= 0 && i < n; i += dir)
                if (!options[i].disabled && !options[i].hidden)
                    break;
            if (i < 0 || i >= n)
                return;
            const before = el.value;
            el.selectedIndex = i;
            syncSelection();
            if (el.value !== before)
                DOMUtils.trigger(el, 'change');
        };

        //--  events

        const VALUE_KEYS = ['ArrowDown', 'ArrowUp', 'Home', 'End', 'PageUp', 'PageDown', 'Backspace', 'F4'];

        DOMUtils.on(el, 'keydown', function (e) {
            if (e.isComposing || el.multiple)
                return;
            if (el.hasAttribute('readonly')) {
                if (e.key.length === 1 || VALUE_KEYS.indexOf(e.key) !== -1)
                    e.preventDefault();   // a read-only drop-down shows its value; nothing opens or changes it
                return;
            }
            if (((e.key === 'ArrowDown' || e.key === 'ArrowUp') && e.altKey && !e.ctrlKey && !e.metaKey) || e.key === 'F4') {
                e.preventDefault();       // Alt+Down / Alt+Up / F4 open and close the list, as they do natively
                toggleList();
                return;
            }
            if (e.ctrlKey || e.altKey || e.metaKey)
                return;
            if (isOpen) {
                switch (e.key) {
                    case 'ArrowDown':
                        e.preventDefault();
                        moveSelection(1);
                        return;
                    case 'ArrowUp':
                        e.preventDefault();
                        moveSelection(-1);
                        return;
                    case 'Enter':
                        e.preventDefault();
                        e.stopPropagation();
                        finishInteraction();
                        return;
                    case 'Tab':
                        finishInteraction();
                        return;
                }
            }
            if (e.key === 'Escape') {
                if (filterText) {
                    e.preventDefault();
                    e.stopPropagation();
                    cancelFilter();
                } else if (isOpen) {
                    e.preventDefault();
                    e.stopPropagation();
                    cancelInteraction();
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
            if (e.key === ' ' && !filterText) {
                e.preventDefault();       // a lone space opens and closes the list, as it does natively
                toggleList();
                return;
            }
            if (!opts().length)
                return;
            e.preventDefault();           // suppress the browser's own first-letter type-ahead
            filterText += e.key;
            applyFilter();
        });

        // A touch tap is followed by a synthesized mousedown with no pointerdown of its own,
        // while a real mouse press always announces itself with a mouse pointerdown first.
        DOMUtils.on(el, 'pointerdown', function (e) {
            lastTouch = e.pointerType === 'touch' ? Date.now() : 0;
        });

        DOMUtils.on(el, 'mousedown', function (e) {
            if (e.button !== 0 || el.multiple || el.size > 1)
                return;
            if (Date.now() - lastTouch < 1000)
                return;                   // a touch tap:  leave the native picker to the browser
            e.preventDefault();           // keeps the browser's own pop-up closed
            if (el.disabled)
                return;
            el.focus({preventScroll: true});
            toggleList();
        });

        DOMUtils.on(el, 'blur', function (e) {
            if (isOpen && e.relatedTarget === sharedList())
                return;                   // a press on the list's scrollbar; focus comes straight back
            finishInteraction();
        });

        changeHandler = function () {
            syncSelection();
            if (triggerGlobalChange)
                Utils.someControlValueChanged();
        };
        DOMUtils.on(el, 'change', changeHandler);

        //--

        newElm.clear = function () {
            finishInteraction();
            el.innerHTML = '';
            if (default_option)
                newElm.add('', default_option);
            dataStore = {};
            originalValue = null;
            return this;
        };

        newElm.add = function (val, label, data) {
            finishInteraction();
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
            finishInteraction();
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
            finishInteraction();
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
            finishInteraction();
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
        // filter and close the list here rather than leave them until the next blur.

        newElm.disable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            finishInteraction();
            el.disabled = flg;
            return this;
        };

        newElm.enable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            finishInteraction();
            el.disabled = !flg;
            return this;
        };

        newElm.isDisabled = function () {
            return el.disabled;
        };

        newElm.hide = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            finishInteraction();
            if (flg)
                DOMUtils.hide(el);
            else
                DOMUtils.show(el);
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            finishInteraction();
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
                syncSelection();
                if (triggerGlobalChange)
                    Utils.someControlValueChanged();
                // func gets passed the selected value, label
                if (func) {
                    const option = el.options[el.selectedIndex];
                    func(el.value, option ? option.text : '', dataStore[el.value]);
                }
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
            finishInteraction();
            if (idx >= 0 && idx < el.options.length)
                el.selectedIndex = idx;
            return this;
        };

        newElm.removeByIndex = function (idx) {
            finishInteraction();
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
