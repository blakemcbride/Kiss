/*
 * Author: Blake McBride
 *
 * The "combo-box" component is the equivalent of the Microsoft Windows combobox
 * control.  It is a drop-down list that ALSO allows the user to type in a value
 * that is not in the list (free entry).
 *
 * It is a custom drop-down (not the native HTML <datalist>): the native datalist
 * behaves as a type-ahead filter, so once the user types text that matches no
 * option it shows nothing and the full list can no longer be reopened.  A real
 * Windows combobox always reopens the complete list from the drop-down arrow
 * regardless of what was typed, which is what this control does.
 */

/* global Utils, DOMUtils */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nStyle;
        let required = false;
        if (attr.style)
            nStyle = attr.style;
        else
            nStyle = '';
        let nAttrs = '';
        let id;

        for (let prop of Object.keys(attr)) {
            switch (prop) {

                // New attributes.

                case 'required':
                    required = true;
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

        //  The control is a wrapper <span> containing the editable <input> (which
        //  carries the supplied id so $$('id'), <label for>, and focus all work),
        //  a clickable drop-down arrow, and the drop-down list (a <ul> shown/hidden
        //  on demand).  Structural styling is inline.
        //  The wrapper carries the user's style (so a width/margin sizes the whole
        //  control) and is a flex row: the <input> grows to fill it (flex:1 — no
        //  width:100%) and the drop-down arrow overlays its right edge.  The wrapper
        //  stretches to fill a grid/flex cell exactly like a bare <input>/<select>,
        //  so the control lines up with the others on the screen.
        const newElm = Utils.replaceHTML(id, elm,
            '<span id="{id}-wrap" style="{style}; position:relative; display:flex; align-items:center; box-sizing:border-box;">' +
                '<input type="text" id="{id}" autocomplete="off" ' +
                    'style="flex:1 1 0; min-width:0; padding-right:22px; text-align:left;" {attr}>' +
                '<span id="{id}-arrow" style="position:absolute; top:0; right:0; height:100%; width:22px; ' +
                    'display:flex; align-items:center; justify-content:center; cursor:pointer; ' +
                    'font-size:12px; color:#333; user-select:none;">&#9660;</span>' +
                '<ul id="{id}-list" style="display:none; position:absolute; top:100%; left:0; z-index:1000; ' +
                    'margin:0; padding:0; list-style:none; min-width:100%; max-height:200px; overflow-y:auto; ' +
                    'background:#fff; border:1px solid #999; border-radius:4px; text-align:left; color:#000; ' +
                    'box-shadow:0 2px 6px rgba(0,0,0,0.2); box-sizing:border-box;"></ul>' +
            '</span>', {
                style: nStyle,
                attr: nAttrs,
                content: ''      // static <option> content is parsed in JS below
            });
        if (!newElm)
            return;
        const el = newElm.element;                          // the <input>
        const baseId = el.id;
        const wrap = DOMUtils.getElement(baseId + '-wrap');
        const arrow = DOMUtils.getElement(baseId + '-arrow');
        const list = DOMUtils.getElement(baseId + '-list');

        let originalValue = '';
        let keyIsNumber = false;
        let triggerGlobalChange = true;
        let enterFunction = null;
        let onChangeFn = null;
        let onCChangeFn = null;

        let items = [];          // { value, label, data }
        let isOpen = false;
        let highlightIndex = -1;

        function currentText() {
            const v = el.value;
            return v ? v.trim() : '';
        }

        function isInteractive() {
            return !el.disabled && !el.readOnly;
        }

        //--  drop-down open/close/highlight  --

        function renderHighlight() {
            const lis = list.children;
            for (let i = 0; i < lis.length; i++) {
                if (i === highlightIndex) {
                    lis[i].style.background = '#2563eb';
                    lis[i].style.color = '#fff';
                } else {
                    lis[i].style.background = '';
                    lis[i].style.color = '';
                }
            }
        }

        function setHighlight(i) {
            highlightIndex = i;
            renderHighlight();
        }

        function scrollHighlightIntoView() {
            if (highlightIndex >= 0 && list.children[highlightIndex])
                list.children[highlightIndex].scrollIntoView({ block: 'nearest' });
        }

        function outsideHandler(e) {
            if (!wrap.contains(e.target))
                closeList();
        }

        function openList() {
            if (!isInteractive() || !items.length)
                return;
            //  highlight the item matching the current text (if any)
            const txt = currentText();
            let idx = -1;
            for (let i = 0; i < items.length; i++)
                if (items[i].label === txt) {
                    idx = i;
                    break;
                }
            list.style.display = 'block';
            isOpen = true;
            setHighlight(idx);
            scrollHighlightIntoView();
            //  raw listener (per instance) so multiple combo-boxes do not clobber
            //  each other the way DOMUtils' single-handler-per-event tracking would
            document.addEventListener('mousedown', outsideHandler, true);
        }

        function closeList() {
            if (!isOpen)
                return;
            list.style.display = 'none';
            isOpen = false;
            document.removeEventListener('mousedown', outsideHandler, true);
        }

        function toggleList() {
            if (isOpen)
                closeList();
            else
                openList();
        }

        function selectIndex(i) {
            if (i < 0 || i >= items.length)
                return;
            el.value = items[i].label;     // user action: do not reset originalValue (keeps isDirty correct)
            closeList();
            if (triggerGlobalChange)
                Utils.someControlValueChanged();
            if (onChangeFn)
                onChangeFn(newElm.getValue(), newElm.getText(), newElm.getData());
            el.focus();
        }

        //--  events  --

        DOMUtils.on(arrow, 'mousedown', function (e) {
            e.preventDefault();            // keep the input focused
        });
        DOMUtils.on(arrow, 'click', function () {
            if (!isInteractive())
                return;
            el.focus();
            toggleList();
        });

        //  Clicking into the edit field also drops the list down (so "going into it"
        //  always shows the choices, the way the user expects).  Not prevented, so the
        //  cursor still lands where clicked.
        DOMUtils.on(el, 'mousedown', function () {
            if (isInteractive() && !isOpen)
                openList();
        });

        DOMUtils.on(el, 'input', function () {
            if (triggerGlobalChange)
                Utils.someControlValueChanged();
            if (onCChangeFn)
                onCChangeFn(newElm.getValue());
        });

        DOMUtils.on(el, 'change', function () {
            if (triggerGlobalChange)
                Utils.someControlValueChanged();
            if (onChangeFn)
                onChangeFn(newElm.getValue(), newElm.getText(), newElm.getData());
        });

        DOMUtils.on(el, 'keydown', function (e) {
            switch (e.key) {
                case 'ArrowDown':
                    e.preventDefault();
                    if (!isOpen)
                        openList();
                    else
                        setHighlight(Math.min(highlightIndex + 1, items.length - 1));
                    scrollHighlightIntoView();
                    break;
                case 'ArrowUp':
                    e.preventDefault();
                    if (!isOpen)
                        openList();
                    else
                        setHighlight(Math.max(highlightIndex - 1, 0));
                    scrollHighlightIntoView();
                    break;
                case 'Enter':
                    if (isOpen && highlightIndex >= 0) {
                        e.preventDefault();
                        selectIndex(highlightIndex);
                    } else if (enterFunction) {
                        e.stopPropagation();
                        enterFunction();
                    }
                    break;
                case 'Escape':
                    if (isOpen) {
                        e.preventDefault();
                        closeList();
                    }
                    break;
                case 'Tab':
                    closeList();
                    break;
                default:
                    break;
            }
        });

        //--  API  --

        newElm.clear = function () {
            el.value = originalValue = '';
            return this;
        };

        newElm.clearList = function () {
            items = [];
            list.innerHTML = '';
            highlightIndex = -1;
            return this;
        };

        newElm.add = function (val, label, data) {
            //  Single-argument form -> the value is also the label (a plain string list item).
            if (label === undefined || label === null)
                label = val;
            if (typeof val === 'number')
                keyIsNumber = true;
            items.push({ value: val, label: label, data: data });
            const idx = items.length - 1;
            const li = document.createElement('li');
            li.textContent = label;
            li.style.padding = '3px 8px';
            li.style.cursor = 'pointer';
            li.style.whiteSpace = 'nowrap';
            DOMUtils.on(li, 'mousedown', function (e) {
                e.preventDefault();        // keep input focus; fire selection on mousedown (before blur)
                selectIndex(idx);
            });
            DOMUtils.on(li, 'mouseenter', function () {
                setHighlight(idx);
            });
            list.appendChild(li);
            return this;
        };

        newElm.addItems = function (items_, valField, labelField, dataField) {
            items_ = Utils.assureArray(items_);
            const len = items_.length;
            for (let i = 0; i < len; i++) {
                const item = items_[i];
                const lbl = typeof labelField === 'function' ? labelField(item) : item[labelField];
                const val = item[valField];
                newElm.add(val, lbl, dataField ? item[dataField] : item);
            }
            return this;
        };

        newElm.fill = function (selectedItem, items_, valField, labelField, dataField) {
            newElm.clearList();
            newElm.addItems(items_, valField, labelField, dataField);
            if (selectedItem !== undefined && selectedItem !== null && selectedItem !== '')
                newElm.setValue(selectedItem);
            else
                newElm.clear();
            return this;
        };

        newElm.size = function () {
            return items.length;
        };

        //  Returns the underlying value:  if what is shown matches a known list
        //  item, its value is returned; otherwise the typed (free-entry) text is
        //  returned as-is.
        newElm.getValue = function () {
            const txt = currentText();
            for (let i = 0; i < items.length; i++)
                if (items[i].label === txt) {
                    const v = items[i].value;
                    return keyIsNumber && v !== '' && v !== null && v !== undefined ? Number(v) : v;
                }
            return txt;
        };

        //  Returns the text shown in the control (the label) regardless of whether
        //  it is a list item or free entry.
        newElm.getText = function () {
            return currentText();
        };

        newElm.getLabel = newElm.getText;

        //  Sets the control.  If val matches the underlying value of a list item,
        //  that item's label is shown; otherwise val is shown as free text.
        newElm.setValue = function (val) {
            if (val === undefined || val === null) {
                el.value = originalValue = '';
                return this;
            }
            for (let i = 0; i < items.length; i++)
                if (items[i].value === val || String(items[i].value) === String(val)) {
                    el.value = originalValue = items[i].label;
                    return this;
                }
            el.value = originalValue = String(val);
            return this;
        };

        //  Sets the text shown in the control directly (treats val as the label).
        newElm.setText = function (val) {
            el.value = originalValue = (val === undefined || val === null) ? '' : String(val);
            return this;
        };

        newElm.getData = function () {
            const txt = currentText();
            for (let i = 0; i < items.length; i++)
                if (items[i].label === txt)
                    return items[i].data;
            return undefined;
        };

        newElm.getAllData = function () {
            const r = {};
            for (let i = 0; i < items.length; i++)
                r[items[i].label] = items[i].data;
            return r;
        };

        newElm.getAllLabels = function () {
            const r = [];
            for (let i = 0; i < items.length; i++)
                r.push(items[i].label);
            return r;
        };

        newElm.isDirty = function () {
            return originalValue !== currentText();
        };

        //--

        newElm.readOnly = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.readOnly = flg;
            if (flg)
                closeList();
            return this;
        };

        newElm.readWrite = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.readOnly = !flg;
            return this;
        };

        newElm.isReadOnly = function () {
            return el.readOnly;
        };

        //--

        newElm.disable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.disabled = flg;
            arrow.style.color = flg ? '#bbb' : '#333';
            arrow.style.cursor = flg ? 'default' : 'pointer';
            if (flg)
                closeList();
            return this;
        };

        newElm.enable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            return newElm.disable(!flg);
        };

        newElm.isDisabled = function () {
            return el.disabled;
        };

        //--

        newElm.hide = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                DOMUtils.hide(wrap);
            else
                DOMUtils.show(wrap);
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                DOMUtils.show(wrap);
            else
                DOMUtils.hide(wrap);
            return this;
        };

        newElm.isHidden = function () {
            return DOMUtils.isHidden(wrap);
        };

        newElm.isVisible = function () {
            return !DOMUtils.isHidden(wrap);
        };

        //--

        //  Fires whenever the user commits a change (selection or leaving the field
        //  after editing).  fun is passed (value, text, data).
        newElm.onChange = function (func) {
            onChangeFn = func;
            return this;
        };

        //  Fires immediately on each change the user makes (every keystroke / paste).
        //  fun is passed the current value.
        newElm.onCChange = function (func) {
            onCChangeFn = func;
            return this;
        };

        newElm.onEnter = function (func) {
            enterFunction = func;
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
            const val = newElm.getValue();
            if (val === '' || val === null || val === undefined) {
                Utils.showMessage('Error', desc + ' is required.').then(function () {
                    el.focus();
                });
                return true;
            }
            return false;
        };

        //--  parse any static <option> tag content into the list  --

        if (content && content.trim()) {
            const tmp = document.createElement('div');
            tmp.innerHTML = content;
            const opts = tmp.querySelectorAll('option');
            for (let i = 0; i < opts.length; i++) {
                const o = opts[i];
                const lbl = (o.textContent || '').trim();
                const value = o.getAttribute('value');
                if (value !== null && lbl)
                    newElm.add(value, lbl);
                else if (value !== null)
                    newElm.add(value);
                else if (lbl)
                    newElm.add(lbl);
            }
        }

    };

    const componentInfo = {
        name: 'ComboBox',
        tag: 'combo-box',
        processor: processor
    };

    Utils.newComponent(componentInfo);
})();
