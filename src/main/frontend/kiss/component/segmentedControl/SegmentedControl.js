/*
      Generic Kiss segmented control component.
 */

/* global Utils */

'use strict';

(function () {

    function esc(s) {
        const str = s == null ? '' : String(s);
        return str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function hasAttr(attr, name) {
        return Object.prototype.hasOwnProperty.call(attr, name);
    }

    function boolAttr(attr, name, def) {
        if (!hasAttr(attr, name))
            return def;
        const val = Utils.removeQuotes(attr[name]);
        return val !== 'false' && val !== '0' && val !== 'no';
    }

    function parseSegments(content) {
        const host = document.createElement('div');
        host.innerHTML = content || '';
        return Array.from(host.children).filter((child) => child.localName === 'segment').map((child, idx) => ({
            value: child.getAttribute('value') || String(idx),
            label: child.textContent.trim(),
            html: child.innerHTML,
            selected: child.hasAttribute('selected') || child.hasAttribute('active'),
            disabled: child.hasAttribute('disabled')
        }));
    }

    function renderSegment(seg, selected, idx) {
        return '<button type="button" class="kiss-segment' + (selected ? ' is-active' : '') +
            '" role="tab" aria-selected="' + (selected ? 'true' : 'false') + '" data-value="' +
            esc(seg.value) + '" data-label="' + esc(seg.label) + '" tabindex="' + (selected ? '0' : '-1') +
            '"' + (seg.disabled ? ' disabled aria-disabled="true"' : '') + '>' + seg.html + '</button>';
    }

    const processor = function (elm, attr, content) {
        let id;
        let style = '';
        let classes = '';
        let nattrs = '';
        let value = null;
        let equal = false;
        let disabled = false;
        let ariaLabel = '';

        for (let prop in attr) {
            switch (prop) {
                case 'id':
                    id = Utils.removeQuotes(attr[prop]);
                    break;
                case 'style':
                    style = attr[prop];
                    break;
                case 'class':
                    classes = attr[prop];
                    break;
                case 'value':
                    value = Utils.removeQuotes(attr[prop]);
                    break;
                case 'equal':
                    equal = boolAttr(attr, prop, true);
                    break;
                case 'disabled':
                    disabled = boolAttr(attr, prop, false);
                    break;
                case 'aria-label':
                    ariaLabel = attr[prop];
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        let segments = parseSegments(content);
        if (!segments.length)
            segments = [{value: 'one', label: 'One', html: 'One', selected: true, disabled: false}];
        if (value == null) {
            const selected = segments.find((seg) => seg.selected);
            value = selected ? selected.value : segments[0].value;
        }

        const rootClass = 'kiss-segmented-control' + (equal ? ' kiss-segmented-equal' : '') +
            (disabled ? ' is-disabled' : '') + (classes ? ' ' + classes : '');
        const html = segments.map((seg, idx) => renderSegment(seg, String(seg.value) === String(value), idx)).join('');
        const newElm = Utils.replaceHTML(id, elm,
            '<div id="{id}" class="{class}" style="{style}" role="tablist" aria-label="{ariaLabel}" {attr}>{segments}</div>', {
                class: rootClass,
                style: style,
                ariaLabel: ariaLabel,
                attr: nattrs,
                segments: html
            });
        if (!newElm)
            return;

        const root = newElm.element;
        let changeHandler = null;

        function buttons() {
            return Array.from(root.querySelectorAll('.kiss-segment'));
        }

        function select(val, silent) {
            const all = buttons();
            const next = all.find((btn) => String(btn.getAttribute('data-value')) === String(val) && !btn.disabled);
            if (!next)
                return newElm;
            value = next.getAttribute('data-value');
            all.forEach((btn) => {
                const active = btn === next;
                btn.classList.toggle('is-active', active);
                btn.setAttribute('aria-selected', active ? 'true' : 'false');
                btn.tabIndex = active ? 0 : -1;
            });
            if (!silent) {
                const detail = {value: value, label: next.getAttribute('data-label') || '', item: next};
                if (changeHandler)
                    changeHandler(detail.value, detail.label, detail.item);
                root.dispatchEvent(new CustomEvent('change', {detail: detail}));
            }
            return newElm;
        }

        root.addEventListener('click', (e) => {
            const btn = e.target.closest('.kiss-segment');
            if (btn && root.contains(btn))
                select(btn.getAttribute('data-value'), false);
        });

        root.addEventListener('keydown', (e) => {
            const all = buttons().filter((btn) => !btn.disabled);
            const idx = all.indexOf(document.activeElement);
            let next = null;
            if (e.key === 'ArrowRight' || e.key === 'ArrowDown')
                next = all[(idx + 1 + all.length) % all.length];
            else if (e.key === 'ArrowLeft' || e.key === 'ArrowUp')
                next = all[(idx - 1 + all.length) % all.length];
            else if (e.key === 'Home')
                next = all[0];
            else if (e.key === 'End')
                next = all[all.length - 1];
            if (next) {
                e.preventDefault();
                next.focus();
                select(next.getAttribute('data-value'), false);
            }
        });

        newElm.getValue = function () { return value; };
        newElm.setValue = function (val, silent = true) { return select(val, silent); };
        newElm.onchange = function (fun) { changeHandler = fun; return this; };
        newElm.onChange = newElm.onchange;
        newElm.disable = function (flg = true) {
            root.classList.toggle('is-disabled', !!flg);
            buttons().forEach((btn) => { btn.disabled = !!flg; });
            return this;
        };
        newElm.enable = function (flg = true) { return this.disable(!flg); };
        newElm.fill = function (items, selectedItem, valField = 'value', labelField = 'label') {
            const rows = Array.isArray(items) ? items : [];
            root.innerHTML = rows.map((item, idx) => {
                const seg = typeof item === 'object'
                    ? {value: item[valField], label: item[labelField], html: esc(item[labelField]), disabled: !!item.disabled}
                    : {value: item, label: item, html: esc(item), disabled: false};
                return renderSegment(seg, String(seg.value) === String(selectedItem), idx);
            }).join('');
            return select(selectedItem == null && rows.length ? (typeof rows[0] === 'object' ? rows[0][valField] : rows[0]) : selectedItem, true);
        };
        if (disabled)
            newElm.disable(true);
    };

    Utils.newComponent({
        name: 'SegmentedControl',
        tag: 'segmented-control',
        processor: processor
    });

})();
