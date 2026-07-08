/*
      Generic Kiss accordion / disclosure group component.
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

    function parseItems(content) {
        const host = document.createElement('div');
        host.innerHTML = content || '';
        return Array.from(host.children).filter((child) => child.localName === 'accordion-item').map((child, idx) => ({
            value: child.getAttribute('value') || child.getAttribute('id') || String(idx),
            title: child.getAttribute('title') || (child.querySelector('accordion-title') ? child.querySelector('accordion-title').textContent.trim() : '') || ('Section ' + (idx + 1)),
            note: child.getAttribute('note') || '',
            open: child.hasAttribute('open'),
            disabled: child.hasAttribute('disabled'),
            content: child.querySelector('accordion-title')
                ? child.innerHTML.replace(child.querySelector('accordion-title').outerHTML, '')
                : child.innerHTML
        }));
    }

    const processor = function (elm, attr, content) {
        let id;
        let style = '';
        let classes = '';
        let nattrs = '';
        let multiple = true;
        let persistKey = '';

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
                case 'multiple':
                    multiple = boolAttr(attr, prop, true);
                    break;
                case 'single':
                    multiple = !boolAttr(attr, prop, true);
                    break;
                case 'persist-key':
                    persistKey = Utils.removeQuotes(attr[prop]);
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        const items = parseItems(content);
        if (!id)
            id = Utils.nextID();
        let openValues = items.filter((item) => item.open).map((item) => item.value);
        if (persistKey) {
            try {
                const stored = JSON.parse(window.localStorage.getItem(persistKey) || '[]');
                if (Array.isArray(stored))
                    openValues = stored;
            } catch (e) {
                openValues = openValues;
            }
        }

        const sections = items.map((item, idx) => {
            const open = openValues.indexOf(item.value) !== -1;
            return '<section class="kiss-accordion-item' + (open ? ' is-open' : '') + '" data-value="' + esc(item.value) + '">' +
                '<button type="button" id="' + esc(id) + '-head-' + idx + '" class="kiss-accordion-head" aria-expanded="' + (open ? 'true' : 'false') +
                    '" aria-controls="' + esc(id) + '-body-' + idx + '"' + (item.disabled ? ' disabled' : '') + '>' +
                    '<span class="kiss-accordion-caret" aria-hidden="true"></span>' +
                    '<span class="kiss-accordion-copy"><span class="kiss-accordion-title">' + esc(item.title) + '</span>' +
                    (item.note ? '<span class="kiss-accordion-note">' + esc(item.note) + '</span>' : '') + '</span>' +
                '</button>' +
                '<div id="' + esc(id) + '-body-' + idx + '" class="kiss-accordion-body"' + (open ? '' : ' hidden') + '>' + item.content + '</div>' +
            '</section>';
        }).join('');

        const newElm = Utils.replaceHTML(id, elm,
            '<div id="{id}" class="kiss-accordion {class}" style="{style}" {attr}>{sections}</div>', {
                class: classes,
                style: style,
                attr: nattrs,
                sections: sections
            });
        if (!newElm)
            return;

        const root = newElm.element;
        let changeHandler = null;

        function persist() {
            if (!persistKey)
                return;
            try {
                window.localStorage.setItem(persistKey, JSON.stringify(newElm.getOpenValues()));
            } catch (e) {
                // Persistence is optional.
            }
        }

        function sectionByValue(value) {
            const val = String(value).replace(/\\/g, '\\\\').replace(/"/g, '\\"');
            return root.querySelector('.kiss-accordion-item[data-value="' + val + '"]');
        }

        function setSection(section, open, silent) {
            if (!section)
                return newElm;
            if (open && !multiple) {
                Array.from(root.querySelectorAll('.kiss-accordion-item.is-open')).forEach((other) => {
                    if (other !== section)
                        setSection(other, false, true);
                });
            }
            section.classList.toggle('is-open', open);
            const btn = section.querySelector('.kiss-accordion-head');
            const body = section.querySelector('.kiss-accordion-body');
            btn.setAttribute('aria-expanded', open ? 'true' : 'false');
            body.hidden = !open;
            persist();
            if (!silent) {
                const detail = {value: section.getAttribute('data-value'), open: open, openValues: newElm.getOpenValues()};
                if (changeHandler)
                    changeHandler(detail.value, detail.open, detail.openValues);
                root.dispatchEvent(new CustomEvent('change', {detail: detail}));
            }
            return newElm;
        }

        root.addEventListener('click', (e) => {
            const btn = e.target.closest('.kiss-accordion-head');
            if (!btn || !root.contains(btn))
                return;
            const section = btn.closest('.kiss-accordion-item');
            setSection(section, !section.classList.contains('is-open'), false);
        });

        newElm.open = function (value) { return setSection(sectionByValue(value), true, false); };
        newElm.close = function (value) { return setSection(sectionByValue(value), false, false); };
        newElm.toggle = function (value) {
            const section = sectionByValue(value);
            return setSection(section, section && !section.classList.contains('is-open'), false);
        };
        newElm.getOpenValues = function () {
            return Array.from(root.querySelectorAll('.kiss-accordion-item.is-open')).map((section) => section.getAttribute('data-value'));
        };
        newElm.setOpen = function (values, silent = true) {
            const vals = Utils.assureArray(values).map((val) => String(val));
            Array.from(root.querySelectorAll('.kiss-accordion-item')).forEach((section) => {
                setSection(section, vals.indexOf(section.getAttribute('data-value')) !== -1, true);
            });
            if (!silent)
                root.dispatchEvent(new CustomEvent('change', {detail: {openValues: this.getOpenValues()}}));
            return this;
        };
        newElm.onchange = function (fun) { changeHandler = fun; return this; };
        newElm.onChange = newElm.onchange;
    };

    Utils.newComponent({
        name: 'Accordion',
        tag: 'accordion',
        processor: processor
    });

})();
