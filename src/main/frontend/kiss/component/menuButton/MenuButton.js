/*
      Generic Kiss menu button component.
 */

/* global Utils */

'use strict';

(function () {

    function hasAttr(attr, name) {
        return Object.prototype.hasOwnProperty.call(attr, name);
    }

    function boolAttr(attr, name, def) {
        if (!hasAttr(attr, name))
            return def;
        const val = Utils.removeQuotes(attr[name]);
        return val !== 'false' && val !== '0' && val !== 'no';
    }

    function esc(s) {
        const str = s == null ? '' : String(s);
        return str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function parseItems(content) {
        const host = document.createElement('div');
        host.innerHTML = content || '';
        const trigger = host.querySelector('menu-trigger');
        const triggerHtml = trigger ? trigger.innerHTML : '';
        if (trigger)
            trigger.remove();
        const items = [];
        Array.from(host.children).forEach((child, idx) => {
            const tag = child.localName;
            if (tag === 'menu-separator') {
                items.push({separator: true});
                return;
            }
            if (tag !== 'menu-item')
                return;
            items.push({
                value: child.getAttribute('value') || child.getAttribute('id') || String(idx),
                label: child.textContent.trim(),
                html: child.innerHTML,
                disabled: child.hasAttribute('disabled'),
                danger: child.hasAttribute('danger'),
                href: child.getAttribute('href') || '',
                target: child.getAttribute('target') || ''
            });
        });
        return {triggerHtml: triggerHtml, items: items};
    }

    function itemHtml(item, idx) {
        if (item.separator)
            return '<div class="kiss-menu-separator" role="separator"></div>';
        const cls = 'kiss-menu-item' + (item.danger ? ' kiss-menu-item-danger' : '');
        const disabled = item.disabled ? ' disabled aria-disabled="true"' : '';
        const href = item.href ? ' data-href="' + esc(item.href) + '"' : '';
        const target = item.target ? ' data-target="' + esc(item.target) + '"' : '';
        return '<button type="button" class="' + cls + '" role="menuitem" data-menu-index="' + idx +
            '" data-value="' + esc(item.value) + '" data-label="' + esc(item.label) + '"' +
            href + target + disabled + '>' + item.html + '</button>';
    }

    const processor = function (elm, attr, content) {
        let id;
        let nattrs = '';
        let style = '';
        let classes = '';
        let label = 'Menu';
        let buttonClass = '';
        let menuClass = '';
        let align = 'start';
        let placement = 'bottom';
        let disabled = false;
        let startOpen = false;
        let closeOnSelect = true;

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
                case 'label':
                case 'text':
                    label = Utils.removeQuotes(attr[prop]);
                    break;
                case 'button-class':
                    buttonClass = attr[prop];
                    break;
                case 'menu-class':
                    menuClass = attr[prop];
                    break;
                case 'align':
                    align = Utils.removeQuotes(attr[prop]) || align;
                    break;
                case 'placement':
                    placement = Utils.removeQuotes(attr[prop]) || placement;
                    break;
                case 'disabled':
                    disabled = boolAttr(attr, prop, false);
                    break;
                case 'open':
                    startOpen = boolAttr(attr, prop, false);
                    break;
                case 'close-on-select':
                    closeOnSelect = boolAttr(attr, prop, true);
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        const parsed = parseItems(content);
        if (!id)
            id = Utils.nextID();
        const triggerHtml = parsed.triggerHtml || esc(label);
        const body = parsed.items.map(itemHtml).join('');
        const rootClass = 'kiss-menu-button kiss-menu-align-' + esc(align) +
            ' kiss-menu-placement-' + esc(placement) + (classes ? ' ' + classes : '');

        const newElm = Utils.replaceHTML(id, elm,
            '<div id="{id}" class="{class}" style="{style}" {attr}>' +
                '<button type="button" id="{id}-trigger" class="kiss-menu-trigger {buttonClass}" aria-haspopup="true" aria-expanded="false"{disabled}>{trigger}<span class="kiss-menu-caret" aria-hidden="true"></span></button>' +
                '<div id="{id}-menu" class="kiss-menu-list {menuClass}" role="menu" hidden>{items}</div>' +
            '</div>', {
                class: rootClass,
                style: style,
                attr: nattrs,
                buttonClass: buttonClass,
                menuClass: menuClass,
                disabled: disabled ? ' disabled' : '',
                trigger: triggerHtml,
                items: body
            });
        if (!newElm)
            return;

        const root = newElm.element;
        const trigger = document.getElementById(id + '-trigger');
        const menu = document.getElementById(id + '-menu');
        let selectHandler = null;
        let value = null;

        function focusableItems() {
            return Array.from(menu.querySelectorAll('.kiss-menu-item:not(:disabled)'));
        }

        function open(focusFirst) {
            if (trigger.disabled)
                return newElm;
            menu.hidden = false;
            root.classList.add('is-open');
            trigger.setAttribute('aria-expanded', 'true');
            document.addEventListener('pointerdown', outsidePointer, true);
            document.addEventListener('keydown', docKeydown, true);
            if (focusFirst) {
                const first = focusableItems()[0];
                if (first)
                    first.focus();
            }
            return newElm;
        }

        function close(focusTrigger) {
            menu.hidden = true;
            root.classList.remove('is-open');
            trigger.setAttribute('aria-expanded', 'false');
            document.removeEventListener('pointerdown', outsidePointer, true);
            document.removeEventListener('keydown', docKeydown, true);
            if (focusTrigger)
                trigger.focus();
            return newElm;
        }

        function outsidePointer(e) {
            if (!root.contains(e.target))
                close(false);
        }

        function docKeydown(e) {
            if (e.key === 'Escape') {
                e.preventDefault();
                close(true);
            }
        }

        function select(item) {
            if (!item || item.disabled)
                return;
            value = item.getAttribute('data-value');
            const detail = {
                value: value,
                label: item.getAttribute('data-label') || '',
                item: item
            };
            const href = item.getAttribute('data-href');
            if (selectHandler)
                selectHandler(detail.value, detail.label, detail.item);
            root.dispatchEvent(new CustomEvent('change', {detail: detail}));
            if (href)
                window.open(href, item.getAttribute('data-target') || '_self');
            if (closeOnSelect)
                close(true);
        }

        trigger.addEventListener('click', () => {
            if (menu.hidden)
                open(false);
            else
                close(false);
        });

        trigger.addEventListener('keydown', (e) => {
            if (e.key === 'ArrowDown' || e.key === 'Enter' || e.key === ' ') {
                e.preventDefault();
                open(true);
            }
        });

        menu.addEventListener('click', (e) => {
            const item = e.target.closest('.kiss-menu-item');
            if (item && menu.contains(item))
                select(item);
        });

        menu.addEventListener('keydown', (e) => {
            const items = focusableItems();
            const idx = items.indexOf(document.activeElement);
            if (e.key === 'ArrowDown' || e.key === 'ArrowUp') {
                e.preventDefault();
                const dir = e.key === 'ArrowDown' ? 1 : -1;
                const next = items[(idx + dir + items.length) % items.length];
                if (next)
                    next.focus();
            } else if (e.key === 'Home') {
                e.preventDefault();
                if (items[0])
                    items[0].focus();
            } else if (e.key === 'End') {
                e.preventDefault();
                if (items[items.length - 1])
                    items[items.length - 1].focus();
            }
        });

        newElm.open = function () { return open(false); };
        newElm.close = function () { return close(false); };
        newElm.toggle = function () { return menu.hidden ? open(false) : close(false); };
        newElm.isOpen = function () { return !menu.hidden; };
        newElm.onselect = function (fun) { selectHandler = fun; return this; };
        newElm.onSelect = newElm.onselect;
        newElm.getValue = function () { return value; };
        newElm.setValue = function (val) { value = val; return this; };
        newElm.disable = function (flg = true) { trigger.disabled = !!flg; return this; };
        newElm.enable = function (flg = true) { trigger.disabled = !flg; return this; };
        newElm.focus = function () { trigger.focus(); return this; };

        if (startOpen)
            open(false);
    };

    Utils.newComponent({
        name: 'MenuButton',
        tag: 'menu-button',
        processor: processor
    });

})();
