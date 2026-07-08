/*
      Generic Kiss badge / chip component.
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

    const processor = function (elm, attr, content) {
        let id;
        let style = '';
        let classes = '';
        let nattrs = '';
        let tone = 'neutral';
        let size = 'md';
        let icon = '';
        let dot = false;
        let value = content ? content.trim() : '';

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
                case 'tone':
                case 'type':
                    tone = Utils.removeQuotes(attr[prop]) || tone;
                    break;
                case 'size':
                    size = Utils.removeQuotes(attr[prop]) || size;
                    break;
                case 'icon':
                    icon = Utils.removeQuotes(attr[prop]);
                    break;
                case 'dot':
                    dot = boolAttr(attr, prop, true);
                    break;
                case 'value':
                case 'text':
                    value = Utils.removeQuotes(attr[prop]);
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        const cls = 'kiss-badge kiss-badge-' + esc(tone) + ' kiss-badge-' + esc(size) +
            (dot ? ' kiss-badge-has-dot' : '') + (classes ? ' ' + classes : '');
        const newElm = Utils.replaceHTML(id, elm,
            '<span id="{id}" class="{class}" style="{style}" {attr}>{dot}{icon}<span class="kiss-badge-text">{value}</span></span>', {
                class: cls,
                style: style,
                attr: nattrs,
                dot: dot ? '<span class="kiss-badge-dot" aria-hidden="true"></span>' : '',
                icon: icon ? '<span class="kiss-badge-icon" aria-hidden="true">' + esc(icon) + '</span>' : '',
                value: esc(value)
            });
        if (!newElm)
            return;

        const el = newElm.element;
        newElm.setValue = function (val) {
            el.querySelector('.kiss-badge-text').textContent = val == null ? '' : String(val);
            return this;
        };
        newElm.getValue = function () {
            return el.querySelector('.kiss-badge-text').textContent;
        };
        newElm.setTone = function (val) {
            const tones = ['neutral', 'accent', 'info', 'success', 'warning', 'error', 'danger'];
            Array.from(el.classList).forEach((clsName) => {
                if (tones.indexOf(clsName.replace('kiss-badge-', '')) !== -1)
                    el.classList.remove(clsName);
            });
            el.classList.add('kiss-badge-' + (val || 'neutral'));
            return this;
        };
    };

    Utils.newComponent({
        name: 'Badge',
        tag: 'badge-chip',
        processor: processor
    });

})();
