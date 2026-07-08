/*
      Generic Kiss avatar component.
 */

/* global Utils */

'use strict';

(function () {

    function esc(s) {
        const str = s == null ? '' : String(s);
        return str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function initials(name) {
        const parts = String(name || '?').trim().split(/[\s._-]+/).filter(Boolean);
        if (!parts.length)
            return '?';
        return (parts.length === 1 ? parts[0].slice(0, 2) : parts[0][0] + parts[1][0]).toUpperCase();
    }

    const processor = function (elm, attr, content) {
        let id;
        let style = '';
        let classes = '';
        let nattrs = '';
        let name = content ? content.trim() : '';
        let src = '';
        let size = '';
        let tone = 'neutral';
        let status = '';
        let alt = '';

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
                case 'name':
                    name = Utils.removeQuotes(attr[prop]);
                    break;
                case 'src':
                    src = Utils.removeQuotes(attr[prop]);
                    break;
                case 'size':
                    size = Utils.removeQuotes(attr[prop]);
                    break;
                case 'tone':
                    tone = Utils.removeQuotes(attr[prop]) || tone;
                    break;
                case 'status':
                    status = Utils.removeQuotes(attr[prop]);
                    break;
                case 'alt':
                    alt = Utils.removeQuotes(attr[prop]);
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        const styleSize = size ? '--kiss-avatar-size:' + esc(size) + '; ' : '';
        const label = alt || name || 'Avatar';
        const image = src ? '<img class="kiss-avatar-img" src="' + esc(src) + '" alt="' + esc(label) + '">' :
            '<span class="kiss-avatar-initials">' + esc(initials(name)) + '</span>';
        const cls = 'kiss-avatar kiss-avatar-' + esc(tone) + (status ? ' kiss-avatar-has-status' : '') +
            (classes ? ' ' + classes : '');
        const newElm = Utils.replaceHTML(id, elm,
            '<span id="{id}" class="{class}" style="{styleSize}{style}" role="img" aria-label="{label}" {attr}>{image}{status}</span>', {
                class: cls,
                styleSize: styleSize,
                style: style,
                label: esc(label),
                attr: nattrs,
                image: image,
                status: status ? '<span class="kiss-avatar-status kiss-avatar-status-' + esc(status) + '" aria-hidden="true"></span>' : ''
            });
        if (!newElm)
            return;

        const el = newElm.element;
        function renderName(nextName) {
            el.innerHTML = '<span class="kiss-avatar-initials">' + esc(initials(nextName)) + '</span>' +
                (status ? '<span class="kiss-avatar-status kiss-avatar-status-' + esc(status) + '" aria-hidden="true"></span>' : '');
            el.setAttribute('aria-label', nextName || 'Avatar');
        }

        newElm.setName = function (val) {
            name = val == null ? '' : String(val);
            src = '';
            renderName(name);
            return this;
        };
        newElm.setSrc = function (val, nextAlt) {
            src = val || '';
            el.innerHTML = src ? '<img class="kiss-avatar-img" src="' + esc(src) + '" alt="' + esc(nextAlt || name || 'Avatar') + '">' :
                '<span class="kiss-avatar-initials">' + esc(initials(name)) + '</span>';
            if (status)
                el.insertAdjacentHTML('beforeend', '<span class="kiss-avatar-status kiss-avatar-status-' + esc(status) + '" aria-hidden="true"></span>');
            return this;
        };
        newElm.setStatus = function (val) {
            status = val || '';
            const old = el.querySelector('.kiss-avatar-status');
            if (old)
                old.remove();
            el.classList.toggle('kiss-avatar-has-status', !!status);
            if (status)
                el.insertAdjacentHTML('beforeend', '<span class="kiss-avatar-status kiss-avatar-status-' + esc(status) + '" aria-hidden="true"></span>');
            return this;
        };
    };

    Utils.newComponent({
        name: 'Avatar',
        tag: 'avatar-badge',
        processor: processor
    });

})();
