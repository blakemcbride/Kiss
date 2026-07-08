/*
      Generic Kiss section title component.
 */

/* global Utils */

'use strict';

(function () {

    function esc(s) {
        const str = s == null ? '' : String(s);
        return str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    const processor = function (elm, attr, content) {
        let id;
        let style = '';
        let classes = '';
        let nattrs = '';
        let title = '';
        let subtitle = '';
        let eyebrow = '';
        let level = '2';

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
                case 'title':
                    title = Utils.removeQuotes(attr[prop]);
                    break;
                case 'subtitle':
                case 'note':
                    subtitle = Utils.removeQuotes(attr[prop]);
                    break;
                case 'eyebrow':
                    eyebrow = Utils.removeQuotes(attr[prop]);
                    break;
                case 'level':
                    level = Utils.removeQuotes(attr[prop]) || level;
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        if (!/^[1-6]$/.test(level))
            level = '2';
        if (!title && !subtitle && !eyebrow && content)
            title = content.trim();

        const cls = 'kiss-section-title' + (classes ? ' ' + classes : '');
        const newElm = Utils.replaceHTML(id, elm,
            '<div id="{id}" class="{class}" style="{style}" {attr}>' +
                '<div class="kiss-section-title-copy">' +
                    '{eyebrow}<h{level} class="kiss-section-heading">{title}</h{level}>{subtitle}' +
                '</div>' +
                '<div class="kiss-section-actions">{actions}</div>' +
            '</div>', {
                class: cls,
                style: style,
                attr: nattrs,
                level: level,
                eyebrow: eyebrow ? '<div class="kiss-section-eyebrow">' + esc(eyebrow) + '</div>' : '',
                title: esc(title),
                subtitle: subtitle ? '<div class="kiss-section-subtitle">' + esc(subtitle) + '</div>' : '',
                actions: title || subtitle || eyebrow ? (content || '') : ''
            });
        if (!newElm)
            return;

        const root = newElm.element;
        newElm.setTitle = function (val) {
            const h = root.querySelector('.kiss-section-heading');
            if (h)
                h.textContent = val == null ? '' : String(val);
            return this;
        };
        newElm.setSubtitle = function (val) {
            let sub = root.querySelector('.kiss-section-subtitle');
            if (!sub) {
                sub = document.createElement('div');
                sub.className = 'kiss-section-subtitle';
                root.querySelector('.kiss-section-title-copy').appendChild(sub);
            }
            sub.textContent = val == null ? '' : String(val);
            return this;
        };
    };

    Utils.newComponent({
        name: 'SectionTitle',
        tag: 'section-title',
        processor: processor
    });

})();
