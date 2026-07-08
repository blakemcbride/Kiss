/*
      Generic Kiss card / panel surface component.
 */

/* global Utils, DOMUtils */

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

    const processor = function (elm, attr, content) {
        let id;
        let style = '';
        let classes = '';
        let nattrs = '';
        let tag = 'section';
        let pad = true;
        let hover = false;
        let interactive = false;

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
                case 'as':
                    tag = Utils.removeQuotes(attr[prop]) || tag;
                    break;
                case 'pad':
                    pad = boolAttr(attr, prop, true);
                    break;
                case 'no-pad':
                    pad = !boolAttr(attr, prop, true);
                    break;
                case 'hover':
                    hover = boolAttr(attr, prop, true);
                    break;
                case 'interactive':
                    interactive = boolAttr(attr, prop, true);
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        if (!/^(div|section|article|aside|main|header|footer)$/.test(tag))
            tag = 'section';

        const cls = 'kiss-panel-card' + (pad ? ' kiss-panel-pad' : '') +
            (hover ? ' kiss-panel-hover' : '') + (interactive ? ' kiss-panel-interactive' : '') +
            (classes ? ' ' + classes : '');
        const role = interactive && nattrs.indexOf(' role=') === -1 ? ' role="button" tabindex="0"' : '';
        const newElm = Utils.replaceHTML(id, elm,
            '<' + tag + ' id="{id}" class="{class}" style="{style}" {attr}' + role + '>{content}</' + tag + '>', {
                class: cls,
                style: style,
                attr: nattrs,
                content: content || ''
            });
        if (!newElm)
            return;

        const el = newElm.element;
        let clickHandler = null;
        newElm.onclick = function (fun) {
            if (clickHandler)
                el.removeEventListener('click', clickHandler);
            clickHandler = fun || null;
            if (clickHandler)
                el.addEventListener('click', clickHandler);
            return this;
        };
        newElm.hide = function (flg = true) { flg ? DOMUtils.hide(el) : DOMUtils.show(el); return this; };
        newElm.show = function (flg = true) { flg ? DOMUtils.show(el) : DOMUtils.hide(el); return this; };
        newElm.isHidden = function () { return DOMUtils.isHidden(el); };
        newElm.isVisible = function () { return !DOMUtils.isHidden(el); };
    };

    Utils.newComponent({
        name: 'PanelCard',
        tag: 'panel-card',
        processor: processor
    });

})();
