/*
      Author: Blake McBride
 */

/* global Utils, Component, DOMHelper */

'use strict';

(function () {
    const processor = (elm, attr, content) => {
        let nstyle;
        let nattrs = '';
        let id;
        let src = '';

        if (attr.style)
            nstyle = 'cursor: default; ' + attr.style;
        else
            nstyle = 'cursor: default;';

        for (let prop in attr) {
            switch (prop) {
                case 'style':
                    break;  // already dealing with this
                case 'id':
                    id = Utils.removeQuotes(attr[prop]);
                    break;
                case 'src':
                    src = Utils.removeQuotes(attr[prop]);
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        const newElm = Utils.replaceHTML(id, elm, `<img id="{id}" style="{style}" {attr} src="{src}">`, {
            style: nstyle,
            attr: nattrs,
            src: src
        });
        if (!newElm)
            return;
        const el = newElm.element;
        let clickHandler = null;

        newElm.setValue = function (filename, data) {
            const idx = filename.lastIndexOf(".");
            const ext = filename.substring(idx+1).toLowerCase();
            el.setAttribute('src', 'data:image/' + ext + ';base64,' + data);
        }

        newElm.clear = function () {
            el.setAttribute('src', '');
            return this;
        };

        newElm.onclick = function (fun) {
            // Remove previous handler if exists
            if (clickHandler) {
                el.removeEventListener('click', clickHandler);
                clickHandler = null;
            }
            if (fun) {
                el.style.cursor = 'pointer';
                clickHandler = fun;
                el.addEventListener('click', clickHandler);
            } else {
                el.style.cursor = 'default';
            }
            return this;
        };

        newElm.hide = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                DOMHelper.hide(el);
            else
                DOMHelper.show(el);
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                DOMHelper.show(el);
            else
                DOMHelper.hide(el);
            return this;
        };

        newElm.isHidden = function () {
            return DOMHelper.isHidden(el);
        };

        newElm.isVisible = function () {
            return DOMHelper.isVisible(el);
        };

    };

    const componentInfo = {
        name: 'Picture',
        tag: 'picture',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();
