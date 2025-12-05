/*
      Author: Blake McBride
 */

/* global Utils, Component, Kiss */

'use strict';

(function () {
    const processor = (elm, attr, content) => {
        let nstyle;
        let hasFor = false;

        if (attr.style)
            nstyle = 'cursor: default; ' + attr.style;
        else
            nstyle = 'cursor: default;';

        let nattrs = '';
        let id;

        for (let prop in attr) {
            switch (prop) {
                case 'style':
                    break;  // already dealing with this
                case 'id':
                    id = Utils.removeQuotes(attr[prop]);
                    break;
                case 'for':
                    hasFor = true;
                // no break
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        let newElm;
        if (hasFor)
            newElm = Utils.replaceHTML(id, elm, `<label style="{style}" {attr} id="{id}">${content ? content.trim() : ''}</label>`, {
                style: nstyle,
                attr: nattrs
            });
        else
            newElm = Utils.replaceHTML(id, elm, `<span style="{style}" {attr} id="{id}">${content ? content.trim() : ''}</span>`, {
                style: nstyle,
                attr: nattrs
            });
        if (!newElm)
            return;
        const el = newElm.element;
        let clickHandler = null;

        newElm.getValue = function () {
            let sval = el.textContent;
            return sval ? sval : '';
        };

        newElm.setValue = function (val) {
            if (val !== 0  &&  !val) {
                el.textContent = '';
                return this;
            }
            el.textContent = val;
            return this;
        };

        newElm.setHTMLValue = function (val) {
            if (val !== 0  &&  !val) {
                el.textContent = '';
                return this;
            }
            el.innerHTML = val;
            return this;
        };

        newElm.clear = function () {
            el.textContent = '';
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
                Kiss.hide(el);
            else
                Kiss.show(el);
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                Kiss.show(el);
            else
                Kiss.hide(el);
            return this;
        };

        newElm.isHidden = function () {
            return Kiss.isHidden(el);
        };

        newElm.isVisible = function () {
            return Kiss.isVisible(el);
        };

        newElm.setColor = function (color) {
            el.style.color = color;
            return this;
        };
    };

    const componentInfo = {
        name: 'TextLabel',
        tag: 'text-label',
        processor: processor
    };
    Utils.newComponent(componentInfo);

    Component.TextLabel.$textlabel = function (elm) {
        return elm.value.replace(/^\s+/, "");
    };
})();
