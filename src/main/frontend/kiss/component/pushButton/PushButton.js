/*
      Author: Blake McBride
      Date:  4/25/18
*/

/* global Utils, Kiss */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nstyle;
        let waitForKeyUp = false;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';

        let nattrs = '';
        let id;
        for (let prop in attr) {
            switch (prop) {

                // new attributes


                // preexisting attributes

                case 'style':
                    break;  // already dealing with this
                case 'id':
                    id = Utils.removeQuotes(attr[prop]);
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        const newElm = Utils.replaceHTML(id, elm, '<input type="button" style="{style}" {attr} value="{value}" id="{id}">', {
            style: nstyle,
            attr: nattrs,
            value: content ? content.trim() : ''
        });
        if (!newElm)
            return;
        const el = newElm.element;

        // Track event handlers for removal
        let clickHandler = null;
        let keyupHandler = null;
        let keydownHandler = null;

        const changeHandler = function () {
            //           Utils.someControlValueChanged();
        };
        el.addEventListener('change', changeHandler);

        newElm.onclick = function (fun) {
            // the off() is used to assure that multiple calls to this method doesn't cause the function to execute multiple times
            // but it also limits to a single callback function
            if (clickHandler) {
                el.removeEventListener('click', clickHandler);
                clickHandler = null;
            }
            if (keyupHandler) {
                el.removeEventListener('keyup', keyupHandler);
                keyupHandler = null;
            }
            if (keydownHandler) {
                el.removeEventListener('keydown', keydownHandler);
                keydownHandler = null;
            }
            if (fun) {
                clickHandler = function (e) {
                    if (Utils.suspendDepth < 0)  // should never happen but just in case
                        Utils.suspendDepth = 0;
                    if (!waitForKeyUp && !Utils.suspendDepth)
                        fun();
                };
                keyupHandler = function (e) {
                    if (Utils.suspendDepth < 0)  // should never happen but just in case
                        Utils.suspendDepth = 0;
                    e.stopPropagation();
                    if (waitForKeyUp && e.key === 'Enter' && !Utils.suspendDepth) {
                        fun();
                        waitForKeyUp = false;
                    }
                };
                keydownHandler = function (e) {
                    if (e.key === 'Enter')
                        waitForKeyUp = true;
                    e.stopPropagation();
                };
                el.addEventListener('click', clickHandler);
                el.addEventListener('keyup', keyupHandler);
                el.addEventListener('keydown', keydownHandler);
            }
            return this;
        };

        newElm.click = function () {
            el.click();
            return this;
        };

        newElm.getValue = function () {
            return el.value;
        };

        newElm.setValue = function (val) {
            el.value = val;
            return this;
        };

        //--

        newElm.readOnly = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                el.setAttribute('readonly', 'readonly');
            else
                el.removeAttribute('readonly');
            return this;
        };

        newElm.readWrite = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (!flg)
                el.setAttribute('readonly', 'readonly');
            else
                el.removeAttribute('readonly');
            return this;
        };

        newElm.isReadOnly = function () {
            return el.hasAttribute('readonly');
        };

        //--

        newElm.disable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.disabled = flg;
            return this;
        };

        newElm.enable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.disabled = !flg;
            return this;
        };

        newElm.isDisabled = function () {
            return el.disabled;
        };

        //--

        newElm.hide = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                Kiss.hide(el);
            else {
                Kiss.show(el);
                el.style.visibility = 'visible';
            }
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg) {
                Kiss.show(el);
                el.style.visibility = 'visible';
            }
            else
                Kiss.hide(el);
            return this;
        };

        newElm.isHidden = function () {
            return Kiss.isHidden(el);
        };

        newElm.isVisible = function () {
            return !Kiss.isHidden(el);
        };

        //--

        newElm.focus = function () {
            el.focus();
            return this;
        }
    };

    const componentInfo = {
        name: 'PushButton',
        tag: 'push-button',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();

