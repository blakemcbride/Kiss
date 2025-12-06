/*
      Author: Blake McBride
      Date:  4/25/18

      I was using an editable div for a while to have finer control over html strings.
      I went back to a textarea because it was too difficult to restore the cursor position
      after the control changed through JavaScript.
      This version does not support html.
 */

/* global Utils, Component, DOMHelper */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let originalValue;
        let min = null;
        let max = null;
        let upcase = false;
        let disabled = false;
        let nstyle = "resize: none; ";
        if (attr.style)
            nstyle += attr.style;

        let nattrs = '';
        let id;
        for (let prop in attr) {
            switch (prop) {

                // new attributes
                case 'minlength':
                    min = Number(Utils.removeQuotes(attr[prop]).replace(/-/g, ""));
                    break;
                case 'upcase':
                    upcase = true;
                    break;
                case 'required':
                    if (!min)
                        min = 1;
                    break;

                // preexisting attributes

                case 'maxlength':
                    max = Number(Utils.removeQuotes(attr[prop]).replace(/-/g, ""));
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
                case 'disabled':
                    disabled = true;
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
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

        if (content)
            content = content.trim();

        const newElm = Utils.replaceHTML(id, elm, '<textarea style="{style}" {attr} id="{id}" >{content}</textarea>', {
            style: nstyle,
            attr: nattrs,
            content: content ? content.trim() : ''
        });
        if (!newElm)
            return;
        const el = newElm.element;

        function setColor(disabled) {
            if (disabled) {
                el.style.backgroundColor = '#DDDDDD';
                el.style.color = 'rgb(84, 84, 84)';
            } else {
                el.style.backgroundColor = 'white';
                el.style.color = 'black';
            }
        }

        setColor(disabled);

        el.addEventListener('keydown', (event) => {
            if (!max || event.key && event.key.length > 1 && event.key !== 'Enter')
                return;
            let txt = Utils.htmlToText(el.value).replace(/^\s+/, '');
            txt = Utils.toASCII(txt);
            if (txt && txt.length >= max) {
                event.preventDefault();
                event.stopPropagation();
            }
        });

        function keyUpHandler(event) {
            if (event.code === "Tab")
                return;
            if (Utils.isChangeChar(event) || event.key === 'Enter')
                Utils.someControlValueChanged();
        }

        el.addEventListener('keyup', keyUpHandler);

        el.addEventListener('focusout', () => {
            let sval = Utils.htmlToText(el.value).replace(/^\s+/, '');
            sval = sval ? sval.replace(/ +/g, ' ') : '';
            if (Utils.forceASCII)
                sval = Utils.toASCII(sval);
            if (sval)
                el.value = upcase ? sval.toUpperCase() : sval;
        });

        //--

        newElm.getValue = function () {
            let sval = Utils.htmlToText(el.value);
            sval = sval ? sval.replace(/ +/g, ' ').trim() : '';
            if (Utils.forceASCII)
                sval = Utils.toASCII(sval);
            if (sval && upcase) {
                sval = sval.toUpperCase();
                el.value = sval;
            }
            if (max && sval && sval.length > max)
                sval = Utils.take(sval, max);
            return sval
        };

        newElm.setValue = function (val) {
            if (val)
                val = val.trim();
            if (Utils.forceASCII)
                val = Utils.toASCII(val);
            if (!val) {
                el.value = originalValue = '';
                return this;
            }
            el.value = originalValue = val;
            return this;
        };

        newElm.setHtmlValue = function (val) {
            if (val)
                val = val.trim();
            if (!val) {
                el.value = originalValue = '';
                return this;
            }
            const tempDiv = document.createElement('div');
            tempDiv.textContent = val;
            let encodedValue = tempDiv.innerHTML;
            originalValue = val;
            el.value = encodedValue;
            return this;
        };

        newElm.clear = function () {
            newElm.setValue('');
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getValuel();
        };

        //--

        newElm.readOnly = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.readOnly = flg;
            return this;
        };

        newElm.readWrite = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.readOnly = !flg;
            return this;
        };

        newElm.isReadOnly = function () {
            return el.readOnly;
        };

        //--

        newElm.disable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.disabled = flg;
            setColor(flg);
            return this;
        };

        newElm.enable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.disabled = !flg;
            setColor(!flg);
            return this;
        };

        newElm.isDisabled = function () {
            return el.disabled;
        };

        //--

        newElm.hide = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                DOMHelper.hide(el);
            else {
                DOMHelper.show(el);
                el.style.visibility = 'visible';
            }
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg) {
                DOMHelper.show(el);
                el.style.visibility = 'visible';
            } else
                DOMHelper.hide(el);
            return this;
        };

        newElm.isHidden = function () {
            return DOMHelper.isHidden(el);
        };

        newElm.isVisible = function () {
            return !DOMHelper.isHidden(el);
        };

        //--

        newElm.focus = function () {
            el.focus();
            return this;
        };

        let keyupHandler = null;

        newElm.onCChange = function (fun) {
            if (keyupHandler) {
                el.removeEventListener('keyup', keyupHandler);
            }
            keyupHandler = function (event) {
                keyUpHandler(event);
                if (fun && (Utils.isChangeChar(event) || event.key === 'Enter'))
                    fun(newElm.getValue());
            };
            el.addEventListener('keyup', keyupHandler);
            return this;
        };

        let changeHandler = null;

        newElm.onChange = function (fun) {
            if (changeHandler) {
                el.removeEventListener('change', changeHandler);
                changeHandler = null;
            }
            if (fun) {
                changeHandler = () => {
                    fun(newElm.getValue());
                };
                el.addEventListener('change', changeHandler);
            }
            return this;
        };

        newElm.isError = function (desc) {
            if (min) {
                let val = newElm.getValue();
                if (val.length < min) {
                    let msg;
                    if (min === 1)
                        msg = desc + ' is required.';
                    else
                        msg = desc + ' must be at least ' + min + ' characters long.';
                    Utils.showMessage('Error', msg).then(function () {
                        el.focus();
                    });
                    return true;
                }
            }
            return false;
        };
    };

    const componentInfo = {
        name: 'TextboxInput',
        tag: 'textbox-input',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();


