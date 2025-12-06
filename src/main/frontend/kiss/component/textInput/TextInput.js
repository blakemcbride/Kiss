/*
      Author: Blake McBride
      Date:  4/18/18
 */

/* global Utils, Component, DOMUtils */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nstyle, originalValue;
        let min = null;
        let max = null;
        let password = false;
        let upcase = false;
        let fixcap = false;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';
        let nattrs = '';
        let id;
        let enterFunction = null;
        for (let prop in attr) {
            switch (prop) {

                // new attributes
                case 'minlength':
                    min = Number(Utils.removeQuotes(attr[prop]).replace(/-/g, ""));
                    break;
                case 'upcase':
                    upcase = true;
                    fixcap = false;
                    break;
                case 'required':
                    if (!min)
                        min = 1;
                    break;
                case 'password':
                    password = true;
                    break;
                case 'fixcap':
                    fixcap = true;
                    upcase = false;
                    break;

                // preexisting attributes
                case 'maxlength':
                    max = Number(Utils.removeQuotes(attr[prop]).replace(/-/g, ""));
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

        nattrs += ' autocorrect="off" autocapitalize="off" spellcheck="false"';
        //       nattrs += ' data-lpignore="true"';  // kill lastpass

        const newElm = Utils.replaceHTML(id, elm, '<input type="{type}" style="{style}" {attr} placeholder="{placeholder}" id="{id}">', {
            type: password ? 'password' : 'text',
            style: nstyle,
            attr: nattrs,
            placeholder: content ? content.trim() : ''
        });
        if (!newElm)
            return;
        const el = newElm.element;

        // Event handler tracking for proper removal
        let keyupHandler = null;
        let changeHandler = null;
        let inputHandler = null;

        function defaultKeyUpHandler(event) {
            if (enterFunction && event.key === 'Enter') {
                event.stopPropagation();
                enterFunction();
            }
            if (Utils.isChangeChar(event) || event.key === 'Enter')
                Utils.someControlValueChanged();
        }

        keyupHandler = defaultKeyUpHandler;
        el.addEventListener('keyup', keyupHandler);

        newElm.setPassword = function (val) {
            let prev = password;
            password = val;
            el.type = password ? 'password' : 'text';
            return prev;
        }

        //--

        newElm.getValue = function () {
            let sval = el.value;
            sval = sval ? sval.replace(/\s+/g, ' ').trim() : '';
            if (fixcap && sval)
                sval = Utils.fixCapitalization(sval);
            if (Utils.forceASCII)
                sval = Utils.toASCII(sval);
            if (max && sval && sval.length > max)
                sval = Utils.take(sval, max);
            return sval;
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
            if (upcase)
                val = val.toUpperCase();
            el.value = originalValue = val;
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getValue();
        };

        newElm.clear = function () {
            return newElm.setValue('');
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
                DOMUtils.hide(el);
            else {
                DOMUtils.show(el);
                el.style.visibility = 'visible';
            }
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg) {
                DOMUtils.show(el);
                el.style.visibility = 'visible';
            } else
                DOMUtils.hide(el);
            return this;
        };

        newElm.isHidden = function () {
            return DOMUtils.isHidden(el);
        };

        newElm.isVisible = function () {
            return !DOMUtils.isHidden(el);
        };

        //--

        newElm.onCChange = function (fun) {
            // Remove old keyup handler
            if (keyupHandler)
                el.removeEventListener('keyup', keyupHandler);

            // Create new handler
            keyupHandler = function (event) {
                defaultKeyUpHandler(event);
                if (fun && Utils.isChangeChar(event))
                    fun(newElm.getValue());
            };

            el.addEventListener('keyup', keyupHandler);
            return this;
        };

        newElm.onChange = function (fun) {
            // Remove old change handler
            if (changeHandler)
                el.removeEventListener('change', changeHandler);

            changeHandler = null;
            if (fun) {
                changeHandler = () => {
                    fun(newElm.getValue());
                };
                el.addEventListener('change', changeHandler);
            }
            return this;
        };

        newElm.focus = function () {
            el.focus();
            return this;
        };

        newElm.onEnter = function (fun) {
            enterFunction = fun;
            return this;
        }

        newElm.isError = function (desc) {
            if (min) {
                let val = newElm.getValue();
                val = val ? val.replace(/\s+/g, ' ').trim() : '';
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

        inputHandler = function () {
            let val = el.value.replace(/^\s+/, "");
            if (Utils.forceASCII)
                val = Utils.toASCII(val);
            el.value = upcase ? val.toUpperCase() : val;
        };
        el.addEventListener('input', inputHandler);
    };

    const componentInfo = {
        name: 'TextInput',
        tag: 'text-input',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();


