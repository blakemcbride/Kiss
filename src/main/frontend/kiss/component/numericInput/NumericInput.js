/*
      Author: Blake McBride
      Date:  4/18/18
 */

/* global Utils, Component, Kiss */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nstyle, originalValue;
        let dollar = false;
        let show_zero = false;
        let required = false;
        let min = 0;
        let max = null;
        let enterFunction = null;
        let comma = true;
        let leftJustify = false;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';

        let dp = 0;
        let nattrs = '';
        let id;
        for (let prop in attr) {
            switch (prop) {

                // new attributes

                case 'decimal-places':
                    dp = Number(Utils.removeQuotes(attr[prop]));
                    break;
                case 'dollar-sign':
                    dollar = true;
                    break;
                case 'min':
                    min = Number(Utils.removeQuotes(attr[prop]));
                    break;
                case 'max':
                    max = Number(Utils.removeQuotes(attr[prop]));
                    break;
                case 'show-zero':
                    show_zero = true;
                    break;
                case 'required':
                    required = true;
                    break;
                case 'money':
                    min = 0;
                    dollar = true;
                    dp = 2;
                    break;
                case 'no-comma':
                    comma = false;
                    break;
                case 'left-justify':
                    leftJustify = true;
                    break;

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
        if (!leftJustify)
            nstyle += ' text-align: right;';


        nattrs += ' data-lpignore="true"';  // kill lastpass

        const newElm = Utils.replaceHTML(id, elm, '<input type="text" style="{style}" {attr} id="{id}" placeholder="{placeholder}">', {
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
        let focusoutHandler = null;

        function defaultKeyUpHandler(event) {
            if (Utils.isChangeChar(event))
                Utils.someControlValueChanged();
            if (enterFunction && event.key === 'Enter') {
                event.stopPropagation();
                enterFunction();
            }
        }

        keyupHandler = defaultKeyUpHandler;
        el.addEventListener('keyup', keyupHandler);

        //--

        newElm.getValue = function () {
            let sval = el.value;
            sval = sval.replace(/[^0-9.-]/g, '');  // remove commas and other characters
            return sval ? Number(sval) : 0;
        };

        newElm.setValue = function (val) {
            originalValue = val;
            if (!val)
                if (show_zero)
                    val = 0;
                else {
                    el.value = '';
                    return this;
                }
            if (typeof val !== 'number')
                val = Utils.toNumber(val);
            let str = Utils.format(val, (comma ? "C" : "") + (dollar ? "D" : "") + (show_zero ? "" : "B"), 0, dp);
            el.value = str;
            return this;
        };

        if (show_zero)
            newElm.setValue(0);

        newElm.clear = function () {
            newElm.setValue('');
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getValue();
        };

        newElm.onCChange = function (fun) {
            // Remove old keyup handler
            if (keyupHandler)
                el.removeEventListener('keyup', keyupHandler);

            // Create new handler
            keyupHandler = function (event) {
                if (!/^[0-9.-]/.test(event.key) && event.key !== 'Backspace' && event.key !== 'Delete')
                    return;
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

        newElm.setMinValue = function (val) {
            min = val;
            return this;
        };

        newElm.setMaxValue = function (val) {
            max = val;
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
            if (flg)
                el.removeAttribute('readonly');
            else
                el.setAttribute('readonly', 'readonly');
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
            } else
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
        };

        newElm.onEnter = function (fun) {
            enterFunction = fun;
            return this;
        }

        newElm.isError = function (desc) {
            let val = newElm.getValue();
            if (required  &&  !val) {
                Utils.showMessage('Error', desc + ' is required.').then(function () {
                    el.focus();
                });
                return true;
            }
            if (min !== null  &&  val < min  ||  max !== null  &&  val > max) {
                let msg;
                if ((min  ||  min === 0)  &&  (max  ||  max === 0))
                    msg = desc + ' must be between ' + Utils.format(min, (comma ? "C" : "") + (dollar ? "D" : ""), 0, dp) +
                        ' and ' + Utils.format(max, (comma ? "C" : "") + (dollar ? "D" : ""), 0, dp) + '.';
                else if (min  ||  min === 0)
                    msg = desc + ' must be greater than or equal to ' + Utils.format(min, (comma ? "C" : "") + (dollar ? "D" : ""), 0, dp) + '.';
                else
                    msg = desc + ' must be less than or equal to ' + Utils.format(max, (comma ? "C" : "") + (dollar ? "D" : ""), 0, dp) + '.';
                Utils.showMessage('Error', msg).then(function () {
                    el.focus();
                });
                return true;
            }
            return false;
        };

        inputHandler = function () {
            let val = el.value.trim();
            if (dollar)
                if (typeof min === 'number'  &&  min >= 0)
                    val = val.replace(/[^0-9.,$]/g, '');  // remove characters
                else
                    val = val.replace(/[^0-9.,$-]/g, '');  // remove characters
            else if (typeof min === 'number'  &&  min >= 0)
                val = val.replace(/[^0-9.,]/g, '');  // remove characters
            else
                val = val.replace(/[^0-9.,-]/g, '');  // remove characters
            let ret = '';
            let ndp = 0;  // number of decimal points
            let ndr = 0;  // number of digits to the right of the decimal point
            let andp = dp;
            if (el.value.trim() !== val)
                Utils.beep();
            for (let i = 0; i < val.length; i++) {
                let c = val.charAt(i);
                if (c === '.') {
                    if (!ndp  &&  andp !== 0) {
                        ndp++;
                        ret += c;
                    } else
                        Utils.beep();
                } else if (ndp) {
                    if (ndr < andp  ||  andp < 0) {
                        ndr++;
                        ret += c;
                    } else
                        Utils.beep();
                } else
                    ret += c;
            }
            el.value = ret;
        };
        el.addEventListener('input', inputHandler);

        focusoutHandler = function () {
            let sval = el.value;
            let ndp = dp;
            sval = sval.replace(/[^0-9.-]/g, '');  // remove commas and other characters
            if (!sval.length)
                return sval;
            let nval = Number(sval);
            el.value = Utils.format(nval, (comma ? "C" : "") + (dollar ? 'D' : '') + (show_zero ? "" : "B"), 0, ndp);
        };
        el.addEventListener('focusout', focusoutHandler);

    };

    const componentInfo = {
        name: 'NumericInput',
        tag: 'numeric-input',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();


