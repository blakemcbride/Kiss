/*
      Author: Blake McBride
      Date:  4/22/18
*/

/* global Utils, DateUtils, DOMHelper */

'use strict';


(function () {

    const processor = function (elm, attr, content) {
        let originalValue = 0;
        let nstyle;
        let min = null;
        let max = null;
        let required = false;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';
        let enterFunction = null;
        let nattrs = '';
        let id;
        let placeholder = true;

        for (let prop in attr) {
            switch (prop) {

                // new attributes

                case 'min':
                    min = DateUtils.strToInt(Utils.removeQuotes(attr[prop]));
                    break;
                case 'max':
                    max = DateUtils.strToInt(Utils.removeQuotes(attr[prop]));
                    break;
                case 'required':
                    required = true;
                    break;
                case 'no-placeholder':
                    placeholder = false;
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

        nattrs += ' autocorrect="off" autocapitalize="off" spellcheck="false"';
        nattrs += ' data-lpignore="true"';  // kill lastpass

        const newElm = Utils.replaceHTML(id, elm, '<input type="text" style="{style}" {attr} placeholder="{placeholder}" id="{id}">', {
            style: nstyle,
            attr: nattrs,
            placeholder: content ? content.trim() : (placeholder ? DateUtils.detectDateFormat().toLowerCase() : '')
        });
        if (!newElm)
            return;
        const el = newElm.element;

        // Track event handlers for removal
        let inputHandler = null;
        let keyupHandler = null;
        let focusoutHandler = null;
        let changeHandler = null;

        inputHandler = function () {
            let val = el.value.trim();
            val = val.replace(/[^0-9./-]/g, '');  // remove characters
            el.value = val;
        };
        el.addEventListener('input', inputHandler);

        function baseKeyUpHandler(event) {
            if (enterFunction && event.key === 'Enter') {
                event.stopPropagation();
                enterFunction();
            }
            if (Utils.isChangeChar(event))
                Utils.someControlValueChanged();
        }

        keyupHandler = baseKeyUpHandler;
        el.addEventListener('keyup', keyupHandler);

        focusoutHandler = async function () {
            const val = el.value;
            if (!val)
                return;
            if (!DateUtils.isValid(val)) {
                await Utils.showMessage('Error', 'Invalid date.');
                el.focus();
            }
            else
                el.value = DateUtils.intToStr4(DateUtils.strToInt(val)).trim();
        };
        el.addEventListener('focusout', focusoutHandler);

        //--

        newElm.getValue = function () {
            const val = el.value;
            if (!val)
                return 0;
            return DateUtils.strToInt(val);
        };

        newElm.getIntValue = newElm.getValue; // for historic reasons

        newElm.getSQLValue = function () {
            return DateUtils.intToSQL(newElm.getIntValue());
        };

        newElm.getDateValue = function () {
            return DateUtils.intToDate(newElm.getIntValue());
        };

        newElm.setValue = function (val, timezone) {
            if (!val) {
                el.value = '';
                originalValue = 0;
            }
            else if (typeof val === 'number') {
                if (val > 30000000) {
                    if (timezone)
                        val = DateTimeUtils.epochToDisplayEpoch(val, timezone);
                    val = DateUtils.toInt(val);
                }
                el.value = DateUtils.intToStr4(val).trim();
                originalValue = val;
            }
            else if (typeof val === 'string') {
                if (/^\d+$/.test(val)) {
                    el.value = DateUtils.intToStr4(val=Number(val)).trim();
                    originalValue = val;
                }
                else {
                    originalValue = DateUtils.strToInt(val);
                    el.value = DateUtils.intToStr4(originalValue).trim();
                }
            }
            else if (val instanceof Date) { // Date
                originalValue = DateUtils.toInt(val);
                el.value = DateUtils.intToStr4(originalValue).trim();
            }
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getIntValue();
        };

        newElm.clear = function () {
            newElm.setValue(0);
            return this;
        };

        newElm.setMinValue = function (val) {
            min = DateUtils.toInt(val);
            return this;
        };

        newElm.setMaxValue = function (val) {
            max = DateUtils.toInt(val);
            return this;
        };

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
            }
            else
                DOMHelper.hide(el);
            return this;
        };

        newElm.isHidden = function () {
            return DOMHelper.isHidden(el);
        };

        newElm.isVisible = function () {
            return !DOMHelper.isHidden(el);
        };

        newElm.focus = function () {
            el.focus();
            return this;
        };

        newElm.onCChange = function (fun) {
            if (keyupHandler) {
                el.removeEventListener('keyup', keyupHandler);
            }
            keyupHandler = function (event) {
                baseKeyUpHandler(event);
                if (fun && (Utils.isChangeChar(event) || event.key === 'Enter'))
                    fun(newElm.getIntValue());
            };
            el.addEventListener('keyup', keyupHandler);
            return this;
        };

        newElm.onChange = function (fun) {
            if (changeHandler) {
                el.removeEventListener('change', changeHandler);
                changeHandler = null;
            }
            if (fun) {
                changeHandler = () => {
                    fun(newElm.getIntValue());
                };
                el.addEventListener('change', changeHandler);
            }
            return this;
        };

        newElm.onEnter = function (fun) {
            enterFunction = fun;
            return this;
        };

        newElm.isError = function (desc) {
            let val = newElm.getIntValue();
            if (required  &&  !val) {
                Utils.showMessage('Error', desc + ' is required.').then(function () {
                    el.focus();
                });
                return true;
            }
            if (val  &&  (min !== null  &&  val < min  ||  max !== null  &&  val > max)) {
                let msg;
                if ((min  ||  min === 0)  &&  (max  ||  max === 0))
                    msg = desc + ' must be between ' + DateUtils.intToStr4(min) + ' and ' + DateUtils.intToStr4(max) + '.';
                else if (min  &&  min !== 0)
                    msg = desc + ' must be greater than or equal to ' + DateUtils.intToStr4(min) + '.';
                else
                    msg = desc + ' must be less than or equal to ' + DateUtils.intToStr4(max) + '.';
                Utils.showMessage('Error', msg).then(function () {
                    el.focus();
                });
                return true;
            }
            return false;
        };
    };

    const componentInfo = {
        name: 'DateInput',
        tag: 'date-input',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();


