/*
      Author: Blake McBride
      Date:  4/22/18
 */

/* global Utils, DateUtils, DOMUtils */

'use strict';


(function () {

    const processor = function (elm, attr, content) {
        let originalValue;
        let nstyle;
        let min = null;
        let max = null;
        let required = false;
        let changeFun;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';
        let enterFunction = null;
        let nattrs = '';
        let id;
        let typing = false;
        for (let prop in attr) {
            switch (prop) {

                // new attributes

                case 'min':
                    min = DateUtils.toInt(Utils.removeQuotes(attr[prop]));
                    break;
                case 'max':
                    max = DateUtils.toInt(Utils.removeQuotes(attr[prop]));
                    break;
                case 'required':
                    required = true;
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

        const newElm = Utils.replaceHTML(id, elm, '<input type="date" style="{style}" {attr} placeholder="{placeholder}" id="{id}">', {
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
        let blurHandler = null;
        let keydownHandler = null;

        function defaultKeyUpHandler(event) {
            if (enterFunction && event.key === 'Enter') {
                event.stopPropagation();
                enterFunction();
            }
            if (Utils.isChangeChar(event))
                Utils.someControlValueChanged();
        }

        keyupHandler = defaultKeyUpHandler;
        el.addEventListener('keyup', keyupHandler);

        changeHandler = function () {
            Utils.someControlValueChanged();
        };
        el.addEventListener('change', changeHandler);

        //--

        newElm.getIntValue = function () {
            return DateUtils.SQLtoInt(el.value);
        };

        newElm.getSQLValue = function () {
            return el.value;
        };

        newElm.getDateValue = function () {
            return DateUtils.intToDate(DateUtils.SQLtoInt(el.value));
        };

        newElm.setValue = function (val) {
            if (!val)
                el.value = '';
            else if (typeof val === 'number') {
                if (val > 30000000)
                    val = DateUtils.toInt(val);
                el.value = DateUtils.intToSQL(val);
            } else if (typeof val === 'string') {
                if (/^\d+$/.test(val))
                    el.value = DateUtils.intToSQL(Number(val));
                else
                    el.value = DateUtils.intToSQL(DateUtils.strToInt(val));
            } else if (typeof val === 'object')  // Date
                el.value = DateUtils.intToSQL(DateUtils.dateToInt(val));
            originalValue = newElm.getIntValue();
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getIntValue();
        };

        newElm.clear = function () {
            el.value = '';
            originalValue = newElm.getIntValue();
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
                DOMUtils.hide(el);
            else
                DOMUtils.show(el);
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                DOMUtils.show(el);
            else
                DOMUtils.hide(el);
            return this;
        };

        newElm.isHidden = function () {
            return DOMUtils.isHidden(el);
        };

        newElm.isVisible = function () {
            return !DOMUtils.isHidden(el);
        };

        newElm.focus = function () {
            el.focus();
            return this;
        };

        newElm.onCChange = function (fun) {
            // Remove old change handler
            if (changeHandler)
                el.removeEventListener('change', changeHandler);

            changeHandler = null;
            if (fun) {
                changeHandler = () => {
                    fun(newElm.getIntValue());
                };
                el.addEventListener('change', changeHandler);
            }
            return this;
        };

        newElm.onChange = function (fun) {
            /* The problem here is that the user can either type a date or select a date on a calendar popup.
               We need to know the difference so that if they click on the calendar popup we can execute the
               change function without requiring them to lose focus.  However, if they are typing we require them
               to lose focus so we know they're done typing.

               To complicate matters.  Chrome and Firefox act differently.  When you are typing and hit a tab, Chrome
               takes you completely out of the control while Firefox takes you to the calendar icon.
             */
            // Remove old handlers
            if (blurHandler)
                el.removeEventListener('blur', blurHandler);
            if (changeHandler)
                el.removeEventListener('change', changeHandler);
            if (keydownHandler)
                el.removeEventListener('keydown', keydownHandler);

            blurHandler = null;
            changeHandler = null;
            keydownHandler = null;

            if (fun) {
                keydownHandler = (event) => {
                    typing = true;
                };
                el.addEventListener('keydown', keydownHandler);

                changeHandler = (event) => {
                    if (!typing)
                        fun(newElm.getIntValue());
                };
                el.addEventListener('change', changeHandler);

                blurHandler = (event) => {
                    if (typing)
                        fun(newElm.getIntValue());
                    typing = false;
                };
                el.addEventListener('blur', blurHandler);
            }
            return this;
        };

        newElm.onEnter = function (fun) {
            enterFunction = fun;
            return this;
        }

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
        name: 'NativeDateInput',
        tag: 'native-date-input',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();


