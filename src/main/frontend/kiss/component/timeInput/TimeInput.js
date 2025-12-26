/*
      Author: Blake McBride
      Date:  6/18/18
*/

/* global Utils, TimeUtils, Component, DOMUtils */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nstyle, originalValue;
        let required = false;
        let min = null;
        let max = null;
        let zero_fill = false;
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
                    min = TimeUtils.strToInt(Utils.removeQuotes(attr[prop]));
                    break;
                case 'max':
                    max = TimeUtils.strToInt(Utils.removeQuotes(attr[prop]));
                    break;
                case 'required':
                    required = true;
                    break;
                case 'zero-fill':
                    zero_fill = true;
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

        nattrs += ' oninput="this.value=Component.TimeInput.$timeinput(this)"';
        nattrs += ' data-lpignore="true"';  // kill lastpass

        const newElm = Utils.replaceHTML(id, elm, '<input type="text" style="{style}" {attr} id="{id}" placeholder="{placeholder}">', {
            style: nstyle,
            attr: nattrs,
            placeholder: content ? content.trim() : (placeholder ? 'hh:mm' : '')
        });
        if (!newElm)
            return;
        const el = newElm.element;

        newElm.elementInfo.min = min;
        newElm.elementInfo.max = max;
        newElm.elementInfo.zero_fill = zero_fill;

        function keyUpHandler(event) {
            if (enterFunction && event.key === 'Enter') {
                event.stopPropagation();
                enterFunction();
            }
            if (Utils.isChangeChar(event))
                Utils.someControlValueChanged();
        }

        DOMUtils.on(el, 'keyup', keyUpHandler);

        //--

        newElm.getValue = function () {
            const val = TimeUtils.strToInt(el.value);
            if (val === null)
                return -1;
            const hours = Math.floor(val / 100);
            const minutes = val % 100;
            return hours > 23  ||  minutes > 59 ? null : val;
        };

        newElm.setValue = function (val, timezone) {
            if (val === undefined  ||  val === null  ||  val === '') {
                el.value = '';
                originalValue = -1;
                return this;
            }
            if (typeof val === 'string')
                val = TimeUtils.strToInt(val);
            else if (val instanceof Date)
                val = TimeUtils.toInt(val);
            else if (typeof val === 'number') {
                if (val > 250000000000) {
                    // epoch
                    if (timezone)
                        val = DateTimeUtils.epochToDisplayEpoch(val, timezone);
                    val = TimeUtils.toInt(val);
                } else if (val > 19000101) {
                    // YYYYMMDDHHMM
                    val = DateTimeUtils.getTime(val);
                }
            }
            // val = HHMM
            el.value = TimeUtils.format(val, zero_fill);
            originalValue = newElm.getValue();
            return this;
        };

        el.addEventListener('blur', async function () {
            const val = el.value.trim();
            if (!val)
                return;
            const ival = TimeUtils.strToInt(val);
            if (ival === null) {
                await Utils.showMessage('Error', 'Invalid time.');
                el.focus();
            } else
                el.value = TimeUtils.format(ival);
        });

        /**
         * Sets the control value to an empty string.
         * @return {Component.TimeInput} This object.
         */
        newElm.clear = function () {
            newElm.setValue('');
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getValue();
        };

        newElm.setMinValue = function (val) {
            min = TimeUtils.strToInt(val);
            return this;
        };

        newElm.setMaxValue = function (val) {
            max = TimeUtils.strToInt(val);
            return this;
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

        //--

        newElm.focus = function () {
            el.focus();
            return this;
        };

        newElm.onEnter = function (fun) {
            enterFunction = fun;
            return this;
        }

        let changeKeyUpHandler = null;

        newElm.onCChange = function (fun) {
            if (changeKeyUpHandler) {
                DOMUtils.off(el, 'keyup', changeKeyUpHandler);
                changeKeyUpHandler = null;
            }
            changeKeyUpHandler = function (event) {
                if (!/^[0-9:aApPmM]/.test(event.key) && event.key !== 'Backspace' && event.key !== 'Delete')
                    return;
                keyUpHandler(event);
                if (fun && Utils.isChangeChar(event))
                    fun(newElm.getValue());
            };
            DOMUtils.off(el, 'keyup', keyUpHandler);
            DOMUtils.on(el, 'keyup', changeKeyUpHandler);
            return this;
        };

        let changeHandler = null;

        newElm.onChange = function (fun) {
            if (changeHandler) {
                DOMUtils.off(el, 'change', changeHandler);
                changeHandler = null;
            }
            if (fun) {
                changeHandler = function () {
                    fun(newElm.getValue());
                };
                DOMUtils.on(el, 'change', changeHandler);
            }
            return this;
        };

        newElm.isError = function (desc) {
            const val = TimeUtils.strToInt(el.value);
            if (required  &&  val === null) {
                Utils.showMessage('Error', desc + ' is required.').then(function () {
                    el.focus();
                });
                return true;
            }
            const hours = Math.floor(val / 100);
            const minutes = val % 100;
            if (hours > 23  ||  minutes > 59) {
                Utils.showMessage('Error', desc + ' is not a valid date.').then(function () {
                    el.focus();
                });
                return true;
            }
            if (min !== null  &&  val < min  ||  max !== null  &&  val > max) {
                let msg;
                if ((min  ||  min === 0)  &&  (max  ||  max === 0))
                    msg = desc + ' must be between ' + TimeUtils.format(min) +
                        ' and ' + TimeUtils.format(max) + '.';
                else if (min  ||  min === 0)
                    msg = desc + ' must be greater than or equal to ' + TimeUtils.format(min) + '.';
                else
                    msg = desc + ' must be less than or equal to ' + TimeUtils.format(max) + '.';
                Utils.showMessage('Error', msg).then(function () {
                    el.focus();
                });
                return true;
            }
            return false;
        };

    };

    const componentInfo = {
        name: 'TimeInput',
        tag: 'time-input',
        processor: processor
    };
    Utils.newComponent(componentInfo);

    Component.TimeInput.$timeinput = function (elm) {
        const val1 = elm.value.replace(/^\s+/, '');
        const val2 = val1.replace(/[^0-9 :ampAMP]/g, '');  // remove characters
        if (val1 !== val2) {
            Utils.beep();
            return val2;
        }
        let nc = 0;
        for (let i = 0; i < val2.length; i++) {
            let c = val2.charAt(i);
            if (c === ':')
                if (++nc > 1) {
                    Utils.beep();
                    return val2;
                }
        }
        return val2;
    };

})();


