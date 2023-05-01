/*
      Author: Blake McBride
      Date:  6/18/18
 */

/* global Utils, TimeUtils, Component */

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
        for (let prop in attr) {
            switch (prop) {

                // new attributes

                case 'min':
                    min = Number(Utils.removeQuotes(attr[prop]));
                    break;
                case 'max':
                    max = Number(Utils.removeQuotes(attr[prop]));
                    break;
                case 'required':
                    required = true;
                    break;
                case 'zero-fill':
                    zero_fill = true;
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
            placeholder: content ? content.trim() : ''
        });
        if (!newElm)
            return;
        const jqObj = newElm.jqObj;

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

        jqObj.keyup(keyUpHandler);

        //--

        newElm.getValue = function () {
            const val = TimeUtils.strToInt(newElm.jqObj.val());
            if (val === null)
                return 0;
            const hours = Math.floor(val / 100);
            const minutes = val % 100;
            return hours > 23  ||  minutes > 59 ? null : val;
        };

        newElm.setValue = function (val) {
            if (val === undefined  ||  val === null  ||  val === '') {
                jqObj.val('');
                originalValue = newElm.getValue();
                return this;
            }
            if (typeof val === 'string')
                val = TimeUtils.strToInt(val);
            else if (typeof val === 'object')
                val = TimeUtils.dateToTime(val);
            if (val > 2400)
                val = TimeUtils.millsToInt(val);
            jqObj.val(TimeUtils.format(val, zero_fill));
            originalValue = newElm.getValue();
            return this;
        };

        jqObj.focusout(async function () {
            const val = jqObj.val().trim();
            if (!val)
                return;
            const ival = TimeUtils.strToInt(val);
            if (ival === null) {
                await Utils.showMessage('Error', 'Invalid time.');
                jqObj.focus();
            } else
                jqObj.val(TimeUtils.format(ival));
        });

        newElm.clear = function () {
            newElm.setValue('');
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getValue();
        };

        //--

        newElm.readOnly = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.attr('readonly', flg);
            return this;
        };

        newElm.readWrite = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.attr('readonly', !flg);
            return this;
        };

        newElm.isReadOnly = function () {
            return !!jqObj.attr('readonly');
        };

        //--

        newElm.disable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.prop('disabled', flg);
            return this;
        };

        newElm.enable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.prop('disabled', !flg);
            return this;
        };

        newElm.isDisabled = function () {
            return !!jqObj.attr('disabled');
        };

        //--

        newElm.hide = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                jqObj.hide();
            else
                jqObj.show();
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                jqObj.show();
            else
                jqObj.hide();
            return this;
        };

        newElm.isHidden = function () {
            return jqObj.is(':hidden');
        };

        newElm.isVisible = function () {
            return jqObj.is(':visible');
        };

        //--

        newElm.focus = function () {
            jqObj.focus();
            return this;
        };

        newElm.onEnter = function (fun) {
            enterFunction = fun;
            return this;
        }

        newElm.onChange = function (fun) {
            jqObj.off('change').change(fun);
            return this;
        };

        newElm.isError = function (desc) {
            const val = TimeUtils.strToInt(newElm.jqObj.val());
            if (required  &&  val === null) {
                Utils.showMessage('Error', desc + ' is required.').then(function () {
                    jqObj.focus();
                });
                return true;
            }
            const hours = Math.floor(val / 100);
            const minutes = val % 100;
            if (hours > 23  ||  minutes > 59) {
                Utils.showMessage('Error', desc + ' is not a valid date.').then(function () {
                    jqObj.focus();
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
                    jqObj.focus();
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


