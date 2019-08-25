/*
      Author: Blake McBride
      Date:  6/18/18
 */

/* global Utils, TimeUtils, Component */

'use strict';

(function () {

    var processor = function (elm, attr, content) {
        var nstyle, originalValue;
        var required = false;
        var min = null;
        var max = null;
        var zero_fill = false;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';

        var nattrs = '';
        var id;
        for (var prop in attr) {
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

                // pre-existing attributes

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
        nattrs += ' onfocusout="this.value=Component.TimeInput.$formattime(this)"';

        var newElm = Utils.replaceHTML(id, elm, '<input type="text" style="{style}" {attr} id="{id}" placeholder="{placeholder}">', {
            style: nstyle,
            attr: nattrs,
            placeholder: content ? content.trim() : ''
        });
        var jqObj = newElm.jqObj;

        newElm.elementInfo.min = min;
        newElm.elementInfo.max = max;
        newElm.elementInfo.zero_fill = zero_fill;

        var isDigit = function (c) {
            return c >= '0'  &&  c <= '9';
        };

        newElm.getValue$ = function (sval) {
            if (!sval)
                return null;
            sval = sval.trim();
            if (!sval)
                return null;
            let buf = '';
            let i = 0;

            // hours
            for (; i < sval.length; i++) {
                let c = sval.charAt(i);
                if (!isDigit(c))
                    break;
                buf += c;
            }
            let hours;
            if (!buf)
                hours = 0;
            else
                hours = parseInt(buf);

            for (; i < sval.length; i++) {
                let c = sval.charAt(i);
                if (c !== ' '  &&  c !== ':')
                    break;
            }

            let minutes;
            if (i < sval.length  &&  isDigit(sval.charAt(i))) {
                buf = '';
                for (; i < sval.length; i++) {
                    let c = sval.charAt(i);
                    if (!isDigit(c))
                        break;
                    buf += c;
                }
                minutes = parseInt(buf);
            } else
                minutes = 0;

            for (; i < sval.length  &&  sval.charAt(i) === ' '; i++) ;

            let part;
            if (i >= sval.length)
                part = null;
            else {
                let c = sval.charAt(i);
                if (c === 'a'  ||  c === 'A')
                    part = 'A';
                else if (c === 'p'  ||  c === 'P')
                    part = 'P';
                else
                    part = null;
            }

            if (part === 'A'  &&  hours === 12)
                return (hours - 12) * 100 + minutes;
            if (!part  ||  part === 'A'  ||  hours === 12)
                return hours * 100 + minutes;
            else
                return (hours + 12) * 100 + minutes;
        };

        //--

        newElm.getValue = function () {
            var val = newElm.getValue$(newElm.jqObj.val());
            if (val === null)
                return val;
            var hours = Math.floor(val / 100);
            var minutes = val % 100;
            return hours > 23  ||  minutes > 59 ? null : val;
        };

        newElm.setValue = function (val) {
            if (val === undefined  ||  val === null  ||  val === '') {
                jqObj.val('');
                originalValue = newElm.getValue();
                return this;
            }
            jqObj.val(TimeUtils.format(val, zero_fill));
            originalValue = newElm.getValue();
            return this;
        };

        newElm.clear = function () {
            newElm.setValue('');
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getValue();
        };

        //--

        newElm.readOnly = function () {
            jqObj.attr('readonly', true);
            return this;
        };

        newElm.readWrite = function () {
            jqObj.attr('readonly', false);
            return this;
        };

        newElm.isReadOnly = function () {
            return !!jqObj.attr('readonly');
        };

        //--

        newElm.disable = function () {
            jqObj.prop('disabled', true);
            return this;
        };

        newElm.enable = function () {
            jqObj.prop('disabled', false);
            return this;
        };

        newElm.isDisabled = function () {
            return !!jqObj.attr('disabled');
        };

        //--

        newElm.hide = function () {
            jqObj.hide();
            return this;
        };

        newElm.show = function () {
            jqObj.show();
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

        newElm.isError = function (desc) {
            var val = newElm.getValue$(newElm.jqObj.val());
            if (required  &&  val === null) {
                Utils.showMessage('Error', desc + ' is required.', function () {
                    jqObj.focus();
                });
                return true;
            }
            let hours = Math.floor(val / 100);
            let minutes = val % 100;
            if (hours > 23  ||  minutes > 59) {
                Utils.showMessage('Error', desc + ' is not a valid date.', function () {
                    jqObj.focus();
                });
                return true;
            }
            if (min !== null  &&  val < min  ||  max !== null  &&  val > max) {
                var msg;
                if ((min  ||  min === 0)  &&  (max  ||  max === 0))
                    msg = desc + ' must be between ' + TimeUtils.format(min) +
                        ' and ' + TimeUtils.format(max) + '.';
                else if (min  ||  min === 0)
                    msg = desc + ' must be greater than or equal to ' + TimeUtils.format(min) + '.';
                else
                    msg = desc + ' must be less than or equal to ' + TimeUtils.format(max) + '.';
                Utils.showMessage('Error', msg, function () {
                    jqObj.focus();
                });
                return true;
            }
            return false;
        };

        Component.TimeInput.$formattime = function (elm) {
            var val = newElm.getValue$(elm.value.trim());
            if (val === null)
                return '';
            return TimeUtils.format(val, elm.kiss.elementInfo.zero_fill);
        };

    };

    var componentInfo = {
        name: 'TimeInput',
        tag: 'time-input',
        processor: processor
    };
    Utils.newComponent(componentInfo);

    Component.TimeInput.$timeinput = function (elm) {
        var val1 = elm.value.trim();
        var val2 = val1.replace(/[^0-9 :ampAMP]/g, '');  // remove characters
        if (val1 !== val2) {
            Utils.beep();
            return val2;
        }
        var nc = 0;
        for (var i = 0; i < val2.length; i++) {
            var c = val2.charAt(i);
            if (c === ':')
                if (++nc > 1) {
                    Utils.beep();
                    return val2;
                }
        }
        return val2;
    };

})();


//# sourceURL=kiss/component/timeInput/TimeInput.js
