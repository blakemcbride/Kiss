/*
      Author: Blake McBride
      Date:  4/18/18
 */

/* global Utils, Component */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nstyle, originalValue;
        let dollar = false;
        let show_zero = false;
        let required = false;
        let min = 0;
        let max = null;
        let onchange;
        let enterFunction = null;
        let comma = true;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';
        nstyle += ' text-align: right;';

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

        nattrs += ' data-lpignore="true"';  // kill lastpass

        const newElm = Utils.replaceHTML(id, elm, '<input type="text" style="{style}" {attr} id="{id}" placeholder="{placeholder}">', {
            style: nstyle,
            attr: nattrs,
            placeholder: content ? content.trim() : ''
        });
        if (!newElm)
            return;
        const jqObj = newElm.jqObj;

        function keyUpHandler(event) {
            if (Utils.isChangeChar(event))
                Utils.someControlValueChanged();
            if (enterFunction && event.key === 'Enter') {
                event.stopPropagation();
                enterFunction();
            }
        }

        jqObj.keyup(keyUpHandler);

        //--

        newElm.getValue = function () {
            let sval = jqObj.val();
            sval = sval.replace(/[^0-9.-]/g, '');  // remove commas and other characters
            return sval ? Number(sval) : 0;
        };

        newElm.setValue = function (val) {
            originalValue = val;
            if (!val)
                if (show_zero)
                    val = 0;
                else {
                    jqObj.val('');
                    return this;
                }
            if (typeof val !== 'number')
                val = Utils.toNumber(val);
            let str = Utils.format(val, (comma ? "C" : "") + (dollar ? "D" : "") + (show_zero ? "" : "B"), 0, dp);
            jqObj.val(str);
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

        newElm.onKeyUp = function (fun) {
            jqObj.off('keyup').keyup(function (event) {
                keyUpHandler(event);
                if (fun)
                    fun(event);
            });
            return this;
        };

        newElm.onChange = function (fun) {
            onchange = fun;
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

        newElm.isError = function (desc) {
            let val = newElm.getValue();
            if (required  &&  !val) {
                Utils.showMessage('Error', desc + ' is required.').then(function () {
                    jqObj.focus();
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
                    jqObj.focus();
                });
                return true;
            }
            return false;
        };

        jqObj.on('input', function () {
            let val = jqObj.val().trim();
            if (dollar)
                if ($.isNumeric(min)  &&  min >= 0)
                    val = val.replace(/[^0-9.,$]/g, '');  // remove characters
                else
                    val = val.replace(/[^0-9.,$-]/g, '');  // remove characters
            else if ($.isNumeric(min)  &&  min >= 0)
                val = val.replace(/[^0-9.,]/g, '');  // remove characters
            else
                val = val.replace(/[^0-9.,-]/g, '');  // remove characters
            let ret = '';
            let ndp = 0;  // number of decimal points
            let ndr = 0;  // number of digits to the right of the decimal point
            let andp = dp;
            if (jqObj.val().trim() !== val)
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
            jqObj.val(ret);
        });

        jqObj.on('focusout', function () {
            if (onchange)
                onchange();
            let sval = jqObj.val();
            let ndp = dp;
            sval = sval.replace(/[^0-9.-]/g, '');  // remove commas and other characters
            if (!sval.length)
                return sval;
            let nval = Number(sval);
            jqObj.val(Utils.format(nval, (comma ? "C" : "") + (dollar ? 'D' : '') + (show_zero ? "" : "B"), 0, ndp));
        });

    };

    const componentInfo = {
        name: 'NumericInput',
        tag: 'numeric-input',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();


