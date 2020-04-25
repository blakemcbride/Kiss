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

        nattrs += ' data-lpignore="true"';  // kill lastpass

        const newElm = Utils.replaceHTML(id, elm, '<input type="text" style="{style}" {attr} id="{id}" placeholder="{placeholder}">', {
            style: nstyle,
            attr: nattrs,
            placeholder: content ? content.trim() : ''
        });
        const jqObj = newElm.jqObj;

        jqObj.keydown(function () {
            Utils.someControlValueChanged();
        });

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
            if (typeof val === 'string')
                val = Number(val);
            let str = Utils.format(val, "C" + (dollar ? "D" : "") + (show_zero ? "" : "B"), 0, dp);
            jqObj.val(str);
            return this;
        };

        newElm.clear = function () {
            newElm.setValue('');
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getValue();
        };

        newElm.onChange = function (fun) {
            onchange = fun;
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
            let val = newElm.getValue();
            if (required  &&  !val) {
                Utils.showMessage('Error', desc + ' is required.', function () {
                    jqObj.focus();
                });
                return true;
            }
            if (min !== null  &&  val < min  ||  max !== null  &&  val > max) {
                let msg;
                if ((min  ||  min === 0)  &&  (max  ||  max === 0))
                    msg = desc + ' must be between ' + Utils.format(min, "C" + (dollar ? "D" : ""), 0, dp) +
                        ' and ' + Utils.format(max, "C" + (dollar ? "D" : ""), 0, dp) + '.';
                else if (min  ||  min === 0)
                    msg = desc + ' must be greater than or equal to ' + Utils.format(min, "C" + (dollar ? "D" : ""), 0, dp) + '.';
                else
                    msg = desc + ' must be less than or equal to ' + Utils.format(max, "C" + (dollar ? "D" : ""), 0, dp) + '.';
                Utils.showMessage('Error', msg, function () {
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
            jqObj.val(Utils.format(nval, "C" + (dollar ? 'D' : '') + (show_zero ? "" : "B"), 0, ndp));
        });

    };

    const componentInfo = {
        name: 'NumericInput',
        tag: 'numeric-input',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();


