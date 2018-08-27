/*
      Author: Blake McBride
      Date:  4/18/18
 */

'use strict';

(function () {

    var processor = function (elm, attr, content) {
        var nstyle;
        var dollar = false;
        var show_zero = false;
        var required = false;
        var min = 0;
        var max = null;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';
        nstyle += ' text-align: right;';

        var dp = '0';
        var nattrs = '';
        var id;
        for (var prop in attr) {
            switch (prop) {

                // new attributes

                case 'decimal-places':
                    dp = utils.removeQuotes(attr[prop]);
                    break;
                case 'dollar-sign':
                    dollar = true;
                    break;
                case 'min':
                    min = Number(utils.removeQuotes(attr[prop]));
                    break;
                case 'max':
                    max = Number(utils.removeQuotes(attr[prop]));
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
                    dp = '2';
                    break;
                    
                // pre-existing attributes

                case 'style':
                    break;  // already dealing with this
                case 'id':
                    id = utils.removeQuotes(attr[prop]);
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        nattrs += ' oninput="this.value=Component.NumericInput.$numberinput(this)"';
        nattrs += ' onfocusout="this.value=Component.NumericInput.$formatnumber(this)"';

        var newElm = utils.replaceHTML(id, elm, '<input type="text" style="{style}" {attr} id="{id}" placeholder="{placeholder}">', {
            style: nstyle,
            attr: nattrs,
            placeholder: content
        });
        var jqObj = newElm.jqObj;

        newElm.elementInfo.dp = dp = Number(dp);
        newElm.elementInfo.dollarSign = dollar;
        newElm.elementInfo.blankIfZero = show_zero;
        newElm.elementInfo.min = min;
        newElm.elementInfo.max = max;

        newElm.getValue = function () {
            var sval = jqObj.val();
            sval = sval.replace(/[^0-9\.-]/g,'');  // remove commas and other characters
            return sval ? Number(sval) : 0;
        };

        newElm.setValue = function (val) {
            if (!val)
                if (show_zero)
                    val = 0;
                else {
                    jqObj.val('');
                    return this;
                }
            if (typeof val === 'string')
                val = Number(val);
            var str = utils.format(val, "C"+(dollar?"D":"")+(show_zero?"":"B"), 0, dp);
            jqObj.val(str);
            return this;
        };

        newElm.clear = function () {
            jqObj.val('');
            return this;
        };

        newElm.disable = function () {
            jqObj.prop('disabled', true);
            return this;
        };

        newElm.enable = function () {
            jqObj.prop('disabled', false);
            return this;
        };

        newElm.hide = function () {
            jqObj.hide();
            return this;
        };

        newElm.show = function () {
            jqObj.show();
            return this;
        };

        newElm.focus = function () {
            jqObj.focus();
            return this;
        };

        newElm.isError = function (desc) {
            var val = newElm.getValue();
            if (required && !val) {
                utils.showMessage('Error', desc + ' is required.', function () {
                    jqObj.focus();
                });
                return true;
            }
            if (min !== null  &&  val < min  ||  max != null && val > max) {
                var msg;
                if ((min || min === 0) && (max || max === 0))
                    msg = desc + ' must be between ' + utils.format(min, "C"+(dollar?"D":""), 0, dp) +
                        ' and ' + utils.format(max, "C"+(dollar?"D":""), 0, dp) + '.';
                else if (min  ||  min === 0)
                    msg = desc + ' must be greater than or equal to ' + utils.format(min, "C"+(dollar?"D":""), 0, dp) + '.';
                else
                    msg = desc + ' must be less than or equal to ' + utils.format(max, "C"+(dollar?"D":""), 0, dp) + '.';
                utils.showMessage('Error', msg, function () {
                    jqObj.focus();
                });
                return true;
            }
            return false;
        };

    };

    var componentInfo = {
        name: 'NumericInput',
        tag: 'numeric-input',
        processor: processor
    };
    utils.newComponent(componentInfo);

    Component.NumericInput.$numberinput = function (elm) {
        var val = elm.value.trim();
        if (elm.kiss.elementInfo.dollarSign)
            if ($.isNumeric(elm.kiss.elementInfo.min) && elm.kiss.elementInfo.min >= 0)
                val = val.replace(/[^0-9\.,$]/g,'');  // remove characters
            else
                val = val.replace(/[^0-9\.,$-]/g,'');  // remove characters
        else if ($.isNumeric(elm.kiss.elementInfo.min) && elm.kiss.elementInfo.min >= 0)
            val = val.replace(/[^0-9\.,]/g,'');  // remove characters
        else
            val = val.replace(/[^0-9\.,-]/g,'');  // remove characters
        var ret = '';
        var ndp = 0;  // number of decimal points
        var ndr = 0;  // number of digits to the right of the decimal point
        var andp = elm.kiss.elementInfo.dp;
        if (elm.value.trim() !== val)
            utils.beep();
        for (var i=0 ; i < val.length ; i++) {
            var c = val.charAt(i);
            if (c === '.') {
                if (!ndp && andp !== 0) {
                    ndp++;
                    ret += c;
                } else
                    utils.beep();
            } else if (ndp) {
                if (ndr < andp  ||  andp < 0) {
                    ndr++;
                    ret += c;
                } else
                    utils.beep();
            } else
                ret += c;
        }
        return ret;
    };

    Component.NumericInput.$formatnumber = function (elm) {
        var sval = elm.value;
        var ndp = elm.kiss.elementInfo.dp;
        sval = sval.replace(/[^0-9\.-]/g,'');  // remove commas and other characters
        if (!sval.length)
            return sval;
        var nval = Number(sval);
        return utils.format(nval, "C" + (elm.kiss.elementInfo.dollarSign ? 'D' : '')+(elm.kiss.elementInfo.blankIfZero?"":"B"), 0, ndp);
    };

})();


//# sourceURL=kiss/component/numericInput/NumericInput.js
