/*
      Author: Blake McBride
      Date:  4/22/18
 */

/* global Utils, DateUtils */

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
                    min = Number(Utils.removeQuotes(attr[prop]).replace(/-/g, ""));
                    break;
                case 'max':
                    max = Number(Utils.removeQuotes(attr[prop]).replace(/-/g, ""));
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
            placeholder: content ? content.trim() : (placeholder ? 'mm/dd/yyyy' : '')
        });
        if (!newElm)
            return;
        const jqObj = newElm.jqObj;

        jqObj.on('input', function () {
            let val = jqObj.val().trim();
            val = val.replace(/[^0-9./-]/g, '');  // remove characters
            jqObj.val(val);
        });

        function keyUpHandler(event) {
            if (enterFunction && event.key === 'Enter') {
                event.stopPropagation();
                enterFunction();
            }
            if (Utils.isChangeChar(event))
                Utils.someControlValueChanged();
        }

        jqObj.keyup(keyUpHandler);

        jqObj.focusout(async function () {
            const val = jqObj.val();
            if (!val)
                return;
            if (!DateUtils.isValid(val)) {
                await Utils.showMessage('Error', 'Invalid date.');
                jqObj.focus();
            } else
                jqObj.val(DateUtils.intToStr4(DateUtils.strToInt(val)).trim());
        })

        //--

        newElm.getIntValue = function () {
            const val = jqObj.val();
            if (!val)
                return 0;
            return DateUtils.strToInt(val);
        };

        newElm.getSQLValue = function () {
            return DateUtils.intToSQL(newElm.getIntValue());
        };

        newElm.getDateValue = function () {
            return DateUtils.intToDate(newElm.getIntValue());
        };

        newElm.setValue = function (val) {
            if (!val) {
                jqObj.val('');
                originalValue = 0;
            } else if (typeof val === 'number') {
                if (val > 30000000)
                    val = DateUtils.millsToInt(val);
                jqObj.val(DateUtils.intToStr4(val).trim())
                originalValue = val;
            } else if (typeof val === 'string') {
                if (/^\d+$/.test(val)) {
                    jqObj.val(DateUtils.intToStr4(val=Number(val)).trim());
                    originalValue = val;
                } else {
                    originalValue = DateUtils.strToInt(val);
                    jqObj.val(DateUtils.intToStr4(originalValue).trim());
                }
            } else if (typeof val === 'object') { // Date
                originalValue = DateUtils.dateToInt(val);
                jqObj.val(DateUtils.intToStr4(originalValue).trim());
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

        newElm.focus = function () {
            jqObj.focus();
            return this;
        };

        newElm.onChange = function (fun) {
            jqObj.off('change').change(fun);
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
                    jqObj.focus();
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
                    jqObj.focus();
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


