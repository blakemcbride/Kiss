/*
      Author: Blake McBride
      Date:  4/22/18
 */

/* global Utils, DateUtils */

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
                    min = Number(Utils.removeQuotes(attr[prop]).replace(/-/g, ""));
                    break;
                case 'max':
                    max = Number(Utils.removeQuotes(attr[prop]).replace(/-/g, ""));
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
        const jqObj = newElm.jqObj;

        function keyUpHandler(event) {
            if (enterFunction && event.key === 'Enter') {
                event.stopPropagation();
                enterFunction();
            }
            if (Utils.isChangeChar(event))
                Utils.someControlValueChanged();
        }

        jqObj.keyup(keyUpHandler);

        jqObj.on('change', function () {
            Utils.someControlValueChanged();
        });

        //--

        newElm.getIntValue = function () {
            return DateUtils.SQLtoInt(jqObj.val());
        };

        newElm.getSQLValue = function () {
            return jqObj.val();
        };

        newElm.getDateValue = function () {
            return DateUtils.intToDate(DateUtils.SQLtoInt(jqObj.val()));
        };

        newElm.setValue = function (val) {
            if (!val)
                jqObj.val('');
            else if (typeof val === 'number') {
                if (val > 30000000)
                    val = DateUtils.millsToInt(val);
                jqObj.val(DateUtils.intToSQL(val));
            } else if (typeof val === 'string') {
                if (/^\d+$/.test(val))
                    jqObj.val(DateUtils.intToSQL(Number(val)));
                else
                    jqObj.val(DateUtils.intToSQL(DateUtils.strToInt(val)));
            } else if (typeof val === 'object')  // Date
                jqObj.val(DateUtils.intToSQL(DateUtils.dateToInt(val)));
            originalValue = newElm.getIntValue();
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getIntValue();
        };

        newElm.clear = function () {
            jqObj.val('');
            originalValue = newElm.getIntValue();
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
                jqObj.show().css('visibility', 'visible');
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                jqObj.show().css('visibility', 'visible');
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

        newElm.onCChange = function (fun) {
            jqObj.off('change');
            if (fun)
                jqObj.change(() => {
                    fun(newElm.getIntValue());
                });
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
            jqObj.off('blur').off('change').off('keydown');
            if (fun) {
                jqObj.on('keydown', (event) => {
                    typing = true;
                });
                jqObj.on('change', (event) => {
                    if (!typing)
                        fun(newElm.getIntValue());
                });
                jqObj.on('blur', (event) => {
                    if (typing)
                        fun(newElm.getIntValue());
                    typing = false;
                });
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
        name: 'NativeDateInput',
        tag: 'native-date-input',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();


