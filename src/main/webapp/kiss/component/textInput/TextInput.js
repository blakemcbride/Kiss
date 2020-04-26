/*
      Author: Blake McBride
      Date:  4/18/18
 */

/* global Utils, Component */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nstyle, originalValue;
        let min = null;
        let password = false;
        let upcase = false;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';
        let nattrs = '';
        let id;
        for (let prop in attr) {
            switch (prop) {

                // new attributes
                case 'minlength':
                    min = Number(Utils.removeQuotes(attr[prop]).replace(/-/g, ""));
                    break;
                case 'upcase':
                    upcase = true;
                    break;
                case 'required':
                    if (!min)
                        min = 1;
                    break;
                case 'password':
                    password = true;
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

        nattrs += ' autocorrect="off" autocapitalize="off" spellcheck="false"';
        nattrs += ' data-lpignore="true"';  // kill lastpass

        const newElm = Utils.replaceHTML(id, elm, '<input type="{type}" style="{style}" {attr} placeholder="{placeholder}" id="{id}">', {
            type: password ? 'password' : 'text',
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
            return sval ? sval : '';
        };

        newElm.setValue = function (val) {
            if (val !== 0  &&  !val) {
                jqObj.val(originalValue = '');
                return this;
            }
            if (upcase)
                val = val.toUpperCase();
            jqObj.val(originalValue = val);
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getValue();
        };

        newElm.clear = function () {
            return newElm.setValue('');
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

        newElm.onKeyDown = function (fun) {
            jqObj.off('keydown').keydown(function (event) {
                Utils.someControlValueChanged();
                if (fun)
                    fun(event);
            });
            return this;
        };

        newElm.onChange = function (fun) {
            jqObj.off('change').change(fun);
            return this;
        };

        newElm.focus = function () {
            jqObj.focus();
            return this;
        };

        newElm.isError = function (desc) {
            if (min) {
                let val = newElm.getValue();
                if (val.length < min) {
                    let msg;
                    if (min === 1)
                        msg = desc + ' is required.';
                    else
                        msg = desc + ' must be at least ' + min + ' characters long.';
                    Utils.showMessage('Error', msg, function () {
                        jqObj.focus();
                    });
                    return true;
                }
            }
            return false;
        };

        jqObj.on('input', function () {
            let val = jqObj.val().replace(/^\s+/, "");
            jqObj.val(upcase ? val.toUpperCase() : val);
        });
    };

    const componentInfo = {
        name: 'TextInput',
        tag: 'text-input',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();


