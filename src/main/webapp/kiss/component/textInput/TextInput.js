/*
      Author: Blake McBride
      Date:  4/18/18
 */

/* global Utils, Component */

'use strict';

(function () {

    var processor = function (elm, attr, content) {
        var nstyle, originalValue;
        var min = null;
        var password = false;
        var upcase = false;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';

        var nattrs = '';
        var id;
        for (var prop in attr) {
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

        nattrs += ' oninput="this.value=Component.TextInput.$textinput(this)" autocorrect="off" autocapitalize="off" spellcheck="false"';
        nattrs += ' data-lpignore="true"';  // kill lastpass

        var newElm = Utils.replaceHTML(id, elm, '<input type="{type}" style="{style}" {attr} placeholder="{placeholder}" id="{id}">', {
            type: password ? 'password' : 'text',
            style: nstyle,
            attr: nattrs,
            placeholder: content ? content.trim() : ''
        });
        var jqObj = newElm.jqObj;

        newElm.elementInfo.upcase = upcase;

        jqObj.keydown(function () {
            Utils.someControlValueChanged();
        });

        //--

        newElm.getValue = function () {
            var sval = jqObj.val();
            return sval ? sval : '';
        };

        newElm.setValue = function (val) {
            if (val !== 0  &&  !val) {
                jqObj.val(originalValue = '');
                return this;
            }
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
            jqObj.unbind('keydown').keydown(fun);
            return this;
        };

        newElm.onChange = function (fun) {
            jqObj.unbind('change').change(fun);
            return this;
        };

        newElm.focus = function () {
            jqObj.focus();
            return this;
        };

        newElm.isError = function (desc) {
            if (min) {
                var val = newElm.getValue();
                if (val.length < min) {
                    var msg;
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
    };

    var componentInfo = {
        name: 'TextInput',
        tag: 'text-input',
        processor: processor
    };
    Utils.newComponent(componentInfo);


    Component.TextInput.$textinput = function (elm) {
        var val = elm.value.replace(/^\s+/, "");
        return elm.kiss.elementInfo.upcase ? val.toUpperCase() : val;
    };


})();


