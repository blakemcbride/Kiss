/*
      Author: Blake McBride
      Date:  4/25/18

      I was using an editable div for a while to have finer control over html strings.
      I went back to a textarea because it was too difficult to restore the cursor position
      after the control changed through JavaScript.
      This version does not support html.
 */

/* global Utils, Component */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let originalValue;
        let min = null;
        let max = null;
        let upcase = false;
        let disabled = false;
        let nstyle = "resize: none; ";
        if (attr.style)
            nstyle += attr.style;

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

                // preexisting attributes

                case 'maxlength':
                    max = Number(Utils.removeQuotes(attr[prop]).replace(/-/g, ""));
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
                case 'disabled':
                    disabled = true;
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
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

        if (content)
            content = content.trim();

        const newElm = Utils.replaceHTML(id, elm, '<textarea style="{style}" {attr} id="{id}" >{content}</textarea>', {
            style: nstyle,
            attr: nattrs,
            content: content ? content.trim() : ''
        });
        if (!newElm)
            return;
        const jqObj = newElm.jqObj;

        function setColor(disabled) {
            if (disabled) {
                jqObj.css('background-color', '#DDDDDD');
                jqObj.css('color', 'rgb(84, 84, 84)');
            } else {
                jqObj.css('background-color', 'white');
                jqObj.css('color', 'black');
            }
        }

        setColor(disabled);

        jqObj.keydown((event) => {
            if (!max || event.key && event.key.length > 1 && event.key !== 'Enter')
                return;
            let txt = Utils.htmlToText(jqObj.val()).replace(/^\s+/, '');
            txt = Utils.toASCII(txt);
            if (txt && txt.length >= max) {
                event.preventDefault();
                event.stopPropagation();
            }
        });

        function keyUpHandler(event) {
            if (event.code === "Tab")
                return;
            if (Utils.isChangeChar(event) || event.key === 'Enter')
                Utils.someControlValueChanged();
        }

        jqObj.keyup(keyUpHandler);

        jqObj.focusout(() => {
            let sval = Utils.htmlToText(jqObj.val()).replace(/^\s+/, '');
            sval = sval ? sval.replace(/ +/g, ' ') : '';
            if (Utils.forceASCII)
                sval = Utils.toASCII(sval);
            if (sval)
                jqObj.val(upcase ? sval.toUpperCase() : sval);
        });

        //--

        newElm.getValue = function () {
            let sval = Utils.htmlToText(jqObj.val());
            sval = sval ? sval.replace(/ +/g, ' ').trim() : '';
            if (Utils.forceASCII)
                sval = Utils.toASCII(sval);
            if (sval && upcase) {
                sval = sval.toUpperCase();
                jqObj.val(sval);
            }
            if (max && sval && sval.length > max)
                sval = Utils.take(sval, max);
            return sval
        };

        newElm.setValue = function (val) {
            if (val)
                val = val.trim();
            if (Utils.forceASCII)
                val = Utils.toASCII(val);
            if (!val) {
                jqObj.val(originalValue='');
                return this;
            }
            jqObj.val(originalValue=val);
            return this;
        };

        newElm.clear = function () {
            newElm.setValue('');
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getValuel();
        };

        //--

        newElm.readOnly = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.prop('readonly', flg);
            return this;
        };

        newElm.readWrite = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.prop('readonly', !flg);
            return this;
        };

        newElm.isReadOnly = function () {
            return !!jqObj.attr('readonly');
        };

        //--

        newElm.disable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.prop('disabled', flg);
            setColor(flg);
            return this;
        };

        newElm.enable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.prop('disabled', !flg);
            setColor(!flg);
            return this;
        };

        newElm.isDisabled = function () {
            return jqObj.is(':disabled');
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

        newElm.onCChange = function (fun) {
            jqObj.off('keyup').keyup(function (event) {
                keyUpHandler(event);
                if (fun && (Utils.isChangeChar(event) || event.key === 'Enter'))
                    fun(newElm.getValue());
            });
            return this;
        };

        newElm.onChange = function (fun) {
            jqObj.off('change');
            if (fun)
                jqObj.change(() => {
                    fun(newElm.getValue());
                });
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
                    Utils.showMessage('Error', msg).then(function () {
                        jqObj.focus();
                    });
                    return true;
                }
            }
            return false;
        };
    };

    const componentInfo = {
        name: 'TextboxInput',
        tag: 'textbox-input',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();


