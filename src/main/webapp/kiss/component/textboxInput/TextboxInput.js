/*
      Author: Blake McBride
      Date:  4/25/18
 */

/* global Utils, Component */

/*
  I need to use a div rather than a textarea because a textarea doesn't support HTML contents.  A div does.
 */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let originalValue;
        let min = null;
        let max = null;
        let upcase = false;
        let disabled = false;
        let resetContent = false;
        let placeholder = null;
        let nstyle = "background-color: white; border: 2px solid #d0d5d5; display: inline-block; padding: 5px; word-wrap: anywhere; overflow-y: auto; ";
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
                case 'maxlength':
                    max = Number(Utils.removeQuotes(attr[prop]).replace(/-/g, ""));
                    break;
                case 'upcase':
                    upcase = true;
                    break;
                case 'required':
                    if (!min)
                        min = 1;
                    break;
                case 'disabled':
                    disabled = true;
                    break;
                case 'placeholder':
                    placeholder = Utils.removeQuotes(attr[prop]);
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

        if (!disabled)
            nattrs += ' contenteditable="true"';

        if (content)
            content = content.trim();
        if (placeholder  &&  !content) {
            content = '<span style="color: gray;">' + placeholder + '</span>';
            resetContent = true;
        }

        const newElm = Utils.replaceHTML(id, elm, '<div style="{style}" {attr} id="{id}" >{content}</div>', {
            style: nstyle,
            attr: nattrs,
            content: content ? content.trim() : ''
        });
        if (!newElm)
            return;
        const jqObj = newElm.jqObj;

        function removePlaceholder() {
            if (resetContent) {
                jqObj.text('');
                resetContent = false;
            }
        }

        jqObj.keydown((event) => {
            if (!max || event.key && event.key.length > 1 && event.key !== 'Enter')
                return;

            const html = jqObj.html();
            const txt = html ? Utils.htmlToText(html) : '';

            if (txt && txt.length >= max) {
                event.preventDefault();
                event.stopPropagation();
            }
        });

        jqObj.keyup(function (event) {
            if (event.code === "Tab")
                return;
            removePlaceholder();
            Utils.someControlValueChanged();
        });

        //--

        newElm.getValue = function () {
            let sval = resetContent ? '' : jqObj.text();
            return sval ? sval.replace(/\s+/g, ' ') : '';
        };

        newElm.setValue = function (val) {
            removePlaceholder();
            if (val !== 0  &&  !val) {
                jqObj.text(originalValue='');
                return this;
            }
            jqObj.html(Utils.textToHtml(originalValue=val));
            return this;
        };

        newElm.clear = function () {
            newElm.setValue('');
            if (placeholder) {
                let content = '<span style="color: gray;">' + placeholder + '</span>';
                jqObj.html(content);
                resetContent = true;
            }
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getValuel();
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
            jqObj.attr('contenteditable', false);
            return this;
        };

        newElm.enable = function () {
            jqObj.attr('contenteditable', true);
            return this;
        };

        newElm.isDisabled = function () {
            return !!jqObj.attr('contenteditable');
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

        newElm.onKeyUp = function (fun) {
            jqObj.off('keyup').keyup(function (event) {
                if (!Utils.isChangeChar(event))
                    return;
                removePlaceholder();
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

        jqObj.on('input', function (elm) {
            if (upcase) {
                const val = jqObj.text();
                if (val) {
                    const p = jqObj.caret();
                    jqObj.text(val.toUpperCase());
                    jqObj.caret(p);
                }
            }
        });

        jqObj.on('focus', function (elm) {
            if (resetContent)
                setTimeout(function () {
                    jqObj.caret(0);
                }, 1);
        });

    };

    const componentInfo = {
        name: 'TextboxInput',
        tag: 'textbox-input',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();


