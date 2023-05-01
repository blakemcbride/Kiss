/*
      Author: Blake McBride
      Date:  4/25/18
 */

/* global Utils, Component */

/*
  I need to use a div rather than a textarea because a textarea doesn't support HTML contents.  A div does.
  (I think the problem had to do with newlines.)
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
        let nstyle = "background-color: white; border: 2px solid #d0d5d5; display: inline-block; padding: 5px; word-wrap: anywhere; overflow-y: auto; cursor: text;";
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

        function removePlaceholder() {
            if (resetContent) {
                jqObj.text('');
                resetContent = false;
            }
        }

        jqObj.keydown((event) => {
            if (!max || event.key && event.key.length > 1 && event.key !== 'Enter')
                return;
            const txt = Utils.htmlToText(jqObj.html()).replace(/^\s+/, '');
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

        jqObj.focusout(() => {
            if (upcase) {
                let sval = resetContent ? '' : Utils.htmlToText(jqObj.html()).replace(/^\s+/, '');
                sval = sval ? sval.replace(/ +/g, ' ') : '';
                if (sval)
                    jqObj.html(Utils.textToHtml(sval.toUpperCase()));
            }
        });

        //--

        newElm.getValue = function () {
            let sval = resetContent ? '' : Utils.htmlToText(jqObj.html()).replace(/^\s+/, '');
            sval = sval ? sval.replace(/ +/g, ' ').trim() : '';
            if (sval && upcase) {
                sval = sval.toUpperCase();
                jqObj.html(Utils.textToHtml(sval));
            }
            if (max && sval && sval.length > max)
                sval = Utils.take(sval, max);
            return sval
        };

        newElm.setValue = function (val) {
             if (val)
                val = val.trim();
            if (!val) {
                jqObj.text(originalValue='');
                return this;
            }
            removePlaceholder();
            jqObj.html(Utils.textToHtml(originalValue=val));
            return this;
        };

        newElm.clear = function () {
            newElm.setValue('');
            if (placeholder) {
                const content = '<span style="color: gray;">' + placeholder + '</span>';
                jqObj.html(content);
                resetContent = true;
            }
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getValuel();
        };

        //--

        newElm.readOnly = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.attr('contenteditable', !flg);
            return this;
        };

        newElm.readWrite = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.attr('contenteditable', flg);
            return this;
        };

        newElm.isReadOnly = function () {
            return jqObj.attr('contenteditable') === 'false';
        };

        //--

        newElm.disable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.attr('contenteditable', !flg);
            setColor(flg);
            return this;
        };

        newElm.enable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.attr('contenteditable', flg);
            setColor(!flg);
            return this;
        };

        newElm.isDisabled = function () {
            return jqObj.attr('contenteditable') === 'false';
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
            jqObj.off('DOMSubtreeModified').on('DOMSubtreeModified', fun);
            return this;
        };

        newElm.isError = function (desc) {
            if (min) {
                let val = newElm.getValue();
                val = val ? val.replace(/\s+/g, ' ').trim() : '';
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
            let html = jqObj.html();
            let txt = Utils.htmlToText(html).replace(/^\s+/, '');
            if (max && (txt.length > max || html.length > max)) {
                txt = Utils.take(txt, max);
                html = Utils.textToHtml(txt);
                html = Utils.take(html, max);
                jqObj.html(html);
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


