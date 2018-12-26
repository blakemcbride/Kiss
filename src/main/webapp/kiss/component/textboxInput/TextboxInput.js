/*
      Author: Blake McBride
      Date:  4/25/18
 */

/* global utils, Component */

'use strict';

(function () {

    var processor = function (elm, attr, content) {
        var nstyle;
        var min = null;
        var upcase = false;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';
        nstyle = 'resize: none; ' + nstyle;

        var nattrs = '';
        var id;
        for (var prop in attr) {
            switch (prop) {

                // new attributes
                case 'minlength':
                    min = Number(utils.removeQuotes(attr[prop]).replace(/-/g, ""));
                    break;
                case 'upcase':
                    upcase = true;
                    break;
                case 'required':
                    if (!min)
                        min = 1;
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

        nattrs += ' oninput="this.value=Component.TextboxInput.$textinput(this)"';

        var newElm = utils.replaceHTML(id, elm, '<textarea style="{style}" {attr} placeholder="{placeholder}" id="{id}"></textarea>', {
            style: nstyle,
            attr: nattrs,
            placeholder: content ? content.trim() : ''
        });
        var jqObj = newElm.jqObj;
        
        newElm.elementInfo.upcase = upcase;

        newElm.getValue = function () {
            var sval = jqObj.val();
            return sval ? sval : '';
        };

        newElm.setValue = function (val) {
            if (val !== 0  &&  !val) {
                jqObj.val('');
                return this;
            }
            jqObj.val(val);
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
            if (min) {
                var val = newElm.getValue();
                if (val.length < min) {
                    var msg;
                    if (min === 1)
                        msg = desc + ' is required.';
                    else
                        msg = desc + ' must be at least ' + min + ' characters long.';
                    utils.showMessage('Error', msg, function () {
                        jqObj.focus();
                    });
                    return true;
                }
            }
            return false;
        };

    };

    var componentInfo = {
        name: 'TextboxInput',
        tag: 'textbox-input',
        processor: processor
    };
    utils.newComponent(componentInfo);


    Component.TextboxInput.$textinput = function (elm) {
        var val = elm.value.replace(/^\s+/, "");
        return elm.kiss.elementInfo.upcase ? val.toUpperCase() : val;
    };


})();


//# sourceURL=kiss/component/textboxInput/TextboxInput.js
