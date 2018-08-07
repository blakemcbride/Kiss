/*
      Author: Blake McBride
      Date:  4/18/18
 */

'use strict';

(function () {

    var processor = function (elm, attr, content) {
        var nstyle;
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
                    min = Number(utils.removeQuotes(attr[prop]).replace(/-/g, ""));
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
                    id = utils.removeQuotes(attr[prop]);
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        nattrs += ' oninput="this.value=Component.TextInput.$textinput(this)" autocorrect="off" autocapitalize="off" spellcheck="false"';

        var newElm = utils.replaceHTML(id, elm, '<input type="{type}" style="{style}" {attr} placeholder="{placeholder}" id="{id}">', {
            type: password ? 'password' : 'text',
            style: nstyle,
            attr: nattrs,
            placeholder: content
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
        name: 'TextInput',
        tag: 'text-input',
        processor: processor
    };
    utils.newComponent(componentInfo);


    Component.TextInput.$textinput = function (elm) {
        var val = elm.value.replace(/^\s+/, "");
        return elm.kiss.elementInfo.upcase ? val.toUpperCase() : val;
    };


})();


//# sourceURL=htmlFlex/components/textInput/TextInput.js
