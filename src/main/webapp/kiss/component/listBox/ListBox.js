/*
      Author: Blake McBride
      Date:  5/25/18
 */

'use strict';

(function () {

    var processor = function (elm, attr, content) {
        var nstyle;
        var required = false;
        var size = null;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';
        nstyle = 'overflow-y: auto; ' + nstyle;  // cause vertical scrollbar only when necessary

        var nattrs = '';
        var id;
        for (var prop in attr) {
            switch (prop) {

                // new attributes

                case 'required':
                    required = true;
                    break;


                // pre-existing attributes

                case 'style':
                    break;  // already dealing with this
                case 'id':
                    id = utils.removeQuotes(attr[prop]);
                    break;
                case 'size':
                    size = attr[prop];
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }
        if (!size)
            size = '2';  // make sure it isn't a dropdown

        var newElm = utils.replaceHTML(id, elm, '<select style="{style}" {attr} id="{id}" size="{size}">{content}</select>', {
            style: nstyle,
            attr: nattrs,
            content: content,
            size: size
        });
        var jqObj = newElm.jqObj;

        newElm.clear = function () {
            jqObj.empty();
        };

        newElm.add = function (val, label) {
            jqObj.append($('<option></option>').attr('value', val).text(label));
        };

        newElm.getValue = function () {
            return jqObj.val();
        };

        newElm.setValue = function (val) {
            jqObj.val(val).change();
        };

        newElm.disable = function () {
            jqObj.prop('disabled', true);
        };

        newElm.enable = function () {
            jqObj.prop('disabled', false);
        };

        newElm.hide = function () {
            jqObj.hide();
        };

        newElm.show = function () {
            jqObj.show();
        };

        newElm.isError = function (desc) {
            if (!required)
                return false;
            var val = newElm.getValue();
            if (!val) {
                utils.showMessage('Error', desc + ' is required.', function () {
                    jqObj.focus();
                });
                return true;
            }
            return false;
        };

        var timeout;

        newElm.onClick = function (fun) {
            jqObj.click(function () {
                if (jqObj.val()) {
                    timeout = setTimeout(function () {
                        if (timeout)
                            fun(jqObj.val(), jqObj.find('option:selected').text());
                    }, 300);
                }
            });
        };

        newElm.onDblClick = function (fun) {
            jqObj.dblclick(function () {
                if (jqObj.val()) {
                    if (timeout) {
                        clearTimeout(timeout);
                        timeout = null;
                    }
                    fun(jqObj.val(), jqObj.find('option:selected').text());
                }
            });
        };

        newElm.size = function () {
            return jqObj.children('option').length;
        };

        newElm.selectedIndex = function () {
            return jqObj.prop('selectedIndex');
        };

        newElm.removeByIndex = function (idx) {
            if (idx < jqObj.children('option').length)
                jqObj.find('option').eq(idx).remove();
        };

    };

    var componentInfo = {
        name: 'ListBox',
        tag: 'list-box',
        processor: processor
    };
    utils.newComponent(componentInfo);

})();


//# sourceURL=kiss/component/listBox/ListBox.js
