/*
      Author: Blake McBride
      Date:  4/25/18
 */

'use strict';

(function () {

    var processor = function (elm, attr, content) {
        var nstyle;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';

        var nattrs = '';
        var id;
        for (var prop in attr) {
            switch (prop) {

                // new attributes


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

        var newElm = utils.replaceHTML(id, elm, '<input type="button" style="{style}" {attr} value="{value}" id="{id}">', {
            style: nstyle,
            attr: nattrs,
            value: content ? content : ''
        });
        var jqObj = newElm.jqObj;

        newElm.onclick = function (fun) {
            // the unbind is used to assure that multiple calls to this method doesn't cause the function to execute multiple times
            // but it also limits to a single callback function
            jqObj.unbind('click').click(fun);
        };

        newElm.click = function () {
            jqObj.click();
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

    };

    var componentInfo = {
        name: 'PushButton',
        tag: 'push-button',
        processor: processor
    };
    utils.newComponent(componentInfo);

})();




//# sourceURL=kiss/component/pushButton/PushButton.js
