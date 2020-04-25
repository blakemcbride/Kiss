/*
      Author: Blake McBride
      Date:  4/24/20
 */

/* global Utils, Component */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nstyle;
        let height = null;
        let width = null;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';
        let nattrs = '';
        let id;
        for (let prop in attr) {
            switch (prop) {

                // new attributes
                case 'height':
                    height = Utils.removeQuotes(attr[prop]);
                    break;
                case 'width':
                    width = Utils.removeQuotes(attr[prop]);
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

        nattrs += ' hidden';
        nstyle = 'height: ' + height + '; width: ' + width + '; -' + nstyle;

        content = content.replace(/popup-title/g, 'div').replace(/popup-body/g, 'div');

        let newElm = Utils.replaceHTML(id, elm, '<div id="{id}" style="{style}" {attr}>{content}</div>', {
            style: nstyle,
            attr: nattrs,
            content: content
        });
    };

    let componentInfo = {
        name: 'Popup',
        tag: 'popup',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();


