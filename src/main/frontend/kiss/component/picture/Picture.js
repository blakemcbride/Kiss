/*
      Author: Blake McBride
 */

/* global Utils, Component */

'use strict';

(function () {
    const processor = (elm, attr, content) => {
        let nstyle;
        let nattrs = '';
        let id;
        let src = '';

        if (attr.style)
            nstyle = 'cursor: default; ' + attr.style;
        else
            nstyle = 'cursor: default;';

        for (let prop in attr) {
            switch (prop) {
                case 'style':
                    break;  // already dealing with this
                case 'id':
                    id = Utils.removeQuotes(attr[prop]);
                    break;
                case 'src':
                    src = Utils.removeQuotes(attr[prop]);
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        const newElm = Utils.replaceHTML(id, elm, `<img id="{id}" style="{style}" {attr} src="{src}">`, {
            style: nstyle,
            attr: nattrs,
            src: src
        });
        if (!newElm)
            return;
        const jqObj = newElm.jqObj;

        newElm.setValue = function (filename, data) {
            const idx = filename.lastIndexOf(".");
            const ext = filename.substring(idx+1).toLowerCase();
            jqObj.attr('src', 'data:image/' + ext + ';base64,' + data);
        }

        newElm.clear = function () {
            jqObj.attr('src', '');
            return this;
        };

        newElm.onclick = function (fun) {
            // the off() is used to assure that multiple calls to this method doesn't cause the function to execute multiple times
            // but it also limits to a single callback function
            jqObj.off('click');
            if (fun)
                jqObj.css('cursor', 'pointer').click(fun);
            else
                jqObj.css('cursor', 'default');
            return this;
        };

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

    };

    const componentInfo = {
        name: 'Picture',
        tag: 'picture',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();
