/*
      Author: Blake McBride
      Date:  4/25/18
 */

/* global Utils */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nstyle;
        let waitForKeyUp = false;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';

        let nattrs = '';
        let id;
        for (let prop in attr) {
            switch (prop) {

                // new attributes


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

        const newElm = Utils.replaceHTML(id, elm, '<input type="button" style="{style}" {attr} value="{value}" id="{id}">', {
            style: nstyle,
            attr: nattrs,
            value: content ? content.trim() : ''
        });
        if (!newElm)
            return;
        const jqObj = newElm.jqObj;

        jqObj.on('change', function () {
            //           Utils.someControlValueChanged();
        });

        newElm.onclick = function (fun) {
            // the off() is used to assure that multiple calls to this method doesn't cause the function to execute multiple times
            // but it also limits to a single callback function
            jqObj.off('click').off('keyup').off('keydown');
            if (fun)
                jqObj.on('click', function (e) {
                    if (Utils.suspendDepth < 0)  // should never happen but just in case
                        Utils.suspendDepth = 0;
                    if (!waitForKeyUp && !Utils.suspendDepth)
                        fun();
                }).on('keyup', function (e) {
                    if (Utils.suspendDepth < 0)  // should never happen but just in case
                        Utils.suspendDepth = 0;
                    e.stopPropagation();
                    if (waitForKeyUp && e.key === 'Enter' && !Utils.suspendDepth) {
                        fun();
                        waitForKeyUp = false;
                    }
                }).on('keydown', function (e) {
                    if (e.key === 'Enter')
                        waitForKeyUp = true;
                    e.stopPropagation();
                });
            return this;
        };

        newElm.click = function () {
            jqObj.click();
            return this;
        };

        newElm.getValue = function () {
            return jqObj.val();
        };

        newElm.setValue = function (val) {
            jqObj.val(val);
            return this;
        };

        //--

        newElm.readOnly = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.attr('readonly', flg);
            return this;
        };

        newElm.readWrite = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.attr('readonly', !flg);
            return this;
        };

        newElm.isReadOnly = function () {
            return !!jqObj.attr('readonly');
        };

        //--

        newElm.disable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.prop('disabled', flg);
            return this;
        };

        newElm.enable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            jqObj.prop('disabled', !flg);
            return this;
        };

        newElm.isDisabled = function () {
            return !!jqObj.attr('disabled');
        };

        //--

        newElm.hide = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                jqObj.hide();
            else
                qObj.show();
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
        }
    };

    const componentInfo = {
        name: 'PushButton',
        tag: 'push-button',
        processor: processor
    };
    Utils.newComponent(componentInfo);

})();

