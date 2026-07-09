/*
      Author: Blake McBride
      Date:  4/24/20
 */

/* global Utils, Component, DOMUtils */

'use strict';

(function () {

    function escapeAttr(val) {
        return String(val == null ? '' : val)
            .replace(/&/g, '&amp;')
            .replace(/"/g, '&quot;')
            .replace(/</g, '&lt;')
            .replace(/>/g, '&gt;');
    }

    const processor = function (elm, attr, content) {
        let nstyle;
        let height = null;
        let width = null;
        let draggable = null;
        let closeButton = false;
        let closeLabel = 'Close';
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
                case 'draggable':
                    draggable = Utils.removeQuotes(attr[prop]);
                    break;
                case 'close-button':
                case 'closeButton':
                    closeButton = String(Utils.removeQuotes(attr[prop])).toLowerCase() !== 'false';
                    break;
                case 'close-label':
                case 'closeLabel':
                    closeLabel = Utils.removeQuotes(attr[prop]) || closeLabel;
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

        if (draggable != null)
            nattrs += ' data-popup-draggable="' + (String(draggable).toLowerCase() === 'false' ? 'false' : 'true') + '"';
        if (closeButton)
            nattrs += ' data-popup-close-button="true"';
        nattrs += ' hidden';
        nstyle = 'height: ' + height + '; width: ' + width + '; ' + nstyle;

        if (closeButton) {
            const closeMarkup = '<button type="button" class="popup-icon-close" data-popup-close ' +
                    'aria-label="' + escapeAttr(closeLabel) + '">&times;</button>';
            content = content.replace(/<\/popup-title>/, closeMarkup + '</popup-title>');
        }
        content = content.replace(/<popup-title/, '<div').replace(/<\/popup-title>/, '</div>');
        content = content.replace(/<popup-body/, '<div').replace(/<\/popup-body>/, '</div>');

        Utils.replaceHTML(id, elm, '<div id="{id}" style="{style}" {attr}>{content}</div>', {
            style: nstyle,
            attr: nattrs,
            content: content
        });
    };

    const componentInfo = {
        name: 'Popup',
        tag: 'popup',
        processor: processor
    };
    Utils.newComponent(componentInfo);

    document.addEventListener('click', function (evt) {
        const closeBtn = evt.target.closest('[data-popup-close]');
        if (!closeBtn || closeBtn.disabled)
            return;
        evt.preventDefault();
        const popup = closeBtn.closest('.popup-background');
        Utils.popup_close(popup ? popup.id : null);
    });

})();
