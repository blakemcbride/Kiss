/*
      Author: Blake McBride
      Date:  4/23/18
 */

'use strict';

Kiss.RadioButtons = {};  // we want this to be zeroed out each time
Kiss.RadioButtons.groups = {};


(function () {

    var processor = function (elm, attr, content) {
        var nstyle;
        var group;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';

        var nattrs = '';
        for (var prop in attr) {
            switch (prop) {

                // new attributes

                case 'group':
                case 'name':
                    group = utils.removeQuotes(attr[prop]);
                    break;

                // pre-existing attributes

                case 'style':
                    break;  // already dealing with this
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        Kiss.RadioButtons.groups[group] = true;

        var nhtml = utils.tagReplace('<input type="radio" style="{style}" {attr} name="{name}">{content}', {
            style: nstyle,
            attr: nattrs,
            name: group,
            content: content
        });

        elm.replaceWith(nhtml);
    };

    var componentInfo = {
        name: 'RadioButton',
        tag: 'radio-button',
        processor: processor
    };
    utils.newComponent(componentInfo);

})();

Kiss.RadioButtons.getValue = function (group) {
    return $('input[type=radio][name=' + group + ']:checked').val();
};

Kiss.RadioButtons.setValue = function (group, val) {
    return $('input[type=radio][name=' + group + '][value='+val+']').prop('checked', true);
};

Kiss.RadioButtons.onChange = function (group, fun) {
    return $('input[type=radio][name=' + group + ']').change(fun);
};


//# sourceURL=kiss/component/radioButton/RadioButton.js
