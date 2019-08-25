/*
      Author: Blake McBride
      Date:  4/23/18
 */

/* global Kiss, Utils */

'use strict';

Kiss.RadioButtons = {};
Kiss.RadioButtons.groups = {};

(function () {

    let processor = function (elm, attr, content) {
        let nStyle;
        let id;
        let group;
        let required = false;
        let label_style = '';
        let align_vertical, div_style='';
        if (attr.style)
            nStyle = attr.style;
        else
            nStyle = '';

        let nAttrs = '';
        for (let prop in attr) {
            switch (prop) {

                // new attributes

                case 'group':
                case 'name':
                    group = Utils.removeQuotes(attr[prop]);
                    break;
                case 'required':
                    required = true;
                    break;
                case 'label-style':
                    label_style = Utils.removeQuotes(attr[prop]);
                    break;
                case 'align-horizontal':  // default
                    align_vertical = false;
                    break;
                case 'align-vertical':
                    align_vertical = true;
                    break;

                // pre-existing attributes

                case 'style':
                    break;  // already dealing with this
                case 'id':
                    id = Utils.removeQuotes(attr[prop]);
                    break;
                default:
                    nAttrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        if (!Kiss.RadioButtons.groups[group]) {
            Kiss.RadioButtons.groups[group] = {};
            Kiss.RadioButtons.groups[group].required = false;
        }

        if (required)
            Kiss.RadioButtons.groups[group].required = true;

        if (!align_vertical)
            div_style = 'display: inline-block;';

        let newElm = Utils.replaceHTML(id, elm, '<div style="{div_style}"><input type="radio" {attr} style="{style} name="{name} id="{id}"><label style="{label_style}">{content}</label></div>', {
            style: nStyle,
            attr: nAttrs,
            name: group,
            label_style: label_style,
            div_style: div_style,
            content: content ? content.trim() : ''
        });
        let jqObj = newElm.jqObj;

        /*
           RadioButtons have the ability to control a single radio button or a group of radio buttons.
           The functionality for individual radio buttons is as follows.
         */

        newElm.disable = function () {
            jqObj.prop('disabled', true);
            return this;
        };

        newElm.enable = function () {
            jqObj.prop('disabled', false);
            return this;
        };

        newElm.isDisabled = function () {
            return !!jqObj.attr('disabled');
        };

        //--

        newElm.hide = function () {
            jqObj.hide();
            return this;
        };

        newElm.show = function () {
            jqObj.show();
            return this;
        };

        newElm.isHidden = function () {
            return jqObj.is(':hidden');
        };

        newElm.isVisible = function () {
            return jqObj.is(':visible');
        };

    };

    let componentInfo = {
        name: 'RadioButton',
        tag: 'radio-button',
        processor: processor
    };

    Utils.newComponent(componentInfo);
})();


/*
      Functionality for groups of radio buttons is as follows.
 */

Kiss.RadioButtons.getValue = function (group) {
    return $('input[type=radio][name=' + group + ']:checked').val();
};

Kiss.RadioButtons.setValue = function (group, val) {
    let jqObj = $('input[type=radio][name=' + group + ']');
    let res = jqObj.prop('checked', val);
};

Kiss.RadioButtons.clear = function (group) {
    let jqObj = $('input[type=radio][name=' + group + ']');
    jqObj.attr('checked', false);
};

//--

Kiss.RadioButtons.readOnly = function (group) {
    $('input[type=radio][name=]' + group + ']').attr('readonly', true);
};

Kiss.RadioButtons.readWrite = function (group) {
    $('input[type=radio][name=]' + group + ']').attr('readonly', false);
};

Kiss.RadioButtons.isReadOnly = function (group) {
    return !!$('input[type=radio][name=]' + group + ']').attr('readonly');
};

//--

Kiss.RadioButtons.hide = function (group) {
    $('input[type=radio][name=]' + group + ']').attr('readonly', true);
};

Kiss.RadioButtons.show = function (group) {
    $('input[type=radio][name=]' + group + ']').attr('readonly', false);
};

Kiss.RadioButtons.isHidden = function (group) {
    return $('input[type=radio][name=]' + group + ']').is(':hitten');
};

Kiss.RadioButtons.isVisible = function (group) {
    return $('input[type=radio][name=]' + group + ']').is(':visible');
};

//--

Kiss.RadioButtons.isError = function (group, desc) {
    if (!Kiss.RadioButtons.groups[group].required)
        return false;

    let val = $('input[type=radio][name=' + group + ']:checked').val();
    if (!val) {
        Utils.showMessage('Error', 'A ' + desc + ' selection is required.');
        return true;
    }
    return false;
};

Kiss.RadioButtons.onChange = function (group, fun) {
    return $('input[type=radio][name=' + group + ']').change(fun);
};

Kiss.RadioButtons.resetGroups = function () {
    Kiss.RadioButtons.groups = {};
    return this;
};

Kiss.RadioButtons.enable = function (group) {
    $('input[type=radio][name=]' + group + ']').attr('disabled', false);
};

Kiss.RadioButtons.disable = function (group) {
    $('input[type=radio][name=]' + group + ']').attr('disabled', true);
};

Kiss.RadioButtons.isDisabled = function (group) {
    return !!$('input[type=radio][name=]' + group + ']').attr('disabled');
};

Kiss.RadioButtons.focus = function (group) {
    $('input[type=radio][name=]' + group + ']').focus();
};

//# sourceURL=kiss/component/radioButton/RadioButton.js
