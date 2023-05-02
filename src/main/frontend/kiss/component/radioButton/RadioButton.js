/*
      Author: Blake McBride
      Date:  4/23/18
 */

/* global Kiss, Utils */

'use strict';

Kiss.RadioButtons = {};
Kiss.RadioButtons.groups = {};

(function () {

    const processor = function (elm, attr, content) {
        let div_style;
        let button_style = 'margin-left: 8px;';
        let id;
        let group;
        let required = false;
        let label_style = 'margin-left: 4px;';
        let align_vertical;
        let checked = false;
        let value;
        if (attr.style)
            div_style = attr.style;
        else
            div_style = '';
        let cls = '';

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
                case 'button-style':
                    button_style = Utils.removeQuotes(attr[prop]);
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
                case 'checked':
                    checked = true;
                    nAttrs += ' checked';
                    break;
                case 'value':
                    value = attr[prop];
                    nAttrs += ' ' + prop + '="' + value + '"';
                    break;

                // preexisting attributes

                case 'style':
                    break;  // already dealing with this
                case 'id':
                    id = Utils.removeQuotes(attr[prop]);
                    break;
                case 'class':
                    cls = attr[prop];
                    break;
                default:
                    nAttrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        if (!id)
            id = Utils.nextID();

        if (!Kiss.RadioButtons.groups[group]) {
            Kiss.RadioButtons.groups[group] = {};
            Kiss.RadioButtons.groups[group].required = false;
            Kiss.RadioButtons.groups[group].ids = [];  //  the individual ids in the group
        }
        if (checked)
            Kiss.RadioButtons.groups[group].default_value = value;
        if (required)
            Kiss.RadioButtons.groups[group].required = true;
        Kiss.RadioButtons.groups[group].ids.push(id);

        if (!align_vertical)
            div_style = 'display: inline-block;' + div_style;

        const newElm = Utils.replaceHTML(id, elm, '<div id="{id}--div" class="{class}" style="{div_style}"><input type="radio" {attr} style="{button_style}" name="{name}" id="{id}"><label for="{id}" style="{label_style}">{content}</label></div>', {
            id: id,
            attr: nAttrs,
            name: group,
            class: cls,
            button_style: button_style,
            label_style: label_style,
            div_style: div_style,
            content: content ? content.trim() : ''
        });
        if (!newElm)
            return;
        const jqObj = newElm.jqObj;

        jqObj.on('change', function () {
            Utils.someControlValueChanged();
        });

        newElm.setLabel = function(lbl) {
            jqObj.next().text(lbl);
            return this;
        }

        /*
           RadioButtons have the ability to control a single radio button or a group of radio buttons.
           The functionality for individual radio buttons is as follows.
         */

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
                $('#' + id + '--div').hide();
            else
                $('#' + id + '--div').show();
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                $('#' + id + '--div').show();
            else
                $('#' + id + '--div').hide();
            return this;
        };

        newElm.isHidden = function () {
            return $('#' + id + '--div').is(':hidden');
        };

        newElm.isVisible = function () {
            return $('#' + id + '--div').is(':visible');
        };

    };

    const componentInfo = {
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
    return $('input[type=radio][name="' + group + '"]:checked').val();
};

Kiss.RadioButtons.setValue = function (group, val) {
    if (!val)
        Kiss.RadioButtons.clear(group);
    else
        $('input[type=radio][name="' + group + '"][value="'+val+'"]').prop('checked', true);
};

Kiss.RadioButtons.clear = function (group) {
    const jqObj = $('input[type=radio][name="' + group + '"]');
    if (Kiss.RadioButtons.groups[group].default_value !== undefined)
        Kiss.RadioButtons.setValue(group, Kiss.RadioButtons.groups[group].default_value);
    else
        jqObj.prop('checked', false);
};

//--

Kiss.RadioButtons.readOnly = function (group) {
    $('input[type=radio][name="' + group + '"]').attr('readonly', true);
};

Kiss.RadioButtons.readWrite = function (group) {
    $('input[type=radio][name="' + group + '"]').attr('readonly', false);
};

Kiss.RadioButtons.isReadOnly = function (group) {
    return !!$('input[type=radio][name="' + group + '"]').attr('readonly');
};

//--

Kiss.RadioButtons.hide = function (group) {
    const ids = Kiss.RadioButtons.groups[group].ids;
    ids.forEach(id => {
        $('#' + id + '--div').hide();
    });
};

Kiss.RadioButtons.show = function (group) {
    const ids = Kiss.RadioButtons.groups[group].ids;
    ids.forEach(id => {
        $('#' + id + '--div').show();
    });
};

Kiss.RadioButtons.isHidden = function (group) {
    return $('input[type=radio][name="' + group + '"]').is(':hidden');
};

Kiss.RadioButtons.isVisible = function (group) {
    return $('input[type=radio][name="' + group + '"]').is(':visible');
};

//--

Kiss.RadioButtons.isError = function (group, desc) {
    if (!Kiss.RadioButtons.groups[group].required)
        return false;

    const val = $('input[type=radio][name="' + group + '"]:checked').val();
    if (!val) {
        Utils.showMessage('Error', desc + ' selection is required.');
        return true;
    }
    return false;
};

Kiss.RadioButtons.onChange = function (group, fun) {
    const ctl = $('input[type=radio][name="' + group + '"]');
    return ctl.off('change').change(() => {
        if (fun)
            fun($('input[type=radio][name="' + group + '"]:checked').val());
        Utils.someControlValueChanged();
    });
};

Kiss.RadioButtons.resetGroups = function () {
    Kiss.RadioButtons.groups = {};
    return this;
};

Kiss.RadioButtons.enable = function (group) {
    $('input[type=radio][name="' + group + '"]').attr('disabled', false);
};

Kiss.RadioButtons.disable = function (group) {
    $('input[type=radio][name="' + group + '"]').attr('disabled', true);
};

Kiss.RadioButtons.isDisabled = function (group) {
    return !!$('input[type=radio][name="' + group + '"]').attr('disabled');
};

Kiss.RadioButtons.focus = function (group) {
    $('input[type=radio][name="' + group + '"]').focus();
};

