/*
      Author: Blake McBride
      Date:  4/23/18
*/

/* global DOMHelper, Utils */

'use strict';

DOMHelper.RadioButtons = {};
DOMHelper.RadioButtons.groups = {};

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

        if (!DOMHelper.RadioButtons.groups[group]) {
            DOMHelper.RadioButtons.groups[group] = {};
            DOMHelper.RadioButtons.groups[group].required = false;
            DOMHelper.RadioButtons.groups[group].ids = [];  //  the individual ids in the group
        }
        if (checked)
            DOMHelper.RadioButtons.groups[group].default_value = value;
        if (required)
            DOMHelper.RadioButtons.groups[group].required = true;
        DOMHelper.RadioButtons.groups[group].ids.push(id);

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
        const el = newElm.element;

        el.addEventListener('change', function () {
            Utils.someControlValueChanged();
        });

        newElm.setLabel = function(lbl) {
            el.nextElementSibling.textContent = lbl;
            return this;
        };

        /*
           RadioButtons have the ability to control a single radio button or a group of radio buttons.
           The functionality for individual radio buttons is as follows.
         */

        newElm.disable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.disabled = flg;
            return this;
        };

        newElm.enable = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.disabled = !flg;
            return this;
        };

        newElm.isDisabled = function () {
            return el.disabled;
        };

        //--

        newElm.hide = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            const divEl = document.getElementById(id + '--div');
            if (flg)
                DOMHelper.hide(divEl);
            else {
                DOMHelper.show(divEl);
                divEl.style.visibility = 'visible';
            }
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            const divEl = document.getElementById(id + '--div');
            if (flg) {
                DOMHelper.show(divEl);
                divEl.style.visibility = 'visible';
            }
            else
                DOMHelper.hide(divEl);
            return this;
        };

        newElm.isHidden = function () {
            return DOMHelper.isHidden(document.getElementById(id + '--div'));
        };

        newElm.isVisible = function () {
            return !DOMHelper.isHidden(document.getElementById(id + '--div'));
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

DOMHelper.RadioButtons.getValue = function (group) {
    const checked = document.querySelector('input[type=radio][name="' + group + '"]:checked');
    return checked ? checked.value : undefined;
};

DOMHelper.RadioButtons.setValue = function (group, val) {
    if (!val)
        DOMHelper.RadioButtons.clear(group);
    else {
        const radio = document.querySelector('input[type=radio][name="' + group + '"][value="'+val+'"]');
        if (radio)
            radio.checked = true;
    }
};

DOMHelper.RadioButtons.clear = function (group) {
    const radios = document.querySelectorAll('input[type=radio][name="' + group + '"]');
    if (DOMHelper.RadioButtons.groups[group].default_value !== undefined)
        DOMHelper.RadioButtons.setValue(group, DOMHelper.RadioButtons.groups[group].default_value);
    else
        radios.forEach(radio => radio.checked = false);
};

//--

DOMHelper.RadioButtons.readOnly = function (group) {
    const radios = document.querySelectorAll('input[type=radio][name="' + group + '"]');
    radios.forEach(radio => radio.setAttribute('readonly', 'true'));
};

DOMHelper.RadioButtons.readWrite = function (group) {
    const radios = document.querySelectorAll('input[type=radio][name="' + group + '"]');
    radios.forEach(radio => radio.removeAttribute('readonly'));
};

DOMHelper.RadioButtons.isReadOnly = function (group) {
    const radio = document.querySelector('input[type=radio][name="' + group + '"]');
    return radio ? radio.hasAttribute('readonly') : false;
};

//--

DOMHelper.RadioButtons.hide = function (group, flg=true) {
    const ids = DOMHelper.RadioButtons.groups[group].ids;
    ids.forEach(id => {
        const divEl = document.getElementById(id + '--div');
        if (flg)
            DOMHelper.hide(divEl);
        else {
            DOMHelper.show(divEl);
            divEl.style.visibility = 'visible';
        }
    });
};

DOMHelper.RadioButtons.show = function (group, flg=true) {
    const ids = DOMHelper.RadioButtons.groups[group].ids;
    ids.forEach(id => {
        const divEl = document.getElementById(id + '--div');
        if (flg) {
            DOMHelper.show(divEl);
            divEl.style.visibility = 'visible';
        }
        else
            DOMHelper.hide(divEl);
    });
};

DOMHelper.RadioButtons.isHidden = function (group) {
    const radio = document.querySelector('input[type=radio][name="' + group + '"]');
    return radio ? DOMHelper.isHidden(radio) : true;
};

DOMHelper.RadioButtons.isVisible = function (group) {
    const radio = document.querySelector('input[type=radio][name="' + group + '"]');
    return radio ? !DOMHelper.isHidden(radio) : false;
};

//--

DOMHelper.RadioButtons.isError = function (group, desc) {
    if (!DOMHelper.RadioButtons.groups[group].required)
        return false;

    const checked = document.querySelector('input[type=radio][name="' + group + '"]:checked');
    const val = checked ? checked.value : undefined;
    if (!val) {
        Utils.showMessage('Error', desc + ' selection is required.');
        return true;
    }
    return false;
};

DOMHelper.RadioButtons.onChange = function (group, fun) {
    const radios = document.querySelectorAll('input[type=radio][name="' + group + '"]');
    radios.forEach(radio => {
        // Remove existing change listeners by cloning
        const newRadio = radio.cloneNode(true);
        radio.parentNode.replaceChild(newRadio, radio);

        newRadio.addEventListener('change', () => {
            if (fun) {
                const checked = document.querySelector('input[type=radio][name="' + group + '"]:checked');
                fun(checked ? checked.value : undefined);
            }
            Utils.someControlValueChanged();
        });
    });
};

DOMHelper.RadioButtons.resetGroups = function () {
    DOMHelper.RadioButtons.groups = {};
    return this;
};

DOMHelper.RadioButtons.enable = function (group, flg=true) {
    const radios = document.querySelectorAll('input[type=radio][name="' + group + '"]');
    radios.forEach(radio => radio.disabled = !flg);
};

DOMHelper.RadioButtons.disable = function (group, flg=true) {
    const radios = document.querySelectorAll('input[type=radio][name="' + group + '"]');
    radios.forEach(radio => radio.disabled = flg);
};

DOMHelper.RadioButtons.isDisabled = function (group) {
    const radio = document.querySelector('input[type=radio][name="' + group + '"]');
    return radio ? radio.disabled : false;
};

DOMHelper.RadioButtons.focus = function (group) {
    const radio = document.querySelector('input[type=radio][name="' + group + '"]');
    if (radio)
        radio.focus();
};
