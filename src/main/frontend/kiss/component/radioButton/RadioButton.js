/*
      Author: Blake McBride
      Date:  4/23/18
*/

/* global DOMUtils, Utils */

'use strict';

DOMUtils.RadioButtons = {};
DOMUtils.RadioButtons.groups = {};

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

        if (!DOMUtils.RadioButtons.groups[group]) {
            DOMUtils.RadioButtons.groups[group] = {};
            DOMUtils.RadioButtons.groups[group].required = false;
            DOMUtils.RadioButtons.groups[group].ids = [];  //  the individual ids in the group
        }
        if (checked)
            DOMUtils.RadioButtons.groups[group].default_value = value;
        if (required)
            DOMUtils.RadioButtons.groups[group].required = true;
        DOMUtils.RadioButtons.groups[group].ids.push(id);

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
                DOMUtils.hide(divEl);
            else {
                DOMUtils.show(divEl);
                divEl.style.visibility = 'visible';
            }
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            const divEl = document.getElementById(id + '--div');
            if (flg) {
                DOMUtils.show(divEl);
                divEl.style.visibility = 'visible';
            }
            else
                DOMUtils.hide(divEl);
            return this;
        };

        newElm.isHidden = function () {
            return DOMUtils.isHidden(document.getElementById(id + '--div'));
        };

        newElm.isVisible = function () {
            return !DOMUtils.isHidden(document.getElementById(id + '--div'));
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

DOMUtils.RadioButtons.getValue = function (group) {
    const checked = document.querySelector('input[type=radio][name="' + group + '"]:checked');
    return checked ? checked.value : undefined;
};

DOMUtils.RadioButtons.setValue = function (group, val) {
    if (!val)
        DOMUtils.RadioButtons.clear(group);
    else {
        const radio = document.querySelector('input[type=radio][name="' + group + '"][value="'+val+'"]');
        if (radio)
            radio.checked = true;
    }
};

DOMUtils.RadioButtons.clear = function (group) {
    const radios = document.querySelectorAll('input[type=radio][name="' + group + '"]');
    if (DOMUtils.RadioButtons.groups[group].default_value !== undefined)
        DOMUtils.RadioButtons.setValue(group, DOMUtils.RadioButtons.groups[group].default_value);
    else
        radios.forEach(radio => radio.checked = false);
};

//--

DOMUtils.RadioButtons.readOnly = function (group) {
    const radios = document.querySelectorAll('input[type=radio][name="' + group + '"]');
    radios.forEach(radio => radio.setAttribute('readonly', 'true'));
};

DOMUtils.RadioButtons.readWrite = function (group) {
    const radios = document.querySelectorAll('input[type=radio][name="' + group + '"]');
    radios.forEach(radio => radio.removeAttribute('readonly'));
};

DOMUtils.RadioButtons.isReadOnly = function (group) {
    const radio = document.querySelector('input[type=radio][name="' + group + '"]');
    return radio ? radio.hasAttribute('readonly') : false;
};

//--

DOMUtils.RadioButtons.hide = function (group, flg=true) {
    const ids = DOMUtils.RadioButtons.groups[group].ids;
    ids.forEach(id => {
        const divEl = document.getElementById(id + '--div');
        if (flg)
            DOMUtils.hide(divEl);
        else {
            DOMUtils.show(divEl);
            divEl.style.visibility = 'visible';
        }
    });
};

DOMUtils.RadioButtons.show = function (group, flg=true) {
    const ids = DOMUtils.RadioButtons.groups[group].ids;
    ids.forEach(id => {
        const divEl = document.getElementById(id + '--div');
        if (flg) {
            DOMUtils.show(divEl);
            divEl.style.visibility = 'visible';
        }
        else
            DOMUtils.hide(divEl);
    });
};

DOMUtils.RadioButtons.isHidden = function (group) {
    const radio = document.querySelector('input[type=radio][name="' + group + '"]');
    return radio ? DOMUtils.isHidden(radio) : true;
};

DOMUtils.RadioButtons.isVisible = function (group) {
    const radio = document.querySelector('input[type=radio][name="' + group + '"]');
    return radio ? !DOMUtils.isHidden(radio) : false;
};

//--

DOMUtils.RadioButtons.isError = function (group, desc) {
    if (!DOMUtils.RadioButtons.groups[group].required)
        return false;

    const checked = document.querySelector('input[type=radio][name="' + group + '"]:checked');
    const val = checked ? checked.value : undefined;
    if (!val) {
        Utils.showMessage('Error', desc + ' selection is required.');
        return true;
    }
    return false;
};

DOMUtils.RadioButtons.onChange = function (group, fun) {
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

DOMUtils.RadioButtons.resetGroups = function () {
    DOMUtils.RadioButtons.groups = {};
    return this;
};

DOMUtils.RadioButtons.enable = function (group, flg=true) {
    const radios = document.querySelectorAll('input[type=radio][name="' + group + '"]');
    radios.forEach(radio => radio.disabled = !flg);
};

DOMUtils.RadioButtons.disable = function (group, flg=true) {
    const radios = document.querySelectorAll('input[type=radio][name="' + group + '"]');
    radios.forEach(radio => radio.disabled = flg);
};

DOMUtils.RadioButtons.isDisabled = function (group) {
    const radio = document.querySelector('input[type=radio][name="' + group + '"]');
    return radio ? radio.disabled : false;
};

DOMUtils.RadioButtons.focus = function (group) {
    const radio = document.querySelector('input[type=radio][name="' + group + '"]');
    if (radio)
        radio.focus();
};
