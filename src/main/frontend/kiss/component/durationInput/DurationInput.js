/*
      Author: Blake McBride
*/

/* global Utils, Component, DOMUtils */

'use strict';

(function () {

    const processor = function (elm, attr, content) {
        let nstyle, originalValue;
        let required = false;
        let min = null;
        let max = null;
        if (attr.style)
            nstyle = attr.style;
        else
            nstyle = '';
        let enterFunction = null;
        let nattrs = '';
        let id;
        let placeholder = true;

        for (let prop in attr) {
            switch (prop) {

                // new attributes

                case 'min':
                    min = parseDuration(Utils.removeQuotes(attr[prop]));
                    break;
                case 'max':
                    max = parseDuration(Utils.removeQuotes(attr[prop]));
                    break;
                case 'required':
                    required = true;
                    break;
                case 'no-placeholder':
                    placeholder = false;
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

        nattrs += ' oninput="this.value=Component.DurationInput.$durationinput(this)"';
        nattrs += ' data-lpignore="true"';  // kill lastpass

        const newElm = Utils.replaceHTML(id, elm, '<input type="text" style="{style}" {attr} id="{id}" placeholder="{placeholder}">', {
            style: nstyle,
            attr: nattrs,
            placeholder: content ? content.trim() : (placeholder ? 'h:mm' : '')
        });
        if (!newElm)
            return;
        const el = newElm.element;

        newElm.elementInfo.min = min;
        newElm.elementInfo.max = max;

        function keyUpHandler(event) {
            if (enterFunction && event.key === 'Enter') {
                event.stopPropagation();
                enterFunction();
            }
            if (Utils.isChangeChar(event))
                Utils.someControlValueChanged();
        }

        DOMUtils.on(el, 'keyup', keyUpHandler);

        //--

        // getValue returns total minutes (0 if blank or invalid)
        newElm.getValue = function () {
            const val = el.value.trim();
            if (!val)
                return 0;
            const m = parseDuration(val);
            return m === null ? 0 : m;
        };

        // setValue accepts total minutes
        newElm.setValue = function (val) {
            if (val === undefined || val === null || val === '') {
                el.value = '';
                originalValue = 0;
                return this;
            }
            if (typeof val === 'string') {
                const parsed = parseDuration(val);
                el.value = parsed === null ? '' : formatDuration(parsed);
            } else
                el.value = formatDuration(val);
            originalValue = newElm.getValue();
            return this;
        };

        // getHours returns the value as decimal hours (e.g. 2:15 -> 2.25)
        newElm.getHours = function () {
            return newElm.getValue() / 60;
        };

        newElm.setHours = function (val) {
            if (val === undefined || val === null || val === '')
                return newElm.setValue('');
            return newElm.setValue(Math.round(Number(val) * 60));
        };

        el.addEventListener('blur', async function () {
            const val = el.value.trim();
            if (!val)
                return;
            const m = parseDuration(val);
            if (m === null) {
                await Utils.showMessage('Error', 'Invalid duration.');
                el.focus();
            } else
                el.value = formatDuration(m);
        });

        /**
         * Sets the control value to an empty string.
         * @return {Component.DurationInput} This object.
         */
        newElm.clear = function () {
            newElm.setValue('');
            return this;
        };

        newElm.isDirty = function () {
            return originalValue !== newElm.getValue();
        };

        newElm.setMinValue = function (val) {
            min = typeof val === 'string' ? parseDuration(val) : val;
            return this;
        };

        newElm.setMaxValue = function (val) {
            max = typeof val === 'string' ? parseDuration(val) : val;
            return this;
        };

        //--

        newElm.readOnly = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.readOnly = flg;
            return this;
        };

        newElm.readWrite = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            el.readOnly = !flg;
            return this;
        };

        newElm.isReadOnly = function () {
            return el.readOnly;
        };

        //--

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
            if (flg)
                DOMUtils.hide(el);
            else
                DOMUtils.show(el);
            return this;
        };

        newElm.show = function (flg = true) {
            flg = flg && (!Array.isArray(flg) || flg.length); // make zero length arrays false too
            if (flg)
                DOMUtils.show(el);
            else
                DOMUtils.hide(el);
            return this;
        };

        newElm.isHidden = function () {
            return DOMUtils.isHidden(el);
        };

        newElm.isVisible = function () {
            return !DOMUtils.isHidden(el);
        };

        //--

        newElm.focus = function () {
            el.focus();
            return this;
        };

        newElm.onEnter = function (fun) {
            enterFunction = fun;
            return this;
        };

        let changeKeyUpHandler = null;

        newElm.onCChange = function (fun) {
            if (changeKeyUpHandler) {
                DOMUtils.off(el, 'keyup', changeKeyUpHandler);
                changeKeyUpHandler = null;
            }
            changeKeyUpHandler = function (event) {
                if (!/^[0-9:]/.test(event.key) && event.key !== 'Backspace' && event.key !== 'Delete')
                    return;
                keyUpHandler(event);
                if (fun && Utils.isChangeChar(event))
                    fun(newElm.getValue());
            };
            DOMUtils.off(el, 'keyup', keyUpHandler);
            DOMUtils.on(el, 'keyup', changeKeyUpHandler);
            return this;
        };

        let changeHandler = null;

        newElm.onChange = function (fun) {
            if (changeHandler) {
                DOMUtils.off(el, 'change', changeHandler);
                changeHandler = null;
            }
            if (fun) {
                changeHandler = function () {
                    fun(newElm.getValue());
                };
                DOMUtils.on(el, 'change', changeHandler);
            }
            return this;
        };

        newElm.isError = function (desc) {
            const raw = el.value.trim();
            if (required && !raw) {
                Utils.showMessage('Error', desc + ' is required.').then(function () {
                    el.focus();
                });
                return true;
            }
            if (!raw)
                return false;
            const val = parseDuration(raw);
            if (val === null) {
                Utils.showMessage('Error', desc + ' is not a valid duration.').then(function () {
                    el.focus();
                });
                return true;
            }
            if (min !== null && val < min || max !== null && val > max) {
                let msg;
                if ((min || min === 0) && (max || max === 0))
                    msg = desc + ' must be between ' + formatDuration(min) +
                        ' and ' + formatDuration(max) + '.';
                else if (min || min === 0)
                    msg = desc + ' must be greater than or equal to ' + formatDuration(min) + '.';
                else
                    msg = desc + ' must be less than or equal to ' + formatDuration(max) + '.';
                Utils.showMessage('Error', msg).then(function () {
                    el.focus();
                });
                return true;
            }
            return false;
        };

    };

    // Parse a "H[:MM]" string into total minutes.  Returns null on invalid input or empty string.
    function parseDuration(str) {
        if (str === null || str === undefined)
            return null;
        str = String(str).trim();
        if (!str)
            return null;
        if (!/^\d+(:\d{0,2})?$/.test(str))
            return null;
        const parts = str.split(':');
        const hours = parseInt(parts[0], 10);
        const minutes = parts.length > 1 && parts[1] !== '' ? parseInt(parts[1], 10) : 0;
        if (isNaN(hours) || isNaN(minutes) || minutes > 59)
            return null;
        return hours * 60 + minutes;
    }

    // Format total minutes as "H:MM".  Returns '' for null/undefined.
    function formatDuration(totalMinutes) {
        if (totalMinutes === null || totalMinutes === undefined || totalMinutes === '')
            return '';
        const n = Math.floor(Number(totalMinutes));
        if (isNaN(n))
            return '';
        const h = Math.floor(n / 60);
        const m = n % 60;
        return h + ':' + (m < 10 ? '0' + m : m);
    }

    const componentInfo = {
        name: 'DurationInput',
        tag: 'duration-input',
        processor: processor
    };
    Utils.newComponent(componentInfo);

    Component.DurationInput.$durationinput = function (elm) {
        const val1 = elm.value.replace(/^\s+/, '');
        const stripped = val1.replace(/[^0-9:]/g, '');  // only digits and colons
        let beeped = stripped !== val1;
        // Allow at most one colon
        let nc = 0;
        let result = '';
        for (let i = 0; i < stripped.length; i++) {
            const c = stripped.charAt(i);
            if (c === ':') {
                if (nc === 0) {
                    nc++;
                    result += c;
                } else
                    beeped = true;
            } else
                result += c;
        }
        if (beeped)
            Utils.beep();
        return result;
    };

})();
