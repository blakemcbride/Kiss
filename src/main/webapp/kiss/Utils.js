/**
 * Created by Blake McBride on 1/20/16.
 */

'use strict';


let Component = {};

Component.ComponentsBeingLoaded = 0;

Component.ComponentList = [];

let Kiss = {};

/**
 * This is how components are accessed.
 *
 * @param {string} id  for radio buttons this is the id of the group, for other controls this is the control's id
 * @returns {*} the component object
 */
function $$(id) {
    if (typeof Kiss !== 'undefined' && typeof Kiss.RadioButtons !== 'undefined' && Kiss.RadioButtons.groups[id]) {
        const rbObj = {};
        let originalValue;
        rbObj.getValue = function () {
            return Kiss.RadioButtons.getValue(id);
        };
        rbObj.getIntValue = function () {
            let val = Kiss.RadioButtons.getValue(id);
            return val ? Number(val) : 0;
        };
        rbObj.setValue = function (val) {
            Kiss.RadioButtons.setValue(id, val);
            originalValue = rbObj.getValue();
            return rbObj;
        };
        rbObj.clear = function () {
            Kiss.RadioButtons.clear(id);
            originalValue = rbObj.getValue();
            return rbObj;
        };
        rbObj.isDirty = function () {
            return originalValue !== rbObj.getValue(id);
        };
        rbObj.readOnly = function () {
            Kiss.RadioButtons.readOnly(id);
            return rbObj;
        };
        rbObj.readWrite = function () {
            Kiss.RadioButtons.readWrite(id);
            return rbObj;
        };
        rbObj.isReadOnly = function () {
            return Kiss.RadioButtons.isReadOnly(id);
        };
        rbObj.onChange = function (fun) {
            Kiss.RadioButtons.onChange(id, fun);
            return rbObj;
        };
        rbObj.isError = function (lbl) {
            return Kiss.RadioButtons.isError(id, lbl);
        };
        rbObj.enable = function () {
            Kiss.RadioButtons.enable(id);
            return rbObj;
        };
        rbObj.disable = function () {
            Kiss.RadioButtons.disable(id);
            return rbObj;
        };
        rbObj.hide = function () {
            Kiss.RadioButtons.hide(id);
            return rbObj;
        };
        rbObj.show = function () {
            Kiss.RadioButtons.show(id);
            return rbObj;
        };
        rbObj.isHidden = function () {
            return Kiss.RadioButtons.isHidden(id);
        };
        rbObj.isVisible = function () {
            return Kiss.RadioButtons.isVisible(id);
        };
        rbObj.focus = function () {
            Kiss.RadioButtons.focus(id);
            return rbObj;
        };
        return rbObj;
    }
    const e = $((id.charAt(0) === '#' ? '' : '#') + id);
    return e.length ? e[0].kiss : null;
}

/**
 * General utilities class
 */
class Utils {

    /**
     * Display a popup window with a message to the user.  The user will click "Ok" when they have read the message.
     * If the title is 'Error' the popup will appear in red.
     *
     * @param {string} title appears on the title bar of the message window
     * @param {string} message the message to be displayed
     * @returns {Promise} when popup disappears
     */
    static showMessage(title, message) {
        const self = this;
        return new Promise(function (resolve, reject) {
            let modal = $('#msg-modal');
            if (!modal.length) {
                $('body').append(
                    '<div id="msg-modal" class="msg-modal">' +
                    '  <!-- Modal content -->' +
                    '  <div class="msg-modal-content" id="msg-modal-content-tab">' +
                    '    <div class="msg-modal-header" id="msg-modal-header-tab">' +
                    '      <span id="msg-close-btn" class="msg-close">&times;</span>' +
                    '      <p id="msg-header" style="margin-top: 2px;">Modal Header</p>' +
                    '    </div>' +
                    '    <div class="msg-modal-body">' +
                    '      <p id="msg-message" style="margin-top: 5px, margin-bottom: 5px;"></p>' +
                    '    </div>' +
                    '    <div class="msg-modal-footer">' +
                    '      <input type="button" value="Ok" id="message-ok" style="margin-top: 5px; margin-bottom: 10px;">' +
                    '    </div>' +
                    '  </div>' +
                    '</div>');
                modal = $('#msg-modal');  // the append changes this
            }

            $('#msg-header').text(title);
            const header = $('#msg-modal-header-tab');
            self.makeDraggable(header, $('#msg-modal-content-tab'));
            $('#msg-message').text(message);
            const closeBtn = $('#msg-close-btn');
            if (title === 'Error')
                header.css('background-color', 'red');
            else
                header.css('background-color', '#6495ed');
            function endfun() {
                modal.hide();
                resolve();
            }
            modal.show();
            let waitForKeyUp = false;
            closeBtn.off('click').click(function (e) {
                endfun();
            });
            $('#message-ok').off('click').off('keyup').click(function (e) {
                if (!waitForKeyUp)
                    endfun();
            }).focus().on('keyup', function (e) {
                e.stopPropagation();
                if (waitForKeyUp && e.key === 'Enter')
                    endfun();
            }).on('keydown', function (e) {
                e.stopPropagation();
                if (e.key === 'Enter')
                     waitForKeyUp = true;
            });
        });
    }

    /**
     * Display a modal popup that asks the user a yes/no question.
     *
     * The yesFun/noFun can be used, or this function returns a promise
     * so that is can be used with async/await.
     *
     * @param {string} title text on the title bar of the popup
     * @param {string} message the question being asked
     * @param {function} yesFun function that gets executed if the user clicks 'Yes'
     * @param {function} noFun function that gets executed if the user clicks 'No'
     */
    static yesNo(title, message, yesFun = null, noFun = null) {
        return new Promise(function(resolve, reject) {
            if (!$('#yesno-modal').length) {
                $('body').append(
                    '<div id="yesno-modal" class="msg-modal">' +
                    '  <!-- Modal content -->' +
                    '  <div class="msg-modal-content" id="yesno-popup-content">' +
                    '    <div class="msg-modal-header" id="yesno-popup-header">' +
                    '      <span id="yesno-close-btn" class="msg-close">&times;</span>' +
                    '      <p id="yesno-header" style="margin-top: 2px;">Modal Header</p>' +
                    '    </div>' +
                    '    <div class="msg-modal-body">' +
                    '      <p id="yesno-message" style="margin-top: 5px, margin-bottom: 5px;"></p>' +
                    '    </div>' +
                    '    <div class="msg-modal-footer">' +
                    '      <input type="button" value="Yes" id="yesno-yes" style="margin-top: 5px; margin-bottom: 10px;">' +
                    '      <input type="button" value="No" id="yesno-no" style="margin-top: 5px; margin-bottom: 10px; margin-left: 10px;">' +
                    '    </div>' +
                    '  </div>' +
                    '</div>');
            }
            Utils.makeDraggable($('#yesno-popup-header'), $('#yesno-popup-content'));

            $('#yesno-header').text(title);
            $('#yesno-message').text(message);
            const modal = $('#yesno-modal');
            const span = $('#yesno-close-btn');
            modal.show();
            span.off('click').click(function () {
                modal.hide();
                if (noFun)
                    noFun();
                resolve(false);
            });
            $('#yesno-yes').off('click').click(function () {
                modal.hide();
                if (yesFun)
                    yesFun();
                resolve(true);
            });
            $('#yesno-no').off('click').click(function () {
                modal.hide();
                if (noFun)
                    noFun();
                resolve(false);
            });
        });
    }

    /**
     * Display a modal popup with a message.  The message stays there until the application executes waitMessageEnd().
     * This method is used when the user needs to be notified to wait for a long running process.
     *
     * @param {string} message the message to be displayed
     */
    static waitMessage(message) {
        if (!$('#wmsg-modal').length) {
            $('body').append(
                '<div id="wmsg-modal" class="msg-modal">' +
                '  <!-- Modal content -->' +
                '  <div class="wmsg-modal-content" id="wait-msg-content">' +
                '    <div class="msg-modal-body">' +
                '      <p id="wmsg-message" style="margin-top: 5px, margin-bottom: 5px;"></p>' +
                '    </div>' +
                '  </div>' +
                '</div>');
        }
        this.makeDraggable($('#wait-msg-content'), $('#wait-msg-content'));
        $('#wmsg-message').text(message);
        $('#wmsg-modal').show();
    }

    /**
     * This terminates the wait message initiated by waitMessage()
     */
    static waitMessageEnd() {
        $('#wmsg-modal').hide();
    }

    static getID(id) {
        let e = $('#' + id);
        if (!e.length) {
            id = id.replace(/_/g, '-');
            e = $('#' + id);
        }
        if (!e.length) {
            id = id.replace(/-/g, '_');
            e = $('#' + id);
        }
        return e.length ? id : null;
    }

    static zeroPad(num, places) {
        const zero = places - num.toString().length + 1;
        return Array(+(zero > 0 && zero)).join("0") + num;
    }

    /**
     * APL-like take for strings.
     *
     * @param {string} s
     * @param {number} n
     * @returns {string}
     */
    static take(s, n) {
        if (!s)
            s = '';
        if (s.length === n)
            return s;
        if (n >= 0) {
            if (n < s.length)
                return s.substring(0, n);
            for (n -= s.length; n-- > 0;)
                s += ' ';
            return s;
        } else {
            n = -n;
            if (n < s.length)
                return Utils.drop(s, s.length - n);
            let sb = '';
            for (n -= s.length; n-- > 0;)
                sb += ' ';
            sb += s;
            return sb;
        }
    }

    /**
     * APL-like drop for strings.
     *
     * @param {string} s
     * @param {number} n
     * @returns {string}
     */
    static drop(s, n) {
        if (!s)
            s = '';
        if (!n)
            return s;
        if (n >= s.length || -n >= s.length)
            return '';
        if (n > 0)
            return s.substring(n);
        return s.substring(0, s.length + n);
    }

    /**
     *  Numeric formatter.  Takes a number and converts it to a nicely formatted String in a specified number base.
     *
     * @param {number} num    number to be formatted
     * @param {number} base   numeric base (like base 2 = binary, 16=hex...)
     * @param {string} msk    format mask - any combination of the following:<br>
     *  <ul>
     *     <li>B = blank if zero</li>
     *     <li>C = add commas</li>
     *     <li>L = left justify number</li>
     *     <li>P = put parentheses around negative numbers</li>
     *     <li>Z = zero fill</li>
     *     <li>D = floating dollar sign</li>
     *     <li>U = uppercase letters in conversion</li>
     *     <li>R = add a percent sign to the end of the number</li>
     *  </ul>
     * @param  {number} wth    total field width (0 means auto)
     * @param  {number} dp    number of decimal places (-1 means auto)
     * @return {string} the formatted String
     *<p>
     * Example:
     *<br><br>
     *    let r = Utils.formatb(-12345.348, 10, "CP", 12, 2);
     *<br><br>
     *    result in r:   "(12,345.35)"
     * </p>
     *
     * @see format
     */
    static formatb(num, base, msk, wth, dp) {
        let si, i, r, n;
        let sign, blnk, comma, left, paren, zfill, nd, dol, tw, dl, ez, ucase, cf, percent;
        let dbase;
        const alpha = '0123456789abcdefghijklmnopqrstuvwxyz';

        if (base < 2 || base > alpha.length)
            base = 10;
        dbase = base;

        if (num < 0.0) {
            num = -num;
            sign = 1;
        } else
            sign = 0;

        /*  round number  */

        if (dp >= 0) {
            r = Math.pow(dbase, dp);
//    n = Math.floor(base/20.0 + n * r) / r;
            num = Math.floor(.5 + num * r) / r;
        }
        switch (base) {
            case 10:
                cf = 3;
                dl = num < 1.0 ? 0 : 1 + Math.floor(Math.log10(num));
                /* # of digits left of .  */
                break;
            case 2:
                cf = 4;
                dl = num < 1.0 ? 0 : 1 + Math.floor(Math.log(num) / .6931471806);
                /* # of digits left of .  */
                break;
            case 8:
                cf = 3;
                dl = num < 1.0 ? 0 : 1 + Math.floor(Math.log(num) / 2.079441542);
                /* # of digits left of .  */
                break;
            case 16:
                cf = 4;
                dl = num < 1.0 ? 0 : 1 + Math.floor(Math.log(num) / 2.772588722);
                /* # of digits left of .  */
                break;
            default:
                cf = 3;
                dl = num < 1.0 ? 0 : 1 + Math.floor(Math.log(num) / Math.log(dbase));
                /* # of digits left of .  */
                break;
        }

        if (dp < 0) {   /* calculate the number of digits right of decimal point */
            n = num < 0.0 ? -num : num;
            dp = 0;
            while (dp < 20) {
                n -= Math.floor(n);
                if (1E-5 >= n)
                    break;
                dp++;
                /*  round n to 5 places     */
                r = Math.pow(10, 5);
                r = Math.floor(.5 + Math.abs(n * base * r)) / r;
                n = n < 0.0 ? -r : r;
            }
        }
        blnk = comma = left = paren = zfill = dol = ucase = percent = 0;
        if (msk) {
            msk = msk.toUpperCase();
            for (let i2 = 0; i2 < msk.length; i2++)
                switch (msk.charAt(i2)) {
                    case 'B':  // blank if zero
                        blnk = 1;
                        break;
                    case 'C':  //  add commas
                        comma = Math.floor((dl - 1) / cf);
                        if (comma < 0)
                            comma = 0;
                        break;
                    case 'L':  //  left justify
                        left = 1;
                        break;
                    case 'P':  //  parens around negative numbers
                        paren = 1;
                        break;
                    case 'Z':  //  zero fill
                        zfill = 1;
                        break;
                    case 'D':  //  dollar sign
                        dol = 1;
                        break;
                    case 'U':  //  upper case letters
                        ucase = 1;
                        break;
                    case 'R':  //  add percent
                        percent = 1;
                        break;
                }
        }
        /*  calculate what the number should take up   */

        ez = num < 1.0 ? 1 : 0;
        tw = dol + paren + comma + sign + dl + dp + (dp === 0 ? 0 : 1) + ez + percent;
        if (wth < 1)
            wth = tw;
        else if (tw > wth) {
            if (ez)
                tw -= ez--;
            if ((i = dol) && tw > wth)
                tw -= dol--;
            if (tw > wth && comma) {
                tw -= comma;
                comma = 0;
            }
            if (tw < wth && i) {
                tw++;
                dol = 1;
            }
            if (tw > wth && paren)
                tw -= paren--;
            if (tw > wth && percent)
                tw -= percent--;
            if (tw > wth) {
                let tbuf = '';
                for (i = 0; i < wth; i++)
                    tbuf += '*';
                return tbuf;
            }
        }
        let buf = new Array(wth);
        num = Math.floor(.5 + num * Math.floor(.5 + Math.pow(dbase, dp)));
        if (blnk && num === 0.0) {
            for (i = 0; i < wth;)
                buf[i++] = ' ';
            return buf.join('');
        }
        si = wth;

        if (left && wth > tw)
            for (i = wth - tw; i--;)
                buf[--si] = ' ';
        if (paren)
            buf[--si] = sign ? ')' : ' ';
        if (percent)
            buf[--si] = '%';
        for (nd = 0; nd < dp && si; nd++) {
            num /= dbase;
            i = Math.floor(dbase * (num - Math.floor(num)) + .5);
            num = Math.floor(num);
            buf[--si] = ucase && i > 9 ? alpha.charAt(i).toUpperCase() : alpha.charAt(i);
        }
        if (dp)
            if (si)
                buf[--si] = '.';
            else
                num = 1.0;
        if (ez && si > sign + dol)
            buf[--si] = '0';
        nd = 0;
        while (num > 0.0 && si)
            if (comma && nd === cf) {
                buf[--si] = ',';
                nd = 0;
            } else {
                num /= dbase;
                i = Math.floor(dbase * (num - Math.floor(num)) + .5);
                num = Math.floor(num);
                buf[--si] = ucase && i > 9 ? alpha.charAt(i).toUpperCase() : alpha.charAt(i);
                nd++;
            }
        if (zfill)
            for (i = sign + dol; si > i;)
                buf[--si] = '0';
        if (dol && si)
            buf[--si] = '$';
        if (sign)
            if (si)
                buf[--si] = paren ? '(' : '-';
            else
                num = 1.0;
        /*  signal error condition */

        while (si)
            buf[--si] = ' ';

        if (num !== 0.0)         /*  should never happen. but just in case  */
            for (i = 0; i < wth;)
                buf[i++] = '*';

        return buf.join('');
    }

    /**
     *  Numeric formatter.  Takes a number and converts it to a nicely formatted String (for number in base 10).
     *
     * @param {number} num    number to be formatted
     * @param {string} msk    format mask - any combination of the following:<br>
     *  <ul>
     *     <li>B = blank if zero</li>
     *     <li>C = add commas</li>
     *     <li>L = left justify number</li>
     *     <li>P = put parentheses around negative numbers</li>
     *     <li>Z = zero fill</li>
     *     <li>D = floating dollar sign</li>
     *     <li>R = add a percent sign to the end of the number</li>
     *  </ul>
     * @param  {number} wth    total field width (0 means auto)
     * @param  {number} dp    number of decimal places (-1 means auto)
     * @return {string} string the formatted String
     *<p>
     * example:
     *<br><br>
     *    let r = Utils.format(-12345.348, "CP", 12, 2);
     *<br><br>
     *    result in r:  "(12,345.35)"
     * </p>
     */
    static format(num, msk, wth, dp) {
        return Utils.formatb(num, 10, msk, wth, dp);
    }

    /**
     * Get the extension of the file name.  So, "abc.def" would return "def".
     *
     * @param {string} filename
     * @returns {string}
     */
    static fileNameExtension(filename) {
        const fileSplit = filename.split('.');
        let fileExt = '';
        if (fileSplit.length > 1)
            fileExt = fileSplit[fileSplit.length - 1];
        return fileExt;
    }

    /**
     * When calling a SOAP web service, array returns are not arrays but single objects when there is only one element.
     * (XML to Json problem).  This function assures that an element is an array when one is expected.
     *
     * @param x
     * @returns {Array}
     */
    static assureArray(x) {
        if ($.isArray(x))
            return x;
        let r = [];
        if (x)
            r[0] = x;
        return r;
    }

    /**
     * Parse the URL string and extract the URL parameters.
     *
     * @param {string} key parameter name
     * @returns {string} the parameter value or null if not there
     */
    static getURLParam(key) {
        const url = window.location.href;
        const args = url.split('?');
        if (args.length !== 2)
            return null;
        const pairs = args[1].split('&');
        if (pairs.length === 0)
            return null;
        for (let i = 0; i < pairs.length; i++) {
            let keyval = pairs[i].split('=');
            if (keyval[0] === key)
                if (keyval.length > 0)
                    return decodeURI(keyval[1]);
                else
                    return '';
        }
        return null;
    }

    /**
     * Initialize a tagless component.
     *
     * @param {string} path back-end path to the component
     */
    static useTaglessComponent(path) {
        let npath;
        const loadScript = function (arg) {
            getScript(npath + '.js' + arg, function (data, textStatus, jqxhr) {
            });
        };

        const loadComponent = function (arg) {
            npath = path.charAt(0).toLowerCase() + path.substr(1) + '/' + path.charAt(0).toUpperCase() + path.substr(1);
            npath = '/kiss/component/' + npath;
            loadScript(arg);
        };
        loadComponent(Utils.controlCache ? '?ver=' + Utils.softwareVersion : '');
    }

    // internal
    static tagReplace(inp, rpl) {
        for (let prop in rpl) {
            let val = rpl[prop];
            if (!val && val !== 0)
                val = '';
            inp = inp.replace(new RegExp('{' + prop + '}', 'g'), val);
        }
        return inp;
    }

    // internal
    static afterComponentsLoaded(fun) {
        Component.AfterAllComponentsLoaded = fun;
    }

    /* (internal)
     * Rescan the HTML file and replace KISS components with HTML components.
     * This needs to be done each time new KISS controls are attached.
     */
    static rescan() {
        let n = -1;
        while (n) {  // keep replacing until nothing left to replace
            n = 0;
            for (let i = 0; i < Component.ComponentList.length; i++) {
                let ci = Component.ComponentList[i];
                if (ci.tag && ci.name === 'Popup') {
                    $(ci.tag).each(function () {
                        let elm = $(this);
                        ci.processor(elm, Utils.getAllAttributes(elm), elm.html());
                        n++;
                    });
                    break;
                }
            }
            for (let i = 0; i < Component.ComponentList.length; i++) {
                let ci = Component.ComponentList[i];
                if (ci.tag && ci.name !== "Popup")
                    $(ci.tag).each(function () {
                        let elm = $(this);
                        ci.processor(elm, Utils.getAllAttributes(elm), elm.html());
                        n++;
                    });
            }
        }
    }

    /**
     * Loads a component (new HTML element)
     *
     * @param {string} path back-end path to the component HTML and JS files.
     */
    static useComponent(path) {
        let npath;
        Component.ComponentsBeingLoaded++;
        const loadScript = function (arg) {
            getScript(npath + '.js' + arg, function () {
                if (!--Component.ComponentsBeingLoaded && Component.AfterAllComponentsLoaded) {
                    Utils.rescan();  // does all the tag replacement
                    Component.AfterAllComponentsLoaded();
                    Component.AfterAllComponentsLoaded = null;
                }
            });
        };

        const loadComponent = function (arg) {
            npath = path.charAt(0).toLowerCase() + path.substr(1) + '/' + path.charAt(0).toUpperCase() + path.substr(1);
            npath = 'kiss/component/' + npath;
            loadScript(arg);
        };

        loadComponent(Utils.controlCache ? '?ver=' + Utils.softwareVersion : '');
    }

    static getAllAttributes(elm) {
        const ret = {};
        $.each(elm[0].attributes, function () {
            ret[this.name] = this.value;
        });
        return ret;
    }

    static removeQuotes(s) {
        if (!s || !s.length)
            return '';
        let c = s.charAt(0);
        if (c === '"' || c === "'")
            s = s.substr(1);
        if (!s.length)
            return '';
        c = s.slice(-1);  // last character
        if (c === '"' || c === "'")
            s = s.substring(0, s.length - 1);
        return s;
    }

    static newComponent(ci) {
        Component.ComponentList.push(ci);
        Component[ci.name] = {};
    }

    /**
     * Emits an audible beep on the user's computer.
     *
     */
    static beep() {
        const snd = new Audio("data:audio/wav;base64,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qJrx4KOQjahZxWbcZgztj2c49nKmkId44S71j0c8eV9yDK6uPRzx5X18eDvjvQ6yKo9ZSS6l//8elePK/Lf//IInrOF/FvDoADYAGBMGb7FtErm5MXMlmPAJQVgWta7Zx2go+8xJ0UiCb8LHHdftWyLJE0QIAIsI+UbXu67dZMjmgDGCGl1H+vpF4NSDckSIkk7Vd+sxEhBQMRU8j/12UIRhzSaUdQ+rQU5kGeFxm+hb1oh6pWWmv3uvmReDl0UnvtapVaIzo1jZbf/pD6ElLqSX+rUmOQNpJFa/r+sa4e/pBlAABoAAAAA3CUgShLdGIxsY7AUABPRrgCABdDuQ5GC7DqPQCgbbJUAoRSUj+NIEig0YfyWUho1VBBBA//uQZB4ABZx5zfMakeAAAAmwAAAAF5F3P0w9GtAAACfAAAAAwLhMDmAYWMgVEG1U0FIGCBgXBXAtfMH10000EEEEEECUBYln03TTTdNBDZopopYvrTTdNa325mImNg3TTPV9q3pmY0xoO6bv3r00y+IDGid/9aaaZTGMuj9mpu9Mpio1dXrr5HERTZSmqU36A3CumzN/9Robv/Xx4v9ijkSRSNLQhAWumap82WRSBUqXStV/YcS+XVLnSS+WLDroqArFkMEsAS+eWmrUzrO0oEmE40RlMZ5+ODIkAyKAGUwZ3mVKmcamcJnMW26MRPgUw6j+LkhyHGVGYjSUUKNpuJUQoOIAyDvEyG8S5yfK6dhZc0Tx1KI/gviKL6qvvFs1+bWtaz58uUNnryq6kt5RzOCkPWlVqVX2a/EEBUdU1KrXLf40GoiiFXK///qpoiDXrOgqDR38JB0bw7SoL+ZB9o1RCkQjQ2CBYZKd/+VJxZRRZlqSkKiws0WFxUyCwsKiMy7hUVFhIaCrNQsKkTIsLivwKKigsj8XYlwt/WKi2N4d//uQRCSAAjURNIHpMZBGYiaQPSYyAAABLAAAAAAAACWAAAAApUF/Mg+0aohSIRobBAsMlO//Kk4soosy1JSFRYWaLC4qZBYWFRGZdwqKiwkNBVmoWFSJkWFxX4FFRQWR+LsS4W/rFRb/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////VEFHAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAU291bmRib3kuZGUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMjAwNGh0dHA6Ly93d3cuc291bmRib3kuZGUAAAAAAAAAACU=");
        snd.play();
    }

    static nextID() {
        return "ID-" + Utils.count++;
    }

    static replaceHTML(id, elm, template, rplobj) {
        if (!id)
            id = Utils.nextID();
        rplobj.id = id;
        const newHTML = Utils.tagReplace(template, rplobj);
        elm.replaceWith(newHTML);
        const jqObj = $('#' + id);
        const newElm = jqObj[0];
        if (!newElm) {
            console.log(elm[0].localName + ' is missing an ID');
            return undefined;
        }
        newElm.kiss = {};
        newElm.kiss.jqObj = jqObj;
        newElm.kiss.elementInfo = {};
        return newElm.kiss;
    }

    static async getHTML( url, callback ) {
        let response;
        try {
            response = await fetch(url, {
                method: 'GET',
                headers: {
                    'Content-type': 'text/plain'
                }
            });
        } catch (err) {
            console.log(err.message);
            console.log(err.stack);
        }
        try {
            let r = await response.text();
            callback(r);
        } catch (err) {
            console.log(err.message);
            console.log(err.stack);
        }
        return;

        // Feature detection
        if (!window.XMLHttpRequest)
            return;

        // Create new request
        let xhr = new XMLHttpRequest();

        // Setup callback
        xhr.onload = function() {
            if ( callback && typeof( callback ) === 'function' ) {
                callback( this.responseText );
            }
        }

        // Get the HTML
        xhr.open( 'GET', url );
 //       xhr.responseType = 'document';
        xhr.responseType = 'text';
        xhr.send();
    };

    /**
     * Loads a new HTML/JS page.  The new page will replace the body of the current page.
     * Also, the loaded code is processed for custom tags / components.
     *
     * @param {string} page path to the page to be loaded.
     * @param {string} tag optional ID of div to fill (if empty "body" tag is used)
     * @param {string} initialFocus optional, ID of control to set initial focus on
     */
    static loadPage(page, tag, initialFocus) {
        Utils.cleanup();
        if (typeof Kiss !== 'undefined' && typeof Kiss.RadioButtons !== 'undefined')
            Kiss.RadioButtons.resetGroups();
        const pg = page + '.html' + (Utils.controlCache ? '?ver=' + Utils.softwareVersion : '');
//        $.get(pg, function (text) {
        this.getHTML(pg, function (text) {
            if (tag)
                $('#' + tag).html(text);
            else
                $('body').html(text);
            Utils.rescan();  // does all the tag replacement
            getScript(page + '.js' + (Utils.controlCache ? '?ver=' + Utils.softwareVersion : ''), function () {
                if (initialFocus)
                    $$(initialFocus).focus();
            });
        });
    }

    /**
     * Is the user's computer connected to the Internet?
     *
     * @returns {boolean}
     */
    static isOnline() {
        //return false;          //  used to simulate off-line condition
        return navigator.onLine;
    }

    /**
     * Create a new UUID.
     *
     * @returns {string}
     */
    static uuid() {
        let uuid = '', i, random;
        for (i = 0; i < 32; i++) {
            random = Math.random() * 16 | 0;
            if (i === 8 || i === 12 || i === 16 || i === 20)
                uuid += '-';
            uuid += (i === 12 ? 4 : (i === 16 ? (random & 3 | 8) : random)).toString(16);
        }
        return uuid;
    }

    /**
     * This makes a window draggable.
     *
     * @param header jQuery object
     * @param content jQuery object
     */
    static makeDraggable(header, content) {

        function handle_mousedown(e)
        {
            const body = $('body');
            const drag = {};

            drag.pageX0 = e.pageX;
            drag.pageY0 = e.pageY;
            drag.elem = content;
            drag.offset0 = $(this).offset();

            function handle_dragging(e) {
                const left = drag.offset0.left + (e.pageX - drag.pageX0);
                const top = drag.offset0.top + (e.pageY - drag.pageY0);
                $(drag.elem)
                    .offset({top: top, left: left});
            }

            function handle_mouseup(e) {
                body
                    .off('mousemove')
                    .off('mouseup');
            }

            body.on('mouseup', handle_mouseup)
                .on('mousemove', handle_dragging);
        }

        // for mobile devices
        function handle_touchstart(e)
        {
            const body = $('body');
            const drag = {};

            e.preventDefault();

            drag.pageX0 = e.originalEvent.touches[0].clientX;
            drag.pageY0 = e.originalEvent.touches[0].clientY;

            drag.elem = content;
            drag.offset0 = $(this).offset();

            function handle_dragging(e) {
                const left = drag.offset0.left + (e.originalEvent.touches[0].clientX - drag.pageX0);
                const top = drag.offset0.top + (e.originalEvent.touches[0].clientY - drag.pageY0);
                $(drag.elem)
                    .offset({top: top, left: left});
            }

            function handle_mouseup(e) {
                body
                    .off('touchmove')
                    .off('touchend');
            }

            body
                .on('touchmove', handle_dragging)
                .on('touchend', handle_mouseup);
        }
        header.css('cursor', 'all-scroll');
        header.on('mousedown', handle_mousedown);
        header.on('touchstart', handle_touchstart);
    }

    /**
     * Open a modal popup window identified by id <code>id</code>.
     *
     * @param {string} id the id of the popup to evoke
     * @param {string} focus_ctl optional control to set initial focus
     *
     * @see popup_close
     */
    static popup_open(id, focus_ctl) {
        const w = $('#' + id);
        let content;
        let both_parts;
        let header;
        let body;

        Utils.popup_context.push({
            id: id,
            globalEnterHandler: Utils.globalEnterHandler(null)
        });
        Utils.newGridContext();
        if (!w.hasClass('popup-background')) {
            let width = w.css('width');
            let height = w.css('height');
            w.addClass('popup-background');
            w.css('z-index', Utils.popup_zindex++);
            w.css('width', '100%');
            w.css('height', '100%');
            w.wrapInner('<div></div>');
            content = w.children();
            content.addClass('popup-content');
            content.css('z-index', Utils.popup_zindex++);
            content.attr('id', id + '--width');

            both_parts = content.children();
            header = both_parts.first();
            header.addClass('popup-header');
            body = header.next();
            body.addClass('popup-body');
            body.attr('id', id + '--height');
            content.css('width', width);
            body.css('height', height);
        } else {
            content = w.children();
            both_parts = content.children();
            header = both_parts.first();
            body = header.next();
        }

        w.show();

        this.makeDraggable(header, content);

        if (focus_ctl)
            $('#' + focus_ctl).focus();
        else {
            const ctl = $(':focus');
            if (ctl)
                ctl.blur();
        }
    }

    /**
     * Dynamically change the height of a popup.
     *
     * @param {string} id popup id
     * @param {string} height  like "200px"
     */
    static popup_set_height(id, height) {
        $('#' + id + '--height').css('height', height);
    }

    /**
     * Dynamically change the width of a popup.
     *
     * @param {string} id popup id
     * @param {string} height  like "200px"
     */
    static popup_set_width(id, width) {
        $('#' + id + '--width').css('width', width);
    }

    /**
     * Close the most recent modal popup.
     *
     * @see popup_open
     */
    static popup_close() {
        const context = Utils.popup_context.pop();
        Utils.popGridContext();
        $('#' + context.id).hide();
        Utils.globalEnterHandler(context.globalEnterHandler);
        Utils.popup_zindex -= 2;
        if (Utils.popup_zindex < 10)
            Utils.popup_zindex = 10;
    }

    /**
     * Return the number of files the user selected for upload.
     *
     * @param {string} id the id of the control
     * @returns {number}
     *
     * @see Server.fileUploadSend
     * @see getFileUploadFormData
     */
    static getFileUploadCount(id) {
        const ctl = $('#'+id);
        const file_list = ctl.get(0).files;
        return file_list.length;
    }

    /**
     * Creates and initializes a FormData instance used for file uploading.
     * Before the FormData instance is sent, arbitrary additional data may be added
     * by using <code>fd.append(name, data)</code>
     * <br><br>
     * where <code>fd</code> is the object return by this call.
     *
     * @param {string} id the id of the control
     * @returns {FormData}
     *
     * @see Server.fileUploadSend
     * @see getFileUploadCount
     */
    static getFileUploadFormData(id) {
        const ctl = $('#'+id);
        const file_list = ctl.get(0).files;
        const data = new FormData();
        for (let i=0 ; i < file_list.length ; i++)
            data.append('file-'+i, file_list[i]);
        return data;
    }

    /**
     * Displays a report given a URL from the back-end.
     * Correctly handles dual server development situations.
     *
     * @param url {string} report url
     */
    static showReport(url) {
        let path;
        if (url.charAt(0) !== '/')
            url = '/' + url;
        if (window.location.href.search("localhost:8000") !== -1 || window.location.href.search("localhost:8001") !== -1 || window.location.href.search("localhost:8002") !== -1) // if debugging with a local server
            path = "http://localhost:8080" + url;
        else {
            const server = Server.url;
            let ns = 0;  //  number of slashes
            let ts = 0;  //  index of third slash
            for (let i=0 ; i < server.length ; i++)
                if (server.charAt(i) === '/')
                    if (++ns === 3) {
                        ts = i;
                        break;
                    }
            path = ts ? server.substr(ts) + url : url;
        }
        window.open(path, "_blank");
    }

    //--------------------------

    /**
     * Returns true if the key hit is the kind of keyboard character that changes the value.
     * (Textarea will need to check 'Enter' too.)
     *
     * @param event
     * @returns {boolean}
     */
    static isChangeChar(event) {
        if (!event || !event.key)
            return false;
        return !event.ctrlKey && (event.key.length === 1 || event.key === 'Backspace' || event.key === 'Delete');
    }

    /**
     * This function is called to indicate to the system that <em>some</em> control value changed by the user.
     * Programmatic changed to not set this.
     * It is mainly used internally as all the Kiss controls use this function.
     * <br><br>
     * If there is a function connected to some-control-value-changing via the setSomeControlValueChangeFunction
     * function, it will be executed the <em>first time only</em> that any control value is changed.
     * Subsequent changes will not trigger the some-control-value-changing function.
     * The some-control-value-changing function is passed the single argument <code>true</code>.
     * <br><br>
     * Non-Kiss controls should call this function if their value changes.
     *
     */
    static someControlValueChanged() {
        if (!Utils.someControlValueChangedFlag) {
            Utils.someControlValueChangedFlag = true;
            if (Utils.someControlValueChangedFun)
                Utils.someControlValueChangedFun(Utils.someControlValueChangedFlag);
        }
    }

    /**
     * This function clears the state of some control value having been changed.
     * <br><br>
     * Also, if a function is attached (via the setSomeControlValueChangeFunction function),
     * it will be executed if some value had changed and a <code>true</code> argument is passed
     * to this function.  The some-control-value-changed function will be passed a <code>false</code>.
     *
     * @param executeFunction optional boolean
     */
    static clearSomeControlValueChanged(executeFunction = true) {
        if (Utils.someControlValueChangedFlag) {
            Utils.someControlValueChangedFlag = false;
            if (Utils.someControlValueChangedFun  &&  executeFunction)
                Utils.someControlValueChangedFun(Utils.someControlValueChangedFlag);
        }
    }

    /**
     *
     * @returns {boolean} true if any control value changed, false otherwise
     */
    static didSomeControlValueChange() {
        return Utils.someControlValueChangedFlag;
    }

    /**
     * Used for setting an application specific function to be executed the first
     * time any control value is changed by the user or if the change status is cleared.
     * Other programmatic changes do not trigger this condition.
     * <br><br>
     * A <code>null</code> value may be passed in order to clear the function.
     * <br><br>
     * The function is passed a single boolean value.  <code>true</code> means
     * a control value has changed, and <code>false</code> if the state is
     * being reset.
     *
     * @param fun {function}  the function to be executed
     */
    static setSomeControlValueChangeFunction(fun) {
        Utils.someControlValueChangedFun = fun;
    }

    /**
     * Count properties associated with an object.
     *
     * @param obj
     * @returns {number}
     */
    static countProperties(obj) {
        return Object.keys(obj).length;
    }

    /**
     * Perform a shallow clone on an array.
     *
     * @param ary
     * @returns {Array}  shallow copy of ary
     */
    static cloneArrayShallow(ary) {
        return [...ary];
    }

    /**
     * Set a global handler for the enter/return key.
     * If fun is null, the enter function is cancelled.
     *
     * @param {function} fun the new enter function handler or null
     * @return {function} the previous enter function or null if none
     */
    static globalEnterHandler(fun) {
        const prevFun = Utils.globalEnterFunction;
        Utils.globalEnterFunction = fun;
        const obj = $('body');
        obj.off('keyup');
        if (fun)
            obj.on('keyup', function (e) {
                if (e.key === 'Enter')
                    fun();
            });
        return prevFun;
    }

    /**
     * Perform all cleanup operations between screens
     */
    static cleanup() {
        Utils.clearSomeControlValueChanged(false);
        Kiss.RadioButtons.resetGroups();
        Utils.popAllGridContexts();
        Utils.newGridContext();   //  for the new screen we are loading
        Utils.enterFunction = null;
        Utils.globalEnterHandler(null);
        Utils.popup_context = [];
        const ctl = $(':focus');   // remove any focus
        if (ctl)
            ctl.blur();
    }

    /**
     * Create a new grid context.
     */
    static newGridContext() {
        Utils.gridContext.push([]);
        Utils.enterFunctionStack.push(Utils.enterFunction);
        Utils.enterFunction = null;
    };

    /**
     * Add a grid to the current context.
     *
     * @param grid
     */
    static addGrid(grid) {
        const cc = Utils.gridContext[Utils.gridContext.length - 1];
        cc.push(grid);
    };

    /**
     * Destroy all grids in last context and remove the context
     */
    static popGridContext() {
        const c = Utils.gridContext.pop();
        if (c) {
            for (let i = 0; i < c.length; i++)
                c[i].destroy();
            Utils.enterFunction = Utils.enterFunctionStack.pop();
        }
    };

    /**
     * destroys all popup and screen grids that have been created
     */
    static popAllGridContexts() {
        while (Utils.gridContext.length)
            Utils.popGridContext();
        // Not necessary but just in case
        Utils.enterFunction = null;
        Utils.enterFunctionStack = [];
    };

    /**
     * Function to execute if the user hits the enter key
     *
     * @param fun
     */
    static setEnterFunction(fun) {
        Utils.enterFunction = fun;
    }

    /**
     * Converts a text string into a string suitable to HTML.
     *
     * @param text
     * @returns {string}
     */
    static textToHtml(text) {
        return text
            .replace(/&/g, "&amp;")
            .replace(/</g, "&lt;")
            .replace(/>/g, "&gt;")
            .replace(/"/g, "&quot;")
            .replace(/'/g, "&#039;")
            .replace(/\n/g, '<br>')
            .replace(/ /g, '&nbsp;');
    }

    /**
     * Convert an HTML string into a text string.
     *
     * @param html
     * @returns {string}
     */
    static htmlToText(html) {
        if (!html)
            return '';
        return html
            .replace(/<br>/g, '\n')
            .replace(/&sp;/g, ' ')
            .replace(/&nbsp;/g, ' ')
            .replace(/&amp;/g, '&')
            .replace(/&lt;/g, '<')
            .replace(/&gt;/g, '>')
            .replace(/&quot;/g, '"')
            .replace(/<div[^>]*>/g, '\n')
            .replace(/<\/div>/g, '')
            .replace(/<span[^>]*>/g, '')
            .replace(/<\/span>/g, '')
            .replace(/<a>/g, '')
            .replace(/<[ap] +[^>]*>/g, '')
            .replace(/<\/[ap]>/g, '')
            ;
    }

    /**
     * Append html (as text) to the list of children of a node.
     *<br><br>
     * <code>tag</code> can be the id of the control or its jQuery node.
     * This function returns the jQuery node that can be used
     * for additional calls to this function (so it'll be faster).
     *
     * @param tag {object|string} see above
     * @param html {string} the html to be
     * @returns {object} the jQuery node
     */
    static appendChild(tag, html) {
        const node = typeof tag === 'string' ? $('#' + tag) : tag;
        if (!node.length) {
            console.log('tag ' + tag +' not found.');
            return;
        }
        node.append(html);
        return node;
    }

}

// Class variables
Utils.count = 1;
Utils.popup_zindex = 10;
Utils.popup_context = [];
Utils.someControlValueChangedFlag = false;
Utils.someControlValueChangedFun = null;
Utils.globalEnterFunction = null;


Utils.gridContext = [];              //  An array of arrays.  The outer array represents a stack of contexts
                                     //  The inner array is an array of grids that'll need to be disposed
                                     //  Basically, each context (except the first) represents a popup
                                     //  The first represents the current screen
                                     //  Each inner array contains an array of grids on that context

Utils.enterFunction = null;          //  If defined, execute function when enter key hit
Utils.enterFunctionStack = [];       //  Save stack for enter key to handle popups


$(document).on('keypress', function(e) {
    if (Utils.enterFunction  &&  e.key === 'Enter')
        Utils.enterFunction();
});



// taken from https://github.com/accursoft/caret/blob/master/jquery.caret.js
(function($) {
    function focus(target) {
        if (!document.activeElement || document.activeElement !== target) {
            target.focus();
        }
    }

    $.fn.caret = function(pos) {
        let target = this[0];
        let isContentEditable = target && target.contentEditable === 'true';
        if (arguments.length === 0) {
            //get
            if (target) {
                //HTML5
                if (window.getSelection) {
                    //contenteditable
                    if (isContentEditable) {
                        focus(target);
                        let selection = window.getSelection();
                        // Opera 12 check
                        if (!selection.rangeCount) {
                            return 0;
                        }
                        let range1 = selection.getRangeAt(0),
                            range2 = range1.cloneRange();
                        range2.selectNodeContents(target);
                        range2.setEnd(range1.endContainer, range1.endOffset);
                        return range2.toString().length;
                    }
                    //textarea
                    return target.selectionStart;
                }
                //IE<9
                if (document.selection) {
                    focus(target);
                    //contenteditable
                    if (isContentEditable) {
                        let range1 = document.selection.createRange(),
                            range2 = document.body.createTextRange();
                        range2.moveToElementText(target);
                        range2.setEndPoint('EndToEnd', range1);
                        return range2.text.length;
                    }
                    //textarea
                    let pos = 0,
                        range = target.createTextRange(),
                        range2 = document.selection.createRange().duplicate(),
                        bookmark = range2.getBookmark();
                    range.moveToBookmark(bookmark);
                    while (range.moveStart('character', -1) !== 0) pos++;
                    return pos;
                }
                // Addition for jsdom support
                if (target.selectionStart)
                    return target.selectionStart;
            }
            //not supported
            return;
        }
        //set
        if (target) {
            if (pos === -1)
                pos = this[isContentEditable? 'text' : 'val']().length;
            //HTML5
            if (window.getSelection) {
                //contenteditable
                if (isContentEditable) {
                    focus(target);
                    window.getSelection().collapse(target.firstChild, pos);
                }
                //textarea
                else
                    target.setSelectionRange(pos, pos);
            }
            //IE<9
            else if (document.body.createTextRange) {
                if (isContentEditable) {
                    let range = document.body.createTextRange();
                    range.moveToElementText(target);
                    range.moveStart('character', pos);
                    range.collapse(true);
                    range.select();
                } else {
                    let range = target.createTextRange();
                    range.move('character', pos);
                    range.select();
                }
            }
            if (!isContentEditable)
                focus(target);
        }
        return this;
    }
})(jQuery);
