/**
 * Created by Blake McBride on 1/20/16.
 */

'use strict';


var Component = {};

Component.ComponentsBeingLoaded = 0;

Component.ComponentList = [];

var Kiss = {};

/**
 * This is how components are accessed.
 *
 * @param {string} id  id of the component
 * @returns {*} the component object
 */
function $$(id) {
    if (typeof Kiss !== 'undefined' && typeof Kiss.RadioButtons !== 'undefined' && Kiss.RadioButtons.groups[id]) {
        var rbobj = {};
        rbobj.getValue = function () {
            return Kiss.RadioButtons.getValue(id);
        };
        rbobj.setValue = function (val) {
            Kiss.RadioButtons.setValue(id, val);
        };
        rbobj.onChange = function (fun) {
            Kiss.RadioButtons.onChange(id, fun);
        };
        rbobj.isError = function (lbl) {
            return Kiss.RadioButtons.isError(id, lbl);
        };
        return rbobj;
    }
    var e = $((id.charAt(0) === '#' ? '' : '#') + id);
    return e.length ? e[0].kiss : null;
}

/**
 * General utilities class
 */
class utils {

    /**
     * Display a popup window with a message to the user.  The user will click "Ok" when they have read the message.
     * If the title is 'Error' the popup will appear in red.
     *
     * @param {string} title appears on the title bar of the message window
     * @param {string} message the message to be displayed
     * @param {function} afterFun an optional function to execute when the user clicks 'Ok'
     */
    static showMessage(title, message, afterFun) {
        if (!$('#msg-modal').length) {
            $('body').append(
                '<div id="msg-modal" class="msg-modal">' +
                '  <!-- Modal content -->' +
                '  <div class="msg-modal-content">' +
                '    <div class="msg-modal-header" id="msg-modal-header-tab">' +
                '      <span id="msg-close-btn" class="msg-close">&times;</span>' +
                '      <p id="msg-header" style="margin-top: 2px;">Modal Header</p>' +
                '    </div>' +
                '    <div class="msg-modal-body">' +
                '      <p id="msg-message" style="margin-top: 5px, margin-bottom: 5px;"></p>' +
                '    </div>' +
                '    <div class="msg-modal-footer">' +
                '      <input type="button" value="Ok" id="message-ok" style="margin-top: 5px; margin-bottom: 10px;"">' +
                '    </div>' +
                '  </div>' +
                '</div>');
        }

        $('#msg-header').text(title);
        $('#msg-message').text(message);
        var modal = $('#msg-modal');
        var span = $('#msg-close-btn');
        if (title === 'Error')
            $('#msg-modal-header-tab').css('background-color', 'red');
        else
            $('#msg-modal-header-tab').css('background-color', '#6495ed');
        modal.show();
        var endfun = function () {
            modal.hide();
            if (afterFun)
                afterFun();
        };
        span.unbind('click').click(function () {
            endfun();
        });
        $('#message-ok').unbind('click').click(function () {
            endfun();
        });
    }

    /**
     * Display a modal popup that asks the user a yes/no question.
     *
     * @param {string} title text on the title bar of the popup
     * @param {string} message the question being asked
     * @param {function} yesFun function that gets executed if the user clicks 'Yes'
     * @param {function} noFun function that gets executed if the user clicks 'No'
     */
    static yesNo(title, message, yesFun, noFun) {
        if (!$('#yesno-modal').length) {
            $('body').append(
                '<div id="yesno-modal" class="msg-modal">' +
                '  <!-- Modal content -->' +
                '  <div class="msg-modal-content">' +
                '    <div class="msg-modal-header">' +
                '      <span id="yesno-close-btn" class="msg-close">&times;</span>' +
                '      <p id="yesno-header" style="margin-top: 2px;">Modal Header</p>' +
                '    </div>' +
                '    <div class="msg-modal-body">' +
                '      <p id="yesno-message" style="margin-top: 5px, margin-bottom: 5px;"></p>' +
                '    </div>' +
                '    <div class="msg-modal-footer">' +
                '      <input type="button" value="Yes" id="yesno-yes" style="margin-top: 5px; margin-bottom: 10px;"">' +
                '      <input type="button" value="No" id="yesno-no" style="margin-top: 5px; margin-bottom: 10px;"">' +
                '    </div>' +
                '  </div>' +
                '</div>');
        }

        $('#yesno-header').text(title);
        $('#yesno-message').text(message);
        var modal = $('#yesno-modal');
        var span = $('#yesno-close-btn');
        modal.show();
        span.unbind('click').click(function () {
            modal.hide();
            if (noFun)
                noFun();
        });
        $('#yesno-yes').unbind('click').click(function () {
            modal.hide();
            if (yesFun)
                yesFun();
        });
        $('#yesno-no').unbind('click').click(function () {
            modal.hide();
            if (noFun)
                noFun();
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
                '  <div class="wmsg-modal-content">' +
                '    <div class="msg-modal-body">' +
                '      <p id="wmsg-message" style="margin-top: 5px, margin-bottom: 5px;"></p>' +
                '    </div>' +
                '  </div>' +
                '</div>');
        }

        $('#wmsg-message').text(message);
        var modal = $('#wmsg-modal');
        modal.show();
    }

    /**
     * This terminates the wait message initiated by waitMessage()
     */
    static waitMessageEnd() {
        $('#wmsg-modal').hide();
    }

    static getID(id) {
        var e = $('#' + id);
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
        var zero = places - num.toString().length + 1;
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
                return utils.drop(s, s.length - n);
            var sb = '';
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
     *    var r = utils.formatb(-12345.348, 10, "CP", 12, 2);
     *<br><br>
     *    result in r:   "(12,345.35)"
     * </p>
     *
     * @see format
     */
    static formatb(num, base, msk, wth, dp) {
        var si, i, r, n;
        var sign, blnk, comma, left, paren, zfill, nd, dol, tw, dl, ez, ucase, cf, percent;
        var dbase;
        var alpha = '0123456789abcdefghijklmnopqrstuvwxyz';

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
                dl = num < 1.0 ? 0 : 1 + Math.floor(Math.log(num) / Math.LN10);
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
            for (var i2 = 0; i2 < msk.length; i2++)
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
                var tbuf = '';
                for (i = 0; i < wth; i++)
                    tbuf += '*';
                return tbuf;
            }
        }
        var buf = new Array(wth);
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
        if (percent)
            buf[--si] = '%';

        if (paren)
            buf[--si] = sign ? ')' : ' ';

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
        if (sign)
            if (si)
                buf[--si] = paren ? '(' : '-';
            else
                num = 1.0;
        /*  signal error condition */

        if (dol && si)
            buf[--si] = '$';

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
     *    var r = utils.format(-12345.348, "CP", 12, 2);
     *<br><br>
     *    result in r:  "(12,345.35)"
     * </p>
     */
    static format(num, msk, wth, dp) {
        return utils.formatb(num, 10, msk, wth, dp);
    }

    /**
     * Get the extension of the file name.  So, "abc.def" would return "def".
     *
     * @param {string}filename
     * @returns {string}
     */
    static fileNameExtension(filename) {
        var fileSplit = filename.split('.');
        var fileExt = '';
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
        var r = [];
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
        var url = window.location.href;
        var args = url.split('?');
        if (args.length !== 2)
            return null;
        var pairs = args[1].split('&');
        if (pairs.length === 0)
            return null;
        var i;
        for (i = 0; i < pairs.length; i++) {
            var keyval = pairs[i].split('=');
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

        var npath;

        var loadScript = function (arg) {
            $.getScript(npath + '.js' + arg, function (data, textStatus, jqxhr) {

            });
        };

        var loadComponent = function (arg) {
            npath = path.charAt(0).toLowerCase() + path.substr(1) + '/' + path.charAt(0).toUpperCase() + path.substr(1);
            npath = '/kiss/component/' + npath;
            loadScript(arg);
        };
        loadComponent(utils.controlCache ? '?ver=' + utils.softwareVersion : '');
    }

    // internal
    static tagReplace(inp, rpl) {
        for (var prop in rpl) {
            var val = rpl[prop];
            if (!val && val !== 0)
                val = '';
            inp = inp.replace('{' + prop + '}', val);
        }
        return inp;
    }

    // internal
    static afterComponentsLoaded(fun) {
        Component.AfterAllComponentsLoaded = fun;
    }

    // internal
    static rescan() {
        var n = -1;
        while (n) {  // keep replacing until nothing left to replace
            n = 0;
            for (var i = 0; i < Component.ComponentList.length; i++) {
                var ci = Component.ComponentList[i];
                if (ci.tag)
                    $(ci.tag).each(function () {
                        var elm = $(this);
                        ci.processor(elm, utils.getAllAttributes(elm), elm.html());
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

        var npath;

        Component.ComponentsBeingLoaded++;

        var loadScript = function (arg) {
            $.getScript(npath + '.js' + arg, function () {
                if (!--Component.ComponentsBeingLoaded && Component.AfterAllComponentsLoaded) {
                    utils.rescan();  // does all the tag replacement
                    Component.AfterAllComponentsLoaded();
                    Component.AfterAllComponentsLoaded = null;
                }
            });
        };

        var loadComponent = function (arg) {
            npath = path.charAt(0).toLowerCase() + path.substr(1) + '/' + path.charAt(0).toUpperCase() + path.substr(1);
            npath = 'kiss/component/' + npath;
            loadScript(arg);
        };

        loadComponent(utils.controlCache ? '?ver=' + utils.softwareVersion : '');
    }

    static getAllAttributes(elm) {
        var ret = {};
        $.each(elm[0].attributes, function () {
            ret[this.name] = this.value;
        });
        return ret;
    }

    static removeQuotes(s) {
        if (!s || !s.length)
            return '';
        var c = s.charAt(0);
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
        var snd = new Audio("data:audio/wav;base64,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qJrx4KOQjahZxWbcZgztj2c49nKmkId44S71j0c8eV9yDK6uPRzx5X18eDvjvQ6yKo9ZSS6l//8elePK/Lf//IInrOF/FvDoADYAGBMGb7FtErm5MXMlmPAJQVgWta7Zx2go+8xJ0UiCb8LHHdftWyLJE0QIAIsI+UbXu67dZMjmgDGCGl1H+vpF4NSDckSIkk7Vd+sxEhBQMRU8j/12UIRhzSaUdQ+rQU5kGeFxm+hb1oh6pWWmv3uvmReDl0UnvtapVaIzo1jZbf/pD6ElLqSX+rUmOQNpJFa/r+sa4e/pBlAABoAAAAA3CUgShLdGIxsY7AUABPRrgCABdDuQ5GC7DqPQCgbbJUAoRSUj+NIEig0YfyWUho1VBBBA//uQZB4ABZx5zfMakeAAAAmwAAAAF5F3P0w9GtAAACfAAAAAwLhMDmAYWMgVEG1U0FIGCBgXBXAtfMH10000EEEEEECUBYln03TTTdNBDZopopYvrTTdNa325mImNg3TTPV9q3pmY0xoO6bv3r00y+IDGid/9aaaZTGMuj9mpu9Mpio1dXrr5HERTZSmqU36A3CumzN/9Robv/Xx4v9ijkSRSNLQhAWumap82WRSBUqXStV/YcS+XVLnSS+WLDroqArFkMEsAS+eWmrUzrO0oEmE40RlMZ5+ODIkAyKAGUwZ3mVKmcamcJnMW26MRPgUw6j+LkhyHGVGYjSUUKNpuJUQoOIAyDvEyG8S5yfK6dhZc0Tx1KI/gviKL6qvvFs1+bWtaz58uUNnryq6kt5RzOCkPWlVqVX2a/EEBUdU1KrXLf40GoiiFXK///qpoiDXrOgqDR38JB0bw7SoL+ZB9o1RCkQjQ2CBYZKd/+VJxZRRZlqSkKiws0WFxUyCwsKiMy7hUVFhIaCrNQsKkTIsLivwKKigsj8XYlwt/WKi2N4d//uQRCSAAjURNIHpMZBGYiaQPSYyAAABLAAAAAAAACWAAAAApUF/Mg+0aohSIRobBAsMlO//Kk4soosy1JSFRYWaLC4qZBYWFRGZdwqKiwkNBVmoWFSJkWFxX4FFRQWR+LsS4W/rFRb/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////VEFHAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAU291bmRib3kuZGUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMjAwNGh0dHA6Ly93d3cuc291bmRib3kuZGUAAAAAAAAAACU=");
        snd.play();
    }

    static nextID() {
        return "ID-" + utils.count++;
    }

    static replaceHTML(id, elm, template, rplobj) {
        if (!id)
            id = utils.nextID();
        rplobj.id = id;
        var newHTML = utils.tagReplace(template, rplobj);
        elm.replaceWith(newHTML);
        var jqObj = $('#' + id);
        var newElm = jqObj[0];
        newElm.kiss = {};
        newElm.kiss.jqObj = jqObj;
        newElm.kiss.elementInfo = {};
        return newElm.kiss;
    }

    /**
     * Loads a new HTML/JS page.  The new page will replace the body of the current page.
     * Also, the loaded code is processed for custom tags / components.
     *
     * @param {string} page path to the page to be loaded.
     * @param {string} tag optional ID of div to fill (if empty "body" tag is used)
     */
    static loadPage(page, tag) {
        if (typeof Kiss !== 'undefined' && typeof Kiss.RadioButtons !== 'undefined')
            Kiss.RadioButtons.resetGroups();
        $.get(page + '.html' + (utils.controlCache ? '?ver=' + utils.softwareVersion : ''), function (text) {
            if (tag)
                $('#' + tag).html(text);
            else
                $('body').html(text);
            utils.rescan();  // does all the tag replacement
            $.getScript(page + '.js' + (utils.controlCache ? '?ver=' + utils.softwareVersion : ''), function () {
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
        var uuid = '', i, random;
        for (i = 0; i < 32; i++) {
            random = Math.random() * 16 | 0;
            if (i === 8 || i === 12 || i === 16 || i === 20)
                uuid += '-';
            uuid += (i === 12 ? 4 : (i === 16 ? (random & 3 | 8) : random)).toString(16);
        }
        return uuid;
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
        let w = $('#' + id);
        let content;
        let both_parts;
        let header;
        let body;
        let drag = {};

        function handle_mousedown(e){
            let body = $('body');
            drag.pageX0 = e.pageX;
            drag.pageY0 = e.pageY;
            drag.elem = content;
            drag.offset0 = $(this).offset();
            function handle_dragging(e){
                let left = drag.offset0.left + (e.pageX - drag.pageX0);
                let top = drag.offset0.top + (e.pageY - drag.pageY0);
                $(drag.elem)
                    .offset({top: top, left: left});
            }
            function handle_mouseup(e){
                body
                    .off('mousemove', handle_dragging)
                    .off('mouseup', handle_mouseup);
            }
            body
                .on('mouseup', handle_mouseup)
                .on('mousemove', handle_dragging);
        }

        if (!w.hasClass('popup-modal')) {
            w.addClass('popup-modal');
            w.css('z-index', utils.popup_zindex++);

            let width = w.css('width');
            let height = w.css('height');
            w.css('width', '100%');
            w.css('height', '100%');

            w.wrapInner('<div class="popup-modal-content"></div>');
            content = w.children();

            // content.css('position', 'absolute');
            content.css('z-index', utils.popup_zindex++);
            content.css('top', '50%');
            content.css('left', '50%');
            content.css('transform', 'translate(-50%, -50%)');

            both_parts = content.children();
            header = both_parts.first();
            header.wrapInner('<div style="margin-top: 3px;"></div>div>');
            body = header.next();
            header.addClass('popup-modal-header');
            header.css('cursor', 'all-scroll');
            body.addClass('popup-modal-body');
            content.css('width', width);
            body.css('height', height);
        } else {
            content = w.children();
            both_parts = content.children();
            header = both_parts.first();
            body = header.next();
        }

        w.show();

        header.mousedown(handle_mousedown);

        if (focus_ctl)
            $('#' + focus_ctl).focus();
    }

    /**
     * Close a modal popup.
     *
     * @param {string} id the id of the popup to close
     *
     * @see popup_open
     */
    static popup_close(id) {
        $('#' + id).hide();
        utils.popup_zindex -= 2;
        if (utils.popup_zindex < 10)
            utils.popup_zindex = 10;

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
        let ctl = $('#'+id);
        let file_list = ctl.get(0).files;
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
        let ctl = $('#'+id);
        let file_list = ctl.get(0).files;
        let data = new FormData();
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
        if (window.location.href.search("localhost:8000") !== -1) // if debugging with a local server
            path = "http://localhost:8080" + url;
        else
            path = url;
        window.open(path, "_blank");
    }

}

utils.count = 1;

utils.popup_zindex = 10;


//# sourceURL=kiss/utils.js
