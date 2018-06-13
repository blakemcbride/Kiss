/**
 * Created by Blake McBride on 1/20/16.
 */

'use strict';

var utils = function () {};


utils.clear_all_input = function (form_id) {
    $.map($('#'+form_id).find('[id]'), function(elm) {
        $(elm).val('');
    });

};

// works on bootstrap
utils.set_delay_focus = function (dialog_id, input_id) {
    $('#'+dialog_id).on('shown.bs.modal', function () {
        $('#'+input_id).focus();
    });
};

utils.check_required = function (id, desc) {
    if (!$('#'+id).val()) {
        utils.showMessage('error', desc + ' is required.', function () {
            $('#'+id).focus();
        });
        return true;  //  bad
    }
    return false;  //  good
};

utils.check_member_num = function (id, arr, desc) {
    var val = $('#'+id).val();
    if (!val) {
        utils.showMessage('error', desc + ' ' + val+ ' is required.', function () {
            $('#'+id).focus();
        });
        return true;  //  bad
    }
    if (-1 === $.inArray(parseInt(val), arr)) {
        utils.showMessage('error', desc + ' ' + val+ ' is invalid.', function () {
            $('#'+id).focus();
        });
        return true;  //  bad
    }
    return false;  //  good
};

utils.copy_values_to_json = function (form, json) {
    $.map($('#'+form).find('[id]'), function(elm) {
        json[elm.id.replace(/-/g, '_')] = $(elm).val();
    });
};

utils.copy_values_to_form = function (form, json) {
    $.map($('#'+form).find('[id]'), function(elm) {
        var fname = elm.id.replace(/-/g, '_');
        if (json[fname] !== undefined)
            $(elm).val(json[fname]);
    });
};

// display Okay popup message
utils.showMessage = function(title, message, afterFun) {
    if (!$('#msg-modal').length) {
        $('body').append(
            '<div id="msg-modal" class="msg-modal">' +
            '  <!-- Modal content -->' +
            '  <div class="msg-modal-content">' +
            '    <div class="msg-modal-header">' +
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
};

// display Okay popup message
utils.YesNo = function(title, message, yesFun, noFun) {
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
};

utils.waitMessage = function(message) {
    if (!$('#wmsg-modal').length) {
        $('body').append(
            '<div id="wmsg-modal" class="msg-modal">' +
            '  <!-- Modal content -->' +
            '  <div class="msg-modal-content">' +
            '    <div class="msg-modal-body">' +
            '      <p id="wmsg-message" style="margin-top: 5px, margin-bottom: 5px;"></p>' +
            '    </div>' +
            '  </div>' +
            '</div>');
    }

    $('#wmsg-message').text(message);
    var modal = $('#wmsg-modal');
    modal.show();
};

utils.waitMessageEnd = function () {
    $('#wmsg-modal').hide();
};

utils.check_date = function (id, required, minDate, maxDate, desc, ignoreMissing) {
    id = utils.getID(id);
    if (ignoreMissing  &&  !id)
        return false;  //  ok
    var val = $('#'+id).val();
    if (val)
        val = $.trim(val);
    if (!val)
        if (!required)
            return false;  //  good
        else {
            utils.showMessage('error', desc + ' is required.', function () {
                $('#'+id).focus();
            });
            return true;  //  bad
        }
    var dt = dateutils.SQLtoInt(val);
    if (!dt) {
        utils.showMessage('error', desc + ' has an invalid date.  Use:  mm/dd/yy or mm/dd/yyyy', function () {
            $('#'+id).focus();
        });
        return true;  //  bad
    }
    if (dt < minDate  ||  dt > maxDate) {
        utils.showMessage('error', desc + ' must be between ' + dateutils.intToStr4(minDate) + ' and ' + dateutils.intToStr4(maxDate) + '.', function () {
            $('#'+id).focus();
        });
        return true;  //  bad
    }
    return false;  //  good
};

utils.check_dropdown = function (id, required, desc) {
    id = utils.getID(id);
    var val = $('#'+id).val();
    if (!val  &&  required) {
        utils.showMessage('error', desc + ' is required.', function () {
            $('#'+id).focus();
        });
        return true;  //  bad
    }
    return false;  //  good
};

utils.getID = function (id) {
    var e = $('#'+id);
    if (!e.length) {
        id = id.replace(/_/g, '-');
        e = $('#'+id);
    }
    if (!e.length) {
        id = id.replace(/-/g, '_');
        e = $('#'+id);
    }
    return e.length ? id : null;
};

utils.check_num = function (id, required, minVal, maxVal, minInc, desc, ignoreMissing) {
    id = utils.getID(id);
    if (ignoreMissing  &&  !id)
        return false;  //  ok
    var val = $('#'+id).val();
    if (val)
        val = $.trim(val).replace(/,/g, '');
    if (!val)
        if (!required)
            return false;  //  good
        else {
            utils.showMessage('error', desc + ' is required.', function () {
                $('#'+id).focus();
            });
            return true;  //  bad
        }
    var nval = Number(val);
    if (nval < minVal  ||  nval > maxVal) {
        utils.showMessage('error', desc + ' must be between ' + utils.format(minVal, 'C', -1, -1) + ' and ' + utils.format(maxVal, 'C', -1, -1) + '.', function () {
            $('#'+id).focus();
        });
        return true;  //  bad
    }
    var a = nval / minInc;
    var b = Math.floor(a);
    if (a-b) {
        utils.showMessage('error', desc + ' must be in increments of ' + utils.format(minInc, 'C', -1, -1), function () {
            $('#'+id).focus();
        });
        return true;  //  bad
    }
    return false;  //  good
};

utils.check_str = function (id, minLen, maxLen, desc, ignoreMissing) {
    id = utils.getID(id);
    if (ignoreMissing  &&  !id)
        return false;  //  ok
    var val = $('#'+id).val();
    if (val)
        val = $.trim(val).replace(/,/g, '');
    if (!val)
        if (!minLen)
            return false;  //  good
        else {
            utils.showMessage('error', desc + ' is required.', function () {
                $('#'+id).focus();
            });
            return true;  //  bad
        }
    if (val.length < minLen  ||  val.length > maxLen) {
        utils.showMessage('error', desc + ' must be between ' + minLen + ' and ' + maxLen + ' characters long.', function () {
            $('#'+id).focus();
        });
        return true;  //  bad
    }
    return false;  //  good
};

utils.check_fields = function (fieldInfo) {
    for (var i in fieldInfo) {
        var fi = fieldInfo[i];
        switch (fi.type) {
            case 'number':
                if (utils.check_num(fi.name, fi.required, fi.minValue, fi.maxValue, fi.minIncrement, fi.desc, true))
                    return true;  //  bad
                break;
            case 'string':
                if (utils.check_str(fi.name, fi.minLength, fi.maxLength, fi.desc, true))
                    return true;  //  bad
                break;
            case 'date':
                if (utils.check_date(fi.name, fi.required, fi.minValue, fi.maxValue, fi.desc, true))
                    return true;  //  bad
                break;
        }
    }
    return false;  // good
};

utils.zeroPad = function (num, places) {
    var zero = places - num.toString().length + 1;
    return Array(+(zero > 0 && zero)).join("0") + num;
};

/**
 * APL-like take for strings.
 *
 * @param s
 * @param n
 * @returns {*}
 */
utils.take = function (s, n) {
    if (s.length === n)
        return s;
    if (n >= 0) {
        if (n < s.length)
            return s.substring(0, n);
        for (n -= s.length ; n-- > 0 ; )
            s += ' ';
        return s;
    } else {
        n = -n;
        if (n < s.length)
            return utils.drop(s, s.length - n);
        var sb = '';
        for (n -= s.length ; n-- > 0 ; )
            sb += ' ';
        sb += s;
        return sb;
    }
};

/**
 * APL-like drop for strings.
 *
 * @param s
 * @param n
 * @returns {*}
 */
utils.drop = function (s, n) {
    if (!n)
        return s;
    if (n >= s.length  ||  -n >= s.length)
        return '';
    if (n > 0)
        return s.substring(n);
    return s.substring(0, s.length + n);
};


/**
 *  Numeric formatter.  Takes a number and converts it to a nicely formatted String in a specified number base.
 *
 * @param num    number to be formatted
 * @param base   numeric base (like base 2 = binary, 16=hex...)
 * @param msk    format mask - any combination of the following:<br>
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
 * @param  wth    total field width (0 means auto)
 * @param  dp    number of decimal places (-1 means auto)
 * @return string the formatted String
 *<p>
 * example:
 *
 *    var r = utils.formatb(-12345.348, 10, "CP", 12, 2);
 *
 *    result in r:   "(12,345.35)"
 * </p>
 */

utils.formatb = function (num, base, msk, wth, dp) {
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
            dl = num < 1.0 ? 0 : 1 + Math.floor(Math.log(num) / Math.LN10);    /* # of digits left of .  */
            break;
        case 2:
            cf = 4;
            dl = num < 1.0 ? 0 : 1 + Math.floor(Math.log(num) / .6931471806);  /* # of digits left of .  */
            break;
        case 8:
            cf = 3;
            dl = num < 1.0 ? 0 : 1 + Math.floor(Math.log(num) / 2.079441542);  /* # of digits left of .  */
            break;
        case 16:
            cf = 4;
            dl = num < 1.0 ? 0 : 1 + Math.floor(Math.log(num) / 2.772588722);  /* # of digits left of .  */
            break;
        default:
            cf = 3;
            dl = num < 1.0 ? 0 : 1 + Math.floor(Math.log(num) / Math.log(dbase));  /* # of digits left of .  */
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
            var  tbuf = '';
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
        for (i = wth - tw ; i-- ;)
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
        for (i = sign + dol ; si > i ;)
            buf[--si] = '0';
    if (sign)
        if (si)
            buf[--si] = paren ? '(' : '-';
        else
            num = 1.0; /*  signal error condition */

    if (dol && si)
        buf[--si] = '$';

    while (si)
        buf[--si] = ' ';

    if (num !== 0.0)         /*  should never happen. but just in case  */
        for (i = 0; i < wth;)
            buf[i++] = '*';

    return buf.join('');
};

/**
 *  Numeric formatter.  Takes a number and converts it to a nicely formatted String (for number in base 10).
 *
 * @param num    number to be formatted
 * @param msk    format mask - any combination of the following:<br>
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
 * @param  wth    total field width (0 means auto)
 * @param  dp    number of decimal places (-1 means auto)
 * @return string the formatted String
 *<p>
 * example:
 *
 *    var r = utils.format(-12345.348, "CP", 12, 2);
 *
 *    result in r:  "(12,345.35)"
 * </p>
 */

utils.format = function (num, msk, wth, dp) {
    return utils.formatb(num, 10, msk, wth, dp);
};

utils.dateCharLimit = function (ctl) {
    ctl.value = ctl.value.replace(/[^-0-9\/\\.]/g,'');
    if (ctl.value.length > 10)
        ctl.value = utils.take(ctl.value, 10);
};

utils.nextDate = function (ctlName, days) {
    var ctl = $('#'+ctlName);
    var val = ctl.val();
    if (!dateutils.isSQLDate(val))
        return null;
    var ival = dateutils.SQLtoInt(val);
    var dval = dateutils.intToDate(ival);
    dval = dateutils.dateAddDays(dval, days);
    ival = dateutils.dateToInt(dval);
    val = dateutils.intToSQL(ival);
    ctl.val(val);
};

utils.fileNameExtension = function (filename) {
    var fileSplit = filename.split('.');
    var fileExt = '';
    if (fileSplit.length > 1)
        fileExt = fileSplit[fileSplit.length - 1];
    return fileExt;
};

/**
 * When calling a SOAP web service, array returns are not arrays but single objects when there is only one element.
 * (XML to Json problem).  This function assures that an element is an array when one is expected.
 * @param x
 * @returns {*}
 */
utils.assureArray = function (x) {
    if ($.isArray(x))
        return x;
    var r = [];
    if (x)
        r[0] = x;
    return r;
};

utils.getURLParam = function (key) {
    var url = window.location.href;
    var args = url.split('?');
    if (args.length !== 2)
        return null;
    var pairs = args[1].split('&');
    if (pairs.length === 0)
        return null;
    var i;
    for (i=0 ; i < pairs.length ; i++) {
        var keyval = pairs[i].split('=');
        if (keyval[0] === key)
            if (keyval.length > 0)
                return decodeURI(keyval[1]);
            else
                return '';
    }
    return null;
};

/**
 *
 * @param val  string value
 * @param andp allowed number of decimal places or -1 if any
 * @returns {string}
 */
utils.numberinput = function (val, andp) {
    val = val.replace(/[^0-9\.,]/g,'');  // remove characters
    var ret = '';
    var ndp = 0;  // number of decimal points
    var ndr = 0;  // number of digits to the right of the decimal point
    for (var i=0 ; i < val.length ; i++) {
        var c = val.charAt(i);
        if (c === '.') {
            if (!ndp && andp !== 0) {
                ndp++;
                ret += c;
            }
        } else if (ndp) {
            if (ndr < andp  ||  andp < 0) {
                ndr++;
                ret += c;
            }
        } else
            ret += c;
    }
    return ret;
};

/**
 *
 * @param sval string value
 * @param ndp number of decimal places or -1 if any
 * @returns {string}
 */
utils.formatnumber = function (sval, ndp) {
    sval = sval.replace(/[^0-9\.]/g,'');  // remove commas and other characters
    if (!sval.length)
        return sval;
    var nval = Number(sval);
    return utils.format(nval, "C", 0, ndp);

};

/**
 * Take a string and return a number
 *
 * @param sval
 * @returns {number}
 */
utils.toNumber = function (sval) {
    if (!sval)
        return 0;
    sval = sval.replace(/[^0-9\.]/g,'');  // remove commas and other characters
    if (!sval.length)
        return 0;
    return Number(sval);
};

/**
 * Add an option to an HTML dropdown.
 *
 * @param id
 * @param val
 * @param txt
 */
utils.addToDropDown = function (id, val, txt) {
    $('#'+id).append($("<option></option>")
        .attr("value", val)
        .text(txt));
};

utils.useTaglessComponent = function (path, onentry, onexit, onload) {

    var npath;

    var loadScript = function (arg) {
        $.getScript(npath + '.js' + arg, function (data, textStatus, jqxhr) {
            if (onentry || onexit) {
                var code = 'Component.' + path.charAt(0).toUpperCase() + path.substr(1) + '.setup(onentry, onexit);';
                eval(code);
            }
            if (onload)
                onload();
        });
    };

    var loadComponent = function (arg) {
        npath = path.charAt(0).toLowerCase() + path.substr(1) + '/' + path.charAt(0).toUpperCase() + path.substr(1);
        npath = '/kiss/component/' + npath;
        loadScript(arg);
    };
    loadComponent(utils.controlCache ? '?ver=' + utils.softwareVersion : '');
};

utils.tagReplace = function (inp, rpl) {
    for (var prop in rpl) {
        var val = rpl[prop];
        if (!val  &&  val !== 0)
            val = '';
        inp = inp.replace('{' + prop + '}', val);
    }
    return inp;
};


var Component = {};

Component.ComponentsBeingLoaded = 0;

Component.ComponentList = [];

utils.afterComponentsLoaded = function (fun) {
    Component.AfterAllComponentsLoaded = fun;
};

utils.rescan = function () {
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
};

utils.useComponent = function (path) {

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
};

utils.getAllAttributes = function (elm) {
    var ret = {};
    $.each(elm[0].attributes, function () {
        ret[this.name] = this.value;
    });
    return ret;
};

utils.removeQuotes = function (s) {
    if (!s  ||  !s.length)
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
};

utils.newComponent = function (ci) {
    Component.ComponentList.push(ci);
    Component[ci.name] = {};
};

utils.beep = function () {
    var snd = new Audio("data:audio/wav;base64,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qJrx4KOQjahZxWbcZgztj2c49nKmkId44S71j0c8eV9yDK6uPRzx5X18eDvjvQ6yKo9ZSS6l//8elePK/Lf//IInrOF/FvDoADYAGBMGb7FtErm5MXMlmPAJQVgWta7Zx2go+8xJ0UiCb8LHHdftWyLJE0QIAIsI+UbXu67dZMjmgDGCGl1H+vpF4NSDckSIkk7Vd+sxEhBQMRU8j/12UIRhzSaUdQ+rQU5kGeFxm+hb1oh6pWWmv3uvmReDl0UnvtapVaIzo1jZbf/pD6ElLqSX+rUmOQNpJFa/r+sa4e/pBlAABoAAAAA3CUgShLdGIxsY7AUABPRrgCABdDuQ5GC7DqPQCgbbJUAoRSUj+NIEig0YfyWUho1VBBBA//uQZB4ABZx5zfMakeAAAAmwAAAAF5F3P0w9GtAAACfAAAAAwLhMDmAYWMgVEG1U0FIGCBgXBXAtfMH10000EEEEEECUBYln03TTTdNBDZopopYvrTTdNa325mImNg3TTPV9q3pmY0xoO6bv3r00y+IDGid/9aaaZTGMuj9mpu9Mpio1dXrr5HERTZSmqU36A3CumzN/9Robv/Xx4v9ijkSRSNLQhAWumap82WRSBUqXStV/YcS+XVLnSS+WLDroqArFkMEsAS+eWmrUzrO0oEmE40RlMZ5+ODIkAyKAGUwZ3mVKmcamcJnMW26MRPgUw6j+LkhyHGVGYjSUUKNpuJUQoOIAyDvEyG8S5yfK6dhZc0Tx1KI/gviKL6qvvFs1+bWtaz58uUNnryq6kt5RzOCkPWlVqVX2a/EEBUdU1KrXLf40GoiiFXK///qpoiDXrOgqDR38JB0bw7SoL+ZB9o1RCkQjQ2CBYZKd/+VJxZRRZlqSkKiws0WFxUyCwsKiMy7hUVFhIaCrNQsKkTIsLivwKKigsj8XYlwt/WKi2N4d//uQRCSAAjURNIHpMZBGYiaQPSYyAAABLAAAAAAAACWAAAAApUF/Mg+0aohSIRobBAsMlO//Kk4soosy1JSFRYWaLC4qZBYWFRGZdwqKiwkNBVmoWFSJkWFxX4FFRQWR+LsS4W/rFRb/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////VEFHAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAU291bmRib3kuZGUAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAMjAwNGh0dHA6Ly93d3cuc291bmRib3kuZGUAAAAAAAAAACU=");
    snd.play();
};

if (!utils.count)
    utils.count = 1;

utils.nextID = function () {
    return "ID-" + utils.count++;
};

var Kiss = {};

var $$ = function (id) {
    if (typeof Kiss !== 'undefined'  &&  typeof Kiss.RadioButtons !== 'undefined' && Kiss.RadioButtons.groups[id]) {
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
        return rbobj;
    }
    var e = $((id.charAt(0) === '#'?'':'#')+id);
    return e.length ? e[0].kiss : null;
};

utils.replaceHTML = function (id, elm, template, rplobj) {
    if (!id)
        id = utils.nextID();
    rplobj.id = id;
    var newHTML = utils.tagReplace(template, rplobj);
    elm.replaceWith(newHTML);
    var jqObj = $('#'+id);
    var newElm = jqObj[0];
    newElm.kiss = {};
    newElm.kiss.jqObj = jqObj;
    newElm.kiss.elementInfo = {};
    return newElm.kiss;
};

utils.loadPage = function (page) {
    $.get(page + '.html' + (utils.controlCache ? '?ver=' + utils.softwareVersion : ''), function (text) {
        $('body').html(text);
        utils.rescan();  // does all the tag replacement
        $.getScript(page + '.js' + (utils.controlCache ? '?ver=' + utils.softwareVersion : ''), function () {
        });
    });
};

utils.isOnline = function () {
    //return false;          //  used to simulate off-line condition
    return navigator.onLine;
};


//# sourceURL=kiss/utils.js
