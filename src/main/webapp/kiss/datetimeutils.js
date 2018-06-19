/**
* Created by Blake McBride on 6/16/18.
*/

'use strict';


var datetimeutils = function () {};


/**
 * Format a Date to a string representation.
 *
 * @param dt a Date type
 * @returns {string}
 */
datetimeutils.formatDate = function (dt) {
    if (!dt)
        return '';
    var hours = dt.getHours();
    var sf;
    if (hours > 12)
        hours -= 12;
    if (hours >= 12)
        sf = ' PM';
    else
        sf = ' AM';
    if (!hours)
        hours = 12;
    var min = dt.getMinutes();
    if (min < 10)
        min = '0' + min.toString();
    else
        min = min.toString();
    return (dt.getMonth()+1).toString() + '/' + dt.getDate().toString() + '/' + dt.getFullYear().toString() + ' ' + hours.toString() + ':' + min + sf;
};

/**
 *
 * @param dt    int YYYYMMDD
 * @param time  int 1231
 * @returns {*}
 */
datetimeutils.formatInt = function (dt, time) {
    if (!dt && (time === undefined  ||  time === null  ||  time === ''))
        return '';
    var res = dateutils.intToStr4(dt);
    var tf = timeutils.format(time);
    if (res && tf)
        return res + ' ' + tf;
    else
        return res + tf;
};


//# sourceURL=kiss/datetimeutils.js
