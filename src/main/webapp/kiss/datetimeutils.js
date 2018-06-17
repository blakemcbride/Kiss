/**
 * Created by Blake McBride on 6/16/18.
 */

'use strict';


var datetimeutils = function () {};



datetimeutils.format = function (dt) {
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


//# sourceURL=kiss/datetimeutils.js
