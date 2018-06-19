/**
 * Created by Blake McBride on 6/18/18.
 */

'use strict';


var timeutils = function () {};

timeutils.format = function (val, zero_fill) {
    if (val === null  ||  val === undefined  ||  val === '')
        return '';
    let hours = Math.floor(val/100);
    let minutes = val % 100;
    let width;
    let msk;
    if (zero_fill) {
        width = 2;
        msk = 'Z';
    } else {
        width = 0;
        msk = '';
    }
    if (hours >= 13) {
        hours -= 12;
        return utils.format(hours, msk, width, 0) + ':' + utils.format(minutes, 'Z', 2, 0) + ' PM';
    } else
        return utils.format(hours, msk, width, 0) + ':' + utils.format(minutes, 'Z', 2, 0) + ' AM';
};



//# sourceURL=kiss/timeutils.js
