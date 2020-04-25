/* global Utils */

/**
 * Created by Blake McBride on 6/18/18.
 */

'use strict';

/**
 * Time utilities.
 *
 */
class TimeUtils {

    /**
     * Format time.
     *
     * @param  val integer HHMM or Date object
     * @param {boolean} zero_fill
     * @returns {string}
     */
    static format(val, zero_fill) {
        if (val === null || val === undefined || val === ''  ||  val < 0)
            return '';
        if (typeof val === 'object')
            val = DateTimeUtils.dateToIntTime(val);
        const hours = Math.floor(val / 100);
        const minutes = val % 100;
        let width;
        let msk;
        if (zero_fill) {
            width = 2;
            msk = 'Z';
        } else {
            width = 0;
            msk = '';
        }
        if (hours === 0)
            return Utils.format(hours + 12, msk, width, 0) + ':' + Utils.format(minutes, 'Z', 2, 0) + ' AM';
        if (hours >= 13)
            return Utils.format(hours - 12, msk, width, 0) + ':' + Utils.format(minutes, 'Z', 2, 0) + ' PM';
        else
            return Utils.format(hours, msk, width, 0) + ':' + Utils.format(minutes, 'Z', 2, 0) + (hours === 12 ? ' PM' : ' AM');
    }

}

