
/* global $$ */

'use strict';

$$('ok').onclick(function () {
    if ($$('ctl-text').isError('Text'))
        return;

    if ($$('ctl-number').isError('Numeric'))
        return;

    const text_val = $$('ctl-text').getValue();

    const num_val = $$('ctl-number').getValue();


    //  all tests passed
});

$$('my-popup-button').onclick(function () {
    Utils.popup_open("my-popup", null);
});

$$('popup-ok').onclick(function () {
   Utils.popup_close("my-popup");
});
