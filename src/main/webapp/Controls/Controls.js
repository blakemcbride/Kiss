
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


