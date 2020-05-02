/*
      On the front-end, each screen is made up of two files - an HTML file, and a JavaScript file - each with the same
      file name (except the extension).

      The JavaScript is wrapped in a single, unnamed function that gets executed immediately.  This serves two purposes.
      First, it allows the creation of screen-specific global variables thus allowing data sharing throughout the screen
      without interfering with other screens.  Second, it best assures that there is no trace of the screen left when
      the screen is replaces with another.

      The entire HTML file is wrapped in a <div>.  The <div> includes any screen specific popups.  However, the Kiss
      system has a different facility to support popups that are shared among many screens.  (See Utils.component())
 */

/* global $$ */

'use strict';

(function () {

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

})();