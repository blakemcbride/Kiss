
/* global Utils, Server */

'use strict';

Utils.afterComponentsLoaded(function () {
    let url = Utils.getAppUrl();
    url = url.replace(/:8000/, ':8080');
    Server.setURL(url);

    Utils.forceASCII = false;  // Force all text entry to ASCII (see Utils.forceASCII)

    const screenPixels = screen.height * screen.width;
    if (screenPixels < 600000)
        Utils.loadPage("mobile/login");
    /*
    else if (screenPixels < 1000000)
        Utils.loadPage("tablet/login");
    */
    else
        Utils.loadPage('login');
});


(function () {
    Utils.useComponent('Popup');
    Utils.useComponent('CheckBox');
    Utils.useComponent('DateInput');
    Utils.useComponent('DropDown');
    Utils.useComponent('ListBox');
    Utils.useComponent('NumericInput');
    Utils.useComponent('PushButton');
    Utils.useComponent('RadioButton');
    Utils.useComponent('TextboxInput');
    Utils.useComponent('TextInput');
    Utils.useComponent('TextLabel');
    Utils.useComponent('TimeInput');
    Utils.useComponent('FileUpload');
    Utils.useComponent('NativeDateInput');
    Utils.useComponent('Picture');
})();

