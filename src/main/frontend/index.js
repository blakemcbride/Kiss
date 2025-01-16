
/* global Utils, Server */

'use strict';

Utils.afterComponentsLoaded(function () {
    if (SystemInfo.backendUrl) {
        // explicit backend URL
        Server.setURL(SystemInfo.backendUrl);
    } else if (window.location.protocol === "file:") {
        //  electron desktop frontend
        Server.setURL('http://localhost:8080');
    } else if (window.location.protocol === "http:" && window.location.port >= 8000) {
        //  Development environment
        Server.setURL('http://' + window.location.hostname + ':8080');
    } else {
        //  Production environment with front-end & back-end as one unit
        let url = Utils.getAppUrl();
        Server.setURL(url);
    }

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

