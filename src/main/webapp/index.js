
/* global Utils, Server */

'use strict';

Utils.afterComponentsLoaded(function () {
    const href = window.location.href;
    let url = href.substr(0, href.lastIndexOf('/'));
    url = url.replace('8000', '8080');

    Server.setURL(url);

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
    Utils.useComponent("RadioButton");
    Utils.useComponent("TextboxInput");
    Utils.useComponent("TextInput");
    Utils.useComponent("TextLabel");
    Utils.useComponent("TimeInput");
})();

