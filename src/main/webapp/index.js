
'use strict';

utils.afterComponentsLoaded(function () {
    var href = window.location.href;
    var len = href.lastIndexOf('/');
    var url = href.substr(0, len);
    url = url.replace('8000', '8080');

    Server.setURL(url);

    utils.loadPage('login');

});


(function () {
    utils.useComponent('CheckBox');
    utils.useComponent('DateInput');
    utils.useComponent('DropDown');
    utils.useComponent('ListBox');
    utils.useComponent('NumericInput');
    utils.useComponent('PushButton');
    utils.useComponent("RadioButton");
    utils.useComponent("TextboxInput");
    utils.useComponent("TextInput");
    utils.useComponent("TimeInput");
})();

//# sourceURL=index.js
