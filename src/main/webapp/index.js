
'use strict';

utils.afterComponentsLoaded(function () {

    Server.setURL('http://localhost:8080');

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
