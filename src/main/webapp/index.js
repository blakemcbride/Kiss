
'use strict';

utils.afterComponentsLoaded(function () {

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
})();

//# sourceURL=index.js
