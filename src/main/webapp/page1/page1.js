
'use strict';

$$('groovy-button').onclick(function () {
    $$('input-3').setValue('');
    var data = {
        num1: $$('input-1').getValue(),
        num2: $$('input-2').getValue()
    };
    Server.call('page1', 'GroovyPage1', data).then(function (res) {
        if (res._Success) {
            $$('input-3').setValue(res.num3);
        }
    });
});

$$('java-button').onclick(function () {
    $$('input-3').setValue('');
    var data = {
        num1: $$('input-1').getValue(),
        num2: $$('input-2').getValue()
    };
    Server.call('page1', 'JavaPage1', data).then(function (res) {
        if (res._Success) {
            $$('input-3').setValue(res.num3);
        }
    });
});

$$('lisp-button').onclick(function () {
    $$('input-3').setValue('');
    var data = {
        num1: $$('input-1').getValue(),
        num2: $$('input-2').getValue()
    };
    Server.call('page1', 'LispPage1', data).then(function (res) {
        if (res._Success) {
            $$('input-3').setValue(res.num3);
        }
    });
});

$$('logout').onclick(function () {
    Server.setUUID('');
    history.go(0);
});


//# sourceURL=page1/page1.js
