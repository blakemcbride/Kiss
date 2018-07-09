
'use strict';

$$('groovy-button').onclick(async function () {
    $$('input-3').setValue('');
    let data = {
        num1: $$('input-1').getValue(),
        num2: $$('input-2').getValue()
    };
    let res = await Server.call('services/MyGroovyService', 'addNumbers', data);
    if (res._Success) {
        $$('input-3').setValue(res.num3);
    }
});

$$('java-button').onclick(async function () {
    $$('input-3').setValue('');
    let data = {
        num1: $$('input-1').getValue(),
        num2: $$('input-2').getValue()
    };
    let res = await Server.call('services/MyJavaService', 'addNumbers', data);
    if (res._Success) {
        $$('input-3').setValue(res.num3);
    }
});

$$('lisp-button').onclick(async function () {
    $$('input-3').setValue('');
    let data = {
        num1: $$('input-1').getValue(),
        num2: $$('input-2').getValue()
    };
    let res = await Server.call('services/MyLispService', 'addNumbers', data);
    if (res._Success) {
        $$('input-3').setValue(res.num3);
    }
});

$$('logout').onclick(function () {
    Server.setUUID('');
    history.go(0);
});


//# sourceURL=page1/page1.js
