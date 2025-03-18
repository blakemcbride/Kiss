
/* global $$, Server */

'use strict';

(async function () {

    let context = '';

    $$('prompt').clear().focus();

    let res = await Server.call('services/OllamaQuery', 'isOllamaUp');
    if (res._Success) {
        if (!res.isOllamaUp) {
            await Utils.showMessage('Error', 'The Ollama server is not running on your machine.');
            return;
        }
    } else
        return;

    res = await Server.call('services/OllamaQuery', 'listModels');
    if (res._Success) {
        const models = res.models;
        if (!models || models.length === 0) {
            await Utils.showMessage('Error', 'There are no Olamma models available.');
            return;
        }
        const ctl = $$('models').clear().add('', '(select a model)');
        for (let i = 0; i < models.length; i++)
            ctl.add(models[i], models[i]);
    } else
        return;

    $$('send').onclick(async function () {
        $$('response').clear();
        if ($$('models').isError("Model"))
            return;
        if ($$('prompt').isError("Query"))
            return;
        let prompt = $$('prompt').getValue();prompt;
        const indata = {
            model: $$('models').getValue(),
            prompt: context ? context + "My new question is: " + prompt : prompt
        };
        Utils.waitMessage('Thinking...');
        let res = await Server.call('services/OllamaQuery', 'ask', indata);
        Utils.waitMessageEnd();
        if (res._Success) {
            $$('response').setHTMLValue(res.htmlResponse);
            if (!context)
                context = "Our prior conversations are as follows:\n";
            context += prompt + '\n' + res.textResponse + '\n';
        }
    });

    $$('new-context').onclick(function () {
        context = '';
        $$('prompt').clear();
        $$('response').clear();
    });

})();
