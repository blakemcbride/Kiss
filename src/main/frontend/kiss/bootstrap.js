/* global SystemInfo, AppState */
/* exported getScript, getScripts, getURLParameter */

'use strict';

/**
 * Kiss front-end bootstrap loader.
 *
 * Split out of index.html so the page can run under a strict
 * Content-Security-Policy (no inline scripts and no inline event handlers).
 * Defines the global script loaders used throughout the framework
 * (getScript / getScripts / getURLParameter), then loads the framework on
 * window 'load'.
 */

/**
 * Load a JavaScript file.
 */
function getScript(url) {
    return new Promise(function (resolve, reject) {
        const head = document.getElementsByTagName("head")[0];
        const script = document.createElement("script");
        script.type = "text/javascript";
        script.src = url + (SystemInfo.controlCache ? '?ver=' + SystemInfo.softwareVersion : '');

        // Handle Script loading
        {
            let done = false;

            // Attach handlers for all browsers
            script.onload = script.onreadystatechange = function () {
                if (!done && (!this.readyState ||  this.readyState === "loaded" || this.readyState === "complete")) {
                    done = true;
                    // Handle memory leak in IE
                    script.onload = script.onreadystatechange = null;
                    resolve();
                } else {
                    console.log("getScript:  error loading " + url);
                    reject();
                }
            };
            script.onerror = function () {
                console.log("getScript: error loading " + url);
                reject();
            };
        }
        head.appendChild(script);
    });
}

/**
 * Load several script files in parallel.  Returns a promise that resolves when *every* script finished loading;
 * rejects as soon as the first one fails.
 *
 * @param {string[]} urls – one or more absolute/relative URLs.
 * @return {Promise<void>} – resolves when *every* script finished loading;
 *                           rejects as soon as the first one fails.
 */
function getScripts(urls) {
    return Promise.all(urls.map(getScript));
}

function getURLParameter(sParam) {
    const urlArgs = window.location.search.substring(1);
    const argArray = urlArgs.split('&');
    for (let i = 0; i < argArray.length; i++)  {
        let param = argArray[i].split('=');
        if (param[0] === sParam)
            return param[1];
    }
    return undefined;
}

function startup() {

    async function loadUtils() {
        // Load DOMUtils first (must complete before Utils.js)
        await getScript("kiss/DOMUtils.js");
        // AppState must be defined and initialized before Server.js is used
        await getScript("kiss/AppState.js");
        AppState.init();
        // Load remaining scripts in parallel
        await getScripts([
            "kiss/Utils.js",
            "kiss/DateUtils.js",
            "kiss/DateTimeUtils.js",
            "kiss/TimeUtils.js",
            "kiss/NumberUtils.js",
            "kiss/Server.js",
            "kiss/AGGrid.js",
            "kiss/Editor.js",
            "kiss/MutableString.js"
        ]);
        getScript("index.js");
    }

    if (SystemInfo.controlCache) {
        const now = getURLParameter("now");
        let urlArgs = window.location.search.substring(1);
        if (now) {
            const diff = Math.abs(((new Date()).getTime() - Number(now)) / 1000);
            if (diff > 30) {
                window.onbeforeunload = null;
                urlArgs = urlArgs.replaceAll(/now=[^&]*&*/g, '');
                window.location.href = 'index.html?now=' + (new Date()).getTime() + (urlArgs ? '&' + urlArgs : '');
            } else {
                loadUtils();
            }
        } else {
            window.onbeforeunload = null;
            window.location.href = 'index.html?now=' + (new Date()).getTime() + (urlArgs ? '&' + urlArgs : '');
        }
    } else
        loadUtils();
}

window.addEventListener('load', startup);
