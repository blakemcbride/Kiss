/* global SystemInfo, AppState */
/* exported getScript, getScripts, getURLParameter */

'use strict';

/**
 * Kiss front-end bootstrap loader.
 *
 * Loaded (version-busted) by the byte-stable kernel in index.html, which has already
 * ensured index.html is fresh (?now) and defined window.cacheBust().  This file loads every
 * other downloaded resource — stylesheets, third-party libraries, framework and
 * application JavaScript — through window.cacheBust(), so a single app-version bump in
 * index.html force-refreshes all of them.
 *
 * Defines the global script loaders used throughout the framework
 * (getScript / getScripts / getURLParameter).
 */

/**
 * Append the cache-busting token to a URL (the app-version in production, a per-page-load
 * timestamp in development).
 */
function cacheBustUrl(url) {
    return (typeof window.cacheBust === 'function') ? window.cacheBust(url) : url;
}

/**
 * Load a JavaScript file (version-busted).
 */
function getScript(url) {
    return new Promise(function (resolve, reject) {
        const head = document.getElementsByTagName("head")[0];
        const script = document.createElement("script");
        script.type = "text/javascript";
        script.src = cacheBustUrl(url);

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

/**
 * Add a stylesheet (version-busted).
 */
function addStylesheet(url) {
    const link = document.createElement("link");
    link.rel = "stylesheet";
    link.href = cacheBustUrl(url);
    document.getElementsByTagName("head")[0].appendChild(link);
}

async function loadUtils() {
    // Stylesheets
    addStylesheet("normalize.css");
    addStylesheet("normalize2.css");
    addStylesheet("kiss/Utils.css");
    addStylesheet("kiss/ag-grid-custom.css");

    // Third-party libraries — must load before the kiss AGGrid/Editor wrappers that use them
    await getScripts([
        "lib/ag-grid-community.min.noStyle.js",
        "lib/ckeditor.js"
    ]);

    // Per-deployment configuration (needed by AppState.init).  The cache-control values
    // come from the index.html <meta> tags (always fresh); mirror them onto SystemInfo so
    // existing references to SystemInfo.softwareVersion / .controlCache keep working.
    await getScript("SystemInfo.js");
    SystemInfo.softwareVersion = window.KissVersion;
    SystemInfo.controlCache = window.KissCacheOn;

    // DOMUtils first (must complete before Utils.js); AppState before Server.js uses it
    await getScript("kiss/DOMUtils.js");
    await getScript("kiss/AppState.js");
    AppState.init();

    await getScripts([
        "kiss/Utils.js",
        "kiss/DateUtils.js",
        "kiss/DateTimeUtils.js",
        "kiss/TimeUtils.js",
        "kiss/NumberUtils.js",
        "kiss/Server.js",
        "kiss/Router.js",
        "kiss/AGGrid.js",
        "kiss/Editor.js",
        "kiss/MutableString.js"
    ]);

    // routes.js (application-owned) registers routes and must load before index.js calls Router.start()
    await getScript("routes.js");
    getScript("index.js");
}

loadUtils();
