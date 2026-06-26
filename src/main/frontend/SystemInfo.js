/* exported SystemInfo */

'use strict';

/**
 * Application / deployment configuration for the Kiss front-end.
 *
 * This file was split out of index.html so the page can run under a strict
 * Content-Security-Policy (no inline scripts).  Per-deployment values are
 * edited here (search for "EDIT").
 */

const SystemInfo = {};

// The following parameters must be set in a production system
SystemInfo.softwareVersion = "EDIT-1";  // used to uniquely identify a version of the system
SystemInfo.releaseDate = "EDIT-2";
SystemInfo.controlCache = false;  // normally true but use false during debugging EDIT-3

// Where the front-end keeps state (incl. the session token) between pages/reloads:
//   'local'   - localStorage  (survives reload, new tabs, Electron restarts; shared across tabs)  [default]
//   'session' - sessionStorage (survives reload; per-tab; cleared on tab close)
//   'memory'  - in-memory only (classic single-page behavior; lost on reload)
SystemInfo.stateStore = 'local';  // EDIT-4

// The following is only set on a production system that has the front-end and back-end separated.
// When set, also add this origin to the connect-src of the CSP <meta> tag in index.html.
//SystemInfo.backendUrl = 'https://[YOUR-URL]/[back-end]';
