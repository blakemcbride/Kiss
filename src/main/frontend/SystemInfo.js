/* exported SystemInfo */

'use strict';

/**
 * Application / deployment configuration for the Kiss front-end.
 *
 * NOTE: the cache-busting values — the software version and the cache-control flag —
 * are NOT here.  They live in index.html as <meta name="kiss-version"> and
 * <meta name="kiss-cache-control">, because index.html is the only file kept
 * perpetually fresh (via the ?now redirect).  bootstrap.js mirrors them back onto
 * SystemInfo.softwareVersion / SystemInfo.controlCache after this file loads, so existing
 * references keep working.
 *
 * Per-deployment values below are edited here (search for "EDIT").
 */

const SystemInfo = {};

SystemInfo.releaseDate = "EDIT-2";

// Where the front-end keeps state (incl. the session token) between pages/reloads:
//   'session' - sessionStorage (survives reload; PER-TAB; cleared on tab close)  [default]
//   'local'   - localStorage   (survives reload, new tabs, Electron restarts; SHARED across tabs)
//   'memory'  - in-memory only  (classic single-page behavior; lost on reload)
// Per-tab ('session') is the default: each tab is its own isolated session, so two tabs
// never clobber each other's state, and a new tab requires its own login.
SystemInfo.stateStore = 'session';  // EDIT-4

// The following is only set on a production system that has the front-end and back-end separated.
// When set, also add this origin to the connect-src of the CSP in SecurityHeadersFilter.
//SystemInfo.backendUrl = 'https://[YOUR-URL]/[back-end]';
