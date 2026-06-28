/* global Utils, AppState */

'use strict';

/**
 * Router — hash-based client-side router for the Kiss framework.
 * <br><br>
 * Maps the URL hash (e.g. <code>#/controls</code>, <code>#/customer/123</code>)
 * onto Kiss's two navigation levels:
 * <ul>
 *   <li><b>Full-body screens</b> (no <code>tag</code>) — a "shell" or the login
 *       page; loaded by replacing <code>document.body</code>.</li>
 *   <li><b>Sub-screens</b> (a <code>tag</code> plus a <code>shell</code>) — loaded
 *       into a region of their shell (e.g. <code>app-screen-area</code>).</li>
 * </ul>
 * Hash routing needs no server-side URL rewriting, so it works identically on the
 * dev static server, <code>file://</code>, Electron, and a deployed WAR. The hash
 * changes per screen (deep-linkable, bookmarkable) and the browser Back/Forward
 * buttons become in-app navigation.
 * <br><br>
 * Routes are registered (typically in <code>routes.js</code>) with {@link Router.add}:
 * <pre>
 *   Router.add('/login',        { page: 'login', auth: false });
 *   Router.add('/',             { page: 'screens/Framework/Framework' });
 *   Router.add('/controls',     { page: 'screens/Controls/Controls', shell: '/', tag: 'app-screen-area', focus: 'ctl-text' });
 *   Router.add('/customer/:id', { page: 'screens/Customer/Customer', shell: '/', tag: 'app-screen-area' });
 * </pre>
 * Route definition fields:
 * <ul>
 *   <li><code>page</code> — loadPage path, or a function returning one (for
 *       device-aware screens).</li>
 *   <li><code>tag</code> — container element id; omit for a full-body screen.</li>
 *   <li><code>shell</code> — for a sub-screen (a route with a <code>tag</code>), the route
 *       whose page hosts it and must occupy the body first.  Defaults to the default route
 *       (the app's main shell), so it is usually omitted.</li>
 *   <li><code>focus</code> — id of the control to focus after load.</li>
 *   <li><code>auth</code> — defaults to <code>true</code> (requires a session);
 *       set <code>false</code> for public routes such as login.</li>
 * </ul>
 * Path parameters (<code>:name</code>) are parsed and passed to the screen via the
 * existing <code>loadPage</code> <code>argv</code> mechanism, and are also readable
 * with {@link Router.params}, so a deep-linked screen can rebuild its state from
 * the URL.
 */
class Router {

    /** Registered routes: {path, regex, keys, def}. */
    static #routes = [];

    /** The page string currently loaded into document.body (shell or full-body screen). */
    static #bodyPage = null;

    /** The full route path currently rendered (distinguishes the shell route from a sub-screen sharing the same shell). */
    static #bodyRoute = null;

    static #currentPath = null;
    static #params = {};
    static #query = {};
    static #defaultPath = '/';
    static #started = false;

    //  File-based fallback (see setScreenRoot): a path matching no registered route is
    //  loaded as a screen under #screenRoot, with these defaults.
    static #screenRoot = null;
    static #fallbackShell = null;
    static #fallbackTag = null;

    /**
     * Register a route.
     *
     * @param {string} path route path, may contain <code>:name</code> parameters
     * @param {object} def route definition ({page, tag, shell, focus, auth})
     * @returns {Router} for chaining
     */
    static add(path, def) {
        const keys = [];
        const pattern = path.replace(/:[^/]+/g, function (m) {
            keys.push(m.substring(1));
            return '([^/]+)';
        });
        Router.#routes.push({ path: path, regex: new RegExp('^' + pattern + '$'), keys: keys, def: def });
        return Router;
    }

    /**
     * Set the route used when the hash is empty or unmatched.
     *
     * @param {string} path
     * @returns {Router} for chaining
     */
    static setDefault(path) {
        Router.#defaultPath = path;
        return Router;
    }

    /**
     * Enable file-based fallback routing.  A URL path matching no registered route is then
     * treated as a screen path under <code>root</code> — e.g. with root <code>'screens'</code>,
     * <code>#/Reports/Daily</code> loads <code>screens/Reports/Daily</code>.  Register a route
     * only for screens that need a clean/stable URL, params, or special metadata; the rest
     * resolve by convention.
     * <br><br>
     * <code>opts.shell</code> / <code>opts.tag</code> set where such screens render (shell
     * defaults to the default route; without a tag they load full-body).  A <code>?tag=</code>
     * on the URL overrides the tag per navigation.  Fallback screens always require
     * authentication — register a route with <code>auth:false</code> for a public screen.
     * <br><br>
     * Note: this makes every screen file under the root URL-addressable (the route table is no
     * longer an allowlist).  Path traversal (<code>..</code>) is rejected.
     *
     * @param {string} root screen-path prefix, e.g. 'screens'
     * @param {object} [opts] {shell, tag}
     * @returns {Router} for chaining
     */
    static setScreenRoot(root, opts) {
        Router.#screenRoot = root;
        opts = opts || {};
        Router.#fallbackShell = opts.shell || null;
        Router.#fallbackTag = opts.tag || null;
        return Router;
    }

    /**
     * Begin routing: listen for hash changes and dispatch the current hash.
     * Call once at startup (after components are loaded).
     */
    static start() {
        if (Router.#started)
            return;
        Router.#started = true;
        window.addEventListener('hashchange', Router.#dispatch);
        Router.#dispatch();
    }

    /**
     * @returns {boolean} true if {@link Router.start} has been called
     */
    static isStarted() {
        return Router.#started;
    }

    /**
     * Navigate to a path, pushing a Back-button history entry.
     *
     * @param {string} path
     * @param {string} [tag] optional region to load the screen into; added to the URL as
     *                       <code>?tag=</code> so it overrides the route's default tag
     */
    static go(path, tag) {
        path = Router.#normalize(path);
        if (tag)
            path += (path.indexOf('?') >= 0 ? '&' : '?') + 'tag=' + encodeURIComponent(tag);
        if (window.location.hash === '#' + path)
            Router.#dispatch();   // same hash fires no hashchange — dispatch directly
        else
            window.location.hash = '#' + path;
    }

    /**
     * Navigate to a path, replacing the current history entry (no new Back step).
     * Used, e.g., after login so Back does not return to the login screen.
     *
     * @param {string} path
     */
    static replace(path) {
        path = Router.#normalize(path);
        const base = window.location.pathname + window.location.search;
        window.history.replaceState(null, '', base + '#' + path);
        Router.#dispatch();   // replaceState fires no hashchange
    }

    /**
     * Navigate to the login screen.  When <code>captureReturn</code> is true the
     * current location is remembered as the login <code>return</code> so the user
     * lands back where they were after authenticating (used for session-expiry).
     *
     * @param {boolean} captureReturn
     */
    static gotoLogin(captureReturn) {
        if (captureReturn) {
            const cur = window.location.hash;   // includes leading '#'
            if (cur && cur.indexOf('#/login') !== 0) {
                Router.go('/login?return=' + encodeURIComponent(cur));
                return;
            }
        }
        Router.go('/login');
    }

    /**
     * @returns {object} the current route's path parameters, e.g. {id: '123'}
     */
    static params() {
        return Router.#params;
    }

    /**
     * @returns {object} the current hash query string parsed to an object,
     *                   e.g. {return: '#/controls'}
     */
    static query() {
        return Router.#query;
    }

    /**
     * The post-login destination: the <code>return</code> query parameter when present
     * (the route the user was headed to before being sent to login), otherwise the home
     * route <code>'/'</code>.  Used by the login screen to decide where to go on success.
     *
     * @returns {string}
     */
    static returnTarget() {
        return Router.#query.return || '/';
    }

    /**
     * @returns {string} the currently matched route path
     */
    static current() {
        return Router.#currentPath;
    }

    static #normalize(path) {
        if (!path)
            return '/';
        if (path.charAt(0) === '#')
            path = path.substring(1);
        if (path.charAt(0) !== '/')
            path = '/' + path;
        return path;
    }

    static #parseHash() {
        let hash = window.location.hash || '';
        if (hash.charAt(0) === '#')
            hash = hash.substring(1);
        if (!hash)
            hash = Router.#defaultPath;
        let queryStr = '';
        const qi = hash.indexOf('?');
        if (qi >= 0) {
            queryStr = hash.substring(qi + 1);
            hash = hash.substring(0, qi);
        }
        return { path: hash, query: Router.#parseQuery(queryStr) };
    }

    static #parseQuery(queryStr) {
        const out = {};
        if (!queryStr)
            return out;
        const pairs = queryStr.split('&');
        for (let i = 0; i < pairs.length; i++) {
            const idx = pairs[i].indexOf('=');
            if (idx >= 0)
                out[decodeURIComponent(pairs[i].substring(0, idx))] = decodeURIComponent(pairs[i].substring(idx + 1));
            else
                out[decodeURIComponent(pairs[i])] = '';
        }
        return out;
    }

    static #findRoute(path) {
        for (let i = 0; i < Router.#routes.length; i++)
            if (Router.#routes[i].path === path)
                return Router.#routes[i];
        return null;
    }

    static #match(path) {
        for (let i = 0; i < Router.#routes.length; i++) {
            const r = Router.#routes[i];
            const m = r.regex.exec(path);
            if (m) {
                const params = {};
                for (let k = 0; k < r.keys.length; k++)
                    params[r.keys[k]] = decodeURIComponent(m[k + 1]);
                return { route: r, params: params };
            }
        }
        return null;
    }

    /**
     * Build a route on the fly for a path that matches no registered route, by treating it
     * as a screen path under the configured screen root (see {@link Router.setScreenRoot}).
     * Returns null for an empty or unsafe path (path traversal via "." / ".." is rejected).
     */
    static #fallbackRoute(parsed) {
        let p = parsed.path;
        if (p.charAt(0) === '/')
            p = p.substring(1);
        if (!p)
            return null;
        const segs = p.split('/');
        for (let i = 0; i < segs.length; i++)
            if (segs[i] === '' || segs[i] === '.' || segs[i] === '..')
                return null;   // no empty/relative segments — cannot escape the screen root
        const def = {
            page: Router.#screenRoot + '/' + p,
            shell: Router.#fallbackShell || undefined,   // undefined => dispatch uses the default route
            tag: Router.#fallbackTag,                    // ?tag= on the URL overrides this in dispatch
            auth: true                                   // fallback screens always require login
        };
        return { route: { path: parsed.path, def: def }, params: {} };
    }

    static #resolvePage(def) {
        return (typeof def.page === 'function') ? def.page() : def.page;
    }

    static async #dispatch() {
        const parsed = Router.#parseHash();
        let matched = Router.#match(parsed.path);
        if (!matched && Router.#screenRoot)
            matched = Router.#fallbackRoute(parsed);   // file-based fallback
        if (!matched)
            matched = Router.#match(Router.#defaultPath);
        if (!matched) {
            console.log('Router: no route matches "' + parsed.path + '" and no default route is registered');
            return;
        }
        const def = matched.route.def;

        //  Auth gate: send unauthenticated users to login, remembering where they were headed.
        if (def.auth !== false && !AppState.get('_uuid')) {
            if (!Router.#findRoute('/login')) {
                console.log('Router: authentication required but no /login route is registered');
                return;
            }
            Router.replace('/login?return=' + encodeURIComponent('#' + parsed.path));
            return;
        }

        Router.#currentPath = matched.route.path;
        Router.#params = matched.params;
        Router.#query = parsed.query;

        //  The region to load into: a ?tag= on the URL overrides the route's tag, so a
        //  screen can be directed into a specific region at navigation time.
        const tag = Router.#query.tag || def.tag;

        try {
            if (tag) {
                //  Sub-screen: make sure its shell occupies the body, then load into the region.
                //  shell defaults to the default route (the app's main shell).
                const shellPath = def.shell || Router.#defaultPath;
                const shellRoute = Router.#findRoute(shellPath);
                if (!shellRoute) {
                    console.log('Router: unknown shell "' + shellPath + '" for route "' + matched.route.path + '"');
                    return;
                }
                const shellPage = Router.#resolvePage(shellRoute.def);
                if (Router.#bodyPage !== shellPage) {
                    await Utils.loadPage(shellPage);
                    Router.#bodyPage = shellPage;
                }
                await Utils.loadPage(Router.#resolvePage(def), tag, def.focus, matched.params);
            } else {
                //  Full-body screen (shell or login).  Reload unless we are already rendering
                //  exactly this route — comparing the *route* (not just the body page) so that
                //  returning to the shell route from one of its sub-screens resets the body
                //  instead of leaving the last sub-screen's content showing.
                const page = Router.#resolvePage(def);
                if (Router.#bodyRoute !== matched.route.path) {
                    await Utils.loadPage(page, null, def.focus, matched.params);
                    Router.#bodyPage = page;
                }
            }
            Router.#bodyRoute = matched.route.path;
        } catch (e) {
            console.log('Router: error loading route "' + matched.route.path + '": ' + (e && e.message ? e.message : e));
        }
    }
}
