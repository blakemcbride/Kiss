/* global SystemInfo */

'use strict';

/**
 * AppState — unified client-side state store for the Kiss framework.
 * <br><br>
 * A single, simple place to keep arbitrary front-end state that needs to
 * survive page navigation and reloads.  The session token is not treated
 * specially; it is just the reserved entry <code>_uuid</code> stored alongside
 * any other application state.  Because everything lives in one store, logout is
 * a single {@link AppState.clear} call.
 * <br><br>
 * The backing store is selected once at startup from
 * <code>SystemInfo.stateStore</code>:
 * <ul>
 *   <li><code>'session'</code> — sessionStorage: survives reload, <b>per-tab</b>,
 *       cleared when the tab closes (default).  Each tab is its own isolated
 *       session, so two tabs never clobber each other's state.</li>
 *   <li><code>'local'</code>   — localStorage: survives reload, new tabs, and
 *       Electron restarts; <b>shared</b> across tabs of the same origin.</li>
 *   <li><code>'memory'</code>  — in-memory only: lost on reload (the framework's
 *       classic single-page behavior).</li>
 * </ul>
 * If Web Storage is unavailable (private mode, disabled, quota), AppState falls
 * back to in-memory so the application still runs.
 * <br><br>
 * This is the only module permitted to touch <code>window.localStorage</code> /
 * <code>window.sessionStorage</code> directly, mirroring the way DOMUtils is the
 * only module permitted to touch the DOM.
 */
class AppState {

    /** Namespace applied to every stored key so Kiss state never collides with
     *  anything else on the origin and {@link AppState.clear} only removes Kiss keys. */
    static #PREFIX = 'kiss.';

    /** Resolved backend: 'memory' | 'session' | 'local'. */
    static #backend = 'memory';

    /** The Web Storage object when the backend is 'session'/'local'; null for 'memory'. */
    static #store = null;

    /** In-memory map: the 'memory' backend, and the per-key fallback when a Web Storage write fails. */
    static #mem = new Map();

    /**
     * Select the backing store from <code>SystemInfo.stateStore</code> and probe it.
     * Called once by the bootstrap before any other AppState use.
     */
    static init() {
        let requested = (typeof SystemInfo !== 'undefined' && SystemInfo.stateStore) ? SystemInfo.stateStore : 'session';
        requested = ('' + requested).toLowerCase();
        if (requested !== 'session' && requested !== 'local' && requested !== 'memory')
            requested = 'session';

        if (requested === 'memory') {
            AppState.#backend = 'memory';
            AppState.#store = null;
            return;
        }

        const candidate = requested === 'session' ? window.sessionStorage : window.localStorage;
        if (AppState.#probe(candidate)) {
            AppState.#backend = requested;
            AppState.#store = candidate;
        } else {
            console.log('AppState: Web Storage unavailable; falling back to in-memory state.');
            AppState.#backend = 'memory';
            AppState.#store = null;
        }
    }

    static #probe(store) {
        try {
            const k = AppState.#PREFIX + '__probe__';
            store.setItem(k, '1');
            store.removeItem(k);
            return true;
        } catch (e) {
            return false;
        }
    }

    /**
     * The resolved backend in use.
     *
     * @returns {string} 'memory' | 'session' | 'local'
     */
    static backend() {
        return AppState.#backend;
    }

    /**
     * Store a JSON-serializable value.  Passing <code>undefined</code> removes the key.
     *
     * @param {string} key
     * @param {*} value any JSON-serializable value
     */
    static set(key, value) {
        if (value === undefined) {
            AppState.remove(key);
            return;
        }
        if (AppState.#store) {
            try {
                AppState.#store.setItem(AppState.#PREFIX + key, JSON.stringify(value));
                AppState.#mem.delete(key);
            } catch (e) {
                //  Quota / serialization failure — keep the app working by holding this key in memory.
                console.log('AppState: write failed for "' + key + '" (' + e.name + '); using in-memory for this key.');
                AppState.#mem.set(key, value);
            }
        } else
            AppState.#mem.set(key, value);
    }

    /**
     * Retrieve a previously stored value.
     *
     * @param {string} key
     * @returns {*} the deserialized value, or <code>undefined</code> if absent
     */
    static get(key) {
        if (AppState.#store) {
            const raw = AppState.#store.getItem(AppState.#PREFIX + key);
            if (raw === null)
                return AppState.#mem.has(key) ? AppState.#mem.get(key) : undefined;
            try {
                return JSON.parse(raw);
            } catch (e) {
                //  Corrupt entry — discard it rather than throw.
                AppState.#store.removeItem(AppState.#PREFIX + key);
                return undefined;
            }
        }
        return AppState.#mem.has(key) ? AppState.#mem.get(key) : undefined;
    }

    /**
     * @param {string} key
     * @returns {boolean} true if the key is present
     */
    static has(key) {
        return AppState.get(key) !== undefined;
    }

    /**
     * Remove a single key.
     *
     * @param {string} key
     */
    static remove(key) {
        AppState.#mem.delete(key);
        if (AppState.#store)
            AppState.#store.removeItem(AppState.#PREFIX + key);
    }

    /**
     * All application keys currently held (namespace stripped).
     *
     * @returns {string[]}
     */
    static keys() {
        const out = [];
        if (AppState.#store) {
            for (let i = 0; i < AppState.#store.length; i++) {
                const k = AppState.#store.key(i);
                if (k && k.indexOf(AppState.#PREFIX) === 0)
                    out.push(k.substring(AppState.#PREFIX.length));
            }
        }
        for (const k of AppState.#mem.keys())
            if (out.indexOf(k) === -1)
                out.push(k);
        return out;
    }

    /**
     * Remove all Kiss state (used by logout).  Only Kiss-namespaced keys are
     * removed; unrelated keys on the same origin are left untouched.
     */
    static clear() {
        AppState.#mem.clear();
        if (AppState.#store) {
            const del = [];
            for (let i = 0; i < AppState.#store.length; i++) {
                const k = AppState.#store.key(i);
                if (k && k.indexOf(AppState.#PREFIX) === 0)
                    del.push(k);
            }
            for (const k of del)
                AppState.#store.removeItem(k);
        }
    }
}
