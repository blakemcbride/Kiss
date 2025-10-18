package org.kissweb;

import java.util.HashMap;
import java.util.IdentityHashMap;

/**
 * Per-thread String→V cache providing thread-local storage for key-value pairs.
 * <p>
 * This class provides a thread-safe cache where each thread has its own isolated storage.
 * Key features:
 * <ul>
 *   <li>Values are explicitly put/get - no automatic loading</li>
 *   <li>Multiple ThreadLevelCache instances can coexist per thread</li>
 *   <li>Values persist for the lifetime of the thread (unless explicitly cleared)</li>
 *   <li>{@link #clearThreadCaches()} clears ALL instances in the current thread</li>
 *   <li>{@link #releaseThreadLocal()} removes per-thread maps/registrations for this instance</li>
 * </ul>
 *
 * @param <V> the type of values stored in this cache
 */
public final class ThreadLevelCache<V> {

    /* ---- Per-thread registry of all cache instances ---- */
    private static final ThreadLocal<IdentityHashMap<ThreadLevelCache<?>, Boolean>> REGISTRY = ThreadLocal.withInitial(IdentityHashMap::new);

    /* ---- Per-instance, per-thread storage ---- */
    private final ThreadLocal<HashMap<String, V>> local = ThreadLocal.withInitial(HashMap::new);

    /**
     * Creates a new ThreadLevelCache instance.
     * Each instance maintains its own separate cache storage per thread.
     * The instance is automatically registered for cleanup via {@link #clearThreadCaches()}.
     */
    public ThreadLevelCache() {
        // Register this instance in the current thread's registry
        REGISTRY.get().put(this, Boolean.TRUE);
    }

    /* ---- Public API (explicit put/get) ---- */

    /**
     * Returns the value for the specified key in the current thread, or null if absent.
     *
     * @param key the key whose associated value is to be returned
     * @return the value associated with the specified key, or null if no mapping exists
     */
    public V get(String key) {
        return local.get().get(key);
    }

    /**
     * Returns the value for the specified key in the current thread, or adds and returns
     * the default value if absent.
     *
     * @param key the key whose associated value is to be returned
     * @param def the default value to be returned (and stored) if the key is not present
     * @return the value associated with the specified key, or def if no mapping existed
     */
    public V getOrDefault(String key, V def) {
        HashMap<String, V> hm = local.get();
        V val = hm.get(key);
        if (val == null) {
            hm.put(key, def);
            return def;
        }
        return val;
    }

    /**
     * Returns true if this cache (in the current thread) contains the specified key.
     *
     * @param key the key whose presence is to be tested
     * @return true if this cache contains a mapping for the specified key
     */
    public boolean containsKey(String key) {
        return local.get().containsKey(key);
    }

    /**
     * Puts (inserts or replaces) an entry in this cache for the current thread.
     *
     * @param key the key with which the specified value is to be associated
     * @param value the value to be associated with the specified key
     * @return the previous value associated with key, or null if there was no mapping
     */
    public V put(String key, V value) {
        return local.get().put(key, value);
    }

    /**
     * Removes and returns a single cached entry in this cache for the current thread.
     *
     * @param key the key whose mapping is to be removed from the cache
     * @return the previous value associated with key, or null if there was no mapping
     */
    public V invalidate(String key) {
        return local.get().remove(key);
    }

    /**
     * Clears ONLY this cache instance in the current thread.
     * Other ThreadLevelCache instances remain unaffected.
     */
    public void clear() {
        local.get().clear();
    }

    /**
     * Returns the number of entries in this cache for the current thread.
     *
     * @return the number of key-value mappings in this cache
     */
    public int size() {
        return local.get().size();
    }

    /**
     * Drops this instance's ThreadLocal map and registration for the current thread.
     * This helps prevent memory leaks when a thread is long-lived and no longer needs
     * this cache instance.
     */
    public void releaseThreadLocal() {
        local.remove();
        IdentityHashMap<ThreadLevelCache<?>, Boolean> reg = REGISTRY.get();
        reg.remove(this);
        if (reg.isEmpty())
            REGISTRY.remove();
    }

    /* ---- Static utilities (apply to ALL ThreadLevelCache instances in the current thread) ---- */

    /**
     * Clears ALL ThreadLevelCache instances for the CURRENT thread.
     * This is a static utility method that affects all cache instances
     * that have been used in the current thread.
     */
    public static void clearThreadCaches() {
        IdentityHashMap<ThreadLevelCache<?>, Boolean> reg = REGISTRY.get();
        if (reg.isEmpty())
            return;
        for (ThreadLevelCache<?> cache : reg.keySet()) {
            // Safe: each instance has its own ThreadLocal map
            cache.local.get().clear();
        }
    }
}


