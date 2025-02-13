package org.kissweb;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Collections;

/**
 * LRU (Least Recently Used) cache for storing key-value pairs.
 * The cache will evict entries that are old so that the cache doesn't get too big.
 * This class is thread safe.
 *
 * @param <K> the type of keys maintained by this cache
 * @param <V> the type of values
 */
public class LRUCache<K, V> {
    private final Map<K, V> cache;
    private final int maxEntries;

    public LRUCache(int maxEntries) {
        this.maxEntries = maxEntries;

        // Create a LinkedHashMap with accessOrder=true, so it moves
        // entries to the end when they're accessed (i.e., LRU).
        // Then wrap that map with synchronizedMap for thread safety.
        LinkedHashMap<K, V> temp = new LinkedHashMap<K, V>(maxEntries, 0.75f, true) {
            @Override
            protected boolean removeEldestEntry(Map.Entry<K, V> eldest) {
                // Evict the eldest entry once we exceed the maximum size
                return size() > LRUCache.this.maxEntries;
            }
        };

        this.cache = Collections.synchronizedMap(temp);
    }

    /**
     * Returns the value associated with the given key, or null if not found.
     */
    public V get(K key) {
        return cache.get(key);
    }

    /**
     * Adds or updates the value for the given key.
     * Returns the previous value associated with key, or null if there was none.
     */
    public V put(K key, V value) {
        return cache.put(key, value);
    }

    /**
     * Removes the entry for the specified key if it exists.
     * Returns the value that was removed, or null if the key wasn't present.
     */
    public V remove(K key) {
        return cache.remove(key);
    }

    /**
     * Returns the current number of entries in the cache.
     */
    public int size() {
        // We must synchronize on the same lock used by the map
        synchronized (cache) {
            return cache.size();
        }
    }

    /**
     * Clears all entries from the cache.
     */
    public void clear() {
        synchronized (cache) {
            cache.clear();
        }
    }
}
