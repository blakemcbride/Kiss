package org.kissweb;

/**
 * A caches set with a least-recently used eviction policy based on max number of entries and/or expiration duration.
 * <br><br>
 * This class is thread-safe.
 * <br><br>
 * Author: Blake McBride<br>
 * Date: 3/5/22
 */
public class LRUSet<K> extends LRUCache<K, Object> {
    /**
     * Public constructor that requires an expiration time in seconds.
     *
     * @param maxEntries        the maximum number of elements to store or zero for no limit
     * @param expirationSeconds the number of seconds after which a key is considered expired or zero for no timed expiration
     * @throws IllegalArgumentException if {@code expirationSeconds} is less than or equal to zero.
     */
    public LRUSet(long maxEntries, long expirationSeconds) {
        super(maxEntries, expirationSeconds);
    }

    /**
     * Unsupported.
     *
     * @param key     the key to add
     * @param value the arbitrary object to associate with the key
     * @return unsupported operation
     */
    public boolean add(K key, Object value) {
        throw new UnsupportedOperationException("Unsupported method");
    }

    /**
     * Unsupported.
     *
     * @param key the key whose associated object is to be returned.
     * @return unsupported operation
     */
    public Object get(K key) {
        throw new UnsupportedOperationException("Unsupported method");
    }
}
