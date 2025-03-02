package org.kissweb;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * A cache with a least-recently used eviction policy based on max number of entries and/or expiration duration.
 * <br><br>
 * This class is thread-safe.
 * <br><br>
 * Author: Blake McBride<br>
 * Date: 3/5/22
 */
public class LRUCache<K,V> {

    /** Expiration duration in nanoseconds for this instance. */
    private final long expirationNanos;
    private final long maxEntries;

    /**
     * A node in the doubly-linked list representing a key, its associated object, and its last access time.
     */
    private class Node {
        K key;
        V value;
        long lastAccessTime;
        Node prev;
        Node next;

        Node(K key, V value, long lastAccessTime) {
            this.key = key;
            this.value = value;
            this.lastAccessTime = lastAccessTime;
        }
    }

    /** Map for quick lookup of elements to their corresponding nodes. */
    private final Map<K, Node> map = new HashMap<>();
    /** Head of the doubly-linked list (least-recently accessed). */
    private Node head;
    /** Tail of the doubly-linked list (most-recently accessed). */
    private Node tail;

    /**
     * Public constructor that requires an expiration time in seconds.
     *
     * @param maxEntries the maximum number of elements to store or zero for no limit
     * @param expirationSeconds the number of seconds after which a key is considered expired or zero for no timed expiration
     * @throws IllegalArgumentException if {@code expirationSeconds} is less than or equal to zero.
     */
    public LRUCache(long maxEntries, long expirationSeconds) {
        this.expirationNanos = expirationSeconds > 0L ? TimeUnit.SECONDS.toNanos(expirationSeconds) : 0L;
        this.maxEntries = maxEntries;
    }

    /**
     * Private no-argument constructor to prevent instantiation without an expiration time.
     */
    @SuppressWarnings("unused")
    private LRUCache() {
        throw new UnsupportedOperationException(
                "No-argument constructor is not supported. Use TimedSet(long expirationSeconds) instead.");
    }

    /**
     * Purges expired elements from the head of the list.
     * <p>
     * This method checks the key at the head of the list and removes it if it has not been accessed
     * within the expiration time. It repeats this process until the head is fresh or the list is empty.
     * </p>
     */
    private void purgeExpired() {
        if (expirationNanos > 0) {
            long now = System.nanoTime();
            while (head != null && now - head.lastAccessTime > expirationNanos) {
                map.remove(head.key);
                head = head.next;
                if (head != null)
                    head.prev = null;
                else  // List became empty.
                    tail = null;
            }
        }
    }
    
    /**
     * Purges elements from the head of the list until the list is of size at most {@code maxEntries}.
     * <p>
     * This method checks the list size and removes the head key if it exceeds the maximum size. It repeats
     * this process until the list size is within the maximum or the list is empty.
     * </p>
     */
    private void purgeOversized() {
        if (maxEntries > 0)
            while (head != null && map.size() > maxEntries) {
                map.remove(head.key);
                head = head.next;
                if (head != null)
                    head.prev = null;
                else  // List became empty.
                    tail = null;
            }
    }

    /**
     * Adds the given key to the set with no associated object.
     * <p>
     * This is equivalent to calling {@code add(key, null)}.
     * </p>
     *
     * @param key the key to add
     * @return {@code true} if the key was not already present and has been added;
     *         {@code false} if the key was already present (in which case its last access time is updated)
     */
    public synchronized boolean add(K key) {
        return add(key, null);
    }

    /**
     * Adds the given key to the set and associates it with the provided object.
     * <p>
     * Before adding, any expired elements are purged. If the key already exists,
     * its last access time and associated object are updated, and it is moved to the tail.
     * </p>
     *
     * @param key     the key to add
     * @param value the arbitrary object to associate with the key
     * @return {@code true} if the key was not already present and has been added;
     *         {@code false} if the key was already present (in which case its associated object is updated)
     */
    public synchronized boolean add(K key, V value) {
        boolean rtn;
        purgeExpired();
        long now = System.nanoTime();
        Node node = map.get(key);
        if (node != null) {
            // Element exists; update access time and value.
            node.lastAccessTime = now;
            node.value = value;
            if (node != tail) {
                removeNode(node);
                addNodeAtTail(node);
            }
            rtn = false;
        } else {
            Node newNode = new Node(key, value, now);
            map.put(key, newNode);
            addNodeAtTail(newNode);
            rtn = true;
        }
        purgeOversized();
        return rtn;
    }

    /**
     * Checks if the set contains the given key.
     * <p>
     * Before checking, expired elements are purged. If the key is found, its last access time is updated
     * and it is moved to the tail.
     * </p>
     *
     * @param key the key to check for membership
     * @return {@code true} if the key is present in the set; {@code false} otherwise.
     */
    public synchronized boolean contains(K key) {
        purgeExpired();
        Node node = map.get(key);
        if (node != null) {
            node.lastAccessTime = System.nanoTime();
            if (node != tail) {
                removeNode(node);
                addNodeAtTail(node);
            }
            return true;
        }
        return false;
    }

    /**
     * Retrieves the associated object for the given key.
     * <p>
     * Before retrieving, expired elements are purged. If the key is found, its last access time is updated
     * and it is moved to the tail.
     * </p>
     *
     * @param key the key whose associated object is to be returned.
     * @return the associated object if the key is present; {@code null} otherwise.
     */
    public synchronized V get(K key) {
        purgeExpired();
        Node node = map.get(key);
        if (node != null) {
            node.lastAccessTime = System.nanoTime();
            if (node != tail) {
                removeNode(node);
                addNodeAtTail(node);
            }
            return node.value;
        }
        return null;
    }

    /**
     * Removes the given key from the set.
     *
     * @param key the key to remove
     * @return {@code true} if the key was present and has been removed;
     *         {@code false} otherwise.
     */
    public synchronized boolean remove(K key) {
        Node node = map.remove(key);
        if (node != null) {
            removeNode(node);
            return true;
        }
        return false;
    }

    /**
     * Returns the current number of entries in the cache.
     *
     * @return the current number of entries in the cache.
     */
    public int size() {
        return map.size();
    }

    /**
     * Removes all entries from the cache.
     */
    public void clear() {
        map.clear();
        head = tail = null;
    }

    /**
     * Removes a node from the doubly-linked list.
     *
     * @param node the node to remove.
     */
    private void removeNode(Node node) {
        if (node.prev != null)
            node.prev.next = node.next;
        else
            head = node.next;
        if (node.next != null)
            node.next.prev = node.prev;
        else
            tail = node.prev;
        node.prev = null;
        node.next = null;
    }

    /**
     * Adds a node to the tail of the doubly-linked list.
     *
     * @param node the node to add.
     */
    private void addNodeAtTail(Node node) {
        if (tail == null) { // The list is empty.
            head = node;
            tail = node;
        } else {
            tail.next = node;
            node.prev = tail;
            tail = node;
        }
    }

    /**
     * Example usage of the {@code TimedSet}.
     *
     * @param args command line arguments (not used)
     * @throws InterruptedException if the thread sleep is interrupted
     */
    public static void main(String[] args) throws InterruptedException {
        // Create a TimedSet with an expiration time of 10 seconds.
        LRUCache<String,String> timedSet = new LRUCache<>(10, 0);
        String key = "hello";

        System.out.println("Adding key \"hello\" with associated value \"world\": " +
                timedSet.add(key, "world"));
        System.out.println("Contains \"hello\": " + timedSet.contains(key));
        System.out.println("Associated value for \"hello\": " + timedSet.get(key));

        // Wait for 11 seconds to let the key expire.
        System.out.println("Sleeping for 11 seconds...");
        Thread.sleep(11000);

        System.out.println("Contains \"hello\" after sleep: " + timedSet.contains(key));
        System.out.println("Associated value for \"hello\" after sleep: " + timedSet.get(key));
    }
}
