package org.kissweb;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * A thread-safe timed set that stores elements of type {@code T} and discards those that have not
 * been accessed (via {@code add} or {@code contains}) within the specified expiration time.
 * <p>
 * This implementation maintains a doubly-linked list to order elements by their last access time.
 * The head of the list is the least-recently accessed element while the tail is the most-recently
 * accessed. Each node in the list stores its last access time. When an element is added or its
 * membership is checked, its last access time is updated and it is moved to the tail of the list.
 * The {@code purgeExpired} method only examines the head of the list, removing expired elements
 * until the head is fresh.
 * </p>
 *
 * @param <T> the type of elements maintained by this set
 */
public class TimedSet<T> {

    /** Expiration duration in nanoseconds for this instance. */
    private final long expirationNanos;

    /**
     * A node in the doubly-linked list representing an element and its last access time.
     */
    private class Node {
        T element;
        long lastAccessTime;
        Node prev;
        Node next;

        Node(T element, long lastAccessTime) {
            this.element = element;
            this.lastAccessTime = lastAccessTime;
        }
    }

    /** Map for quick lookup of elements to their corresponding nodes. */
    private final Map<T, Node> map = new HashMap<>();
    /** Head of the doubly-linked list (least-recently accessed). */
    private Node head;
    /** Tail of the doubly-linked list (most-recently accessed). */
    private Node tail;

    /**
     * Creates a new {@code TimedSet} with the specified expiration time.
     *
     * @param expirationSeconds the number of seconds after which an element is considered expired
     */
    public TimedSet(long expirationSeconds) {
        this.expirationNanos = TimeUnit.SECONDS.toNanos(expirationSeconds);
    }

    /**
     * Private no-argument constructor to prevent instantiation without an expiration time.
     * Even if called via reflection, this will throw an exception.
     */
    @SuppressWarnings("unused")
    private TimedSet() {
        throw new UnsupportedOperationException("No-argument constructor is not supported. Use TimedSet(long expirationSeconds) instead.");
    }

    /**
     * Purges expired elements from the head of the list.
     * <p>
     * This method checks the element at the head of the list and removes it if it has not been
     * accessed within the expiration time. It repeats this process until the head is fresh or the list
     * is empty.
     * </p>
     */
    private void purgeExpired() {
        long now = System.nanoTime();
        while (head != null && now - head.lastAccessTime > expirationNanos) {
            map.remove(head.element);
            head = head.next;
            if (head != null) {
                head.prev = null;
            } else { // List became empty.
                tail = null;
            }
        }
    }

    /**
     * Adds the given element to the set.
     * <p>
     * Before adding, any expired elements are purged. If the element already exists, its last access
     * time is updated and it is moved to the tail.
     * </p>
     *
     * @param element the element to add
     * @return {@code true} if the element was not already present and has been added;
     *         {@code false} if the element was already present
     */
    public synchronized boolean add(T element) {
        purgeExpired();
        long now = System.nanoTime();
        Node node = map.get(element);
        if (node != null) {
            // Element already exists; update its access time and move it to the tail.
            node.lastAccessTime = now;
            if (node != tail) {
                removeNode(node);
                addNodeAtTail(node);
            }
            return false;
        } else {
            Node newNode = new Node(element, now);
            map.put(element, newNode);
            addNodeAtTail(newNode);
            return true;
        }
    }

    /**
     * Checks if the set contains the given element.
     * <p>
     * Before checking, expired elements are purged. If the element is found, its last access time
     * is updated and it is moved to the tail.
     * </p>
     *
     * @param element the element to check for membership
     * @return {@code true} if the element is present in the set; {@code false} otherwise
     */
    public synchronized boolean contains(T element) {
        purgeExpired();
        Node node = map.get(element);
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
     * Removes the given element from the set.
     *
     * @param element the element to remove
     * @return {@code true} if the element was present and has been removed;
     *         {@code false} otherwise
     */
    public synchronized boolean remove(T element) {
        Node node = map.remove(element);
        if (node != null) {
            removeNode(node);
            return true;
        }
        return false;
    }

    /**
     * Removes a node from the doubly-linked list.
     *
     * @param node the node to remove
     */
    private void removeNode(Node node) {
        if (node.prev != null) {
            node.prev.next = node.next;
        } else {
            head = node.next;
        }
        if (node.next != null) {
            node.next.prev = node.prev;
        } else {
            tail = node.prev;
        }
        node.prev = null;
        node.next = null;
    }

    /**
     * Adds a node to the tail of the doubly-linked list.
     *
     * @param node the node to add
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
        TimedSet<String> timedSet = new TimedSet<>(10);
        String element = "hello";

        System.out.println("Adding element \"hello\": " + timedSet.add(element));
        System.out.println("Contains \"hello\": " + timedSet.contains(element));

        // Wait for 11 seconds to let the element expire.
        System.out.println("Sleeping for 11 seconds...");
        Thread.sleep(11000);

        System.out.println("Contains \"hello\" after sleep: " + timedSet.contains(element));
    }
}
