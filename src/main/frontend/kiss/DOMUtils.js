/**
 * DOMUtils DOM Helper Utilities
 * Native DOM API wrappers to replace jQuery functionality
 */
const DOMUtils = {
    // Radio button groups storage (moved from global scope)
    RadioButtons: {
        groups: {}
    },

    // Track event handlers to ensure only one handler per event type per element
    _eventHandlers: new WeakMap(),

    /**
     * Internal helper to resolve element from string or element
     * @param {string|HTMLElement} elementOrId - Element or ID string
     * @returns {HTMLElement|null}
     */
    _resolveElement: (elementOrId) => {
        if (typeof elementOrId === 'string') {
            const element = DOMUtils.getElement(elementOrId);
            if (!element) {
                console.log(`DOMUtils: Element with id "${elementOrId}" not found`);
            }
            return element;
        }
        return elementOrId;
    },

    /**
     * Get element by ID (handles optional # prefix)
     * @param {string} id - Element ID (with or without #)
     * @returns {HTMLElement|null}
     */
    getElement: (id) => document.getElementById(id.replace(/^#/, '')),

    /**
     * Query single element
     * @param {string} selector - CSS selector
     * @param {Element} parent - Parent element (default: document)
     * @returns {Element|null}
     */
    query: (selector, parent = document) => parent.querySelector(selector),

    /**
     * Query all matching elements
     * @param {string} selector - CSS selector
     * @param {Element} parent - Parent element (default: document)
     * @returns {NodeList}
     */
    queryAll: (selector, parent = document) => parent.querySelectorAll(selector),

    /**
     * Show an element
     * @param {HTMLElement|string} el - Element or element ID
     */
    show: (el) => {
        el = DOMUtils._resolveElement(el);
        if (el) {
            el.removeAttribute('hidden');
            // Restore original display if we stored it, otherwise clear inline style
            const originalDisplay = el.dataset.originalDisplay;
            if (originalDisplay !== undefined) {
                el.style.display = originalDisplay;
            } else {
                // Temporarily clear inline display to check what CSS would apply
                const currentInlineDisplay = el.style.display;
                el.style.display = '';

                // Get the computed style after clearing inline display
                const computedDisplay = getComputedStyle(el).display;

                // If CSS (not inline) is making it 'none', we need to override
                if (computedDisplay === 'none') {
                    // Try to determine the appropriate display value
                    // 1. Check if element has inline-block children (like radio buttons)
                    const hasInlineBlockChild = el.querySelector('[style*="inline-block"]') !== null;
                    // 2. Check for common inline elements
                    const isInlineElement = ['SPAN', 'A', 'LABEL', 'INPUT', 'BUTTON'].includes(el.tagName);

                    if (hasInlineBlockChild || isInlineElement) {
                        el.style.display = 'inline-block';
                    } else {
                        el.style.display = 'block';
                    }
                } else if (currentInlineDisplay && currentInlineDisplay !== 'none') {
                    // If there was a previous inline display value, restore it
                    el.style.display = currentInlineDisplay;
                }
                // Otherwise leave it cleared - CSS will handle it
            }
            el.style.visibility = 'visible';
        }
    },

    /**
     * Hide an element
     * @param {HTMLElement|string} el - Element or element ID
     */
    hide: (el) => {
        el = DOMUtils._resolveElement(el);
        if (el) {
            // Store original display before hiding (only if currently visible)
            const computedDisplay = getComputedStyle(el).display;
            if (computedDisplay !== 'none') {
                el.dataset.originalDisplay = computedDisplay;
            }
            el.setAttribute('hidden', '');
            el.style.display = 'none';
            el.style.visibility = 'hidden';
        }
    },

    /**
     * Check if element is hidden
     * @param {HTMLElement|string} el - Element or element ID
     * @returns {boolean}
     */
    isHidden: (el) => {
        el = DOMUtils._resolveElement(el);
        if (!el)
            return true;
        return el.offsetParent === null ||
               getComputedStyle(el).display === 'none' ||
               getComputedStyle(el).visibility === 'hidden';
    },

    /**
     * Check if element is visible
     * @param {HTMLElement|string} el - Element or element ID
     * @returns {boolean}
     */
    isVisible: (el) => !DOMUtils.isHidden(el),

    /**
     * Set CSS property
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} prop - CSS property name
     * @param {string} val - CSS value
     */
    css: (el, prop, val) => {
        el = DOMUtils._resolveElement(el);
        if (el) {
            el.style[prop] = val;
        }
    },

    /**
     * Get or set attribute
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} name - Attribute name
     * @param {string} [val] - Value to set (omit to get)
     * @returns {string|null|undefined}
     */
    attr: (el, name, val) => {
        el = DOMUtils._resolveElement(el);
        if (!el)
            return undefined;
        if (val === undefined) {
            return el.getAttribute(name);
        }
        el.setAttribute(name, val);
    },

    /**
     * Remove attribute
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} name - Attribute name
     */
    removeAttr: (el, name) => {
        el = DOMUtils._resolveElement(el);
        if (el) {
            el.removeAttribute(name);
        }
    },

    /**
     * Get or set property
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} name - Property name
     * @param {*} [val] - Value to set (omit to get)
     * @returns {*}
     */
    prop: (el, name, val) => {
        el = DOMUtils._resolveElement(el);
        if (!el)
            return undefined;
        if (val === undefined) {
            return el[name];
        }
        el[name] = val;
    },

    /**
     * Add event listener
     * Automatically removes any previous handler for the same event type to ensure only one handler at a time
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} event - Event type
     * @param {Function} handler - Event handler (pass null to remove handler without adding a new one)
     */
    on: (el, event, handler) => {
        el = DOMUtils._resolveElement(el);
        if (!el)
            return;

        // Get or create the handlers map for this element
        let handlersMap = DOMUtils._eventHandlers.get(el);
        if (!handlersMap) {
            handlersMap = new Map();
            DOMUtils._eventHandlers.set(el, handlersMap);
        }

        // Remove previous handler for this event type if it exists
        const previousHandler = handlersMap.get(event);
        if (previousHandler) {
            el.removeEventListener(event, previousHandler);
            handlersMap.delete(event);
        }

        // Add new handler if provided
        if (handler) {
            el.addEventListener(event, handler);
            handlersMap.set(event, handler);
        }
    },

    /**
     * Remove event listener
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} event - Event type
     * @param {Function} [handler] - Event handler reference (optional - if not provided, removes tracked handler)
     */
    off: (el, event, handler) => {
        el = DOMUtils._resolveElement(el);
        if (!el)
            return;

        // If no specific handler provided, remove the tracked handler
        if (!handler) {
            const handlersMap = DOMUtils._eventHandlers.get(el);
            if (handlersMap) {
                const trackedHandler = handlersMap.get(event);
                if (trackedHandler) {
                    el.removeEventListener(event, trackedHandler);
                    handlersMap.delete(event);
                }
            }
        } else {
            // Remove the specific handler
            el.removeEventListener(event, handler);
            // Also remove from tracking if it matches
            const handlersMap = DOMUtils._eventHandlers.get(el);
            if (handlersMap && handlersMap.get(event) === handler) {
                handlersMap.delete(event);
            }
        }
    },

    /**
     * Append HTML string to element
     * @param {HTMLElement|string} parent - Element or element ID
     * @param {string} html - HTML string
     */
    append: (parent, html) => {
        parent = DOMUtils._resolveElement(parent);
        if (parent) {
            parent.insertAdjacentHTML('beforeend', html);
        }
    },

    /**
     * Append child element
     * @param {HTMLElement|string} parent - Element or element ID
     * @param {HTMLElement} child
     */
    appendChild: (parent, child) => {
        parent = DOMUtils._resolveElement(parent);
        if (parent && child) {
            parent.appendChild(child);
        }
    },

    /**
     * Clear element contents
     * @param {HTMLElement|string} el - Element or element ID
     */
    empty: (el) => {
        el = DOMUtils._resolveElement(el);
        if (el) {
            el.innerHTML = '';
        }
    },

    /**
     * Remove element from DOM
     * @param {HTMLElement|string} el - Element or element ID
     */
    remove: (el) => {
        el = DOMUtils._resolveElement(el);
        if (el) {
            el.remove();
        }
    },

    /**
     * Create element with optional attributes
     * @param {string} tag - Tag name
     * @param {Object} [attrs] - Attributes to set
     * @returns {HTMLElement}
     */
    create: (tag, attrs = {}) => {
        const el = document.createElement(tag);
        Object.entries(attrs).forEach(([k, v]) => el.setAttribute(k, v));
        return el;
    },

    /**
     * Get children as array
     * @param {HTMLElement|string} el - Element or element ID
     * @returns {HTMLElement[]}
     */
    children: (el) => {
        el = DOMUtils._resolveElement(el);
        return el ? Array.from(el.children) : [];
    },

    /**
     * Find single descendant
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} selector
     * @returns {Element|null}
     */
    find: (el, selector) => {
        el = DOMUtils._resolveElement(el);
        return el ? el.querySelector(selector) : null;
    },

    /**
     * Find all descendants
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} selector
     * @returns {NodeList}
     */
    findAll: (el, selector) => {
        el = DOMUtils._resolveElement(el);
        return el ? el.querySelectorAll(selector) : [];
    },

    /**
     * Get next sibling element
     * @param {HTMLElement|string} el - Element or element ID
     * @returns {Element|null}
     */
    next: (el) => {
        el = DOMUtils._resolveElement(el);
        return el ? el.nextElementSibling : null;
    },

    /**
     * Get parent element
     * @param {HTMLElement|string} el - Element or element ID
     * @returns {Element|null}
     */
    parent: (el) => {
        el = DOMUtils._resolveElement(el);
        return el ? el.parentElement : null;
    },

    /**
     * Get closest ancestor matching selector
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} selector
     * @returns {Element|null}
     */
    closest: (el, selector) => {
        el = DOMUtils._resolveElement(el);
        return el ? el.closest(selector) : null;
    },

    /**
     * Get element offset (position relative to document)
     * @param {HTMLElement|string} el - Element or element ID
     * @returns {{top: number, left: number}}
     */
    offset: (el) => {
        el = DOMUtils._resolveElement(el);
        if (!el)
            return { top: 0, left: 0 };
        const rect = el.getBoundingClientRect();
        return {
            top: rect.top + window.scrollY,
            left: rect.left + window.scrollX
        };
    },

    /**
     * Set element offset (absolute position)
     * Like jQuery's .offset() setter, this accounts for the element's offsetParent.
     * @param {HTMLElement|string} el - Element or element ID
     * @param {{top: number, left: number}} pos - Desired position relative to document
     */
    setOffset: (el, { top, left }) => {
        el = DOMUtils._resolveElement(el);
        if (!el) return;

        // Ensure element is positioned
        const currentPosition = getComputedStyle(el).position;
        if (currentPosition === 'static') {
            el.style.position = 'relative';
        }

        // Get current offset from document
        const curOffset = DOMUtils.offset(el);

        // Get current CSS top/left values (may be 'auto')
        const curCSSTop = parseFloat(getComputedStyle(el).top) || 0;
        const curCSSLeft = parseFloat(getComputedStyle(el).left) || 0;

        // Calculate the adjustment needed
        // The CSS top/left properties are relative to the offsetParent,
        // but offset() returns document-relative coordinates.
        // To set a document-relative position, we need to adjust for this difference.
        const props = {};
        if (top != null) {
            props.top = (top - curOffset.top) + curCSSTop;
        }
        if (left != null) {
            props.left = (left - curOffset.left) + curCSSLeft;
        }

        // Set the calculated values
        if (props.top !== undefined) {
            el.style.top = props.top + 'px';
        }
        if (props.left !== undefined) {
            el.style.left = props.left + 'px';
        }
    },

    /**
     * Get or set form element value
     * @param {HTMLElement|string} el - Element or element ID
     * @param {*} [value] - Value to set (omit to get)
     * @returns {*}
     */
    val: (el, value) => {
        el = DOMUtils._resolveElement(el);
        if (!el)
            return undefined;
        if (value === undefined) {
            return el.value;
        }
        el.value = value;
    },

    /**
     * Iterate over collection
     * @param {NodeList|Array} collection
     * @param {Function} fn - Callback (element, index)
     */
    each: (collection, fn) => {
        Array.from(collection).forEach(fn);
    },

    /**
     * Add CSS class
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} cls - Class name
     */
    addClass: (el, cls) => {
        el = DOMUtils._resolveElement(el);
        if (el) {
            el.classList.add(cls);
        }
    },

    /**
     * Remove CSS class
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} cls - Class name
     */
    removeClass: (el, cls) => {
        el = DOMUtils._resolveElement(el);
        if (el) {
            el.classList.remove(cls);
        }
    },

    /**
     * Check if element has class
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} cls - Class name
     * @returns {boolean}
     */
    hasClass: (el, cls) => {
        el = DOMUtils._resolveElement(el);
        return el ? el.classList.contains(cls) : false;
    },

    /**
     * Toggle CSS class
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} cls - Class name
     */
    toggleClass: (el, cls) => {
        el = DOMUtils._resolveElement(el);
        if (el) {
            el.classList.toggle(cls);
        }
    },

    /**
     * Get element width
     * @param {HTMLElement|string} el - Element or element ID
     * @returns {number}
     */
    width: (el) => {
        el = DOMUtils._resolveElement(el);
        return el ? el.offsetWidth : 0;
    },

    /**
     * Set element width
     * @param {HTMLElement|string} el - Element or element ID
     * @param {number|string} val
     */
    setWidth: (el, val) => {
        el = DOMUtils._resolveElement(el);
        if (el) {
            el.style.width = typeof val === 'number' ? val + 'px' : val;
        }
    },

    /**
     * Get element height
     * @param {HTMLElement|string} el - Element or element ID
     * @returns {number}
     */
    height: (el) => {
        el = DOMUtils._resolveElement(el);
        return el ? el.offsetHeight : 0;
    },

    /**
     * Set element height
     * @param {HTMLElement|string} el - Element or element ID
     * @param {number|string} val
     */
    setHeight: (el, val) => {
        el = DOMUtils._resolveElement(el);
        if (el) {
            el.style.height = typeof val === 'number' ? val + 'px' : val;
        }
    },

    /**
     * Focus element
     * @param {HTMLElement|string} el - Element or element ID
     */
    focus: (el) => {
        el = DOMUtils._resolveElement(el);
        if (el) {
            el.focus();
        }
    },

    /**
     * Blur element
     * @param {HTMLElement|string} el - Element or element ID
     */
    blur: (el) => {
        el = DOMUtils._resolveElement(el);
        if (el) {
            el.blur();
        }
    },

    /**
     * Trigger click event
     * @param {HTMLElement|string} el - Element or element ID
     */
    click: (el) => {
        el = DOMUtils._resolveElement(el);
        if (el) {
            el.click();
        }
    },

    /**
     * Get/set text content
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} [text] - Text to set (omit to get)
     * @returns {string|undefined}
     */
    text: (el, text) => {
        el = DOMUtils._resolveElement(el);
        if (!el)
            return undefined;
        if (text === undefined) {
            return el.textContent;
        }
        el.textContent = text;
    },

    /**
     * Get/set HTML content
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} [html] - HTML to set (omit to get)
     * @returns {string|undefined}
     */
    html: (el, html) => {
        el = DOMUtils._resolveElement(el);
        if (!el)
            return undefined;
        if (html === undefined) {
            return el.innerHTML;
        }
        el.innerHTML = html;
    },

    /**
     * Replace element with new content
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string|HTMLElement} content - HTML string or element
     */
    replaceWith: (el, content) => {
        el = DOMUtils._resolveElement(el);
        if (!el)
            return;
        if (typeof content === 'string') {
            el.outerHTML = content;
        } else {
            el.replaceWith(content);
        }
    },

    /**
     * Wrap element's inner content in a new element
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} wrapperTag - Tag name for wrapper
     * @returns {HTMLElement} The wrapper element
     */
    wrapInner: (el, wrapperTag = 'div') => {
        el = DOMUtils._resolveElement(el);
        if (!el)
            return null;
        const wrapper = document.createElement(wrapperTag);
        while (el.firstChild) {
            wrapper.appendChild(el.firstChild);
        }
        el.appendChild(wrapper);
        return wrapper;
    },

    /**
     * Get all attributes as object
     * @param {HTMLElement|string} el - Element or element ID
     * @returns {Object}
     */
    getAllAttributes: (el) => {
        el = DOMUtils._resolveElement(el);
        if (!el)
            return {};
        const ret = {};
        Array.from(el.attributes).forEach(attr => {
            ret[attr.name] = attr.value;
        });
        return ret;
    },

    /**
     * Get/set caret position in input/textarea
     * Converted from jQuery caret plugin
     * @param {HTMLElement|string} target - Element or element ID
     * @param {number|Array} [pos] - Position to set (omit to get)
     * @returns {number|{start: number, end: number}|undefined}
     */
    caret: (target, pos) => {
        target = DOMUtils._resolveElement(target);
        if (!target)
            return undefined;

        const isContentEditable = target.contentEditable === 'true';

        // Get caret position
        if (pos === undefined) {
            // Textarea/input
            if (target.selectionStart !== undefined) {
                return target.selectionStart;
            }
            // ContentEditable
            if (isContentEditable) {
                target.focus();
                const selection = window.getSelection();
                if (selection.rangeCount === 0) return 0;
                const range1 = selection.getRangeAt(0);
                const range2 = range1.cloneRange();
                range2.selectNodeContents(target);
                range2.setEnd(range1.endContainer, range1.endOffset);
                return range2.toString().length;
            }
            return undefined;
        }

        // Set caret position
        if (pos === -1) {
            pos = isContentEditable ? target.textContent.length : target.value.length;
        }

        // Handle array [start, end] for selection
        if (Array.isArray(pos)) {
            if (target.setSelectionRange) {
                target.setSelectionRange(pos[0], pos[1]);
            }
            return;
        }

        // Set single position
        if (target.setSelectionRange) {
            target.setSelectionRange(pos, pos);
        } else if (isContentEditable) {
            target.focus();
            const selection = window.getSelection();
            const range = document.createRange();

            // Find the text node and offset
            let charCount = 0;
            let found = false;

            const walker = document.createTreeWalker(
                target,
                NodeFilter.SHOW_TEXT,
                null,
                false
            );

            let node;
            while ((node = walker.nextNode()) && !found) {
                const nodeLength = node.textContent.length;
                if (charCount + nodeLength >= pos) {
                    range.setStart(node, pos - charCount);
                    range.setEnd(node, pos - charCount);
                    found = true;
                }
                charCount += nodeLength;
            }

            if (found) {
                selection.removeAllRanges();
                selection.addRange(range);
            }
        }
    },

    /**
     * Trigger an event on an element
     * Replacement for $('#el').trigger('eventName')
     * @param {HTMLElement|string} el - Element or element ID
     * @param {string} eventName - Name of the event (e.g., 'click', 'change')
     */
    trigger: (el, eventName) => {
        el = DOMUtils._resolveElement(el);
        if (!el)
            return;
        const event = new Event(eventName, {bubbles: true, cancelable: true});
        el.dispatchEvent(event);
    },

    /**
     * Execute callback when DOM is ready
     * Replacement for $(function() {}) pattern
     * @param {Function} callback - Function to execute when DOM is ready
     */
    documentReady: (callback) => {
        if (document.readyState === 'loading') {
            document.addEventListener('DOMContentLoaded', callback);
        } else {
            callback();
        }
    },

    /**
     * Get currently focused element
     * Replacement for $(':focus')
     * @returns {Element|null}
     */
    getFocusedElement: () => document.activeElement,

    /**
     * Fetch HTML content from URL
     * Replacement for $.get()
     * @param {string} url - URL to fetch
     * @param {Function} successCallback - Callback for success
     * @param {Function} [errorCallback] - Callback for error
     */
    fetchHTML: (url, successCallback, errorCallback) => {
        fetch(url)
            .then(response => {
                if (!response.ok) {
                    throw new Error(`HTTP error! status: ${response.status}`);
                }
                return response.text();
            })
            .then(data => {
                if (successCallback) {
                    successCallback(data);
                }
            })
            .catch(error => {
                if (errorCallback) {
                    errorCallback(error);
                } else {
                    console.error('Fetch failed:', error);
                }
            });
    },

    /**
     * Remove all event listeners from element
     * Replacement for $('#el').off() without parameters
     * @param {HTMLElement|string} el - Element or element ID
     * @returns {HTMLElement} The new cloned element
     */
    removeAllListeners: (el) => {
        el = DOMUtils._resolveElement(el);
        if (!el || !el.parentNode)
            return el;
        const newElement = el.cloneNode(true);
        el.parentNode.replaceChild(newElement, el);
        return newElement;
    },

    /**
     * Get element by ID and remove all its event listeners in one atomic operation.
     * This is useful for popup buttons that may have handlers attached multiple times.
     * @param {string} id - Element ID (with or without #)
     * @returns {HTMLElement|null} The new cloned element with listeners removed, or null if not found
     */
    getElementWithCleanListeners: (id) => {
        const el = DOMUtils.getElement(id);
        if (!el)
            return null;
        return DOMUtils.removeAllListeners(el);
    }
};
