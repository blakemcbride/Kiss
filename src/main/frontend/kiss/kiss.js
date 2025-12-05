/**
 * Kiss DOM Helper Utilities
 * Native DOM API wrappers to replace jQuery functionality
 */

const Kiss = {
    // Radio button groups storage (moved from global scope)
    RadioButtons: {
        groups: {}
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
     * @param {HTMLElement} el
     */
    show: (el) => {
        if (el) {
            el.removeAttribute('hidden');
            // Must set explicit display value to override CSS rules like "display: none"
            // Check computed style to see if element is hidden by CSS
            if (getComputedStyle(el).display === 'none') {
                el.style.display = 'block';
            }
            el.style.visibility = 'visible';
        }
    },

    /**
     * Hide an element
     * @param {HTMLElement} el
     */
    hide: (el) => {
        if (el) {
            el.style.display = 'none';
        }
    },

    /**
     * Check if element is hidden
     * @param {HTMLElement} el
     * @returns {boolean}
     */
    isHidden: (el) => {
        if (!el) return true;
        return el.offsetParent === null ||
               getComputedStyle(el).display === 'none' ||
               getComputedStyle(el).visibility === 'hidden';
    },

    /**
     * Check if element is visible
     * @param {HTMLElement} el
     * @returns {boolean}
     */
    isVisible: (el) => !Kiss.isHidden(el),

    /**
     * Set CSS property
     * @param {HTMLElement} el
     * @param {string} prop - CSS property name
     * @param {string} val - CSS value
     */
    css: (el, prop, val) => {
        if (el) {
            el.style[prop] = val;
        }
    },

    /**
     * Get or set attribute
     * @param {HTMLElement} el
     * @param {string} name - Attribute name
     * @param {string} [val] - Value to set (omit to get)
     * @returns {string|null|undefined}
     */
    attr: (el, name, val) => {
        if (!el) return undefined;
        if (val === undefined) {
            return el.getAttribute(name);
        }
        el.setAttribute(name, val);
    },

    /**
     * Remove attribute
     * @param {HTMLElement} el
     * @param {string} name - Attribute name
     */
    removeAttr: (el, name) => {
        if (el) {
            el.removeAttribute(name);
        }
    },

    /**
     * Get or set property
     * @param {HTMLElement} el
     * @param {string} name - Property name
     * @param {*} [val] - Value to set (omit to get)
     * @returns {*}
     */
    prop: (el, name, val) => {
        if (!el) return undefined;
        if (val === undefined) {
            return el[name];
        }
        el[name] = val;
    },

    /**
     * Add event listener
     * @param {HTMLElement} el
     * @param {string} event - Event type
     * @param {Function} handler - Event handler
     */
    on: (el, event, handler) => {
        if (el) {
            el.addEventListener(event, handler);
        }
    },

    /**
     * Remove event listener
     * @param {HTMLElement} el
     * @param {string} event - Event type
     * @param {Function} handler - Event handler reference
     */
    off: (el, event, handler) => {
        if (el) {
            el.removeEventListener(event, handler);
        }
    },

    /**
     * Append HTML string to element
     * @param {HTMLElement} parent
     * @param {string} html - HTML string
     */
    append: (parent, html) => {
        if (parent) {
            parent.insertAdjacentHTML('beforeend', html);
        }
    },

    /**
     * Append child element
     * @param {HTMLElement} parent
     * @param {HTMLElement} child
     */
    appendChild: (parent, child) => {
        if (parent && child) {
            parent.appendChild(child);
        }
    },

    /**
     * Clear element contents
     * @param {HTMLElement} el
     */
    empty: (el) => {
        if (el) {
            el.innerHTML = '';
        }
    },

    /**
     * Remove element from DOM
     * @param {HTMLElement} el
     */
    remove: (el) => {
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
     * @param {HTMLElement} el
     * @returns {HTMLElement[]}
     */
    children: (el) => el ? Array.from(el.children) : [],

    /**
     * Find single descendant
     * @param {HTMLElement} el
     * @param {string} selector
     * @returns {Element|null}
     */
    find: (el, selector) => el ? el.querySelector(selector) : null,

    /**
     * Find all descendants
     * @param {HTMLElement} el
     * @param {string} selector
     * @returns {NodeList}
     */
    findAll: (el, selector) => el ? el.querySelectorAll(selector) : [],

    /**
     * Get next sibling element
     * @param {HTMLElement} el
     * @returns {Element|null}
     */
    next: (el) => el ? el.nextElementSibling : null,

    /**
     * Get parent element
     * @param {HTMLElement} el
     * @returns {Element|null}
     */
    parent: (el) => el ? el.parentElement : null,

    /**
     * Get closest ancestor matching selector
     * @param {HTMLElement} el
     * @param {string} selector
     * @returns {Element|null}
     */
    closest: (el, selector) => el ? el.closest(selector) : null,

    /**
     * Get element offset (position relative to document)
     * @param {HTMLElement} el
     * @returns {{top: number, left: number}}
     */
    offset: (el) => {
        if (!el) return { top: 0, left: 0 };
        const rect = el.getBoundingClientRect();
        return {
            top: rect.top + window.scrollY,
            left: rect.left + window.scrollX
        };
    },

    /**
     * Set element offset (absolute position)
     * @param {HTMLElement} el
     * @param {{top: number, left: number}} pos
     */
    setOffset: (el, { top, left }) => {
        if (el) {
            el.style.position = 'absolute';
            el.style.top = top + 'px';
            el.style.left = left + 'px';
        }
    },

    /**
     * Get or set form element value
     * @param {HTMLElement} el
     * @param {*} [value] - Value to set (omit to get)
     * @returns {*}
     */
    val: (el, value) => {
        if (!el) return undefined;
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
     * @param {HTMLElement} el
     * @param {string} cls - Class name
     */
    addClass: (el, cls) => {
        if (el) {
            el.classList.add(cls);
        }
    },

    /**
     * Remove CSS class
     * @param {HTMLElement} el
     * @param {string} cls - Class name
     */
    removeClass: (el, cls) => {
        if (el) {
            el.classList.remove(cls);
        }
    },

    /**
     * Check if element has class
     * @param {HTMLElement} el
     * @param {string} cls - Class name
     * @returns {boolean}
     */
    hasClass: (el, cls) => el ? el.classList.contains(cls) : false,

    /**
     * Toggle CSS class
     * @param {HTMLElement} el
     * @param {string} cls - Class name
     */
    toggleClass: (el, cls) => {
        if (el) {
            el.classList.toggle(cls);
        }
    },

    /**
     * Get element width
     * @param {HTMLElement} el
     * @returns {number}
     */
    width: (el) => el ? el.offsetWidth : 0,

    /**
     * Set element width
     * @param {HTMLElement} el
     * @param {number|string} val
     */
    setWidth: (el, val) => {
        if (el) {
            el.style.width = typeof val === 'number' ? val + 'px' : val;
        }
    },

    /**
     * Get element height
     * @param {HTMLElement} el
     * @returns {number}
     */
    height: (el) => el ? el.offsetHeight : 0,

    /**
     * Set element height
     * @param {HTMLElement} el
     * @param {number|string} val
     */
    setHeight: (el, val) => {
        if (el) {
            el.style.height = typeof val === 'number' ? val + 'px' : val;
        }
    },

    /**
     * Focus element
     * @param {HTMLElement} el
     */
    focus: (el) => {
        if (el) {
            el.focus();
        }
    },

    /**
     * Blur element
     * @param {HTMLElement} el
     */
    blur: (el) => {
        if (el) {
            el.blur();
        }
    },

    /**
     * Trigger click event
     * @param {HTMLElement} el
     */
    click: (el) => {
        if (el) {
            el.click();
        }
    },

    /**
     * Get/set text content
     * @param {HTMLElement} el
     * @param {string} [text] - Text to set (omit to get)
     * @returns {string|undefined}
     */
    text: (el, text) => {
        if (!el) return undefined;
        if (text === undefined) {
            return el.textContent;
        }
        el.textContent = text;
    },

    /**
     * Get/set HTML content
     * @param {HTMLElement} el
     * @param {string} [html] - HTML to set (omit to get)
     * @returns {string|undefined}
     */
    html: (el, html) => {
        if (!el) return undefined;
        if (html === undefined) {
            return el.innerHTML;
        }
        el.innerHTML = html;
    },

    /**
     * Replace element with new content
     * @param {HTMLElement} el
     * @param {string|HTMLElement} content - HTML string or element
     */
    replaceWith: (el, content) => {
        if (!el) return;
        if (typeof content === 'string') {
            el.outerHTML = content;
        } else {
            el.replaceWith(content);
        }
    },

    /**
     * Wrap element's inner content in a new element
     * @param {HTMLElement} el
     * @param {string} wrapperTag - Tag name for wrapper
     * @returns {HTMLElement} The wrapper element
     */
    wrapInner: (el, wrapperTag = 'div') => {
        if (!el) return null;
        const wrapper = document.createElement(wrapperTag);
        while (el.firstChild) {
            wrapper.appendChild(el.firstChild);
        }
        el.appendChild(wrapper);
        return wrapper;
    },

    /**
     * Get all attributes as object
     * @param {HTMLElement} el
     * @returns {Object}
     */
    getAllAttributes: (el) => {
        if (!el) return {};
        const ret = {};
        Array.from(el.attributes).forEach(attr => {
            ret[attr.name] = attr.value;
        });
        return ret;
    },

    /**
     * Get/set caret position in input/textarea
     * Converted from jQuery caret plugin
     * @param {HTMLElement} target
     * @param {number|Array} [pos] - Position to set (omit to get)
     * @returns {number|{start: number, end: number}|undefined}
     */
    caret: (target, pos) => {
        if (!target) return undefined;

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
    }
};
