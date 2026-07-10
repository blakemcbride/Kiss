/*
      Author: Blake McBride
 */

/* global Utils, Component, DOMUtils */

'use strict';

(function () {

    const DEFAULTS = {
        type: 'info',
        position: 'top-center',
        duration: 3000,
        autoDismiss: true,
        maxVisible: 3,
        dismissible: true
    };

    const TYPES = {
        info: true,
        success: true,
        warning: true,
        error: true
    };

    const POSITIONS = {
        'top-left': true,
        'top-center': true,
        'top-right': true,
        'bottom-left': true,
        'bottom-center': true,
        'bottom-right': true
    };

    function removeElement(el) {
        if (!el)
            return;
        if (el.remove)
            el.remove();
        else if (el.parentNode)
            el.parentNode.removeChild(el);
    }

    function cleanType(type) {
        type = String(type || DEFAULTS.type).toLowerCase();
        return TYPES[type] ? type : DEFAULTS.type;
    }

    function cleanPosition(position) {
        position = String(position || DEFAULTS.position).toLowerCase();
        return POSITIONS[position] ? position : DEFAULTS.position;
    }

    function cleanNumber(val, fallback, min) {
        val = Number(val);
        if (!Number.isFinite(val))
            return fallback;
        if (min !== undefined && val < min)
            return min;
        return val;
    }

    function cleanBoolean(val, fallback) {
        if (val === undefined || val === null)
            return fallback;
        if (typeof val === 'boolean')
            return val;
        val = String(val).toLowerCase();
        return !(val === 'false' || val === 'no' || val === '0');
    }

    function normalizeOptions(options, defaults) {
        if (typeof options === 'string')
            options = {type: options};
        const raw = options || {};
        const normalized = Object.assign({}, defaults || DEFAULTS, raw);
        if (raw.time !== undefined && raw.duration === undefined)
            normalized.duration = raw.time;
        if (raw.timeout !== undefined && raw.duration === undefined)
            normalized.duration = raw.timeout;
        if (raw.ms !== undefined && raw.duration === undefined)
            normalized.duration = raw.ms;
        if (raw.manual === true)
            normalized.autoDismiss = false;
        normalized.type = cleanType(normalized.type);
        normalized.position = cleanPosition(normalized.position);
        normalized.autoDismiss = cleanBoolean(normalized.autoDismiss, DEFAULTS.autoDismiss);
        normalized.duration = cleanNumber(normalized.duration, DEFAULTS.duration, 0);
        if (!normalized.autoDismiss)
            normalized.duration = 0;
        normalized.maxVisible = cleanNumber(normalized.maxVisible, DEFAULTS.maxVisible, 1);
        normalized.dismissible = cleanBoolean(normalized.dismissible, DEFAULTS.dismissible);
        if (raw.manual === true)
            normalized.dismissible = true;
        return normalized;
    }

    function setRegionPosition(region, position) {
        for (const pos in POSITIONS)
            region.classList.remove('kiss-toast-region-' + pos);
        region.classList.add('kiss-toast-region-' + position);
        region.kissToastPosition = position;
        syncRegionTopOffset(region);
    }

    function syncRegionTopOffset(region) {
        const position = region.kissToastPosition || DEFAULTS.position;
        if (position.indexOf('top-') !== 0) {
            region.style.removeProperty('--kiss-toast-top-offset');
            return;
        }

        const nav = document.querySelector('.app-nav');
        if (!nav) {
            region.style.removeProperty('--kiss-toast-top-offset');
            return;
        }

        const rect = nav.getBoundingClientRect();
        if (rect.height <= 0 || rect.bottom <= 0) {
            region.style.removeProperty('--kiss-toast-top-offset');
            return;
        }
        region.style.setProperty('--kiss-toast-top-offset', Math.round(rect.bottom + 12) + 'px');
    }

    function createSvgIcon(type) {
        const svgNS = 'http://www.w3.org/2000/svg';
        const svg = document.createElementNS(svgNS, 'svg');
        svg.setAttribute('viewBox', '0 0 16 16');
        svg.setAttribute('width', '15');
        svg.setAttribute('height', '15');
        svg.setAttribute('fill', 'none');
        svg.setAttribute('aria-hidden', 'true');

        function add(tag, attrs) {
            const node = document.createElementNS(svgNS, tag);
            for (const name in attrs)
                node.setAttribute(name, attrs[name]);
            svg.appendChild(node);
        }

        if (type === 'success') {
            add('path', {
                d: 'M3.5 8.2 6.6 11.3 12.6 4.8',
                stroke: 'currentColor',
                'stroke-width': '1.9',
                'stroke-linecap': 'round',
                'stroke-linejoin': 'round'
            });
        } else if (type === 'error') {
            add('circle', {
                cx: '8',
                cy: '8',
                r: '5.6',
                stroke: 'currentColor',
                'stroke-width': '1.45'
            });
            add('path', {
                d: 'M8 4.9v4',
                stroke: 'currentColor',
                'stroke-width': '1.65',
                'stroke-linecap': 'round'
            });
            add('path', {
                d: 'M8 11.2h.01',
                stroke: 'currentColor',
                'stroke-width': '2',
                'stroke-linecap': 'round'
            });
        } else {
            add('circle', {
                cx: '8',
                cy: '8',
                r: '4.4',
                fill: 'currentColor'
            });
        }

        return svg;
    }

    function ToastController(region, defaults) {
        this.region = region;
        this.defaults = normalizeOptions(defaults || {});
        this.items = [];
        setRegionPosition(region, this.defaults.position);
        region.classList.add('kiss-toast-region');
        region.setAttribute('aria-live', 'polite');
        region.setAttribute('aria-atomic', 'false');
    }

    ToastController.prototype.configure = function (options) {
        this.defaults = normalizeOptions(options, this.defaults);
        setRegionPosition(this.region, this.defaults.position);
        return this;
    };

    ToastController.prototype.dismiss = function (entry, immediate) {
        if (!entry || entry.closed)
            return this;
        entry.closed = true;
        window.clearTimeout(entry.timer);

        const idx = this.items.indexOf(entry);
        if (idx >= 0)
            this.items.splice(idx, 1);

        if (immediate) {
            removeElement(entry.element);
            return this;
        }

        entry.element.classList.remove('is-visible');
        entry.element.classList.add('is-leaving');
        window.setTimeout(function () {
            removeElement(entry.element);
        }, 220);
        return this;
    };

    ToastController.prototype.clear = function () {
        const items = this.items.slice();
        for (let i = 0; i < items.length; i++)
            this.dismiss(items[i], true);
        return this;
    };

    ToastController.prototype.trim = function (maxVisible) {
        while (this.items.length > maxVisible)
            this.dismiss(this.items[0], true);
    };

    ToastController.prototype.show = function (message, options) {
        const opts = normalizeOptions(options, this.defaults);
        const toast = document.createElement('div');
        const body = document.createElement('div');
        const text = document.createElement('div');
        const icon = document.createElement('span');
        const entry = {element: toast, timer: null, closed: false};

        syncRegionTopOffset(this.region);
        toast.className = 'kiss-toast kiss-toast-' + opts.type;
        toast.setAttribute('role', opts.type === 'error' ? 'alert' : 'status');

        icon.className = 'kiss-toast-icon';
        icon.setAttribute('aria-hidden', 'true');
        if (opts.type === 'success') {
            const successCheck = document.createElement('span');
            successCheck.className = 't-success-check';
            successCheck.setAttribute('data-state', 'out');
            successCheck.setAttribute('aria-hidden', 'true');
            successCheck.appendChild(createSvgIcon(opts.type));
            icon.appendChild(successCheck);
        } else {
            icon.appendChild(createSvgIcon(opts.type));
        }
        toast.appendChild(icon);

        body.className = 'kiss-toast-body';
        if (opts.title) {
            const title = document.createElement('div');
            title.className = 'kiss-toast-title';
            title.textContent = String(opts.title);
            body.appendChild(title);
        }
        text.className = 'kiss-toast-text';
        text.textContent = message === undefined || message === null ? '' : String(message);
        body.appendChild(text);
        toast.appendChild(body);

        if (opts.dismissible) {
            const close = document.createElement('button');
            close.type = 'button';
            close.className = 'kiss-toast-close';
            close.setAttribute('aria-label', 'Dismiss notification');
            close.textContent = 'x';
            close.addEventListener('click', (e) => {
                e.preventDefault();
                e.stopPropagation();
                this.dismiss(entry);
            });
            toast.appendChild(close);
        }

        this.region.appendChild(toast);
        this.items.push(entry);
        this.trim(opts.maxVisible);

        window.requestAnimationFrame(function () {
            if (!entry.closed) {
                toast.classList.add('is-visible');
                const successCheck = toast.querySelector('.t-success-check');
                if (successCheck)
                    successCheck.setAttribute('data-state', 'in');
            }
        });

        if (opts.duration > 0) {
            entry.timer = window.setTimeout(() => {
                this.dismiss(entry);
            }, opts.duration);
        }

        return {
            element: toast,
            close: () => this.dismiss(entry)
        };
    };

    ToastController.prototype.success = function (message, options) {
        return this.show(message, Object.assign({}, options || {}, {type: 'success'}));
    };

    ToastController.prototype.info = function (message, options) {
        return this.show(message, Object.assign({}, options || {}, {type: 'info'}));
    };

    ToastController.prototype.warning = function (message, options) {
        return this.show(message, Object.assign({}, options || {}, {type: 'warning'}));
    };

    ToastController.prototype.error = function (message, options) {
        return this.show(message, Object.assign({}, options || {}, {type: 'error'}));
    };

    ToastController.prototype.manual = function (message, options) {
        return this.show(message, Object.assign({}, options || {}, {manual: true}));
    };

    function createGlobalRegion(position) {
        const id = 'kiss-toast-region-' + position;
        let region = DOMUtils.getElement(id);
        if (!region) {
            region = document.createElement('div');
            region.id = id;
            region.className = 'kiss-toast-region';
            document.body.appendChild(region);
        }
        return region;
    }

    const Toast = {
        controllers: {},
        getController: function (position) {
            position = cleanPosition(position);
            if (!Toast.controllers[position])
                Toast.controllers[position] = new ToastController(createGlobalRegion(position), {position: position});
            return Toast.controllers[position];
        },
        show: function (message, options) {
            const opts = normalizeOptions(options || {});
            return Toast.getController(opts.position).show(message, opts);
        }
    };

    Toast.success = function (message, options) {
        return Toast.show(message, Object.assign({}, options || {}, {type: 'success'}));
    };
    Toast.info = function (message, options) {
        return Toast.show(message, Object.assign({}, options || {}, {type: 'info'}));
    };
    Toast.warning = function (message, options) {
        return Toast.show(message, Object.assign({}, options || {}, {type: 'warning'}));
    };
    Toast.error = function (message, options) {
        return Toast.show(message, Object.assign({}, options || {}, {type: 'error'}));
    };
    Toast.manual = function (message, options) {
        return Toast.show(message, Object.assign({}, options || {}, {manual: true}));
    };
    Toast.clear = function (position) {
        if (position) {
            const controller = Toast.controllers[cleanPosition(position)];
            if (controller)
                controller.clear();
            return;
        }
        for (const pos in Toast.controllers)
            Toast.controllers[pos].clear();
    };

    Utils.toast = function (message, options) {
        return Toast.show(message, options);
    };
    Utils.toast.success = Toast.success;
    Utils.toast.info = Toast.info;
    Utils.toast.warning = Toast.warning;
    Utils.toast.error = Toast.error;
    Utils.toast.manual = Toast.manual;
    Utils.toast.clear = Toast.clear;
    Utils.showToast = Utils.toast;

    const processor = function (elm, attr, content) {
        let id;
        let nattrs = '';
        let nstyle = '';
        let nclass = '';
        const defaults = {};

        for (let prop in attr) {
            switch (prop) {
                case 'id':
                    id = Utils.removeQuotes(attr[prop]);
                    break;
                case 'class':
                    nclass = ' ' + Utils.removeQuotes(attr[prop]);
                    break;
                case 'style':
                    nstyle = attr[prop];
                    break;
                case 'type':
                    defaults.type = Utils.removeQuotes(attr[prop]);
                    break;
                case 'position':
                    defaults.position = Utils.removeQuotes(attr[prop]);
                    break;
                case 'duration':
                    defaults.duration = Utils.removeQuotes(attr[prop]);
                    break;
                case 'time':
                    defaults.time = Utils.removeQuotes(attr[prop]);
                    break;
                case 'timeout':
                    defaults.timeout = Utils.removeQuotes(attr[prop]);
                    break;
                case 'auto-dismiss':
                    defaults.autoDismiss = Utils.removeQuotes(attr[prop]);
                    break;
                case 'manual':
                    defaults.manual = true;
                    break;
                case 'max-visible':
                    defaults.maxVisible = Utils.removeQuotes(attr[prop]);
                    break;
                case 'no-dismiss':
                    defaults.dismissible = false;
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        const newElm = Utils.replaceHTML(id, elm, '<div class="kiss-toast-region{class}" style="{style}" {attr} id="{id}"></div>', {
            class: nclass,
            style: nstyle,
            attr: nattrs
        });
        if (!newElm)
            return;

        const controller = new ToastController(newElm.element, defaults);
        newElm.show = function (message, options) {
            return controller.show(message, options);
        };
        newElm.toast = newElm.show;
        newElm.notify = newElm.show;
        newElm.success = function (message, options) {
            return controller.success(message, options);
        };
        newElm.info = function (message, options) {
            return controller.info(message, options);
        };
        newElm.warning = function (message, options) {
            return controller.warning(message, options);
        };
        newElm.error = function (message, options) {
            return controller.error(message, options);
        };
        newElm.manual = function (message, options) {
            return controller.manual(message, options);
        };
        newElm.clear = function () {
            controller.clear();
            return this;
        };
        newElm.configure = function (options) {
            controller.configure(options);
            return this;
        };

        if (content && content.trim())
            controller.show(content.trim());
    };

    const componentInfo = {
        name: 'Toast',
        tag: 'toast',
        processor: processor
    };
    Utils.newComponent(componentInfo);

    Component.Toast.Controller = ToastController;
    Component.Toast.show = Toast.show;
    Component.Toast.success = Toast.success;
    Component.Toast.info = Toast.info;
    Component.Toast.warning = Toast.warning;
    Component.Toast.error = Toast.error;
    Component.Toast.manual = Toast.manual;
    Component.Toast.clear = Toast.clear;

})();
