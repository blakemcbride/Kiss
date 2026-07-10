/*
      Generic Kiss search input with clear button and optional result list.
 */

/* global Utils */

'use strict';

(function () {

    function esc(s) {
        const str = s == null ? '' : String(s);
        return str.replace(/&/g, '&amp;').replace(/</g, '&lt;').replace(/>/g, '&gt;')
            .replace(/"/g, '&quot;').replace(/'/g, '&#39;');
    }

    function resultHtml(item, idx, opts) {
        const valueField = opts.valueField || 'value';
        const labelField = opts.labelField || 'label';
        const descField = opts.descField || 'description';
        const metaField = opts.metaField || 'meta';
        const value = typeof item === 'object' ? item[valueField] : item;
        const label = typeof item === 'object' ? (item[labelField] || item[valueField]) : item;
        const desc = typeof item === 'object' ? item[descField] : '';
        const meta = typeof item === 'object' ? item[metaField] : '';
        const disabled = typeof item === 'object' && item.disabled;
        return '<button type="button" class="kiss-search-result" role="option" data-index="' + idx +
            '" data-value="' + esc(value) + '" data-label="' + esc(label) + '"' +
            (disabled ? ' disabled aria-disabled="true"' : '') + '>' +
                '<span class="kiss-search-result-main">' + esc(label) + '</span>' +
                (desc ? '<span class="kiss-search-result-desc">' + esc(desc) + '</span>' : '') +
                (meta ? '<span class="kiss-search-result-meta">' + esc(meta) + '</span>' : '') +
            '</button>';
    }

    function clamp(n, min, max) {
        return Math.min(max, Math.max(min, n));
    }

    function easeOutCubic(t) {
        return 1 - Math.pow(1 - clamp(t, 0, 1), 3);
    }

    function easeOutBack(t) {
        t = clamp(t, 0, 1);
        const c1 = 1.70158;
        const c3 = c1 + 1;
        return 1 + c3 * Math.pow(t - 1, 3) + c1 * Math.pow(t - 1, 2);
    }

    function cssNumber(name, fallback) {
        const raw = window.getComputedStyle(document.documentElement).getPropertyValue(name).trim();
        const val = parseFloat(raw);
        return Number.isFinite(val) ? val : fallback;
    }

    function cssMs(name, fallback) {
        const raw = window.getComputedStyle(document.documentElement).getPropertyValue(name).trim();
        if (!raw)
            return fallback;
        if (raw.endsWith('ms'))
            return parseFloat(raw) || fallback;
        if (raw.endsWith('s'))
            return (parseFloat(raw) || fallback / 1000) * 1000;
        return parseFloat(raw) || fallback;
    }

    function cssString(name, fallback) {
        const raw = window.getComputedStyle(document.documentElement).getPropertyValue(name).trim();
        return raw || fallback;
    }

    function prefersReducedMotion() {
        return window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;
    }

    const processor = function (elm, attr, content) {
        let id;
        let style = '';
        let classes = '';
        let nattrs = '';
        let value = '';
        let placeholder = '';
        let disabled = false;
        let required = false;
        let maxlength = '';
        let autocomplete = 'off';
        let spellcheck = 'false';
        let clearLabel = 'Clear search';
        let ariaLabel = '';

        for (let prop in attr) {
            switch (prop) {
                case 'id':
                    id = Utils.removeQuotes(attr[prop]);
                    break;
                case 'style':
                    style = attr[prop];
                    break;
                case 'class':
                    classes = attr[prop];
                    break;
                case 'value':
                    value = Utils.removeQuotes(attr[prop]);
                    break;
                case 'placeholder':
                    placeholder = Utils.removeQuotes(attr[prop]);
                    break;
                case 'disabled':
                    disabled = attr[prop] !== 'false';
                    break;
                case 'required':
                    required = attr[prop] !== 'false';
                    break;
                case 'maxlength':
                    maxlength = Utils.removeQuotes(attr[prop]);
                    break;
                case 'autocomplete':
                    autocomplete = Utils.removeQuotes(attr[prop]) || 'off';
                    break;
                case 'spellcheck':
                    spellcheck = Utils.removeQuotes(attr[prop]) || 'false';
                    break;
                case 'clear-label':
                    clearLabel = Utils.removeQuotes(attr[prop]) || clearLabel;
                    break;
                case 'aria-label':
                    ariaLabel = attr[prop];
                    break;
                default:
                    nattrs += ' ' + prop + '="' + attr[prop] + '"';
                    break;
            }
        }

        if (!ariaLabel)
            ariaLabel = placeholder || 'Search';
        if (!id)
            id = Utils.nextID();
        const rootClass = 'kiss-search-input' + (value ? ' has-value' : '') + (classes ? ' ' + classes : '');
        const maxAttr = maxlength ? ' maxlength="' + esc(maxlength) + '"' : '';
        const newElm = Utils.replaceHTML(id, elm,
            '<div id="{id}" class="{class}" style="{style}" {attr}>' +
                '<div id="{id}-control" class="kiss-search-control t-clear{valueClass}">' +
                    '<span class="kiss-search-icon" aria-hidden="true"><svg width="15" height="15" viewBox="0 0 16 16" fill="none"><circle cx="7" cy="7" r="4.5" stroke="currentColor" stroke-width="1.4"/><path d="M11 11l3 3" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/></svg></span>' +
                    '<input id="{id}-input" class="kiss-search-field" type="search" name="{id}-search" value="{value}" placeholder="{placeholder}" autocomplete="{autocomplete}" autocorrect="off" autocapitalize="off" spellcheck="{spellcheck}" aria-label="{ariaLabel}" aria-controls="{id}-results" aria-autocomplete="list" data-1p-ignore data-lpignore="true" data-form-type="other"{maxAttr}{required}{disabled}>' +
                    '<div id="{id}-mirror" class="t-clear-mirror kiss-search-mirror" aria-hidden="true">{value}</div>' +
                    '<div id="{id}-placeholder" class="t-clear-placeholder kiss-search-placeholder" aria-hidden="true">{placeholder}</div>' +
                    '<div id="{id}-glow" class="t-clear-glow" aria-hidden="true"></div>' +
                    '<button type="button" id="{id}-clear" class="kiss-search-clear t-clear-btn" aria-label="{clearLabel}"><svg width="14" height="14" viewBox="0 0 16 16" fill="none" aria-hidden="true"><path d="M4.5 4.5 11.5 11.5M11.5 4.5 4.5 11.5" stroke="currentColor" stroke-width="1.7" stroke-linecap="round"/></svg></button>' +
                '</div>' +
                '<div id="{id}-results" class="kiss-search-results" role="listbox" hidden></div>' +
            '</div>', {
                class: rootClass,
                style: style,
                attr: nattrs,
                value: esc(value),
                valueClass: value ? ' has-value' : '',
                placeholder: esc(placeholder),
                autocomplete: esc(autocomplete),
                spellcheck: esc(spellcheck),
                ariaLabel: esc(ariaLabel),
                maxAttr: maxAttr,
                required: required ? ' required' : '',
                disabled: disabled ? ' disabled' : '',
                clearLabel: esc(clearLabel)
            });
        if (!newElm)
            return;

        const root = newElm.element;
        const control = document.getElementById(id + '-control');
        const input = document.getElementById(id + '-input');
        const clear = document.getElementById(id + '-clear');
        const mirror = document.getElementById(id + '-mirror');
        const placeholderEl = document.getElementById(id + '-placeholder');
        const glow = document.getElementById(id + '-glow');
        const results = document.getElementById(id + '-results');
        let inputHandler = null;
        let searchHandler = null;
        let clearHandler = null;
        let selectHandler = null;
        let resultItems = [];
        let clearFrame = null;

        function syncValueClass() {
            const hasValue = !!input.value;
            root.classList.toggle('has-value', hasValue);
            control.classList.toggle('has-value', hasValue);
            mirror.textContent = input.value;
            placeholderEl.textContent = placeholder;
        }

        function clearAnimationStyles() {
            if (clearFrame) {
                window.cancelAnimationFrame(clearFrame);
                clearFrame = null;
            }
            control.classList.remove('is-clearing');
            mirror.style.removeProperty('opacity');
            mirror.style.removeProperty('transform');
            mirror.style.removeProperty('filter');
            placeholderEl.style.removeProperty('opacity');
            placeholderEl.style.removeProperty('transform');
            placeholderEl.style.removeProperty('filter');
            glow.style.removeProperty('opacity');
            glow.style.removeProperty('background');
        }

        function wordRects(text) {
            mirror.textContent = text;
            const node = mirror.firstChild;
            const controlRect = control.getBoundingClientRect();
            if (!node || !controlRect.width)
                return [];
            const range = document.createRange();
            const rects = [];
            const pattern = /\S+/g;
            let match;
            while ((match = pattern.exec(text)) !== null) {
                range.setStart(node, match.index);
                range.setEnd(node, match.index + match[0].length);
                const rect = range.getBoundingClientRect();
                if (rect.width > 0 && rect.height > 0) {
                    rects.push({
                        x: rect.left - controlRect.left + rect.width / 2,
                        y: rect.top - controlRect.top + rect.height / 2,
                        w: rect.width,
                        h: rect.height
                    });
                }
            }
            range.detach();
            return rects;
        }

        function runClearDissolve(previousValue) {
            clearAnimationStyles();
            if (!previousValue || prefersReducedMotion())
                return;

            const dur = cssMs('--clear-dur', 1000);
            const outDur = cssMs('--clear-out-dur', 400);
            const inDur = cssMs('--clear-in-dur', 400);
            const outFly = cssNumber('--clear-out-fly', 12);
            const inFly = cssNumber('--clear-in-fly', 12);
            const blur = cssNumber('--clear-blur', 2);
            const glowDelay = cssMs('--glow-delay', 50);
            const glowPeakAt = clamp(cssNumber('--glow-peak-at', 0.15), 0.01, 0.99);
            const glowOpacity = cssNumber('--glow-opacity', 0.85);
            const glowSpread = cssNumber('--glow-spread', 1.5);
            const glowColor = cssString('--glow-color', '100, 116, 139');
            const rects = wordRects(previousValue);
            const start = performance.now();

            control.classList.add('is-clearing');
            mirror.textContent = previousValue;

            function tick(now) {
                const elapsed = now - start;
                const outP = easeOutCubic(elapsed / outDur);
                const inP = easeOutCubic(elapsed / inDur);
                const bobP = easeOutBack(elapsed / inDur);
                mirror.style.opacity = String(Math.max(0, 1 - outP));
                mirror.style.transform = 'translateY(' + (-outFly * outP).toFixed(2) + 'px)';
                mirror.style.filter = 'blur(' + (blur * outP).toFixed(2) + 'px)';
                placeholderEl.style.opacity = String(clamp(inP, 0, 1));
                placeholderEl.style.transform = 'translateY(' + (inFly * (1 - bobP)).toFixed(2) + 'px)';
                placeholderEl.style.filter = 'blur(' + (blur * (1 - inP)).toFixed(2) + 'px)';

                const glowT = clamp((elapsed - glowDelay) / Math.max(1, dur - glowDelay), 0, 1);
                const glowEnvelope = glowT <= glowPeakAt
                    ? glowT / glowPeakAt
                    : (1 - glowT) / (1 - glowPeakAt);
                const glowA = clamp(glowEnvelope, 0, 1) * glowOpacity;
                glow.style.opacity = String(glowA);
                if (glowA > 0 && rects.length) {
                    const layers = rects.map((rect, i) => {
                        const y = rect.y - outFly * (0.35 + glowT * 0.9) - i * 0.4;
                        const rx = Math.max(18, rect.w * glowSpread);
                        const ry = Math.max(10, rect.h * 1.65);
                        const a = Math.max(0, glowA * (1 - i * 0.025));
                        return 'radial-gradient(ellipse ' + rx.toFixed(1) + 'px ' + ry.toFixed(1) +
                            'px at ' + rect.x.toFixed(1) + 'px ' + y.toFixed(1) + 'px, rgba(' + glowColor + ', ' +
                            a.toFixed(3) + ') 0%, rgba(' + glowColor + ', 0) 72%)';
                    });
                    glow.style.background = layers.join(',');
                } else {
                    glow.style.background = '';
                }

                if (elapsed < dur) {
                    clearFrame = window.requestAnimationFrame(tick);
                } else {
                    clearAnimationStyles();
                    syncValueClass();
                }
            }

            clearFrame = window.requestAnimationFrame(tick);
        }

        function hideResults() {
            results.hidden = true;
            root.classList.remove('has-results');
            input.removeAttribute('aria-expanded');
        }

        function showResults() {
            if (!results.innerHTML)
                return;
            results.hidden = false;
            root.classList.add('has-results');
            input.setAttribute('aria-expanded', 'true');
        }

        function selectResult(btn) {
            if (!btn || btn.disabled)
                return;
            const idx = Number(btn.getAttribute('data-index'));
            const detail = {
                value: btn.getAttribute('data-value'),
                label: btn.getAttribute('data-label') || '',
                item: resultItems[idx],
                element: btn
            };
            input.value = detail.label;
            clearAnimationStyles();
            syncValueClass();
            hideResults();
            if (selectHandler)
                selectHandler(detail.value, detail.label, detail.item, detail.element);
            root.dispatchEvent(new CustomEvent('select', {detail: detail}));
        }

        input.addEventListener('input', () => {
            clearAnimationStyles();
            syncValueClass();
            if (inputHandler)
                inputHandler(input.value);
            root.dispatchEvent(new CustomEvent('input', {detail: {value: input.value}}));
        });

        input.addEventListener('keydown', (e) => {
            const opts = Array.from(results.querySelectorAll('.kiss-search-result:not(:disabled)'));
            if (e.key === 'Enter') {
                if (!results.hidden && document.activeElement !== input)
                    return;
                if (searchHandler)
                    searchHandler(input.value);
            } else if (e.key === 'ArrowDown' && opts.length) {
                e.preventDefault();
                showResults();
                opts[0].focus();
            } else if (e.key === 'Escape') {
                hideResults();
            }
        });

        clear.addEventListener('click', () => {
            const previousValue = input.value;
            input.value = '';
            syncValueClass();
            hideResults();
            runClearDissolve(previousValue);
            input.focus();
            if (clearHandler)
                clearHandler();
            if (inputHandler)
                inputHandler(input.value);
        });

        results.addEventListener('click', (e) => {
            selectResult(e.target.closest('.kiss-search-result'));
        });

        results.addEventListener('keydown', (e) => {
            const opts = Array.from(results.querySelectorAll('.kiss-search-result:not(:disabled)'));
            const idx = opts.indexOf(document.activeElement);
            let next = null;
            if (e.key === 'ArrowDown')
                next = opts[(idx + 1 + opts.length) % opts.length];
            else if (e.key === 'ArrowUp')
                next = idx <= 0 ? input : opts[(idx - 1 + opts.length) % opts.length];
            else if (e.key === 'Enter')
                return selectResult(document.activeElement);
            else if (e.key === 'Escape') {
                hideResults();
                input.focus();
            }
            if (next) {
                e.preventDefault();
                next.focus();
            }
        });

        document.addEventListener('pointerdown', (e) => {
            if (!root.contains(e.target))
                hideResults();
        }, true);

        newElm.getValue = function () { return input.value; };
        newElm.setValue = function (val) { clearAnimationStyles(); input.value = val == null ? '' : String(val); syncValueClass(); return this; };
        newElm.setPlaceholder = function (val) {
            placeholder = val == null ? '' : String(val);
            input.setAttribute('placeholder', placeholder);
            syncValueClass();
            return this;
        };
        newElm.clear = function () { clearAnimationStyles(); input.value = ''; syncValueClass(); hideResults(); return this; };
        newElm.focus = function () { input.focus(); return this; };
        newElm.oninput = function (fun) { inputHandler = fun; return this; };
        newElm.onInput = newElm.oninput;
        newElm.onChange = newElm.oninput;
        newElm.onsearch = function (fun) { searchHandler = fun; return this; };
        newElm.onSearch = newElm.onsearch;
        newElm.onclear = function (fun) { clearHandler = fun; return this; };
        newElm.onClear = newElm.onclear;
        newElm.onselect = function (fun) { selectHandler = fun; return this; };
        newElm.onSelect = newElm.onselect;
        newElm.showResults = function () { showResults(); return this; };
        newElm.hideResults = function () { hideResults(); return this; };
        newElm.setResults = function (items, opts = {}) {
            if (opts.descriptionField && !opts.descField)
                opts.descField = opts.descriptionField;
            resultItems = Array.isArray(items) ? items : [];
            results.innerHTML = resultItems.map((item, idx) => resultHtml(item, idx, opts)).join('');
            if (resultItems.length)
                showResults();
            else
                hideResults();
            return this;
        };
        newElm.disable = function (flg = true) { input.disabled = !!flg; clear.disabled = !!flg; return this; };
        newElm.enable = function (flg = true) { return this.disable(!flg); };
    };

    Utils.newComponent({
        name: 'SearchInput',
        tag: 'search-input',
        processor: processor
    });

})();
