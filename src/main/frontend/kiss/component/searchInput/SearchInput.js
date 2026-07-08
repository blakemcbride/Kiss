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
                '<span class="kiss-search-icon" aria-hidden="true"><svg width="15" height="15" viewBox="0 0 16 16" fill="none"><circle cx="7" cy="7" r="4.5" stroke="currentColor" stroke-width="1.4"/><path d="M11 11l3 3" stroke="currentColor" stroke-width="1.4" stroke-linecap="round"/></svg></span>' +
                '<input id="{id}-input" class="kiss-search-field" type="text" value="{value}" placeholder="{placeholder}" autocomplete="{autocomplete}" spellcheck="{spellcheck}" aria-label="{ariaLabel}" aria-controls="{id}-results" aria-autocomplete="list"{maxAttr}{required}{disabled}>' +
                '<button type="button" id="{id}-clear" class="kiss-search-clear" aria-label="{clearLabel}" title="{clearLabel}"><svg width="14" height="14" viewBox="0 0 16 16" fill="none" aria-hidden="true"><path d="M4.5 4.5 11.5 11.5M11.5 4.5 4.5 11.5" stroke="currentColor" stroke-width="1.7" stroke-linecap="round"/></svg></button>' +
                '<div id="{id}-results" class="kiss-search-results" role="listbox" hidden></div>' +
            '</div>', {
                class: rootClass,
                style: style,
                attr: nattrs,
                value: esc(value),
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
        const input = document.getElementById(id + '-input');
        const clear = document.getElementById(id + '-clear');
        const results = document.getElementById(id + '-results');
        let inputHandler = null;
        let searchHandler = null;
        let clearHandler = null;
        let selectHandler = null;
        let resultItems = [];

        function syncValueClass() {
            root.classList.toggle('has-value', !!input.value);
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
            syncValueClass();
            hideResults();
            if (selectHandler)
                selectHandler(detail.value, detail.label, detail.item, detail.element);
            root.dispatchEvent(new CustomEvent('select', {detail: detail}));
        }

        input.addEventListener('input', () => {
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
            input.value = '';
            syncValueClass();
            hideResults();
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
        newElm.setValue = function (val) { input.value = val == null ? '' : String(val); syncValueClass(); return this; };
        newElm.clear = function () { input.value = ''; syncValueClass(); hideResults(); return this; };
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
