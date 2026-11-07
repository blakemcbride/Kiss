/* global DateUtils, Utils, DateTimeUtils */

'use strict';


/**
 * Provide a grid control.
 * <br><br>
 * This class is a wrapper around the public ag-grid utility.  It is intended that this class be used exclusively
 * rather than any of the raw ag-grid API.  It provides a higher-level and more convenient API.
 * <br><br>
 * Please refer to ag-grid documentation on <a href="https://www.ag-grid.com/documentation-main/documentation.php">https://www.ag-grid.com/documentation-main/documentation.php</a> for more information.
 *
 */
class AGGrid {
    /**
     * Create a new AGGrid instance.
     * <br><br>
     * The HTML portion of this should look like:
     * <br><br>
     * <code>&lt;div id="grid" style="margin-top: 10px; width: 100%; height: calc(100% - 90px);"&gt;&lt;/div&gt;</code>
     * <br><br>
     * CSS <code>calc</code> can be used to set the width or height so that the grid dynamically resizes when the browser window
     * gets resized.
     *
     * @param id {string} the ID of the div that represents the grid
     * @param columns an ag-grid columnDefs data structure
     * @param keyColumn {string} the ID of the key column (optional)
     */
    constructor(id, columns, keyColumn=undefined) {
        this.id = id;
        this.gridElement = document.querySelector('#' + id);
        this.columns = columns;
        this.rowSelection = AGGrid.SINGLE_SELECTION;
        this.data = [];
        this.keyColumn = keyColumn;
        this.components = {};
        this.gridInstantiated = false;
        this.highlightedRows = [];
        this.dragFunction = null;
        this.suppressRowClickSelection = false;
        this.suppressHorizontalScroll = true;
        this.rowStyleFun = null;
        this.disableCtl = false;
        this.rowDoubleClickHandler = null;
        this.selectionChangedHandler = null;
    }

    /**
     * Initialize and show the grid.
     * <br><br>
     * Important!  Once this is called, you must call the <code>destroy()</code> method when the grid is no longer needed.
     *
     * @returns {AGGrid}
     */
    show() {
        if (this.gridInstantiated)
            return this;

        const self = this;
        function setGridReady(api) {
            if (!api || !self.gridOptions)
                return;
            self.gridOptions.api = api;
            if (self.data && self.data.length > 0) {
                api.setGridOption('rowData', self.data);
                self.data = [];
            }
        }
        this.gridOptions = {
            columnDefs: this.sortableColumns_(),
            rowData: this.data || [],
            rowSelection: {
                mode: this.rowSelection,
                enableDeselection: this.rowSelection !== AGGrid.MULTI_SELECTION,
                enableClickSelection: !this.suppressRowClickSelection,
                checkboxes: false,
                headerCheckbox: false
            },
            suppressHorizontalScroll: this.suppressHorizontalScroll,
            components: this.components,
            suppressRowHoverHighlight: this.suppressRowClickSelection,
            suppressCellFocus: true,
            onRowDragEnd: function (e) {
                if (self.dragFunction)
                    self.dragFunction(e.node.data, e.overNode.data);
            },
            defaultColDef: {
                resizable: true
            },
            getRowStyle: this.rowStyleFun,
            // When server sorting is on, ag-grid may mark columns sorted (to show indicators) but must never move rows.
            postSortRows: params => {
                if (!self.serverSortFn || !params.nodes.length)
                    return;
                // restore the order in which the rows were added
                const order = new Map();
                params.nodes[0].parent.allLeafChildren.forEach((n, i) => order.set(n, i));
                params.nodes.sort((a, b) => order.get(a) - order.get(b));
            },
            onGridReady: function (params) {
                setGridReady(params.api);
                
                if (self.suppressHorizontalScroll) {
                    if (self.gridElement.offsetParent != null)
                        params.api.sizeColumnsToFit();

                    window.addEventListener('resize', function() {
                        setTimeout(function() {
                            if (self.gridElement.offsetParent!= null && self.gridInstantiated)
                                params.api.sizeColumnsToFit();
                        }, 100);
                    });
                }
                
                // Re-apply event handlers if they were set after show() was called
                if (self.rowDoubleClickHandler && !self.gridOptions.onRowDoubleClicked) {
                    self.setOnRowDoubleClicked(self.rowDoubleClickHandler);
                }
                if (self.selectionChangedHandler && !self.gridOptions.onSelectionChanged) {
                    self.setOnSelectionChanged(self.selectionChangedHandler);
                }
            }/* ,
            getRowNodeId: function (data) {
                return data[self.keyColumn];
            }
            */
            // Disable all warnings
            // suppressPropertyNamesCheck: true
        };
        
        // Add stored event handlers
        if (this.rowDoubleClickHandler) {
            this.gridOptions.onRowDoubleClicked = () => {
                if (!Utils.suspendDepth && !self.disableCtl)
                    self.rowDoubleClickHandler();
            };
        }
        
        if (this.selectionChangedHandler) {
            this.gridOptions.onSelectionChanged = () => {
                if (!self.disableCtl && self.gridOptions.api)
                    self.selectionChangedHandler(self.gridOptions.api.getSelectedRows());
            };
        }

        if (self.keyColumn)
            this.gridOptions.getRowId = data => String(data.data[self.keyColumn]);

        let eGridDiv = document.querySelector('#' + this.id);
        if (!eGridDiv)
            console.log("grid id " + this.id + " does not exist");
        else {
            // Check if container has height set via CSS or inline styles
            const computedStyle = window.getComputedStyle(eGridDiv);
            if (computedStyle.height === '0px' || computedStyle.height === 'auto') {
                eGridDiv.style.minHeight = '400px';
            }
            // Add theme class for styling
            eGridDiv.classList.add('ag-theme-quartz');
            const api = agGrid.createGrid(eGridDiv, this.gridOptions);
            this.gridInstantiated = true;
            setGridReady(api);
            if (this.serverSortFn)
                this.installSortClick_();
            if (this.sortIndicators)
                this.applySortIndicators_();
        }
        if (!AGGrid.gridContext.length)
            AGGrid.newGridContext();
        AGGrid.addGrid(this);
        return this;
    }

    resizeColumns() {
        if (this.gridElement.offsetParent != null && this.gridInstantiated && this.gridOptions && this.gridOptions.api)
            this.gridOptions.api.sizeColumnsToFit();
        return this;
    }

    /**
     * Free all of the internal data structures associated with the grid.
     * <br><br>
     * This method must be called once a grid is no longer needed.
     * <br><br>
     * Please note that this function is normally handled by the framework.
     * If your application code is calling it, you are most likely doing something wrong.
     */
    destroy() {
        if (this.gridInstantiated) {
            this.gridOptions.api.destroy();
            this.gridOptions = null;
            this.id = null;
            this.columns = null;
            this.rowSelection = null;
            this.data = null;
            this.keyColumn = null;
            this.components = null;
            this.gridInstantiated = false;
        }
    }

    /**
     * If a row is dragged, this function will be called when the drag operation is complete.  <code>fun</code> is
     * passed two arguments.  The first is the row being dragged.  The second is the row over which it was released.
     *
     * @param fun {function}
     * @returns {AGGrid}
     */
    setDragFunction(fun) {
        this.dragFunction = fun;
        return this;
    }

    /**
     * This method allows you to set a function that determines
     * the style of each individual row.
     *
     * @param fun {function}
     * @returns {AGGrid}
     */
    rowStyleFunction(fun) {
        this.rowStyleFun = fun;
        return this;
    }

    /**
     * Erase all the rows in the grid.
     *
     * @returns {AGGrid}
     */
    clear() {
        if (this.gridOptions && this.gridOptions.api)
            this.gridOptions.api.setGridOption('rowData', []);
        else
            this.data = [];
        return this;
    }

    /**
     * Erase all existing rows and set the new rows.
     *
     * @param data {array} the data to display in the grid
     * @returns {AGGrid}
     */
    setRowData(data) {
        if (!this.gridOptions || !this.gridOptions.api)
            this.data = data;
        else {
            this.gridOptions.api.setGridOption('rowData', data);
            this.data = [];  // Clear internal data after setting to grid
        }
        return this;
    }

    /**
     * Delete the row who's key column is equal to <code>id</code>.
     *
     * @param id
     * @returns {AGGrid}
     */
    deleteRow(id) {
        const node = this.gridOptions.api.getRowNode(String(id));
        if (node  &&  node.data)
            this.gridOptions.api.applyTransaction({remove: [node.data]});
        return this;
    }

    /**
     * Disallow any row selection.  This must be called before <code>show()</code>
     *
     * @returns {AGGrid}
     */
    noRowSelection() {
        this.suppressRowClickSelection = true;
        return this;
    }

    /**
     * Highlight a row or an array of particular rows.
     * <br><br>
     * Note that highlighting a row and selecting a row are two different things.
     * <br><br>
     * Pass row index or an array or row indexes to highlight.
     * Pass null to un-highlight all rows.
     *
     * @param idx  null, number, or array of numbers
     */
    highlightRows(idx) {
        this.highlightedRows = idx = Utils.assureArray(idx);
        this.gridOptions.getRowStyle = (params) => {
            for (let i=0 ; i < idx.length ; i++)
                if (params.node.rowIndex === idx[i])
                    return { background: 'lightPink' };
            return null;
        };
        this.gridOptions.api.deselectAll();
        this.gridOptions.api.redrawRows();
        return this;
    }

    /**
     * Select the row specified in which the key column of that row is equal to <code>id</code>.
     *
     * @param id
     * @returns {AGGrid}
     */
    selectId(id) {
        const node = this.gridOptions.api.getRowNode(String(id));
        if (node  &&  node.data)
            node.setSelected(true, true);
        return this;
    }

    /**
     * De-select all rows.
     *
     * @returns {AGGrid}
     */
    deselectAll() {
        this.gridOptions.api.deselectAll();
        return this;
    }

    /**
     * Add an array of records to the grid.
     * <br><br>
     * Note that this method is far faster than a series of <code>addRecord()</code> calls.
     *
     * @param data {array} each element of the array is an object representing a row
     * @returns {AGGrid}
     */
    addRecords(data) {
        data = Utils.assureArray(data);
        if (!this.gridOptions || !this.gridOptions.api)
            this.data = this.data.concat(data);
        else {
            this.gridOptions.api.applyTransaction({add: data});
            this.data = [];  // Clear internal data after adding to grid
            if (this.suppressHorizontalScroll)
                this.resizeColumns();  // when vert scrollbar gets auto-added must resize columns
        }
        return this;
    }

    /**
     * Add a single record to the grid.
     * <br><br>
     * Note that if several records are going to be added at a time, it is far faster to use <code>addRecords()</code>.
     *
     * @param data {object} each element represents a column on the grid
     * @returns {AGGrid}
     */
    addRecord(data) {
        if (!this.gridOptions || !this.gridOptions.api)
            this.data.push(data);
        else {
            this.gridOptions.api.applyTransaction({add: [data]});
            this.data = [];  // Clear internal data after adding to grid
        }
        return this;
    }

    /**
     * Clear the selection from the currently selected rows.
     *
     * @returns {AGGrid}
     */
    clearSelection() {
        if (this.gridOptions && this.gridOptions.api)
            this.gridOptions.api.deselectAll();
        return this;
    }

    /**
     * Update the selected row with the new data provided in <code>row</code>.
     *
     * @param row
     * @returns {AGGrid}
     */
    updateSelectedRecord(row) {
        this.gridOptions.api.applyTransaction({update: [row]});
        return this;
    }

    /**
     * Delete the selected rows.
     *
     * @returns {AGGrid}
     */
    deleteSelectedRows() {
        const selectedData = this.gridOptions.api.getSelectedRows();
        this.gridOptions.api.applyTransaction({remove: selectedData});
        return this;
    }

    /**
     * Return an array containing all of the selected rows.
     *
     * @returns {*}
     */
    getSelectedRows() {
        return this.gridOptions.api.getSelectedRows();
    }

    /**
     * Returns all the rows.
     *
     * @returns {array}
     */
    getAllRows() {
        const rows = [];
        this.gridOptions.api.forEachNode(node => rows.push(node.data));
        return rows;
    }

    /**
     * Returns the number of rows in the grid.
     *
     * @returns {number}
     */
    getNumberOfRows() {
        return this.gridOptions.api.getModel().getRowCount();
    }

    /**
     * Get row at index <code>n</code> (indexes are zero origin)
     *
     * @param n
     * @returns {*}
     */
    getRowAtIndex(n) {
        const rows = this.getAllRows();
        return rows[n];
    }

    /**
     * Returns the first row in the grid.  If there are no rows, a <code>null</code> is returned.
     * Row selection is ignored.
     *
     * @returns {*|null}
     */
    getFirstRow() {
        const rows = this.getAllRows();
        if (!rows.length)
            return null;
        return rows[0];
    }

    /**
     * Returns the last row in the grid.  If there are no rows, a <code>null</code> is returned.
     * Row selection is ignored.
     *
     * @returns {*|null}
     */
    getLastRow() {
        const rows = this.getAllRows();
        if (!rows.length)
            return null;
        return rows[rows.length-1];
    }

    /**
     * Return the index of the selected row.  <code>null</code> is returned if no row is selected.
     * If multiple rows are selected, the index of the first selected row is returned.
     *
     * @returns {null|number}
     */
    getSelectedRowIndex() {
        const rows = this.getAllRows();
        const selectedRow = this.getSelectedRow();
        const key = selectedRow[this.keyColumn];
        for (let i=0 ; i < rows.length ; i++)
            if (key === rows[i][this.keyColumn])
                return i;
        return null;
    }

    /**
     * Return an array of the indexes of all of the selected rows.
     *
     * @returns {array}
     */
    getSelectedRowIndexes() {
        const rows = this.getAllRows();
        const selectedRows = this.getSelectedRows();
        const lst = [];
        for (let i=0 ; i < rows.length ; i++)
            for (let j=0 ; j < selectedRows.length ; j++)
                if (selectedRows[j][this.keyColumn] === rows[i][this.keyColumn]) {
                    lst.push(i);
                    break;
                }
        return lst;
    }

    /**
     * Delete row at index <code>n</code>.
     *
     * @param n
     * @returns {AGGrid}
     */
    deleteRowIndex(n) {
        const row = this.getRowAtIndex(n);
        this.gridOptions.api.applyTransaction({remove: [row]});
        return this;
    }

    /**
     * Return the selected row or <code>null</code> if none is selected.
     *
     * @returns {*}
     */
    getSelectedRow() {
        const sel = this.gridOptions.api.getSelectedRows();
        return sel.length === 1 ? sel[0] : null;
    }

    /**
     * Returns the number of selected rows
     *
     * @returns {*}
     */
    numberOfSelectedRows() {
        return this.gridOptions.api.getSelectedRows().length;
    }

    getDataItems() {
        const dataItems = [];
        this.gridOptions.api.forEachNode(node => {
            dataItems.push(node.data);
        });
        return dataItems;
    }

    /**
     * Execute <code>fn</code> anytime a row selection is changed.
     * <br><br>
     * <code>fn</code> is passwd an array of the selected rows.
     *
     * @param fn {function}
     */
    setOnSelectionChanged(fn) {
        const self = this;
        this.selectionChangedHandler = fn;
        
        if (this.gridOptions && this.gridOptions.api) {
            // Grid already created, use setGridOption to update the event handler
            this.gridOptions.api.setGridOption('onSelectionChanged', fn ? (() => {
                if (!self.disableCtl)
                    fn(self.gridOptions.api.getSelectedRows());
            }) : null);
        }
        return this;
    }

    /**
     * Execute function <code>fn</code> whenever the user double-clicks on a row.
     *
     * @param fn {function}
     * @returns {AGGrid}
     */
    setOnRowDoubleClicked(fn) {
        const self = this;
        this.rowDoubleClickHandler = fn;
        
        if (this.gridOptions && this.gridOptions.api) {
            // Grid already created, use setGridOption to update the event handler
            this.gridOptions.api.setGridOption('onRowDoubleClicked', fn ? (() => {
                if (!Utils.suspendDepth && !self.disableCtl)
                    fn();
            }) : null);
        }
        return this;
    }

    /**
     * Return <code>true</code> if the grid is empty.
     *
     * @returns {boolean}
     */
    isEmpty() {
        return this.gridOptions.api.getModel().getRowCount() === 0;
    }

    sizeColumnsToFit() {
        if (this.gridOptions === undefined)
            console.error("Grid options not found. Make sure the grid is built using the 'build' method.");
        else if (this.gridOptions.api)
            this.gridOptions.api.sizeColumnsToFit();
        return this;
    }

    /**
     * Add a class that defines special cell formatting.
     *
     * @param tag
     * @param cls
     * @returns {AGGrid}
     */
    addComponent(tag, cls) {
        this.components[tag] = cls;
        return this;
    }

    /**
     * By default, grids are single-row-select enabled.  This method enables multi-row-select.
     * It must be called prior to <code>show()</code>.
     *
     * @returns {AGGrid}
     */
    multiSelect() {
        this.rowSelection = AGGrid.MULTI_SELECTION;
        return this;
    }

    /**
     * Enable server-side column sorting.
     * <br><br>
     * Use this when the rows shown are only a (possibly capped) subset of the data, so sorting just the rows in the
     * browser would be wrong.  Clicking a column header (or pressing Enter on a focused header) does <b>not</b> sort
     * the grid.  Instead, <code>fn(colId)</code> is called, where <code>colId</code> is the column's colId (which is its
     * <code>field</code> unless the column def specifies a <code>colId</code>).  The application is expected to
     * re-query the server using whatever sort it chooses and then reload the grid, and to call
     * <code>setSortIndicators()</code> to show the user what the grid is sorted by.
     * <br><br>
     * The grid itself never reorders rows: they always display in the order they were added
     * (<code>setRowData</code>, <code>addRecords</code>, etc.).  The application owns the sort policy (which column
     * becomes primary, direction toggling, etc.).
     * <br><br>
     * Every column becomes header-clickable except columns whose column def says <code>sortable: false</code>
     * (and column groups).  Columns with <code>sortable: false</code> are left alone: clicking them does nothing.
     * <br><br>
     * May be called before or after <code>show()</code>.  Grids that never call this method are not affected in any way.
     * Passing a falsy value turns server sorting back off and clears any indicators.
     *
     * @param {function(string)} fn called with the colId of the clicked column
     * @returns {AGGrid}
     * @see AGGrid#setSortIndicators
     */
    setServerSort(fn) {
        this.serverSortFn = fn || null;
        if (!fn)
            this.sortIndicators = null;
        if (this.gridInstantiated) {
            this.setColumnDefs_(this.sortableColumns_());
            this.installSortClick_();
            this.applySortIndicators_();
        }
        return this;
    }

    /**
     * Display sort indicators (the standard ag-grid arrows) on column headers without sorting or reordering any rows
     * and without calling the function given to <code>setServerSort()</code>.
     * <br><br>
     * <code>list</code> is an array of <code>{field, asc}</code> objects in priority order (first = primary sort).
     * <code>field</code> is a column's colId (its field unless the column def has a <code>colId</code>) and
     * <code>asc</code> is true for ascending, false for descending.  When more than one column is listed, ag-grid also
     * shows each column's priority number.  A field that has no column, or whose column is hidden, is silently ignored.
     * <br><br>
     * <code>null</code> or an empty array clears all indicators.  May be called before or after <code>show()</code>.
     *
     * @param {Array<{field: string, asc: boolean}>|null} list
     * @returns {AGGrid}
     */
    setSortIndicators(list) {
        this.sortIndicators = list && list.length ? list.slice() : null;
        if (this.gridInstantiated)
            this.applySortIndicators_();
        return this;
    }

    /**
     * Internal: the column defs to give ag-grid.  When server sorting is on, every column that has not said
     * <code>sortable: false</code> is made sortable (copies; the caller's column defs are never modified).
     *
     * @private
     */
    sortableColumns_() {
        if (!this.serverSortFn)
            return this.columns;
        const conv = cols => cols.map(c => {
            if (c.children)
                return Object.assign({}, c, { children: conv(c.children) });
            return c.sortable === false ? c : Object.assign({}, c, { sortable: true });
        });
        return conv(this.columns);
    }

    /**
     * Internal: replace the column defs on a live grid (works for ag-grid 30 and 32+).
     *
     * @private
     */
    setColumnDefs_(defs) {
        const api = this.gridOptions.api;
        if (api.setGridOption)
            api.setGridOption('columnDefs', defs);
        else
            api.setColumnDefs(defs);
    }

    /**
     * Internal: the object that carries the column API (ag-grid 32+: the grid api; ag-grid 30: gridOptions.columnApi).
     *
     * @private
     */
    columnApi_() {
        const api = this.gridOptions.api;
        return api.applyColumnState ? api : this.gridOptions.columnApi;
    }

    /**
     * Internal: push this.sortIndicators into ag-grid's column state (works for ag-grid 30 and 32+).
     *
     * @private
     */
    applySortIndicators_() {
        const colApi = this.columnApi_();
        const state = [];
        (this.sortIndicators || []).forEach(s => {
            const col = s && s.field !== undefined ? colApi.getColumn(s.field) : null;
            if (!col || !col.isVisible() || state.some(x => x.colId === col.getColId()))
                return;
            state.push({ colId: col.getColId(), sort: s.asc ? 'asc' : 'desc', sortIndex: state.length });
        });
        colApi.applyColumnState({ state: state, defaultState: { sort: null, sortIndex: null } });
    }

    /**
     * Internal: intercept header clicks (capture phase) so ag-grid never sorts, and report them to the server sort function.
     *
     * @private
     */
    installSortClick_() {
        if (this.sortClickInstalled || !this.gridElement)
            return;
        this.sortClickInstalled = true;
        const self = this;
        const handler = e => {
            if (!self.serverSortFn || !self.gridInstantiated)
                return;
            if (e.type === 'keydown' && e.key !== 'Enter')
                return;
            const t = e.target;
            const cell = t && t.closest ? t.closest('.ag-header-cell') : null;
            if (!cell || !self.gridElement.contains(cell))
                return;
            if (t.closest('.ag-header-cell-resize, .ag-header-cell-menu-button, .ag-header-cell-filter-button, .ag-floating-filter, .ag-checkbox'))
                return;
            const colId = cell.getAttribute('col-id');
            const col = colId ? self.columnApi_().getColumn(colId) : null;
            if (!col || !col.getColDef().sortable)
                return;
            e.stopPropagation();
            e.preventDefault();
            // A press that travelled (a drag that came back to the same header) is not a click.
            if (e.type === 'click' && down && Math.hypot(e.clientX - down.x, e.clientY - down.y) > 4)
                return;
            self.serverSortFn(colId);
        };
        let down = null;
        this.gridElement.addEventListener('mousedown', e => { down = { x: e.clientX, y: e.clientY }; }, true);
        this.gridElement.addEventListener('click', handler, true);
        this.gridElement.addEventListener('keydown', handler, true);
    }

    /**
     * Hide or show column colId.
     *
     * @param colId
     * @param {boolean} val true=show, false=hide
     * @returns {boolean} previous value
     */
    columnShow(colId, val) {
        let hide;
        const state = {
            state: [
                {
                    colId: colId,
                    hide: !val
                }
            ]
        };
        const priorState = this.gridOptions.api.getColumnState();
        for (let prop in priorState) {
            let val = priorState[prop];
            if (val.colId === colId) {
                hide = val.hide;
                break;
            }
        }
        this.gridOptions.api.applyColumnState(state);
        return !hide;
    }

    /**
     * Create a new grid context.
     */
    static newGridContext() {
        AGGrid.gridContext.push([]);
    }

    /**
     * Add a grid to the current context.
     *
     * @param grid
     */
    static addGrid(grid) {
        const cc = AGGrid.gridContext[AGGrid.gridContext.length - 1];
        cc.push(grid);
    }

    /**
     * Destroy all grids in last context and remove the context
     */
    static popGridContext() {
        const c = AGGrid.gridContext.pop();
        if (c)
            for (let i = 0; i < c.length; i++)
                c[i].destroy();
    }

    /**
     * Destroys all popup and screen grids that have been created
     */
    static popAllGridContexts() {
        while (AGGrid.gridContext.length)
            AGGrid.popGridContext();
    }

    /**
     * Format for date times formatted as mm/dd/yyyy hh:mm AM/PM
     * <br><br>
     * Usage:  in columnDefs:  valueFormatter:  AGGrid.dateTime
     *
     * @param params
     * @returns {string}
     */
    static dateTime(params) {
        return DateTimeUtils.formatDate(params.value);
    }

    /**
     * Format for date times formatted as mm/dd/yyyy
     * <br><br>
     * Usage:  in columnDefs:  valueFormatter:  AGGrid.date
     *
     * @param params
     * @returns {string}
     */
    static date(params) {
        return DateUtils.intToStr4(params.value);
    }

    /**
     * Format times as HH:MM AM/PM
     * <br><br>
     * Usage:  in columnDefs:  valueFormatter:  AGGrid.time
     *
     * @param params
     * @returns {string}
     */
    static time(params) {
        return TimeUtils.format(params.value);
    }

    /**
     * Format a numeric field
     * <br><br>
     * Usage:  in columnDefs:  valueFormatter:  AGGrid.numericFormat, mask:  'xxx', decimalPlaces: N
     *
     * @param params
     * @returns {string}
     *
     * @see Utils.format()
     */
    static  numericFormat(params) {
        let val = params.value;
        if (!val)
            val = 0;
        val = typeof val !== 'string' ? val : Number(val.replaceAll('$', '').replaceAll(',', ''));
        const msk = params.colDef.mask ? params.colDef.mask : '';
        const dp  = params.colDef.decimalPlaces ? params.colDef.decimalPlaces : 0;
        return Utils.format(val, msk, 0, dp);
    }

    /**
     * Disables the clicks on the AGGrid control.
     *
     * @param {boolean} flg - true=disable, false=enable
     */
    disable(flg = true) {
        this.disableCtl = flg;
    }

    /**
     * Enables the clicks on the AGGrid control.
     *
     * @param {boolean} flg - true=enable, false=disable
     */
    enable(flg = true) {
        this.disableCtl = !flg;
    }

}

// class variables
AGGrid.SINGLE_SELECTION = 'singleRow';
AGGrid.MULTI_SELECTION = 'multiRow';

AGGrid.gridContext = [];        //  An array of arrays.  The outer array represents a stack of contexts.
                                //  The inner array is an array of grids that'll need to be disposed.
                                //  Basically, each context (except the first) represents a popup.
                                //  The first represents the current screen.
                                //  Each inner array contains an array of grids in that context.
