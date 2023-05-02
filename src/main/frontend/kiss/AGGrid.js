/* global DateUtils, Utils, DateTimeUtils */

'use strict';


/**
 * This class is a wrapper around the public ag-grid utility.  It is intended that this class be used exclusively
 * rather than any of the raw ag-grid API.  It provides a higher-level and more convenient API.
 * <br><br>
 * Please refer to agGrid documentation on <a href="https://www.ag-grid.com/documentation-main/documentation.php">https://www.ag-grid.com/documentation-main/documentation.php</a> for more information.
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
    }

    /**
     * Initialize and show the grid.
     * <br><br>
     * Important!  Once this is called, you must call the <code>destroy()</code> method when the grid is no longer needed.
     *
     * @returns {AGGrid}
     */
    show() {
        const self = this;
        this.gridOptions = {
            columnDefs: this.columns,
            rowData: this.data,
            rowSelection: this.rowSelection,
            suppressRowDeselection: this.rowSelection === AGGrid.MULTI_SELECTION,
            suppressHorizontalScroll: this.suppressHorizontalScroll,
            suppressCellSelection: true,
            components: this.components,
            suppressRowClickSelection: this.suppressRowClickSelection,
            suppressRowHoverHighlight: this.suppressRowClickSelection,
            onRowDragEnd: function (e) {
                if (self.dragFunction)
                    self.dragFunction(e.node.data, e.overNode.data);
            },
            defaultColDef: {
                resizable: true
            },
            getRowStyle: this.rowStyleFun,
            onGridReady: function (params) {
                if (this.suppressHorizontalScroll) {
                    //                   params.api.sizeColumnsToFit();

                    window.addEventListener('resize', function() {
                        setTimeout(function() {
                            params.api.sizeColumnsToFit();
                        });
                    });
                }
            }/* ,
            getRowNodeId: function (data) {
                return data[self.keyColumn];
            }
            */
            // Disable all warnings
            // suppressPropertyNamesCheck: true
        };

        if (self.keyColumn)
            this.gridOptions.getRowNodeId = data => data[self.keyColumn];

        let eGridDiv = document.querySelector('#' + this.id);
        if (!eGridDiv)
            console.log("grid id " + this.id + " does not exist");
        else {
            eGridDiv.classList.add('ag-theme-balham');
            new agGrid.Grid(eGridDiv, this.gridOptions);
            this.gridInstantiated = true;
        }
        if (!AGGrid.gridContext.length)
            AGGrid.newGridContext();
        AGGrid.addGrid(this);
        return this;
    }

    resizeColumns() {
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
        this.gridOptions.api.setRowData([]);
        return this;
    }

    setRowData(data) {
        if (!this.gridOptions)
            this.data = data;
        else
            this.gridOptions.api.setRowData(data);
        return this;
    }

    /**
     * Delete the row who's key column is equal to <code>id</code>.
     *
     * @param id
     * @returns {AGGrid}
     */
    deleteRow(id) {
        const node = this.gridOptions.api.getRowNode(id);
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
        const node = this.gridOptions.api.getRowNode(id);
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
        if (!this.gridOptions)
            this.data = this.data.concat(data);
        else {
            this.gridOptions.api.applyTransaction({add: data});
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
        if (!this.gridOptions)
            this.data.push(data);
        else
            this.gridOptions.api.applyTransaction({add: [data]});
        return this;
    }

    /**
     * Clear the selection from the currently selected rows.
     *
     * @returns {AGGrid}
     */
    clearSelection() {
        if (this.gridOptions)
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
        return this.gridOptions.api.rowModel.getRowCount();
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
        this.gridOptions.api.rowModel.forEachNode(node => {
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
        this.gridOptions.onSelectionChanged = function () {
            if (fn)
                fn(self.gridOptions.api.getSelectedRows());
        };
        return this;
    }

    /**
     * Execute function <code>fn</code> whenever the user double-clicks on a row.
     *
     * @param fn {function}
     * @returns {AGGrid}
     */
    setOnRowDoubleClicked(fn) {
        if (fn)
            this.gridOptions.onRowDoubleClicked = () => {
                if (!Utils.suspendDepth)
                    fn();
            };
        else
            this.gridOptions.onRowDoubleClicked = fn;
        return this;
    }

    /**
     * Return <code>true</code> if the grid is empty.
     *
     * @returns {boolean}
     */
    isEmpty() {
        return this.gridOptions.api.rowModel.getRowCount() === 0;
    }

    sizeColumnsToFit() {
        if (this.gridOptions === undefined)
            console.error("Grid options not found. Make sure the grid is built using the 'build' method.");
        else
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
        const priorState = this.gridOptions.columnApi.getColumnState();
        for (let prop in priorState) {
            let val = priorState[prop];
            if (val.colId === colId) {
                hide = val.hide;
                break;
            }
        }
        this.gridOptions.columnApi.applyColumnState(state);
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

}

// class variables
AGGrid.SINGLE_SELECTION = 'single';
AGGrid.MULTI_SELECTION = 'multiple';

AGGrid.gridContext = [];        //  An array of arrays.  The outer array represents a stack of contexts.
                                //  The inner array is an array of grids that'll need to be disposed.
                                //  Basically, each context (except the first) represents a popup.
                                //  The first represents the current screen.
                                //  Each inner array contains an array of grids in that context.

