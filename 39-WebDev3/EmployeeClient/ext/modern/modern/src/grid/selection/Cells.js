/**
 * A class which encapsulates a range of cells defining a selection in a grid.
 *
 * Note that when range start and end points are represented by an array, the
 * order is traditional `x, y` order, that is column index followed by row index.
 *
 */
Ext.define('Ext.grid.selection.Cells', {
    extend: 'Ext.dataview.selection.Selection',
    alias: 'selection.cells',

    requires: [
        'Ext.grid.Location'
    ],

    /**
     * @property {Boolean} isCells
     * This property indicates the this selection represents selected cells.
     * @readonly
     */
    isCells: true,

    //-------------------------------------------------------------------------
    // Base Selection API

    clone: function() {
        var me = this,
            result = new me.self(me.view);

        if (me.startCell) {
            result.startCell = me.startCell.clone();
            result.endCell = me.endCell.clone();
        }
        return result;
    },

    /**
     * Returns `true` if the passed {@link Ext.grid.Location cell context} is selected.
     * @param {Number/Ext.grid.Location} recordIndex The record index or `location` instance.
     * @param {Number} [columnIndex] The column index if `recordIndex` is not the actual `location`.
     * @return {Boolean} `true` if the passed {@link Ext.grid.Location cell context} is selected.
     */
    isSelected: function(recordIndex, columnIndex) {
        var range;

        if (this.startCell) {
            if (recordIndex.isGridLocation) {
                columnIndex = recordIndex.columnIndex;
                recordIndex = recordIndex.recordIndex;
            }
            //<debug>
            if (!(Ext.isNumber(recordIndex) && Ext.isNumber(columnIndex))) {
                Ext.raise('Cells#isSelected must be passed either a GridLocation of a row and column index');
            }
            //</debug>

            // get start and end rows in the range
            range = this.getRowRange();

            if (recordIndex >= range[0] && recordIndex <= range[1]) {
                // get start and end columns in the range
                range = this.getColumnRange();
                return (columnIndex >= range[0] && columnIndex <= range[1]);
            }
        }

        return false;
    },

    eachRow: function(fn, scope) {
        var me = this,
            rowRange = me.getRowRange(),
            store = me.view.store,
            rowIdx;

        for (rowIdx = rowRange[0]; rowIdx <= rowRange[1]; rowIdx++) {
            if (fn.call(scope || me, store.getAt(rowIdx)) === false) {
                return;
            }
        }
    },

    eachColumn: function(fn, scope) {
        var colRange = this.getColumnRange(),
            columns = this.view.getVisibleColumns(),
            i;

        for (i = colRange[0]; i <= colRange[1]; i++) {
            if (fn.call(scope || this, columns[i], i) === false) {
                return;
            }
        }
    },

    eachCell: function(fn, scope) {
        var me = this,
            view = me.view,
            store = view.store,
            rowRange = me.getRowRange(),
            colRange = me.getColumnRange(),
            baseLocation, location, rowIdx, colIdx;

        for (rowIdx = rowRange[0]; rowIdx <= rowRange[1]; rowIdx++) {
            baseLocation = new Ext.grid.Location(view, store.getAt(rowIdx));
            for (colIdx = colRange[0]; colIdx <= colRange[1]; colIdx++) {
                location = baseLocation.cloneForColumn(colIdx);
                if (fn.call(scope || me, location, colIdx, rowIdx) === false) {
                    return;
                }
            }
        }
    },

    /**
     * @return {Number} The row index of the first row in the range or zero if no range.
     */
    getFirstRowIndex: function() {
        return this.startCell ? Math.min(this.startCell.recordIndex, this.endCell.recordIndex) : 0;
    },

    /**
     * @return {Number} The row index of the last row in the range or -1 if no range.
     */
    getLastRowIndex: function() {
        return this.startCell ? Math.max(this.startCell.recordIndex, this.endCell.recordIndex) : -1;
    },

    /**
     * @return {Number} The column index of the first column in the range or zero if no range.
     */
    getFirstColumnIndex: function() {
        return this.startCell ? Math.min(this.startCell.columnIndex, this.endCell.columnIndex) : 0;
    },

    /**
     * @return {Number} The column index of the last column in the range or -1 if no range.
     */
    getLastColumnIndex: function() {
        return this.startCell ? Math.max(this.startCell.columnIndex, this.endCell.columnIndex) : -1;
    },

    //-------------------------------------------------------------------------

    privates: {
        /**
         * @private
         */
        clear: function(suppressEvent) {
            var me = this,
                view = me.view,
                changed;

            if (view.getVisibleColumns().length) {
                me.eachCell(function(location) {
                    view.onCellDeselect(location);
                    changed = true;
                });
            }
            me.startCell = me.endCell = null;

            if (changed && !suppressEvent) {
                this.getSelectionModel().fireSelectionChange();
            }
        },

        /**
         * Used during drag/shift+downarrow range selection on start.
         * @param {Ext.grid.Location} startCell The start cell of the cell drag selection.
         * @private
         */
        setRangeStart: function (startCell) {
            // Must clone them. Users might use one instance and reconfigure it to navigate.
            this.startCell = (this.endCell = startCell.clone()).clone();
            this.view.onCellSelect(startCell);
        },

        /**
         * Used during drag/shift+downarrow range selection on drag.
         * @param {Ext.grid.Location} endCell The end cell of the cell drag selection.
         * @private
         */
        setRangeEnd: function (endCell) {
            var me = this,
                view = me.view,
                store = view.store,
                renderInfo = view.renderInfo,
                maxColIdx = view.getVisibleColumns().length - 1,
                range, lastRange, rowStart, rowEnd, colStart,
                colEnd, rowIdx, colIdx, location, baseLocation;

            me.endCell = endCell.clone();
            range = me.getRange();
            lastRange = me.lastRange || range;

            rowStart = Math.max(Math.min(range[0][1], lastRange[0][1]), renderInfo.indexTop);
            rowEnd   = Math.min(Math.max(range[1][1], lastRange[1][1]), renderInfo.indexBottom - 1);

            colStart = Math.min(range[0][0], lastRange[0][0]);
            colEnd   = Math.min(Math.max(range[1][0], lastRange[1][0]), maxColIdx);

            // Loop through the union of last range and current range
            for (rowIdx = rowStart; rowIdx <= rowEnd; rowIdx++) {
                baseLocation = new Ext.grid.Location(view, store.getAt(rowIdx));
                for (colIdx = colStart; colIdx <= colEnd; colIdx++) {
                    location = baseLocation.cloneForColumn(colIdx);

                    // If we are outside the current range, deselect
                    if (rowIdx < range[0][1] || rowIdx > range[1][1] || colIdx < range[0][0] || colIdx > range[1][0]) {
                        view.onCellDeselect(location);
                    } else {
                        view.onCellSelect(location);
                    }
                }
            }
            me.lastRange = range;
        },

        extendRange: function(extensionVector) {
            var me = this,
                view = me.view,
                newEndCell;

            if (extensionVector[extensionVector.type] < 0) {
                newEndCell = new Ext.grid.Location(view, {
                    record: me.getLastRowIndex(),
                    column: me.getLastColumnIndex()
                });
                me.startCell = extensionVector.start.clone();
                me.setRangeEnd(newEndCell);
                me.view.getNavigationModel().setLocation(extensionVector.start);
            } else {
                me.startCell = new Ext.grid.Location(view, {
                    record: me.getFirstRowIndex(),
                    column: me.getFirstColumnIndex()
                });
                me.setRangeEnd(extensionVector.end);
                me.view.getNavigationModel().setLocation(extensionVector.end);
            }
        },

        /**
         * Returns the `[[x, y],[x,y]]` coordinates in top-left to bottom-right order
         * of the current selection.
         *
         * If no selection, returns [[0, 0],[-1, -1]] so that an incrementing iteration
         * will not execute.
         *
         * @return {Number[][]}
         * @private
         */
        getRange: function() {
            return [[this.getFirstColumnIndex(), this.getFirstRowIndex()], [this.getLastColumnIndex(), this.getLastRowIndex()]];
        },

        /**
         * Returns the size of the selection rectangle.
         * @return {Number}
         * @private
         */
        getRangeSize: function() {
            return this.getCount();
        },

        /**
         * @private
         * Used by the SelectionModel to fire the selectionchange event with the batch of selected records
         */
        getRecords: function() {
            var rowRange = this.getRowRange();

            return this.getSelectionModel().getStore().getRange(rowRange[0], rowRange[1]);
        },

        /**
         * Returns the number of cells selected.
         * @return {Number} The nuimber of cells selected
         * @private
         */
        getCount: function() {
            var range = this.getRange();

            return (range[1][0] - range[0][0] + 1) * (range[1][1] - range[0][1] + 1);
        },

        /**
         * @private
         */
        selectAll: function() {
            var me = this,
                view = me.view,
                columns = view.getVisibleColumns();

            me.clear();
            me.setRangeStart(new Ext.grid.Location(view, {record: 0, column: 0}));
            me.setRangeEnd(new Ext.grid.Location(view, {record: view.store.last(), column: columns[columns.length - 1]}));
        },

        /**
         * @return {Boolean}
         * @private
         */
        isAllSelected: function() {
            var start = this.startCell,
                end = this.endCell;

            // All selected only if we encompass the entire store and every visible column
            if (start) {
                if (!start.columnIndex && !start.recordIndex) {
                    return end.columnIndex === end.view.getVisibleColumns().length - 1 && end.recordIndex === end.view.store.getCount() - 1;
                }
            }
            return false;
        },

        /**
         * @return {Number[]} The column range which encapsulates the range.
         * @private
         */
        getColumnRange: function() {
            return [this.getFirstColumnIndex(), this.getLastColumnIndex()];
        },

        /**
         * @private
         * Called through {@link Ext.grid.selection.SpreadsheetModel#getLastSelected} by {@link Ext.panel.Table#updateBindSelection} when publishing the `selection` property.
         * It should yield the last record selected.
         */
        getLastSelected: function() {
            return this.view.getStore().getAt(this.endCell.recordIndex);
        },

        /**
         * Returns the row range which encapsulates the range - the view range that needs
         * updating.
         * @return {Number[]}
         * @private
         */
        getRowRange: function() {
            return [this.getFirstRowIndex(), this.getLastRowIndex()];
        },

        onSelectionFinish: function() {
            var me = this,
                view = me.view;

            if (me.getCount()) {
                me.getSelectionModel().onSelectionFinish(me,
                    new Ext.grid.Location(view, {record: me.getFirstRowIndex(), column: me.getFirstColumnIndex()}),
                    new Ext.grid.Location(view, {record: me.getLastRowIndex(), column: me.getLastColumnIndex()}));
            } else {
                me.getSelectionModel().onSelectionFinish(me);
            }
        }
    }
});
