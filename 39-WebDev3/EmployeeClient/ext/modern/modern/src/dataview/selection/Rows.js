/**
 * A class which encapsulates a range of rows defining a selection in a grid.
 *
 */
Ext.define('Ext.dataview.selection.Rows', {
    extend: 'Ext.dataview.selection.Selection',
    alias: 'selection.rows',

    requires: [
        'Ext.util.Spans'
    ],

    /**
     * @property {Boolean} isRows
     * This property indicates the this selection represents selected rows.
     * @readonly
     */
    isRows: true,

    config: {
        /**
         * @cfg {Ext.util.Spans} selected
         * A cache of start/end row ranges which encpsulates the selected rows.
         * @readonly
         */
        selected: true
    },

    //-------------------------------------------------------------------------
    // Base Selection API

    clone: function() {
        return new this.self({
            selectionModel: this.getSelectionModel(),
            selected: new Ext.util.Spans().unstash(this.getSelected().stash())
        });
    },

    //-------------------------------------------------------------------------
    // Methods unique to this type of Selection

    add: function(range, keepExisting, suppressEvent) {
        var me = this,
            view = me.view,
            rowIdx;

        // Single element array - extract it.
        // We cannot accept an array of records in this Selection class
        // because we deal in row ranges.
        if (range.length === 1) {
            range = range[0];
        }
        // Adding a record selects that index
        if (range.isEntity) {
            range = view.mapToRecordIndex(range);
        }
        // Adding a single index - create an *EXCLUSIVE* range
        if (typeof range === 'number') {
            range = [range, range + 1];
        }

        //<debug>
        if (range.length !== 2 || (typeof range[0] !== 'number') || (typeof range[1] !== 'number')) {
            Ext.raise('add must be called with a [start, end] row index *EXCLUSIVE* range');
        }
        //</debug>

        if (range[0] > range[1]) {
            var tmp = range[1];
            range[1] = range[0];
            range[0] = tmp;
        }

        me.lastSelected = range[1];
        if (!keepExisting) {
            me.clear();
        }
        me.getSelected().add(range);

        for (rowIdx = range[0]; rowIdx < range[1]; rowIdx++) {
            view.onItemSelect(rowIdx);
        }

        if (!suppressEvent) {
            me.getSelectionModel().fireSelectionChange();
        }
    },

    remove: function(range, suppressEvent) {
        var me = this,
            selModel = me.getSelectionModel(),
            view = me.view,
            rowIdx;

        // If the selection model is deselectable: false, which means there must
        // always be a selection, reject deselection of the last record
        if (!selModel.getDeselectable() && me.getCount() === 1) {
            return;
        }
        // Single element array - extract it.
        // We cannot accept an array of records in this Selection class
        // because we deal in row ranges.
        if (range.length === 1) {
            range = range[0];
        }
        // Removing a record selects that index
        if (range.isEntity) {
            range = view.mapToRecordIndex(range);
        }
        // Removing a single index - create an *EXCLUSIVE* range
        if (typeof range === 'number') {
            range = [range, range + 1];
        }

        //<debug>
        if (!range.length === 2 && (typeof range[0] === 'number') && (typeof range[1] === 'number')) {
            Ext.raise('remove must be called with a [start, end] record *EXCLUSIVE* range');
        }
        if (range[0] > range[1]) {
            Ext.raise('A range MUST have the start index first, and the exclusive end index second');
        }
        //</debug>

        me.getSelected().remove(range);

        for (rowIdx = range[0]; rowIdx < range[1]; rowIdx++) {
            view.onItemDeselect(rowIdx);
        }

        if (!suppressEvent) {
            selModel.fireSelectionChange();
        }
    },

    /**
     * Returns `true` if the passed {@link Ext.data.Model record} is selected.
     * @param {Ext.data.Model} record The record to test.
     * @return {Boolean} `true` if the passed {@link Ext.data.Model record} is selected.
     */
    isSelected: function (record) {
        var me = this,
            ranges = me.getSelected().spans,
            len = ranges.length,
            recIndex, range, i;

        recIndex = record.isEntity ? me.view.getStore().indexOf(record) : record;
        for (i = 0; i < len; i++) {
            range = ranges[i];
            if (recIndex >= range[0] && recIndex < range[1]) {
                return true;
            }
        }

        return false;
    },

    /**
     * Returns the number of records selected
     * @return {Number} The number of records selected.
     */
    getCount: function() {
        return this.getSelected().getCount();
    },

    selectAll: function() {
        var view = this.view,
            items = view.dataItems,
            len = items.length,
            i;

        // Apply selected rendition to all view items.
        // Buffer rendered items will appear selected
        // because the rendering pathway consults the selection.
        for (i = 0; i < len; i++) {
            view.onItemSelect(i);
        }

        // We have just one range encompassing all rows.
        // Note that the Spans API is exclusive of range end index.
        this.getSelected().add(0, view.store.getTotalCount() || view.store.getCount());

        this.getSelectionModel().fireSelectionChange();
    },

    /**
     * @return {Number} The row index of the first row in the range or zero if no range.
     */
    getFirstRowIndex: function() {
        var ranges = this.getSelected().spans;

        return ranges.length ? this.getSelected().spans[0][0] : 0;
    },

    /**
     * @return {Number} The row index of the last row in the range or -1 if no range.
     */
    getLastRowIndex: function() {
        var ranges = this.getSelected().spans;

        return ranges.length ? ranges[ranges.length - 1][1] - 1 : 0;
    },

    eachRow: function(fn, scope) {
        var me = this,
            ranges = me.getSelected().spans,
            len = ranges && ranges.length,
            result, range, i, j;

        for (i = 0; i < len; i++) {
            range = ranges[i];
            for (j = range[0]; result !== false && j < range[1]; j++) {
                result = fn.call(this || scope, j);
            }
        }
    },

    eachColumn: function(fn, scope) {
        var columns = this.view.getHeaderContainer().getVisibleColumns(),
            len = columns.length,
            i;

        // If we have any records selected, then all visible columns are selected.
        if (this.getCount()) {
            for (i = 0; i < len; i++) {
                if (fn.call(this || scope, columns[i], i) === false) {
                    return;
                }
            }
        }
    },

    eachCell: function(fn, scope) {
        var me = this,
            selection = me.getSelected(),
            view = me.view,
            columns = view.ownerGrid.getVisibleColumnManager().getColumns(),
            range = me.dragRange,
            colCount,
            i,
            j,
            location,
            recCount,
            abort = false;

        if (columns) {
            colCount = columns.length;
            location = new Ext.grid.Location(view);

            // Use Collection#each instead of copying the entire dataset into an array and iterating that.
            if (selection) {
                me.eachRow(function(recordIndex) {
                    location.setItem(recordIndex);
                    for (i = 0; i < colCount; i++) {
                        location.setColumn(columns[i]);
                        if (fn.call(scope || me, location, location.columnIndex, location.recordIndex) === false) {
                            abort = true;
                            return false;
                        }
                    }
                });
            }
            
            // If called during a drag select, or SHIFT+arrow select, include the drag range
            if (!abort && range != null) {
                me.view.getStore().getRange(range[0], range[1], {
                    forRender: false,
                    callback: function(records) {
                        recCount = records.length;
                        for (i = 0; !abort && i < recCount; i++) {
                            location.setItem(records[i]);
                            for (j = 0; !abort && j < colCount; j++) {
                                location.setColumn(columns[j]);
                                if (fn.call(scope || me, location, location.columnIndex, location.recordIndex) === false) {
                                    abort = true;
                                }
                            }
                        }
                    }
                });
            }
        }
    },

    //-------------------------------------------------------------------------

    privates: {
        applySelected: function(spans) {
            if (!spans.isSpans) {
                spans = new Ext.util.Spans();
            }
            return spans;
        },

        compareRanges: function(lhs, rhs) {
            return lhs[0] - rhs[0];
        },

        /**
         * @private
         */
        clear: function(suppressEvent) {
            var me = this,
                selModel = me.getSelectionModel(),
                view = me.view,
                items = view.dataItems,
                len = items.length,
                i;

            // Apply selected rendition to all view items.
            // Buffer rendered items will appear selected
            // because the rendering pathway consults the selection.
            for (i = 0; i < len; i++) {
                view.onItemDeselect(i);
            }
            me.getSelected().clear();

            // Enforce our selection model's deselectable: false by re-adding the last selected index.
            // Suppress event because we might be firing it.
            if (!selModel.getDeselectable() && me.lastSelected) {
                me.add(me.lastSelected, true, true);
            }

            if (!suppressEvent) {
                selModel.fireSelectionChange();
            }
        },

        addRecordRange: function(start, end) {
            return this.add([start, end + 1], true);
        },

        removeRecordRange: function(start, end) {
            return this.remove([start, end + 1]);
        },

        /**
         * @return {Boolean}
         * @private
         */
        isAllSelected: function() {
            return this.getCount() === (this.view.getStore().getTotalCount() || this.view.store.getCount());
        },

        /**
         * Used during drag/shift+downarrow range selection on start.
         * @param {Number} start The start row index of the row drag selection.
         * @private
         */
        setRangeStart: function(start) {
            if (start == null) {
                this.dragRange = null;
            } else {
                this.dragRange = [start, start];

                // This is just theoretical for now - we are simply defining a range, not adding to the collection.
                // So we have to programatically sync the view state.
                this.view.onItemSelect(start);
            }
        },

        /**
         * Used during drag/shift+downarrow range selection on change of row.
         * @param {Number} end The end row index of the row drag selection.
         * @private
         */
        setRangeEnd: function(end) {
            var me = this,
                dragRange = me.dragRange,
                oldEnd = dragRange[1] || 0,
                start = dragRange[0],
                view = me.view,
                renderInfo = view.renderInfo,
                tmp = dragRange[1] = end,
                rowIdx, limit;

            // Ranges retain whatever start end end point, regardless of order
            // We just need the real start and end index to test candidates for inclusion.
            if (start > end) {
                end = start;
                start = tmp;
            }

            // Loop through the union of previous range and newly set range
            for (rowIdx = Math.max(Math.min(dragRange[0], start, oldEnd, end), renderInfo.indexTop),
                limit = Math.min(Math.max(dragRange[0], start, oldEnd, end), renderInfo.indexBottom - 1); rowIdx <= limit; rowIdx++) {

                // If we are outside the current dragRange, deselect
                if (rowIdx < start || rowIdx > end) {
                    // If we are deselecting - swiping back towards the start, the record index must be genuinely deselected
                    me.removeRowRange(rowIdx, rowIdx);

                    // This is just theoretical for now - we are simply defining a dragRange, not adding to the collection.
                    // So we have to programatically sync the view state.
                    view.onItemDeselect(rowIdx);
                } else {
                    view.onItemSelect(rowIdx);
                }
            }

            me.lastSelectedIndex = end;
        },

        /**
         * Called at the end of a drag, or shift+downarrow row range select.
         * The record range delineated by the start and end row indices is added to the selected Collection.
         * @private
         */
        addRange: function(keep) {
            var range = this.dragRange;

            if (range) {
                // Must use addRecordRange.
                // Subclass's add API uses records, not indices.
                // the recordRange API always uses indices/
                this.addRecordRange(range[0], range[1]);

                if (!keep) {
                    this.dragRange = null;
                }
            }
        },

        extendRange: function(extensionVector) {
            // Must use addRecordRange.
            // Subclass's add API uses records, not indices.
            // the recordRange API always uses indices/
            this.addRecordRange(extensionVector.start, extensionVector.end);
        },

        /**
         * @return {Number[]}
         * @private
         */
        getRange: function() {
            var range = this.dragRange;

            if (range == null) {
                return [0, -1];
            }

            if (range[0] <= range[1]) {
                return range;
            }
            return [range[1], range[0]];
        },

        /**
         * Returns the size of the mousedown+drag, or SHIFT+arrow selection range.
         * @return {Number}
         * @private
         */
        getRangeSize: function() {
            var range = this.getRange();
            return range[1] - range[0] + 1;
        },

        onSelectionFinish: function() {
            var me = this,
                range = me.getContiguousSelection();

            if (range) {
                me.getSelectionModel().onSelectionFinish(me,
                    new Ext.grid.Location(me.view, {record: range[0], column: 0}),
                    new Ext.grid.Location(me.view, {record: range[1], column: me.view.getHeaderContainer().getVisibleColumns().length - 1}));
            } else {
                me.getSelectionModel().onSelectionFinish(me);
            }
        },

        getContiguousSelection: function() {
            var selected = this.getSelected(),
                store = this.view.store,
                spans = selected.spans;

            // If there's only one span, and the store contains the start and end, we can
            // allow the range extender.
            if (spans === 1 && store.getAt(spans[0][0]) && store.getAt(spans[0][1])) {
                return selected.spans[0];
            }
        }
    }
});
