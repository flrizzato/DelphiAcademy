/**
 * A class which encapsulates a collections of records defining a selection in a {@link Ext.dataview.DataView}
 *
 */
Ext.define('Ext.dataview.selection.Records', {
    extend: 'Ext.dataview.selection.Rows',
    alias: 'selection.records',

    /**
     * @property {Boolean} isRecords
     * This property indicates the this selection represents selected records.
     * @readonly
     */
    isRecords: true,

    config: {
        /**
         * @cfg {Ext.util.Collection} selected
         * A {@link Ext.util.Collection} instance, or configuration object used to create
         * the collection of selected records.
         * @readonly
         */
        selected: null
    },

    //<debug>
    constructor: function(config) {
        this.callParent([config]);
        var selected = this.getSelected();
        if (!(selected && selected.isCollection)) {
            Ext.raise('An Ext.dataview.selection.Records object MUST be configured with a "selected" Collection');
        }
    },
    //</debug>

    //-------------------------------------------------------------------------
    // Base Selection API

    clone: function() {
        return new this.self({
            selectionModel: this.getSelectionModel(),
            selected: this.getSelected()
        });
    },

    //-------------------------------------------------------------------------
    // Methods unique to this type of Selection

    addRowRange: function(start, end, keepExisting, suppressEvent) {
        //<debug>
        if (typeof start !== 'number' || typeof end !== 'number') {
            Ext.raise('addRange must be called with a [start, end] row index *EXCLUSIVE* range');
        }
        //</debug>

        // swap values
        if (start > end) {
            var tmp = end;
            end = start;
            start = tmp;
        }

        // Maintainer: The Store getRange API is historically inclusive
        this.add(this.getSelectionModel().getStore().getRange(start, end - 1), keepExisting, suppressEvent);
    },

    removeRowRange: function(start, end, suppressEvent) {
        //<debug>
        if (typeof start !== 'number' || typeof end !== 'number') {
            Ext.raise('addRange must be called with a [start, end] row index *EXCLUSIVE* range');
        }
        //</debug>

        // swap values
        if (start > end) {
            var tmp = end;
            end = start;
            start = tmp;
        }

        // Maintainer: The Store getRange API is historically inclusive
        this.remove(this.getSelectionModel().getStore().getRange(start, end - 1), suppressEvent);
    },

    add: function(records, keepExisting, suppressEvent) {
        records = Ext.Array.from(records);
        //<debug> Ensure they are all records
        for (var i = 0, ln = records.length; i < ln; i++) {
            if (!records[i].isEntity) {
                Ext.raise('add must be called with records or an array of records');
            }
        }
        //</debug>

        var me = this,
            selected = me.getSelected(),
            selectionCount = selected.getCount(),
            args = [keepExisting ? selectionCount : 0, keepExisting ? 0 : selectionCount, records];

        // Potentially remove existing records, and append the selected record(s) atomically.
        // The selModel will react to successful removal as an observer.
        // The selModel will need to know at that time whether the event is suppressed.
        selected.suppressEvent = suppressEvent;
        selected.splice.apply(selected, args);
        selected.suppressEvent = false;
    },

    remove: function(records, suppressEvent) {
        records = Ext.Array.from(records);
        //<debug> Ensure they are all records
        for (var i = 0, ln = records.length; i < ln; i++) {
            if (!records[i].isEntity) {
                Ext.raise('add must be called with records or an array of records');
            }
        }
        //</debug>

        var selected = this.getSelected();

        // If the selection model is deselectable: false, which means there must
        // always be a selection, reject deselection of the last record.
        if (!this.getSelectionModel().getDeselectable() && selected.getCount() === 1) {
            Ext.Array.remove(records, selected.first());
        }

        if (records.length) {
            selected.suppressEvent = suppressEvent;
            selected.remove(records);
            selected.suppressEvent = false;
        }
    },

    /**
     * Returns `true` if the passed {@link Ext.data.Model record} is selected.
     * @param {Ext.data.Model} record The record to test.
     * @return {Boolean} `true` if the passed {@link Ext.data.Model record} is selected.
     */
    isSelected: function (record) {
        if (!record || !record.isModel) {
            return false;
        }

        return !!this.getSelected().byInternalId.get(record.internalId);
    },

    /**
     * Returns the records selected.
     * @return {Ext.data.Model[]} The records selected.
     */
    getRecords: function() {
        return this.getSelected().getRange();
    },

    selectAll: function(suppressEvent) {
        var selected = this.getSelected();
        selected.suppressEvent = suppressEvent;
        selected.add(this.getSelectionModel().getStore().getRange());
        selected.suppressEvent = false;
    },

    /**
     * @return {Number} The row index of the first row in the range or zero if no range.
     */
    getFirstRowIndex: function() {
        return this.getCount() ? this.view.getStore().indexOf(this.getSelected().first()) : 0;
    },

    /**
     * @return {Number} The row index of the last row in the range or -1 if no range.
     */
    getLastRowIndex: function() {
        return this.getCount() ? this.view.getStore().indexOf(this.getSelected().last()) : -1;
    },

    eachRow: function(fn, scope) {
        var selectedRecords = this.getSelected();

        if (selectedRecords) {
            selectedRecords.each(fn, scope || this);
        }
    },

    eachColumn: function(fn, scope) {
        var columns = this.view.getHeaderContainer().getVisibleColumns(),
            len = columns.length,
            i;

        // If we have any records selected, then all visible columns are selected.
        if (this.getSelected().getCount()) {
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
            columns = view.getHeaderContainer().getVisibleColumns(),
            colCount,
            i,
            j,
            baseLocation, location,
            abort = false;

        if (columns) {
            colCount = columns.length;
            baseLocation = new Ext.grid.Location(view);

            // Use Collection#each instead of copying the entire dataset into an array and iterating that.
            if (selection) {
                selection.each(function(record) {
                    location = baseLocation.clone({
                        record: record
                    });
                    for (i = 0; i < colCount; i++) {
                        location = location.cloneForColumn(columns[i]);
                        if (fn.call(scope || me, location, location.columnIndex, location.recordIndex) === false) {
                            abort = true;
                            return false;
                        }
                    }
                });
            }
        }
    },

    /**
     * This method is called to indicate the start of multiple changes to the selected record set.
     *
     * Internally this method increments a counter that is decremented by `{@link #endUpdate}`. It
     * is important, therefore, that if you call `beginUpdate` directly you match that
     * call with a call to `endUpdate` or you will prevent the collection from updating
     * properly.
     */
    beginUpdate: function() {
        this.getSelected().beginUpdate();
    },

    /**
     * This method is called after modifications are complete on a selected row set. For details
     * see `{@link #beginUpdate}`.
     */
    endUpdate: function() {
        this.getSelected().endUpdate();
    },

    //-------------------------------------------------------------------------

    privates: {
        /**
         * @private
         */
        clear: function(suppressEvent) {
            var selected = this.getSelected(),
                spliceArgs;

            if (selected) {
                spliceArgs = [0, selected.getCount()];

                // Enforce the selection model's deselectable: false by re-adding the last selected record
                if (!this.getSelectionModel().getDeselectable()) {
                    spliceArgs[2] = selected.last();
                }

                // The SelectionModel is observer of the Collection and it will update the view.
                selected.suppressEvent = suppressEvent;
                selected.splice.apply(selected, spliceArgs);
                selected.suppressEvent = false;
            }
        },

        addRecordRange: function(start, end) {
            var tmp = end;

            if (start > end) {
                end = start;
                start = tmp;
            }
            this.getSelected().add(this.view.getStore().getRange(start, end || start));
        },

        removeRecordRange: function(start, end) {
            var tmp = end;

            if (start > end) {
                end = start;
                start = tmp;
            }
            this.getSelected().remove(this.view.getStore().getRange(start, end || start));
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

        /**
         * @return {Array} `[startRowIndex, endRowIndex]` if the selection represents a visually contiguous set of rows.
         * The SelectionReplicator is only enabled if there is a contiguous block.
         * @private
         */
        getContiguousSelection: function() {
            var store = this.view.getStore(),
                selection, len, i;

            selection = Ext.Array.sort(this.getSelected().getRange(), function(r1, r2) {
                return store.indexOf(r1) - store.indexOf(r2);
            });
            len = selection.length;
            if (len) {
                if (len === 1 && store.indexOf(selection[0]) === -1) {
                    return false;
                }
                for (i = 1; i < len; i++) {
                    if (store.indexOf(selection[i]) !== store.indexOf(selection[i - 1]) + 1) {
                        return false;
                    }
                }
                return [store.indexOf(selection[0]), store.indexOf(selection[len - 1])];
            }
        },

        // We MUST override the Rows class's implementation because that imposes a
        // clean Ext.util.Spans instance, and the Records class needs to pass the value
        // unchanged through to the updater.
        applySelected: function(selected) {
            //<debug>
            if (!selected) {
                Ext.raise("The selection model's own selected Collection must always be passed into a Records Selection");
            }
            //</debug>

            return selected;
        },

        /**
         * Called when the store is reloaded, or the data is mutated to synchronize the
         * selected collection with what is now in the store.
         */
        refresh: function() {
            var me = this,
                view = me.view,
                selModel = me.getSelectionModel(),
                storeCollection = view.getStore().getData(),
                filterFn = storeCollection.getFilters().getFilterFn(),
                ignoredFilter = selModel.ignoredFilter,
                selected = me.getSelected(),
                lastSelected = selModel.getLastSelected(),
                newLastSelected,
                selections,
                toDeselect = [],
                toReselect = [],
                selectionLength, i, rec,
                matchingSelection;

            // Uncover the unfiltered selection if it's there.
            // We only want to prune from the selection records which are
            // *really* no longer in the store.
            if (ignoredFilter) {
                if (ignoredFilter.getDisabled()) {
                    ignoredFilter = null;
                } else {
                    ignoredFilter.setDisabled(true);
                    storeCollection = storeCollection.getSource() || storeCollection;
                }
            }

            // Update the lastSelected instance with the new version from the store if any.
            if (lastSelected) {
                newLastSelected = storeCollection.get(storeCollection.getKey(lastSelected));

                // We are using the unfiltered source collection, so we must
                // filter using all filters except the ignored filter.
                // This is to accommodate a ComboBox's primaryFilter which must not
                // evict selected records from the selection.
                if (newLastSelected && ignoredFilter && !filterFn(newLastSelected)) {
                    newLastSelected = null;
                }
            }

            // If there is a current selection, build the toDeselect and toReselect lists
            if (me.getCount()) {
                selections = selected.getRange();
                selectionLength = selections.length;

                for (i = 0; i < selectionLength; i++) {
                    rec = selections[i];
                    matchingSelection = storeCollection.get(storeCollection.getKey(rec));

                    // If we are using the unfiltered source because of having to ignore only one
                    // filter, then test the filter condition here with that one filter disabled.
                    // Evict the record if it still does not pass the filter.
                    if (matchingSelection && ignoredFilter && !filterFn(matchingSelection)) {
                        matchingSelection = null;
                    }

                    if (matchingSelection) {
                        if (matchingSelection !== rec) {
                            toDeselect.push(rec);
                            toReselect.push(matchingSelection);
                        }
                    } else {
                        toDeselect.push(rec);
                    }
                }

                // Give the view an opportunity to intervene in the selection model refresh.
                // BoundLists remove any interactively added "isEntered" records from the
                // toDeselect array because they are outside the scope of the field's supplied Store.
                if (view.beforeSelectionRefresh) {
                    view.beforeSelectionRefresh(toDeselect, toReselect);
                }

                // Update the selected Collection.
                // Records which are no longer present will be in the toDeselect list
                // Records which have the same id which have returned will be in the toSelect list.
                // The SelectionModel will react to successful removal as an observer.
                // It will need to know at that time whether the event is suppressed.
                selected.suppressEvent = true;
                selected.splice(selected.getCount(), toDeselect, toReselect);
                selected.suppressEvent = false;
            }

            if (ignoredFilter) {
                ignoredFilter.setDisabled(false);
            }

            // Keep any lastSelected up to date with what's now in the store
            selModel.setLastSelected(newLastSelected || toReselect[toReselect.length - 1] || null);
        }
    }
});
