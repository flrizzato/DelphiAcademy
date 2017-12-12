/**
 * A selection model for {@link Ext.grid.Grid grids} which allows you to select data in
 * a spreadsheet-like manner.
 *
 * Supported features:
 *
 *  - Single / Range / Multiple individual row selection
 *  - Single / Range cell selection
 *  - Column selection by click selecting column headers
 *  - Select / deselect all by clicking in the top-left, header
 *  - Adds row number column to enable row selection
 *  - Optionally you can enable row selection using checkboxes
 *
 * # Example usage
 *
 *     @example
 *     Ext.create({
 *         xtype: 'grid',
 *         title: 'Simpsons',
 *         store: [{
 *             name: 'Lisa',
 *             email: 'lisa@simpsons.com',
 *             phone: '555-111-1224'
 *         }, {
 *             name: 'Bart',
 *             email: 'bart@simpsons.com',
 *             phone: '555-222-1234'
 *         }, {
 *             name: 'Homer',
 *             email: 'homer@simpsons.com',
 *             phone: '555-222-1244'
 *         }],
 *         width: 400,
 *         height: 300,
 *         renderTo: Ext.getBody(),
 *         columns: [{
 *             text: 'Name',
 *             dataIndex: 'name'
 *         }, {
 *             text: 'Email',
 *             dataIndex: 'email',
 *             flex: 1
 *         }, {
 *             text: 'Phone',
 *             dataIndex: 'phone'
 *         }],
 *         selectable: {
 *             columns: false, // Can select cells and rows, but not columns
 *             extensible: true // Uses the draggable selection extender
 *         }
 *     });
 *
 * # Using {@link Ext.data.virtual.Store}s
 * It is very important to remember that a {@link Ext.data.virtual.Store} does *not* contain the
 * full dataset. The purpose of a VirtualStore is to only hold in the client, a range of
 * pages from the dataset that corresponds with what is currently visible in the grid
 * (plus a few pages above and below the visible range to allow fast scrolling).
 *
 * When using "select all" rows and a VirtualStore, an `allSelected` flag is set, and so all
 * records which are read into the client side cache will thenceforth be selected, and will
 * be rendered as selected in the grid.
 *
 * *But records which have not been read into the cache will obviously not be available
 * when interrogating selected records. What is stored as being selected is row index ranges.*
 *
 */
Ext.define('Ext.grid.selection.Model', {
    extend: 'Ext.dataview.selection.Model',
    requires: [
        'Ext.grid.Location',
        'Ext.grid.selection.*'
    ],

    alias: 'selmodel.grid',

    isGridSelectionModel: true,

    config: {
        /**
         * @cfg {Boolean} columns
         * Set to `true` to enable selection of columns.
         *
         * **NOTE**: This will disable sorting on header click and instead provide column
         * selection and deselection. Sorting is still available via column header menu.
         */
        columns: {
            $value: false,
            lazy: true
        },

        /**
         * @cfg {Boolean} cells
         * Set to `true` to enable selection of individual cells or a single rectangular
         * range of cells. This will provide cell range selection using click, and
         * potentially drag to select a rectangular range if (@link #cfg!drag} is `true`.
         * You can also use "SHIFT + arrow" key navigation to select a range of cells.
         */
        cells: {
            $value: false,
            lazy: true
        },

        /**
         * @cfg {Boolean} rows
         * Set to `true` to enable selection of rows by clicking on the selection model's
         * {@link #cfg!checkbox} column, {@link Ext.grid.Grid#cfg!rowNumbers row number column}
         * or, if {@link #cfg!drag} is `true`, by swiping down the
         * {@link Ext.grid.Grid#cfg!rowNumbers row number column}.
         */
        rows: {
            $value: true,
            lazy: true
        },

        /**
         * @cfg {Boolean} drag
         * Set to `true` to enables cell and row range selection by dragging.
         */
        drag: false,

        /**
         * @cfg {String} extensible
         * This configures whether this selection model is to implement a mouse based
         * dragging gesture to extend a *contiguous* selection.
         *
         * Note that if there are multiple, discontiguous selected rows or columns,
         * selection extension is not available.
         *
         * If set, then the bottom right corner of the contiguous selection will display a
         * drag handle. By dragging this, an extension area may be defined into which the
         * selection is extended.
         *
         * The {@link Ext.grid.Grid#beforeselectionextend beforeselectionextend} event fires
         * at the end of the drag though the owning grid. Event handlers may manipulate the
         * store data in any way.
         *
         * Possible values for this configuration are
         *
         *    - `"x"` Only allow extending the block to the left or right.
         *    - `"y"` Only allow extending the block above or below.
         *    - `"xy"` Allow extending the block in both dimensions.
         *    - `"both"` Allow extending the block in both dimensions.
         *    - `true` Allow extending the block in both dimensions.
         *    - `false` Disable the extensible feature
         *    - `null` Disable the extensible feature
         *
         * It's important to notice that setting this to `"both"`, `"xy"` or `true` will
         * allow you to extend the selection in both directions, but only one direction at
         * a time. It will NOT be possible to drag it diagonally.
         */
        extensible: {
            $value: false,
            lazy: true
        },

        /**
         * @cfg {Boolean} checkbox
         * Configure as `true` to include a checkbox to indicate selection of *Records*. The
         * checkbox cell plays no part in cell or column selection apart from being a selected
         * cell and part of any iteration through selections.
         *
         * See {@link #cfg!headerCheckbox} for inclusion of a "select all" checkbox in the
         * column header of the checkbox column.
         *
         * See {@link #cfg!checkboxDefaults} for how to influence the configuration of the
         * checkbox column header.
         */
        checkbox: false,

        /**
         * @cfg {Boolean} headerCheckbox
         * Configure as `false` to not display the header checkbox at the top of the checkbox
         * column when {@link #checkboxSelect} is set.
         */
        headerCheckbox: true,

        /**
         * @cfg {Object} checkboxDefaults
         * A config object to configure the checkbox column header if {@link #cfg!checkbox}
         * is set.
         */
        checkboxDefaults: {
            xtype: 'selectioncolumn',
            text: null,
            width: 30
        },

        showNumbererColumn: false
    },

    /**
     * @event selectionchange
     * Fired *by the grid* after the selection changes. Return `false` to veto the selection
     * extension.
     *
     * @param {Ext.grid.Panel} grid The grid whose selection has changed.
     * @param {Ext.dataview.selection.Selection} selection A subclass of
     * {@link Ext.dataview.selection.Selection} describing the new selection.
     */

    /**
     * @cfg {Boolean} checkboxSelect
     * Enables selection of the row via clicking on checkbox. Note: this feature will add
     * new column at position specified by {@link #checkboxColumnIndex}.
     */
    checkboxSelect: false,

    /**
     * @cfg {Number/String} checkboxColumnIndex
     * The index at which to insert the checkbox column.
     * Supported values are a numeric index, and the strings 'first' and 'last'. Only valid
     * when set before render.
     */
    checkboxColumnIndex: 0,

    mode: 'multi',

    columnSelectCls: Ext.baseCSSPrefix + 'selmodel-column-select',
    rowNumbererHeaderCls: Ext.baseCSSPrefix + 'selmodel-row-numberer-hd',

    /**
     * @member Ext.grid.Grid
     * @event beforeselectionextend An event fired when an extension block is extended
     * using a drag gesture. Only fired when the grid's
     * `{@link Ext.grid.Grid.selectable #cfg!selectable}` is configured with the
     * {@link Ext.grid.selection.Model#extensible extensible} config.
     *
     * @param {Ext.grid.Grid} grid The owning grid.
     * @param {Ext.dataview.selection.Selection} An object which encapsulates a contiguous
     * selection block.
     * @param {Object} extension An object describing the type and size of extension.
     * @param {String} extension.type `"rows"` or `"columns"`
     * @param {Ext.grid.Location} extension.start The start (top left) cell of the
     * extension area.
     * @param {Ext.grid.Location} extension.end The end (bottom right) cell of the
     * extension area.
     * @param {number} [extension.columns] The number of columns extended (-ve means on
     * the left side).
     * @param {number} [extension.rows] The number of rows extended (-ve means on the top
     * side).
     */

    /**
     * @member Ext.grid.Grid
     * @event selectionextenderdrag An event fired when an extension block is dragged to
     * encompass a new range. Only fired when the grid's `{@link Ext.grid.Grid.selectable #cfg!selectable}`
     * is configured with the {@link Ext.grid.selection.Model#extensible extensible} config.
     * @param {Ext.grid.Grid} grid The owning grid.
     * @param {Ext.dataview.selection.Selection} An object which encapsulates a contiguous selection block.
     * @param {Object} extension An object describing the type and size of extension.
     * @param {String} extension.type `"rows"` or `"columns"`
     * @param {HTMLElement} extension.overCell The grid cell over which the mouse is being dragged.
     * @param {Ext.grid.Location} extension.start The start (top left) cell of the extension area.
     * @param {Ext.grid.Location} extension.end The end (bottom right) cell of the extension area.
     * @param {number} [extension.columns] The number of columns extended (-ve means on the left side).
     * @param {number} [extension.rows] The number of rows extended (-ve means on the top side).
     */

    /**
     * @private
     */
    updateView: function (view, oldView) {
        var me = this,
            rowNumberer = me.numbererColumn = view.getRowNumbers(),
            checkbox = me.getCheckbox();

        me.callParent([view, oldView]);

        if (oldView) {
            me.navigationModel = null;
            Ext.destroy(me.viewListeners);
        }

        if (view) {
            // If there is a row numberer column we can use, add our classes to it so it can
            // get a different UI if the theme requires it. Also the header cursor is a
            // "select all" diagonal arrow.
            if (rowNumberer) {
                // If the grid shows a row numberer, add our class
                rowNumberer.setCell({
                    cls: me.rowNumbererCellCls
                });
                rowNumberer.setCls(me.rowNumbererHeaderCls);
            }

            if (checkbox) {
                view.registerColumn(checkbox);
            }

            me.viewListeners = view.on(me.getViewListeners());
        }
    },

    /**
     * @private
     * Called after the view has completed its initialization.
     * @param view
     */
    onViewCreated: function(view) {
        // Add class to add special cursor pointer to column headers
        if (this.getColumns()) {
            view.addCls(this.columnSelectCls);
        }
        this.updateHeaderState();
    },

    updateDrag: function (drag) {
        var view = this.getView(),
            viewListeners = {
                dragstart: 'onViewDragStart',
                delegate: view.eventDelegate,
                scope: this
            };

        // Start a drag on longpress if touch is supported.
        if (Ext.supports.Touch) {
            viewListeners.longpress = 'onViewLongpress';
        }
        view.innerCt[drag ? 'on' : 'un'](viewListeners);
    },

    /**
     * @private
     * @param {String} what {"rows"/"records'/"cells"/"columns"} What kind of object is to be selected.
     * @param {Boolean} reset
     * @return {Ext.dataview.selection.Selection} A Selection object of the required type.
     */
    getSelection: function (what, reset) {
        // The two are interchangeable, to callers, but virtual stores use
        // row range selection as opposed to record collection.
        if (what === 'rows' || what === 'records') {
            what = this.getStore().isVirtualStore ? 'rows' : 'records';
        }

        var result = this.callParent(),
            config;

        // If called with a required type, ensure that the selection object
        // is of that type.
        if (what) {
            what = what.toLowerCase();
            if (!result || result.type !== what) {
                config = {
                    type: what
                };
                if (what === 'records') {
                    config.selected = this.getSelected();
                }
                this.setSelection(config);
                result = this.callParent();
            } else if (reset) {
                result.clear(true);
            }
        }
        return result;
    },

    /**
     * Retrieve a configuration to be used in a HeaderContainer.
     * This should be used when checkboxSelect is set to false.
     * @private
     */
    createCheckboxColumn: function(checkboxDefaults) {
        var me = this;

        return Ext.apply({
            headerCheckbox: me.getHeaderCheckbox() !== false
        }, checkboxDefaults);
    },

    /**
     * @private
     */
    onHeaderTap: function(headerCt, header, e) {
        var me = this,
            sel = me.getSelection(),
            range, columns, i;

        // A click on the numberer column toggles all
        if (header === this.numbererColumn) {
            me.toggleAll(header, e);
        }
        // A column select click: exclude the checkbox column
        else if (me.getColumns() && header !== me.getCheckbox()) {

            // SHIFT means select range from last selected to here
            if (e.shiftKey && sel && sel.lastColumnSelected) {

                // CTRL means keep current selection
                if (!e.ctrlKey) {
                    sel.clear();
                }
                headerCt = me.getView().getHeaderContainer();
                columns = headerCt.getColumns();
                range = Ext.Array.sort([headerCt.indexOfLeaf(sel.lastColumnSelected), 
                                        headerCt.indexOf(header)], Ext.Array.numericSortFn);

                for (i = range[0]; i <= range[1]; i++) {
                    me.selectColumn(columns[i], true);
                }
            } else {
                if (me.isColumnSelected(header)) {
                    me.deselectColumn(header);
                    me.getSelection().lastColumnSelected = null;
                } else {
                    me.selectColumn(header, e.ctrlKey);
                    me.getSelection().lastColumnSelected = header;
                }
            }
        }
    },

    /**
     * @private
     */
    toggleAll: function(header, e) {
        var me = this,
            sel = me.getSelection();

        e.stopEvent();
        // Not all selected, select all
        if (!sel || !sel.isAllSelected()) {
            me.selectAll();
        } else {
            me.deselectAll();
        }
        me.updateHeaderState();
        me.lastColumnSelected = null;
    },

    selectByLocation: function(location) {
        //<debug>
        if (!location.isGridLocation) {
            Ext.raise('selectByLocation MUST be passed an Ext.grid.Location');
        }
        //</debug>
        var me = this,
            record = location.record,
            column = location.column;

        if (me.getCells()) {
            me.selectCells(location, location);
        } else if (me.getRows() && record) {
            this.select(record);
        } else if (me.getColumns() && column) {
            me.selectColumn(column);
        }
    },

    /**
     * @private
     */
    updateHeaderState: function() {
        // check to see if all records are selected
        var me = this,
            store = me.getStore(),
            sel = me.getSelection(),
            isChecked = false,
            checkHd = me.getCheckbox(),
            storeCount;

        if (store && sel && sel.isRows) {
            storeCount = store.getCount();
            if (store.isBufferedStore) {
                isChecked = sel.allSelected;
            } else {
                isChecked = storeCount > 0 && (storeCount === sel.getCount());
            }
        }

        if (checkHd) {
            checkHd.setHeaderStatus(isChecked);
        }
    },

    /**
     * Intercepts the grid's updateColumns method.  Adds the checkbox header.
     * @param headerCt
     * @param {Object[]} columns
     * @private
     */
    onColumnUpdate: function(headerCt, columns) {
        var me = this,
            checkColumn = me.getCheckbox();

        if (checkColumn) {
            // This is being called from a reconfigure operation - from updateColumns
            // so we have to preserve our column from destruction
            if (headerCt) {
                headerCt.remove(checkColumn, false);
            }

            columns.push(checkColumn);
        }
    },

    select: function(records, keepExisting, suppressEvent) {
        // API docs are inherited
        var me = this,
            sel = me.getSelection('records'),
            store = me.getStore(),
            len, i, record;

        if (!Ext.isArray(records)) {
            records = [records];
        }
        len = records.length;
        for (i = 0; i < len; i++) {
            record = records[i];
            if (typeof record === 'number') {
                records[i] = record = store.getAt(record);
            }
        }

        // SelectionObject will call fireSelectionChange if necessary
        sel.add(records, keepExisting, suppressEvent);
    },

    deselect: function(records, suppressEvent) {
        // API docs are inherited
        var me = this,
            sel = me.getSelection('records'),
            store = me.getView().getStore(),
            len, i, record;

        if (sel && sel.isRecords) {
            if (!Ext.isArray(records)) {
                records = [records];
            }
            len = records.length;
            for (i = 0; i < len; i++) {
                record = records[i];
                if (typeof record === 'number') {
                    records[i] = record = store.getAt(record);
                }
            }
        }

        // SelectionObject will call fireSelectionChange if necessary
        sel.remove(records, suppressEvent);
    },

    onCollectionRemove: function(selectedCollection, chunk) {
        this.updateHeaderState();
        this.callParent([selectedCollection, chunk]);
    },

    onCollectionAdd: function(selectedCollection, adds) {
        this.updateHeaderState();
        this.callParent([selectedCollection, adds]);
    },

    /**
     * This method allows programmatic selection of the cell range.
     *
     *     @example
     *     var store = Ext.create('Ext.data.Store', {
     *         fields  : ['name', 'email', 'phone'],
     *         data    : {
     *             items : [
     *                 { name : 'Lisa',  email : 'lisa@simpsons.com',  phone : '555-111-1224' },
     *                 { name : 'Bart',  email : 'bart@simpsons.com',  phone : '555-222-1234' },
     *                 { name : 'Homer', email : 'homer@simpsons.com', phone : '555-222-1244' },
     *                 { name : 'Marge', email : 'marge@simpsons.com', phone : '555-222-1254' }
     *             ]
     *         },
     *         proxy   : {
     *             type   : 'memory',
     *             reader : {
     *                 type : 'json',
     *                 root : 'items'
     *             }
     *         }
     *     });
     *
     *     var grid = Ext.create('Ext.grid.Grid', {
     *         title    : 'Simpsons',
     *         store    : store,
     *         width    : 400,
     *         renderTo : Ext.getBody(),
     *         columns  : [
     *            columns: [
     *               { text: 'Name',  dataIndex: 'name' },
     *               { text: 'Email', dataIndex: 'email', flex: 1 },
     *               { text: 'Phone', dataIndex: 'phone', width:120 },
     *               {
     *                   text:'Combined', dataIndex: 'name', width : 300,
     *                   renderer: function (value, metaData, record, rowIndex,
     *                                       colIndex, store, view) {
     *                       console.log(arguments);
     *                       return value + ' has email: ' + record.get('email');
     *                   }
     *               }
     *           ],
     *         ],
     *         selType: 'spreadsheet'
     *     });
     *
     *     var selectable = grid.getSelectable();  // get selection model
     *
     *     // We will create range of 4 cells.
     *
     *     // Now set the range  and prevent rangeselect event from being fired.
     *     // We can use a simple array when we have no locked columns.
     *     selectable.selectCells([0, 0], [1, 1], true);
     *
     * @param rangeStart {Ext.grid.Location/Number[]} Range starting position. Can be
     * either Cell context or a `[rowIndex, columnIndex]` numeric array.
     *
     * Note that when a numeric array is used in a locking grid, the column indices are
     * relative to the outermost grid, encompassing locked *and* normal sides.
     * @param rangeEnd {Ext.grid.Location/Number[]} Range end position. Can be either Cell
     * context or a `[rowIndex, columnIndex]` numeric array.
     *
     * Note that when a numeric array is used in a locking grid, the column indices are
     * relative to the outermost grid, encompassing locked *and* normal sides.
     * @param {Boolean} [suppressEvent] Pass `true` to not fire the `{@link #selectionchange}`
     * event.
     */
    selectCells: function(rangeStart, rangeEnd, suppressEvent) {
        var me = this,
            view = me.getView(),
            sel;

        rangeStart = rangeStart.isGridLocation ? rangeStart.clone() : new Ext.grid.Location(view, {
            record: rangeStart[0],
            column: rangeStart[1]
        });
        rangeEnd = rangeEnd.isGridLocation ? rangeEnd.clone() : new Ext.grid.Location(view, {
            record: rangeEnd[0],
            column: rangeEnd[1]
        });

        me.resetSelection(true);

        sel = me.getSelection('cells');
        sel.setRangeStart(rangeStart);
        sel.setRangeEnd(rangeEnd);

        if (!suppressEvent) {
            me.fireSelectionChange();
        }
    },

    /**
     * Select all the data if possible.
     *
     * If {@link #rows} is `true`, then all *records* will be selected.
     *
     * If {@link #cells} is `true`, then all *rendered cells* will be selected.
     *
     * If {@link #columns} is `true`, then all *columns* will be selected.
     *
     * @param {Boolean} [suppressEvent] Pass `true` to prevent firing the
     * `{@link #selectionchange}` event.
     */
    selectAll: function (suppressEvent) {
        var me = this,
            sel = me.getSelection(),
            doSelect;

        if (me.getRows()) {
            sel = me.getSelection('records');
            doSelect = true;
        }
        else if (me.getCells()) {
            sel = me.getSelection('cells');
            doSelect = true;
        }
        else if (me.getColumns()) {
            sel = me.getSelection('columns');
            doSelect = true;
        }

        if (doSelect) {
            sel.selectAll(suppressEvent); //this populates the selection with the records
        }
    },

    /**
     * Clears the selection.
     * @param {Boolean} [suppressEvent] Pass `true` to prevent firing the
     * `{@link #selectionchange}` event.
     */
    deselectAll: function (suppressEvent) {
        var sel = this.getSelection();
        
        if (sel && sel.getCount()) {
            sel.clear(suppressEvent);
        }
    },

    /**
     * Select one or more rows.
     * @param rows {Ext.data.Model[]} Records to select.
     * @param {Boolean} [keepSelection=false] Pass `true` to keep previous selection.
     * @param {Boolean} [suppressEvent] Pass `true` to prevent firing the
     * `{@link #selectionchange}` event.
     */
    selectRows: function(rows, keepSelection, suppressEvent) {
        var sel = this.getSelection('records');

        if (!keepSelection) {
            this.resetSelection(true);
        }
        sel.add(rows, keepSelection, suppressEvent);
    },

    isSelected: function(record) {
        // API docs are inherited.
        return this.isRowSelected(record);
    },

    /**
     * Selects a column.
     * @param {Ext.grid.column.Column} column Column to select.
     * @param {Boolean} [keepSelection=false] Pass `true` to keep previous selection.
     * @param {Boolean} [suppressEvent] Pass `true` to prevent firing the
     * `{@link #selectionchange}` event.
     */
    selectColumn: function(column, keepSelection, suppressEvent) {
        var selData = this.getSelection('columns');

        if (!selData.isSelected(column)) {
            if (!keepSelection) {
                selData.clear(suppressEvent);
            }
            selData.add(column);
        }
    },

    /**
     * Deselects a column.
     * @param {Ext.grid.column.Column} column Column to deselect.
     * @param {Boolean} [suppressEvent] Pass `true` to prevent firing the
     * `{@link #selectionchange}` event.
     */
    deselectColumn: function(column, suppressEvent) {
        var selData = this.getSelection();

        if (selData && selData.isColumns && selData.isSelected(column)) {
            selData.remove(column, suppressEvent);
        }
    },

    destroy: function() {
        var me = this,
            view = me.getView(),
            checkbox = me.checkbox;

        if (view && !view.destroying && checkbox) {
            view.unregisterColumn(checkbox, true);
        }

        Ext.destroy(me.viewListeners, me.extensible);
        me.callParent();
    },

    //-------------------------------------------------------------------------

    privates: {
        /**
         * @property {Object} axesConfigs
         * Use when converting the extensible config into a SelectionExtender to create
         * its `axes` config to specify which axes it may extend.
         * @private
         */
        axesConfigs: {
            x: 1,
            y: 2,
            xy: 3,
            both: 3,
            "true": 3 // reserved word MUST be quoted when used an a property name
        },

        /**
         * @return {Object}
         * @private
         */
        getViewListeners: function() {
            return {
                columnschanged: 'onColumnsChanged',
                columnmove: 'onColumnMove',
                keyup: {
                    element: 'innerCt',
                    fn: 'onViewKeyUp',
                    scope: this
                },
                scope: this,
                destroyable: true
            };
        },

        /**
         * @private
         */
        onViewKeyUp: function(e) {
            var sel = this.getSelection();

            // Released the shift key, terminate a keyboard based range selection
            if (e.keyCode === e.SHIFT && sel && sel.isRows && sel.getRangeSize()) {
                // Copy the drag range into the selected records collection
                sel.addRange();
            }
        },

        /**
         * @private
         */
        refreshSelection: function() {
            if (this.getSelection().isRecords) {
                this.callParent();
            }
            else {
                this.resetSelection();
            }
        },

        /**
         * @private
         */
        onColumnsChanged: function() {
            var me = this,
                selData = me.getSelection(),
                view, selectionChanged;

            // When columns have changed, we have to deselect *every* cell in the row range
            // because we do not know where the columns have gone to.
            if (selData) {
                view = selData.view;

                if (selData.isCells) {
                    if (view.visibleColumns().length) {
                        selData.eachCell(function(location) {
                            view.onCellDeselect(location);
                        });
                    } else {
                        me.clearSelections();
                    }
                }

                // We have to deselect columns which have been hidden/removed
                else if (selData.isColumns) {
                    selectionChanged = false;
                    selData.eachColumn(function(column) {
                        if (!column.isVisible() || !view.isAncestor(column)) {
                            me.remove(column);
                            selectionChanged = true;
                        }
                    });
                }
            }

            // This event is fired directly from the HeaderContainer before the view updates.
            // So we have to wait until idle to update the selection UI.
            // NB: fireSelectionChange calls updateSelectionExtender after firing its event.
            Ext.on('idle', selectionChanged ? me.fireSelectionChange : me.updateSelectionExtender, me, {
                single: true
            });
        },

        // The selection may have acquired or lost contiguity, so the replicator may need
        // enabling or disabling
        onColumnMove: function() {
            this.updateSelectionExtender();
        },

        /**
         * @private
         */
        resetSelection: function(suppressEvent) {
            var sel = this.getSelection();

            if (sel) {
                sel.clear(suppressEvent);
            }
        },

        onViewLongpress: function(e) {
            if (e.pointerType === 'touch') {
                e.startDrag();
            }
        },

        /**
         * Plumbing for drag selection of cell range
         * @private
         */
        onViewDragStart: function(e) {
            // For touch gestures, only initiate drags on longpress
            if (e.pointerType === 'touch' && !e.longpress) {
                return;
            }

            var me = this,
                view = me.getView(),
                location = new Ext.grid.Location(view, e),
                header = location.column,
                viewLocation = view.getNavigationModel().getLocation(),
                isCheckClick = header === me.getCheckbox(),
                sel;

            if (!location.cell) {
                return;
            }

            // Ignore right click, shift and alt modifiers.
            // Ignore when actionableMode is true so we can select the text inside an editor
            if (e.claimed || e.button > 0 || e.shiftKey || e.altKey || 
                    (viewLocation && viewLocation.actionable) || !view.shouldSelectItem(e)) {
                return;
            }

            if (header) {
                e.claimGesture();
                me.mousedownPosition = location.clone();

                if (isCheckClick) {
                    me.checkCellClicked = location.cell.element.dom;
                }

                // Differentiate between row and cell selections.
                if (header === me.numbererColumn || isCheckClick || !me.getCells()) {
                    // Enforce rows setting
                    if (me.getRows()) {
                        // If checkOnly is set, and we're attempting to select a row 
                        // outside of the checkbox column, reject
                        if (!isCheckClick && me.checkboxOnly) {
                            return;
                        }
                        sel = me.getSelection('records');
                        if (!e.ctrlKey && !isCheckClick) {
                            sel.clear();
                        }
                    } else if (me.getColumns()) {
                        sel = me.getSelection('columns');
                        if (!e.ctrlKey && !isCheckClick) {
                            sel.clear();
                        }
                    } else {
                        return false;
                    }
                } else {
                    sel = me.getSelection('cells');
                    sel.clear();
                }

                me.lastDragLocation = null;

                // If it was a lomgpress, begin selection now.
                // If it was a mousemove, then there will be a drag gesture coming right along.
                if (e.longpress) {
                    location.row.removeCls(view.pressedCls);
                    me.onViewSelectionDrag(e);
                }

                // Only begin the drag process if configured to select what they asked for
                if (sel) {
                    // Add the listener after the view has potentially been corrected
                    view.innerCt.on('dragend', me.onViewDragEnd, me, { single: true });

                    me.mousemoveListener = view.innerCt.on({
                        drag: 'onViewSelectionDrag',
                        scope: me,
                        delegate: view.eventDelegate,
                        destroyable: true
                    });
                }
            }
        },

        /**
         * Selects range based on mouse movements
         * @param e
         * @private
         */
        onViewSelectionDrag: function(e) {
            me = this;
            view = me.getView();

            // The target of a Touch object remains unchanged from the touchstart target
            // even if the touch point moves outside of the original target.
            // We determine view Location from the "over" target, so polyfill using
            // the touch coordinates and document.elementFromPoint.
            if (e.changedTouches) {
                touch = e.changedTouches[0];

                // If the target does not contain the touch point, we have to correct it.
                if (touch && !Ext.fly(touch.target).getRegion().contains(touch.point)) {
                    realTarget = Ext.event.Event.resolveTextNode(
                        Ext.Element.fromPagePoint(touch.pageX, touch.pageY, true));

                    // Points can sometimes go negative and return no target.
                    if (realTarget) {
                        e.target = realTarget;
                    }
                }
            }

            // Will fire when outside cells (on borders, row bodies and headers/footers).
            // We must only process cells.
            if (!Ext.fly(e.target).up(view.eventDelegate)) {
                return;
            }

            var me,
                view,
                newLocation = me.dragLocation = new Ext.grid.Location(view, e),
                overColumn = newLocation.column,
                overRecord = newLocation.record,
                overRowIdx = newLocation.recordIndex,
                lastDragLocation = me.lastDragLocation,
                selData, lastOverRecord, lastOverColumn, recChange, colChange, touch, realTarget;

            e.claimGesture();
            if (lastDragLocation) {
                lastOverRecord = lastDragLocation.record;
                lastOverColumn = lastDragLocation.column;
            }

            // When the mousedown happens in a checkcolumn....
            if (me.checkCellClicked) {
                selData = me.getSelection('rows');
                selData.setRangeStart(me.getStore().indexOf(overRecord));
                me.checkCellClicked = null;
                return;
            } else {
                selData = me.getSelection();
            }

            // Disable until a valid new selection is announced in fireSelectionChange
            if (me.getExtensible()) {
                me.getExtensible().disable();
            }

            if (overColumn) {
                recChange = overRecord !== lastOverRecord;
                colChange = overColumn !== lastOverColumn;

                // Initial mousedown was in rownumberer or checkbox overColumn
                if (selData.isRows || selData.isRecords) {
                    // Only react if we've changed row
                    if (recChange) {
                        if (lastOverRecord) {
                            selData.setRangeEnd(overRowIdx);
                        } else {
                            selData.setRangeStart(overRowIdx);
                        }
                    }
                }
                // Selecting cells
                else if (selData.isCells) {
                    // Only react if we've changed row or overColumn
                    if (recChange || colChange) {
                        if (lastOverRecord) {
                            selData.setRangeEnd(newLocation);
                        } else {
                            selData.setRangeStart(newLocation);
                        }
                    }
                }
                // Selecting columns
                else if (selData.isColumns) {
                    // Only react if we've changed overColumn
                    if (colChange) {
                        if (lastOverColumn) {
                            selData.setRangeEnd(newLocation.column);
                        } else {
                            selData.setRangeStart(newLocation.column);
                        }
                    }
                }

                // Focus MUST follow the mouse.
                // Otherwise the focus may scroll out of the rendered range and revert to document
                if (recChange || colChange) {
                    view.getNavigationModel().setLocation(newLocation);
                }
                me.lastDragLocation = newLocation;
            }
        },

        /**
         * Clean up mousemove event
         * @param e
         * @private
         */
        onViewDragEnd: function(e) {
            var me = this,
                view = me.getView(),
                dragLocation = me.dragLocation,
                changedCell = !dragLocation || !dragLocation.equals(me.mousedownPosition),
                location = e.location;

            me.checkCellClicked = null;

            if (view && !view.destroyed) {
                // If we catch the event before the View sees it and stamps a position in,
                // we need to know where they mouseupped.
                if (!location) {
                    e.location = new Ext.grid.Location(view, e);
                }

                // Disable until a valid new selection is announced in fireSelectionChange
                // unless it's a click
                if (me.getExtensible() && changedCell) {
                    me.getExtensible().disable();
                }

                me.mousemoveListener.destroy();

                // Copy the records encompassed by the drag range into the record collection
                if (me.getSelection().isRows) {
                    me.getSelection().addRange();
                }

                // Fire selection change only if we have dragged - if the mouseup position
                // is different from the mousedown position.
                // If there has been no drag, the click handler will select the single row
                else if (changedCell) {
                    me.fireSelectionChange();
                }
            }
        },

        /**
         * Called when the grid's Navigation model detects navigation events (`mousedown`,
         * `click` and certain `keydown` events).
         * @param {Ext.event.Event} navigateEvent The event which caused navigation.
         * @private
         */
        onNavigate: function(navigateEvent) {
            var me = this,
                selectingRows = me.getRows(),
                selectingCells = me.getCells(),
                selectingColumns = me.getColumns(),
                checkbox = me.getCheckbox(),
                checkboxOnly = me.checkboxOnly,
                mode = me.getMode(),
                location = navigateEvent.to,
                toColumn = location.column,
                record = location.record,
                sel = me.getSelection(),
                ctrlKey = navigateEvent.ctrlKey,
                shiftKey = navigateEvent.shiftKey,
                changedRow, selectionChanged;

            // Honour the stopSelection flag which any prior handlers may set.
            // A SelectionColumn handles its own processing.
            if (navigateEvent.stopSelection || toColumn === me.checkboxColumn) {
                return;
            }

            // *key* navigation
            if (!navigateEvent.pointerType && navigateEvent.getKey() !== navigateEvent.SPACE) {
                // CTRL/key just navigates, does not select
                if (ctrlKey) {
                    return;
                }

                // If within a row and not going to affect cell or column selection, then ignore.
                changedRow = !navigateEvent.from || (location.recordIndex !== navigateEvent.from.recordIndex);
                if (!changedRow && !(selectingCells || selectingColumns)) {
                    return;
                }
            }

            // Click is the mouseup at the end of a multi-cell/multi-column select swipe; reject.
            if (sel && (sel.isCells || (sel.isColumns && selectingRows && !(ctrlKey || shiftKey))) &&
                    sel.getCount() > 1 && !shiftKey && navigateEvent.type === 'click') {
                return;
            }

            // If all selection types are disabled, or it's not a selecting event, return
            if (!(selectingCells || selectingColumns || selectingRows) || !record || 
                    navigateEvent.type === 'mousedown') {
                return;
            }

            // Ctrl/A key - Deselect current selection, or select all if no selection
            if (ctrlKey && navigateEvent.keyCode === navigateEvent.A && mode === 'multi') {
                // No selection, or only one, select all
                if (!sel || sel.getCount() < 2) {
                    me.selectAll();
                } else {
                    me.deselectAll();
                }
                me.updateHeaderState();
                return;
            }

            if (shiftKey && mode === 'multi') {
                // If the event is in one of the row selecting cells, or cell selecting is
                // turned off
                if (toColumn === me.numbererColumn || toColumn === me.checkColumn || 
                        !(selectingCells || selectingColumns) ||
                        (sel && (sel.isRows || sel.isRecords))) {
                    if (selectingRows) {
                        // If checkOnly is set, and we're attempting to select a row outside
                        // of the checkbox column, reject
                        if (toColumn !== checkbox && checkboxOnly) {
                            return;
                        }
                        // Ensure selection object is of the correct type
                        sel = me.getSelection('records');
                        // First shift
                        if (!sel.getRangeSize()) {
                            sel.setRangeStart(me.selectionStart || location.recordIndex);
                        }
                        sel.setRangeEnd(location.recordIndex);
                        sel.addRange(true);
                        selectionChanged = true;
                    }
                }
                // Navigate event in a normal cell
                else {
                    if (selectingCells) {
                        // Ensure selection object is of the correct type
                        sel = me.getSelection('cells');
                        // First shift
                        if (!sel.getRangeSize()) {
                            sel.setRangeStart(navigateEvent.from || new Ext.grid.Location(me.getView(), {
                                record: 0,
                                column: 0
                            }));
                        }
                        sel.setRangeEnd(location);
                        selectionChanged = true;
                    } else if (selectingColumns) {
                        // Ensure selection object is of the correct type
                        sel = me.getSelection('columns');
                        if (!sel.getCount()) {
                            sel.setRangeStart(toColumn);
                        }
                        sel.setRangeEnd(toColumn);
                        selectionChanged = true;
                    }
                }
            } else {
                me.selectionStart = null;
                if (sel && mode !== 'multi') {
                    sel.clear(true);
                }

                // If we are selecting rows and (the event is in one of the row selecting
                // cells or we're *only* selecting rows) then select this row
                if (selectingRows && (toColumn === me.numbererColumn ||
                        toColumn === checkbox || !selectingCells)) {
                    // If checkOnly is set, and we're attempting to select a row outside
                    // of the checkbox column, reject
                    // Also reject if we're navigating by key within the same row.
                    if (toColumn !== checkbox && checkboxOnly || (navigateEvent.keyCode &&
                            navigateEvent.from && record === navigateEvent.from.record)) {
                        return;
                    }

                    // Ensure selection object is of the correct type
                    sel = me.getSelection('records');

                    if (sel.isSelected(record)) {
                        if (ctrlKey || toColumn === checkbox || me.getDeselectable()) {
                            sel.remove(record);
                            selectionChanged = true;
                        }
                    } else {
                        sel.add(record, ctrlKey || toColumn === checkbox);
                        selectionChanged = true;
                    }
                    me.selectionStart = location.recordIndex;
                }
                // Navigate event in a normal cell
                else {
                    // Prioritize cell selection over column selection
                    if (selectingCells) {
                        // Ensure selection object is of the correct type and cleared.
                        sel = me.getSelection('cells', true);
                        sel.setRangeStart(location);
                        selectionChanged = true;
                    }
                    else if (selectingColumns) {
                        // Ensure selection object is of the correct type
                        sel = me.getSelection('columns');

                        if (ctrlKey) {
                            if (sel.isSelected(toColumn)) {
                                sel.remove(toColumn);
                            } else {
                                sel.add(toColumn);
                            }
                        } else {
                            sel.setRangeStart(toColumn);
                        }
                        selectionChanged = true;
                    }
                }
            }

            // If our configuration allowed selection changes, update check header and fire event
            if (selectionChanged) {
                // Base class reacts to RecordSelection mutating its record Collection
                // It will fire the events and update the checked header state.
                if (!sel.isRecords) {
                    me.fireSelectionChange(null, true);
                }
            }
        },

        /**
         * Check if given column is currently selected.
         *
         * @param {Ext.grid.column.Column} column
         * @return {Boolean}
         * @private
         */
        isColumnSelected: function(column) {
            var me = this,
                sel = me.getSelection(),
                ret = false;

            if (sel && sel.isColumns) {
                ret = sel.isSelected(column);
            }

            return ret;
        },

        /**
         * Returns true if specified cell within specified view is selected
         *
         * Used in {@link Ext.grid.Row} rendering to decide upon cell UI treatment.
         * @param {Number/Ext.grid.Location/Ext.data.Model} row - The Row index/record or
         * {@link Ext.grid.Location the grid Location} to test.
         * @param {Number} column - Column index to test.
         *
         * @return {Boolean}
         * @private
         */
        isCellSelected: function(row, column) {
            var sel = this.getSelection();

            if (sel) {
                if (sel.isColumns) {
                    if (typeof column === 'number') {
                        column = this.getView().getVisibleColumns()[column];
                    }
                    return sel.isSelected(column);
                }

                if (sel.isCells) {
                    return sel.isSelected(row, column);
                }

                // We're selecting records or rows.
                // The cell is selected if the record is.
                return sel.isSelected(row);
            }

            return false;
        },

        /**
         * @private
         */
        updateSelection: function(selection, oldSelection) {
            var view = this.getView();

            // Destroy old selection.
            Ext.destroy(oldSelection);

            // Update the UI to match the new selection
            if (selection && selection.getCount()) {
                view = selection.view;

                // Rows; update each selection row
                if (selection.isRows) {
                    selection.eachRow(view.onRowSelect, view);
                }
                // Columns; update the selection columns for all rows
                else if (selection.isColumns) {
                    selection.eachCell(view.onCellSelect, view);
                }
                // Cells; update each selection cell
                else if (selection.isCells) {
                    selection.eachCell(view.onCellSelect, view);
                }
            }
        },

        /**
         * Show/hide the extra column headers depending upon rowSelection.
         * @private
         */
        updateRows: function(rows) {
            var sel;

            if (!rows) {
                // checkboxSelect depends on rowsSelect
                this.setCheckbox(false);

                sel = this.getSelection()
                if (sel && sel.isRows) {
                    sel.clear();
                }
            }
        },

        /**
         * Enable/disable the HeaderContainer's sortOnClick in line with column select on
         * column click.
         * @private
         */
        updateColumns: function(columns) {
            var me = this,
                view = me.getView(),
                sel = me.getSelection();

            if (!columns && sel && sel.isColumns) {
                sel.clear();
                me.fireSelectionChange();
            }
            view.toggleCls(me.columnSelectCls, !!columns);
        },

        /**
         * @private
         */
        updateCells: function(cells) {
            var me = this,
                sel = me.getSelection();

            if (!cells && sel && sel.isCells) {
                sel.clear();
                me.fireSelectionChange();
            }
        },

        updateMode: function(mode) {
            // If multi, we can use drag or not, so revert to initial value
            if (mode === 'multi') {
                this.setDrag(this.getInitialConfig().drag);
            }
            // With 'simple' or 'single', drag makes no sense
            else if (!this.isConfiguring) {
                this.setDrag(false);
            }
        },

        /**
         * @private
         * @param {Ext.data.Model[]} records. *ONLY* passed if called from the base class's
         * onCollectionAdd/Remove observers on the *record* collection.
         * @param {Boolean} selecting. *ONLY* passed if called from the base class's
         * onCollectionAdd/Remove observers on the *record* collection.
         */
        fireSelectionChange: function(records, selecting) {
            var me = this,
                view = me.getView(),
                selection = me.getSelection();

            // Inform selection object that we're done
            me.updateSelectionExtender();

            // Our own event
            me.fireEvent('selectionchange', view,  me.getSelection());

            // Fire Grid's selectionchange event.
            // Only pass records if the selection type can yield them
            view.fireEvent('selectionchange', view, 
                    selection.isRecords ? records : 
                           (selection.isCells ? selection.getRecords() : null),
                    selecting, me.getSelection());
        },

        updateSelectionExtender: function() {
            var sel = this.getSelection();
            if (sel) {
                sel.onSelectionFinish();
            }
        },

        /**
         * Called when a selection has been made. The selection object's onSelectionFinish
         * calls back into this.
         * @param {Ext.dataview.selection.Selection} sel The selection object specific to
         * the selection performed.
         * @param {Ext.grid.Location} [firstCell] The left/top most selected cell.
         * Will be undefined if the selection is clear.
         * @param {Ext.grid.Location} [lastCell] The bottom/right most selected cell.
         * Will be undefined if the selection is clear.
         * @private
         */
        onSelectionFinish: function(sel, firstCell, lastCell) {
            var extensible = this.getExtensible();

            if (extensible) {
                extensible.setHandle(firstCell, lastCell);
            }
        },

        applyExtensible: function(extensible, oldExtensible) {
            var me = this,
                axes;

            // if extensible is false/null we should return undefined so the value
            // does not get set and we don't call updateExtensible
            if (!extensible) {
                return undefined;
            }

            if (extensible === true || typeof extensible === 'string') {
                axes = me.axesConfigs[extensible];

                // if we already have an extensible, just update it's config
                if (oldExtensible) {
                    oldExtensible.setAxes(axes);
                    return oldExtensible;
                }

                extensible = {
                    axes: axes
                };
            } else {
                extensible = Ext.Object.chain(extensible); // don't mutate the user's config
            }

            extensible.view = me.getView();

            // if this wasn't a simple axes update, destroy the old extensible
            // so we don't end up with multiple extensibles on the view.
            if (oldExtensible) {
                oldExtensible.destroy();
            }

            return new Ext.grid.selection.SelectionExtender(extensible);
        },

        applyCheckbox: function(checkbox) {
            var me = this;

            if (checkbox) {
                me.checkboxOnly = checkbox === 'only';
                me.checkboxColumn = checkbox = Ext.create(me.createCheckboxColumn(
                        me.getCheckboxDefaults()));
            }

            return checkbox;
        },

        updateCheckbox: function(checkbox, oldCheckbox) {
            var me = this,
                view;

            if (!me.isConfiguring) {
                view = me.getView();
                if (oldCheckbox) {
                    view.unregisterColumn(oldCheckbox, true);
                }

                if (checkbox) {
                    view.registerColumn(checkbox);
                    // rows selection is required so force it
                    me.setRows(true);
                }
            }
        },

        applyView: function(view) {
            // In a locking assembly, we talk to the owner
            return view.ownerGrid;
        },

        /**
         * Called when the SelectionExtender has the mouse released.
         * @param {Object} extension An object describing the type and size of extension.
         * @param {String} extension.type `"rows"` or `"columns"`
         * @param {Ext.grid.Location} extension.start The start (top left) cell of the
         * extension area.
         * @param {Ext.grid.Location} extension.end The end (bottom right) cell of the
         * extension area.
         * @param {number} [extension.columns] The number of columns extended (-ve means
         * on the left side).
         * @param {number} [extension.rows] The number of rows extended (-ve means on the
         * top side).
         * @private
         */
        extendSelection: function(extension) {
            var me = this,
                view = me.getView(),
                sel = me.getSelection();

            // Announce that the selection is to be extended, and if no objections, extend it
            if (view.fireEvent('beforeselectionextend', view, sel, extension) !== false) {
                sel.extendRange(extension);

                // Base class reacts to RowSelection mutating its record Collection
                // It will fire the events.
                if (!sel.isRows) {
                    me.fireSelectionChange();
                }
            }
        },

        /**
         * @private
         */
        onIdChanged: function(store, rec, oldId, newId) {
            var sel = this.getSelection();

            if (sel && sel.isRecords) {
                sel.getSelected().updateKey(rec, oldId);
            }
        },

        /**
         * @private
         */
        onSelectionStoreAdd: function() {
            this.callParent(arguments);
            this.updateHeaderState();
        },

        /**
         * @private
         */
        onSelectionStoreClear: function() {
            this.callParent(arguments);
            this.updateHeaderState();
        },

        /**
         * @private
         */
        onSelectionStoreLoad: function() {
            this.callParent(arguments);
            this.updateHeaderState();
        }
    }
}, function (GridModel) {
    var RowNumberer = Ext.ClassManager.get('Ext.grid.column.RowNumberer'),
        cellCls;

    if (RowNumberer) {
        cellCls = Ext.grid.column.RowNumberer.prototype.cellCls;
        // This class adds the e-resize cursor on hover to indicate availability of selection
        GridModel.prototype.rowNumbererCellCls =
            (cellCls ? (cellCls + ' ') : '') + Ext.baseCSSPrefix + 'selmodel-row-numberer-cell';
    }
});
