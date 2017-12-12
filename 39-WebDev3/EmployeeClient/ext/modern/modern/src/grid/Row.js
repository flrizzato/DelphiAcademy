/**
 * This class is created by a {@link Ext.grid.Grid grid} to manage each record. Rows act
 * as containers for {@link Ext.grid.cell.Base cells}.
 *
 * Row does not extend {@link Ext.Container} to keep overhead to a minimum. Application
 * code should not need to create instances of this class directly. Rows are created by
 * the {@link Ext.dataview.List} base as configured by {@link Ext.grid.Grid}.
 */
Ext.define('Ext.grid.Row', {
    extend: 'Ext.Component',
    xtype: 'gridrow',

    requires: [
        'Ext.grid.cell.Cell',
        'Ext.grid.RowBody'
    ],

    mixins: [
        'Ext.mixin.Queryable',
        'Ext.dataview.GenericItem',
        'Ext.dataview.Pinnable'
    ],

    isGridRow: true,
    isRecordRefreshable: true,

    cachedConfig: {
        collapsed: true
    },

    config: {
        /**
         * @cfg {Object} body
         * A config object for this row's {@link Ext.grid.RowBody Row Body}.
         * When a {@link Ext.grid.plugin.RowExpander Row Expander} is used all row bodies
         * begin collapsed, and can be expanded by clicking on the row expander icon.
         * When no Row Expander is present row bodies are always expanded by default but
         * can be collapsed programmatically using {@link #collapse}.
         *
         * Be aware that if you specify a row body, the owning grid is automatically configured
         * with `{@link Ext.dataview.List#variableHeights}: true`.
         */
        body: null,

        /**
         * @cfg {String} expandedField
         * The name of a `boolean` field in the grid's record which is to be used to check expanded state.
         * Note that this field should be `true` to indicate expanded, and `false` to indicate collapsed.
         * By default the expanded state of a record is stored on the associated `grid` component allowing
         * that record to have different expand/collapse states on a per-grid basis.
         */
        expandedField: null,

        /**
         * @cfg {String} defaultCellUI
         * A default {@link #ui ui} to use for {@link Ext.grid.cell.Base cells} in this row.
         */
        defaultCellUI: null,

        /**
         * @private
         */
        stickyVisibility: null
    },

    classCls: [
        Ext.baseCSSPrefix + 'listitem',
        Ext.baseCSSPrefix + 'gridrow'
    ],

    inheritUi: true,

    expandedCls: Ext.baseCSSPrefix + 'expanded',

    element: {
        reference: 'element',
        children: [{
            reference: 'cellsElement',
            className: Ext.baseCSSPrefix + 'cells-el'
        }]
    },

    constructor: function (config) {
        this.cells = [];
        this.columnMap = {};

        this.callParent([config]);
    },

    doDestroy: function () {
        var me = this;

        me.setRecord(null);
        me.setBody(null);
        me.cells = Ext.destroy(me.cells);

        me.callParent();
    },

    /**
     * Collapses the row {@link #body}
     */
    collapse: function () {
        this.setCollapsed(true);
    },

    /**
     * Expands the row {@link #body}
     */
    expand: function () {
        this.setCollapsed(false);
    },

    toggleCollapsed: function () {
        this.setCollapsed(!this.getCollapsed());
    },

    updateCollapsed: function (collapsed) {
        var me = this,
            body = me.getBody(),
            grid = me.getParent(),
            record = me.getRecord(),
            expandField = me.getExpandedField(),
            expandedCls = me.expandedCls,
            expanderCell = me.expanderCell,
            recordsExpanded;

        // Set state correctly before any other code executes which may read this.
        if (record) {
            // We have to track the state separately, if we are not using a record
            // field to track expanded state.
            if (expandField) {
                record.set(expandField, !collapsed);
            } else {
                recordsExpanded = grid.$recordsExpanded || (grid.$recordsExpanded = {});

                if (collapsed) {
                    delete recordsExpanded[record.internalId];
                } else {
                    recordsExpanded[record.internalId] = true;
                }
            }
        }

        if (expanderCell) {
            expanderCell.setCollapsed(collapsed);
        }

        if (body) {
            if (collapsed) {
                body.hide();
                me.removeCls(expandedCls);
            } else {
                body.show();
                me.addCls(expandedCls);
            }
        }
    },

    // // Rows shrinkwrap content, so no callParent.
    // // However their headers must be widthed.
    // updateWidth: function(width) {
    //     // Do not trigger its creation, just see if we have one.
    //     var header = this.getConfig('header', false, true);
    //
    //     if (header) {
    //         header.setWidth(width);
    //     }
    // },

    applyBody: function (config, existing) {
        return Ext.updateWidget(existing, config, this, 'createBody');
    },

    createBody: function (body) {
        return Ext.merge({
            xtype: 'rowbody',
            ownerCmp: this,
            row: this,
            hidden: true
        }, body);
    },

    updateBody: function (body) {
        var me = this,
            grid = me.getParent();

        if (body) {
            me.bodyElement.appendChild(body.element);

            if (me.rendered && !body.rendered) {
                body.setRendered(true);
            }
        }

        if (grid) {
            grid.setVariableHeights(true);
            if (!grid.hasRowExpander) {
                me.expand();
            }
        }
    },

    onAdded: function (grid) {
        var me = this,
            cells = me.cells,
            cell, col, columns, i, k, n;

        me.callParent(arguments);

        if (grid) {
            columns = grid.getColumns();

            for (i = 0, n = columns.length; i < n; i++) {
                cell = cells[i];
                col = columns[i];

                // Rows can be removed and added back (due to itemCache), so make sure
                // the cells (if they exist) have the proper column. If not, we need to
                // remove all cells from that index to the end. We do that backwards to
                // make things more efficient.
                if (cell) {
                    if (cell.getColumn() === col) {
                        continue; // keep what we can
                    }

                    for (k = cells.length; k-- > i; ) {
                        cell = cells[k];
                        me.removeColumn(cell.getColumn());
                    }
                }

                me.addColumn(columns[i]);
            }
        }
    },

    addColumn: function (column) {
        this.insertColumn(this.cells.length, column);
    },

    /**
     * Returns the cells owned by this Row.
     *
     * Optionally filters the results by the passed {@link Ext.ComponentQuery
     * ComponentQuery} selector.
     * @param {String} [selector] The {@link Ext.ComponentQuery ComponentQuery} selector
     * to filter the results by.
     * @returns {Ext.grid.cell.Cell[]} The matching cells.
     */
    getCells: function(selector) {
        return selector ? Ext.ComponentQuery.query(selector, this.cells) : this.cells;
    },

    getRefItems: function(deep) {
        var result = [],
            body = this.getConfig('body', false, true),  // Don't initialize lazy
            cells = this.cells,
            len = cells && cells.length,
            i, cell;

        for (i = 0; i < len; i++) {
            cell = cells[i];
            result.push(cell);

            if (deep && cell.getRefItems) {
                result.push.apply(result, cell.getRefItems());
            }
        }

        if (body) {
            result.push(body);

            if (deep && body.getRefItems) {
                result.push.apply(result, body.getRefItems());
            }
        }

        return result;
    },

    insertColumn: function (index, column) {
        var me = this,
            cells = me.cells,
            cell;

        if (column.isHeaderGroup) {
            return;
        }

        cell = me.createCell(column);

        if (index >= cells.length) {
            me.cellsElement.appendChild(cell.element);
            cells.push(cell);
        } else {
            cell.element.insertBefore(cells[index].element);
            cells.splice(index, 0, cell);
        }

        me.columnMap[column.getId()] = cell;

        if (cell.isExpanderCell) {
            me.expanderCell = cell;
        }

        if (me.rendered) {
            cell.setRendered(true);
        }
    },

    insertColumnBefore: function(column, ref) {
        var me = this,
            map = me.columnMap,
            id = column.getId(),
            cell = map[id],
            cells = me.cells,
            refCell, refIndex, index;

        if (ref) {
            refCell = me.getCellByColumn(ref);
            refIndex = cells.indexOf(refCell);
        } else {
            refIndex = cells.length;
        }

        if (cell) {
            // Moving an existing column
            index = cells.indexOf(cell);
            Ext.Array.move(cells, index, refIndex);
            if (refCell) {
                cell.element.insertBefore(refCell.element);
            } else {
                me.cellsElement.appendChild(cell.element);
            }
        } else {
            me.insertColumn(refIndex, column);
        }
    },

    removeColumn: function (column) {
        var me = this,
            columnMap = me.columnMap,
            columnId = column.getId(),
            cell = columnMap[columnId];

        if (cell) {
            Ext.Array.remove(me.cells, cell);
            delete columnMap[columnId];
            cell.destroy();
        }
    },

    updateRecord: function (record) {
        if (!this.destroyed && !this.destroying) {
            this.refresh();
        }
    },

    setColumnWidth: function (column) {
        var cell = this.getCellByColumn(column);
        if (cell) {
            cell.setWidth(column.getComputedWidth());
        }
    },

    showColumn: function (column) {
        this.setCellHidden(column, false);
    },

    hideColumn: function (column) {
        this.setCellHidden(column, true);
    },

    getCellByColumn: function (column) {
        return this.columnMap[column.getId()];
    },

    getColumnByCell: function (cell) {
        return cell.getColumn();
    },

    updateStickyVisibility: function (value) {
        this.fireEvent('stickyvisiblitychange', value);
    },

    refresh: function (context) {
        var me = this,
            cells = me.cells,
            body = me.getBody(),
            len = cells.length,
            expandField = me.getExpandedField(),
            grid = me.getParent(),
            sm = grid.getSelectable(),
            selection = sm.getSelection(),
            isCellSelection = selection.isCells || selection.isColumns,
            i, visibleIndex, cell, record, recordsExpanded;

        // Allows cells/body to know we are bulk updating so they can avoid
        // things like calling record.getData(true) multiple times.
        me.refreshContext = context = me.beginRefresh(context);

        record = context.record;

        me.syncDirty(record);

        for (i = 0, visibleIndex = 0; i < len; ++i) {
            cell = cells[i];

            if (!context.summary || !cell.getColumn().getIgnore()) {
                if (cell.getRecord() === record) {
                    cell.refresh(context);
                }
                else {
                    cell.refreshContext = context;
                    cell.setRecord(record);
                    cell.refreshContext = null;
                }
                if (isCellSelection) {
                    cell.toggleCls(grid.selectedCls, sm.isCellSelected(me._recordIndex, visibleIndex));
                }
            }
            // Cell and column selection work on visible index.
            if (!cell.isHidden()) {
                visibleIndex++;
            }
        }

        context.cell = context.column = context.dataIndex = context.scope = null;

        if (body) {
            body.refreshContext = context;

            if (body.getRecord() === record) {
                body.updateRecord(record);
            } else {
                body.setRecord(record);
            }

            body.refreshContext = null;

            // If the plugin knows that the record contains an expanded flag
            // ensure our state is synchronized with our record.
            // Maintainer: We are testing the result of the assignment of expandedField
            // in order to avoid a messy, multiple level if...else.
            if (expandField) {
                me.setCollapsed(!record.get(expandField));
            }
            else {
                recordsExpanded = grid.$recordsExpanded || (grid.$recordsExpanded = {});

                if (grid.hasRowExpander) {
                    me.setCollapsed(!recordsExpanded[record.internalId]);
                }
            }
        }

        me.refreshContext = null;
    },

    privates: {
        refreshContext: null,

        beginRefresh: function (context) {
            var me = this,
                grid = me.getParent();

            context = context || {};

            //<debug>
            context.from = context.from || 'row';
            //</debug>
            context.grid = grid;
            context.record = me.getRecord();
            context.row = me;
            context.store = grid.store;

            return context;
        },

        createCell: function (column) {
            var cell = column.createCell(this);

            cell = Ext.create(cell);
            delete cell.$initParent;

            if (cell.inheritUi) {
                cell.doInheritUi();
            }

            // The cell element must accept focus for navigation to occur.
            // The cell component must not be focusable. It must not participate in a
            // FocusableContainer relationship with the List's container,
            // and must not react to focus events or its focus API itself.
            // It is a slave of the NavigationModel.
            cell.el.setTabIndex(-1);

            return cell;
        },

        setCellHidden: function (column, hidden) {
            var cell = this.getCellByColumn(column);

            if (cell) {
                cell.setHidden(hidden);
            }
        },

        getGrid: function () {
            return this.getParent();  // backwards compat
        }
    }
});
