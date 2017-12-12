/**
 * This is the base class for {@link Ext.grid.Grid grid} cells.
 *
 * {@link Ext.grid.Row Rows} create cells based on the {@link Ext.grid.column.Column#cell}
 * config. Application code would rarely create cells directly.
 */
Ext.define('Ext.grid.cell.Base', {
    extend: 'Ext.Widget',
    xtype: 'gridcellbase',

    isGridCell: true,

    mixins: [
        'Ext.mixin.Toolable'
    ],

    cachedConfig: {
        /**
         * @cfg {"left"/"center"/"right"} align
         * The value for the `text-align` of the cell content.
         */
        align: null,

        /**
         * @cfg {String} cls
         * An arbitrary CSS class to add to the cell's outermost element.
         */
        cls: null,

        /**
         * @cfg {String} bodyCls
         * An arbitrary CSS class to add to the cell's inner element (the element that
         * typically contains the cell's text).
         */
        bodyCls: null,

        /**
         * @cfg {String/Object} bodyStyle
         * Additional CSS styles that will be rendered into the cell's inner element (the
         * element that typically contains the cell's text).
         *
         * You can pass either a string syntax:
         *
         *     bodyStyle: 'background:red'
         *
         * Or by using an object:
         *
         *     bodyStyle: {
         *         background: 'red'
         *     }
         *
         * When using the object syntax, you can define CSS Properties by using a string:
         *
         *     bodyStyle: {
         *         'border-left': '1px solid red'
         *     }
         *
         * Although the object syntax is much easier to read, we suggest you to use the
         * string syntax for better performance.
         */
        bodyStyle: null,

        /**
         * @cfg {String} cellCls
         *
         * @protected
         */
        cellCls: null,

        /**
         * @cfg {Boolean} [selectable=true]
         * Set to `false` to disable selection of the record when tapping on this cell.
         */
        selectable: null
    },

    config: {
        /**
         * @cfg {Ext.grid.column.Column} column
         * The grid column that created this cell.
         * @readonly
         */
        column: null,

        /**
         * @cfg {Boolean} hidden
         * The hidden state of this cell (propagated from the column's hidden state).
         * @private
         */
        hidden: false,

        /**
         * @cfg {Ext.data.Model} record
         * The currently associated record.
         * @readonly
         */
        record: null,

        /**
         * @cfg {Mixed} value
         * The value of the {@link Ext.grid.column.Column#dataIndex dataIndex} field of
         * the associated record. Application code should not need to set this value.
         */
        value: null
    },

    classCls: Ext.baseCSSPrefix + 'gridcell',
    dirtyCls: Ext.baseCSSPrefix + 'dirty',

    alignCls: {
        left: Ext.baseCSSPrefix + 'align-left',
        center: Ext.baseCSSPrefix + 'align-center',
        right: Ext.baseCSSPrefix + 'align-right'
    },

    inheritUi: true,

    cellSelector: '.' + Ext.baseCSSPrefix + 'gridcell',

    defaultBindProperty: 'value',

    toolDefaults: {
        zone: 'head',
        ui: 'gridcell'
    },

    getTemplate: function() {
        var template = {
            reference: 'bodyElement',
            cls: Ext.baseCSSPrefix + 'body-el',
            uiCls: 'body-el'
        };

        // hook for subclasses to add elements inside the inner element
        // e.g. checkcell, expandercell
        if (!(template.children = this.innerTemplate)) {
            // Otherwise ensure that cells have content and achieve a proper height
            template.html = '\xA0';
        }

        return [template];
    },

    doDestroy: function() {
        this.setColumn(null);
        this.setRecord(null);
        this.mixins.toolable.doDestroy.call(this);

        this.callParent();
    },

    getComputedWidth: function() {
        return this.getHidden() ? 0 : this.getWidth();
    },

    updateAlign: function (align, oldAlign) {
        var me = this,
            alignCls = me.alignCls;

        if (oldAlign) {
            me.removeCls(alignCls[oldAlign]);
        }

        if (align) {
            //<debug>
            if (!alignCls[align]) {
                Ext.raise("Invalid value for align: '" + align + "'");
            }
            //</debug>
            me.addCls(alignCls[align]);
        }

        me.syncToolableAlign();
    },

    updateBodyCls: function(cellCls, oldCellCls) {
        if (cellCls || oldCellCls) {
            this.bodyElement.replaceCls(oldCellCls, cellCls);
        }
    },

    updateBodyStyle: function(style){
        this.bodyElement.applyStyles(style);
    },

    updateCellCls: function(cls, oldCls) {
        this.element.replaceCls(oldCls, cls);
    },

    updateCls: function(cls, oldCls) {
        this.element.replaceCls(oldCls, cls);
    },

    updateColumn: function (column) {
        var dataIndex = null,
            row = this.row;

        if (column) {
            dataIndex = ((row && row.isSummaryRow) && column.getSummaryDataIndex()) ||
                    column.getDataIndex();
        }

        this.dataIndex = dataIndex;
    },

    updateRecord: function () {
        if (!this.destroyed && !this.destroying) {
            this.refresh();
        }
    },

    updateSelectable: function (value) {
        this.toggleCls(Ext.baseCSSPrefix + 'item-no-select', value === false);
    },

    refresh: function (ctx) {
        var me = this,
            was = me.refreshContext,
            context, modified, value;

        if (!me.isBound('value')) {
            ctx = ctx || was;
            modified = ctx && ctx.modified;

            if (!modified || me.bound(modified)) {
                me.refreshContext = context = me.beginRefresh(ctx);

                value = me.refreshValue(context);

                if (value !== me.getValue()) {
                    me.setValue(value);
                }
                else if (me.writeValue) {
                    me.writeValue();
                }

                me.refreshContext = was;
            }
        }
    },

    refreshValue: function (context) {
        var me = this,
            record = context.record,
            dataIndex = context.dataIndex,
            value, dirty, modified;

        if (context.summary) {
            value = me.summarize(context);
        }
        else if (record && dataIndex) {
            value = record.get(dataIndex);
            modified = record.modified;
            dirty = !!(modified && modified.hasOwnProperty(dataIndex));

            if (dirty !== me.$dirty) {
                me.toggleCls(me.dirtyCls, dirty);

                me.$dirty = dirty;
            }
        }

        return value;
    },

    privates: {
        //<debug>
        refreshCounter: 0,
        //</debug>

        $dirty: false,

        /**
         * @property {Object} refreshContext
         * This object holds a cache of information used across the cells of a row during
         * a `refresh` pass (when the record changes).
         * @since 6.5.0
         * @private
         */
        refreshContext: null,

        storeMethodRe: /^(?:average|max|min|sum)$/,

        augmentToolHandler: function (tool, args) {
            // args = [ cell, tool, ev ]   ==>   [ grid, info ]
            var info = args[1] = {
                event: args.pop(),
                record: this.getRecord(),
                column: this.getColumn(),
                cell: args[0],
                tool: args[1]
            };

            args[0] = info.grid = info.column.getGrid();
        },

        beginRefresh: function (context) {
            var me = this,
                column = me.getColumn(),
                row = me.row;

            // Ask our parent row or column to kick things off...
            context = context || (row ? row.beginRefresh() : {
                    record: me.getRecord()
                });

            //<debug>
            ++me.refreshCounter; // for testing
            context.from = context.from || 'cell';
            //</debug>

            context.cell = me;
            context.column = column;
            context.dataIndex = me.dataIndex;
            context.scope = column.getScope();

            return context;
        },

        /**
         * Returns `true` if this cell's value is bound to any of the given `fields`. This
         * is typically due to the `dataIndex`.
         * @param {Object} fields An object keyed by field names with truthy values.
         * @return {Boolean}
         * @since 6.5.1
         * @private
         */
        bound: function (fields) {
            return !!fields[this.dataIndex];
        },

        summarize: function (context) {
            var me = this,
                column = context.column,
                summaryType = column.getSummaryType(),
                dataIndex = context.dataIndex,
                group = context.group,
                store = context.store,
                records = context.records,
                value;

            if (summaryType) {
                //<debug>
                if (!column.$warnSummaryType) {
                    column.$warnSummaryType = true;
                    Ext.log.warn('[column] summaryType is deprecated; use summaryRenderer (' +
                        column.getId() + ')');
                }
                //</debug>

                if (Ext.isFunction(summaryType)) {
                    value = summaryType.call(store, store.data.items.slice(), dataIndex);
                }
                else if (summaryType === 'count') {
                    value = store.getCount();
                }
                else if (me.storeMethodRe.test(summaryType)) {
                    value = store[summaryType](dataIndex);
                }
                else {
                    value = Ext.callback(summaryType, null,
                        [ store.data.items.slice(), dataIndex, store ], 0, me);
                }
            }
            else if (!(summaryType = column.getSummary())) {
                if (dataIndex) {
                    value = context.record.get(dataIndex);
                }
            }
            // summaryType is an Ext.data.summary.* fellow
            //<debug>
            else if (!dataIndex) {
                Ext.raise('Cannot use summary config w/o summaryDataIndex or dataIndex (' +
                        context.grid.getId() + ')');
            }
            //</debug>
            else {
                //<debug>
                if (group) {
                    if (group.isVirtualGroup) {
                        Ext.raise('Cannot calculate a group summary on a virtual store (' +
                            context.grid.getId() + ')');
                    }
                }
                else if (store.getRemoteSort()) {
                    Ext.raise('Cannot calculate a summary on a remoteSort store (' +
                        context.grid.getId() + ')');
                }
                //</debug>

                value = summaryType.calculate(records, dataIndex, 'data', 0, records.length);
            }

            return value;
        }
    }, // privates

    deprecated: {
        '6.5': {
            configs: {
                innerStyle: 'bodyStyle',
                innerCls: 'bodyCls'
            }
        }
    }
});
