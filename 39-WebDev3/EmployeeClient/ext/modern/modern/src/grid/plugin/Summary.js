/**
 * This {@link Ext.grid.Grid grid} plugin manages a bottom-docked summary {@link #row row}.
 *
 * By default, the column's {@link Ext.grid.column.Column#cfg!dataIndex dataIndex} is used
 * to read from the {@link Ext.data.Store#getSummaryRecord summary record} as controlled by
 * the model's {@link Ext.data.Model#cfg!summary summary} definition. To use a different
 * field, the {@link Ext.grid.column.Column#cfg!summaryDataIndex summaryDataIndex} can be
 * specified.
 *
 * The {@link Ext.grid.column.Column#cfg!summary summary} config can be used to perform
 * column-specific summarization. The `summary` config uses one of the registered summary
 * types (see below). Custom summary types can be defined, or a column-specific algorithm
 * can be provided with a {@link Ext.grid.column.Column#cfg!summaryRenderer summaryRenderer}.
 *
 * ## Summary Types
 *
 * The `summary` type can be one of the predefined summary types:
 *
 * + {@link Ext.data.summary.Average average}
 * + {@link Ext.data.summary.Count count}
 * + {@link Ext.data.summary.Max max}
 * + {@link Ext.data.summary.Min min}
 * + {@link Ext.data.summary.Sum sum}
 *
 *
 *     @example
 *     var store = Ext.create('Ext.data.Store', {
 *         fields: ['fname', 'lname', 'talent', 'wins'],
 *         data: [
 *             { 'fname': 'Barry',  'lname': 'Allen', 'talent': 'Speedster', 'wins': 150  },
 *             { 'fname': 'Oliver', 'lname': 'Queen', 'talent': 'Archery', 'wins': 27  },
 *             { 'fname': 'Kara',   'lname': 'Zor-El', 'talent': 'All', 'wins': 75  },
 *             { 'fname': 'Helena', 'lname': 'Bertinelli', 'talent': 'Weapons Expert', 'wins': 7  },
 *             { 'fname': 'Hal',    'lname': 'Jordan', 'talent': 'Willpower', 'wins': 198  },
 *         ]
 *     });
 *
 *     Ext.create('Ext.grid.Grid', {
 *         title: 'DC Personnel',
 *
 *         store: store,
 *         plugins: {
 *             gridsummaryrow: true
 *         },
 *         columns: [
 *             { text: 'First Name', dataIndex: 'fname',  flex: 1 },
 *             { text: 'Last Name',  dataIndex: 'lname',  flex: 1 },
 *             { text: 'Talent',     dataIndex: 'talent', flex: 1 },
 *             { text: 'Wins',       dataIndex: 'wins',   flex: 1,  summary: 'sum' }
 *         ],
 *         fullscreen: true,
 *         height:275
 *     });
 *
 */
Ext.define('Ext.grid.plugin.Summary', {
    extend: 'Ext.plugin.Abstract',
    alias: [
        'plugin.gridsummary',
        'plugin.summaryrow',
        'plugin.gridsummaryrow'
    ],
    alternateClassName: 'Ext.grid.plugin.SummaryRow',

    mixins: [
        'Ext.mixin.Bufferable',
        'Ext.mixin.StoreWatcher'
    ],

    config: {
        /**
         * @cfg {Ext.grid.SummaryRow/Object} row
         * The configuration object for the docked summary row managed by this plugin.
         * @since 6.5.0
         */
        row: {
            lazy: true,
            $value: {
                xtype: 'gridsummaryrow',
                docked: 'bottom'
            }
        }
    },

    inheritUi: true,

    storeListeners: {
        add: 'syncSummary',
        clear: 'syncSummary',
        remove: 'syncSummary',
        refresh: 'syncSummary',
        update: 'syncSummary'
    },

    bufferableMethods: {
        // buffer updates to reduce re-summarization passes over the entire store.
        syncSummary: 5
    },

    init: function (grid) {
        var scrollable = grid.getScrollable(),
            row, rowScroller;

        this.setOwner(grid);
        row = this.getRow();

        grid.addCls(Ext.baseCSSPrefix + 'grid-has-summaryrow');
        if (scrollable) {
            rowScroller = row.getScrollable();
            if (!rowScroller) {
                row.setScrollable({
                    x: false,
                    y: false
                });
                rowScroller = row.getScrollable();
            }
            rowScroller.addPartner(scrollable, 'x');
        }
    },

    destroy: function () {
        this.setOwner(null);

        this.callParent();
    },

    createRow: function (config) {
        return Ext.apply({
            viewModel: this.getOwner().getItemConfig().viewModel
        }, config);
    },

    applyRow: function (row) {
        if (row) {
            row = this.createRow(row);
            row = this.cmp.add(row);
        }

        return row;
    },

    updateStore: function (store, oldStore) {
        this.mixins.storewatcher.updateStore.call(this, store, oldStore);

        if (store && store.isLoaded()) {
            // if the store is already loaded then we update summaries
            this.syncSummary();
        }
    },

    privates: {
        doSyncSummary: function () {
            var row = this.getRow();

            if (row) {
                row.syncSummary();
            }
        },

        onContainerScroll: function (scr, x) {
            var item = this.getRow(),
                scroller;

            if (!(scroller = item.getScrollable())) {
                item.setScrollable({
                    x: false,
                    y: false
                });

                scroller = item.getScrollable();
            }

            scroller.scrollTo(x, null);
        }
    }
});
