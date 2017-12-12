/**
 * The Row Expander plugin provides an "expander" column to give the user the ability to show
 * or hide the {@link Ext.grid.Row#cfg!body body} of each row.
 *
 *     @example
 *     var store = Ext.create('Ext.data.Store', {
 *         fields: ['fname', 'lname', 'talent', 'powers'],
 *         groupField: 'powers',
 *         data: [
 *             { 'fname': 'Barry',  'lname': 'Allen', 'talent': 'Speedster', 'powers': true  },
 *             { 'fname': 'Oliver', 'lname': 'Queen', 'talent': 'Archery', 'powers': false  },
 *             { 'fname': 'Kara',   'lname': 'Zor-El', 'talent': 'All', 'powers': true  },
 *             { 'fname': 'Helena', 'lname': 'Bertinelli', 'talent': 'Weapons Expert', 'powers': false  },
 *             { 'fname': 'Hal',    'lname': 'Jordan', 'talent': 'Willpower', 'powers': true  },
 *         ]
 *     });
 *
 *     Ext.create('Ext.grid.Grid', {
 *         title: 'DC Personnel',
 *         grouped: true,
 *         store: store,
 *         plugins: {
 *             rowexpander: true
 *         },
 *         itemConfig: {
 *             body: {
 *                 tpl: '<img height="100" src="http://www.sencha.com/assets/images/sencha-avatar-64x64.png"/>'
 *             }
 *         },
 *         columns: [
 *             { text: 'First Name', dataIndex: 'fname',  flex: 1 },
 *             { text: 'Last Name',  dataIndex: 'lname',  flex: 1 },
 *             { text: 'Talent',     dataIndex: 'talent', flex: 1 },
 *             { text: 'Powers?',    dataIndex: 'powers', flex: 1 }
 *         ],
 *         height: 400,
 *         layout: 'fit',
 *         fullscreen: true
 *     });
 *
 * @since 6.2.0
 */
Ext.define('Ext.grid.plugin.RowExpander', {
    extend: 'Ext.plugin.Abstract',

    requires: [
        'Ext.grid.cell.Expander'
    ],

    alias: 'plugin.rowexpander',

    config: {
        grid: null,
        column: {
            weight: -1100,
            xtype: 'gridcolumn',
            align: 'center',
            text: '',
            width: 50,
            resizable: false,
            hideable: false,
            sortable: false,
            editable: false,
            ignore: true,
            ignoreExport: true,
            cell: {
                xtype: 'expandercell'
            },
            menuDisabled: true
        }
    },

    expanderSelector: '.' + Ext.baseCSSPrefix + 'expandercell .' + Ext.baseCSSPrefix + 'icon-el',

    init: function (grid) {
        grid.setVariableHeights(true);
        this.setGrid(grid);
    },

    destroy: function() {
        var grid = this.getGrid(),
            col = this.colInstance;

        if (col && !grid.destroying) {
            grid.unregisterColumn(col, true);
        }
        this.callParent();
    },

    applyColumn: function(column, oldColumn) {
        return Ext.factory(Ext.apply({}, column), null, oldColumn);
    },

    updateGrid: function (grid) {
        var me = this;

        if (grid) {
            grid.hasRowExpander = true;
            grid.addCls(Ext.baseCSSPrefix + 'has-rowexpander');

            me.colInstance = grid.registerColumn(me.getColumn());
            grid.refreshScrollerSize();

            grid.element.on({
                tap: 'onGridTap',
                delegate: me.expanderSelector,
                scope: me
            });
        }
    },

    onGridTap: function(e) {
        var cell = Ext.Component.from(e),
            row = cell.row;

        // May have tapped on a descendant grid row. We're only interested in our own.
        if (row.getGrid() === this.getGrid()) {
            row.toggleCollapsed();
        }
    }
});
