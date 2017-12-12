/**
 * This {@link Ext.grid.column.Column column} displays the row number in its cells. This
 * column is automatically created by the {@link Ext.grid.Grid#cfg!rowNumbers rowNumbers}
 * config and is not normally created directly.
 */
Ext.define('Ext.grid.column.RowNumberer', {
    extend: 'Ext.grid.column.Number',
    xtype: 'rownumberer',

    requires: [
        'Ext.util.TextMetrics'
    ],

    isRowNumberer: true,

    cell: {
        xtype: 'rownumberercell'
    },

    /**
     * @cfg {Boolean} [menu=null]
     * There is no column menu is disabled by default for check columns.
     */
    menu: null,

    align: 'right',
    hideable: false,
    ignore: true,
    ignoreExport: true,
    sortable: false,
    text: '',

    width: 'auto',
    minWidth: null,

    onAdded: function(parent, instanced) {
        var me = this,
            grid;

        me.callParent([parent, instanced]);
        me.checkWidth();

        grid = me.getGrid();

        me.gridListeners = grid.on({
            storechange: 'attachStoreListeners',
            scope: me,
            destroyable: true
        });
        me.attachStoreListeners(grid.getStore());
    },

    onRemoved: function(destroying) {
        Ext.destroy(this.gridListeners, this.storeListeners);
        this.callParent([destroying]);
    },

    privates: {
        attachStoreListeners: function(store) {
            Ext.destroy(this.storeListeners);

            if (store) {
                this.storeListeners = store.on({
                    datachanged: 'checkWidth',
                    totalcountchange: 'checkWidth',
                    scope: this,
                    destroyable: true
                });
            }
        },

        getCharWidth: function() {
            var me = this,
                charWidth = me._charWidth,
                text, cell, cellUi, gridUi, textWidth;

            if (!charWidth) {
                text = '0000000000';
                cell = me.getScratchCell();

                // Set the scratch cell's UI based on the column's cell config
                // and inherit UI from the grid like a real cell would.
                // This ensures font metrics match those of the actual cells.
                cellUi = me.getCell().ui;

                if (cellUi) {
                    cell.addUi(cellUi);
                }

                gridUi = me.getGrid().getUi();

                if (gridUi) {
                    cell.addUi(gridUi);
                }

                // Scratch cell must be in document to acquire style.
                document.body.appendChild(cell.el.dom);
                textWidth = Ext.util.TextMetrics.measure(cell.bodyElement, text).width;
                document.body.removeChild(cell.el.dom);

                me._charWidth = charWidth = textWidth / text.length;
            }

            return charWidth;
        },

        checkWidth: function() {
            var me = this;

            // If the app renders a grid on load, we must wait until fonts have loaded before
            // we measure if we have the CSS Font Loading API
            if (document.fonts) {
                document.fonts.ready.then(function() {
                    if (!me.destroyed) {
                        me.doCheckWidth();
                    }
                });
            } else {
                me.doCheckWidth();
            }
        },

        doCheckWidth: function() {
            var me = this,
                store = me.getGrid().getStore(),
                charLen = 1,
                charWidth = me.getCharWidth();

            if (store && store.getTotalCount()) {
                // Ensure we measure the *formatted* length of the largest row number
                charLen = me.getScratchCell().printValue(store.getTotalCount()).length;
            }

            me.textElement.setStyle('min-width', Math.ceil(charLen * charWidth) + 'px');
        }
    }
});
