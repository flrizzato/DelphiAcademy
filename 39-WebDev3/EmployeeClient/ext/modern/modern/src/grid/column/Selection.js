/**
 * A grid column used by the {@link Ext.grid.plugin.RowOperations RowOperations} plugin.
 *
 * This class should not be directly instantiated. Instances are created automatically
 * when using a {@link Ext.grid.plugin.RowOperations RowOperations} plugin.
 */
Ext.define('Ext.grid.column.Selection', {
    extend: 'Ext.grid.column.Check',
    xtype: 'selectioncolumn',

    classCls: Ext.baseCSSPrefix + 'selectioncolumn',

    cell: {
        cls: Ext.baseCSSPrefix + 'selection-cell'
    },

    // Not quite as far left as the numberer column
    weight: -900,

    menu: null,
    sortable: false,
    draggable: false,
    resizable: false,
    hideable: false,
    ignore: true,

    /**
     * @cfg {String} stopSelection
     * @hide
     */
    stopSelection: false,

    updateHeaderState: function() {
        if (!this.isConfiguring) {
            this.getGrid().getSelectable().updateHeaderState();
        }
    },

    toggleAll: function(e) {
        this.getGrid().getSelectable().toggleAll(this, e);
    },

    setRecordChecked:  function(record, checked, e) {
        var selectionModel = this.getGrid().getSelectable();

        if (checked) {
            selectionModel.select(record, selectionModel.getMode() !== 'single');
        } else {
            selectionModel.deselect(record);
        }
    },

    isRecordChecked: function(record) {
        return this.getGrid().getSelectable().isRowSelected(record);
    }
});
