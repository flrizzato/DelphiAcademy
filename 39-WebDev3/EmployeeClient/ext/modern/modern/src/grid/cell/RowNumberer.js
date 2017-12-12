/**
 * This column type displays the record index of the record in the store.
 */
Ext.define('Ext.grid.cell.RowNumberer', {
    extend: 'Ext.grid.cell.Number',
    xtype: 'rownumberercell',

    classCls: Ext.baseCSSPrefix + 'rownumberercell',

    /**
     * @cfg {String} format
     * A format string as used by {@link Ext.util.Format#number} to format values for
     * this column.
     */
    format: '0,000',

    refreshValue: function (context) {
        var row = context.row,
            ret;

        if (context.summary) {
            ret = '\xA0';
        } else {
            ret = row ? row.$datasetIndex + 1 : null;
        }
        return ret;
    }
});
