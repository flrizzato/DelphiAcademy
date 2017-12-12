/**
 * A column for simple {@link Ext.grid.cell.Text text cell}s.
 */
Ext.define('Ext.grid.column.Text', {
    extend: 'Ext.grid.column.Column',

    requires: [
        'Ext.grid.cell.Text'
    ],

    xtype: 'textcolumn',

    cell: {
        xtype: 'textcell'
    }
});
