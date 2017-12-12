/**
 * This column renders the hierarchy column of a tree or tree grid.
 */
Ext.define('Ext.grid.column.Tree', {
    extend: 'Ext.grid.column.Column',

    xtype: 'treecolumn',

    config: {
        cell: {
            xtype: 'treecell'
        }
    },

    isTreeColumn: true
});
