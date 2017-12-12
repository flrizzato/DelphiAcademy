/**
 * This class is used in the column menu of a `Ext.grid.Grid`.
 * @since 6.5.0
 */
Ext.define('Ext.grid.menu.ShowInGroups', {
    extend: 'Ext.menu.CheckItem',

    xtype: 'gridshowingroupsmenuitem',

    hideOnClick: true,

    /**
     * @cfg {String} text
     * Text displayed in the grid header for enabling/disabling grouping.
     * @locale
     */
    text: 'Show in groups'
});
