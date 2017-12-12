/**
 * This class is used in the column menu of a `Ext.grid.Grid`.
 * @since 6.5.0
 */
Ext.define('Ext.grid.menu.GroupByThis', {
    extend: 'Ext.menu.Item',

    xtype: 'gridgroupbythismenuitem',

    iconCls: Ext.baseCSSPrefix + 'headermenu-group-by-this',

    /**
     * @cfg {String} text
     * The menu item text for the "Group by this field" menu item.
     * @locale
     */
    text: 'Group by this field'
});
