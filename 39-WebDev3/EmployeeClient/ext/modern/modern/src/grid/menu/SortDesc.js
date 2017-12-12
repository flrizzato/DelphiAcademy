/**
 * The menu item shown in a grid column's menu that when clicked
 * triggers descending sorting on that column.
 */
Ext.define('Ext.grid.menu.SortDesc', {
    extend: 'Ext.menu.RadioItem',

    xtype: 'gridsortdescmenuitem',

    iconCls: Ext.baseCSSPrefix + 'headermenu-sort-desc',

    /**
     * @cfg {String} text
     * The menu item text for the Sort Ascending menu item.
     * @locale
     */
    text: 'Sort Descending',

    value: 'DESC',

    allowUncheck: true,

    group: 'grid-sorters'
});
