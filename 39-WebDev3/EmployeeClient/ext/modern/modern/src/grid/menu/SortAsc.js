/**
 * The menu item shown in a grid column's menu that when clicked
 * triggers ascending sorting on that column.
 */
Ext.define('Ext.grid.menu.SortAsc', {
    extend: 'Ext.menu.RadioItem',

    xtype: 'gridsortascmenuitem',

    iconCls: Ext.baseCSSPrefix + 'headermenu-sort-asc',

    /**
     * @cfg {String} text
     * The menu item text for the Sort Ascending menu item.
     * @locale
     */
    text: 'Sort Ascending',

    value: 'ASC',

    allowUncheck: true,

    group: 'grid-sorters'
});
