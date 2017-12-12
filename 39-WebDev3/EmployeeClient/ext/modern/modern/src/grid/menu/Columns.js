/**
 * This class is created by `Ext.grid.Grid` to display the columns in a menu.
 * @since 6.5.0
 */
Ext.define('Ext.grid.menu.Columns', {
    extend: 'Ext.menu.Item',

    xtype: 'gridcolumnsmenu',

    iconCls: Ext.baseCSSPrefix + 'headermenu-columns-icon',

    /**
     * @cfg {String} text
     * The menu item text for the column visibility sub-menu.
     * @locale
     */
    text: 'Columns',

    menu: {},

    updateMenu: function (menu, oldMenu) {
        this.callParent([menu, oldMenu]);
        if (menu) {
            this.menuListeners = menu.on({
                beforeshow: 'onBeforeShowColumnsMenu',
                checkchange: {
                    fn: 'onCheckItem',
                    delegate: 'menucheckitem'
                },
                scope: this,
                destroyable: true
            });
        } else {
            Ext.destroy(this.menuListeners);
        }
    },

    onBeforeShowColumnsMenu: function (menu) {
        var me = this,
            grid = me.grid,
            columns = grid.getHeaderContainer().items.items,
            items = [],
            len = columns.length,
            i, column;

        for (i = 0; i < len; ++i) {
            column = columns[i];

            // If the column has the ability to hide, add it to the menu.
            // The item itself enables/disables depending on whether it is
            // contextually hideable. That means that there are other
            // menu offering columns still visible.
            // See HeaderContainer#updateMenuDisabledState for keeping this
            // synched while hiding and showing columns.
            if (column.getHideable()) {
                items.push(column.getHideShowMenuItem());
            }
        }

        // The MenuCheckItems are persistent, and lazily owned by each column.
        // We just remove non-destructively here, and add the new payload.
        menu.removeAll(false);
        menu.add(items);
    },

    onCheckItem: function (menuItem, checked) {
        menuItem.column.setHidden(!checked);
    }
});
