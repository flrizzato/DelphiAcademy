/**
 * A field trigger that can show a menu aligned to this trigger.
 */
Ext.define('Ext.field.trigger.Menu', {
    extend: 'Ext.field.trigger.Trigger',
    xtype: 'menutrigger',
    alias: 'trigger.menu',

    cachedConfig: {
        /**
         * @cfg {String} menuAlign
         * The position to align the menu to (see {@link Ext.util.Positionable#alignTo} for more details).
         */
        menuAlign: 'tl-bl?',

        /**
         * @cfg {Boolean} [destroyMenu]
         * Whether or not to destroy any associated menu when this trigger is destroyed.
         * In addition, a value of `true` for this config will destroy the currently bound menu
         * when a new menu is set in {@link #setMenu} unless overridden by that method's destroyMenu
         * function argument.
         */
        destroyMenu: true
    },

    config: {
        // @cmd-auto-dependency { defaultType: "Ext.menu.Menu", requires: ["Ext.menu.Menu"] }
        /**
         * @cfg {Ext.menu.Menu/String/Object} menu
         * A menu or menu configuration. This can be a reference to a menu instance, a menu
         * config object or the `xtype` alias of a {@link Ext.menu.Menu menu}-derived class.
         */
        menu: {
            lazy: true,
            $value: null
        }
    },

    doDestroy: function () {
        this.setMenu(null);

        this.callParent();
    },

    applyMenu: function (menu, oldMenu) {
        if (menu) {
            if (Ext.isArray(menu)) {
                menu = {
                    items: menu
                };
            }
        }

        if (oldMenu && !this.getDestroyMenu()) {
            //do not allow Ext.factory to destroy the old menu
            oldMenu = null;
        }

        return Ext.factory(menu, Ext.menu.Menu, oldMenu);
    },

    updateMenu: function (menu, oldMenu) {
        if (oldMenu && oldMenu.ownerCmp === this) {
            delete oldMenu.ownerCmp;
        }

        if (menu) {
            menu.ownerCmp = this;
        }
    },

    onClick: function (e) {
        var menu = this.getMenu();

        if (menu) {
            this.showMenu(e, menu);
        } else {
            this.callParent([e]);
        }
    },

    showMenu: function (e, menu) {
        var isPointerEvent = !e || e.pointerType;

        menu = menu || this.getMenu();

        if (menu) {
            if (menu.isVisible()) {
                // Click/tap toggles the menu visibility.
                if (isPointerEvent) {
                    menu.hide();
                } else {
                    menu.focus();
                }
            } else {
                menu.autoFocus = !isPointerEvent;

                if (menu.isMenu) {
                    menu.showBy(this.element, this.getMenuAlign());
                } else if (menu.isViewportMenu) {
                    menu.setDisplayed(!menu.getDisplayed());
                } else {
                    menu.show();
                }
            }
        }
    }
});
