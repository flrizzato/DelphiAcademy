/**
 * A base class for all menu items that require menu-related functionality such as click handling,
 * sub-menus, icons, etc.
 *
 *     @example
 *     var mainPanel = Ext.create('Ext.Panel', {
 *         fullscreen: true,
 *
 *         items: {
 *             xtype: 'menu',
 *             floated: false,
 *             docked: 'left',
 *             items: [{
 *                 text: 'regular item 1'
 *             },{
 *                 text: 'regular item 2'
 *             },{
 *                 text: 'regular item 3'
 *             }]
 *         }
 *     });
 */
Ext.define('Ext.menu.Item', {
    extend: 'Ext.Component',
    alias: 'widget.menuitem',
    alternateClassName: 'Ext.menu.TextItem',

    /**
     * @property {Boolean} isMenuItem
     * `true` in this class to identify an object as an instantiated Menu Item, or subclass thereof.
     */
    isMenuItem: true,

    /**
     * @cfg {Number} menuExpandDelay
     * The delay in milliseconds before this item's sub-menu expands after this item is moused over.
     */
    menuExpandDelay: 200,

    /**
     * @cfg {Number} menuHideDelay
     * The delay in milliseconds before this item's sub-menu hides after this item is moused out.
     */
    menuHideDelay: 200,

    /**
     * @cfg {Object} scope
     * The scope (`this` refeence) in which the configured {@link #handler} will be executed,
     * unless the scope is a ViewController method nmame.
     * @accessor
     */
    scope: null,

    /**
     * @cfg {Boolean} destroyMenu
     * Whether or not to destroy any associated sub-menu when this item is destroyed.
     */
    destroyMenu: true,

    /**
    * @cfg {Number} clickHideDelay
    * The delay in milliseconds to wait before hiding the menu after clicking the menu item.
    * This only has an effect when `hideOnClick: true`.
    */
   clickHideDelay: 0,

    /**
     * @cfg {Boolean} [hideOnClick=true]
     * Whether to not to hide the owning menu when this item is clicked.
     */
    hideOnClick: true,

    config: {
        /**
         * @cfg {String} [href='#']
         * The href attribute to use for the underlying anchor link.
         */
        href: null,

        /**
         * @cfg {String} target
         * The target attribute to use for the underlying anchor link.
         */
        target: null,

        /**
         * @cfg {Function/String} handler
         * A function called when the menu item is clicked (can be used instead of {@link #click} event).
         * @cfg {Ext.menu.Item} handler.item The item that was clicked
         * @cfg {Ext.event.Event} handler.e The underlying {@link Ext.event.Event}.
         * @controllable
         */
        handler: null,

        /**
         * @cfg {String} [text]
         * The text to display in the menu item.
         */
        text: null,

        /**
         * @cfg {Ext.menu.Menu/Object} menu
         * Either an instance of {@link Ext.menu.Menu} or a config object for an {@link Ext.menu.Menu}
         * which will act as a sub-menu to this item.
         */
        menu: {
            lazy: true,
            $value: null
        },

        /**
         * @cfg {String} menuAlign
         * The default {@link Ext.util.Positionable#getAlignToXY Ext.util.Positionable.getAlignToXY} anchor position value for this
         * item's sub-menu relative to this item's position.
         */
        menuAlign: 'tl-tr?',

        /**
         * @cfg {String} [icon]
         * The url of an icon to display as the background image of the icon.
         */
        icon: null,

        /**
         * @cfg {String} [iconCls]
         * The CSS class to apply to the icon.
         */
        iconCls: null,

        /**
         * @cfg {'left'/'right'}
         * The position of the icon relative to the text
         */
        iconAlign: 'left',

        /**
         * @cfg {Boolean} [indented=true]
         * By default menu items reserve space at their start for an icon, depending on their
         * containing menu's {@link Ext.menu.Menu#indented} value.
         *
         * This option allows the indented behavior to be overridden for an individual menu item.
         */
        indented: null,

        /**
         * @cfg {Boolean} [separator=false]
         * If `true`, this item places an {@link Ext.menu.Separator} above itself unless it is the first visible item.
         */
        separator: null
    },

    inheritUi: true,

    ariaRole: 'menuitem',

    focusable: true,

    classCls: Ext.baseCSSPrefix + 'menuitem',

    activeCls: Ext.baseCSSPrefix + 'active',
    hasLeftIconCls: Ext.baseCSSPrefix + 'has-left-icon',
    hasRightIconCls: Ext.baseCSSPrefix + 'has-right-icon',
    hasArrowCls: Ext.baseCSSPrefix + 'has-arrow',
    hasHrefCls: Ext.baseCSSPrefix + 'has-href',

    isMenuOwner: true,

    template: [{
        reference: 'bodyElement',
        tag: 'a',
        href: '#',
        cls: Ext.baseCSSPrefix + 'body-el ' +
            Ext.baseCSSPrefix + 'unselectable',
        children: [{
            reference: 'leftIconWrapElement',
            cls: Ext.baseCSSPrefix + 'left-icon-wrap-el ' +
            Ext.baseCSSPrefix + 'icon-wrap-el',
            children: [{
                reference: 'leftIconElement',
                cls: Ext.baseCSSPrefix + 'left-icon-el ' +
                Ext.baseCSSPrefix + 'icon-el ' +
                Ext.baseCSSPrefix + 'font-icon'
            }]
        }, {
            html: '\u00a0',
            reference: 'textElement',
            cls: Ext.baseCSSPrefix + 'text-el'
        }, {
            reference: 'rightIconWrapElement',
            cls: Ext.baseCSSPrefix + 'right-icon-wrap-el ' +
            Ext.baseCSSPrefix + 'icon-wrap-el',
            children: [{
                reference: 'rightIconElement',
                cls: Ext.baseCSSPrefix + 'right-icon-el ' +
                Ext.baseCSSPrefix + 'icon-el ' +
                Ext.baseCSSPrefix + 'font-icon'
            }]
        }, {
            reference: 'arrowElement',
            cls: Ext.baseCSSPrefix + 'arrow-el ' +
            Ext.baseCSSPrefix + 'font-icon'
        }]
    }],

    ariaEl: 'bodyElement',

    focusEl: 'bodyElement',

    initialize: function () {
        this.callParent();
        this.syncHasIconCls();
    },

    getFocusClsEl: function() {
        return this.el;
    },

    /**
     * Expand this item's menu.
     * @param {Ext.event.Event} event Optional. Menus auto focus when invoked by key events.
     */
    expandMenu: function(event) {
        var me = this,
            menu = me.getMenu();

        // An item can be focused (active), but disabled.
        // Disabled items must not action on click (or left/right arrow)
        // http://www.w3.org/TR/2013/WD-wai-aria-practices-20130307/#menu
        // "Disabled menu items receive focus but have no action when Enter or Left Arrow/Right Arrow is pressed."
        if (!me.getDisabled() && menu) {

            // Needs an upward link
            menu.parentMenu = me.parentMenu;

            // hideOnClick makes no sense when there's a child menu
            me.hideOnClick = false;

            if (menu.isVisible()) {
                // Keyboard events should focus the first menu item even if it was already expanded
                if (event && event.type === 'keydown') {
                    menu.focus();
                }
            } else {
                // Pointer-invoked menus do not auto focus, key invoked ones do.
                menu.autoFocus = !event || !event.pointerType;
                menu.showBy(me, me.getMenuAlign(), {
                    axisLock: true  // Flips left/right when constrained
                                    // instead of covering the menu.
                });
            }
        }
    },

    getRefItems: function(deep) {
    // This is not a Container, so needs a special implementation to
    // return its subtree.
        var menu = this.getMenu(),
            items;

        if (menu) {
            items = menu.getRefItems(deep);
            items.unshift(menu);
        }
        return items || [];
    },

    onFocusEnter: function(e) {
        var me = this;

        me.callParent([e]);

        // We do not refuse activation if the Item is disabled.
        // http://www.w3.org/TR/2013/WD-wai-aria-practices-20130307/#menu
        // "Disabled menu items receive focus but have no action when Enter or Left Arrow/Right Arrow is pressed."
        me.addCls(me.activeCls);

        me.activated = true;
        if (me.hasListeners.activate) {
            me.fireEvent('activate', me);
        }

        if (me.parentMenu) {
            me.parentMenu.setActiveItem(me);
        }
    },

    onFocusLeave: function(e) {
        var me = this,
            // Do not call the menu into existence.
            // This property is set by updateMenu.
            menu = me.menu;

        me.callParent([e]);
        me.removeCls(me.activeCls);
        if (menu) {
            menu.hide();
        }
        me.activated = false;
        if (me.hasListeners.deactivate) {
            me.fireEvent('deactivate', me);
        }

        if (me.parentMenu) {
            me.parentMenu.setActiveItem(null);
        }
    },

    onRemoved: function(destroying) {
        this.callParent([destroying]);
        this.parentMenu = null;
    },

    doDestroy: function() {
        var me = this;

        me.separatorElement = Ext.destroy(me.separatorElement);
        me.setMenu(null);

        me.callParent();
    },

    updateText: function (text) {
        if (text == null || text === '') {
            text = '\u00a0';
        }
        this.textElement.dom.firstChild.data = text;
    },

    applyMenu: function (menu) {
        var me = this,
            ariaDom = me.ariaEl.dom;

        if (menu) {
            if (menu.isMenu) {
                menu.setConstrainAlign(Ext.getBody());
                menu.ownerCmp = me;
            } else {
                menu = Ext.menu.Menu.create(menu, {
                    ownerCmp: me,
                    $initParent: me,
                    constrainAlign: Ext.getBody()
                });
            }

            ariaDom.setAttribute('aria-haspopup', true);
            ariaDom.setAttribute('aria-owns', menu.id);
        } else {
            ariaDom.removeAttribute('aria-haspopup');
            ariaDom.removeAttribute('aria-owns');
        }

        me.toggleCls(me.hasArrowCls, !!menu);

        return menu;
    },

    updateMenu: function(menu, oldMenu) {
        if (oldMenu) {
            if (this.destroyMenu) {
                Ext.destroy(oldMenu);
            } else {
                oldMenu.parentMenu = null;
            }
        }
        // A property which will only exist when the Menu has been instantiated.
        this.menu = menu;
    },

    updateHref: function (href) {
        this.bodyElement.dom.href = href;
        this.toggleCls(this.hasHrefCls, !!href);
    },

    updateTarget: function (target) {
        this.bodyElement.dom.target = target;
    },

    updateIcon: function (icon) {
        var me = this,
            iconElement = (me.getIconAlign() === 'left') ? this.leftIconElement: this.rightIconElement;

        if (icon) {
            iconElement.setStyle('background-image', 'url(' + icon + ')');
        } else {
            iconElement.setStyle('background-image', '');
        }

        if (!me.isConfiguring) {
            me.syncHasIconCls();
        }
    },

    updateIconCls: function (iconCls, oldIconCls) {
        var me = this,
            iconElement = (me.getIconAlign() === 'left') ? this.leftIconElement: this.rightIconElement;

        if (iconCls) {
            iconElement.replaceCls(oldIconCls, iconCls);
        } else {
            iconElement.removeCls(oldIconCls);
        }

        if (!me.isConfiguring) {
            me.syncHasIconCls();
        }
    },

    updateIconAlign: function (iconAlign) {
        if (!this.isConfiguring) {
            this.syncHasIconCls();
        }
    },

    updateSeparator: function (separator) {
        var me = this,
            separatorElement = me.separatorElement;

        if (separator) {
            if (separatorElement) {
                separatorElement.show();
            }
            else {
                me.separatorElement = separatorElement = Ext.Element.create({
                    cls: Ext.baseCSSPrefix + 'menuseparator'
                });
                me.el.dom.insertBefore(separatorElement.dom, me.el.dom.firstChild);
            }
        }
        else if (separatorElement) {
            separatorElement.hide();
        }
    },

    privates: {
        onSpace: function(e) {
            return this.onClick(e);
        },

        onClick: function (e) {
            var me = this,
                href = me.getHref(),
                clickHideDelay = me.clickHideDelay,
                browserEvent = e.browserEvent,
                handler = me.getHandler(),
                clickResult;

            // Stop clicks on the anchor if there's no href, or we're disabled
            if ((!href || me.getDisabled()) && me.bodyElement.dom === e.getTarget('a')) {
                e.stopEvent();
                if (me.getDisabled()) {
                    return false;
                }
            }

            if (me.getDisabled() || me.handlingClick) {
                return;
            }

            if (me.hideOnClick && !me.getMenu()) {
                if (!clickHideDelay) {
                    me.hideParentMenus();
                } else {
                    me.hideParentMenusTimer = Ext.defer(me.hideParentMenus, clickHideDelay, me);
                }
            }

            // Click event may have destroyed the menu, don't do anything further
            clickResult = me.fireEvent('click', me, e);

            // Click listener could have destroyed the menu and/or item.
            if (me.destroyed) {
                return;
            }

            if (clickResult !== false && handler) {
                Ext.callback(handler, me.scope, [me, e], 0, me);
            }

            // And the handler could have done the same. We check this twice
            // because if the menu was destroyed in the click listener, the handler
            // should not have been called.
            if (me.destroyed) {
                return;
            }

            // We only manually need to trigger the click event if it's come from a key event and the event has not had preventDefault called.
            if (href && e.type !== 'click' && !browserEvent.defaultPrevented) {
                me.handlingClick = true;
                me.bodyElement.dom.click();
                me.handlingClick = false;
            }

            return clickResult;
        },

        /**
         * @private
         * Hides the entire floating menu tree that we are within.
         * Walks up the refOwner axis hiding each Menu instance it find until it hits
         * a non-floating ancestor.
         */
        hideParentMenus: function() {
            for (var menu = this.getRefOwner(); menu && ((menu.isMenu && menu.getFloated()) || menu.isMenuItem); menu = menu.getRefOwner()) {
                if (menu.isMenu) {
                    menu.hide();
                }
            }
        },

        hasIcon: function() {
            return !!(this.getIconCls() || this.getIcon());
        },

        syncHasIconCls: function() {
            var me = this,
                rightCls = me.hasRightIconCls,
                leftCls = me.hasLeftIconCls,
                iconAlign = me.getIconAlign();

            if (me.hasIcon()) {
                if (iconAlign === 'left') {
                    me.replaceCls(rightCls, leftCls);
                } else if (iconAlign === 'right') {
                    me.replaceCls(leftCls, rightCls);
                }
            } else {
                me.removeCls([leftCls, rightCls]);
            }
        }
    }
});
