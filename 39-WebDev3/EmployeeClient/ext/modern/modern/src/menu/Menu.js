/**
 * A menu object. This is the container to which you may add {@link Ext.menu.Item menu items}.
 *
 * Menus may contain either {@link Ext.menu.Item menu items}, or general
 * {@link Ext.Component Components}. Menus may also contain docked items because it
 * extends {@link Ext.Panel}.
 *
 * By default, Menus are absolutely positioned, floated Components. By configuring a
 * Menu with `{@link #cfg-floated}: false`, a Menu may be used as a child of a 
 * {@link Ext.Container Container}.
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
 *
 * @since 6.5.0
 */
Ext.define('Ext.menu.Menu', {
    extend: 'Ext.Panel',
    xtype: 'menu',

    requires: [
        'Ext.menu.Item',
        'Ext.menu.Manager',
        'Ext.layout.VBox'
    ],

    /**
     * @property {Boolean} isMenu
     * `true` in this class to identify an object as an instantiated Menu, or subclass thereof.
     */
    isMenu: true,

    config: {
        /**
         * @cfg {String} align
         */
        align: 'tl-bl?', // TODO: Override in RTL

        /**
         * @cfg {Boolean} indented
         * By default menu items reserve space at their start for an icon.  Set indented
         * to `false` to remove this space.  This behavior can be overridden at the level
         * of an individual menu item using the item's {@link Ext.menu.Item#indented} config.
         * Items that are not {@link Ext.menu.Item Menu Items} can provide an `indented`
         * property in their initial config object to control their indentation behavior.
         *
         * When set to `false` no vertical `separator` will be shown.
         */
        indented: true,

        /**
         * @cfg {Boolean} [separator=false]
         * True to show a vertical icon separator line between the icons and the menu text.
         */
        separator: null,

        /**
         * @cfg {Boolean} [autoHide=true]
         * `false` to prevent the menu from auto-hiding when focus moves elsewhere
         */
        autoHide: null,

        /**
         * @cfg {Object} groups
         * This object is a dictionary of {@link Ext.menu.RadioItem#cfg!group radio group}
         * keys and {@link Ext.menu.RadioItem#cfg!value values}. This map is maintained by
         * the individual radio items in this menu but can also be useful for data binding.
         *
         * For example:
         *
         *      @example
         *      Ext.Viewport.add({
         *          xtype: 'container',
         *          items: [{
         *              xtype: 'button',
         *              bind: 'Call {menuGroups.option}',
         *               
         *              viewModel: {
         *                  data: {
         *                      menuGroups: {
         *                          option: 'home'
         *                      }
         *                  }
         *              },
         *               
         *              menu: {
         *                  bind: {
         *                      groups: '{menuGroups}'
         *                  },
         *                  items: [{
         *                      text: 'Home',
         *                      group: 'option',
         *                      value: 'home'
         *                  }, {
         *                      text: 'Work',
         *                      group: 'option',
         *                      value: 'work'
         *                  }, {
         *                      text: 'Mobile',
         *                      group: 'option',
         *                      value: 'mobile'
         *                  }]
         *              }
         *          }]
         *      });
         *
         * The presence of the `group` property in the configuration of the above
         * {@link Ext.menu.Menu menu} causes the menu to create a
         * {@link Ext.menu.RadioItem RadioItem} instances.
         */
        groups: null
    },

    /**
     * @cfg {Boolean} allowOtherMenus
     * True to allow multiple menus to be displayed at the same time.
     */
    allowOtherMenus: false,

    /**
     * @cfg {Boolean} ignoreParentClicks
     * True to ignore clicks on any item in this menu that is a parent item (displays a submenu)
     * so that the submenu is not dismissed when clicking the parent item.
     */
    ignoreParentClicks: false,

    /**
     * @cfg {Number} mouseLeaveDelay
     * The delay in ms as to how long the framework should wait before firing a mouseleave event.
     * This allows submenus not to be collapsed while hovering other menu items.
     */
    mouseLeaveDelay: 50,
    
    defaultType: 'menuitem',

    autoSize: null,

    keyMap: {
        scope: 'this',

        // Space key clicks
        SPACE: 'onSpaceKey',

        // ESC hides
        ESC: 'onEscKey'
    },

    layout: {
        type: 'vbox',
        align: 'stretch'
    },

    classCls: Ext.baseCSSPrefix + 'menu',
    indentedCls: Ext.baseCSSPrefix + 'indented',
    hasSeparatorCls: Ext.baseCSSPrefix + 'has-separator',
    nonMenuItemCls: Ext.baseCSSPrefix + 'non-menuitem',

    // We need to focus disabled menu items when navigating as per WAI-ARIA:
    // http://www.w3.org/TR/wai-aria-practices/#menu
    allowFocusingDisabledChildren: true,

    border: true,

    // When a Menu is used as a carrier to float some focusable Component such as a
    // DatePicker or ColorPicker. This will be used to delegate focus to its focusable
    // child. In normal usage, a Menu is a FocusableContainer, and this will not be
    // consulted.
    defaultFocus: ':focusable',

    floated: true,

    // May be asked to focus, will delegate down to its first focusable child
    focusable: true,

    focusableContainer: true,

    nameHolder: true,
    weighted: true,

    /**
     * @event groupchange
     * Fires when a child {@link Ext.menu.RadioItem radio item} in a menu
     * {@link Ext.menu.RadioItem#cfg!group group} changes {@link Ext.menu.RadioItem#cfg!checked}
     * state, and the group's value therefore changes.
     *
     * The value changes to the {@link Ext.menu.RadioItem#cfg!value} of the sole checked
     * member of the group, or `null` if all members have become
     * {@link Ext.menu.RadioItem#cfg!allowUncheck unchecked}.
     *
     * @param {Ext.menu.Menu} menu The menu firing this event.
     * @param {String} groupName The name of the group of items.
     * @param {Object} newValue The new value of the group.
     * @param {Object} oldValue The old value of the group.
     * @since 6.5.1
     */

    initialize: function() {
        var me = this,
            listeners = {
                click: me.onClick,
                mouseover: me.onMouseOver,
                scope: me
            };

        me.callParent();

        if (Ext.supports.Touch) {
            listeners.pointerdown = me.onMouseOver;
        }
        me.element.on(listeners);

        // Child item mouseovers are handled on a delay so that
        // rapid movement down a menu does not activate/deactivate during mouse motion.
        // Also, allow for rapid reentry when user moves mouse quickly.
        me.itemOverTask = new Ext.util.DelayedTask(me.handleItemOver, me);

        me.mouseMonitor = me.el.monitorMouseLeave(me.mouseLeaveDelay, me.onMouseLeave, me);
    },

    doDestroy: function() {
        var me = this;

        // Cancel any impending mouseover consequences
        me.itemOverTask.cancel();

        // Menu can be destroyed while shown;
        // we should notify the Manager
        Ext.menu.Manager.onHide(me);
        
        me.parentMenu = me.ownerCmp = null;

        if (me.rendered) {
            me.el.un(me.mouseMonitor);
        }
        
        me.callParent();
    },

    showBy: function(component, alignment, options) {
        this.callParent([component, alignment || this.getAlign(), options]);
    },

    onFocusEnter: function(e) {
        var me = this,
            hierarchyState;

        me.callParent([e]);

        me.mixins.focusablecontainer.onFocusEnter.call(me, e);
        if (me.getFloated()) {
            hierarchyState = me.getInherited();

            // The topmost focusEnter event upon entry into a floating menu stack
            // is recorded in the hierarchy state.
            //
            // Focusing upwards from descendant menus in a stack will NOT trigger onFocusEnter
            // on the parent menu because focus is already in its component tree.
            // For focusing downwards we check for presence of the topmostFocusEvent
            // already being present in the hierarchy.
            //
            // If we need to explicitly access a focus reversion point, we can use that.
            // This is only ever needed if tabbing forwards from the menu. We explicitly
            // push focus to the topmost focusEnter component, and then allow natural
            // tabbing to proceed from there.
            //
            // In all other focus reversion scenarios we use the immediate focusEnter event
            if (!hierarchyState.topmostFocusEvent) {
                hierarchyState.topmostFocusEvent = e;
            }
        }
    },

    onFocusLeave: function(e) {
        this.callParent([e]);
        
        if (this.getAutoHide() !== false) {
            this.hide();
        }
    },

    onItemAdd: function(item, index) {
        this.callParent([item, index]);

        this.syncItemIndentedCls(item);

        if (!item.isMenuItem && !item.isMenuSeparator) {
            item.addCls(this.nonMenuItemCls);
        }
    },

    onItemRemove: function(item, index, destroying) {
        this.callParent([item, index, destroying]);

        item.removeCls(this.indentedCls, this.nonMenuItemCls);
    },

    beforeShow: function() {
        var me = this,
            parent;

        // If this is the topmost in a stack of menus, hide "other" menus
        // if we are configured not to tolerate other menus being visible.
        if (me.getFloated()) {
            parent = me.hasFloatMenuParent();

            if (!parent && !me.allowOtherMenus) {
                Ext.menu.Manager.hideAll();
            }
        }

        me.callParent(arguments);
    },

    afterShow: function() {
        var me = this,
            ariaDom = me.ariaEl.dom;

        me.callParent(arguments);
        Ext.menu.Manager.onShow(me);

        if (me.getFloated() && ariaDom) {
            ariaDom.setAttribute('aria-expanded', true);
        }
        
        // Restore configured maxHeight
        if (me.getFloated()) {
            me.maxHeight = me.savedMaxHeight;
        }
        if (me.autoFocus) {
            me.focus();
        }
    },

    afterHide: function() {
        var me = this,
            ariaDom = me.ariaEl.dom;

        me.callParent();
        me.lastHide = Ext.Date.now();
        Ext.menu.Manager.onHide(me);

        if (me.getFloated() && ariaDom) {
            ariaDom.setAttribute('aria-expanded', false);
        }

        // Top level focusEnter is only valid when focused
        delete me.getInherited().topmostFocusEvent;
    },

    factoryItem: function(cfg) {
        var result;

        if (typeof cfg === 'string' && cfg[0] !== '@') {
            if (cfg === '-') {
                cfg = { xtype: 'menuseparator' };
            } else {
                cfg = {};
            }
        }

        result = this.callParent([cfg]);

        if (result.isMenuItem) {
            result.parentMenu = this;
        }

        return result;
    },

    updateIndented: function(indented) {
        var me = this;

        if (!me.isConfiguring) {
            me.bodyElement.toggleCls(me.hasSeparatorCls, !!(indented && me.getSeparator()));
            me.items.each(me.syncItemIndentedCls, me);
        }
    },

    updateSeparator: function(separator) {
        this.bodyElement.toggleCls(this.hasSeparatorCls, !!(separator && this.getIndented()));
    },

    privates: {
        applyItemDefaults: function (item) {
            item = this.callParent([item]);

            if (!item.isComponent && !item.xtype && !item.xclass) {
                // If configured with group or name, then it's a RadioItem
                if (item.group || item.name) {
                    item.xtype = 'menuradioitem';
                }
                // The presence of a checked config defaults the type to a CheckItem
                else if ('checked' in item) {
                    item.xtype = 'menucheckitem';
                }
            }

            return item;
        },

        applyGroups: function (groups, oldGroups) {
            var me = this,
                currentGroups = Ext.apply({}, oldGroups),
                isConfiguring = me.isConfiguring,
                groupName, members, len, i, item, value, oldValue;

            if (groups) {
                for (groupName in groups) {
                    oldValue = currentGroups[groupName];
                    currentGroups[groupName] = value = groups[groupName];

                    if (!isConfiguring) {
                        members = me.lookupName(groupName);
                        for (i = 0, len = members.length; i < len; i++) {
                            item = members[i];

                            // Set checked state depending on whether the value is the group's value
                            item.setChecked(item.getValue() === value);
                        }
                        me.fireEvent('groupchange', me, groupName, value, oldValue);
                    }
                }

                // Creates a bindable updater on first call after configuration is done.
                // We only want one if this menu *has* RadioItem groups.
                if (!isConfiguring) {
                    me.addBindableUpdater('groups');
                }
            }

            return currentGroups;
        },

        processFocusableContainerKeyEvent: function(e) {
            var keyCode = e.keyCode,
                item;

            // FocusableContainer ignores events from input fields.
            // In Menus we have a special case. The ESC key, or arrow from <input type="checkbox"> must be handled.
            if (keyCode === e.ESC || (Ext.fly(e.target).is('input[type=checkbox]') && (keyCode === e.LEFT || keyCode === e.RIGHT || keyCode === e.UP || keyCode === e.DOWN))) {
                e.preventDefault();
                // TODO: we should never modify the "target" property of an event
                item = this.getItemFromEvent(e);
                e.target = item && item.focusEl.dom;
            }
            // TAB from textual input fields is converted into UP or DOWN.
            else if (keyCode === e.TAB && Ext.fly(e.target).is('input[type=text],textarea')) {
                e.preventDefault();
                // TODO: we should never modify the "target" property of an event
                item = this.getItemFromEvent(e);
                e.target = item && item.focusEl.dom;
                if (e.shiftKey) {
                    e.shiftKey = false;
                    e.keyCode = e.UP;
                } else {
                    e.keyCode = e.DOWN;
                }
            } else {
                return this.callParent([e]);
            }

            return e;
        },

        onEscKey: function(e) {
            if (this.getFloated()) {
                this.hide();
            }
        },

        onSpaceKey: function(e) {
            var clickedItem = this.getItemFromEvent(e);

            if (clickedItem && clickedItem.isMenuItem) {
                clickedItem.onSpace(e);
            }
        },

        onFocusableContainerLeftKey: function(e) {
            // The default action is to scroll the nearest horizontally scrollable container
            e.preventDefault();

            // Focus reversion will focus the activating MenuItem
            if (this.parentMenu) {
                this.hide();
            }
        },

        onFocusableContainerRightKey: function(e) {
            var clickedItem = this.getItemFromEvent(e);

            // The default action is to scroll the nearest horizontally scrollable container
            e.preventDefault();

            if (clickedItem) {
                clickedItem.expandMenu(e);
            }
        },

        onClick: function(e) {
            var me = this,
                type = e.type,
                clickedItem,
                clickResult,
                isKeyEvent = type === 'keydown',
                isTouchEvent = e.pointerType === 'touch';

            if (me.getDisabled()) {
                return e.stopEvent();
            }

            clickedItem = me.getItemFromEvent(e);
            if (clickedItem && clickedItem.isMenuItem) {
                if (!clickedItem.getMenu() || !me.ignoreParentClicks) {
                    clickResult = clickedItem.onClick(e);
                }
                else {
                    e.stopEvent();
                }

                // Click handler on the item could have destroyed the menu
                if (me.destroyed) {
                    return;
                }

                // SPACE and ENTER invokes the menu
                if (clickedItem.getMenu() && clickResult !== false && (isKeyEvent || isTouchEvent)) {
                    clickedItem.expandMenu(e);
                }
            }
            // Click event may be fired without an item, so we need a second check
            if (!clickedItem || clickedItem.getDisabled()) {
                clickedItem = undefined;
            }

            me.fireEvent('click', me, clickedItem, e);
        },

        onMouseLeave: function(e) {
            var me = this;

            if (me.itemOverTask) {
                me.itemOverTask.cancel();
            }

            if (me.getDisabled()) {
                return;
            }

            me.fireEvent('mouseleave', me, e);
        },

        /**
         * Handle either pointer moving over the menu's element, or, on 
         * touch capable devices, a touch start on the menu's element.
         */
        onMouseOver: function(e) {
            var me = this,
                activeItem = me.getActiveItem(),
                activeItemMenu = activeItem && activeItem.getMenu && activeItem.getMenu(),
                activeItemExpanded = activeItemMenu && activeItemMenu.isVisible(),
                isTouch = e.pointerType === 'touch',
                mouseEnter, overItem, el;

            if (!me.getDisabled()) {
                
                // If triggered by a touchstart, mouseenter is declared
                // if focus does not already reside within the menu.
                if (isTouch) {
                    mouseEnter = !me.el.contains(document.activeElement);
                } else {
                    mouseEnter = !me.el.contains(e.getRelatedTarget());
                }
                overItem = me.getItemFromEvent(e);

                // Focus the item in time specified by mouseLeaveDelay.
                // If we mouseout, or move to another item this invocation will be canceled.
                if (overItem) {
                    // pointerdown is routed to mouseover, handle pointerdown without delay
                    if (isTouch) {
                        me.handleItemOver(e, overItem);
                    } else {
                        // ignore events on elements outside the bodyElement of menu items
                        // this ensures we don't apply mouseover styling when hovering the
                        // "separator" of a menu item, and we don't fire the menu item's
                        // handler when the separator is clicked.
                        el = overItem.isMenuItem ? overItem.bodyElement : overItem.el;
                        if (!el.contains(e.getRelatedTarget())) {
                            me.itemOverTask.delay(activeItemExpanded ? me.mouseLeaveDelay : 0, null, null, [e, overItem]);
                        }
                    }
                }
                if (mouseEnter) {
                    me.fireEvent('mouseenter', me, e);
                }
                me.fireEvent('mouseover', me, overItem, e);
            }
        },

        /**
         * Handle the delayed consequences of pointer over a child menu.
         * Also called on touch start.
         */
        handleItemOver: function(e, item) {
            var isMouseover = e.pointerType === 'mouse';

            // We'll get here on touchstart on touch devices.
            // Only focus non-MenuItems on real mouseover events.
            if (!item.containsFocus && (isMouseover || item.isMenuItem)) {
                item.focus();
            }
            // Only expand the menu on real mouseover events.
            if (item.expandMenu && isMouseover) {
                item.expandMenu(e);
            }
        },

        /**
         * Gets the immediate child component which the passed event took place within
         */
        getItemFromEvent: function(e) {
            var bodyDom = this.bodyElement.dom,
                toEl = e.getTarget(),
                component;

            // See which immediate child element the event is in and find the
            // component which that element encapsulates.
            while (toEl && toEl.parentNode !== bodyDom) {
                toEl = toEl.parentNode;
            }

            component = toEl && Ext.getCmp(toEl.id);

            if (component && component.isMenuItem && !e.within(component.bodyElement)) {
                // ignore events on elements outside the bodyElement of menu items
                // this ensures we don't apply mouseover styling when hovering the
                // "separator" of a menu item, and we don't fire the menu item's
                // handler when the separator is clicked.
                component = null;
            }

            return component;
        },

        hasFloatMenuParent: function() {
            return this.parentMenu || this.up('menu[_floated=true]');
        },

        syncItemIndentedCls: function(item) {
            // menu items have an "indented" config
            // non menu items can have an "indented" property
            // The item's "indented" takes precedence over the menu's "indented"
            var indented = item.isMenuItem ? item.getIndented() : item.indented;

            item.toggleCls(this.indentedCls,
                !!(indented || (this.getIndented() && (indented !== false))));
        }
    },

    statics: {
        /**
         * Returns a {@link Ext.menu.Menu} object
         * @param {Object/Object[]} menu An array of menu item configs,
         * or a Menu config that will be used to generate and return a new Menu.
         * @param {Object} [config] A configuration to use when creating the menu.
         * @return {Ext.menu.Menu}
         */
        create: function(menu, config) {
            if (Ext.isArray(menu)) { // array of menu items
                menu = Ext.apply({xtype: 'menu', items: menu}, config);
            } else {
                menu = Ext.apply({xtype: 'menu'}, menu, config);
            }
            return Ext.create(menu);
        }
    },

    deprecated: {
        '6.5': {
            configs: {
                plain: {
                    message: 'To achieve classic toolkit "plain" effect, use "indented".'
                },
                showSeparator: {
                    message: 'To achieve classic toolkit "showSeparator" effect, use "separator".'
                }
            }
        }
    }
    
});
