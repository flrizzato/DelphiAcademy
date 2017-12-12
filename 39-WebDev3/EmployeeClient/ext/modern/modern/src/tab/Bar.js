/**
 * Ext.tab.Bar is used internally by {@link Ext.tab.Panel} to create the bar of tabs that appears at the top of the tab
 * panel. It can also be used as a standalone component to recreate the look and feel of tabs.
 */
Ext.define('Ext.tab.Bar', {
    extend: 'Ext.Toolbar',
    alternateClassName: 'Ext.TabBar',
    xtype: 'tabbar',
    isTabBar: true,

    requires: [
        'Ext.layout.HBox',
        'Ext.tab.Tab'
    ],

    config: {
        /**
         * @cfg {String} defaultTabUI
         * A default {@link Ext.Component#ui ui} to use for {@link Ext.tab.Tab Tab} items.
         */
        defaultTabUI: null,

        /**
         * @cfg {Boolean} animateIndicator
         * Determines if the active indicator below the tab should animate or snap
         */
        animateIndicator: false
    },

    /**
     * @cfg defaultType
     * @inheritdoc
     */
    defaultType: 'tab',

    /**
     * @cfg layout
     * @inheritdoc
     */
    layout: {
        type: 'hbox',
        align: 'stretch'
    },

    eventedConfig: {
        /**
         * @cfg {Number/String/Ext.Component} activeTab
         * The initially activated tab. Can be specified as numeric index, itemId,
         * component ID, or as the component instance itself.
         * @accessor
         * @evented
         */
        activeTab: null
    },

    /**
     * @property baseCls
     * @inheritdoc
     */
    baseCls: Ext.baseCSSPrefix + 'tabbar',

    /**
     * Speed in which the Indicator will move per Tab in milliseconds
     */
    indicatorAnimationSpeed: 150,

    /**
     * @event tabchange
     * Fired when active tab changes.
     * @param {Ext.tab.Bar} this
     * @param {Ext.tab.Tab} newTab The new Tab
     * @param {Ext.tab.Tab} oldTab The old Tab
     */

    initialize: function() {
        var me = this;
        me.callParent();

        me.on({
            tap: 'onTabTap',

            delegate: '> tab',
            scope   : me
        });
    },

    getTemplate: function() {
        var template = this.callParent();
        template.push({
            reference: 'stripElement',
            cls: Ext.baseCSSPrefix + 'strip-el'
        });

        return template;
    },

    /**
     * @private
     */
    onTabTap: function(tab) {
        this.setActiveTab(tab);
    },

    /**
     * @private
     */
    applyActiveTab: function(newActiveTab, oldActiveTab) {
        if (!newActiveTab && newActiveTab !== 0) {
            return;
        }

        var newTabInstance = this.parseActiveTab(newActiveTab);

        if (!newTabInstance) {
            // <debug>
            if (oldActiveTab) {
                Ext.Logger.warn('Trying to set a non-existent activeTab');
            }
            // </debug>
            return;
        }
        return newTabInstance;
    },

    /**
     * @private
     * Default pack to center when docked to the bottom, otherwise default pack to left
     */
    updateDocked: function(newDocked) {
        var layout = this.getLayout(),
            initialConfig = this.getInitialConfig(),
            pack;

        if (!initialConfig.layout || !initialConfig.layout.pack) {
            pack = (newDocked == 'bottom') ? 'center' : 'left';
            //layout isn't guaranteed to be instantiated so must test
            if (layout.isLayout) {
                layout.setPack(pack);
            } else {
                layout.pack = (layout && layout.pack) ? layout.pack : pack;
            }
        }

		this.callParent(arguments);
    },

    /**
     * @private
     * Sets the active tab
     */
    updateActiveTab: function(newTab, oldTab) {
        var me = this,
            animateIndicator = this.getAnimateIndicator();

        if (animateIndicator && newTab && oldTab && oldTab.parent) {
            me.animateTabIndicator(newTab, oldTab);
        } else {

            if (newTab) {
                newTab.setActive(true);
            }

            //Check if the parent is present, if not it is destroyed
            if (oldTab && oldTab.parent) {
                oldTab.setActive(false);
                this.previousTab = oldTab;
            }

        }
    },

    updateAnimateIndicator: function() {
        var me = this;

        if (me.$animateIndicatorElement) {
            me.$animateIndicatorElement.destroy();
        }

        if (me.$indicatorAnimationListeners) {
            me.$indicatorAnimationListeners.destroy()
        }

        me.$indicatorAnimationListeners = me.$animateIndicatorElement = null;
    },

    animateTabIndicator: function(newTab, oldTab) {
        var me = this,
            newTabElement = newTab.element,
            oldTabElement = oldTab.element,
            oldIndicator = oldTab.activeIndicatorElement,
            newIndicator = newTab.activeIndicatorElement,
            tabbarElement = me.element,
            oldIndicatorProps, newIndicatorProps,
            animateIndicatorElement;

        newTab.setActive(true);
        oldIndicatorProps = {
            transform: {
                translateX: oldTabElement.getX() - tabbarElement.getX()
            },
            width: oldTabElement.getWidth(),
            height: oldIndicator.getHeight(),
            'background-color': oldIndicator.getStyle('background-color')
        };

        newIndicatorProps = {
            transform: {
                translateX: newTabElement.getX() - tabbarElement.getX()
            },
            width: newTabElement.getWidth(),
            height: newIndicator.getHeight(),
            'background-color': newIndicator.getStyle('background-color')
        };
        oldTab.setActive(false);
        newIndicator.hide();

        if (oldIndicatorProps.height || newIndicatorProps.height) {

            animateIndicatorElement = me.$animateIndicatorElement;
            if (!animateIndicatorElement) {
                animateIndicatorElement = me.$animateIndicatorElement = me.element.insertFirst({cls: Ext.baseCSSPrefix + 'active-indicator-el'});
            }

            animateIndicatorElement.show();
            if (me.$indicatorAnimationListeners) {
                me.$indicatorAnimationListeners.destroy();
                me.$indicatorAnimationListeners = null;
            }

            me.$indicatorAnimation = animateIndicatorElement.animate({
                duration: me.indicatorAnimationSpeed,
                from: oldIndicatorProps,
                to: newIndicatorProps
            });

            me.$indicatorAnimationListeners = me.$indicatorAnimation.on({
                destroyable: true,
                animationend: {
                    fn: function() {
                        newIndicator.show();
                        animateIndicatorElement.hide();
                        me.$indicatorAnimationListeners.destroy();
                        me.$indicatorAnimation = me.$indicatorAnimationListeners = null
                    },
                    single: true
                }
            });
        }
    },

    /**
     * @private
     * Parses the active tab, which can be a number or string
     */
    parseActiveTab: function(tab) {
        //we need to call getItems to initialize the items, otherwise they will not exist yet.
        if (typeof tab == 'number') {
			return this.getItems().items[tab];
        }
        else if (typeof tab == 'string') {
            tab = this.getComponent(tab) || Ext.getCmp(tab);
        }
        return tab;
    },

    onItemAdd: function(item, index) {
        var defaultTabUI = this.getDefaultTabUI();

        if (defaultTabUI && item.isTab && (item.getUi() == null)) {
            item.setUi(defaultTabUI);
        }

        this.callParent([item, index]);
    },

    privates: {
        /**
         * @private
         * Determines the next tab to activate when one tab is closed.
         * @param {Ext.tab.Tab} tabToClose
         */
        findNextActivatableTab: function (tabToClose) {
            var me = this,
                previousTab = me.previousTab,
                nextTab;

            if (tabToClose.getActive() && me.getItems().getCount() > 1) {
                if (previousTab && previousTab !== tabToClose && !previousTab.getDisabled()) {
                    nextTab = previousTab;
                }
                else {
                    nextTab = tabToClose.next('tab:not([disabled=true])') || tabToClose.prev('tab:not([disabled=true])');
                }
            }

            // If we couldn't find the next tab to activate, fall back
            // to the currently active one. We need to have a focused tab
            // at all times.
            return nextTab || me.getActiveTab();
        },

        /**
         * @private
         * @param {Ext.tab.Tab} tab
         */
        closeTab: function(tab) {
            var me = this,
                nextActivatableTab = me.findNextActivatableTab(tab),
                parent = me.parent;

            if (parent && parent.isTabPanel) {
                // setting the active card on a tab panel also sets the active tab in the tab bar
                if (nextActivatableTab) {
                    parent.setActiveItem(nextActivatableTab.card);
                }
                // removing card from tab panel also removes the tab from the tab bar
                parent.remove(tab.card);
            } else {
                if (nextActivatableTab) {
                    me.setActiveTab(nextActivatableTab);
                }
                me.remove(tab);
            }
        }
    }
});
