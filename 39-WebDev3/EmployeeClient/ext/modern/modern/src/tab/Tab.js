/**
 * Used in the {@link Ext.tab.Bar} component. This shouldn't be used directly, instead use
 * {@link Ext.tab.Bar} or {@link Ext.tab.Panel}.
 */
Ext.define('Ext.tab.Tab', {
    extend: 'Ext.Button',
    xtype: 'tab',
    alternateClassName: 'Ext.Tab',

    /**
     * @private
     */
    isTab: true,

    config: {
        /**
         * @cfg {Boolean}
         * Set this to `true` to have the tab be active by default.
         */
        active: null,

        /**
         * @cfg {String}
         * The title of the card that this tab is bound to.
         */
        title: null,

        /**
         * @cfg {Boolean} [closable=false]
         * True to make the Tab closable and display the close icon
         */
        closable: null
    },

    pressedDelay: true,

    classCls: Ext.baseCSSPrefix + 'tab',
    activeCls: Ext.baseCSSPrefix + 'active',
    closableCls: Ext.baseCSSPrefix + 'closable',

    getTemplate: function() {
        var template = this.callParent();

        template.push({
            reference: 'activeIndicatorElement',
            cls: Ext.baseCSSPrefix + 'active-indicator-el'
        }, {
            reference: 'closeIconElement',
            cls: Ext.baseCSSPrefix + 'close-icon-el ' + Ext.baseCSSPrefix + 'font-icon ' + Ext.baseCSSPrefix + 'no-ripple' ,
            onclick: 'return Ext.doEv(this, event);'
        });

        return template;
    },

    shouldRipple: function() {
        return this.getRipple();
    },

    /**
     * @event activate
     * Fires when a tab is activated
     * @param {Ext.tab.Tab} this
     */

    /**
     * @event deactivate
     * Fires when a tab is deactivated
     * @param {Ext.tab.Tab} this
     */

    onClick: function(e) {
        var me = this,
            tabBar = me.tabBar;

        if (e.currentTarget === me.closeIconElement.dom) {
            if (tabBar && !me.getDisabled()) {
                tabBar.closeTab(me);
            }

            e.stopPropagation();
        } else {
            return me.callParent([e]);
        }
    },

    updateTitle: function(title) {
        this.setText(title);
    },

    updateActive: function(active, oldActive) {
        var me = this,
            el = me.el,
            activeCls = me.activeCls;

        if (active && !oldActive) {
            el.addCls(activeCls);
            me.fireEvent('activate', me);
        } else if (oldActive) {
            el.removeCls(activeCls);
            me.fireEvent('deactivate', me);
        }
    },

    updateClosable: function(closable) {
        this.toggleCls(this.closableCls, !!closable);
    },

    onAdded: function (parent, instanced) {
        this.callParent([parent, instanced]);

        this.tabBar = parent.isTabBar ? parent : null;
    },

    onRemoved: function (destroying) {
        this.callParent([destroying]);

        this.tabBar = null;
    }
    
}, function() {
    this.override({
        activate: function() {
            this.setActive(true);
        },

        deactivate: function() {
            this.setActive(false);
        }
    });
});
