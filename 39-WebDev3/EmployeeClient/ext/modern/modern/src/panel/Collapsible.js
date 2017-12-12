/**
 * @class Ext.Panel
 */

Ext.define('Ext.panel.Collapsible', {
    override: 'Ext.Panel',

    config: {
        /**
         * @cfg {Boolean} collapsed
         * `true` to start collapsed.
         */
        collapsed: null,

        /**
         * @cfg {'top'/'right'/'bottom'/'left'/Boolean/Object} collapsible
         * A configuration for a {@link Ext.panel.Collapser Collapser}.
         *
         * True to make the panel collapsible and have an expand/collapse toggle Tool added into the header tool button
         * area.
         *
         * You can also set `top`/`right`/`bottom`/`left` to directly specify the collapse direction.
         *
         * @since 6.5.0
         */
        collapsible: null
    },

    /**
     * @event beforecollapse
     * Fires before collapse starts. Return `false` to cancel collapse.
     * @param {Ext.panel.Panel} this
     *
     * @since 6.5.0
     */

    /**
     * @event beforeexpand
     * Fires before expand starts. Return `false` to cancel expand.
     * @param {Ext.panel.Panel} this
     *
     * @since 6.5.0
     */

    /**
     * @event collapse
     * Fires when the collapse starts.
     * @param {Ext.panel.Panel} this
     *
     * @since 6.5.0
     */

    /**
     * @event drawerhide
     * Fires then the {@link Ext.panel.Collapser#cfg-drawer drawer} hides.
     *
     * @param {Ext.panel.Panel} this
     * @since 6.5.0
     */

    /**
     * @event drawershow
     * Fires then the {@link Ext.panel.Collapser#cfg-drawer drawer} shows.
     *
     * @param {Ext.panel.Panel} this
     * @since 6.5.0
     */

    /**
     * @event expand
     * Fires when the expand starts.
     * @param {Ext.panel.Panel} this
     *
     * @since 6.5.0
     */

    /**
     * @property {Boolean} hasCollapsible
     * `true` if this panel has the collapsible override added.
     *
     * @since 6.5.0
     */
    hasCollapsible: true,

    defaultCollapserCls: 'Ext.panel.Collapser',

    doDestroy: function() {
        this.setCollapsible(null);
        this.callParent();
    },

    /**
     * @method collapse
     * @inheritdoc Ext.panel.Collapser#method-collapse
     */
    collapse: function(animation) {
        return this.getCollapsible().collapse(animation);
    },

    /**
     * @method expand
     * @inheritdoc Ext.panel.Collapser#method-expand
     */
    expand: function(animation) {
        return this.getCollapsible().expand(animation);
    },

    /**
     * @method toggleCollapsed
     * @inheritdoc Ext.panel.Collapser#method-toggleCollapsed
     */
    toggleCollapsed: function(collapsed, animation) {
        return this.getCollapsible().toggleCollapsed(collapsed, animation);
    },

    getCollapsed: function() {
        // The collapsed state should always be governed by the collapsible
        var collapsible = this.getCollapsible();
        return collapsible ? collapsible.getCollapsed() : false;
    },

    updateCollapsed: function(collapsed) {
        var collapsible = this.getCollapsible();
        if (collapsible) {
            collapsible.setCollapsed(collapsed);
        }
    },

    applyCollapsible: function (collapsible, collapser) {
        if (collapsible === true) {
            collapsible = { direction: this.getHeaderPosition() };
        } else if (typeof collapsible === 'string') {
            collapsible = { direction: collapsible };
        } else if (!collapsible) {
            return null;
        }

        if (collapser) {
            collapser.setConfig(collapsible);
        } else {
            collapsible = Ext.apply({
                xclass: this.defaultCollapserCls,
                target: this
            }, collapsible);
            collapser = Ext.create(collapsible);
        }

        return collapser;
    },

    updateCollapsible: function(collapsible, oldCollapsible) {
        if (oldCollapsible) {
            // before making the panel uncollapsible let's expand it without animations
            if (!this.destroying) {
                oldCollapsible.doExpandCollapse(false);
            }
            oldCollapsible.destroy();
        }

        if (collapsible && this.rendered) {
            this.initCollapsible(collapsible);
        }
    },

    updateHeader: function(header, oldHeader) {
        var collapsible = this.getCollapsible();

        this.callParent([header, oldHeader]);

        if (this.isConfiguring && collapsible) {
            collapsible.toggleCollapsed(collapsible.getCollapsed(), false);
        }
    },

    updateHeaderPosition: function(headerPosition, oldHeaderPosition) {
        var collapsible = this.getCollapsible();

        if (collapsible && collapsible.getCollapsed()) {
            headerPosition = collapsible.getDirection();
        }
        this.moveHeaderPosition(headerPosition, oldHeaderPosition);
    },

    privates: {
        initCollapsible: function(collapsible) {
            this.ensureHeader();
            collapsible.initialize();
        },

        onCollapsibleRendered: function() {
            var collapsible = this.getCollapsible();
            if (collapsible) {
                this.initCollapsible(collapsible);
            }
        },

        reattachBodyWrap: function() {
            var me = this,
                header = me._header,
                el = me.maxHeightElement || me.element,
                bodyWrap = me.bodyWrapElement;

            if (bodyWrap.parent() !== el) {

                 // We need to make sure the collapser node
                 // is after the header in case the resizer
                 // node is present. The resizer node needs
                 // to be on top to function.
                if (header) {
                    bodyWrap.insertAfter(header.element);
                } else {
                    el.insertFirst(bodyWrap);
                }
            }
        }
    }
});
