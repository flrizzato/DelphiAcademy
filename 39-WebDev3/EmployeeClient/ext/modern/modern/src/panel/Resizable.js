/**
 * @class Ext.Panel
 */

Ext.define('Ext.panel.Resizable', {
    override: 'Ext.Panel',

    config: {
        /**
         * @cfg {Object} [resizable]
         * A configuration for a {@link Ext.panel.Resizer Resizer}.
         *
         * @since 6.5.0
         */
        resizable: null
    },

    /**
     * @event beforeresizedragstart
     * Fires before resize starts. Return `false` to cancel dragging.
     * @param {Ext.Panel} this
     * @param {Object} context
     * @param {String} context.edge The edge being resized.
     * @param {Ext.event.Event} context.event The event.
     *
     * @since 6.5.0
     */
    
    /**
     * @event resizedragstart
     * Fires when the resize starts.
     * @param {Ext.Panel} this
     * @param {Object} context
     * @param {String} context.edge The edge being resized.
     * @param {Ext.event.Event} context.event The event.
     *
     * @since 6.5.0
     */
    
    /**
     * @event resizedrag
     * Fires for each tick while a drag is active.
     * @param {Ext.Panel} this
     * @param {Object} context
     * @param {String} context.edge The edge being resized.
     * @param {Ext.event.Event} context.event The event.
     * @param {Number} context.width The current resized width in pixels.
     * @param {Number} context.height The current resized height in pixels.
     *
     * @since 6.5.0
     */
    
    /**
     * @event resizedragend
     * Fires when the drag resize is complete.
     * @param {Ext.Panel} this
     * @param {Object} context
     * @param {String} context.edge The edge being resized.
     * @param {Ext.event.Event} context.event The event.
     * @param {Number} context.width The final resized width in pixels.
     * @param {Number} context.height The final resized height in pixels.
     *
     * @since 6.5.0
     */
    
    /**
     * @event resizedragcancel
     * Fires when the drag resize is cancelled.
     * @param {Ext.Panel} this
     * @param {Object} context
     * @param {String} context.edge The edge being resized.
     * @param {Ext.event.Event} context.event The event.
     *
     * @since 6.5.0
     */

     /**
      * @property {Boolean} hasResizable
      * `true` if this panel has the resizable override added.
      *
      * @since 6.5.0
      */
    hasResizable: true,

    defaultResizerCls: 'Ext.panel.Resizer',

    applyResizable: function(resizable) {
        if (resizable) {
            if (resizable === true) {
                resizable = {};
            }
            resizable = Ext.create(Ext.apply({
                xclass: this.defaultResizerCls,
                target: this,
                ui: this.getUi()
            }, resizable));
        }
        return resizable;
    },

    updateResizable: function(resizable, oldResizable) {
        if (oldResizable) {
            oldResizable.destroy();
        }
    },

    doDestroy: function() {
        this.setResizable(null);
        this.callParent();
    },

    privates: {
        onResizableUiChange: function(ui, oldUi) {
            var resizable = this.getResizable();
            if (resizable) {
                resizable.setUi(ui);
            }
        }
    }
});