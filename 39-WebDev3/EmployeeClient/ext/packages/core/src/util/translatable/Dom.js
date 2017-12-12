/**
 * @private
 */
Ext.define('Ext.util.translatable.Dom', {
    extend: 'Ext.util.translatable.Abstract',

    alias: 'translatable.dom', // also configures Factoryable

    config: {
        element: null
    },

    applyElement: function(element) {
        if (!element) {
            return;
        }

        return Ext.get(element);
    },

    updateElement: function() {
        this.refresh();
    }
});
