/**
 * @private
 */
Ext.define('Ext.carousel.Item', {
    extend: 'Ext.Decorator',

    config: {
        component: null,
        translatable: true
    },

    baseCls: Ext.baseCSSPrefix + 'carousel-item'
});
