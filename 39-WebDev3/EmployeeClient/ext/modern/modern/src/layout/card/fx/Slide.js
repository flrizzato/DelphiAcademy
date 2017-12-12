/**
 * @private
 */
Ext.define('Ext.layout.card.fx.Slide', {
    extend: 'Ext.layout.card.fx.Style',

    alias: 'layout.card.fx.slide',

    config: {
        inAnimation: {
            type: 'slide',
            easing: 'ease-out'
        },
        outAnimation: {
            type: 'slide',
            easing: 'ease-out',
            out: true
        }
    }
});
