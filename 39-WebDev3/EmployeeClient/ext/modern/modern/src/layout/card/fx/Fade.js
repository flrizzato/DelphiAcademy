/**
 * @private
 */
Ext.define('Ext.layout.card.fx.Fade', {
    extend: 'Ext.layout.card.fx.Serial',

    alias: 'layout.card.fx.fade',

    config: {
        reverse: null,

        inAnimation: {
            type: 'fade',
            easing: 'ease-out'
        },
        outAnimation: {
            type: 'fade',
            easing: 'ease-out',
            out: true
        }
    }
});
