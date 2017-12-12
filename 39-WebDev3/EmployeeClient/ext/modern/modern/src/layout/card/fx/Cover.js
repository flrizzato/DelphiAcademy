/**
 * @private
 */
Ext.define('Ext.layout.card.fx.Cover', {
    extend: 'Ext.layout.card.fx.Style',

    alias: 'layout.card.fx.cover',

    config: {
        reverse: null,

        inAnimation: {
            before: {
                'z-index': 100
            },
            after: {
                'z-index': 0
            },
            type: 'slide',
            easing: 'ease-out'
        },
        outAnimation: {
            easing: 'ease-out',
            from: {
                opacity: 0.99
            },
            to: {
                opacity: 1
            },
            out: true
        }
    }
});
