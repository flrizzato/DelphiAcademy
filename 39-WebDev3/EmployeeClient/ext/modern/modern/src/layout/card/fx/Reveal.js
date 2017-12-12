/**
 * @private
 */
Ext.define('Ext.layout.card.fx.Reveal', {
    extend: 'Ext.layout.card.fx.Style',

    alias: 'layout.card.fx.reveal',

    config: {
        inAnimation: {
            easing: 'ease-out',
            from: {
                opacity: 0.99
            },
            to: {
                opacity: 1
            }
        },
        outAnimation: {
            before: {
                'z-index': 100
            },
            after: {
                'z-index': 0
            },
            type: 'slide',
            easing: 'ease-out',
            out: true
        }
    }
});
