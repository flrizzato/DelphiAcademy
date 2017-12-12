/**
 * @private
 */
Ext.define('Ext.layout.card.fx.Pop', {
    extend: 'Ext.layout.card.fx.Serial',

    alias: 'layout.card.fx.pop',

    config: {
        inAnimation: {
            type: 'pop',
            easing: 'ease-out'
        },
        outAnimation: {
            type: 'pop',
            easing: 'ease-in',
            out: true
        }
    }
});
