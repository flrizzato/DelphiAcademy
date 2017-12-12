/**
 * @private
 */
Ext.define('Ext.layout.card.fx.Cube', {
    extend: 'Ext.layout.card.fx.Style',

    alias: 'layout.card.fx.cube',

    config: {
        reverse: null,
        inAnimation: {
            type: 'cube'
        },
        outAnimation: {
            type: 'cube',
            out: true
        }
    }
});
