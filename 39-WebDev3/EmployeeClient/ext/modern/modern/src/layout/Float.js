/**
 *
 */
Ext.define('Ext.layout.Float', {
    extend: 'Ext.layout.Auto',

    alias: 'layout.float',

    config: {
        direction: 'left'
    },

    cls: Ext.baseCSSPrefix + 'layout-float',

    itemCls: Ext.baseCSSPrefix + 'layout-float-item',

    updateDirection: function(direction, oldDirection) {
        var prefix = 'direction-';

        this.getContainer().getRenderTarget().swapCls(prefix + direction, prefix + oldDirection);
    }
});
