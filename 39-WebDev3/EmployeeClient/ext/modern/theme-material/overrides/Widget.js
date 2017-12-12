Ext.define('Ext.theme.material.Widget', {
    override: 'Ext.Widget',

    statics: {
        floatInset: 16 / (window.devicePixelRatio || 1)
    }
});