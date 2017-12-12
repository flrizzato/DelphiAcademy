Ext.define('Ext.theme.material.field.Toggle', {
    override: 'Ext.field.Toggle',
    config: {
        ripple: {
            delegate: '.' + Ext.baseCSSPrefix + 'thumb',
            bound: false,
            fit: false,
            color: 'default'
        }
    }
});