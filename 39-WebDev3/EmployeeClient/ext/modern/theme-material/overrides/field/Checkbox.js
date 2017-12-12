Ext.define('Ext.theme.material.field.Checkbox', {
    override: 'Ext.field.Checkbox',
    config: {
        labelAlign: 'left',
        bodyAlign: 'end',
        ripple: {
            delegate: '.' + Ext.baseCSSPrefix + 'icon-el',
            bound: false,
            color: 'default'
        }
    }
});