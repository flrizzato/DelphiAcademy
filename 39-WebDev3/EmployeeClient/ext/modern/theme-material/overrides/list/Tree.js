Ext.define('Ext.theme.material.list.Tree', {
    override: 'Ext.list.Tree',

    config: {
        itemRipple: {
            release: true,
            delegate: '.' + Ext.baseCSSPrefix + 'treelist-row',
            color: 'default'
        }
    }
});