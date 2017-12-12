Ext.define('Ext.theme.material.grid.cell.Check', {
    override: 'Ext.grid.cell.Check',

    config: {
        ripple: {
            delegate: '.' + Ext.baseCSSPrefix + 'checkbox-el',
            bound: false,
            color: 'default',
            centered: true
        }
    }
});