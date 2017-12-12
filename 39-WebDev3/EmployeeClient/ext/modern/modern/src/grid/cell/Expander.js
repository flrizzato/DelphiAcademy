Ext.define('Ext.grid.cell.Expander', {
    extend: 'Ext.grid.cell.Base',
    xtype: 'expandercell',
    isExpanderCell: true,

    config: {
        collapsed: true
    },

    align: 'center',

    classCls: Ext.baseCSSPrefix + 'expandercell',

    expandedCls: Ext.baseCSSPrefix + 'expanded',

    innerTemplate: [{
        reference: 'iconElement',
        cls: Ext.baseCSSPrefix + 'icon-el ' +
             Ext.baseCSSPrefix + 'font-icon'
    }],

    updateCollapsed: function(collapsed) {
        this.element.toggleCls(this.expandedCls, !collapsed);
    }
});
