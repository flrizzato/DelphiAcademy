Ext.define('Ext.theme.material.tab.Bar', {
    override: 'Ext.tab.Bar',
    config: {
        animateIndicator: true
    },

    platformConfig: {
        desktop: {
            layout: {
                pack: 'center'
            }
        }
    }
});
