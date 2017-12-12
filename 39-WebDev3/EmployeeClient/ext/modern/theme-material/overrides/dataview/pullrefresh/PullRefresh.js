Ext.define('Ext.theme.material.dataview.pullrefresh.PullRefresh', {
    override: 'Ext.dataview.pullrefresh.PullRefresh',

    config: {
        overlay: true,
        widget: {
            xtype: 'pullrefreshspinner'
        }
    }
});
