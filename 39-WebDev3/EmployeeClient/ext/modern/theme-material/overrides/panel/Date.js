Ext.define('Ext.theme.material.panel.Date', {
    override: 'Ext.panel.Date',

    config: {
        headerFormat: 'D, M j',
        hideCaptions: false,
        hideOutside: true,
        navigationPosition: 'caption',
        selectOnNavigate: false,
        showTodayButton: false,
        splitTitle: true,
        titleAnimation: false,
        tools: {
            previousYear: null,
            nextYear: null
        }
    }
});
