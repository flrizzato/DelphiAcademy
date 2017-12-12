Ext.define('Ext.theme.material.field.Date', {
    override: 'Ext.field.Date',
    config: {
        floatedPicker: {
            selectOnNavigate: true,
            header: {
                hidden: true
            }
        }
    }
});