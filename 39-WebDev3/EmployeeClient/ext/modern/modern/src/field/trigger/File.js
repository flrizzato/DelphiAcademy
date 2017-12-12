Ext.define('Ext.field.trigger.File', {
    extend: 'Ext.field.trigger.Component',
    alias: 'trigger.file',
    classCls: Ext.baseCSSPrefix + 'filetrigger',

    component: {
        xtype: 'filebutton'
    },

    // private
    disableOnReadOnly: false
});
