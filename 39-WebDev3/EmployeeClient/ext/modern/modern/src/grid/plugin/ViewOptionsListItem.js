Ext.define('Ext.grid.plugin.ViewOptionsListItem', {
    extend: 'Ext.dataview.SimpleListItem',
    xtype: 'viewoptionslistitem',

    toolDefaults: {
        // Keep the same dimension even if tools are hidden
        hideMode: 'visibility'
    },

    tools: {
        handle: {
            zone: 'start',
            cls: Ext.baseCSSPrefix + 'no-ripple',
            iconCls: Ext.baseCSSPrefix + 'column-options-sortablehandle'
        },
        icon: {
            zone: 'start',
            // Determined dynamically if it's a group header
            iconCls: ''
        },
        group: {
            zone: 'end',
            iconCls: Ext.baseCSSPrefix + 'column-options-groupindicator',
            // Allow these to climb up to the plugin. This should eventually
            // be disabled when we have a better way of allowing scope
            // resolution to be able to bubble up to plugins
            stopEvent: false
        },
        hide: {
            zone: 'end',
            iconCls: Ext.baseCSSPrefix + 'column-options-visibleindicator',
            // Allow these to climb up to the plugin. This should eventually
            // be disabled when we have a better way of allowing scope
            // resolution to be able to bubble up to plugins
            stopEvent: false
        }
    },
    cls: Ext.baseCSSPrefix + 'column-options-item',

    updateRecord: function(record, oldRecord) {
        if (!record) {
            return;
        }

        var me = this,
            tool;

        me.callParent([record, oldRecord]);

        me.toggleCls(me.hiddenColumnCls, record.get('hidden'));
        me.toggleCls(me.groupedColumnCls, record.get('grouped'));

        tool = me.lookupTool('icon');
        tool.setIconCls(record.get('header') ? me.headerCls : me.leafCls);

        me.lookupTool('group').setHidden(!record.get('groupable'));
        me.lookupTool('hide').setHidden(!record.get('hideable'));
    },

    privates: {
        groupedColumnCls: Ext.baseCSSPrefix + 'column-options-grouped',
        headerCls: Ext.baseCSSPrefix + 'column-options-folder',
        hiddenColumnCls: Ext.baseCSSPrefix + 'column-options-hidden',
        leafCls: Ext.baseCSSPrefix + 'column-options-leaf'
    }
});