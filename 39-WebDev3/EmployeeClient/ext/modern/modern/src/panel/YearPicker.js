/**
 * A year picker for the {@link Ext.panel.Date DatePanel}. This component cannot
 * be used standalone, only in conjunction with the {@link Ext.panel.Date yearPicker} config.
 *
 * @since 6.5.1
 */
Ext.define('Ext.panel.YearPicker', {
    extend: 'Ext.dataview.BoundList',
    xtype: 'yearpicker',

    classCls: Ext.baseCSSPrefix + 'yearpicker',

    config: {
        defaultOffset: 100,
        end: undefined,
        start: undefined
    },

    itemConfig: {
        ui: 'yearpicker',
        tools: null
    },
    itemTpl: '{year}',
    pinFooters: false,
    pinHeaders: false,
    scrollToTopOnRefresh: false,

    initialize: function() {
        this.callParent();
        this.rebuildStore();
    },

    focusYear: function(year) {
        var me = this,
            rec = me.getStore().getById(year),
            item = me.mapToItem(rec),
            scrollable = me.getScrollable(),
            y = scrollable.getEnsureVisibleXY(item.element, {
                align: {
                    y: 'center?'
                }
            }).y;

        scrollable.scrollTo(null, y, false);
        me.getNavigationModel().setLocation(rec);
        me.select(rec);
    },

    onChildTap: function(location) {
        var rec = location.record;
        if (rec) {
            this.fireEvent('yeartap', this, rec.id);
        }
    },

    applyEnd: function(end) {
        if (!end) {
            end = (new Date()).getFullYear() + this.getDefaultOffset();
        }
        return end;
    },

    updateEnd: function() {
        this.rebuildStore();
    },

    applyStart: function(start) {
        if (!start) {
            start = (new Date()).getFullYear() - this.getDefaultOffset();
        }
        return start;
    },

    updateStart: function() {
        this.rebuildStore();
    },

    privates: {
        forceRefreshOnRender: true,

        rebuildStore: function() {
            var me = this,
                start = me.getStart(),
                end = me.getEnd(),
                store = me.getStore(),
                data, i;

            if (me.isConfiguring) {
                return;
            }

            if (!store) {
                store = new Ext.data.Store({
                    autoDestroy: true
                });
                me.setStore(store);
            }

            data = [];
            for (i = start; i <= end; ++i) {
                data.push({
                    id: i,
                    year: i
                });
            }

            store.loadData(data);
        }
    }
});