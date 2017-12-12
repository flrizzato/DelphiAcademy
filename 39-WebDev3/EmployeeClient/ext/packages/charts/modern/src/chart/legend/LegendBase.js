/**
 * The legend base class adapater for modern toolkit.
 */
Ext.define('Ext.chart.legend.LegendBase', {
    extend: 'Ext.dataview.DataView',
    config: {
        itemTpl: [
            '<span class=\"', Ext.baseCSSPrefix, 'legend-item-marker {[ values.disabled ? Ext.baseCSSPrefix + \'legend-item-inactive\' : \'\' ]}\" style=\"background:{mark};\"></span>{name}'
        ],
        inline: true,

        scrollable: false // for IE11 vertical align
    },

    constructor: function (config) {
        this.callParent([config]);

        var scroller = this.getScrollable(),
            onDrag = scroller.onDrag;

        scroller.onDrag = function (e) {
            e.stopPropagation();
            onDrag.call(this, e);
        };
    },

    updateDocked: function (docked, oldDocked) {
        var me = this,
            el = me.el;

        me.callParent([docked, oldDocked]);

        switch (docked) {
            case 'top':
            case 'bottom':
                el.addCls(me.horizontalCls);
                el.removeCls(me.verticalCls);
                break;
            case 'left':
            case 'right':
                el.addCls(me.verticalCls);
                el.removeCls(me.horizontalCls);
                break;
        }
    },

    onChildTap: function (view, context) {
        this.callParent([view, context]);
        this.toggleItem(context.viewIndex);
    }
});
