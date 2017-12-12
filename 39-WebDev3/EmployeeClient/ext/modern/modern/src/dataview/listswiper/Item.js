/**
 *
 */
Ext.define('Ext.dataview.listswiper.Item', {
    extend: 'Ext.Container',
    xtype: 'listswiperitem',

    classCls: Ext.baseCSSPrefix + 'listswiperitem',

    config: {
        leftActions: null,
        rightActions: null,


        /**
         * @cfg {Object} undo
         * A config object for the undo button.
         */
        undo: {
            lazy: true,
            $value: {
                xtype: 'button',
                text: 'Undo',
                touchAction: {
                    panX: false,
                    panY: false
                }
            }
        },

        /**
         * @private
         */
        action: null,

        /**
         * @private
         */
        state: null,

        /**
         * @cfg {Ext.dom.Element}
         * @private
         * Determines what element will be translated during the swipe
         */
        translationTarget: null
    },

    autoSize: null,

    initialize: function() {
        this.callParent();
        this.ownerCmp.on({
            scope: this,
            destroy: 'onItemDestroy',
            removed: 'onItemDestroy'
        });
    },

    applyUndo: function(config) {
        var action = this.getAction();
        return Ext.apply({}, action && action.undoable, config);
    },

    onItemDestroy: function(item) {
        var me = this,
            plugin = me.owner;
        plugin.destroyItem(item);
    },

    invokeAction: function(action, type) {
        var me = this,
            plugin = me.owner,
            list = plugin.cmp,
            item = me.ownerCmp,
            fn = action[type],
            obj = {
                item: item,
                record: item && item.getRecord(),
                action: action
            };

        list.fireEvent('itemaction' + type, list, obj);

        return Ext.callback(fn,
            action.getScope && action.getScope() || action.scope,
            [list, obj],
            0, me);
    },

    updateState: function(state, oldState) {
        if (oldState) {
            this.removeCls(Ext.baseCSSPrefix + oldState);
        }
        
        if (state) {
            this.addCls(Ext.baseCSSPrefix + state);
        }
    },

    onDragStart: Ext.emptyFn,
    onDragMove: Ext.emptyFn,
    onDragEnd: Ext.emptyFn
});
