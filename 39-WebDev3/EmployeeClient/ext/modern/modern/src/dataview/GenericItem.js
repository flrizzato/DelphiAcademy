/**
 * This mixin is applied to {@link Ext.dataview.DataItem dataitem} and to
 * {@link Ext.dataview.SimpleListItem simplelistitem}.
 * @private
 * @since 6.5.0
 */
Ext.define('Ext.dataview.GenericItem', {
    mixinId: 'dataviewitem',

    isDataViewItem: true,

    config: {
        innerCls: null,
        contentCls: null,

        /**
         * @cfg {Number} recordIndex
         * The 0-based index of the record for this item in the {@link Ext.data.Store store}.
         * When using {@link Ext.data.virtual.Store virtual stores} it can happen that this
         * value is known when the {@link Ext.Component#cfg!record record} is not yet
         * known.
         * @since 6.5.0
         */
        recordIndex: null
    },

    updateRecordIndex: function (value) {
        this.el.dom.setAttribute('data-recordindex', value);
    },

    getDataview: function () {
        return this.parent; // backwards compat
    },

    updateInnerCls: function (cls, old) {
        this.innerElement.replaceCls(old, cls);
    },

    updateContentCls: function (cls, old) {
        this.getInnerHtmlElement().replaceCls(old, cls);
    },

    privates: {
        $dirty: false,

        dirtyCls: Ext.baseCSSPrefix + 'dirty',

        augmentToolHandler: function (tool, args) {
            // args = [ dataitem, tool, ev ]   ==>   [ dataitem, info ]
            var me = this;

            args[1] = {
                event: args.pop(),
                item: me,
                list: me.parent,
                record: me.getRecord(),
                tool: args[1]
            };
        },

        handleEmptyText: function(html) {
            var parent;
            
            if (!html) {
                parent = this.parent;

                if (parent && parent.getEmptyItemText) {
                    html = parent.getEmptyItemText();
                }
            }
        
            return html;
        },

        syncDirty: function(record) {
            var me = this,
                dirty = record.dirty;

            if (dirty !== me.$dirty) {
                me.toggleCls(me.dirtyCls, dirty);

                me.$dirty = dirty;
            }
        }
    }
});
