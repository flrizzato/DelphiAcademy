/**
 * This mixin is applied to {@link Ext.dataview.DataItem dataitem} and to
 * {@link Ext.dataview.SimpleListItem simplelistitem}.
 * @private
 * @since 6.5.0
 */
Ext.define('Ext.dataview.Disclosable', {
    mixinId: 'disclosable',

    isListItem: true,

    toolDefaults: {
        ui: 'listitem'
    },

    toolAnchorName: 'innerElement',

    getDisclosure: function () {
        return this.lookupTool('disclosure');
    },

    privates: {
        invokeDisclosure: function (tool, handler, e) {
            var parent = this.parent;

            if (tool.type === 'disclosure' && !handler) {

                if (parent && parent.onItemDisclosureTap) {
                    parent.onItemDisclosureTap(this, e);
                    return true;
                }
            }
        },

        syncDisclosure: function (record) {
            var me = this,
                disclosure = me.getDisclosure(),
                parent = me.parent;

            if (disclosure) {
                disclosure.setHidden(parent.shouldHideDisclosure(record));
            }
        }
    }
});
