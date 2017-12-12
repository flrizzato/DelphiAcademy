/**
 * @private
 * Class used to display popup selection lists bound to fields.
 *
 * A BoundList is not focusable, has no `focusEl`, and has no `tabIndex` stamped into it.
 *
 * Its keyboard events are provided by its owning field, referenced by its `ownerCmp`, and
 * the `BoundListNavigationModel` uses the field as the key event source.
 */
Ext.define('Ext.dataview.BoundList', {
    extend: 'Ext.dataview.List',
    xtype: 'boundlist',
    requires: [
        'Ext.dataview.BoundListNavigationModel'
    ],

    tabIndex: null,
    focusEl: null,
    itemsFocusable: false,
    navigationModel: {
        type: 'boundlist'
    },
    itemConfig: {
        cls: Ext.baseCSSPrefix + 'boundlistitem',
        tools: {
            selected: {
                zone: 'start',
                passive: true,
                cls: Ext.baseCSSPrefix + 'selected-icon',
                iconCls: Ext.baseCSSPrefix + 'fa fa-check'
            }
        }
    },

    /**
     * @cfg {'tap'} triggerEvent
     * @hide
     * BoundLists always use tap. This is ignored.
     */
    onFocusEnter: Ext.emptyFn,
    onFocusLeave: Ext.emptyFn,

    privates: {
        /**
         * The selection model informs the view when it refreshes itself due to store
         * churn - for example reloading, filtering etc.
         *
         * The view must have the final say what records exit the selection because of
         * records inserted as a result of forceSelection: false.
         * @param {Ext.data.Model[]} toDeselect Records to be removed from the selection
         * @param {Ext.data.Model[]} toReselect Records to be added to the collection.
         */
        beforeSelectionRefresh: function (toDeselect, toReselect) {
            var len = toDeselect.length,
                i, rec;

            for (i = 0; i < len;) {
                rec = toDeselect[i];

                // If this is a record added as a result of forceSelection: false,
                // remove it from the eviction list.
                if (rec.isEntered) {
                    toDeselect.splice(i, 1);
                    len--;
                } else {
                    i++;
                }
            }
        }
    }
});