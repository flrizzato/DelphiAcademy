/**
 * @private
 * @since 6.5.0
 */
Ext.define('Ext.dataview.Pinnable', {
    mixinId: 'dataviewpinnable',

    isDataViewPinnable: true,

    config: {
        /**
         * @private
         */
        pinned: null
    },

    pinnedCls: Ext.baseCSSPrefix + 'pinned',

    updatePinned: function (value) {
        var me = this,
            el = me.el,
            pinnedCls = me.pinnedCls,
            pinnedClsMap = me._pinnedClsMap,
            classes = el.getClassMap(/*clone=*/false);

        delete classes[pinnedClsMap.top];
        delete classes[pinnedClsMap.bottom];

        if (value) {
            classes[pinnedCls] = true;

            pinnedCls = pinnedClsMap[value];
            if (pinnedCls) {
                classes[pinnedCls] = true;
            }
        } else {
            delete classes[pinnedCls];
        }

        el.setClassMap(classes, /*keep=*/true);
    },

    _pinnedClsMap: {
        top: Ext.baseCSSPrefix + 'pinned-top',
        bottom: Ext.baseCSSPrefix + 'pinned-bottom'
    }
});
