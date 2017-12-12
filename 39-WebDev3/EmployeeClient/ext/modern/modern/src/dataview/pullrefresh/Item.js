/**
 * This component is the base class for {@link Ext.dataview.pullrefresh.Bar pullrefreshbar}
 * and {@link Ext.dataview.pullrefresh.Spinner pullrefreshspinner}.
 * @since 6.5.0
 * @private
 */
Ext.define('Ext.dataview.pullrefresh.Item', {
    extend: 'Ext.Component',

    config: {
        /**
         * @private
         */
        lastUpdated: null,

        /**
         * @cfg {'loading'/'loaded'/'pulling'/'holding'} state
         * @private
         */
        state: null,

        /**
         * @cfg {Number} pull
         * The value from 0 (not pulled) to 1 (fully pulled) and potentially more for
         * over-pulling or over-scrolling.
         * @private
         */
        pull: null
    },

    privates: {
        clsMap: {
            loaded: Ext.baseCSSPrefix + 'pullrefresh-loaded',
            loading: Ext.baseCSSPrefix + 'pullrefresh-loading',
            pulling: Ext.baseCSSPrefix + 'pullrefresh-pulling',
            holding: Ext.baseCSSPrefix + 'pullrefresh-holding'
        },

        isLoading: function (state) {
            state = state || this.getState();

            return state === 'loading' || state === 'loaded';
        },

        updateState: function (value) {
            var el = this.el,
                map = this.clsMap,
                classes;

            if (el) {
                classes = el.getClassMap(/*clone=*/false);

                delete classes[map.loaded];
                delete classes[map.loading];
                delete classes[map.pulling];
                delete classes[map.holding];

                classes[map[value]] = 1;

                el.setClassMap(classes, /*keep=*/true);

            }
        }
    }
});
