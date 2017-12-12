/**
 * @since 6.5.0
 */
Ext.define('Ext.dataview.pullrefresh.Bar', {
    extend: 'Ext.dataview.pullrefresh.Item',
    xtype: 'pullrefreshbar',

    baseCls: Ext.baseCSSPrefix + 'pullrefreshbar',

    cachedConfig: {
        /**
         * @cfg {String} lastUpdatedDateFormat
         * The format to be used on the last updated date.
         * @locale
         */
        lastUpdatedDateFormat: 'm/d/Y h:iA',

        /**
         * @cfg {String} lastUpdatedText
         * The text to be shown in front of the last updated time.
         * @locale
         */
        lastUpdatedText: 'Last Updated:\xA0',

        /**
         * @cfg {String} loadedText
         * The text that will be when data has been loaded.
         * @locale
         */
        loadedText: 'Loaded.',

        /**
         * @cfg {String} loadingText
         * The text that will be shown while the list is refreshing.
         * @locale
         */
        loadingText: 'Loading...',

        /**
         * @cfg {'message'/'spinner'} mode
         * Set to `'message'` to display information messages or `'spinner'` to display
         * a progress spinner.
         */
        mode: 'message',

        /**
         * @cfg {String} pullText
         * The text that will be shown while you are pulling down.
         * @locale
         */
        pullText: 'Pull down to refresh...',

        /**
         * @cfg {String} releaseText
         * The text that will be shown after you have pulled down enough to show the release message.
         * @locale
         */
        releaseText: 'Release to refresh...'
    },

    hidden: true,
    showInEmptyState: null,
    scrollDock: 'start',

    template: [{
        cls: Ext.baseCSSPrefix + 'font-icon ' + Ext.baseCSSPrefix + 'pullrefreshbar-arrow'
    }, {
        cls: Ext.baseCSSPrefix + 'pullrefreshbar-loading-wrap',

        children: [{
            cls: Ext.baseCSSPrefix + 'pullrefreshbar-loading ' +
                 Ext.baseCSSPrefix + 'loading-spinner',

            children: [{
                tag: 'span',
                cls: Ext.baseCSSPrefix + 'loading-top'
            }, {
                tag: 'span',
                cls: Ext.baseCSSPrefix + 'loading-right'
            }, {
                tag: 'span',
                cls: Ext.baseCSSPrefix + 'loading-bottom'
            }, {
                tag: 'span',
                cls: Ext.baseCSSPrefix + 'loading-left'
            }]
        }]
    }, {
        cls: Ext.baseCSSPrefix + 'pullrefreshbar-info-wrap',

        children: [{
            reference: 'infoMessageEl',
            cls: Ext.baseCSSPrefix + 'pullrefreshbar-info-message'
        }, {
            reference: 'infoUpdatedEl',
            cls: Ext.baseCSSPrefix + 'pullrefreshbar-info-updated'
        }]
    }],

    privates: {
        modeCls: Ext.baseCSSPrefix + 'pullrefreshbar-mode',

        textMap: {
            loaded: 'getLoadedText',
            loading: 'getLoadingText',
            pulling: 'getPullText',
            holding: 'getReleaseText'
        },

        updateLastUpdated: function(value) {
            var me = this,
                lastUpdated = value ? me.getLastUpdatedText() +
                    Ext.util.Format.date(value, me.getLastUpdatedDateFormat()) : value;

            me.infoUpdatedEl.setText(lastUpdated);
        },

        updateMode: function (value, oldValue) {
            this.el.replaceCls(oldValue, value, this.modeCls);
        },

        updateState: function (state) {
            var me = this,
                fn = me.textMap[state],
                text = fn && me[fn]();

            me.infoMessageEl.setText(text || '');

            this.callParent(arguments);
        }
    }
});
