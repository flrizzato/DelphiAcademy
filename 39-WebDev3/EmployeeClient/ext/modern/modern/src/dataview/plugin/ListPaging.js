/**
 * Adds a Load More button at the bottom of the list. When the user presses this button,
 * the next page of data will be loaded into the store and appended to the List.
 *
 * By specifying `{@link #autoPaging}: true`, an 'infinite scroll' effect can be achieved,
 * i.e., the next page of content will load automatically when the user scrolls near the
 * bottom of the list.
 *
 * ## Example
 *
 *     Ext.create('Ext.dataview.List', {
 *
 *         store: Ext.create('TweetStore'),
 *
 *         plugins: {
 *             listpaging: {
 *                 autoPaging: true
 *             }
 *         },
 *
 *         itemTpl: [
 *             '<img src="{profile_image_url}" />',
 *             '<div class="tweet">{text}</div>'
 *         ]
 *     });
 */
Ext.define('Ext.dataview.plugin.ListPaging', {
    extend: 'Ext.plugin.Abstract',
    alias: 'plugin.listpaging',
    alternateClassName: 'Ext.plugin.ListPaging',

    config: {
        /**
         * @cfg {Boolean} autoPaging
         * True to automatically load the next page as soon as less than {@link #bufferZone}
         * items are available besides the ones currently visible.
         */
        autoPaging: false,

        /**
         * @cfg {Number} bufferZone
         * Amount of items, besides the ones currently visible, that need to be available until
         * the next page is loaded. If 0 (or null), the next page is loaded when the list is
         * scrolled to the bottom. This config only applies if {@link #autoPaging} is true.
         */
        bufferZone: 8,

        /**
         * @cfg {String} loadMoreText The text used as the label of the Load More button.
         *
         * @locale
         */
        loadMoreText: 'Load More...',

        /**
         * @cfg {String} noMoreRecordsText The text used as the label of the Load More button when the Store's
         * {@link Ext.data.Store#totalCount totalCount} indicates that all of the records available on the server are
         * already loaded
         *
         * @locale
         */
        noMoreRecordsText: 'No More Records',

        /**
         * @cfg {Object} loadMoreCmp
         * @private
         */
        loadMoreCmp: {
            xtype: 'component',
            cls: Ext.baseCSSPrefix + 'listpaging',
            scrollDock: 'end',
            hidden: true,
            inheritUi: true
        },

        /**
         * @private
         * @cfg {Boolean} loading True if the plugin has initiated a Store load that has not yet completed
         */
        loading: false
    },

    loadTpl:
        '<div class="'+Ext.baseCSSPrefix+'loading-spinner">' +
             '<span class="'+Ext.baseCSSPrefix+'loading-top"></span>' +
             '<span class="'+Ext.baseCSSPrefix+'loading-right"></span>' +
             '<span class="'+Ext.baseCSSPrefix+'loading-bottom"></span>' +
             '<span class="'+Ext.baseCSSPrefix+'loading-left"></span>' +
        '</div>' +
        '<div class="'+Ext.baseCSSPrefix+'message">{message}</div>',

    /**
     * @private
     * Sets up all of the references the plugin needs
     */
    init: function(list) {
        var me = this;

        list.on('storechange', 'onStoreChange', me);
        me.bindStore(list.getStore());
        me.addLoadMoreCmp();
    },

    destroy: function() {
        Ext.destroy(this._storeListeners);
        this.callParent();
    },

    updateAutoPaging: function(enabled) {
        var scroller = this.getCmp().getScrollable(),
            listeners = {
                scroll: 'onScroll',
                scope: this
            };

        if (enabled) {
            scroller.on(listeners);
            this.ensureBufferZone();
        } else {
            scroller.un(listeners);
        }
    },

    /**
     * @private
     */
    bindStore: function(store) {
        var me = this,
            listeners = {
                beforeload: 'onStoreBeforeLoad',
                load: 'onStoreLoad',
                filter: 'onFilter',
                destroyable: true,
                scope: me
            };

        me._storeListeners = Ext.destroy(me._storeListeners);
        if (store) {
            me._storeListeners = store.on(listeners);
        }
    },

    /**
     * @private
     * Removes the List/DataView's loading mask because we show our own in the plugin. The logic here disables the
     * loading mask immediately if the store is autoloading. If it's not autoloading, allow the mask to show the first
     * time the Store loads, then disable it and use the plugin's loading spinner.
     * @param {Ext.data.Store} store The store that is bound to the DataView
     */
    disableDataViewMask: function() {
        var list = this.cmp;
            this._listMask = list.getLoadingText();

        list.setLoadingText(null);
    },

    enableDataViewMask: function() {
        if (this._listMask) {
            var list = this.cmp;

            list.setLoadingText(this._listMask);
            delete this._listMask;
        }
    },

    /**
     * @private
     */
    applyLoadMoreCmp: function (config, instance) {
        return Ext.updateWidget(instance, config, this, 'createLoadMoreCmp');
    },

    createLoadMoreCmp: function (config) {
        return Ext.apply({
            html: this.getLoadTpl().apply({
                message: this.getLoadMoreText()
            })
        }, config);
    },

    updateLoadMoreCmp: function (loadMoreCmp, old) {
        Ext.destroy(old);

        if (loadMoreCmp) {
            loadMoreCmp.el.on({
                tap: 'loadNextPage',
                scope: this
            });
        }
    },

    /**
     * @private
     * If we're using autoPaging and detect that the user has scrolled to the bottom, kick off loading of the next page
     */
    onScroll: function() {
        this.ensureBufferZone();
    },

    /**
     * @private
     * Makes sure we add/remove the loading CSS class while the Store is loading
     */
    updateLoading: function(isLoading) {
        this.getLoadMoreCmp().toggleCls(this.loadingCls, isLoading);
    },

    /**
     * @private
     */
    onStoreChange: function(list, store) {
        this.bindStore(store);
    },

    /**
     * @private
     * If the Store is just about to load but it's currently empty, we hide the load more button because this is
     * usually an outcome of setting a new Store on the List so we don't want the load more button to flash while
     * the new Store loads
     */
    onStoreBeforeLoad: function(store) {
        if (store.getCount() === 0) {
            this.getLoadMoreCmp().hide();
        }
    },

    /**
     * @private
     */
    onStoreLoad: function () {
        this.syncState();
    },

    onFilter: function(store) {
        this.getLoadMoreCmp.setVisible(store.getCount() === 0);
    },

    /**
     * @private
     * Because the attached List's inner list element is rendered after our init function is called,
     * we need to dynamically add the loadMoreCmp later. This does this once and caches the result.
     */
    addLoadMoreCmp: function() {
        var me = this;

        if (!me.isAdded) {
            me.cmp.add(me.getLoadMoreCmp());
            me.isAdded = true;
            me.syncState();
        }
    },

    /**
     * @private
     * Returns true if the Store is detected as being fully loaded, or the server did not return a total count, which
     * means we're in 'infinite' mode
     * @return {Boolean}
     */
    storeFullyLoaded: function() {
        var store = this.cmp.getStore(),
            total = store? store.getTotalCount() : null;

        return total !== null ? total <= (store.currentPage * store.getPageSize()) : false;
    },

    /**
     * @private
     */
    loadNextPage: function() {
        var me = this,
            list = me.cmp;

        if (me.storeFullyLoaded()) {
            return;
        }

        me.setLoading(true);
        me.disableDataViewMask();
        me.currentScrollToTopOnRefresh = list.getScrollToTopOnRefresh();
        list.setScrollToTopOnRefresh(false);
        list.getStore().nextPage({ addRecords: true });
    },

    privates: {
        loadingCls: Ext.baseCSSPrefix + 'loading',

        ensureBufferZone: function() {
            var me = this,
                list = me.cmp;

            if (list.isPainted()) {
                me.ensureBufferZone = me.doEnsureBufferZone;
                me.doEnsureBufferZone();
                return;
            }

            if (!me.waitingForPainted) {
                me.waitingForPainted = true;
                list.on({
                    painted: {
                        single: true,
                        fn: function() {
                            delete me.waitingForPainted;
                            me.ensureBufferZone();
                        }
                    }
                });
            }
        },

        doEnsureBufferZone: function() {
            var me = this,
                list = me.cmp,
                store = list.getStore(),
                scroller = list.getScrollable(),
                count = store && store.getCount(),
                bufferZone = me.getBufferZone(),
                item, box, y, index;

            if (!store || !count || !scroller || me.getLoading()) {
                return;
            }

            index = Math.min(Math.max(0, count - bufferZone), count - 1);
            item = list.mapToItem(store.getAt(index));
            box = item && item.element.getBox();
            if (!box) {
                return;
            }

            // if bufferZone is 0, loading the next page should happen when reaching the end
            // of the list (the bottom of the last item), else, if bufferZone is greater than
            // 0, loading the next page should happen when the first row of pixels of the
            // leading buffer zone item appears in the view.
            y = bufferZone > 0 ? box.top + 1 : box.bottom;
            if (y > scroller.getElement().getBox().bottom) {
                return;
            }

            me.loadNextPage();
        },

        getLoadTpl: function() {
            return Ext.XTemplate.getTpl(this, 'loadTpl');
        },

        syncState: function () {
            var me = this,
                list = me.cmp,
                loadCmp = me.getLoadMoreCmp(),
                full = me.storeFullyLoaded(),
                store = list.store,
                message = full ? me.getNoMoreRecordsText() : me.getLoadMoreText();

            if (store && store.getCount()) {
                loadCmp.show();
            }

            me.setLoading(false);

            //if we've reached the end of the data set, switch to the noMoreRecordsText
            loadCmp.setHtml(me.getLoadTpl().apply({
                message: message
            }));
            loadCmp.setDisabled(full);

            if (me.currentScrollToTopOnRefresh !== undefined) {
                list.setScrollToTopOnRefresh(me.currentScrollToTopOnRefresh);
                delete me.currentScrollToTopOnRefresh;
            }

            me.enableDataViewMask();

            if (me.getAutoPaging()) {
                me.ensureBufferZone();
            }
        }
    }
});
