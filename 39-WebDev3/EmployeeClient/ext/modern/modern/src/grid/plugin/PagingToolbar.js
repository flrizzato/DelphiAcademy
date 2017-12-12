/**
 * The Paging Toolbar is a specialized toolbar that is
 * bound to a `Ext.data.Store` and provides automatic paging control.
 *
 *     @example
 *     var store = Ext.create('Ext.data.Store', {
 *         fields: ['fname', 'lname', 'talent'],
 *         pageSize: 3,
 *         data: [
 *             { 'fname': 'Barry',  'lname': 'Allen',      'talent': 'Speedster' },
 *             { 'fname': 'Oliver', 'lname': 'Queen',      'talent': 'Archery'  },
 *             { 'fname': 'Kara',   'lname': 'Zor-El',     'talent': 'All'  },
 *             { 'fname': 'Helena', 'lname': 'Bertinelli', 'talent': 'Weapons Expert'  },
 *             { 'fname': 'Hal',    'lname': 'Jordan',     'talent': 'Willpower'  },
 *         ]
 *     });
 *
 *     Ext.create('Ext.grid.Grid', {
 *         title: 'DC Personnel',
 *
 *         store: store,
 *         plugins: {
 *             pagingtoolbar: true
 *         },
 *
 *         columns: [
 *             { text: 'First Name', dataIndex: 'fname',  flex: 1 },
 *             { text: 'Last Name',  dataIndex: 'lname',  flex: 1 },
 *             { text: 'Talent',     dataIndex: 'talent', flex: 1 }
 *         ],
 *
 *         height: 230,
 *         layout: 'fit',
 *         fullscreen: true
 *     });
 */
Ext.define('Ext.grid.plugin.PagingToolbar', {
    extend: 'Ext.plugin.Abstract',
    alias: ['plugin.pagingtoolbar', 'plugin.gridpagingtoolbar'],
    mixins: ['Ext.mixin.Hookable'],

    requires: [
        'Ext.grid.PagingToolbar',
        'Ext.util.DelayedTask'
    ],

    config: {
        /**
         * @cfg grid
         * @private
         */
        grid: null,
        currentPage: 1,

        /**
         * @cfg pageSize
         * @inheritdoc Ext.data.AbstractStore#cfg!pageSize
         */
        pageSize: 0,

        totalCount: 0,
        totalPages: 0,
        loadPages: null,

        /**
         * @cfg {Number|'dragend'} buffer
         * If a number, this is the number of milliseconds to delay after dragging stops
         * but the drag has not ended. If it is 'dragend', fetches from the remote server
         * will be suspended until dragging is completed.
         */
        buffer: 250,

        toolbar: {
            xtype: 'pagingtoolbar',
            docked: 'bottom'
        }
    },

    init: function(grid) {
        this.setGrid(grid);
        grid.add(this.getToolbar());
    },

    destroy: function(){
        this.setBuffer(null);
        this.setGrid(null);
        this.callParent();
    },

    updateGrid: function(grid, oldGrid) {
        var me = this;

        me.gridListeners = me.storeListeners = me.scrollListeners = Ext.destroy(me.gridListeners, me.storeListeners, me.scrollListeners);

        if (grid) {
            me.gridListeners = grid.on({
                updatevisiblecount: 'onUpdateVisibleCount',
                storechange: 'onStoreChanged',
                destroyable: true,
                scope: me
            });
            me.scrollListeners = grid.getScrollable().on({
                scrollend: 'checkPageChange',
                buffer: 100,
                scope: me
            });

            me.bindStore(grid.getStore());
        }
    },

    bindStore: function(store){
        var me = this;

        Ext.destroy(me.storeListeners);
        me.getToolbar().setDisabled(!!store);

        if(!store){
            return;
        }

        me.storeListeners = store.on({
            add: 'onTotalCountChange',
            remove: 'onTotalCountChange',
            refresh: 'onTotalCountChange',
            clear: 'onTotalCountChange',
            destroyable: true,
            scope: me
        });

        /* we have two scenarios:
         1. pageSize = 0, which means that we have the entire data in the store
         and we just need to show current page in the toolbar

         2. we have pageSize > 0 which means that we probably don't have the
         entire data in the store and we need to load it page by page
         */
        me.setLoadPages(store.pageSize > 0);

        me.cancelBufferTask();

        if(store.isLoaded()){
            me.onTotalCountChange(store);
        }
    },

    onStoreChanged: function(grid, store){
        this.bindStore(store);
    },

    /**
     * @private
     */
    getPageData: function() {
        var grid = this.getGrid(),
            store = grid.getStore(),
            totalCount = store.getTotalCount() || store.getCount(),
            pageSize = this.getLoadPages() ? store.pageSize : grid.visibleCount,
            pageCount = Math.ceil(totalCount / pageSize);

        return {
            totalCount : totalCount,
            totalPages: Ext.Number.isFinite(pageCount) ? pageCount : 1,
            currentPage : store.currentPage,
            pageSize: pageSize
        };
    },

    checkPageChange: function() {
        var me = this,
            grid = me.getGrid(),
            pageSize = me.getPageSize(),
            currentPage = me.getCurrentPage(),
            topVisibleIndex = grid.topVisibleIndex,
            newPage = Math.ceil( (topVisibleIndex + pageSize) / pageSize); // on the first page topVisibleIndex is 0

        if (grid.getStore() && !me.getLoadPages() && newPage > 0 && newPage !== currentPage) {
            me.preventGridScroll = true;
            me.setCurrentPage(newPage);
            me.preventGridScroll = false;
        }
    },

    updateBuffer: function(buffer) {
        var me = this,
            bufferTask = me.bufferTask;

        if (Ext.isNumber(buffer)) {
            me.bufferTask = bufferTask || new Ext.util.DelayedTask(me.bufferTaskRun, me)
            me.cancelBufferTask();
        }
        else if (bufferTask) {
            bufferTask.cancel();
            me.bufferTask = null;
        }
    },

    cancelBufferTask: function() {
        if (this.bufferTask) {
            this.bufferTask.cancel();
        }
    },

    loadCurrentPage: function() {
        this.getGrid().getStore().loadPage(this.getCurrentPage());
    },

    bufferTaskRun: function() {
        this.loadCurrentPage();
    },

    applyToolbar: function(toolbar, oldToolbar) {
        return Ext.factory(toolbar, Ext.Toolbar, oldToolbar);
    },

    updateToolbar: function(toolbar) {
        var me = this;

        if (toolbar) {
            toolbar.getSliderField().on({
                change: 'onPageChange',
                dragstart: 'onPageSliderDrag',
                drag: 'onPageSliderDrag',
                dragend: 'onPageSliderDragEnd',
                scope: me
            });

            toolbar.getNextButton().on({
                tap: 'onNextPageTap',
                scope: me
            });

            toolbar.getPrevButton().on({
                tap: 'onPreviousPageTap',
                scope: me
            });
        }
    },

    onPageChange: function(field, value) {
        this.setCurrentPage(value);
    },

    onPageSliderDrag: function(field, slider, value) {
        this.isDragging = true;
        this.setCurrentPage(Ext.isArray(value) ? value[0] : value);
    },

    onPageSliderDragEnd: function() {
        var me = this;

        me.isDragging = false;
        if (me.getBuffer() === 'dragend' || me.bufferTask.Id) {
            me.cancelBufferTask();
            me.loadCurrentPage();
        }
    },

    onNextPageTap: function() {
        var nextPage = this.getCurrentPage() + 1;
        if (nextPage <= this.getTotalPages()) {
            this.setCurrentPage(nextPage);
        }
    },

    onPreviousPageTap: function() {
        var previousPage = this.getCurrentPage() - 1;
        if (previousPage > 0) {
            this.setCurrentPage(previousPage);
        }
    },

    onTotalCountChange: function(store) {
        var me = this,
            data = me.getPageData();

        me.bulkConfigs = true;
        me.setConfig(data);
        me.bulkConfigs = false;
        me.syncSummary();
    },

    onUpdateVisibleCount: function(grid, visibleCount) {
        var store = grid.getStore(),
            totalCount;

        if(store && !this.getLoadPages()){
            visibleCount -= 1;
            this.setPageSize(visibleCount);
            totalCount = store.getTotalCount() || store.getCount();
            this.setTotalPages( Math.ceil(totalCount / visibleCount) );
        }
    },

    updateTotalPages: function() {
        if(!this.isConfiguring) {
            this.syncSummary();
        }
    },

    updateCurrentPage: function(page) {
        var me = this,
            isDragging = me.isDragging,
            bufferTask = me.bufferTask,
            buffer = me.getBuffer();

        if(!me.isConfiguring) {
            if(me.getLoadPages()){
                if (bufferTask && Ext.isNumber(buffer) && isDragging) {
                    bufferTask.delay(buffer);
                }
                else if (buffer !== 'dragend' || !isDragging) {
                    me.getGrid().getStore().loadPage(page);
                }
            }
            else{
                me.syncSummary();
            }
        }
    },

    updateTotalCount: function(totalCount) {
        if(!this.isConfiguring) {
            this.syncSummary();
        }
    },

    getPageTopRecord: function(page) {
        var grid = this.getGrid(),
            store = grid && grid.getStore(),
            pageSize = this.getPageSize(),
            pageTopRecordIndex = (page - 1) * pageSize;

        return store && store.getAt(pageTopRecordIndex);
    },

    privates: {
        syncSummary: function() {
            var me = this,
                grid = me.getGrid(),
                toolbar = me.getToolbar(),
                sliderField = toolbar.getSliderField(),
                currentPage = me.getCurrentPage(),
                totalPages = me.getTotalPages(),
                pageTopRecord;

            if(me.bulkConfigs){
                return;
            }

            // TODO: Calling setHtml causes a performance issue while live scrolling,
            // this might be worth looking into.
            toolbar.getSummaryComponent().element.dom.innerHTML = currentPage + ' / ' + totalPages;

            sliderField.setMaxValue(totalPages || 1);
            sliderField.setValue(currentPage);
            sliderField.setDisabled(totalPages <= 1);

            pageTopRecord = me.getPageTopRecord(currentPage);
            if (grid && !me.preventGridScroll && pageTopRecord) {
                grid.scrollToRecord(pageTopRecord);
            }

            toolbar.getNextButton().setDisabled(currentPage === totalPages);
            toolbar.getPrevButton().setDisabled(currentPage === 1);
        }
    }
});
