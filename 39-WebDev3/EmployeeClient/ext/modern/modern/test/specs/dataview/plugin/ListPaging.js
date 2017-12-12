xtopSuite("Ext.dataview.plugin.ListPaging", [
    'Ext.List', 'Ext.data.ArrayStore', 'Ext.layout.Fit'
], function() {
    var views = [];

    function acquireView(config) {
        var view = Ext.create(Ext.merge({
            xtype: 'list',
            renderTo: Ext.getBody(),
            itemTpl: '{value}',
            height: 256,
            width: 256,
            plugins: {
                type: 'listpaging',
                loadMoreCmp: {
                    itemId: 'listpaging-trigger'
                }
            },
            store: {
                autoLoad: true,
                pageSize: 10,
                proxy: {
                    type: 'ajax',
                    url: 'ext.plugin.listpaging.data',
                    reader: {
                        type: 'json',
                        rootProperty: 'data'
                    }
                }
            }
        }, config || {}));

        views[view.getId()] = view;
        view.refresh();
        return view;
    }

    function releaseView(view) {
        var id = view.getId();
        delete views[id];
        view.destroy();
    }

    function sendResponse(page, limit, total) {
        var start = (page - 1) * limit;
        Ext.Ajax.mockCompleteWithData({
            total: total || 25,
            data: Array.apply(null, Array(limit)).map(function(value, index) {
                return { id: start + index, value: 'Item ' + (start + index) };
            })
        });
    }

    function waitForLoaded(store, timeout) {
        return new Ext.Promise(function (resolve) {
            var timer = Ext.defer(function() {
                Ext.destroy(listener);
                resolve(false);
            }, timeout || 500);

            var listener = store.on({
                destroyable: true,
                load: {
                    single: true,
                    fn: function() {
                        Ext.undefer(timer);
                        resolve(true);
                    }
                }
            });
        });
    }

    function waitForLoadingAndReply(store, timeout) {
        store.on({
            beforeLoad: {
                single: true,
                fn: function(store, operation) {
                    var config = operation.getConfig();
                    Ext.asap(sendResponse, null, [
                        config.page,
                        config.limit,
                        128
                    ]);
                }
            }
        });

        return waitForLoaded(store, timeout);
    }

    function click(item) {
        jasmine.fireMouseEvent(item.ariaEl.dom, 'click');
    }

    beforeEach(function() {
        MockAjaxManager.addMethods();
    });

    afterEach(function() {
        Ext.Object.getValues(views).forEach(releaseView);
        MockAjaxManager.removeMethods();
    });

    describe('manual paging', function() {
        it('should load next page when clicking the "load more" button', function(done) {
            var list = acquireView({
                    plugin: {
                        autoPaging: false
                    }
                }),
                trigger = list.down('#listpaging-trigger'),
                store = list.getStore();

            waitForLoadingAndReply(store).then(function(loaded) {
                expect(loaded).toBeTruthy();
                expect(store.currentPage).toBe(1);
                expect(store.getCount()).toBe(10);
                expect(store.getTotalCount()).toBe(128);

                click(trigger);

                return waitForLoadingAndReply(store);
            }).then(function(loaded) {
                expect(loaded).toBeTruthy();
                expect(store.currentPage).toBe(2);
                expect(store.getCount()).toBe(20);
                expect(store.isLoading()).toBeFalsy();
            }).then(done).done();
        });
    });

    describe('auto paging', function() {
        it('should not load next page when scrolling to bottom if autoPaging is false', function(done) {
            var list = acquireView({
                    plugin: {
                        autoPaging: false
                    }
                }),
                scroller = list.getScrollable(),
                store = list.getStore();

            waitForLoadingAndReply(store).then(function(loaded) {
                expect(loaded).toBeTruthy();
                expect(store.currentPage).toBe(1);
                expect(store.getCount()).toBe(10);
                expect(store.getTotalCount()).toBe(128);

                scroller.scrollTo(null, Infinity);
                return waitForLoadingAndReply(store);
            }).then(function(loaded) {
                expect(loaded).toBeFalsy();
                expect(store.currentPage).toBe(1);
                expect(store.getCount()).toBe(10);
                expect(store.isLoading()).toBeFalsy();
            }).then(done).done();
        });

        it('should load next page only when scrolling below the buffer zone', function(done) {
            var list = acquireView({
                    itemHeight: 42,
                    height: 210,    // 5 items in view
                    plugins: {
                        autoPaging: true,
                        bufferZone: 3
                    },
                    store: {
                        pageSize: 10
                    }
                }),
                scroller = list.getScrollable(),
                store = list.getStore();

            waitForLoadingAndReply(store).then(function(loaded) {
                expect(loaded).toBeTruthy();
                expect(store.currentPage).toBe(1);
                expect(store.getCount()).toBe(10);
                expect(store.getTotalCount()).toBe(128);

                // scroll to item just above the buffer zone (10 - 4)
                var el = list.getItemAt(6).element;
                scroller.ensureVisible(el);
                return waitForLoadingAndReply(store);
            }).then(function(loaded) {
                expect(loaded).toBeFalsy();
                expect(store.currentPage).toBe(1);
                expect(store.getCount()).toBe(10);
                expect(store.isLoading()).toBeFalsy();

                // scroll to item at the top of the buffer zone (10 - 3)
                var el = list.getItemAt(7).element;
                scroller.ensureVisible(el);
                return waitForLoadingAndReply(store);
            }).then(function(loaded) {
                expect(loaded).toBeTruthy();
                expect(store.currentPage).toBe(2);
                expect(store.getCount()).toBe(20);
                expect(store.isLoading()).toBeFalsy();
            }).then(done).done();
        });

        it('should load next page only when reaching the end of the list if bufferZone == 0', function(done) {
            var list = acquireView({
                    itemHeight: 42,
                    height: 210,    // 5 items in view
                    plugins: {
                        autoPaging: true,
                        bufferZone: 0
                    },
                    store: {
                        pageSize: 10
                    }
                }),
                scroller = list.getScrollable(),
                store = list.getStore();

            waitForLoadingAndReply(store).then(function(loaded) {
                expect(loaded).toBeTruthy();
                expect(store.currentPage).toBe(1);
                expect(store.getCount()).toBe(10);
                expect(store.getTotalCount()).toBe(128);

                // scroll to the last "almost" bottom of the last item
                var el = list.getItemAt(9).element;
                var y = scroller.getEnsureVisibleXY(el).y - 1;
                scroller.scrollTo(null, y);
                return waitForLoadingAndReply(store);
            }).then(function(loaded) {
                expect(loaded).toBeFalsy();
                expect(store.currentPage).toBe(1);
                expect(store.getCount()).toBe(10);
                expect(store.isLoading()).toBeFalsy();

                // scroll one pixel further to reach the end of the list (bottom of the last item)
                scroller.scrollBy(null, 1);
                return waitForLoadingAndReply(store);
            }).then(function(loaded) {
                expect(loaded).toBeTruthy();
                expect(store.currentPage).toBe(2);
                expect(store.getCount()).toBe(20);
                expect(store.isLoading()).toBeFalsy();
            }).then(done).done();
        });

        it('should properly disconnect if the plugin is destroyed', function(done) {
            var list = acquireView({
                    plugins: {
                        autoPaging: true,
                        bufferZone: 8
                    },
                    store: {
                        pageSize: 1
                    }
                }),
                plugin = list.findPlugin('listpaging'),
                store = list.getStore();

            waitForLoadingAndReply(store).then(function(loaded) {
                expect(loaded).toBeTruthy();
                expect(store.isLoading()).toBeTruthy();

                // Note: the view contains only 1 item so the plugin still
                // load next pages until the buffer zone is respected.

                releaseView(list);

                Ext.asap(sendResponse, null, [2, 10, 128]);
                return waitForLoaded(store);
            }).then(function(loaded) {
                expect(loaded).toBeTruthy();
                expect(list.destroyed).toBeTruthy();
                expect(plugin.destroyed).toBeTruthy();
                expect(store.isLoading()).toBeFalsy();

                return waitForLoadingAndReply(store);
            }).then(function(loaded) {
                expect(loaded).toBeFalsy();
            }).then(done).done();
        });
    });
});
