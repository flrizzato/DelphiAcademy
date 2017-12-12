/* global Ext, expect, jasmine, xit */
topSuite("Ext.grid.plugin.PagingToolbar",
    ['Ext.grid.Grid', 'Ext.grid.plugin.PagingToolbar', 'Ext.form.Panel',
        'Ext.field.*'],
function() {
    var ajax = Ext.Ajax,
        tb, store, grid,
        describeNotIE9_10 = Ext.isIE9 || Ext.isIE10 ? xdescribe : describe,
        synchronousLoad = true,
        proxyStoreLoad = Ext.data.ProxyStore.prototype.load,
        loadStore = function () {
            proxyStoreLoad.apply(this, arguments);
            if (synchronousLoad) {
                this.flushLoad.apply(this, arguments);
            }
            return this;
        };

    var M = Ext.define(null, {
        extend: 'Ext.data.Model',
        fields: ['name']
    });

    function makeStore(pageSize) {
        store = new Ext.data.Store({
            model: M,
            pageSize: pageSize != null ? pageSize : 5,
            proxy: {
                type: 'ajax',
                url: 'fakeUrl',
                reader: {
                    type: 'json',
                    rootProperty: 'data',
                    totalProperty: 'total'
                }
            }
        });
        return store;
    }

    function makeGrid(buffer) {
        grid = new Ext.grid.Grid({
            requires: [
                'Ext.grid.plugin.PagingToolbar'
            ],

            renderTo: Ext.getBody(),
            height: 400,
            width: 600,

            plugins: [
                { type: 'gridpagingtoolbar', buffer: buffer }
            ],

            store: makeStore(),

            columns: [{
                text: 'Name',
                dataIndex: 'name',
                flex: 1,
                sortable: false
            }]
        });
        tb = grid.getPlugins()[0];
        return grid;
    }

    function makeData(total, start, limit) {
        var data = [],
            i;

        if (limit === undefined) {
            limit = start + store.pageSize;
        }

        for (i = start; i < limit; ++i) {
            data.push({
                name: 'Item ' + (i + 1)
            });
        }

        return Ext.encode({
            data: data,
            total: total
        });
    }

    function mockComplete(responseText, status) {
        Ext.Ajax.mockComplete({
            status: status || 200,
            responseText: responseText
        });
    }
    function requestCount() {
        return Ext.Ajax.mockGetAllRequests().length;
    }

    beforeEach(function () {
        // Override so that we can control asynchronous loading
        Ext.data.ProxyStore.prototype.load = loadStore;
        MockAjaxManager.addMethods();
    });

    afterEach(function () {
        // Undo the overrides.

        Ext.data.ProxyStore.prototype.load = proxyStoreLoad;

        MockAjaxManager.removeMethods();

        grid = Ext.destroy(grid);
        tb = Ext.destroy(tb);
        store = Ext.destroy(store);
        Ext.data.Model.schema.clear();
    });

    describe("buffer/delay", function () {
        it("should not delay if not dragging", function() {
            makeGrid(250);
            store.load();
            mockComplete(makeData(200, 1));
            expect(requestCount()).toBe(0);

            tb.setCurrentPage(10);
            expect(requestCount()).toBe(1);
            mockComplete(makeData(200, tb.getCurrentPage()*store.pageSize));
            expect(requestCount()).toBe(0);
            tb.setCurrentPage(30);
            expect(requestCount()).toBe(1);
            mockComplete(makeData(200, tb.getCurrentPage()*store.pageSize));
            expect(requestCount()).toBe(0);
        });

        it("should delay if dragging", function() {
            makeGrid(250);
            store.load();
            mockComplete(makeData(200, 1));
            expect(requestCount()).toBe(0);

            tb.isDragging = true;
            tb.setCurrentPage(10);
            expect(requestCount()).toBe(0);
            waits(200);
            runs(function() {
                expect(requestCount()).toBe(0);
            });
            waitsFor(function() {
                return !!requestCount();
            });
            runs(function() {
                mockComplete(makeData(200, tb.getCurrentPage()*store.pageSize));
                expect(requestCount()).toBe(0);
            });
        });

        it("should only page if buffer ms pause in dragging", function() {
            makeGrid(250);
            store.load();
            mockComplete(makeData(200, 1));
            expect(requestCount()).toBe(0);

            tb.isDragging = true;
            tb.setCurrentPage(10);
            waits(200);
            runs(function() {
                expect(requestCount()).toBe(0);
                tb.setCurrentPage(20);
                expect(requestCount()).toBe(0);
            });
            waitsFor(function() {
                return !!requestCount();
            });
            runs(function() {
                mockComplete(makeData(200, tb.getCurrentPage()*store.pageSize));
                expect(requestCount()).toBe(0);
            });
        });

        it("should not page while dragging if buffer is set to 'dragend'", function() {
            makeGrid('dragend');
            store.load();
            mockComplete(makeData(200, 1));
            expect(requestCount()).toBe(0);

            tb.isDragging = true;
            tb.setCurrentPage(10);
            expect(requestCount()).toBe(0);
            tb.setCurrentPage(20);
            expect(requestCount()).toBe(0);
            waits(500);
            runs(function() {
                expect(requestCount()).toBe(0);
                tb.onPageSliderDragEnd();
                expect(requestCount()).toBe(1);
                mockComplete(makeData(200, tb.getCurrentPage()*store.pageSize));
            });
        });

        it("should only page on drag end if buffer is set to 'dragend'", function() {
            makeGrid('dragend');
            store.load();
            mockComplete(makeData(200, 1));
            expect(requestCount()).toBe(0);

            tb.isDragging = true;
            tb.setCurrentPage(10);
            expect(requestCount()).toBe(0);
            tb.setCurrentPage(20);
            waits(200);
            runs(function() {
                expect(requestCount()).toBe(0);
            });
            waits(100);
            runs(function() {
                expect(requestCount()).toBe(0);
                tb.onPageSliderDragEnd();
                expect(requestCount()).toBe(1);
                mockComplete(makeData(200, tb.getCurrentPage()*store.pageSize));
            });
        });

    });
});

