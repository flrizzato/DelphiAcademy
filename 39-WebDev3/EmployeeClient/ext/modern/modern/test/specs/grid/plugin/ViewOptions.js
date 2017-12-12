/* global Ext, expect, jasmine, xit */
topSuite('Ext.grid.plugin.ViewOptions',
    ['Ext.grid.Grid', 'Ext.grid.plugin.ViewOptions'],
function () {
    var grid, store, plugin;

    jasmine.usesViewport();

    var Model = Ext.define(null, {
        extend: 'Ext.data.Model',
        fields: ['group', 'f1', 'f2', 'f3', 'f4', 'f5', 'f6', 'f7', 'f8', 'f9']
    });

    function makeStore (rows, storeOptions) {
        var data = [],
            i;

        if (rows) {
            if (typeof rows !== 'number') {
                data = rows;
            }
        } else if (rows !== 0) {
            rows = 20;
        }

        for (i = 1; i <= rows; ++i) {
            data.push({
                group: 'g' + Math.ceil(i / 10),
                f1: 'f1' + i,
                f2: 'f2' + i,
                f3: 'f3' + i,
                f4: 'f4' + i,
                f5: 'f5' + i,
                f6: 'f6' + i,
                f7: 'f7' + i,
                f8: 'f8' + i,
                f9: 'f9' + i
            });
        }

        store = new Ext.data.Store(Ext.apply({
            model: Model,
            data: data
        }, storeOptions));

        return store;
    }

    function makeGrid (colOptions, gridOptions, data, storeOptions) {
        gridOptions = gridOptions || {};

        if (!gridOptions.store) {
            makeStore(data, storeOptions);
        }

        if (colOptions) {
            for (var i = 0; i < colOptions.length; i++) {
                if (!colOptions[i].text) {
                    colOptions[i].text = 'F' + (i + 1);
                }
            }
        } else {
            colOptions = [{
                dataIndex: 'f1',
                width: 100,
                text: 'F1',
                itemId: 'colf1'
            }, {
                dataIndex: 'f2',
                width: 100,
                text: 'F2',
                itemId: 'colf2'
            }, {
                dataIndex: 'f3',
                width: 100,
                text: 'F3',
                itemId: 'colf3'
            }, {
                dataIndex: 'f4',
                width: 100,
                text: 'F4',
                itemId: 'colf4'
            }, {
                dataIndex: 'f5',
                width: 100,
                text: 'F5',
                itemId: 'colf5'
            }];
        }

        if (colOptions) {
            colOptions.forEach(function (col, i) {
                if (!col.dataIndex) {
                    col.dataIndex = 'f' + (i + 1);
                }
            });
        }

        grid = new Ext.grid.Grid(Ext.merge({
            renderTo: Ext.getBody(),
            width: 600,
            height: 1200,
            rowNumbers: true,
            store: store,
            columns: colOptions,
            plugins: {
                gridviewoptions: true
            }
        }, gridOptions));

        plugin = grid.getPlugin('gridviewoptions');
    }

    afterEach(function () {
        store = grid = plugin =
            Ext.destroy(grid, store);
    });

    describe('sheet', function () {
        it('should not create sheet', function () {
            makeGrid();

            var sheet = plugin.getConfig('sheet', null, true);

            expect(sheet).toBeNull();
        });

        it('should show sheet', function () {
            makeGrid();

            var sheet = plugin.getSheet();

            plugin.showViewOptions();

            expect(plugin.doneSetup).toBeTruthy();

            waitsFor(function () {
                return !sheet.activeAnimation;
            });

            runs(function () {
                expect(sheet.isVisible()).toBeTruthy();
            });
        });
    });

    describe('columnList', function () {
        it('should not create columnList', function () {
            makeGrid();

            var columnList = plugin.getConfig('columnList', null, true);

            expect(columnList).toBeNull();
        });

        it('should add to sheet', function () {
            makeGrid();

            var sheet = plugin.getSheet(),
                // let's make sure updateSheet created columnList
                columnList = plugin.getConfig('columnList', null, true);

            expect(columnList.parent).toBe(sheet);
        });
    });

    describe('RowNumberer', function () {
        it('should not show RowNumberer column in list', function () {
            makeGrid();

            var columns = grid.getColumns(),
                column = columns[0],
                columnList = plugin.getColumnList(),
                sheet = plugin.getSheet();

            expect(column.isRowNumberer).toBe(true);
            expect(columns.length).toBe(6); // 5 regular columns + row numberer

            plugin.showViewOptions();

            waitsFor(function () {
                return !sheet.activeAnimation;
            });

            runs(function () {
                var store = columnList.getStore();

                expect(store.getCount()).toBe(5); // just 5 regular columns
            });
        });
    });
});
