/* global Ext, jasmine, expect */

topSuite("Ext.grid.Grid_Tools",
    ['Ext.grid.Grid', 'Ext.data.ArrayStore', 'Ext.layout.Fit'],
function() {

    var Model = Ext.define(null, {
        extend: 'Ext.data.Model',
        fields: [ 'f1', 'f2', 'f3', 'f4', 'f5', 'gf' ]
    });

    var grid, store, colMap;

    function makeStore(rows, groups) {
        var data = [],
            i;

        if (rows) {
            if (typeof rows !== 'number') {
                data = rows;
            }
        } else if (rows !== 0) {
            rows = 20;
        }

        groups = groups || 1;

        for (i = 1; i <= rows; ++i) {
            data.push({
                f1: 'f1' + i,
                f2: 'f2' + i,
                f3: 'f3' + i,
                f4: 'f4' + i,
                f5: 'f5' + i,

                gf: 'gf' + (i % groups)
            });
        }

        store = new Ext.data.Store({
            model: Model,
            data: data
        });
    }

    afterEach(function() {
        store = grid = Ext.destroy(grid, store);
    });

    function makeColumns (num, group) {
        var columns = [{/* f1 */}, {/* f2 */}, {/* f3 */}, {/* f4 */}, {/* f5 */}, {
            dataIndex: 'gf',
            itemId: 'groupFld'
        }];

        if (!group) {
            columns.pop();
        }

        while (num && columns.length > num) {
            columns.pop();
        }

        return columns;
    }

    function makeGrid (options) {
        options = options || {};

        var columns = options.columns;
        var gridOptions = options.grid || {};

        if (!store && !gridOptions.store) {
            makeStore(options.rows || 50, options.groups);
        }

        if (typeof columns === 'number') {
            columns = makeColumns(columns, options.groups);
        }
        else if (!columns && columns !== null && !gridOptions.columns) {
            columns = makeColumns(5, options.groups);
        }

        if (columns) {
            columns.forEach(function(col, i) {
                col.dataIndex = col.dataIndex || ('f' + (i + 1));
                col.itemId = col.itemId || ('col' + col.dataIndex);
                col.text = col.text || col.dataIndex.toUpperCase();
                col.width = col.width || 100;
            });
        }

        grid = new Ext.grid.Grid(Ext.apply({
            width: 600,
            height: 500,
            store: store,
            columns: columns
        }, gridOptions));

        if (options.groups) {
            grid.getStore().group('gf');
        }

        setColMap();
    }

    function renderGrid (el) {
        grid.render(el || Ext.getBody());
        grid.refresh();
    }

    function setColMap () {
        colMap = {};
        grid.query('column').forEach(function(col) {
            colMap[col.getItemId()] = col;
        });
    }

    function getCells (col) {
        var cells = [];

        if (typeof col === 'number') {
            col = grid.getColumns()[col];
        }

        store.each(function(rec) {
            var row = grid.mapToItem(rec);

            if (row) {
                cells.push(row.getCellByColumn(col));
            }
        });

        return cells;
    }

    describe('Cell tools', function () {
        it('should create tools in isolation', function () {
            makeGrid({
                columns: [{
                    cell: {
                        tools: {
                            gear: 'onGear'
                        }
                    }
                }]
            });
            renderGrid();

            var cells = getCells(0);

            cells.forEach(function (cell) {
                var tools = cell.getTools();
                var tool = tools[0];

                expect(tools.length).toBe(1);
                expect(cell.el.contains(tool.el)).toBe(true);
                expect(tool.handler).toBe('onGear');

                var zone = tool.el.up('.x-tool-zone');

                expect(zone.hasCls('x-head')).toBe(true);
                expect(cell.el.contains(zone)).toBe(true);
            });
        });

        it('should create tools in isolation with specified zone', function () {
            makeGrid({
                columns: [{
                    cell: {
                        tools: {
                            gear: 'onGear',
                            search: {
                                zone: 'end',
                                handler: 'onSearch'
                            }
                        }
                    }
                }]
            });
            renderGrid();

            var cells = getCells(0);

            cells.forEach(function (cell) {
                var tools = cell.getTools();
                var gear = tools[0];
                var search = tools[1];

                expect(tools.length).toBe(2);

                expect(cell.el.contains(gear.el)).toBe(true);
                expect(cell.el.contains(search.el)).toBe(true);
                expect(gear.handler).toBe('onGear');
                expect(search.handler).toBe('onSearch');

                var zone = gear.el.up('.x-tool-zone');

                expect(zone.hasCls('x-head')).toBe(true);
                expect(cell.el.contains(zone)).toBe(true);

                zone = search.el.up('.x-tool-zone');

                expect(zone.hasCls('x-end')).toBe(true);
                expect(cell.el.contains(zone)).toBe(true);
            });
        });
    });

    describe('Header tools', function () {
        it('should create tools in isolation', function () {
            makeGrid({
                groups: 5,

                grid: {
                    grouped: true,
                    groupHeader: {
                        tools: {
                            print: 'onPrint'
                        }
                    }
                }
            });

            renderGrid();
            store.group('gf');

            // No API for peeking at headers
            var headerIndices = grid.headerIndices;

            for (var index in headerIndices) {
                var row = grid.mapToItem(store.getAt(+index));

                if (row) {
                    var header = row.$header,
                        tools = header.getTools(),
                        tool = tools[0];

                    expect(tools.length).toBe(1);
                    expect(header.el.contains(tool.el)).toBe(true);
                    expect(tool.handler).toBe('onPrint');

                    var zone = tool.el.up('.x-tool-zone');

                    expect(zone.hasCls('x-tool-zone-start')).toBe(true);
                    expect(zone.hasCls('x-tool-zone-end')).toBe(false);
                    expect(header.el.contains(zone)).toBe(true);
                }
            }
        });

        it('should create tools in all 3 zones', function () {
            makeGrid({
                groups: 5,

                grid: {
                    grouped: true,
                    itemConfig: {
                        header: {
                            tools: {
                                print: 'onPrint',
                                gear: {
                                    zone: 'tail',
                                    handler: 'onTailGear'
                                },
                                pin: {
                                    zone: 'end',
                                    handler: 'onPinEnd'
                                }
                            }
                        }
                    }
                }
            });

            renderGrid();
            store.group('gf');

            // No API for peeking at headers
            var headerIndices = grid.headerIndices;

            for (var index in headerIndices) {
                var row = grid.mapToItem(store.getAt(+index));

                if (row) {
                    var header = row.$header;
                    var tools = header.getTools();
                    var print = tools[0];
                    var gear = tools[1];
                    var pin = tools[2];

                    expect(tools.length).toBe(3);
                    expect(header.el.contains(print.el)).toBe(true);
                    expect(print.handler).toBe('onPrint');

                    var zone = print.el.up('.x-tool-zone');

                    expect(zone.hasCls('x-tool-zone-start')).toBe(true);
                    expect(zone.hasCls('x-tool-zone-tail')).toBe(false);
                    expect(zone.hasCls('x-tool-zone-end')).toBe(false);
                    expect(header.el.contains(zone)).toBe(true);

                    zone = gear.el.up('.x-tool-zone');

                    expect(zone.hasCls('x-tool-zone-start')).toBe(false);
                    expect(zone.hasCls('x-tool-zone-tail')).toBe(true);
                    expect(zone.hasCls('x-tool-zone-end')).toBe(false);
                    expect(header.el.contains(zone)).toBe(true);

                    zone = pin.el.up('.x-tool-zone');

                    expect(zone.hasCls('x-tool-zone-start')).toBe(false);
                    expect(zone.hasCls('x-tool-zone-tail')).toBe(false);
                    expect(zone.hasCls('x-tool-zone-end')).toBe(true);
                    expect(header.el.contains(zone)).toBe(true);
                }
            }
        });

        describe('Adding/removing pressed class on mousedown and mouseup', function() {
            var printTool;

            beforeEach(function() {
                makeGrid({
                    groups: 5,

                    grid: {
                        grouped: true,
                        groupHeader: {
                            tools: {
                                print: Ext.emptyFn
                            }
                        }
                    }
                });

                renderGrid();
                store.group('gf');
                printTool = grid.mapToItem(store.getAt(0)).$header.getTools()[0];
            });

            (jasmine.supportsTouch ? describe : xdescribe)("touchstart + touchend sequence", function() {
                it("should call onRelease", function() {
                    spyOn(printTool, "onRelease").andCallThrough();

                    Ext.testHelper.fireEvent('start', printTool.el);
                    expect(printTool.onRelease).not.toHaveBeenCalled();
                    expect(printTool).toHaveCls(printTool.pressedCls);

                    // Touch end anywhere in the document should release pressed state
                    Ext.testHelper.fireEvent('end', document.body);
                    expect(printTool.onRelease).toHaveBeenCalled();
                    expect(printTool).not.toHaveCls(printTool.pressedCls);
                });
            });
            (jasmine.supportsTouch ? xdescribe : describe)("mousedown + mouseup sequence", function() {
                it("should call onRelease", function() {
                    spyOn(printTool, "onRelease").andCallThrough();

                    jasmine.fireMouseEvent(printTool.el, 'mousedown');
                    expect(printTool.onRelease).not.toHaveBeenCalled();
                    expect(printTool).toHaveCls(printTool.pressedCls);

                    // Mouse up anywhere in the document should release pressed state
                    jasmine.fireMouseEvent(document.body, 'mouseup');
                    expect(printTool.onRelease).toHaveBeenCalled();
                    expect(printTool).not.toHaveCls(printTool.pressedCls);
                });
            });
        });
    });

    //TODO test handler parameters for cell tools
    //TODO test handler parameters for header tools
    //TODO test binding to cells
    //TODO test binding to headers
    //TODO implement and test CQ like down('tool')
});
