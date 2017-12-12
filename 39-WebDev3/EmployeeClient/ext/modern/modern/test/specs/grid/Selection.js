/* global expect, Ext, jasmine, xdescribe, MockAjaxManager */

topSuite('Ext.grid.Selection', [
    false, // because Ext.grid.Selection isn't a thing
    'Ext.grid.Grid', 'Ext.app.ViewModel',
    'Ext.grid.plugin.CellEditing', 'Ext.grid.selection.Replicator',
    'Ext.grid.plugin.PagingToolbar', 'Ext.Button',
    'Ext.data.virtual.Store'],
function() {
    var itNotTouch = jasmine.supportsTouch ? xit : it,
        xitNotTouch = xit,
        grid, view, store, selModel, colRef,
        // Unreliable synthetic events on IE.
        // SelModel tests are not browser-dependent though
        smDescribe = Ext.isIE ? xdescribe : describe,
        synchronousLoad = true,
        proxyStoreLoad = Ext.data.ProxyStore.prototype.load,
        loadStore = function () {
            proxyStoreLoad.apply(this, arguments);
            if (synchronousLoad) {
                this.flushLoad.apply(this, arguments);
            }
            return this;
        };

    function triggerCellMouseEvent(type, rowIdx, cellIdx, button, x, y) {
        var target = findCell(rowIdx, cellIdx);
        jasmine.fireMouseEvent(target, type, x, y, button);
    }

    function findCell(rowIdx, cellIdx) {
        var row = grid.mapToItem(store.getAt(rowIdx));
        return row.cells[cellIdx].element.dom;
    }

    function isCellSelected(row, col) {
        return view.getSelectable().isCellSelected(row, col);
    }

    function isColumnSelected(index) {
        return view.getSelectable().isColumnSelected(colRef[index]);
    }

    function isRowSelected(index) {
        return view.getSelectable().isSelected(index);
    }

    function clickRowNumberer(index, ctrlKey) {
        var target = findCell(index, 0);
        jasmine.fireMouseEvent(target, 'click', null, null, null, null, ctrlKey);
    }

    function spyOnEvent(object, eventName, fn) {
        var obj = {
                fn: fn || Ext.emptyFn
            },
            spy = spyOn(obj, 'fn');

        object.addListener(eventName, obj.fn);
        return spy;
    }

    function makeLocation(rowIdx, colIdx) {
        return new Ext.grid.Location(grid, {
            record: store.getAt(rowIdx),
            column: grid.getVisibleColumns()[colIdx]
        });
    }

    function makeGrid(columns, cfg, selModelCfg, storeCfg, locked, dataCount) {
        var data = [],
            defaultCols = [],
            i;

        if (!columns) {
            for (i = 1; i <= 5; ++i) {
                defaultCols.push({
                    name: 'F' + i,
                    text: 'F' + i,
                    dataIndex: 'field' + i,

                    // First column locked if locked passed
                    locked: locked && i === 1,
                    cell: {
                        cls: 'x-testcell-' + i
                    },
                    cellSelector: '.x-testcell-' + i
                });
            }
        }

        for (i = 1; i <= (dataCount || 10); ++i) {
            if (storeCfg && storeCfg.numeric) {
                data.push({
                    field1: i * 10 + 1,
                    field2: i * 10 + 2,
                    field3: i * 10 + 3,
                    field4: i * 10 + 4,
                    field5: i * 10 + 5
                });
            } else {
                data.push({
                    field1: i + '.' + 1,
                    field2: i + '.' + 2,
                    field3: i + '.' + 3,
                    field4: i + '.' + 4,
                    field5: i + '.' + 5
                });
            }
        }

        storeCfg = Ext.apply({
            model: spec.grid.SelectionRecord
        }, storeCfg);

        // Apply the generated data to the store, or if a memory proxy, to the proxy
        if (storeCfg.proxy && storeCfg.proxy.type === 'memory' && !storeCfg.proxy.data) {
            storeCfg.proxy.data = data;
        } else if (!('data' in storeCfg)) {
            storeCfg.data = data;
        }

        store = new Ext.data.Store(storeCfg);

        grid = new Ext.grid.Grid(Ext.apply({
            columns: columns || defaultCols,
            store: store,
            rowNumbers: true,
            selectable: typeof selModelCfg === 'boolean' ? selModelCfg : Ext.apply({
                drag: true,
                cells: true,
                columns: true,
                rows: true,
                checkbox: false
            }, selModelCfg),
            width: 600,
            height: 300,
            renderTo: Ext.getBody()
        }, cfg));
        view = grid;
        selModel = grid.getSelectable();
        colRef = grid.getVisibleColumns();

        waits(100);
    }

    beforeEach(function () {
        Ext.define('spec.grid.SelectionRecord', {
            extend: 'Ext.data.Model',
            fields: [
                'field1',
                'field2',
                'field3',
                'field4',
                'field5'
            ]
        });

        // Override so that we can control asynchronous loading
        Ext.data.ProxyStore.prototype.load = loadStore;
    });

    afterEach(function () {
        // Undo the overrides.
        Ext.data.ProxyStore.prototype.load = proxyStoreLoad;

        Ext.destroy(grid, store);
        selModel = grid = store = view = null;
        Ext.undefine('spec.grid.SelectionRecord');
        Ext.data.Model.schema.clear();

        try {
            delete Ext.global.spec;
        } catch (e) {
            Ext.global.spec = undefined;
        }
    });

    smDescribe("refresh", function () {
        itNotTouch("should retain selection", function () {
            var target,
                cell,
                selected;

            makeGrid();

            target = findCell(1, 2);
            jasmine.fireMouseEvent(target, 'click', null, null, null, null, null);
            target = findCell(8, 4);
            jasmine.fireMouseEvent(target, 'click', null, null, null, true, null);  // shift key down
            view.refresh();
            selected = view.getSelectable().getSelection();
            expect(selected).not.toBeNull();
            cell = selected.startCell;
            expect(cell).not.toBeNull();
            expect(cell.columnIndex).toBe(2);
            expect(cell.recordIndex).toBe(1);
            cell = selected.endCell;
            expect(cell).not.toBeNull();
            expect(cell.columnIndex).toBe(4);
            expect(cell.recordIndex).toBe(8);
        });

    });

    smDescribe('remove records', function () {
        it("should allow removal of last record", function () {
            expect(function () {
                makeGrid();
                var target = findCell(0, 0);
                jasmine.fireMouseEvent(target, 'click', null, null, null, null, null);
                target = findCell(8, 0);
                jasmine.fireMouseEvent(target, 'click', null, null, null, true, null);  // shift key down

                store.remove(selModel.getSelection());
                target = findCell(0, 0);
                jasmine.fireMouseEvent(target, 'click', null, null, null, null, null);
                store.remove(selModel.getSelection());
            }).not.toThrow();
        });
    });

    smDescribe('selectable: false', function() {
        it("should create a disabled SelectionModel", function() {
            makeGrid(null, null, false);
            expect(selModel.getDisabled()).toBe(true);
        });
    });

    smDescribe("with a bound store", function() {
        it("should not throw an exception", function() {
            expect(function() {
                makeGrid(null, {
                    viewModel: {
                        stores: {
                            people: {
                                model: 'spec.grid.SelectionRecord'
                            }
                        }
                    },
                    bind: {
                        store: '{people}'
                    },
                    store: null
                });
            }).not.toThrow();
        });
    });

    smDescribe("Non-rendered operation", function () {
        it("should allow reconfiguration before render", function () {
            makeGrid(null, {
                renderTo: null
            });

            // These configs have to work before render
            expect(function () {
                selModel.setRows(false);
                selModel.setRows(true);
                selModel.setColumns(false);
                selModel.setColumns(true);
                selModel.setCells(false);
                selModel.setCells(true);
            }).not.toThrow();
        });
        it("should allow selection of cells before render", function () {
            var cell;

            makeGrid(null, {
                renderTo: null
            });

            cell = makeLocation(2, 2);

            selModel.selectCells(cell, cell);

            grid.render(document.body);
            expect(isCellSelected(2, 2)).toBe(true);
        });
        it("should allow selection of cells before render using array notation", function () {
            makeGrid(null, {
                renderTo: null
            });

            selModel.selectCells([3, 2], [3, 2]);

            grid.render(document.body);

            expect(isCellSelected(3, 2)).toBe(true);
        });
        it("should allow selection of records before render", function () {
            makeGrid(null, {
                renderTo: null
            });

            selModel.select(store.getAt(2));

            grid.render(document.body);

            // Should have selected row 2
            expect(view.el.query('.' + view.selectedCls).length).toBe(1);
            expect(isRowSelected(2)).toBe(true);
        });
        it("should allow selection of columns before render", function () {
            makeGrid(null, {
                renderTo: null
            });

            selModel.selectColumn(colRef[2]);

            grid.render(document.body);

            // Should have selected all cells under column 2
            expect(view.el.query('.' + view.selectedCls).length).toBe(view.el.query(colRef[2].cellSelector).length);
            expect(isColumnSelected(2)).toBe(true);
        });
    });

    /* TODO: Reinstate this test after 6.5.0
    smDescribe("Select all", function() {
        itNotTouch("should select all on click of header zero", function() {
            makeGrid();
            var r2c0 = findCell(2, 0);

            jasmine.fireMouseEvent(colRef[0].el.dom, 'click');

            // Should have selected all rows
            expect(view.el.dom.querySelectorAll('.'+view.selectedCls).length).toBe(store.getCount());

            jasmine.fireMouseEvent(colRef[0].el.dom, 'click');

            // Should have deselected all rows
            expect(view.el.query('.'+view.selectedCls).length).toBe(0);

            jasmine.fireMouseEvent(colRef[0].el.dom, 'click');

            // Should have selected all rows
            expect(selModel.getSelection().allSelected).toBe(true);
            expect(view.el.query('.'+view.selectedCls).length).toBe(store.getCount());

            // Confirm that row 2 is selected, then click the rownumberer cell in row 2
            expect(selModel.isSelected(2)).toBe(true);

            // CTRL/click
            Ext.testHelper.tap(r2c0, {ctrlKey: true});

            // Should have deselected row 2
            expect(selModel.getSelection().allSelected).toBe(false);
            expect(view.el.query('.'+view.selectedCls).length).toBe(store.getCount() - 1);
            expect(selModel.isSelected(2)).toBe(false);

            // Now that not all is selected, clicking this should select all again.
            jasmine.fireMouseEvent(colRef[0].el.dom, 'click');

            // Should have selected all cells
            expect(selModel.getSelection().allSelected).toBe(true);
            expect(view.el.query('.'+view.selectedCls).length).toBe(store.getCount());
        });
    });
    */

    smDescribe("Column selection", function () {
        it("should select a column on click of a header", function () {
            makeGrid(null, null, {
                extensible: true
            });
            var spy = spyOnEvent(store, "sort").andCallThrough();

            jasmine.fireMouseEvent(colRef[1].el.dom, 'click');

            // Should have selected all cells under column 1
            expect(view.el.query('.' + view.selectedCls).length).toBe(view.el.query(colRef[1].cellSelector).length);
            expect(isColumnSelected(1)).toBe(true);

            // Extender handle must be visible
            expect(selModel.getExtensible().handle.isVisible()).toBe(true);

            jasmine.fireMouseEvent(colRef[1].el.dom, 'click');

            // Should have deselected all cells
            expect(view.el.query('.' + view.selectedCls).length).toBe(0);

            // Extender handle must be hidden
            expect(selModel.getExtensible().handle.isVisible()).toBe(false);

            // Activating the header as a column select should NOT sort
            expect(spy).not.toHaveBeenCalled();
        });
        it("should select a column on click of a header and deselect previous columns", function () {
            makeGrid();
            var spy = spyOnEvent(store, "sort").andCallThrough();
            jasmine.fireMouseEvent(colRef[1].el.dom, 'click');

            // Should have selected all cells under column 1
            expect(view.el.query('.' + view.selectedCls).length).toBe(view.el.query(colRef[1].cellSelector).length);
            expect(isColumnSelected(1)).toBe(true);

            jasmine.fireMouseEvent(colRef[2].el.dom, 'click');

            // Should have selected all cells under column 2
            expect(view.el.query('.' + view.selectedCls).length).toBe(view.el.query(colRef[2].cellSelector).length);
            expect(isColumnSelected(2)).toBe(true);

            /*
            TODO: Reinstate when grid headers are navigable and may be used to select columns
            jasmine.fireKeyEvent(colRef[2].el.dom, 'keydown', Ext.event.Event.RIGHT);
            jasmine.fireKeyEvent(colRef[3].el.dom, 'keydown', Ext.event.Event.SPACE);

            // Should have selected all cells under column 3
            expect(view.el.query('.'+view.selectedCls).length).toBe(view.el.query(colRef[3].cellSelector).length);
            expect(isColumnSelected(3)).toBe(true);

            // Activating the header as a column select should NOT sort
            expect(spy).not.toHaveBeenCalled();
            */
        });
        itNotTouch("should select a column on CTRL/click of a header and not deselect previous columns", function () {
            makeGrid();
            var spy = spyOnEvent(store, "sort").andCallThrough();
            jasmine.fireMouseEvent(colRef[1].el.dom, 'click');

            // Should have selected all cells under column 1
            expect(view.el.query('.' + view.selectedCls).length).toBe(view.el.query(colRef[1].cellSelector).length);
            expect(isColumnSelected(1)).toBe(true);

            // CTRL/click
            Ext.testHelper.tap(colRef[2].el.dom, {ctrlKey: true});

            // Should have selected all cells under column 2
            expect(view.el.query('.' + view.selectedCls).length).toBe(view.el.query(colRef[1].cellSelector).length + view.el.query(colRef[2].cellSelector).length);
            expect(isColumnSelected(1)).toBe(true);
            expect(isColumnSelected(2)).toBe(true);

            /*
            TODO: Reinstate when grid headers are navigable and may be used to select columns
            jasmine.fireKeyEvent(colRef[2].el.dom, 'keydown', Ext.event.Event.RIGHT);
            jasmine.fireKeyEvent(colRef[3].el.dom, 'keydown', Ext.event.Event.SPACE, false, true);

            // Should have selected all cells under column 3
            expect(view.el.query('.'+view.selectedCls).length).toBe(view.el.query(colRef[1].cellSelector).length + view.el.query(colRef[2].cellSelector).length + view.el.query(colRef[3].cellSelector).length);
            expect(isColumnSelected(1)).toBe(true);
            expect(isColumnSelected(2)).toBe(true);
            expect(isColumnSelected(3)).toBe(true);

            // Activating the header as a column select should NOT sort
            expect(spy).not.toHaveBeenCalled();
            */
        });
        itNotTouch("should allow click/drag selection of columns when cell and row selection is disabled", function () {
            makeGrid(null, null, {
                cells: false,
                rows: false,
                columns: true
            });

            var c2 = findCell(2, 2),
                c3 = findCell(3, 3),
                c4 = findCell(4, 4),
                sel;

            jasmine.fireMouseEvent(c2, 'mousedown');
            jasmine.fireMouseEvent(c2, 'mousemove');
            jasmine.fireMouseEvent(c3, 'mousemove');
            jasmine.fireMouseEvent(c4, 'mousemove');
            jasmine.fireMouseEvent(c4, 'mouseup');

            sel = selModel.getSelection();

            // That swipe from 2,2 to 4,4 should have selected three columns
            expect(sel.isColumns).toBe(true);
            expect(sel.getCount()).toBe(3);
            expect(isColumnSelected(2)).toBe(true);
            expect(isColumnSelected(3)).toBe(true);
            expect(isColumnSelected(4)).toBe(true);

            // Should have selected all cells under column 2, 3 and 4
            expect(view.el.query('.' + view.selectedCls).length).toBe(view.el.query(colRef[2].cellSelector).length + view.el.query(colRef[3].cellSelector).length + view.el.query(colRef[4].cellSelector).length);
        });
        itNotTouch("should not allow click/drag selection when selectable is disabled", function () {
            makeGrid(null, null, {
                disabled: true
            });
            var c2 = findCell(2, 2),
                c4 = findCell(4, 4);

            jasmine.fireMouseEvent(c2, 'mousedown');
            jasmine.fireMouseEvent(c2, 'mousemove');
            jasmine.fireMouseEvent(c4, 'mousemove');
            jasmine.fireMouseEvent(c4, 'mouseup');

            // Should not have selected anything
            expect(view.el.query('.' + view.selectedCls).length).toBe(0);
        });
        itNotTouch("should allow click/SHIFT click selection of columns when cell and row selection is disabled", function () {
            makeGrid(null, null, {
                cells: false,
                rows: false,
                columns: true
            });

            var c2 = findCell(2, 2),
                c4 = findCell(4, 4),
                sel;

            jasmine.fireMouseEvent(c2, 'click');
            Ext.testHelper.tap(c4, {shiftKey: true});

            sel = selModel.getSelection();

            // Click on 2,2, and SHIFT/click on 4,4 will select 3 columns
            expect(sel.isColumns).toBe(true);
            expect(sel.getCount()).toBe(3);
            expect(isColumnSelected(2)).toBe(true);
            expect(isColumnSelected(3)).toBe(true);
            expect(isColumnSelected(4)).toBe(true);

            // Should have selected all cells under column 2, 3 and 4
            expect(view.el.query('.' + view.selectedCls).length).toBe(view.el.query(colRef[2].cellSelector).length + view.el.query(colRef[3].cellSelector).length + view.el.query(colRef[4].cellSelector).length);
        });
        itNotTouch("should allow click/CTRL click selection of columns when cell and row selection is disabled", function () {
            makeGrid(null, null, {
                cells: false,
                rows: false,
                columns: true
            });

            var c2 = findCell(2, 2),
                c4 = findCell(4, 4),
                sel;

            jasmine.fireMouseEvent(c2, 'click');
            Ext.testHelper.tap(c4, {ctrlKey: true});

            sel = selModel.getSelection();

            // Click on 2,2, and SHIFT/click on 4,4 will select columns 2 and 4
            expect(sel.isColumns).toBe(true);
            expect(sel.getCount()).toBe(2);
            expect(isColumnSelected(2)).toBe(true);
            expect(isColumnSelected(4)).toBe(true);

            // Should have selected all cells under column 2, 3 and 4
            expect(view.el.query('.' + view.selectedCls).length).toBe(view.el.query(colRef[2].cellSelector).length + view.el.query(colRef[4].cellSelector).length);
        });

        it('should not select a record when clicking on the row numberer when rows and cells are false', function() {
            makeGrid(null, {
                rowNumbers: true
            }, {
                rows: false,
                cells: false
            });

            clickRowNumberer(0, true);

            // Touch taps are delayed by 1ms to allow focus processing to perform navigation
            waitsFor(function() {
                return isColumnSelected(0) === true;
            });

            runs(function() {
                jasmine.fireKeyEvent(document.activeElement, 'keydown', Ext.event.Event.DOWN);
                expect(isColumnSelected(0)).toBe(true);

                jasmine.fireKeyEvent(document.activeElement, 'keydown', Ext.event.Event.DOWN);
                expect(isColumnSelected(0)).toBe(true);

                jasmine.fireKeyEvent(document.activeElement, 'keydown', Ext.event.Event.DOWN);
                expect(isColumnSelected(0)).toBe(true);
            });
        });

        it('should maintain the visual column selection as the grid scrolls', function() {
            var col4cells, i, col4Selected = true;

            makeGrid(null, null, {
                cells: false,
                rows: false,
                columns: true
            }, null, null, 100);
            colRef[1].hide();

            jasmine.fireMouseEvent(colRef[4].el.dom, 'click');
            col4cells = grid.bodyElement.query('.x-testcell-4', false);
            for (i = 0; col4Selected && i < col4cells.length; i++) {
                if (!col4cells[i].hasCls('x-selected')) {
                    col4Selected = false;
                }
            }
            expect(col4Selected).toBe(true);

            jasmine.waitsForScroll(grid.getScrollable(), function(scroller) {
                if (!grid.renderInfo.atBegin) {
                    return true;
                }
                scroller.scrollBy(0, 100);
            });

            // After the scroll the correct columns must be selected
            runs(function() {
                col4cells = grid.bodyElement.query('.x-testcell-4', false);
                for (i = 0; col4Selected && i < col4cells.length; i++) {
                    if (!col4cells[i].hasCls('x-selected')) {
                        col4Selected = false;
                    }
                }
                expect(col4Selected).toBe(true);
            });
        });
    });

    describe("advanced selection", function () {
        itNotTouch("should allow SHIFT select once you already have items selected", function () {
            makeGrid();

            var c1 = findCell(0, 1),
                c2 = findCell(1, 3),
                c3 = findCell(4, 3),
                sel;

            jasmine.fireMouseEvent(c1, 'click');
            Ext.testHelper.tap(c2, {shiftKey: true});
            Ext.testHelper.tap(c3, {shiftKey: true});

            sel = selModel.getSelection();

            expect(sel.isCells).toBe(true);
            expect(sel.startCell.recordIndex).toBe(0);
            expect(sel.startCell.columnIndex).toBe(1);
            expect(sel.endCell.recordIndex).toBe(4);
            expect(sel.endCell.columnIndex).toBe(3);
        });
    });

    smDescribe("Row selection", function () {
        itNotTouch("should set allSelected if all rows manually selected", function () {
            makeGrid();
            clickRowNumberer(0, true);
            expect(selModel.getSelection().allSelected).toBe(false);
            clickRowNumberer(1, true);
            expect(selModel.getSelection().allSelected).toBe(false);
            clickRowNumberer(2, true);
            expect(selModel.getSelection().allSelected).toBe(false);
            clickRowNumberer(3, true);
            expect(selModel.getSelection().allSelected).toBe(false);
            clickRowNumberer(4, true);
            expect(selModel.getSelection().allSelected).toBe(false);
            clickRowNumberer(5, true);
            expect(selModel.getSelection().allSelected).toBe(false);
            clickRowNumberer(6, true);
            expect(selModel.getSelection().allSelected).toBe(false);
            clickRowNumberer(7, true);
            expect(selModel.getSelection().allSelected).toBe(false);
            var spy = spyOnEvent(grid, 'selectionchange');
            clickRowNumberer(8, true);
            expect(selModel.getSelection().allSelected).toBe(false);
            expect(spy.mostRecentCall.args[0].getSelectable().getSelection().allSelected).toBe(false);
            clickRowNumberer(9, true);
            expect(selModel.getSelection().allSelected).toBe(true);
            expect(spy.mostRecentCall.args[0].getSelectable().getSelection().allSelected).toBe(true);
        });

        it("should select a row on click of a rownumberer", function () {
            makeGrid();
            clickRowNumberer(1);

            // Touch taps are delayed by 1ms to allow focus processing to perform navigation
            // Should select row 1
            waitsFor(function() {
                return view.el.query('.' + view.selectedCls).length === 1 &&
                    isRowSelected(1) === true;
            });

            runs(function() {
                jasmine.fireMouseEvent(colRef[1].el.dom, 'click');
            });

            // Touch taps are delayed by 1ms to allow focus processing to perform navigation
            // Should deselect all rows
            waitsFor(function() {
                return view.el.query('.x-gridrow.' + view.selectedCls).length === 0;
            });
        });
        it("should select a row on click of a rownumberer and deselect previous rows", function () {
            makeGrid();
            clickRowNumberer(1);

            // Touch taps are delayed by 1ms to allow focus processing to perform navigation
            // Should select row 1
            waitsFor(function() {
                return view.el.query('.' + view.selectedCls).length === 1 &&
                    isRowSelected(1) === true;
            });

            runs(function() {
                clickRowNumberer(2);
            });

            // Touch taps are delayed by 1ms to allow focus processing to perform navigation
            // Should select row 2
            waitsFor(function() {
                return view.el.query('.' + view.selectedCls).length === 1 &&
                    isRowSelected(2) === true;
            });

            runs(function() {
                jasmine.fireKeyEvent(findCell(2, 0), 'keydown', Ext.event.Event.DOWN, null, true);
                jasmine.fireKeyEvent(findCell(3, 0), 'keydown', Ext.event.Event.SPACE);

                // Should have selected row 3
                expect(view.el.query('.' + view.selectedCls).length).toBe(1);
                expect(isRowSelected(3)).toBe(true);
            });
        });
        itNotTouch("should select a row on CTRL/click of a rownumberer and not deselect previous rows", function () {
            makeGrid();
            clickRowNumberer(1);

            // Should have selected row 1
            expect(view.el.query('.' + view.selectedCls).length).toBe(1);
            expect(isRowSelected(1)).toBe(true);

            // CTRL/click
            clickRowNumberer(2, true);

            // Should have selected row 2
            expect(view.el.query('.' + view.selectedCls).length).toBe(2);
            expect(isRowSelected(1)).toBe(true);
            expect(isRowSelected(2)).toBe(true);

            // CTRL/DOWN
            jasmine.fireKeyEvent(findCell(2, 0), 'keydown', Ext.event.Event.DOWN, null, true);
            jasmine.fireKeyEvent(findCell(3, 0), 'keydown', Ext.event.Event.SPACE, null, true);

            // Should have selected row 3
            expect(view.el.query('.' + view.selectedCls).length).toBe(3);
            expect(isRowSelected(1)).toBe(true);
            expect(isRowSelected(2)).toBe(true);
            expect(isRowSelected(3)).toBe(true);
        });
        it("should fire the selectionchange event when rows are selected and rowSelect is set to false", function () {
            makeGrid();
            var selectSpy = spyOnEvent(grid, 'select'),
                selChangeSpy;

            clickRowNumberer(1);

            // Touch taps are delayed by 1ms to allow focus processing to perform navigation
            // multi select, must pass an array to select event
            waitsFor(function() {
                return selectSpy.mostRecentCall && Ext.Array.equals(selectSpy.mostRecentCall.args[1], [store.getAt(1)]);
            });

            runs(function() {
                // Should have selected row 1
                expect(view.el.query('.' + view.selectedCls).length).toBe(1);
                expect(isRowSelected(1)).toBe(true);
                selChangeSpy = spyOnEvent(grid, 'selectionchange');

                // Disable row selection.
                selModel.setRows(false);

                // Should have deselected all rows
                expect(view.el.query('.' + view.selectedCls).length).toBe(0);

                // Should have deselected the selection
                expect(selChangeSpy).toHaveBeenCalled();

                // Restore row selection, but in single mode
                selModel.setRows(true);
                selModel.setMode('single');
                clickRowNumberer(1);
            });

            // Touch taps are delayed by 1ms to allow focus processing to perform navigation
            // multi select, must pass an array to select event
            waitsFor(function() {
                return selectSpy.mostRecentCall && Ext.Array.equals(selectSpy.mostRecentCall.args[1], store.getAt(1));
            });

            runs(function() {
                // Should have selected row 1
                expect(view.el.query('.' + view.selectedCls).length).toBe(1);
                expect(isRowSelected(1)).toBe(true);
            });
        });
        // TODO Reinstate when we implement clipboard
        xit("should not copy the rownumberer column", function () {
            makeGrid(null, {
                plugins: 'clipboard'
            });
            var clipboard = grid.findPlugin('clipboard'),
                data;

            clickRowNumberer(1);

            // Should have selected row 1
            expect(view.el.query('.' + view.selectedCls).length).toBe(1);
            expect(isRowSelected(1)).toBe(true);

            data = clipboard.getData(false, {
                text: 1
            });

            // Only the data should be here, NOT the row number
            expect(data.text).toBe('2.1\t2.2\t2.3\t2.4\t2.5');
        });

        // https://sencha.jira.com/browse/EXTJS-19404
        it("should fire selectionchange event only once", function () {
            makeGrid();

            var selChangeSpy = spyOnEvent(grid, 'selectionchange');

            clickRowNumberer(1);

            // We need to wait for potential erroneous multiple calls in
            // an asynchronous handler.
            // Touch taps are delayed by 1ms to allow focus processing to perform navigation
            waitsFor(function() {
                return selChangeSpy.callCount === 1;
            });
        });

        it('should not deselect the row when navigating cell-to-cell in a row', function() {
            makeGrid(null, null, {
                deselectable: true,
                cells: false
            });

            var c2 = findCell(2, 2);

            Ext.testHelper.tap(c2);
            expect(isRowSelected(2));

            jasmine.fireKeyEvent(document.activeElement, 'keydown', Ext.event.Event.RIGHT);
            expect(isRowSelected(2));

            jasmine.fireKeyEvent(document.activeElement, 'keydown', Ext.event.Event.RIGHT);
            expect(isRowSelected(2));

            jasmine.fireKeyEvent(document.activeElement, 'keydown', Ext.event.Event.RIGHT);
            expect(isRowSelected(2));
        });

        describe("checkboxSelect", function () {
            it("should not select the checkbox while dragging before the pointer is out of the checkbox", function () {
                var checkbox;
                makeGrid(null, {
                    selectable: {
                        checkbox: true,
                        showNumbererColumn: true
                    }
                });

                checkbox = findCell(0, 1).querySelector('.x-checkbox-el');
                jasmine.fireMouseEvent(checkbox, 'mousedown');
                jasmine.fireMouseEvent(checkbox, 'mousemove');
                expect(isRowSelected(0)).toBe(false);
                jasmine.fireMouseEvent(checkbox, 'mouseup');
            });
            it("should select all records when checkbox is clicked", function () {
                var checkbox, count;
                makeGrid(null, {
                    selectable: {
                        checkbox: true,
                        showNumbererColumn: true,
                        listeners: {
                            selectionchange: function (sm, selection) {
                                count = selection.getCount();
                            }
                        }
                    }
                });

                checkbox = colRef[1].el.down('.x-checkbox-el');
                jasmine.fireMouseEvent(checkbox, 'click');
                expect(grid.getSelectable().getSelection().getCount()).toBe(grid.getStore().getCount());
                expect(count).toBe(grid.getStore().getCount());
                jasmine.fireMouseEvent(checkbox, 'click');
                expect(grid.getSelectable().getSelection().getCount()).toBe(0);
                expect(count).toBe(0);
            });
            it("should force rows selection", function () {
                var checkbox, count, selectable;

                makeGrid(null, {
                    selectable: {
                        checkbox: false,
                        rows: false,
                        listeners: {
                            selectionchange: function (sm, selection) {
                                count = selection.getCount();
                            }
                        }
                    }
                });

                selectable = grid.getSelectable();
                selectable.setCheckbox(true);
                expect(selectable.getRows()).toBe(true);

                colRef = grid.getVisibleColumns();
                checkbox = colRef[1].el.down('.x-checkbox-el');
                jasmine.fireMouseEvent(checkbox, 'click');
                expect(selectable.getSelection().getCount()).toBe(grid.getStore().getCount());
                expect(count).toBe(grid.getStore().getCount());
                jasmine.fireMouseEvent(checkbox, 'click');
                expect(selectable.getSelection().getCount()).toBe(0);
                expect(count).toBe(0);
            });
            it("should be affected by rows selection", function () {
                var checkbox, count, selectable;

                makeGrid(null, {
                    selectable: {
                        checkbox: true,
                        listeners: {
                            selectionchange: function (sm, selection) {
                                count = selection.getCount();
                            }
                        }
                    }
                });

                selectable = grid.getSelectable();
                colRef = grid.getVisibleColumns();
                checkbox = colRef[1].el.down('.x-checkbox-el');

                jasmine.fireMouseEvent(checkbox, 'click');
                expect(selectable.getSelection().getCount()).toBe(grid.getStore().getCount());
                expect(count).toBe(grid.getStore().getCount());

                selectable.setRows(false);
                expect(selectable.getCheckbox()).toBeFalsy();

                expect(selectable.getSelection().getCount()).toBe(0);
                expect(count).toBe(0);
            });
        });
    });

    describe("Row selection using selectRows", function () {
        it("should select a row and clear previous non-row selections", function () {
            makeGrid();

            // Select a cell rage first.
            // It should drop this selection when we call selectRows
            selModel.selectCells(makeLocation(2, 2), makeLocation(2, 4));

            // Should have selected the 3 cells spanned
            expect(view.el.query('.' + view.selectedCls).length).toBe(3);

            expect(isCellSelected(2, 2) &&
                isCellSelected(2, 3) &&
                isCellSelected(2, 4)).toBe(true);

            selModel.selectRows(store.getAt(1));

            // No cells should be selected
            expect(view.el.query('.x-gridcell.' + view.selectedCls).length).toBe(0);

            // Should have selected row 1
            expect(view.el.query('.' + view.selectedCls).length).toBe(1);
            expect(isRowSelected(1)).toBe(true);

            jasmine.fireMouseEvent(colRef[1].el.dom, 'click');

            // Should have deselected all rows
            expect(view.el.query('.x-gridrow.' + view.selectedCls).length).toBe(0);
        });
        it("should select a row and deselect previous rows", function () {
            makeGrid();

            selModel.selectRows(store.getAt(1));

            // Should have selected row 1
            expect(view.el.query('.' + view.selectedCls).length).toBe(1);
            expect(isRowSelected(1)).toBe(true);

            selModel.selectRows(store.getAt(2));

            // Should have selected row 2
            expect(view.el.query('.' + view.selectedCls).length).toBe(1);
            expect(isRowSelected(2)).toBe(true);
        });
        it("should select a row and not deselect previous rows", function () {
            makeGrid();

            selModel.selectRows(store.getAt(1));

            // Should have selected row 1
            expect(view.el.query('.' + view.selectedCls).length).toBe(1);
            expect(isRowSelected(1)).toBe(true);

            // keepSelection

            selModel.selectRows(store.getAt(2), true);

            // Should have selected row 2
            expect(view.el.query('.' + view.selectedCls).length).toBe(2);
            expect(isRowSelected(1)).toBe(true);
            expect(isRowSelected(2)).toBe(true);
        });
        it("should fire the selectionchange event when rows are selected and rowSelect is set to false", function () {
            makeGrid();
            selModel.selectRows(store.getAt(1));

            // Should have selected row 1
            expect(view.el.query('.' + view.selectedCls).length).toBe(1);
            expect(isRowSelected(1)).toBe(true);

            var selChangeSpy = spyOnEvent(grid, 'selectionchange');

            // Disable row selection.
            selModel.setRows(false);

            // Should have deselected all rows
            expect(view.el.query('.x-gridrow.' + view.selectedCls).length).toBe(0);

            // Should have deselected the selection
            expect(selChangeSpy).toHaveBeenCalled();
        });
        // TODO Reinstate when we implement clipboard
        xit("should not copy the rownumberer column", function () {
            makeGrid(null, {
                plugins: 'clipboard'
            });
            var clipboard = grid.findPlugin('clipboard'),
                data;

            selModel.selectRows(store.getAt(1));

            // Should have selected row 1
            expect(view.el.query('.' + view.selectedCls).length).toBe(1);
            expect(isRowSelected(1)).toBe(true);

            data = clipboard.getData(false, {
                text: 1
            });

            // Only the data should be here, NOT the row number
            expect(data.text).toBe('2.1\t2.2\t2.3\t2.4\t2.5');
        });
    });

    smDescribe("Range selection", function () {
        itNotTouch("should select a range on drag", function () {
            makeGrid();
            var c2 = findCell(2, 2),
                c4 = findCell(4, 4);

            jasmine.fireMouseEvent(c2, 'mousedown');
            jasmine.fireMouseEvent(c2, 'mousemove');
            jasmine.fireMouseEvent(c4, 'mousemove');
            jasmine.fireMouseEvent(c4, 'mouseup');

            // Should have selected the 9 cells spanned
            expect(view.el.query('.' + view.selectedCls).length).toBe(9);

            expect(isCellSelected(2, 2) &&
                isCellSelected(3, 2) &&
                isCellSelected(4, 2) &&
                isCellSelected(2, 3) &&
                isCellSelected(3, 3) &&
                isCellSelected(4, 3) &&
                isCellSelected(2, 4) &&
                isCellSelected(3, 4) &&
                isCellSelected(4, 4)).toBe(true);
        });
        itNotTouch("should select a range in a single row on drag", function () {
            makeGrid();
            var c2 = findCell(2, 2),
                c4 = findCell(2, 4);

            jasmine.fireMouseEvent(c2, 'mousedown');
            jasmine.fireMouseEvent(c2, 'mousemove');
            jasmine.fireMouseEvent(c4, 'mousemove');
            jasmine.fireMouseEvent(c4, 'mouseup');

            // Should have selected the 3 cells spanned
            expect(view.el.query('.' + view.selectedCls).length).toBe(3);

            expect(isCellSelected(2, 2) &&
                isCellSelected(2, 3) &&
                isCellSelected(2, 4)).toBe(true);
        });
        itNotTouch("should work when the mouseup is outside the grid", function () {
            makeGrid();
            var c2 = findCell(2, 2),
                c4 = findCell(2, 4);

            jasmine.fireMouseEvent(c2, 'mousedown');
            jasmine.fireMouseEvent(c2, 'mousemove');
            jasmine.fireMouseEvent(c4, 'mousemove');
            jasmine.fireMouseEvent(document.body, 'mouseup');

            // Should have selected the 3 cells spanned
            expect(view.el.query('.' + view.selectedCls).length).toBe(3);

            expect(isCellSelected(2, 2) &&
                isCellSelected(2, 3) &&
                isCellSelected(2, 4)).toBe(true);
        });
        describe("Range selection using selectCells", function () {
            it("should work when using CellContext objects to describe the range", function () {
                makeGrid();

                selModel.selectCells(makeLocation(2, 2), makeLocation(2, 4));

                // Should have selected the 3 cells spanned
                expect(view.el.query('.' + view.selectedCls).length).toBe(3);

                expect(isCellSelected(2, 2) &&
                    isCellSelected(2, 3) &&
                    isCellSelected(2, 4)).toBe(true);
            });
            it('should work when using [x,y] arrays to describe the range', function () {
                makeGrid();

                selModel.selectCells([2, 2], [2, 4]);

                // Should have selected the 3 cells spanned
                expect(view.el.query('.' + view.selectedCls).length).toBe(3);

                expect(isCellSelected(2, 2) &&
                    isCellSelected(2, 3) &&
                    isCellSelected(2, 4)).toBe(true);
            });
        });

        it('should not wrap when SHIFT+RIGHT on last cell', function () {
            makeGrid();

            var c5 = findCell(2, 5),
                c4Position;

            jasmine.fireMouseEvent(c5, 'click');

            waitsFor(function () {
                c4Position = view.getNavigationModel().getLocation();

                return c4Position && c4Position.getCell(true) === c5;
            }, 'NavigationModel to focus the last cell', 2000);

            runs(function () {
                jasmine.fireKeyEvent(c5, 'keydown', Ext.event.Event.RIGHT, true);
            });

            // We expect nothing to happen
            waits(100);

            // Should not have moved
            runs(function () {
                expect(view.getNavigationModel().getLocation().getCell('dom')).toBe(c5);
            });
        });
    });

    smDescribe("Cell selection maintanance during scroll with derender then rerender", function() {
        itNotTouch("should not show any selected cells of the replicator handle when the selected range is not rendered", function() {
            makeGrid(null, {
                plugins: 'selectionreplicator'
            }, {
                extensible: true
            }, null, null, 100);

            var c2 = findCell(2, 2),
                c4 = findCell(4, 4);

            jasmine.fireMouseEvent(c2, 'mousedown');
            jasmine.fireMouseEvent(c2, 'mousemove');
            jasmine.fireMouseEvent(c4, 'mousemove');
            jasmine.fireMouseEvent(c4, 'mouseup');

            // Should have selected the 9 cells spanned
            expect(view.el.query('.'+view.selectedCls).length).toBe(9);

            expect( isCellSelected(2, 2) &&
                    isCellSelected(3, 2) &&
                    isCellSelected(4, 2) &&
                    isCellSelected(2, 3) &&
                    isCellSelected(3, 3) &&
                    isCellSelected(4, 3) &&
                    isCellSelected(2, 4) &&
                    isCellSelected(3, 4) &&
                    isCellSelected(4, 4)).toBe(true);
            expect(selModel.getExtensible().handle.isVisible()).toBe(true);

            // Scroll until the selected block is just outside the rendered area and
            // The extender handle has been hidden.
            jasmine.waitsForScroll(grid.getScrollable(), function(scroller) {
                if (grid.renderInfo.indexTop > 4 && !selModel.getExtensible().handle.isVisible()) {
                    return true;
                }
                scroller.scrollBy(0, 100);
            });

            // All the selected cells are out of range, none should be selected
            // and the extension handle must be hidden.
            runs(function() {
                expect(view.el.query('.'+view.selectedCls).length).toBe(0);
                expect(selModel.getExtensible().handle.isVisible()).toBe(false);
            });

            // Scroll to end. We must never see any selected cells or the handle
            jasmine.waitsForScroll(grid.getScrollable(), function(scroller) {
                expect(view.el.query('.'+view.selectedCls).length).toBe(0);
                expect(selModel.getExtensible().handle.isVisible()).toBe(false);
                if (grid.renderInfo.indexBottom === 100) {
                    return true;
                }
                scroller.scrollBy(0, 100);
            });

            // Scroll back to the top
            jasmine.waitsForScroll(grid.getScrollable(), function(scroller, x, y) {
                if (grid.renderInfo.indexTop === 0 && y === 0) {
                    return true;
                }
                scroller.scrollBy(0, -100);
            });

            // Selected block and handle should be visible again
            runs(function() {
                expect(view.el.query('.'+view.selectedCls).length).toBe(9);
                expect( isCellSelected(2, 2) &&
                    isCellSelected(3, 2) &&
                    isCellSelected(4, 2) &&
                    isCellSelected(2, 3) &&
                    isCellSelected(3, 3) &&
                    isCellSelected(4, 3) &&
                    isCellSelected(2, 4) &&
                    isCellSelected(3, 4) &&
                    isCellSelected(4, 4)).toBe(true);
                expect(selModel.getExtensible().handle.isVisible()).toBe(true);
            });
        });
    });

    smDescribe("Single cell selection", function() {
        it("should select a single cell on click", function() {
            makeGrid();

            jasmine.fireMouseEvent(findCell(2, 2), 'click');

            // Touch taps are delayed by 1ms to allow focus processing to perform navigation
            waitsFor(function() {
                return view.el.query('.' + view.selectedCls).length === 1 &&
                    isCellSelected(2, 2) === true;
            });

            runs(function() {
                jasmine.fireMouseEvent(findCell(5, 5), 'click');
            });

            // Touch taps are delayed by 1ms to allow focus processing to perform navigation
            waitsFor(function() {
                return view.el.query('.' + view.selectedCls).length === 1 &&
                    isCellSelected(5, 5) === true;
            });
        });
    });

    smDescribe("With editor", function() {
        it("should be able to select the text inside of an editor", function() {
            var c1, plugin, editor, field;
            makeGrid([{
                text : 'Field 1',
                dataIndex : 'field1',
                editor : {
                    xtype : 'textfield'
                }
            },{
                text : 'Field 2',
                dataIndex : 'field2'
            }],{
                plugins: {
                    type: 'cellediting'
                }
            });
            
            c1 = findCell(0, 1);
            jasmine.fireMouseEvent(c1, 'dblclick');

            plugin = grid.findPlugin('cellediting');
            editor = plugin.getActiveEditor();
            field = editor.getField();

            jasmine.fireMouseEvent(field.inputElement, 'mousedown');
            jasmine.fireMouseEvent(field.inputElement, 'mousemove');

            expect(editor.editing).toBe(true);

            jasmine.fireMouseEvent(field.inputElement, 'mouseup');
        });
    });

    xdescribe("pruneRemoved", function() {
        describe("pruneRemoved: true", function() {
            it("should remove records from selection by default when they are removed from the store", function() {
                //columns, cfg, selModelCfg, storeCfg
                makeGrid(null, {
                    bbar: {
                        xtype: 'pagingtoolbar'
                    }
                }, null, {
                    autoLoad: false,
                    pageSize: 5,
                    proxy: {
                        type: 'memory',
                        enablePaging: true
                    }
                });
                store.proxy.enablePaging = true;

                var tb = grid.down('pagingtoolbar'),
                    selection;

                tb.setStore(store);
                store.loadPage(1);
                selModel.select(0);
                selection = selModel.getSelection();

                // We have selected the first record
                expect(selection.length).toBe(1);
                expect(selection[0] === store.getAt(0)).toBe(true);

                // Row zero has the selected class
                expect(Ext.fly(view.getNode(0)).hasCls(view.selectedCls)).toBe(true);

                // Load page 2
                tb.moveNext();

                // First row in new page NOT selected
                expect(Ext.fly(view.getNode(0)).hasCls(view.selectedCls)).toBe(false);

                // Go back to page 1
                tb.movePrevious();
                selection = selModel.getSelection();

                // Selection has gone
                expect(selection.length).toBe(0);

                // Row zero must not be selected
                expect(Ext.fly(view.getNode(0)).hasCls(view.selectedCls)).toBe(false);
            });
        });

        describe("pruneRemoved: false", function() {
            it("should NOT remove records from selection if pruneRemoved:false when they are removed from the store", function() {
                makeGrid(null, {
                    bbar: {
                        xtype: 'pagingtoolbar'
                    }
                }, {
                    pruneRemoved: false
                }, {
                    autoLoad: false,
                    pageSize: 5,
                    proxy: {
                        type: 'memory',
                        enablePaging: true
                    }
                });
                store.proxy.enablePaging = true;

                var tb = grid.down('pagingtoolbar'),
                    selection;

                tb.setStore(store);
                store.loadPage(1);
                selModel.select(0);
                selection = selModel.getSelection();

                // We have selected the first record
                expect(selection.length).toBe(1);
                expect(selection[0] === store.getAt(0)).toBe(true);

                // Row zero has the selected class
                expect(Ext.fly(view.getNode(0)).hasCls(view.selectedCls)).toBe(true);

                // Load page 2
                tb.moveNext();

                // First row in new page NOT selected
                expect(Ext.fly(view.getNode(0)).hasCls(view.selectedCls)).toBe(false);

                // Go back to page 1
                tb.movePrevious();
                selection = selModel.getSelection();

                // We have selected the first record
                expect(selection.length).toBe(1);
                expect(selection[0] === store.getAt(0)).toBe(true);

                // Row zero must be selected
                expect(Ext.fly(view.getNode(0)).hasCls(view.selectedCls)).toBe(true);
            });
        });
    });

    describe("view model selection", function() {
        var viewModel, spy, columns;

        function createGrid(gridCfg, selModelCfg, storeCfg) {
            grid = new Ext.grid.Grid(Ext.apply({
                store: new Ext.data.Store(Ext.apply({
                    fields: ['name'],
                    proxy: {
                        type: 'memory',
                        data: [
                            { name: 'Phil' },
                            { name: 'Ben' },
                            { name: 'Evan' },
                            { name: 'Don' },
                            { name: 'Nige' },
                            { name: 'Alex' }
                        ]
                    }
                }, storeCfg)),
                columns: [
                    { text: 'Name',  dataIndex: 'name' }
                ],
                rowNumbers: true,
                selectable: Ext.apply({
                    drag: true,
                    cells: true,
                    columns: true,
                    rows: true,
                    checkbox: false
                }, selModelCfg),
                height: 200,
                width: 200,
                renderTo: Ext.getBody()
            }, gridCfg));
            selModel = grid.getSelectable();
            store = grid.getStore();
            if (!storeCfg || storeCfg.autoLoad !== false) {
                store.load();
            }
            view = grid;
            columns = grid.getVisibleColumns();

            waits(100);
        }

        beforeEach(function() {
            spy = jasmine.createSpy();
            viewModel = new Ext.app.ViewModel();
        });

        afterEach(function() {
            spy = selModel = viewModel = null;
        });

        function selectNotify(rec) {
            selModel.select(rec);
            viewModel.notify();
        }

        function byName(name) {
            var index = store.findExact('name', name);
            return store.getAt(index);
        }

        describe("reference", function() {
            beforeEach(function() {
                viewModel.bind('{userList.selection}', spy);
                createGrid({
                    reference: 'userList',
                    viewModel: viewModel
                });
                viewModel.notify();
            });

            it("should publish null by default", function() {
                var args = spy.mostRecentCall.args;
                expect(args[0]).toBeNull();
                expect(args[1]).toBeUndefined();
            });

            it("should publish the value when selected", function() {
                var rec = byName('Ben');
                selectNotify(rec);
                var args = spy.mostRecentCall.args;
                expect(args[0]).toBe(rec);
                expect(args[1]).toBeNull();
            });

            it("should publish when the selection is changed", function() {
                var rec1 = byName('Ben'),
                    rec2 = byName('Nige');

                selectNotify(rec1);
                spy.reset();
                selectNotify(rec2);
                var args = spy.mostRecentCall.args;
                expect(args[0]).toBe(rec2);
                expect(args[1]).toBe(rec1);
            });

            it("should publish when an item is deselected", function() {
                var rec = byName('Ben');
                selectNotify(rec);
                spy.reset();
                selModel.deselect(rec);
                viewModel.notify();
                var args = spy.mostRecentCall.args;
                expect(args[0]).toBeNull();
                expect(args[1]).toBe(rec);
            });
        });

        describe("two way binding", function() {
            beforeEach(function() {
                createGrid({
                    viewModel: viewModel,
                    bind: {
                        selection: '{foo}'
                    }
                });
                viewModel.bind('{foo}', spy);
                viewModel.notify();
            });

            describe("changing the selection", function() {
                it("should trigger the binding when adding a selection", function() {
                    var rec = byName('Don');
                    selectNotify(rec);
                    var args = spy.mostRecentCall.args;
                    expect(args[0]).toBe(rec);
                    expect(args[1]).toBeUndefined();
                });

                it("should trigger the binding when changing the selection", function() {
                    var rec1 = byName('Ben'),
                        rec2 = byName('Nige');

                    selectNotify(rec1);
                    spy.reset();
                    selectNotify(rec2);
                    var args = spy.mostRecentCall.args;
                    expect(args[0]).toBe(rec2);
                    expect(args[1]).toBe(rec1);
                });

                it("should trigger the binding when an item is deselected", function() {
                    var rec = byName('Don');
                    selectNotify(rec);
                    spy.reset();
                    selModel.deselect(rec);
                    viewModel.notify();
                    var args = spy.mostRecentCall.args;
                    expect(args[0]).toBeNull();
                    expect(args[1]).toBe(rec);
                });
            });

            describe("changing the viewmodel value", function() {
                it("should select the record when setting the value", function() {
                    var rec = byName('Phil');
                    viewModel.set('foo', rec);
                    viewModel.notify();
                    expect(selModel.isSelected(rec)).toBe(true);
                });

                it("should select the record when updating the value", function() {
                    selModel.setMode('single');

                    var rec1 = byName('Phil'),
                        rec2 = byName('Ben');

                    viewModel.set('foo', rec1);
                    viewModel.notify();
                    viewModel.set('foo', rec2);
                    viewModel.notify();
                    expect(selModel.isSelected(rec1)).toBe(false);
                    expect(selModel.isSelected(rec2)).toBe(true);
                });

                it("should deselect when clearing the value", function() {
                    var rec = byName('Evan');

                    viewModel.set('foo', rec);
                    viewModel.notify();
                    viewModel.set('foo', null);
                    viewModel.notify();
                    expect(selModel.isSelected(rec)).toBe(false);
                });
            });
        });
    });

    xdescribe("Locked grids", function() {
        describe("mouse cell selection", function(){
            itNotTouch("should track across from locked to normal", function() {
                makeGrid(null, null, null, null, true);
                var c1 = findCell(1, 1),
                    c3 = findCell(3, 3);

                jasmine.fireMouseEvent(c1, 'mousedown');
                jasmine.fireMouseEvent(c1, 'mousemove');
                jasmine.fireMouseEvent(c3, 'mousemove');

                expect(selModel.getSelection().isCells).toBe(true);

                // Should have selected the 9 cells spanned
                expect(view.el.query('.'+view.selectedCls).length).toBe(9);

                // Selection object should have a count of 9
                expect(selModel.getSelection().getCount()).toBe(9);

                expect( isCellSelected(1, 1) &&
                    isCellSelected(1, 2) &&
                    isCellSelected(1, 3) &&
                    isCellSelected(2, 1) &&
                    isCellSelected(2, 2) &&
                    isCellSelected(2, 3) &&
                    isCellSelected(3, 1) &&
                    isCellSelected(3, 2) &&
                    isCellSelected(3, 2)).toBe(true);

                jasmine.fireMouseEvent(c3, 'mouseup');
            });
        });

        describe("mouse row selection", function(){
            itNotTouch("should track across from locked to normal", function() {
                makeGrid(null, null, null, null, true);
                var c0 = findCell(0, 0),
                    c2 = findCell(2, 2);

                jasmine.fireMouseEvent(c0, 'mousedown');
                jasmine.fireMouseEvent(c0, 'mousemove');
                jasmine.fireMouseEvent(c2, 'mousemove');

                expect(selModel.getSelection().isRows).toBe(true);

                // Should have selected the 6 rows spanned
                expect(view.el.query('.'+view.selectedCls).length).toBe(6);

                // Rows 0, 1, 2 should be selected
                expect(isRowSelected(0)).toBe(true);
                expect(isRowSelected(1)).toBe(true);
                expect(isRowSelected(2)).toBe(true);

                // Selection object should have a count of 3
                expect(selModel.getSelection().getCount()).toBe(3);

                expect(selModel.getSelection().getSelected().contains(store.getAt(0))).toBe(true);
                expect(selModel.getSelection().getSelected().contains(store.getAt(1))).toBe(true);
                expect(selModel.getSelection().getSelected().contains(store.getAt(2))).toBe(true);

                jasmine.fireMouseEvent(c2, 'mouseup');
            });
            
            itNotTouch('should select a range of rows using click followed by shift+click', function() {
                makeGrid(null, null, null, null, true);
                var c0 = findCell(0, 0),
                    c2 = findCell(2, 0);

                // Click on row 0, then shift+click on row 2
                jasmine.fireMouseEvent(c0, 'click');
                Ext.testHelper.tap(c2, {shiftKey: true});

                expect(selModel.getSelection().isRows).toBe(true);

                // Should have selected the 6 rows spanned
                expect(view.el.query('.'+view.selectedCls).length).toBe(6);

                // Rows 0, 1, 2 should be selected
                expect(isRowSelected(0)).toBe(true);
                expect(isRowSelected(1)).toBe(true);
                expect(isRowSelected(2)).toBe(true);

                // Selection object should have a count of 3
                expect(selModel.getSelection().getCount()).toBe(3);

                expect(selModel.getSelection().getSelected().contains(store.getAt(0))).toBe(true);
                expect(selModel.getSelection().getSelected().contains(store.getAt(1))).toBe(true);
                expect(selModel.getSelection().getSelected().contains(store.getAt(2))).toBe(true);

                // Column selection should destroy the row selection https://sencha.jira.com/browse/EXTJS-17325
                jasmine.fireMouseEvent(colRef[1].el.dom, 'click');

                expect(selModel.getSelection().isColumns).toBe(true);

                // Should have selected all cells under column 1
                expect(view.el.query('.'+view.selectedCls).length).toBe(view.el.query(colRef[1].cellSelector).length);
                expect(isColumnSelected(1)).toBe(true);
            });
        });

        xdescribe("locking a selected column", function() {
            it("should successfully deselect", function() {
                makeGrid(null, null, null, null, true);
                
                selModel.selectColumn(colRef[5]);
                expect(isColumnSelected(5)).toBe(true);

                grid.lock(colRef[5]);
                colRef = grid.getColumnManager().getColumns();

                // After refreshing the columns, column 2 will be the moved column 5.
                // It should still be selected.
                expect(isColumnSelected(2)).toBe(true);
            });
        });
        
        xdescribe("copying selected columns from locked grid", function() {
            // TODO Reinstate when we implement clipboard
            xit("should arrange the column data in column-ordinal order according to the outermost grid", function() {
                makeGrid(null, {
                    plugins: 'clipboard'
                }, null, null, true);
                var clipboard = grid.findPlugin('clipboard'),
                    data;

                // This is column 0 in the normal grid
                selModel.selectColumn(colRef[2]);

                // This is column 1 in the locked grid
                selModel.selectColumn(colRef[1], true);

                // But the clipboard should sort them into the order they are in in the outermost grid
                data = clipboard.getCellData();
                expect(data).toEqual("1.1\t1.2\n2.1\t2.2\n3.1\t3.2\n4.1\t4.2\n5.1\t5.2\n6.1\t6.2\n7.1\t7.2\n8.1\t8.2\n9.1\t9.2\n10.1\t10.2");
            });
        });
    });

    describe("mouse column selection", function() {
        xitNotTouch("should select in both locked and normal sides", function() {
            makeGrid(null, null, null, null, true);

            jasmine.fireMouseEvent(colRef[1].el.dom, 'click');

            expect(selModel.getSelection().isColumns).toBe(true);

            // Should have selected all cells under column 1
            expect(view.el.query('.'+view.selectedCls).length).toBe(view.el.query(colRef[1].cellSelector).length);
            expect(isColumnSelected(1)).toBe(true);

            // CTRL/click
            Ext.testHelper.tap(colRef[2].el, {ctrlKey: true});

            // Selection object should have a count of 2
            expect(selModel.getSelection().getCount()).toBe(2);

            // Should have selected all cells under columns 1 and  2
            expect(view.el.query('.'+view.selectedCls).length).toBe(view.el.query(colRef[1].cellSelector).length + view.el.query(colRef[2].cellSelector).length);

            // Both columns should be selected
            expect(isColumnSelected(1)).toBe(true);
            expect(isColumnSelected(2)).toBe(true);
        });

        itNotTouch('should select columns rage using click then shift+click', function() {
            makeGrid(null, null, null, null, true);

            jasmine.fireMouseEvent(colRef[1].el.dom, 'click');

            expect(selModel.getSelection().isColumns).toBe(true);

            // Should have selected all cells under column 1
            expect(view.el.query('.'+view.selectedCls).length).toBe(view.el.query(colRef[1].cellSelector).length);
            expect(isColumnSelected(1)).toBe(true);

            // SHIFT/click column 4
            Ext.testHelper.tap(colRef[4].el, {shiftKey: true});

            // Selection object should have a count of 4
            expect(selModel.getSelection().getCount()).toBe(4);

            // Should have selected all cells under column 1, 2, 3 and 4
            expect(view.el.query('.'+view.selectedCls).length).toBe(
                    view.el.query(colRef[1].cellSelector).length +
                    view.el.query(colRef[2].cellSelector).length +
                    view.el.query(colRef[3].cellSelector).length +
                    view.el.query(colRef[4].cellSelector).length);

            // All four columns should be selected
            expect(isColumnSelected(1)).toBe(true);
            expect(isColumnSelected(2)).toBe(true);
            expect(isColumnSelected(3)).toBe(true);
            expect(isColumnSelected(4)).toBe(true);
        });
    });

    describe("extensible", function() {
        it("should allow to disable extensible", function() {
            makeGrid(null, {
                plugins: 'selectionreplicator'
            }, {
                extensible: false
            }, {
                numeric: true
            });
            
            jasmine.fireMouseEvent(findCell(1, 0), 'click');

            expect(document.body.querySelectorAll(Ext.baseCSSPrefix + 'ssm-extender-drag-handle').length).toBe(0);
            expect(selModel.getExtensible()).toBeUndefined();
        });
    });

    describe("Selection replication", function() {
        var selStart,
            selEnd,
            extStart,
            extEnd,
            r0Data,
            r1Data,
            r2Data,
            r5Data,
            r6Data,
            r7Data,
            focusEnterSpy;

        beforeEach(function() {
            makeGrid(null, {
                plugins: 'selectionreplicator'
            }, {
                extensible: 'y'
            }, {
                numeric: true
            });
            r0Data = Ext.clone(store.getAt(0).data);
            r1Data = Ext.clone(store.getAt(1).data);
            r2Data = Ext.clone(store.getAt(2).data);
            r5Data = Ext.clone(store.getAt(5).data);
            r6Data = Ext.clone(store.getAt(6).data);
            r7Data = Ext.clone(store.getAt(7).data);
            focusEnterSpy = spyOnEvent(grid, 'focusenter');
        });
        
        it("should align the extend handle upon column resize", function() {
            var handleX;

            // Select cell 1, 1
            jasmine.fireMouseEvent(findCell(1, 1), 'click');

            waitsForSpy(focusEnterSpy);
            
            runs(function() {
                // Get extender handle position
                handleX = selModel.getExtensible().handle.getX();

                // Widen the selected column by 100px
                colRef[1].setWidth(colRef[1].getWidth() + 100);
            });
            waits(100);
            runs(function() {
                // Handle should have moved with it.
                var have = selModel.getExtensible().handle.getX();

                // Need a bit of fuzziness for IE8, Firefox, Safari...
                expect(have).toBeWithin(2, handleX + 100);
            });
        });
        
        it('should hide the replicator when cell has been deselected', function () {
           jasmine.fireMouseEvent(findCell(1, 1), 'click');

           waitsForSpy(focusEnterSpy);
           runs(function () {
               selModel.deselectAll();
               expect(selModel.getExtensible().handle.isVisible()).toBe(false);
           });
           
        });
    
        it('should remove the replicator when a column has been hidden', function () {
            var columns = grid.getVisibleColumns(),
                selChangeSpy = jasmine.createSpy(),
                i;

            selModel.on('selectionchange', selChangeSpy);
            
            // First iteration will hide the column 'field 2' that is to the right of the selection.
            // Second iteration will hide the column 'field 1' with the selection. Second time should
            // remove the replicator handle.
            jasmine.fireMouseEvent(findCell(1, 1), 'click');

            // Touch taps are delayed by 1ms to allow focus processing to perform navigation
            waitsForSpy(selChangeSpy);

            runs(function() {
                for (i = 2; i > 0; i--) {
                    expect(selModel.getExtensible().handle.isVisible()).toBe(true);
                    columns[i].hide();
                    expect(selModel.getExtensible().handle.isVisible()).toBe(i === 2);
                }
            });
        });

        describe("multiple selection", function() {
            describe("upwards", function() {
                itNotTouch("should replicate the selection by incrementing the values", function() {

                    selStart = findCell(3, 2);
                    selEnd = findCell(4, 4);
                    extStart = makeLocation(0, 2);
                    extEnd = makeLocation(2, 4);

                    // Zero the data in our intended extension areas
                    // because the replication sets it back to original values.
                    store.getAt(0).set({field2: 0, field3: 0, field4: 0});
                    store.getAt(1).set({field2: 0, field3: 0, field4: 0});
                    store.getAt(2).set({field2: 0, field3: 0, field4: 0});

                    jasmine.fireMouseEvent(selStart, 'mousedown');
                    jasmine.fireMouseEvent(selStart, 'mousemove');
                    jasmine.fireMouseEvent(selEnd, 'mousemove');
                    jasmine.fireMouseEvent(selEnd, 'mouseup');
                    // 3,2 to 4,4 will be selected now.
                    // This operation is tested by a test above.

                    // Replicate upwards
                    grid.fireEvent('beforeselectionextend', grid, selModel.getSelection(), {
                        type: 'rows',
                        start: extStart,
                        end: extEnd,
                        rows: -3
                    });

                    // The values should be restored to being incremented by one as in the initial load
                    expect(store.getAt(0).data).toEqual(r0Data);
                    expect(store.getAt(1).data).toEqual(r1Data);
                    expect(store.getAt(2).data).toEqual(r2Data);
                });
            });
            describe("downwards", function() {
                itNotTouch("should replicate the selection by incrementing the values", function() {

                    selStart = findCell(3, 2);
                    selEnd = findCell(4, 4);
                    extStart = makeLocation(5, 2);
                    extEnd = makeLocation(7, 4);

                    // Zero the data in our intended extension areas
                    // because the replication sets it back to original values.
                    store.getAt(5).set({field2: 0, field3: 0, field4: 0});
                    store.getAt(6).set({field2: 0, field3: 0, field4: 0});
                    store.getAt(7).set({field2: 0, field3: 0, field4: 0});

                    jasmine.fireMouseEvent(selStart, 'mousedown');
                    jasmine.fireMouseEvent(selStart, 'mousemove');
                    jasmine.fireMouseEvent(selEnd, 'mousemove');
                    jasmine.fireMouseEvent(selEnd, 'mouseup');
                    // 3,2 to 4,4 will be selected now.
                    // This operation is tested by a test above.

                    // Replicate downwards
                    grid.fireEvent('beforeselectionextend', grid, selModel.getSelection(), {
                        type: 'rows',
                        start: extStart,
                        end: extEnd,
                        rows: 3
                    });

                    // The values should be restored to being incremented by one as in the initial load
                    expect(store.getAt(5).data).toEqual(r5Data);
                    expect(store.getAt(6).data).toEqual(r6Data);
                    expect(store.getAt(7).data).toEqual(r7Data);
                });
            });
        });

        describe("single selection", function() {
            describe("upwards", function() {
                it("should replicate the selection by repeating the values", function() {
                    var r3Data = store.getAt(3).data,
                        extStart = makeLocation(0, 2),
                        extEnd = makeLocation(2, 4),
                        selChangeSpy = jasmine.createSpy();

                    selModel.on('selectionchange', selChangeSpy);

                    jasmine.fireMouseEvent(findCell(3,0), 'click');

                    // Touch taps are delayed by 1ms to allow focus processing to perform navigation
                    waitsForSpy(selChangeSpy);

                    runs(function() {
                        // Replicate upwards
                        grid.fireEvent('beforeselectionextend', grid, selModel.getSelection(), {
                            type: 'rows',
                            start: extStart,
                            end: extEnd,
                            rows: -3
                        });

                        // The test is going to insist that the data objects be the same.
                        // So force the IDs here.
                        store.getAt(0).data.id = store.getAt(1).data.id = store.getAt(2).data.id = store.getAt(3).data.id;

                        // The values selected in record 3 should be repeated in the extension area
                        expect(store.getAt(0).data).toEqual(r3Data);
                        expect(store.getAt(1).data).toEqual(r3Data);
                        expect(store.getAt(2).data).toEqual(r3Data);
                    });
                });
            });
            describe("downwards", function() {
                it("should replicate the selection by repeating the values", function() {
                    var r4Data = store.getAt(4).data,
                        extStart = makeLocation(5, 2),
                        extEnd = makeLocation(7, 4),
                        selChangeSpy = jasmine.createSpy();

                    selModel.on('selectionchange', selChangeSpy);

                    jasmine.fireMouseEvent(findCell(4, 0), 'click');
                    // Row 4 will be selected now.
                    // This operation is tested by a test above.

                    // Touch taps are delayed by 1ms to allow focus processing to perform navigation
                    waitsForSpy(selChangeSpy);

                    runs(function() {
                        // Replicate downwards
                        grid.fireEvent('beforeselectionextend', grid, selModel.getSelection(), {
                            type: 'rows',
                            start: extStart,
                            end: extEnd,
                            rows: 3
                        });

                        // The test is going to insist that the data objects be the same.
                        // So force the IDs here.
                        store.getAt(5).data.id = store.getAt(6).data.id = store.getAt(7).data.id = store.getAt(4).data.id;

                        // The values selected in record 4 should be repeated in the extension area
                        expect(store.getAt(5).data).toEqual(r4Data);
                        expect(store.getAt(6).data).toEqual(r4Data);
                        expect(store.getAt(7).data).toEqual(r4Data);
                    });
                });
            });
        });
    });

    describe('extending selection beyond rendered block', function() {
        var oldOnError = window.onerror,
            onErrorSpy,
            selStart,
            selEnd,
            extStart,
            extEnd,
            handle;

        beforeEach(function() {
            makeGrid(null, {
                plugins: 'selectionreplicator'
            }, {
                extensible: 'y'
            }, {
                numeric: true
            }, null, 200);
        });

        afterEach(function() {
            window.onerror = oldOnError;
        });

        itNotTouch("should be able to extend via the extender handle when the start cell has been derendered", function() {
            onErrorSpy = jasmine.createSpy('error handler');
            window.onerror = onErrorSpy.andCallFake(function() {
                if (oldOnError) {
                    oldOnError();
                }
            });

            selStart = findCell(0, 2);
            selEnd = findCell(4, 4);
            extStart = makeLocation(5, 2);
            extEnd = makeLocation(7, 4);

            jasmine.fireMouseEvent(selStart, 'mousedown');
            jasmine.fireMouseEvent(selStart, 'mousemove');
            jasmine.fireMouseEvent(selEnd, 'mousemove');
            jasmine.fireMouseEvent(selEnd, 'mouseup');
            // 3,2 to 4,4 will be selected now.
            // This operation is tested by a test above.

            selEnd = Ext.get(findCell(9, 4));
            handle = selModel.getExtensible().handle;

            jasmine.fireMouseEvent(handle, 'mousedown');
            jasmine.fireMouseEvent(document.body, 'mousemove', selEnd.getX(), selEnd.getY());
            jasmine.fireMouseEvent(document.body, 'mouseup');
            handle = selModel.getExtensible().handle;

            jasmine.waitForScroll(grid.getScrollable(), function(scroller, x, y) {
                scroller.scrollBy(null, handle.getOffsetsTo(grid.bodyElement)[1] - grid.rowHeight + handle.getHeight());
                return true;
            });
            // Wait for handle to be repositioned over the selection end cell
            waitsFor(function() {
                return (handle.getRegion().intersect(selEnd.getRegion()));
            });

            runs(function() {
                selEnd = Ext.get(findCell(19, 4));
                jasmine.fireMouseEvent(handle, 'mousedown');
                jasmine.fireMouseEvent(document.body, 'mousemove', selEnd.getX(), selEnd.getY());
                jasmine.fireMouseEvent(document.body, 'mouseup');
            });

            jasmine.waitForScroll(grid.getScrollable(), function(scroller, x, y) {
                scroller.scrollBy(null, handle.getOffsetsTo(grid.bodyElement)[1] - grid.rowHeight + handle.getHeight());
                return true;
            });
            // Wait for handle to be repositioned over the selection end cell
            waitsFor(function() {
                return (handle.getRegion().intersect(selEnd.getRegion()));
            });

            runs(function() {
                selEnd = Ext.get(findCell(29, 4));
                jasmine.fireMouseEvent(handle, 'mousedown');
                jasmine.fireMouseEvent(document.body, 'mousemove', selEnd.getX(), selEnd.getY());
                jasmine.fireMouseEvent(document.body, 'mouseup');
                expect(onErrorSpy).not.toHaveBeenCalled();
            });

            jasmine.waitForScroll(grid.getScrollable(), function(scroller, x, y) {
                scroller.scrollBy(null, handle.getOffsetsTo(grid.bodyElement)[1] - grid.rowHeight + handle.getHeight());
                return true;
            });
            // Wait for handle to be repositioned over the selection end cell
            waitsFor(function() {
                return (handle.getRegion().intersect(selEnd.getRegion()));
            });

            runs(function() {
                selEnd = Ext.get(findCell(39, 4));
                jasmine.fireMouseEvent(handle, 'mousedown');
                jasmine.fireMouseEvent(document.body, 'mousemove', selEnd.getX(), selEnd.getY());
                jasmine.fireMouseEvent(document.body, 'mouseup');
                expect(onErrorSpy).not.toHaveBeenCalled();
            });
        });
    });

    describe("reconfigure", function() {
        var newColumnSet = [],
            i, visibleColumns;

        beforeEach(function() {
            for (i = 1; i <= 5; ++i) {
                newColumnSet.push({
                    name: 'F' + i,
                    text: 'F' + i,
                    dataIndex: 'field' + i,
                    cell: {
                        cls: 'x-testcell-' + i
                    },
                    cellSelector: '.x-testcell-' + i
                });
            }

            makeGrid(newColumnSet, null, {
                checkbox: true,
                checkboxColumnIndex: 1
            });
            visibleColumns = grid.getVisibleColumns();
        });
        it("should re-insert the checkbox and row numberer columns on reconfigure", function() {
            var columns = grid.getVisibleColumns();

            // There should be the checkbox column and the rpw numberer column in addition to the initial column set
            expect(columns.length).toBe(newColumnSet.length + 2);

            // Should be RowNumberer at column zero, and checkbox obeying checkboxColumnIndex at column 1
            expect(visibleColumns[0].isRowNumberer).toBe(true);
            expect(visibleColumns[1].isCheckColumn).toBe(true);

            grid.setColumns(newColumnSet);
            waits(100);
            runs(function() {
                visibleColumns = grid.getVisibleColumns();

                // There should be the checkbox column and the rpw numberer column in addition to the initial column set
                expect(visibleColumns.length).toBe(newColumnSet.length + 2);

                // Should be RowNumberer at column zero, and checkbox obeying checkboxColumnIndex at column 1
                expect(visibleColumns[0].isRowNumberer).toBe(true);
                expect(visibleColumns[1].isCheckColumn).toBe(true);
            });
        });
    });

    smDescribe('Virtual Store', function() {
        var captured = null,
            spans;

        function getData(start, limit) {
            var end = start + limit,
                recs = [],
                i;

            for (i = start; i < end; ++i) {
                recs.push({
                    id: i + 1,
                    threadid: i + 1,
                    title: 'Title' + (i + 1)
                });
            }
            return recs;
        }

        function satisfyRequests(total) {
            var requests = Ext.Ajax.mockGetAllRequests(),
                empty = total === 0,
                request, params, data;

            while (requests.length) {
                request = requests[0];

                captured.push(request.options.params);

                params = request.options.params;
                data = getData(empty ? 0 : params.start, empty ? 0 : params.limit);

                Ext.Ajax.mockComplete({
                    status: 200,
                    responseText: Ext.encode({
                        total: (total || empty) ? total : 5000,
                        data: data
                    })
                });

                requests = Ext.Ajax.mockGetAllRequests();
            }
        }

        function createStore(cfg) {
            return store = new Ext.data.virtual.Store(Ext.apply({
                model: 'spec.ForumThread',
                pageSize: 100,
                proxy: {
                    type: 'ajax',
                    url: 'fakeUrl',
                    reader: {
                        type: 'json',
                        rootProperty: 'data'
                    }
                }
            }, cfg));
        }

        function createGrid(cfg) {
            cfg = Ext.apply({
                title: 'Virtual Store Grid',
                height: 400,
                width: 600,
                rowNumbers: true,
                columns: [{
                    text: 'Thread Id',
                    dataIndex: 'threadid'
                }, {
                    text: 'Title',
                    dataIndex: 'title'
                }],
                store: createStore(),
                renderTo: document.body,
                selectable: {
                    drag: true,
                    checkbox: true
                }
            });
            grid = view = new Ext.grid.Grid(cfg);

            // Kicks the store into action on first refresh, so wait for that
            waits(100);

            // Now satisfy the requests
            runs(function() {
                satisfyRequests();
            });
        }

        beforeEach(function() {
            Ext.define('spec.ForumThread', {
                extend: 'Ext.data.Model',
                fields: [
                    'title', 'forumtitle', 'forumid', 'username', {
                        name: 'replycount',
                        type: 'int'
                    }, {
                        name: 'lastpost',
                        mapping: 'lastpost',
                        type: 'date',
                        dateFormat: 'timestamp'
                    },
                    'lastposter', 'excerpt', 'threadid'
                ],
                idProperty: 'threadid'
            });

            MockAjaxManager.addMethods();
            captured = [];

            createGrid();
        });

        afterEach(function(){
            MockAjaxManager.removeMethods();
            store.destroy();
            captured = store = null;
            Ext.data.Model.schema.clear();
            Ext.undefine('spec.ForumThread');
        });

        itNotTouch('should be able to select rows by dragging', function() {
            var c00 = findCell(0, 0),
                c10 = findCell(1, 0),
                c20 = findCell(2, 0);

            jasmine.fireMouseEvent(c00, 'mousedown');
            jasmine.fireMouseEvent(c00, 'mousemove');
            jasmine.fireMouseEvent(c10, 'mousemove');
            jasmine.fireMouseEvent(c20, 'mousemove');
            jasmine.fireMouseEvent(c20, 'mouseup');

            expect(view.el.query('.'+view.selectedCls).length).toBe(3);
            expect(isRowSelected(0)).toBe(true);
            expect(isRowSelected(1)).toBe(true);
            expect(isRowSelected(2)).toBe(true);
        });

        itNotTouch('should be able to select and rows using checkbox', function() {
            var c01 = findCell(0, 1).querySelector('.x-checkbox-el'),
                c11 = findCell(1, 1).querySelector('.x-checkbox-el'),
                c21 = findCell(2, 1).querySelector('.x-checkbox-el');

            jasmine.fireMouseEvent(c01, 'click');
            jasmine.fireMouseEvent(c11, 'click');
            jasmine.fireMouseEvent(c21, 'click');

            // Check the state of the spans. Should be a single one
            spans = grid.getSelectable().getSelection().getSelected().spans;
            expect(spans.length).toBe(1);
            expect(spans[0]).toEqual([0, 3]);

            expect(view.el.query('.'+view.selectedCls).length).toBe(3);
            expect(isRowSelected(0)).toBe(true);
            expect(isRowSelected(1)).toBe(true);
            expect(isRowSelected(2)).toBe(true);

            jasmine.fireMouseEvent(c11, 'click');
            expect(view.el.query('.'+view.selectedCls).length).toBe(2);
            expect(isRowSelected(0)).toBe(true);
            expect(isRowSelected(1)).toBe(false);
            expect(isRowSelected(2)).toBe(true);

            // Check the state of the spans. We should have knocked a hole in it.
            spans = grid.getSelectable().getSelection().getSelected().spans;
            expect(spans.length).toBe(2);
            expect(spans[0]).toEqual([0, 1]);
            expect(spans[1]).toEqual([2, 3]);
        });
    });
});
