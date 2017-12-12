/* global Ext, expect */

topSuite("Ext.grid.column.RowNumberer",
    ['Ext.grid.Grid', 'Ext.data.ArrayStore', 'Ext.layout.Fit',
     'Ext.app.ViewModel', 'Ext.app.ViewController', 'Ext.data.TreeStore', 'Ext.grid.Tree'],
function() {
    var panel, store, container,
        synchronousLoad = true,
        proxyStoreLoad = Ext.data.ProxyStore.prototype.load,
        loadStore;

    function doItemMouseover(item) {
        var targetEl = item.ariaEl,
            x, y;

        if (jasmine.supportsTouch) {
            x = targetEl.getX() + targetEl.getWidth() / 2;
            y = targetEl.getY() + targetEl.getHeight() / 2;

            Ext.testHelper.touchStart(targetEl, { x: x, y: y });
            Ext.testHelper.touchEnd(targetEl, { x: x, y: y });
        } else {
            jasmine.fireMouseEvent(targetEl, 'mouseover');
        }
    }

    function createGrid(gridCfg, storeCfg, rowNumbererConfig) {
        store = new Ext.data.Store(Ext.apply({
            fields: ['name', 'email', 'phone'],
            data: [
                { 'name': 'Lisa',  'email':'lisa@simpsons.com',  'phone':'555-111-1224'  },
                { 'name': 'Bart',  'email':'bart@simpsons.com',  'phone':'555-222-1234'  },
                { 'name': 'Homer', 'email':'homer@simpsons.com', 'phone':'555-222-1244'  },
                { 'name': 'Marge', 'email':'marge@simpsons.com', 'phone':'555-222-1254'  }
            ],
            autoDestroy: true
        }, storeCfg));

        panel = new Ext.grid.Grid(Ext.apply({
            store: store,
            columns: [
                Ext.apply({ xtype: 'rownumberer'}, rowNumbererConfig),
                { text: 'Name',  dataIndex: 'name', width: 100 },
                { text: 'Email', dataIndex: 'email', width: 100 },
                { text: 'Phone', dataIndex: 'phone', width: 100 }
            ],
            height: 200,
            width: 400
        }, gridCfg));
    }

    function createTree(treeCfg, storeCfg) {
        store = new Ext.data.TreeStore(Ext.apply({
            root: {
                expanded: true,
                children: [{
                    text: 'detention',
                    leaf: true
                }, {
                    text: 'homework',
                    expanded: true,
                    children: [{
                        text: 'book report',
                        leaf: true
                    }, {
                        text: 'algebra',
                        leaf: true
                    }]
                }, {
                    text: 'buy lottery tickets',
                    leaf: true
                }]
            }
        }, storeCfg));

        panel = new Ext.grid.Tree(Ext.apply({
            width: 200,
            height: 150,
            store: store,
            rootVisible: false,
            hideHeaders: true,
            columns: [{
                xtype: 'rownumberer'
            }, {
                text: 'Data',
                dataIndex: 'text',
                flex: 1
            }]
        }, treeCfg));
    }
    
    function getCell(row, column) {
        if (typeof row === 'number') {
            row = panel.itemFromRecord(row);
        }
        return row.getCellByColumn(panel.getColumns()[column]);
    }

    beforeEach(function() {
        // Override so that we can control asynchronous loading
        loadStore = Ext.data.ProxyStore.prototype.load = function() {
            proxyStoreLoad.apply(this, arguments);
            if (synchronousLoad) {
                this.flushLoad.apply(this, arguments);
            }
            return this;
        };
    });
    
    afterEach(function() {
        // Undo the overrides.
        Ext.data.ProxyStore.prototype.load = proxyStoreLoad;

        Ext.destroy(panel);
        panel = store = null;
    });

    describe('grids', function () {
        it('should create numbered rows', function () {
            createGrid({
                renderTo: Ext.getBody()
            });

            var cell = getCell(0,0);
            expect(cell.el).toHaveCls('x-rownumberercell');

            expect(cell.el.down('.x-body-el', true).innerHTML).toBe('1');

            cell = getCell(1,0);
            expect(cell.el.down('.x-body-el', true).innerHTML).toBe('2');
        });

        it("should be able to survive a full row update", function() {
            createGrid();
            var rec = store.first();
            rec.set('name', 'Foo');
            expect(function() {
                rec.commit();
            }).not.toThrow();
        });

        it('should be able to measure styled text', function() {
            var data = [],
                i, rowNumber, cellBody;

            for (i = 0; i < 100; i++) {
                rowNumber = i + 1;
                data.push({
                    name: 'Name ' + rowNumber,
                    email: 'name' + rowNumber + '@foo.com',
                    name: '555 123 ' + rowNumber
                });
            }
            Ext.util.CSS.createStyleSheet('.row-numberer-test {font-size:20px}', 'row-numberer-test');
            createGrid({
                renderTo: Ext.getBody()
            }, {
                data: data
            }, {
                cell: {
                    cls: 'row-numberer-test'
                }
            });

            // Scroll to the end to expose the overflowing cell "100"
            jasmine.waitsForScroll(panel.getScrollable(), function(scroller, x, y) {
                scroller.scrollBy(0, 10000);
                return panel.renderInfo.atEnd && panel;
            });

            runs(function() {
                cellBody = getCell(99, 0).bodyElement.dom;
            });

            // The RowNumberer column must eliminate overflow in its own asynchronous time.
            waitsFor(function() {
                return cellBody.offsetWidth >= cellBody.scrollWidth;
            }, 1000, 'RowNumberer to calculate the correct column width');
        });

        it('should not show in columns menu', function () {
            createGrid({
                renderTo: Ext.getBody()
            });

            var columns = panel.getColumns(),
                column = columns[1],
                menuItem = panel.getColumnsMenuItem(),
                menu = menuItem.getMenu();

            Ext.testHelper.tap(column.triggerElement);

            waitsFor(function () {
                return menuItem.rendered;
            });

            runs(function () {
                doItemMouseover(menuItem);
            });

            waitsFor(function () {
                return menu.rendered;
            });

            runs(function () {
                var items = menu.getItems();

                expect(items.length).toBe(3);
                expect(items.getAt(0).getText()).toBe('Name');
            });
        });
    });

    describe('trees', function () {
        it('should create numbered rows', function () {
            createTree({
                renderTo: Ext.getBody()
            });

            expect(getCell(0, 0).el).toHaveCls('x-rownumberercell');
            expect(getCell(0, 0).el.down('.x-body-el', true).innerHTML).toBe('1');
            expect(getCell(1, 0).el.down('.x-body-el', true).innerHTML).toBe('2');
        });
    });
});
