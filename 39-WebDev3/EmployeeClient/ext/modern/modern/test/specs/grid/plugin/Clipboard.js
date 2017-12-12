/* global Ext, MockAjaxManager, expect, jasmine, spyOn, xit */

topSuite("Ext.grid.plugin.Clipboard",
    ['Ext.grid.Grid', 'Ext.grid.selection.*', 'Ext.grid.plugin.Clipboard'],
    function() {
        var store, clipboard, grid, selectable, record, column, field,
            synchronousLoad = true,
            proxyStoreLoad = Ext.data.ProxyStore.prototype.load,
            loadStore = function() {
                proxyStoreLoad.apply(this, arguments);
                if (synchronousLoad) {
                    this.flushLoad.apply(this, arguments);
                }
                return this;
            };

        function makeGrid(clipboardCfg, gridCfg, storeCfg) {
            store = new Ext.data.Store(Ext.apply({
                fields: ['name', 'email', 'phone'],
                data: [
                    {'name': 'Lisa', 'email': 'lisa@simpsons.com', 'phone': '555-111-1224', 'age': 14},
                    {'name': 'Bart', 'email': 'bart@simpsons.com', 'phone': '555-222-1234', 'age': 12},
                    {'name': 'Homer', 'email': 'homer@simpsons.com', 'phone': '555-222-1244', 'age': 44},
                    {'name': 'Marge', 'email': 'marge@simpsons.com', 'phone': '555-222-1254', 'age': 41}
                ],
                autoDestroy: true
            }, storeCfg));

            clipboard = new Ext.grid.plugin.Clipboard(Ext.merge({}, clipboardCfg));

            grid = new Ext.grid.Grid(Ext.apply({
                columns: [
                    {header: 'Name',  dataIndex: 'name', editor: 'textfield'},
                    {header: 'Email', dataIndex: 'email', flex:1,
                        editor: {
                            xtype: 'textfield',
                            allowBlank: false
                        }
                    },
                    {header: 'Phone', dataIndex: 'phone', editor: 'textfield'},
                    {header: 'Age', dataIndex: 'age', editor: 'textfield'}
                ],
                store: store,
                selectable: {
                    // Disables sorting by header click, though it will be still available via menu
                    columns: true,
                    cells: true,
                    checkbox: true,
                    drag: true,
                    extensible: 'y'
                },
                plugins: [clipboard],
                width: 400,
                height: 400,
                renderTo: Ext.getBody()
            }, gridCfg));

            selectable = grid.getSelectable();
        }

        function clipboardAction(eventName) {
            var key;

            switch (eventName) {
                case "copy" :
                    key = 67;
                    break;
                case "paste" :
                    key = 86;
                    break;
                case "cut" :
                    key = 88;
                    break;
            }

            jasmine.fireKeyEvent(clipboard.getTarget(grid), 'keydown', key, /*shift*/ null, /*ctrl*/ true);
        }


        beforeEach(function() {
            // Override so that we can control asynchronous loading
            Ext.data.ProxyStore.prototype.load = loadStore;

            MockAjaxManager.addMethods();
        });

        afterEach(function() {
            // Undo the overrides.
            Ext.data.ProxyStore.prototype.load = proxyStoreLoad;

            tearDown();
            MockAjaxManager.removeMethods();
        });

        function tearDown() {
            store = clipboard = grid = record = column = field = Ext.destroy(grid);
        }

        describe("Copy to clipboard", function() {
            beforeEach(function() {
                makeGrid();
            });

            it("should copy all cells on selectAll", function() {
                var values;

                spyOn(clipboard, 'getCellData').andCallThrough();
                selectable.selectAll();
                clipboardAction('copy');

                expect(clipboard.getCellData.callCount).toBe(1);
                values = Ext.util.TSV.decode(clipboard.getCellData.calls[0].result);

                expect(values.length).toBe(4);
            });

            it("should cut all cells on selectAll", function() {
                var values;

                spyOn(clipboard, 'getCellData').andCallThrough();
                selectable.selectAll();
                clipboardAction('cut');

                expect(clipboard.getCellData.callCount).toBe(1);
                values = Ext.util.TSV.decode(clipboard.getCellData.calls[0].result);

                expect(values.length).toBe(4);
                expect(store.getAt(0).get('name')).toBeNull();
            });
        });
    });
