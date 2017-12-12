topSuite("Ext.grid.column.Column",
    ['Ext.grid.Grid', 'Ext.data.ArrayStore', 'Ext.layout.Fit', 'Ext.app.ViewModel'],
function() {
    var grid, store,
        synchronousLoad = true,
        proxyStoreLoad = Ext.data.ProxyStore.prototype.load,
        loadStore;

    function createGrid(gridCfg, storeCfg) {
        store = new Ext.data.Store(Ext.apply({
            fields: ['name', 'email', 'phone', 'income'],
            data: [
                { name: 'Lisa',  email:'lisa@simpsons.com',  phone:'555-111-1224', income: 1244.246 },
                { name: 'Bart',  email:'bart@simpsons.com',  phone:'555-222-1234', income: 3444.985 },
                { name: 'Homer', email:'homer@simpsons.com', phone:'555-222-1244', income: 2474.45 },
                { name: 'Marge', email:'marge@simpsons.com', phone:'555-222-1254', income: 244.745 },
                { name: 'Kid', email:'kid@simpsons.com', phone:'555-222-1254', income: 0 }
            ],
            autoDestroy: true
        }, storeCfg));

        grid = new Ext.grid.Grid(Ext.apply({
            store: store,
            columns: [
                { header: 'Income', dataIndex: 'income', width: 100, formatter: 'number("0,000.00")' },
                { header: 'Name',  dataIndex: 'name', width: 100 },
                { header: 'Email', dataIndex: 'email', width: 100 },
                { header: 'Phone', dataIndex: 'phone', width: 100 }
            ],
            height: 200,
            width: 400
        }, gridCfg));
    }

    function getCell(row, column, query) {
        query = query || 'gridcell';
        return grid.getItem(store.getAt(row)).query(query)[column];
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

        grid = store = Ext.destroy(grid);
    });

    describe("binding", function() {
        it("should be able to bind column properties", function() {
            createGrid({
                renderTo: Ext.getBody(),
                viewModel: {
                    data: {
                        theName: 'Foo'
                    }
                },
                columns: [{
                    dataIndex: 'name',
                    itemId: 'col',
                    bind: {
                        text: '{theName}'
                    }
                }]
            });
            grid.getViewModel().notify();
            var col = grid.down('#col');
            expect(col.getInnerHtmlElement()).hasHTML('Foo');
        });
    });

    describe('grids', function () {
        it('should show the correct value in the cell', function () {
            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100
                }],
                renderTo: Ext.getBody()
            });

            expect(getCell(0, 0).el.down('.x-body-el', true).innerHTML).toBe('1244.246');
            expect(getCell(1, 0).el.down('.x-body-el', true).innerHTML).toBe('3444.985');
            expect(getCell(2, 0).el.down('.x-body-el', true).innerHTML).toBe('2474.45');
            expect(getCell(3, 0).el.down('.x-body-el', true).innerHTML).toBe('244.745');
        });

        it('should show the correct zeroValue in the gridcell', function () {
            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100,
                    cell: {
                        zeroValue: 'zero'
                    }
                }],
                renderTo: Ext.getBody()
            });

            expect(getCell(4, 0).el.down('.x-body-el', true).innerHTML).toBe('zero');
        });

        it('should show the correct zeroValue in the textcell', function () {
            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100,
                    cell: {
                        xtype: 'textcell',
                        zeroValue: 'zero'
                    }
                }],
                renderTo: Ext.getBody()
            });

            expect(getCell(4, 0, 'textcell').el.down('.x-body-el', true).innerHTML).toBe('zero');
        });

        it('should apply the zeroValue of gridcell correctly from a VM', function () {
            var vm = new Ext.app.ViewModel({
                data: {
                    zeroValue: 'zero'
                }
            });

            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100,
                    cell: {
                        viewModel: vm,
                        bind: {
                            zeroValue: '{zeroValue}'
                        }
                    }
                }],
                renderTo: Ext.getBody()
            });

            vm.notify();
            expect(getCell(4, 0).el.down('.x-body-el', true).innerHTML).toBe('zero');
        });

        it('should apply the zeroValue of textcell correctly from a VM', function () {
            var vm = new Ext.app.ViewModel({
                data: {
                    zeroValue: 'zero'
                }
            });

            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100,
                    cell: {
                        xtype: 'textcell',
                        viewModel: vm,
                        bind: {
                            zeroValue: '{zeroValue}'
                        }
                    }
                }],
                renderTo: Ext.getBody()
            });

            vm.notify();
            expect(getCell(4, 0, 'textcell').el.down('.x-body-el', true).innerHTML).toBe('zero');
        });

        it('should apply the formatter correctly', function () {
            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100,
                    formatter: 'number("0,000.00")'
                }],
                renderTo: Ext.getBody()
            });

            expect(getCell(0, 0).el.down('.x-body-el', true).innerHTML).toBe('1,244.25');
            expect(getCell(1, 0).el.down('.x-body-el', true).innerHTML).toBe('3,444.99');
            expect(getCell(2, 0).el.down('.x-body-el', true).innerHTML).toBe('2,474.45');
            expect(getCell(3, 0).el.down('.x-body-el', true).innerHTML).toBe('244.75');
        });

        it('should apply the scoped formatter correctly', function () {
            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100,
                    formatter: 'this.myTest("0,000.00")',
                    scope: {
                        myTest: function(v, format){
                            return Ext.util.Format.number(v, format);
                        }
                    }
                }],
                renderTo: Ext.getBody()
            });

            expect(getCell(0, 0).el.down('.x-body-el', true).innerHTML).toBe('1,244.25');
            expect(getCell(1, 0).el.down('.x-body-el', true).innerHTML).toBe('3,444.99');
            expect(getCell(2, 0).el.down('.x-body-el', true).innerHTML).toBe('2,474.45');
            expect(getCell(3, 0).el.down('.x-body-el', true).innerHTML).toBe('244.75');
        });

        it('should apply the cell formatter correctly from a VM', function () {
            var vm = new Ext.app.ViewModel({
                data: {
                    formatter: 'number("0,000")'
                }
            });

            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100,
                    cell: {
                        viewModel: vm,
                        bind: {
                            formatter: '{formatter}'
                        }
                    }
                }],
                renderTo: Ext.getBody()
            });

            vm.notify();
            expect(getCell(0, 0).el.down('.x-body-el', true).innerHTML).toBe('1,244');
            expect(getCell(1, 0).el.down('.x-body-el', true).innerHTML).toBe('3,445');
            expect(getCell(2, 0).el.down('.x-body-el', true).innerHTML).toBe('2,474');
            expect(getCell(3, 0).el.down('.x-body-el', true).innerHTML).toBe('245');
        });

        it('should apply the renderer correctly', function () {
            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100,
                    renderer: Ext.util.Format.numberRenderer("0,000.00")
                }],
                renderTo: Ext.getBody()
            });

            expect(getCell(0, 0).el.down('.x-body-el', true).innerHTML).toBe('1,244.25');
            expect(getCell(1, 0).el.down('.x-body-el', true).innerHTML).toBe('3,444.99');
            expect(getCell(2, 0).el.down('.x-body-el', true).innerHTML).toBe('2,474.45');
            expect(getCell(3, 0).el.down('.x-body-el', true).innerHTML).toBe('244.75');
        });

        it('should apply the scoped renderer correctly', function () {
            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100,
                    renderer: 'myTest',
                    scope: {
                        myTest: function(v){
                            return Ext.util.Format.number(v, '0,000.00');
                        }
                    }
                }],
                renderTo: Ext.getBody()
            });

            expect(getCell(0, 0).el.down('.x-body-el', true).innerHTML).toBe('1,244.25');
            expect(getCell(1, 0).el.down('.x-body-el', true).innerHTML).toBe('3,444.99');
            expect(getCell(2, 0).el.down('.x-body-el', true).innerHTML).toBe('2,474.45');
            expect(getCell(3, 0).el.down('.x-body-el', true).innerHTML).toBe('244.75');
        });

        it('should apply the cell renderer correctly from a VM', function () {
            var vm = new Ext.app.ViewModel({
                data: {
                    renderer: Ext.util.Format.numberRenderer("0,000")
                }
            });

            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100,
                    cell: {
                        viewModel: vm,
                        bind: {
                            renderer: '{renderer}'
                        }
                    }
                }],
                renderTo: Ext.getBody()
            });

            vm.notify();
            expect(getCell(0, 0).el.down('.x-body-el', true).innerHTML).toBe('1,244');
            expect(getCell(1, 0).el.down('.x-body-el', true).innerHTML).toBe('3,445');
            expect(getCell(2, 0).el.down('.x-body-el', true).innerHTML).toBe('2,474');
            expect(getCell(3, 0).el.down('.x-body-el', true).innerHTML).toBe('245');
        });

        it('should apply the template correctly without dataIndex', function () {
            createGrid({
                columns: [{
                    header: 'Income', width: 100,
                    tpl: '{income:number("0,000.00")}'
                }],
                renderTo: Ext.getBody()
            });

            expect(getCell(0, 0).el.down('.x-body-el', true).innerHTML).toBe('1,244.25');
            expect(getCell(1, 0).el.down('.x-body-el', true).innerHTML).toBe('3,444.99');
            expect(getCell(2, 0).el.down('.x-body-el', true).innerHTML).toBe('2,474.45');
            expect(getCell(3, 0).el.down('.x-body-el', true).innerHTML).toBe('244.75');
        });

        it('should apply the scoped template correctly without dataIndex', function () {
            createGrid({
                columns: [{
                    header: 'Income', width: 100,
                    tpl: [
                        '{income:this.myTest("0,000.00")}',
                        {
                            myTest: function(v, format){
                                return Ext.util.Format.number(v, format);
                            }
                        }
                    ]
                }],
                renderTo: Ext.getBody()
            });

            expect(getCell(0, 0).el.down('.x-body-el', true).innerHTML).toBe('1,244.25');
            expect(getCell(1, 0).el.down('.x-body-el', true).innerHTML).toBe('3,444.99');
            expect(getCell(2, 0).el.down('.x-body-el', true).innerHTML).toBe('2,474.45');
            expect(getCell(3, 0).el.down('.x-body-el', true).innerHTML).toBe('244.75');
        });

        it('should apply the template correctly with dataIndex', function () {
            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100,
                    tpl: '{income:number("0,000.00")}'
                }],
                renderTo: Ext.getBody()
            });

            expect(getCell(0, 0).el.down('.x-body-el', true).innerHTML).toBe('1,244.25');
            expect(getCell(1, 0).el.down('.x-body-el', true).innerHTML).toBe('3,444.99');
            expect(getCell(2, 0).el.down('.x-body-el', true).innerHTML).toBe('2,474.45');
            expect(getCell(3, 0).el.down('.x-body-el', true).innerHTML).toBe('244.75');
        });

        it('should apply the scoped template correctly with dataIndex', function () {
            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100,
                    tpl: [
                        '{income:this.myTest("0,000.00")}',
                        {
                            myTest: function (v, format) {
                                return Ext.util.Format.number(v, format);
                            }
                        }
                    ]
                }],
                renderTo: Ext.getBody()
            });

            expect(getCell(0, 0).el.down('.x-body-el', true).innerHTML).toBe('1,244.25');
            expect(getCell(1, 0).el.down('.x-body-el', true).innerHTML).toBe('3,444.99');
            expect(getCell(2, 0).el.down('.x-body-el', true).innerHTML).toBe('2,474.45');
            expect(getCell(3, 0).el.down('.x-body-el', true).innerHTML).toBe('244.75');
        });

        it('should apply the cell template correctly from a VM', function () {
            var vm = new Ext.app.ViewModel({
                data: {
                    template: [
                        '{income:this.myTest("0,000")}',
                        {
                            myTest: function (v, format) {
                                return Ext.util.Format.number(v, format);
                            }
                        }
                    ]
                }
            });

            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100,
                    cell: {
                        viewModel: vm,
                        bind: {
                            tpl: '{template}'
                        }
                    }
                }],
                renderTo: Ext.getBody()
            });

            vm.notify();
            expect(getCell(0, 0).el.down('.x-body-el', true).innerHTML).toBe('1,244');
            expect(getCell(1, 0).el.down('.x-body-el', true).innerHTML).toBe('3,445');
            expect(getCell(2, 0).el.down('.x-body-el', true).innerHTML).toBe('2,474');
            expect(getCell(3, 0).el.down('.x-body-el', true).innerHTML).toBe('245');
        });

    });

    describe("editor", function() {
        var col;

        function makeGridWithCol(colCfg, gridCfg) {
            gridCfg = gridCfg || {};
            gridCfg.columns = [colCfg];
            createGrid(gridCfg);
            col = grid.down('gridcolumn');
        }

        afterEach(function() {
            col = null;
        });

        it("should be null by default", function() {
            makeGridWithCol({});
            expect(col.getEditor()).toBeNull();
        });

        it("should accept a string type", function() {
            makeGridWithCol({
                editor: 'textfield'
            });
            expect(col.getEditor().$className).toBe('Ext.field.Text');
        });

        it("should accept a config", function() {
            makeGridWithCol({
                editor: {
                    xtype: 'numberfield',
                    placeholder: 'foo'
                }
            });
            expect(col.getEditor().$className).toBe('Ext.field.Number');
            expect(col.getEditor().getPlaceholder()).toBe('foo');
        });

        describe("type inference", function() {
            function makeInferGrid(type) {
                makeGridWithCol({
                    dataIndex: 'field',
                    editor: {}
                }, {
                    store: {
                        fields: [{ name: 'field', type: type}]
                    }
                });
            }

            it("should infer the type for a date field", function() {
                makeInferGrid('date');
                expect(col.getDefaultEditor().xtype).toBe('datefield');
            });

            it("should infer the type for a boolean field", function() {
                makeInferGrid('bool');
                expect(col.getDefaultEditor().xtype).toBe('checkboxfield');
            });

            it("should infer the type for a bool field", function() {
                makeInferGrid('bool');
                expect(col.getDefaultEditor().xtype).toBe('checkboxfield');
            });

            it("should infer the type for an integer field", function() {
                makeInferGrid('integer');
                expect(col.getDefaultEditor().xtype).toBe('numberfield');
                expect(col.getDefaultEditor().decimals).toBe(0);
            });

            it("should infer the type for an int field", function() {
                makeInferGrid('int');
                expect(col.getDefaultEditor().xtype).toBe('numberfield');
                expect(col.getDefaultEditor().decimals).toBe(0);
            });

            it("should infer the type for a number field", function() {
                makeInferGrid('number');
                expect(col.getDefaultEditor().xtype).toBe('numberfield');
                expect(col.getDefaultEditor().decimals).toBeUndefined();
            });

            it("should infer the type for a float field", function() {
                makeInferGrid('float');
                expect(col.getDefaultEditor().xtype).toBe('numberfield');
                expect(col.getDefaultEditor().decimals).toBeUndefined();
            });

            it("should infer the type for a string field", function() {
                makeInferGrid('string');
                expect(col.getDefaultEditor().xtype).toBe('textfield');
            });

            it("should infer the type for an auto field", function() {
                makeInferGrid('auto');
                expect(col.getDefaultEditor().xtype).toBe('textfield');
            });

            it("should default to text when no field can be found", function() {
                makeGridWithCol({
                    dataIndex: 'notInTheStore',
                    editor: {}
                });
                expect(col.getDefaultEditor().xtype).toBe('textfield');
            });
        });
    });

    describe('add non-columns', function () {
        var column, component;

        function doTest () {
            var renderTarget = column.getRenderTarget();

            /**
             * Items are rendered under the header el which contains
             * the text and trigger elements.
             */
            expect(column.el.indexOf(column.headerElement)).toBe(0);
            expect(column.el.indexOf(renderTarget)).toBe(1);

            expect(renderTarget.indexOf(component.el)).toBe(0);
            expect(column.indexOf(component)).toBe(0);
        }

        it('should allow adding items via config', function () {
            createGrid({
                renderTo: Ext.getBody(),
                columns: [{
                    header: 'Income',
                    dataIndex: 'income',
                    items: [{
                        xytype: 'component',
                        itemId: 'component'
                    }]
                }]
            });

            column = grid.getColumns()[0];
            component = column.getComponent('component')

            doTest();
        });

        it('should allow adding non-column items', function () {
            createGrid({
                renderTo: Ext.getBody(),
                columns: [{
                    header: 'Income',
                    dataIndex: 'income'
                }]
            });

            column = grid.getColumns()[0];
            component = column.add({
                xtype: 'container'
            });

            doTest();
        });

        it('should allow inserting non-column items', function () {
            createGrid({
                renderTo: Ext.getBody(),
                columns: [{
                    header: 'Income',
                    dataIndex: 'income'
                }]
            });

            column = grid.getColumns()[0];
            component = new Ext.Component();

            column.insert(0, component);

            doTest();
        });
    });
});
