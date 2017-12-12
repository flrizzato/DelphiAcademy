topSuite('Ext.grid.plugin.Summary',
    [
        'Ext.grid.Grid',
        'Ext.data.ArrayStore',
        'Ext.data.summary.Count',
        'Ext.data.summary.Sum',
        'Ext.layout.Fit',
        'Ext.app.ViewModel',
        'Ext.app.ViewController'
    ],
function() {
    var panel, store,
        synchronousLoad = true,
        proxyStoreLoad = Ext.data.ProxyStore.prototype.load,
        loadStore, Controller;

    function createGrid (gridCfg, storeCfg) {
        store = new Ext.data.Store(Ext.apply({
            fields: ['name', 'email', 'phone', 'income'],
            data: [
                { 'name': 'Lisa',  'email':'lisa@simpsons.com',  'phone':'555-111-1224', income: 1244.246 },
                { 'name': 'Bart',  'email':'bart@simpsons.com',  'phone':'555-222-1234', income: 3444.985 },
                { 'name': 'Homer', 'email':'homer@simpsons.com', 'phone':'555-222-1244', income: 2474.45 },
                { 'name': 'Marge', 'email':'marge@simpsons.com', 'phone':'555-222-1254', income: 244.745 }
            ],
            groupField: 'name',
            autoDestroy: true
        }, storeCfg));

        panel = new Ext.grid.Grid(Ext.apply({
            store: store,
            columns: [
                { header: 'Income', dataIndex: 'income', width: 100, summary: 'sum', summaryFormatter: 'number("0,000.00")' },
                { header: 'Name',  dataIndex: 'name', width: 100 },
                { header: 'Email', dataIndex: 'email', width: 100 },
                { header: 'Phone', dataIndex: 'phone', width: 100 }
            ],
            plugins: [{
                type: 'gridsummary'
            }],
            height: 200,
            width: 400
        }, gridCfg));
    }

    function getCell (column) {
        var plugin = panel.findPlugin('gridsummary');
        var row = plugin.getRow();

        // Summary plugin buffers calls to syncSummary...
        plugin.flushSyncSummary();

        return row ? row.query('gridcell')[column] : null;
    }

    beforeEach(function () {
        // Override so that we can control asynchronous loading
        loadStore = Ext.data.ProxyStore.prototype.load = function() {
            proxyStoreLoad.apply(this, arguments);
            if (synchronousLoad) {
                this.flushLoad.apply(this, arguments);
            }
            return this;
        };

        Controller = Ext.define('spec.TestController', {
            extend: 'Ext.app.ViewController',
            alias: 'controller.test'
        });
    });

    afterEach(function () {
        // Undo the overrides.
        Ext.data.ProxyStore.prototype.load = proxyStoreLoad;

        Ext.destroy(panel);
        panel = store = null;

        Ext.undefine('spec.TestController');
        Controller = null;
        Ext.Factory.controller.instance.clearCache();
    });

    describe('grids', function () {
        it('should show the correct value in the cell', function () {
            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100, summary: 'count'
                }],
                renderTo: Ext.getBody()
            });

            var cell = getCell(0);
            expect(cell.el.down('.x-body-el', true).innerHTML).toBe('4');
        });

        it('should apply the formatter correctly', function () {
            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100, summary: 'sum',
                    summaryFormatter: 'number("0,000.00")'
                }],
                renderTo: Ext.getBody()
            });

            var cell = getCell(0);
            expect(cell.el.down('.x-body-el', true).innerHTML).toBe('7,408.43');
        });

        it('should apply the scoped formatter correctly', function () {
            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100, summary: 'sum',
                    summaryFormatter: 'this.myTest("0,000.00")',
                    scope: {
                        myTest: function(v, format){
                            return Ext.util.Format.number(v, format);
                        }
                    }
                }],
                renderTo: Ext.getBody()
            });

            var cell = getCell(0);
            expect(cell.el.down('.x-body-el', true).innerHTML).toBe('7,408.43');
        });

        it('should apply the renderer correctly', function () {
            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100, summary: 'sum',
                    summaryRenderer: Ext.util.Format.numberRenderer("0,000.00")
                }],
                renderTo: Ext.getBody()
            });

            var cell = getCell(0);
            expect(cell.el.down('.x-body-el', true).innerHTML).toBe('7,408.43');
        });

        it('should apply the scoped renderer correctly', function () {
            createGrid({
                columns: [{
                    header: 'Income', dataIndex: 'income', width: 100, summary: 'sum',
                    summaryRenderer: 'myTest',
                    scope: {
                        myTest: function(v){
                            return Ext.util.Format.number(v, '0,000.00');
                        }
                    }
                }],
                renderTo: Ext.getBody()
            });

            var cell = getCell(0);
            expect(cell.el.down('.x-body-el', true).innerHTML).toBe('7,408.43');
        });
    });
});
