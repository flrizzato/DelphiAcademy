topSuite("Ext.grid.plugin.RowExpander", ['Ext.grid.Grid', 'Ext.data.ArrayStore', 'Ext.layout.Fit'], function() {
    var panel, store;

    function createGrid(gridCfg, storeCfg, itemConfig) {
        store = new Ext.data.Store(Ext.apply({
            fields: ['name', 'email', 'phone', 'income'],
            data: [
                { 'name': 'Lisa',  'email':'lisa@simpsons.com',  'phone':'555-111-1224'},
                { 'name': 'Bart',  'email':'bart@simpsons.com',  'phone':'555-222-1234'},
                { 'name': 'Homer', 'email':'homer@simpsons.com', 'phone':'555-222-1244'},
                { 'name': 'Marge', 'email':'marge@simpsons.com', 'phone':'555-222-1254'}
            ],
            autoDestroy: true
        }, storeCfg));

        panel = new Ext.grid.Grid(Ext.apply({
            store: store,
            columns: [
                { header: 'Name',  dataIndex: 'name', width: 100 },
                { header: 'Email', dataIndex: 'email', width: 100 },
                { header: 'Phone', dataIndex: 'phone', width: 100 }
            ],
            plugins: ['rowexpander'],
            itemConfig: Ext.apply({
                body: {
                    tpl: 'body for {email}'
                }
            }, itemConfig),
            height: 200,
            width: 400
        }, gridCfg));
    }

    function getRowByPosition(pos) {
        var cell, row;

        cell = Ext.Component.from(panel.bodyElement.query('.x-expandercell')[pos]);
        row = cell.row;

        return row;
    }

    function toggleRowCollapsed(row) {
        if (Ext.isNumber(row)) {
            row = getRowByPosition(row);
        }

        row.toggleCollapsed();
    }

    afterEach(function() {
        Ext.destroy(panel);
        panel = store = null;
    });

    describe('constructor', function () {
        it('should be rendered as the first column', function () {
            createGrid({
                renderTo: Ext.getBody()
            });

            expect(panel.getColumns()[0].getCell().xtype).toBe('expandercell');
        });
    });

    describe('should work', function() {
        it('should display the expanded body when toggled', function() {
            var row;

            createGrid({
                renderTo: Ext.getBody()
            });

            row = getRowByPosition(0);
            toggleRowCollapsed(0);

            expect(Ext.fly(row.el.query('.x-rowbody')[0]).isVisible()).toBe(true);
        });
    });
});
