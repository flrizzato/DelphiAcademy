/**
 * A simple header component for grouped grids.  Grid row headers are created automatically
 * by {@link Ext.grid.Grid Grids} and should not be directly instantiated.
 */
Ext.define('Ext.grid.RowHeader', {
    extend: 'Ext.dataview.ItemHeader',
    xtype: 'rowheader',
    classCls: Ext.baseCSSPrefix + 'rowheader',

    isRowHeader: true,

    toolDefaults: {
        ui: 'itemheader rowheader'
    },

    privates: {
        augmentToolHandler: function (tool, args) {
            // args = [ itemHeader, tool, ev ]   ==>   [ grid, info ]
            this.callParent([tool, args]);

            var info = args[1];

            info.grid = info.list;
        },

        getGroupHeaderTplData: function () {
            var data = this.callParent([ /*skipHtml=*/true ]),
                grid = this.parent,
                column = data && grid.getColumnForField(data.groupField);

            if (column) {
                data.columnName = column.getText();

                if (column.printValue) {
                    data.html = column.printValue(data.value);
                }
            }
            else if (data) {
                data.html = Ext.htmlEncode(data.name);
            }

            return data;
        }
    }
});
