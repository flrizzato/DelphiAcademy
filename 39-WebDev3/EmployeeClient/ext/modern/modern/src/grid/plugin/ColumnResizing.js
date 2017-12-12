/**
 * The Column Resizing plugin allows users to adjust the width of the grid columns to suit
 * their needs.  This functionality can be included by requiring the plugin and adding
 * it to your grid's plugins object.
 *
 *     @example
 *     var store = Ext.create('Ext.data.Store', {
 *         data: [
 *             { "name": "Lisa", "email": "lisa@simpsons.com", "phone": "555-111-1224" },
 *             { "name": "Bart", "email": "bart@simpsons.com", "phone": "555-222-1234" },
 *             { "name": "Homer", "email": "home@simpsons.com", "phone": "555-222-1244" },
 *             { "name": "Marge", "email": "marge@simpsons.com", "phone": "555-222-1254" }
 *         ]
 *     });
 *
 *     Ext.create('Ext.grid.Grid', {
 *         fullscreen: true,
 *         layout: 'fit',
 *         store: store,
 *         plugins: {
 *             columnresizing: true
 *         },
 *         columns: [{
 *             text: "Name",
 *             dataIndex: "name",
 *             flex: 1
 *         },
 *         {
 *             text: "Email",
 *             dataIndex: "email",
 *             flex: 1
 *         },
 *         {
 *             text: "Phone",
 *             dataIndex: "phone",
 *             flex: 1
 *         }]
 *     });
 *
 */
Ext.define('Ext.grid.plugin.ColumnResizing', {
    extend: 'Ext.Component',

    alias: ['plugin.columnresizing', 'plugin.gridcolumnresizing'],

    config: {
        grid: null,

        /**
         * @cfg {Boolean} realtime
         * When true the whole column will resize in real-time as the user drags. When false only the header will resize
         * until the interaction is done.
         */
        realtime: false
    },

    hasResizingCls: Ext.baseCSSPrefix + 'has-columnresizing',
    resizingCls: Ext.baseCSSPrefix + 'resizing',
    columnSelector: '.' + Ext.baseCSSPrefix + 'gridcolumn',
    resizerSelector: '.' + Ext.baseCSSPrefix + 'gridcolumn .' + Ext.baseCSSPrefix + 'resizer-el',

    init: function (grid) {
        this.setGrid(grid);
        grid.getHeaderContainer().setTouchAction({ panX: false });
    },

    updateGrid: function (grid, oldGrid) {
        var me = this,
            cls = me.hasResizingCls,
            headerContainer, resizeMarker;

        if (oldGrid) {
            headerContainer = oldGrid.getHeaderContainer();

            headerContainer.renderElement.un({
                touchstart: 'onContainerTouchStart',
                scope: me,
                priority: 100
            });

            oldGrid.removeCls(cls);
        }

        if (grid) {
            me._resizeMarker = resizeMarker = grid.resizeMarkerElement;
            me._resizeMarkerParent = resizeMarker.parent();

            headerContainer = grid.getHeaderContainer();
            headerContainer.renderElement.on({
                touchstart: 'onContainerTouchStart',
                scope: me
            });

            grid.addCls(cls);
        }
    },

    onContainerTouchStart: function (e) {
        var me = this,
            target = e.getTarget(me.columnSelector),
            resizer = e.getTarget(me.resizerSelector),
            column;

        if (resizer && !e.multitouch && target && !me._resizeColumn) {
            column = Ext.Component.from(target);

            if (column && column.getResizable()) {
                me._startColumnWidth = column.getComputedWidth();
                me._minColumnWidth = column.getMinWidth();
                me._maxColumnWidth = column.getMaxWidth();
                me._resizeColumn = column;
                me._startX = e.getX();
                column.addCls(me.resizingCls);
                // Prevent drag and longpress gestures being triggered by this mousedown
                e.claimGesture();

                if (!this.getRealtime()) {
                    me._resizeMarker.show();
                    me._resizeMarker.setLeft(column.el.getOffsetsTo(me._resizeMarkerParent)[0] + me._startColumnWidth);
                } else {
                    column.setWidth(me._startColumnWidth);
                }
                me.touchListeners = Ext.getBody().on({
                    touchEnd: 'onTouchEnd',
                    touchMove: 'onTouchMove',
                    scope: me,
                    destroyable: true
                });
            }
        } else if (e.multitouch && me._resizeColumn) {
            me.endResize();
        }
    },

    onTouchMove: function (e) {
        // Single touch only
        if (e.isMultitouch) {
            this.endResize();
            return;
        }

        if (this._resizeColumn) {
            var column = this._resizeColumn,
                resizeAmount = e.getX() - this._startX;

            if (column) {
                this.currentColumnWidth = Math.max(Math.ceil(this._startColumnWidth + resizeAmount), this._minColumnWidth);
                if (this._maxColumnWidth) {
                    this.currentColumnWidth = Math.min(this.currentColumnWidth, this._maxColumnWidth);
                }

                if (this.getRealtime()) {
                    column.setWidth(this.currentColumnWidth);
                    column.renderElement.setWidth(this.currentColumnWidth);
                } else {
                    this._resizeMarker.setLeft(column.el.getOffsetsTo(this._resizeMarkerParent)[0] + this.currentColumnWidth);
                }

                e.claimGesture();
            }
        }
    },

    onTouchEnd: function (e) {
        var column = this._resizeColumn,
            hasResized = e.getX() !== this._startX;

        Ext.destroy(this.touchListeners);
        if (column) {
            this.endResize();

            // Mouse/touch down then up means a tap on the resizer
            if (!hasResized) {
                column.onResizerTap(e);
            }
        }
    },

    endResize: function () {
        var me = this,
            column = me._resizeColumn,
            grid = me.getGrid();

        if (column) {
            if (!me.getRealtime()) {
                grid.resizeMarkerElement.hide();
            }
            if (me.currentColumnWidth) {
                column.setFlex(null);
                column.setWidth(me.currentColumnWidth);
            }
            column.removeCls(me.resizingCls);
            me._resizeColumn = null;
        }
    }
});
