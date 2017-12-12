/**
 * Grids are an excellent way of showing large amounts of tabular data on the client side.
 * Essentially a supercharged `<table>`, Grid makes it easy to fetch, sort and filter large
 * amounts of data.
 *
 * Grids are composed of two main pieces - a {@link Ext.data.Store Store} full of data and
 * a set of columns to render.
 *
 * ## A Basic Grid
 *
 *     var store = Ext.create('Ext.data.Store', {
 *         fields: ['name', 'email', 'phone'],
 *         data: [
 *             { 'name': 'Lisa',  "email":"lisa@simpsons.com",  "phone":"555-111-1224"  },
 *             { 'name': 'Bart',  "email":"bart@simpsons.com",  "phone":"555-222-1234" },
 *             { 'name': 'Homer', "email":"home@simpsons.com",  "phone":"555-222-1244"  },
 *             { 'name': 'Marge', "email":"marge@simpsons.com", "phone":"555-222-1254"  }
 *         ]
 *     });
 *
 *     Ext.create('Ext.grid.Grid', {
 *         title: 'Simpsons',
 *
 *         store: store,
 *
 *         columns: [
 *             { text: 'Name',  dataIndex: 'name', width: 200 },
 *             { text: 'Email', dataIndex: 'email', width: 250 },
 *             { text: 'Phone', dataIndex: 'phone', width: 120 }
 *         ],
 *
 *         height: 200,
 *         layout: 'fit',
 *         fullscreen: true
 *     });
 *
 * The code above produces a simple grid with three columns. We specified a Store which will
 * load JSON data inline. In most apps we would be placing the grid inside another container
 * and wouldn't need to provide the {@link #height}, {@link #width} and 
 * {@link #cfg-fullscreen} options but they are included here to for demonstration.
 *
 * The grid we created above will contain a header bar with a title ('Simpsons'), a row of
 * column headers directly underneath and finally the grid rows under the headers.
 *
 * ## Columns
 *
 * By default, each {@link Ext.grid.column.Column column} is sortable and toggles between
 * ascending and descending sorting when you click on its header. There are several basic
 * configs that can be applied to columns to change these behaviors. For example:
 *
 *     columns: [
 *         {
 *             text: 'Name',
 *             dataIndex: 'name',
 *             sortable: false,  // column cannot be sorted
 *             width: 250
 *         },
 *         {
 *             text: 'Email',
 *             dataIndex: 'email',
 *             hidden: true  // column is initially hidden
 *         },
 *         {
 *             text: 'Phone',
 *             dataIndex: 'phone',
 *             width: 100
 *         }
 *     ]
 *
 * We turned off sorting on the 'Name' column so clicking its header now has no effect. We
 * also made the Email column hidden by default (it can be shown again by using the
 * {@link Ext.grid.plugin.ViewOptions ViewOptions} plugin). See the
 * {@link Ext.grid.column.Column column class} for more details.
 *
 * A top-level column definition may contain a `columns` configuration. This means that the 
 * resulting header will be a group header, and will contain the child columns.
 *
 * ## Rows and Cells
 *
 * Grid extends the `{@link Ext.dataview.List List}` component and connects records in the
 * store to `{@link Ext.grid.Row row components}` for the list's items. The Row component
 * utilizes the configs of the grid's {@link Ext.grid.column.Column columns} to create the
 * appropriate type of {@link Ext.grid.cell.Base cells}. Essentially, a Row is a container
 * for {@link Ext.Widget Cell widgets}.
 *
 * For the most part, configuring a grid is about configuring the columns and their cells.
 * There are several built-in column types to display specific types of data:
 *
 *  - {@link Ext.grid.column.Boolean} for true/false values.
 *  - {@link Ext.grid.column.Date} for date/time values.
 *  - {@link Ext.grid.column.Number} for numeric values.
 *
 * These columns specify (via their {@link Ext.grid.column.Column#cell cell config}) one
 * of these basic cell widget types:
 *
 *  - {@link Ext.grid.cell.Boolean}
 *  - {@link Ext.grid.cell.Date}
 *  - {@link Ext.grid.cell.Number}
 *
 * In addition to the above basic cell types, there are two other useful cell types to
 * know about:
 *
 *  - {@link Ext.grid.cell.Text} is the base class for the boolean, date and number cell
 *    classes. It is useful when a cell contains only text.
 *  - {@link Ext.grid.cell.Widget} is a cell class that manages a single child item (either
 *    a {@link Ext.Component component} or a {@link Ext.Widget widget}). The child item is
 *    configured using the `{@link Ext.grid.cell.Widget#widget widget config}`. The most
 *    important part of this config is the `xtype` of the child item to create.
 *
 * ## Cells and Binding
 *
 * One technique to controll cell content and styling is to use data binding to target
 * cell configs like {@link Ext.grid.cell.Base#cls} and {@link Ext.grid.cell.Base#bodyCls}.
 * This is done by assigning a {@link Ext.app.ViewModel viewModel} to each Row like so:
 *
 *      itemConfig: {
 *          viewModel: true  // create default ViewModel for each item (i.e., Row)
 *      }
 *
 * Now that each Row has a ViewModel, cells can bind to the fields of the associated record
 * like so:
 *
 *      columns: [{
 *          ...
 *          cell: {
 *              bind: {
 *                  cls: '{record.someCls}'
 *              }
 *          }
 *      }]
 *
 * The "record" property in the ViewModel is managed by the Row. As Row instances are
 * recycled due to buffered rendering, the associated record instance simply changes over
 * time.
 *
 * ### Cell Widgets
 *
 * When using {@link Ext.grid.cell.Widget}, the contained widgets can also use binding to
 * configure themsleves using properties of the associated record.
 *
 *      columns: [{
 *          ...
 *          cell: {
 *              xtype: 'widgetcell',
 *              widget: {
 *                  xtype: 'button',
 *                  bind: {
 *                      text: 'Update {record.firstName}'
 *                  }
 *              }
 *          }
 *      }]
 *
 * ### Row ViewModels
 *
 * In some cases a custom ViewModel could be useful, for example to provide useful values
 * via {@link Ext.app.ViewModel#formulas formulas}.
 *
 *      itemConfig: {
 *          viewModel: {
 *              type: 'rowViewModel'
 *          }
 *      }
 *
 * ## Renderers and Templates
 *
 * Columns provide two other mechanisms to format their cell content:
 *
 *  - {@link Ext.grid.column.Column#renderer}
 *  - {@link Ext.grid.column.Column#tpl}
 *
 * These column configs are processed by the {@link Ext.grid.column.Cell default cell type}
 * for a column. These configs have some downsides compared to data binding but are provided
 * for compatibility with previous releases.
 *
 *  - Renderers and templates must update the cell content when _any_ field changes. They
 *    cannot assume that only changes to the dataIndex will affect the rendering. Using
 *    data binding, only the configs affected by the changed data will be updated.
 *  - Updates are processed synchronously in response to the record update notification.
 *    Contrast to ViewModels which provide a buffered update mechanism.
 *  - Constructing HTML blocks in code (even in a template) is a common cause of security
 *    problems such as XSS attacks.
 *
 * ## Sorting & Filtering
 *
 * Every grid is attached to a {@link Ext.data.Store Store}, which provides multi-sort and
 * filtering capabilities. It's easy to set up a grid to be sorted from the start:
 *
 *     var myGrid = Ext.create('Ext.grid.Panel', {
 *         store: {
 *             fields: ['name', 'email', 'phone'],
 *             sorters: ['name', 'phone']
 *         },
 *         columns: [
 *             { text: 'Name',  dataIndex: 'name' },
 *             { text: 'Email', dataIndex: 'email' }
 *         ]
 *     });
 *
 * Sorting at run time is easily accomplished by simply clicking each column header. If you
 * need to perform sorting on more than one field at run time it's easy to do so by adding
 * new sorters to the store:
 *
 *     myGrid.store.sort([
 *         { property: 'name',  direction: 'ASC' },
 *         { property: 'email', direction: 'DESC' }
 *     ]);
 *
 * See {@link Ext.data.Store} for examples of filtering.
 *
 * ## Plugins
 *
 * Grid supports addition of extra functionality through plugins:
 *
 * - {@link Ext.grid.plugin.ViewOptions ViewOptions} - adds the ability to show/hide
 *  columns and reorder them.
 *
 * - {@link Ext.grid.plugin.ColumnResizing ColumnResizing} - allows for the ability to
 *  resize columns.
 *
 * - {@link Ext.grid.plugin.Editable Editable} - editing grid contents one row at a time.
 *
 * - {@link Ext.grid.plugin.RowOperations RowOperations} - selecting and performing tasks
 *  on severalrows at a time (e.g. deleting them).
 *
 * - {@link Ext.grid.plugin.PagingToolbar PagingToolbar} - adds a toolbar at the bottom of
 *   the grid that allows you to quickly navigate to another page of data.
 *
 * - {@link Ext.grid.plugin.SummaryRow SummaryRow} - adds and pins an additional row to the
 *   top of the grid that enables you to display summary data.
 */
Ext.define('Ext.grid.Grid', {
    extend: 'Ext.dataview.List',
    xtype: 'grid',

    isGrid: true,

    requires: [
        'Ext.TitleBar',
        'Ext.grid.NavigationModel',
        'Ext.grid.Row',
        'Ext.grid.column.Column',
        'Ext.grid.column.Date',
        'Ext.grid.column.Template',
        'Ext.grid.menu.*',
        'Ext.grid.HeaderContainer',
        'Ext.grid.selection.*',
        'Ext.grid.plugin.ColumnResizing'
    ],

    mixins: [
        'Ext.mixin.ConfigProxy'
    ],

    storeEventListeners: {
        sort: 'onStoreSort'
    },

    config: {
        /**
         * @cfg {Ext.grid.column.Column[]} columns (required)
         * An array of column definition objects which define all columns that appear in this grid.
         * Each column definition provides the header text for the column, and a definition of where
         * the data for that column comes from.
         *
         * This can also be a configuration object for a {Ext.grid.header.Container HeaderContainer}
         * which may override certain default configurations if necessary. For example, the special
         * layout may be overridden to use a simpler layout, or one can set default values shared
         * by all columns:
         *
         *      columns: {
         *          items: [
         *              {
         *                  text: "Column A"
         *                  dataIndex: "field_A",
         *                  width: 200
         *              },{
         *                  text: "Column B",
         *                  dataIndex: "field_B",
         *                  width: 150
         *              },
         *              ...
         *          ]
         *      }
         *
         */
        columns: null,

        /**
         * @cfg {Object} columnMenu
         * This is a config object which is used by columns in this grid to create their
         * header menus.
         *
         * The default column menu contains the following items.
         *
         * - A "Sort Ascending" menu item
         * - A "Sort Descending" menu item
         * - A Columns menu item with each of the columns in a sub-menu of check items
         *   that is used to hide or show each column.
         * - A "Group by this field" menu item to enable grouping.
         * - A "Show in groups" check menu item to enable/disable grouping.
         *
         * These items have {@link #cfg!weight} of `-100`, `-90` and `-80` respectively to
         * place them at the start of the menu.
         *
         * This can be configured as `null` to prevent columns from showing a column menu.
         */
        columnMenu: {
            xtype: 'menu',
            weighted: true,
            align: 'tl-bl?',
            hideOnParentHide: false,  // Persists when owning Column is hidden
            items: {
                sortAsc: {
                    xtype: 'gridsortascmenuitem',
                    group: 'sortDir',
                    weight: -100 // Wants to be the first
                },
                sortDesc: {
                    xtype: 'gridsortdescmenuitem',
                    group: 'sortDir',
                    weight: -90 // Wants to be the second
                },
                //---------------------------------
                // Columns menu is inserted here
                //---------------------------------
                groupByThis: {
                    xtype: 'gridgroupbythismenuitem',
                    handler: 'column.onGroupByThis',
                    separator: true,
                    weight: -70
                },
                showInGroups: {
                    xtype: 'gridshowingroupsmenuitem',
                    handler: 'column.onToggleShowInGroups',
                    weight: -60
                }
            }
        },

        /**
         * @cfg {Boolean} columnResize
         * Set to `false` to disable column resizing within this grid.
         */
        columnResize: true,

        headerContainer: {
            xtype: 'headercontainer'
        },

        /**
         * @cfg {Boolean} hideHeaders
         * `true` to hide the grid column headers.
         *
         * @since 6.0.1
         */
        hideHeaders: false,

        /**
         * @hide
         * Grid Rows are not focusable. Cells are focusable.
         */
        itemsFocusable: false,

        /**
         * @cfg {String} title
         * The title that will be displayed in the TitleBar at the top of this Grid.
         */
        title: '',

        titleBar: {
            xtype: 'titlebar',
            docked: 'top'
        },

        /**
         * @cfg {Boolean} sortable
         * Configure as `false` to disable column sorting via clicking the header and via
         * the Sorting menu items.
         */
        sortable: true,

        /**
         * @cfg {Boolean} multiColumnSort
         * Configure as `true` to have columns retain their sorted state after other
         * columns have been clicked upon to sort.
         *
         * As subsequent columns are clicked upon, they become the new primary sort key.
         *
         * Clicking on an already sorted column which is *not* the primary sort key does
         * not toggle its direction. Analogous to bringing a window to the top by
         * clicking it, this makes that column's field the primary sort key. Subsequent
         * clicks then toggle it.
         *
         * Clicking on a primary key column toggles `ASC` -> `DESC` -> no sorter.
         *
         * The column sorting menu items may be used to toggle the direction without
         * affecting the sorter priority.
         *
         * The maximum number of sorters allowed in a Store is configurable via its
         * underlying data collection. See {@link Ext.util.Collection#multiSortLimit}
         */
        multiColumnSort: false,

        /**
         * @cfg {Ext.grid.menu.Columns} columnsMenuItem
         * The config object for the grid's column hide/show menu
         */
        columnsMenuItem: {
            lazy: true,
            $value: {
                xtype: 'gridcolumnsmenu',
                weight: -80,
                separator: true
            }
        },

        /**
         * @cfg {Boolean} [columnLines=false]
         * Configure as `true` to display lines between grid cells.
         */
        columnLines: null,

        /**
         * @cfg {Boolean/Object} [rowNumbers=false]
         * Configure as `true` to a {@link Ext.grid.column.RowNumberer row numberer} column which gravitates to the
         * start of the grid.
         *
         * May be a {@link Ext.grid.column.RowNumberer} configuration object. For instance to set the column title use
         *
         *     rowNumbers: {
         *         text: 'Index'
         *     }
         */
        rowNumbers: null
    },

    /**
     * @cfg {Object/Ext.grid.Row} itemConfig
     * The object is used to configure the {@link Ext.grid.Row rows) created by this Grid.
     *
     * An `xtype` property may be included to specify a user-supplied subclass of {@link Ext.grid.Row}.
     *
     * See the {@link Ext.grid.row#cfg!body} and {@link Ext.grid.row#cfg!expandedField} configs on
     * the {@link Ext.grid.RowRow class} to easily add extra content to grid rows.
     *
     * Be aware that if you specify a {@link Ext.grid.row#cfg!body row body}, you must
     * configure the owning grid with `{@link #variableHeights}: true`.
     */
    itemConfig: {
        xtype: 'gridrow'
    },

    /**
     * @cfg groupHeader
     * @inheritdoc
     */
    groupHeader: {
        xtype: 'rowheader'
    },

    /**
     * @cfg infinite
     * @inheritdoc
     */
    infinite: true,

    // The type of navigationMode to create
    navigationModel: 'grid',

    /**
     * @cfg pinnedHeader
     * @inheritdoc
     */
    pinnedHeader: {
        xtype: 'rowheader'
    },

    /**
     * @cfg scrollable
     * @inheritdoc
     */
    scrollable: true,

    /**
     * @cfg scrollToTopOnRefresh
     * @inheritdoc
     */
    scrollToTopOnRefresh: false,

    /**
     * @cfg striped
     * @inheritdoc
     */
    striped: true,

    // Our reserveScrollbar config is propagated down to the headerContainer
    proxyConfig: {
        headerContainer: [
            /**
             * @cfg {Boolean} [reserveScrollbar=false]
             * *only meaningful on platforms which has space-consuming scroll bars*
             *
             * Configure as `true` to leave space for a scrollbar to appear even if the content does not
             * overflow.
             *
             * This is useful for trees which may expand and collapse causing visual flickering
             * when scrollbars appear or disappear.
             */
            'reserveScrollbar'
        ]
    },

    /**
     * @cfg {Object} selectable
     * A configuration object which allows passing of configuration options to create or
     * reconfigure a {@link Ext.dataview.selection.Model selection model}.
     *
     * May contain the following options:
     *
     *     - mode `'single'`, `'multi'` Allow selection of only a single or multiple *records*.
     *     This is only valid when selecting {@link #cfg!rows}.
     *     - deselectable Configure as false to disallow deselecting down to zero selected *records*.
     *     This is only valid when selecting {@link #cfg!rows}.
     *     - drag `true` or `false` to allow drag gestures to swipt a rage of cells or rows.
     *     - columns `true` to enable column selection by clicking on headers. Defaults to `false`
     *     - cells `true` to enable cell selection by clicking or dragging on cells. Defaults to `false`
     *     - rows Set to `false` to disable selecting rows. Defaults to `true`
     *     - checkbox `true` to add a checkbox column to display selected state. `'only'` to indicate
     *     that only clicks on the checkbox affect row selected state.
     *     - extensible `true` to enable the selection to be extended either in the `X` or `Y` axis
     *     or `'x'` or `'y'` to configure
     */

    /**
     * @event columnadd
     * Fires whenever a column is added to the Grid.
     * @param {Ext.grid.Grid} this The Grid instance.
     * @param {Ext.grid.column.Column} column The added column.
     * @param {Number} index The index of the added column.
     */
    
    /**
     * @event columnmove
     * Fires whenever a column is moved in the grid.
     * @param {Ext.grid.Grid} this The Grid instance.
     * @param {Ext.grid.column.Column} column The moved column.
     * @param {Number} fromIndex The index the column was moved from.
     * @param {Number} toIndex The index the column was moved to.
     */

    /**
     * @event columnremove
     * Fires whenever a column is removed from the Grid.
     * @param {Ext.grid.Grid} this The Grid instance.
     * @param {Ext.grid.column.Column} column The removed column.
     */

    /**
     * @event columnshow
     * Fires whenever a column is shown in the Grid.
     * @param {Ext.grid.Grid} this The Grid instance.
     * @param {Ext.grid.column.Column} column The shown column.
     */

    /**
     * @event columnhide
     * Fires whenever a column is hidden in the Grid.
     * @param {Ext.grid.Grid} this The Grid instance.
     * @param {Ext.grid.column.Column} column The shown column.
     */

    /**
     * @event columnresize
     * Fires whenever a column is resized in the Grid.
     * @param {Ext.grid.Grid} this The Grid instance.
     * @param {Ext.grid.column.Column} column The resized column.
     * @param {Number} width The new column width.
     */

    /**
     * @event columnsort
     * Fires whenever a column is sorted in the Grid.
     * @param {Ext.grid.Grid} this The Grid instance.
     * @param {Ext.grid.column.Column} column The sorted column.
     * @param {String} direction The direction of the sort on this Column. Either 'asc' or 'desc'.
     */

    /**
     * @event cellselection
     * Fires when cell selection is being used and cells are selected or deselected.
     * @param {Ext.grid.Grid} grid this Grid
     * @param {Ext.grid.selection.Rows} selection An object which encapsulates the selected cell range(s).
     */

    /**
     * @event columnselection
     * Fires when column selection is being used and columns are selected or deselected.
     * @param {Ext.grid.Grid} grid this Grid
     * @param {Ext.grid.selection.Columns} selection An object which encapsulates the selected columns.
     */

    /**
     * @private
     * @readonly
     * @property {String} [selectionModel=grid]
     * The selection model type to create. Defaults to `'grid'` for grids.
     */
    selectionModel: 'grid',

    /**
     * @property classCls
     * @inheritdoc
     */
    classCls: Ext.baseCSSPrefix + 'grid',
    columnLinesCls: Ext.baseCSSPrefix + 'column-lines',

    getTemplate: function() {
        var template = this.callParent();

        template.push({
            reference: 'resizeMarkerElement',
            className: Ext.baseCSSPrefix + 'resize-marker-el',
            hidden: true
        });

        return template;
    },

    beforeInitialize: function() {
        // In a locking grid assembly, child grids will have an ownerGrid reference.
        // By default, in a non-locking grid, ownerGrid references this grid.
        this.ownerGrid = this;
        this.callParent();
    },

    initialize: function() {
        var me = this,
            titleBar = me.getTitleBar(),
            headerContainer = me.getHeaderContainer(),
            scroller = me.getScrollable(),
            selectable = me.getSelectable();

        me.callParent();

        if (scroller) {
            headerContainer.getScrollable().addPartner(scroller, 'x');
        }

        if (titleBar) {
            me.insert(0, titleBar);
        }

        me.add(headerContainer);

        if (selectable) {
            selectable.onViewCreated(me);
        }
    },

    addColumn: function(column) {
        return this.getHeaderContainer().add([column])[0];
    },

    beforeShowColumnMenu: function (column, menu) {
        return this.fireEvent('beforeshowcolumnmenu', this, column, menu);
    },

    doDestroy: function() {
        this.destroyMembers('columnsMenu', 'columnsMenuItem', 'rowNumbererColumn');
        this.callParent();
    },

    getColumnForField: function (fieldName) {
        return this.getHeaderContainer().getColumnForField(fieldName);
    },

    /**
     * Get columns using a selector to filter which columns
     * to return.
     *
     * @param {String/Function} selector
     * If the selector is a `String`, columns will be found using
     * {@link Ext.ComponentQuery}. If the selector is a `Function`,
     * {@link Ext.Array#filter} will be used to filter the columns.
     * If no selector is provided, all columns will be returned.
     * @return {Array}
     */
    getColumns: function(selector) {
        return this.getHeaderContainer().getColumns(selector);
    },

    getVisibleColumns: function() {
        return this.getHeaderContainer().getVisibleColumns();
    },

    insertColumn: function(index, column) {
        return this.getHeaderContainer().insert(index, column);
    },

    /**
     * Converts the given parameter to a cell.
     * @param {Ext.event.Event/Ext.dom.Element/HTMLElement/Ext.data.Model/Ext.grid.Row} value The value.
     * Can be an event or an element to find the cell via the DOM. Otherwise, a record or row can be passed. If
     * this occurs, the column parameter also needs to be passed.
     * @param {Ext.grid.column.Column} [column] The column. Needed if the first parameter is a model or a row.
     * @return {Ext.grid.cell.Base} The cell, if it can be found.
     *
     * @since 6.5.0
     */
    mapToCell: function(value, column) {
        var me = this,
            ret;

        if (value) {
            if (value.isGridCell && value.row.getGrid() === me) {
                ret = value;
            } else {
                if (value.isEntity) {
                    value = me.mapToItem(value);
                }

                if (value) {
                    if (value.isGridRow) {
                        column = column || me.getFirstVisibleColumn();
                        if (column) {
                            ret = value.getCellByColumn(column);
                        }
                    } else {
                        ret = Ext.Component.from(value, me.innerCt, 'gridcellbase');
                    }
                }
            }
        }
        return ret || null;
    },

    mapToItem: function(value, as) {
        if (value && value.isGridCell) {
            value = value.row;
        }
        return this.callParent([value, as]);
    },

    /**
     * Converts the given parameter to a row body.
     * @param {Ext.event.Event/Ext.dom.Element/HTMLElement/Ext.data.Model/Ext.grid.Row} value The value.
     * Can be an event or an element to find the row body via the DOM. Otherwise, a record or row can be passed.
     * @return {Ext.grid.RowBody} The row body, if it can be found.
     *
     * @since 6.5.0
     */
    mapToRowBody: function(value) {
        if (value) {
            if (!value.isGridRow) {
                value = this.mapToItem(value);
            }

            if (value && value.isGridRow) {
                value = value.getBody();
            }
        }
        return value || null;
    },

    removeColumn: function(column) {
        return this.getHeaderContainer().remove(column);
    },

    /**
     * @protected
     * This method is for use by plugins which require the grid to enter actionable mode
     * to focus in-cell elements.
     *
     * An example of this can be found in the {@link Ext.grid.plugin.CellEditing cell editing} plugin.
     *
     * Actionable plugins must implement the `{@link Ext.grid.plugin.CellEditing#activateCell activateCell}`
     * method which will be called whenever the application wants to enter actionable mode
     * on a certain cell. A {@link Ext.grid.Location grid location} object will be passed.
     *
     * The `activateCell` method must return an {@link Ext.grid.Location} if it accepts
     * control, indicating in its {@link Ext.grid.Location#element element} setting
     * exactly where focus has moved to.
     *
     * Actionable plugins may also expose a `triggerEvent` config which is the name of an
     * event to be used to trigger actioning that plugin, in addition fo the ARIA standard
     * method of the user pressing `F2` or `ENTER` when focused on a cell.
     *
     * @param {Object} actionable A plugin which creates or manipulates in-cell focusable
     * elements.
     */
    registerActionable: function(actionable) {
        this.getNavigationModel().registerActionable(actionable);
    },

    /**
     * @protected
     * This method is for use by plugins which require the grid to enter actionable mode
     * to focus in-cell elements. See {@link #method!registerActionable}.
     *
     * @param {Object} actionable The actionable plugin to unregister.
     */
    unregisterActionable: function(actionable) {
        this.getNavigationModel().unregisterActionable(actionable);
    },

    //-------------------------
    // Event handlers

    onColumnAdd: function (container, column, columnIndex) {
        var me = this,
            items, ln, i, row;

        if (!me.initializingColumns && !me.destroying) {
            items = me.items.items;
            ln = items.length;

            for (i = 0; i < ln; i++) {
                row = items[i];
                if (row.isGridRow) {
                    row.insertColumn(columnIndex, column);
                }
            }

            me.onColumnChange('columnadd', [me, column, columnIndex]);
        }
    },

    onColumnHide: function (container, column) {
        var me = this,
            items, ln, i, row;

        if (me.initialized && !me.destroying) {
            items = me.items.items;
            ln = items.length;

            for (i = 0; i < ln; i++) {
                row = items[i];

                if (row.isGridRow) {
                    row.hideColumn(column);
                }
            }

            me.onColumnChange('columnhide', [me, column]);
        }
    },

    onColumnMove: function (container, columns, group, fromIdx) {
        var me = this,
            before = null,
            colLen = columns.length,
            items, ln, i, j, row, column,
            index, leaves;

        if (me.initialized && !me.destroying) {
            items = me.items.items;
            ln = items.length;

            // Find the item that will be after the last leaf we're going to insert
            // Don't bother checking the array bounds, if it goes out of bounds then
            // null is the right answer
            leaves = me.getHeaderContainer().getLeaves();
            index = leaves.indexOf(columns[colLen - 1]);
            before = leaves[index + 1] || null;

            for (i = colLen - 1; i >= 0; --i) {
                column = columns[i];
                for (j = 0; j < ln; j++) {
                    row = items[j];
                    if (row.isGridRow) {
                        row.insertColumnBefore(column, before);
                    }
                }
                me.onColumnChange('columnmove', [me, column, fromIdx + i, leaves.indexOf(column)]);

                before = column;
            }
        }
    },

    onColumnRemove: function (container, column) {
        var me = this,
            items, ln, i, row;

        if (me.initialized && !me.destroying) {
            if (column === me.sortedColumn) {
                me.sortedColumn = null;
            }

            items = me.items.items;
            ln = items.length;

            for (i = 0; i < ln; i++) {
                row = items[i];
                if (row.isGridRow) {
                    row.removeColumn(column);
                }
            }

            me.onColumnChange('columnremove', [me, column]);
        }
    },

    onColumnResize: function (container, column, width, oldWidth) {
        if (!this.destroying) {
            // Will be null on the first time
            if (oldWidth && !column.getHidden()) {
                this.fireEvent('columnresize', this, column, width);
            }
        }
    },

    onColumnShow: function (container, column) {
        var me = this,
            items, ln, i, row;

        if (me.initialized && !me.destroying) {
            items = me.items.items;
            ln = items.length;

            for (i = 0; i < ln; i++) {
                row = items[i];

                if (row.isGridRow) {
                    row.showColumn(column);
                }
            }

            me.onColumnChange('columnshow', [me, column]);
        }
    },

    onColumnSort: function(container, column, direction) {
        this.fireEvent('columnsort', this, column, direction);
    },

    onRender: function() {
        var hideHeaders = this._hideHeaders;

        this.callParent();

        // hideHeaders requires measure, so must be done on render
        if (hideHeaders) {
            this.updateHideHeaders(hideHeaders);
        }
    },

    privates: {
        dataItemMap: {
            header: 1,
            footer: 1
        },

        handleStoreSort: function() {
            if (this.rendered) {
                this.getHeaderContainer().setSortState();
            }
        },

        onStoreGroupChange: function(store, grouper) {
            this.callParent([store, grouper]);
            this.handleStoreSort();
        },

        onStoreSort: function() {
            this.handleStoreSort();
        },

        registerColumn: function(column) {
            var me = this,
                columns = me.registeredColumns,
                headerCt = me.getHeaderContainer();

            if (!column.isGridColumn) {
                column = Ext.create(column);
            }

            if (!columns) {
                me.registeredColumns = columns = [];
            }

            columns.push(column);
            // We may have already configured the columns, even if we are
            // configuring, so check if we have items
            if (!me.isConfiguring || (headerCt && headerCt.items.getCount())) {
                headerCt.add(column);
            }

            return column;
        },

        unregisterColumn: function(column, destroy) {
            var columns = this.registeredColumns,
                headerCt = this.getHeaderContainer();

            if (!this.destroying) {
                if (columns) {
                    Ext.Array.remove(columns, column);
                }

                if (headerCt) {
                    headerCt.remove(column, destroy === true);
                }
            }

            return column;
        },

        /**
         * @private
         * We MUST use our own cells as delegates for grid-based events.
         * Cell events will not work without this. The event system would not
         * carry cell information if we don't delegate onto our cells.
         */
        generateSelectorFunctions: function() {
            var me = this;

            me.callParent();

            // This is used solely by the view event listener to filter the event reactions
            // to the level of granularity needed.
            // At the Grid level, this will be cell elements.
            me.eventDelegate = function (candidate) {
                var comp = Ext.Component.from(candidate),
                    ret = true,
                    row;

                // Don't fire child events for the grid itself
                if (!comp || comp === me) {
                    return false;
                }

                // If it's a direct child of the grid, and it's a row or header/footer, it's ok
                if (comp.getRefOwner() === me) {
                    ret = comp.isGridRow || me.dataItemMap[comp.$dataItem];
                } else {
                    // Otherwise, this is to check for either:
                    // a) cell
                    // b) a row body
                    //
                    // We don't want to fire events for things inside the row body, or items inside cells
                    row = comp.row;

                    // GroupHeaders and GroupFooters are created at the List class level
                    // so they do not get a "grid" upward link, so check their "list" upward link.
                    ret = row && row.isGridRow && (row.grid || row.list) === me;
                }

                return ret;
            };
        },

        getFirstVisibleColumn: function() {
            var columns = this.getVisibleColumns();
            return columns.length ? columns[0] : null;
        },

        getLastVisibleColumn: function() {
            var columns = this.getVisibleColumns(),
                len = columns.length;

            return len ? columns[len - 1] : null;
        },

        isFirstVisibleColumn: function(column) {
            return this.getFirstVisibleColumn() === column;
        },

        isLastVisibleColumn: function(column) {
            return this.getLastVisibleColumn() === column;
        },

        createDataItem: function (cfg) {
            var item = this.callParent([ cfg ]);

            item.grid = this;

            return item;
        },

        // -----------------------
        // Event handlers

        onColumnChange: function(changeEvent, eventArgs) {
            var me = this;

            // Total width will change upon add/remove/hide/show
            // So keep innerCt size synced
            if (changeEvent !== 'columnmove' && changeEvent !== 'columnadd' && changeEvent !== 'columnremove') {
                me.refreshInnerWidth();
            }

            if (!me.isConfiguring) {
                me.fireEventArgs(changeEvent, eventArgs);
            }

            me.clearItemCaches();
            // TODO: This may cause a change in row heights, currently should
            // be handled by using variableHeights, but the grid could re-measure as
            // needed
            //this.refreshScrollerSize();
        },

        refreshInnerWidth: function () {
            var headerCtBody = this.getHeaderContainer().bodyElement.dom,
                scrollWidth;

            // Set the item containing element to the correct width.
            scrollWidth = headerCtBody.scrollWidth;
            this.setInnerWidth(scrollWidth > headerCtBody.clientWidth ? scrollWidth : null);
        },

        onColumnComputedWidthChange: function (changedColumns, totalColumnWidth) {
            var me = this,
                groupingInfo = me.groupingInfo;

            if (!me.destroying) {
                // Set the item containing element to the correct width.
                me.setInnerWidth(totalColumnWidth);

                me.setCellSizes(changedColumns, me.items.items);
                me.setCellSizes(changedColumns, me.itemCache);

                if (me.isGrouping()) {
                    me.setCellSizes(changedColumns, groupingInfo.headers.unused);
                    me.setCellSizes(changedColumns, groupingInfo.footers.unused);
                }

                // Row sizing rules change if we have flexed columns.
                me.fireEvent('columnlayout', me, changedColumns, totalColumnWidth);
            }
        },

        onCellSelect: function(location) {
            var cell = location.getCell();

            if (cell) {
                cell.addCls(this.selectedCls);
            }
        },

        onCellDeselect: function(location) {
            var cell = location.getCell();

            if (cell) {
                cell.removeCls(this.selectedCls);
            }
        },

        setCellSizes: function(changedColumns, items) {
            var len = items.length,
                changedColCount = changedColumns.length,
                row, i, j;

            // Size the cells
            for (i = 0; i < len; i++) {
                row = items[i];

                if (row.isGridRow) {
                    for (j = 0; j < changedColCount; j++) {
                        row.setColumnWidth(changedColumns[j]);
                    }
                }
            }
        },

        // -----------------------
        // Configs

        // columnLines

        updateColumnLines: function (columnLines) {
            this.el.toggleCls(this.columnLinesCls, columnLines);
        },

        // columnResize

        updateColumnResize: function (enabled) {
            var me = this,
                plugin = me.findPlugin('columnresizing');

            if (!plugin) {
                if (enabled) {
                    me.addPlugin('columnresizing');
                }
            }
            else {
                plugin.setGrid(enabled ? me : null);
            }
        },

        // columns

        updateColumns: function (columns) {
            var me = this,
                header = me.getHeaderContainer(),
                count = columns && columns.length,
                persist = me.registeredColumns;

            // If the header container is an instance, then it's already
            // peeked at the columns config and included it, so bail out
            if (header) {

                // With a new column set, the rowHeight must be invalidated.
                // The new columns may bring in a different data shape.
                me.rowHeight = null;

                if (header) {
                    header.beginColumnUpdate();

                    if (header.getItems().getCount()) {
                        // Preserve persistent columns
                        if (persist) {
                            header.remove(persist, false);
                        }

                        // Also preserve any returning columns...
                        if (count) {
                            header.remove(columns.filter(function (col) {
                                return col.isInstance;
                            }), /*destroy=*/false);
                        }

                        header.removeAll(/*destroy=*/true, /*everything=*/true);
                    }

                    if (count) {
                        me.initializingColumns = me.isConfiguring;

                        header.setColumns(columns);

                        // Re-add any persistent columns, any adjusted weights are recalculated
                        if (persist) {
                            header.add(persist);
                        }

                        delete me.initializingColumns;

                        // TODO: This may cause a change in row heights, currently should
                        // be handled by using variableHeights, but the grid could re-measure as
                        // needed
                        //me.refreshScrollerSize();
                    }

                    header.endColumnUpdate();
                }
            }
        },

        applyRowNumbers: function(rowNumbers) {
            var me = this;

            if (rowNumbers) {
                rowNumbers = me.rowNumbererColumn = Ext.create(Ext.apply({
                    xtype: 'rownumberer',
                    weight: -1000,
                    editRenderer: me.renderEmpty
                }, rowNumbers));
            }

            return rowNumbers;
        },

        updateRowNumbers: function(rowNumbers, oldRowNumbers) {
            if (oldRowNumbers) {
                this.unregisterColumn(oldRowNumbers, true);
            }

            if (rowNumbers) {
                this.registerColumn(rowNumbers);
            }
        },

        renderEmpty: function() {
            return '\u00a0';
        },

        // columnsMenuItem

        applyColumnsMenuItem: function (config, existing) {
            return Ext.updateWidget(existing, config, this, 'createColumnsMenuItem');
        },

        createColumnsMenuItem: function (config) {
            return Ext.apply({
                grid: this
            }, config);
        },

        // headerContainer

        applyHeaderContainer: function (config, existing) {
            return Ext.updateWidget(existing, config, this, 'createHeaderContainer');
            //
            // if (headerContainer && !headerContainer.isComponent) {
            //     headerContainer = Ext.factory(Ext.apply({
            //         sortable: this.getSortable(),
            //         grid: this
            //     }, headerContainer), Ext.grid.HeaderContainer);
            // }
            //
            // return headerContainer;
        },

        createHeaderContainer: function (config) {
            config = this.mergeProxiedConfigs('headerContainer', config, /*alwaysClone=*/true);
            config.sortable = this.getSortable();
            config.grid = this;
            return config;
        },

        updateHeaderContainer: function (headerContainer) {
            if (headerContainer) {
                //TODO just call these methods directly from rootHeaderCt?
                // the old headerContainers are destroyed if they are replaced...
                headerContainer.on({
                    columnresize: 'onColumnResize',
                    columnshow: 'onColumnShow',
                    columnhide: 'onColumnHide',
                    columnadd: 'onColumnAdd',
                    columnmove: 'onColumnMove',
                    columnremove: 'onColumnRemove',
                    columnsort: 'onColumnSort',
                    scope: this
                });
            }
        },

        // hideHeaders

        updateHideHeaders: function(hideHeaders) {
            if (this.isRendered) {
                var headerContainer = this.getHeaderContainer();

                // To hide the headers, just pull the following element upwards to cover it
                if (hideHeaders) {
                    headerContainer.el.setStyle({
                        marginBottom: '-' + headerContainer.el.measure('h') + 'px'
                    });
                } else {
                    headerContainer.el.setStyle({
                        marginBottom: ''
                    });
                }
            }
        },

        // title

        updateTitle: function(title) {
            var titleBar = this.getTitleBar();

            if (titleBar) {
                if (title) {
                    titleBar.setTitle(title);

                    if (titleBar.isHidden()) {
                        titleBar.show();
                    }
                } else {
                    titleBar.hide();
                }
            }
        },

        // titleBar

        applyTitleBar: function (config, existing) {
            return Ext.updateWidget(existing, config);
        },

        updateTitleBar: function (titleBar) {
            if (titleBar && !titleBar.getTitle()) {
                titleBar.setTitle(this.getTitle());
            }
        },

        // totalColumnWidth

        applyTotalColumnWidth: function (totalColumnWidth) {
            var rows = this.dataItems;

            // If we don't have any items yet, wait
            return rows.length === 0 ? undefined : totalColumnWidth;
        },

        // verticalOverflow

        updateVerticalOverflow: function (value, was) {
            var headerContainer = this.getHeaderContainer(),
                verticalScrollbarWidth = Ext.getScrollbarSize().width;

            this.callParent([ value, was ]);

            headerContainer.setVerticalOverflow(verticalScrollbarWidth > 0 && value);
        }
    } // privates
},
function (Grid) {
    Grid.prototype.indexModifiedFields = Ext.Array.toMap;
});
