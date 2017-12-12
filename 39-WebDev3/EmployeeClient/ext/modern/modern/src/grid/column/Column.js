/**
 * This class specifies the definition for a column inside a {@link Ext.grid.Grid}. It
 * encompasses both the grid header configuration as well as displaying data within the
 * grid itself.
 *
 * In general an array of column configurations will be passed to the grid:
 *
 *     @example
 *     Ext.create({
 *         xtype: 'grid',
 *         title: 'Tree Grid Demo',
 *         itemConfig: {
 *             viewModel: true
 *         },
 *         store: {
 *              data: [
 *                  {firstname:"Michael", lastname:"Scott", seniority:7, department:"Management", hired:"01/10/2004"},
 *                  {firstname:"Dwight", lastname:"Schrute", seniority:2, department:"Sales", hired:"04/01/2004"},
 *                  {firstname:"Jim", lastname:"Halpert", seniority:3, department:"Sales", hired:"02/22/2006"},
 *                  {firstname:"Kevin", lastname:"Malone", seniority:4, department:"Accounting", hired:"06/10/2007"},
 *                  {firstname:"Angela", lastname:"Martin", seniority:5, department:"Accounting", hired:"10/21/2008"}
 *              ]
 *         },
 *         columns: [
 *             {text: 'First Name',  dataIndex:'firstname'},
 *             {text: 'Last Name',  dataIndex:'lastname'},
 *             {text: 'Hired Month',  dataIndex:'hired'},
 *             {
 *                 text: 'Department',
 *                 width: 200,
 *                 cell: {
 *                    bind: '{record.department} ({record.seniority})'
 *                 }
 *             }
 *         ],
 *         width: 500,
 *         fullscreen: true
 *     });
 *
 * # Convenience Subclasses
 *
 * There are several column subclasses that provide default rendering for various data types
 *
 *  - {@link Ext.grid.column.Boolean}: Renders for boolean values
 *  - {@link Ext.grid.column.Date}: Renders for date values
 *  - {@link Ext.grid.column.Number}: Renders for numeric values
 *
 * For more information about configuring cell content, see {@link Ext.grid.Grid}.
 *
 * # Setting Sizes
 *
 * The columns can be only be given an explicit width value. If no width is specified the
 * grid will automatically the size the column to 20px.
 *
 * # Header Options
 *
 *  - {@link #text}: Sets the header text for the column
 *  - {@link #sortable}: Specifies whether the column can be sorted by clicking the header
 *    or using the column menu
 *
 * # Data Options
 *
 *  - {@link #dataIndex}: The dataIndex is the field in the underlying {@link Ext.data.Store}
 *    to use as the value for the column.
 *  - {@link #renderer}: Allows the underlying store value to be transformed before being
 *    displayed in the grid.
 */
Ext.define('Ext.grid.column.Column', {
    extend: 'Ext.grid.HeaderContainer',
    alternateClassName: 'Ext.grid.column.Template',

    xtype: ['gridcolumn', 'column', 'templatecolumn'],

    /**
     * @property {Boolean} isGridColumn
     * Set in this class to identify, at runtime, instances which are not instances of the
     * HeaderContainer base class, but are in fact, the subclass: Ext.grid.Column.
     */
    isGridColumn: true,

    mixins: [
        // This mixin is used to cache the padding size for cells in this column,
        // to be shared by all cells in the column.
        'Ext.mixin.StyleCacher',
        'Ext.mixin.Toolable'
    ],

    /**
     * @property {Boolean} isLeafHeader
     * This will be set to `true` if the column has no child columns.
     */

    /**
     * @property {Boolean} isHeaderGroup
     * This will be set to `true` if the column has child columns.
     */

    config: {
        /**
         * @cfg {String} [align='left']
         * Sets the alignment of the header and rendered columns.
         * Possible values are: `'left'`, `'center'`, and `'right'`.
         */
        align: undefined, // undefined so applier will run to determine default value

        /**
         * @cfg {Object} cell
         * The config object used to create {@link Ext.grid.cell.Base cells} for this column.
         * By default, cells use the {@link Ext.grid.cell.Cell gridcell} `xtype`. To create
         * a different type of cell, simply provide this config and the desired `xtype`.
         */
        cell: {
            xtype: 'gridcell'
        },

        /**
         * @cfg {String} dataIndex
         * The name of the field in the grid's {@link Ext.data.Store}'s {@link Ext.data.Model} definition from
         * which to draw the column's value. **Required.**
         */
        dataIndex: null,

        /**
         * @cfg {Number} defaultWidth
         * A width to apply if the {@link #flex} or {@link #width} configurations have not
         * been specified.
         *
         * @since 6.2.0
         */
        defaultWidth: 100,

        /**
         * @cfg {String[]} depends
         * Set this config to the field names that effect this column's rendering. This is
         * important for best performance when using a `renderer`, a `summaryRenderer` or
         * a `tpl` to render the cell's content. This is because such mechanisms can use
         * any field and as such must be refreshed on *any* field change. When this config
         * is provided, only changes to these fields (or the `dataIndex`) will cause a
         * refresh.
         *
         * When not using these mechanisms, only changes to the `dataIndex` will cause the
         * cell content to be refreshed.
         * @since 6.5.1
         */
        depends: null,

        emptyText: {
            cached: true,
            $value: '\xA0'
        },

        /**
         * @cfg {String} text
         * The header text to be used as innerHTML (html tags are accepted) to display in the Grid.
         * **Note**: to have a clickable header with no text displayed you can use the default of `&#160;` aka `&nbsp;`.
         */
        text: '\xa0',

        /**
         * @cfg {Boolean} sortable
         * False to disable sorting of this column. Whether local/remote sorting is used is specified in
         * `{@link Ext.data.Store#remoteSort}`.
         */
        sortable: true,

        /**
         * @cfg {Boolean} groupable
         * If the grid is {@link Ext.grid.Grid#grouped grouped}, the menu for this column will
         * offer to "Group by this column" if this is set to `true`.
         *
         * If using the {@link Ext.grid.plugin.ViewOptions ViewOptions} plugin, this option may be used to
         * disable the option to group by this column.
         */
        groupable: true,

        /**
         * @cfg {Boolean} resizable
         * False to prevent the column from being resizable.
         * Note that this configuration only works when the
         * {@link Ext.grid.plugin.ColumnResizing ColumnResizing} plugin is enabled on the
         * {@link Ext.grid.Grid Grid}.
         */
        resizable: true,

        /**
         * @cfg {Boolean} hideable
         * False to prevent the user from hiding this column.
         *
         * @since 6.5.0
         */
        hideable: true,

        /**
         * @cfg {Function/String} renderer
         * A renderer is a method which can be used to transform data (value, appearance, etc.)
         * before it is rendered.
         *
         * For example:
         *
         *      {
         *          text: 'Some column',
         *          dataIndex: 'fieldName',
         *
         *          renderer: function (value, record) {
         *              if (value === 1) {
         *                  return '1 person';
         *              }
         *              return value + ' people';
         *          }
         *      }
         *
         * If a string is supplied, it should be the name of a renderer method from the
         * appropriate {@link Ext.app.ViewController}.
         *
         * This config is only processed if the {@link #cell} type is the default of
         * {@link Ext.grid.cell.Cell gridcell}.
         *
         * **Note** See {@link Ext.grid.Grid} documentation for other, better alternatives
         * to rendering cell content.
         *
         * @cfg {Object} renderer.value The data value for the current cell.
         * @cfg {Ext.data.Model} renderer.record The record for the current row.
         * @cfg {Number} renderer.dataIndex The dataIndex of the current column.
         * @cfg {Ext.grid.cell.Base} renderer.cell The current cell.
         * @cfg {Ext.grid.column.Column} renderer.column The current column.
         * @cfg {String} renderer.return The HTML string to be rendered. *Note*: to
         * render HTML into the cell, you will have to configure the column's {@link #cell}
         * with `encodeHtml: false`
         */
        renderer: null,

        /**
         * @cfg {String} formatter
         * This config accepts a format specification as would be used in a `Ext.Template`
         * formatted token. For example `'round(2)'` to round numbers to 2 decimal places
         * or `'date("Y-m-d")'` to format a Date.
         *
         * In previous releases the `renderer` config had limited abilities to use one
         * of the `Ext.util.Format` methods but `formatter` now replaces that usage and
         * can also handle formatting parameters.
         *
         * When the value begins with `"this."` (for example, `"this.foo(2)"`), the
         * implied scope on which "foo" is found is the `scope` config for the column.
         *
         * If the `scope` is not given, or implied using a prefix of `"this"`, then either the
         * {@link #method-getController ViewController} or the closest ancestor component
         * configured as {@link #defaultListenerScope} is assumed to be the object with the
         * method.
         * @since 6.2.0
         */
        formatter: null,

        /**
         * @cfg {Object} scope
         * The scope to use when calling the {@link #renderer} or {@link #formatter} function.
         */
        scope: null,

        /**
         * @cfg {Boolean} editable
         * Set this to true to make this column editable.
         * Only applicable if the grid is using an {@link Ext.grid.plugin.Editable Editable} plugin.
         */
        editable: null,

        /**
         * @cfg {Object/String} editor
         * The `xtype` or config object for a {@link Ext.field.Field Field} to use for
         * editing. This config is used by the {@link Ext.grid.plugin.Editable grideditable}
         * plugin.
         *
         * If this config is not set, and {@link #editable} is set to true, the
         * {@link #defaultEditor} is used.
         */
        editor: null,

        /**
         * @cfg {Object/Ext.field.Field} defaultEditor
         * An optional config object that should not really be modified. This is used to
         * create a default editor used by the {@link Ext.grid.plugin.Editable grideditable}
         * plugin when no {@link #editor} is specified.
         */
        defaultEditor: {
            lazy: true,
            $value: {}
        },

        /**
         * @cfg {Boolean} ignore
         * Setting to `true` prevents this column from being used by plugins such as
         * {@link Ext.grid.plugin.ViewOptions} or {@link Ext.grid.plugin.Summary}. It is
         * intended for special columns such as the row number or checkbox selection.
         */
        ignore: false,

        /**
         * @cfg {Boolean} ignoreExport
         * This flag indicates that this column will be ignored when grid data is exported.
         *
         * When grid data is exported you may want to export only some columns that are important
         * and not everything. You can set this flag on any column that you want to be ignored during export.
         *
         * This is used by {@link Ext.grid.plugin.Exporter exporter plugin}.
         */
        ignoreExport: false,

        /**
         * @cfg {Ext.exporter.file.Style/Ext.exporter.file.Style[]} exportStyle
         *
         * A style definition that is used during data export via the {@link Ext.grid.plugin.Exporter exporter plugin}.
         * This style will be applied to the columns generated in the exported file.
         *
         * You could define it as a single object that will be used by all exporters:
         *
         *      {
         *          xtype: 'numbercolumn',
         *          dataIndex: 'price',
         *          exportStyle: {
         *              format: 'Currency',
         *              alignment: {
         *                  horizontal: 'Right'
         *              },
         *              font: {
         *                  italic: true
         *              }
         *          }
         *      }
         *
         * You could also define it as an array of objects, each object having a `type` that specifies by
         * which exporter will be used:
         *
         *      {
         *          xtype: 'numbercolumn',
         *          dataIndex: 'price',
         *          exportStyle: [{
         *              type: 'html', // used by the `html` exporter
         *              format: 'Currency',
         *              alignment: {
         *                  horizontal: 'Right'
         *              },
         *              font: {
         *                  italic: true
         *              }
         *          },{
         *              type: 'csv', // used by the `csv` exporter
         *              format: 'General'
         *          }]
         *      }
         *
         * Or you can define it as an array of objects that has:
         *
         * - one object with no `type` key that is considered the style to use by all exporters
         * - objects with the `type` key defined that are exceptions of the above rule
         *
         *      {
         *          xtype: 'numbercolumn',
         *          dataIndex: 'price',
         *          exportStyle: [{
         *              // no type defined means this is the default
         *              format: 'Currency',
         *              alignment: {
         *                  horizontal: 'Right'
         *              },
         *              font: {
         *                  italic: true
         *              }
         *          },{
         *              type: 'csv', // only the CSV exporter has a special style
         *              format: 'General'
         *          }]
         *      }
         *
         */
        exportStyle: null,

        /**
         * @cfg {Boolean/Function/String} exportRenderer
         *
         * During data export via the {@link Ext.grid.plugin.Exporter} plugin the data for
         * this column could be formatted in multiple ways:
         *
         * - using the `exportStyle.format`
         * - using the `formatter` if no `exportStyle` is defined
         * - using the `exportRenderer`
         *
         * If you want to use the `renderer` defined on this column then set `exportRenderer`
         * to `true`. Beware that this should only happen if the `renderer` deals only with
         * data on the record or value and it does NOT style the cell or returns an html
         * string.
         *
         *      {
         *          xtype: 'numbercolumn',
         *          dataIndex: 'price',
         *          text: 'Price',
         *          renderer: function (value, record, dataIndex, cell, column) {
         *              return Ext.util.Format.currency(value);
         *          },
         *          exportRenderer: true
         *      }
         *
         * If you don't want to use the `renderer` during export but you still want to format
         * the value in a special way then you can provide a function to `exportRenderer` or
         * a string (which is a function name on the ViewController).
         * The provided function has the same signature as the renderer.
         *
         *      {
         *          xtype: 'numbercolumn',
         *          dataIndex: 'price',
         *          text: 'Price',
         *          exportRenderer: function (value, record, dataIndex, cell, column) {
         *              return Ext.util.Format.currency(value);
         *          }
         *      }
         *
         *
         *      {
         *          xtype: 'numbercolumn',
         *          dataIndex: 'price',
         *          text: 'Price',
         *          exportRenderer: 'exportAsCurrency' // this is a function on the ViewController
         *      }
         *
         *
         * If `exportStyle.format`, `formatter` and `exportRenderer` are all defined on the
         * column then the `exportStyle` wins and will be used to format the data for this
         * column.
         */
        exportRenderer: false,

        /**
         * @cfg {String} summary
         * This config replaces the default mechanism of acquiring a summary result from
         * the summary record. When specified, this string is the name of a summary type:
         *
         *  - {@link Ext.data.summary.Average average}
         *  - {@link Ext.data.summary.Count count}
         *  - {@link Ext.data.summary.Max max}
         *  - {@link Ext.data.summary.Min min}
         *  - {@link Ext.data.summary.Sum sum}
         *
         * The summary is based on either the {@link #cfg!summaryDataIndex} or the
         * {@link #cfg!dataIndex} if there is no `summaryDataIndex`.
         *
         * This config is only valid when all data is available client-side to calculate
         * summaries.
         *
         * It is generally best to allow the summary {@link Ext.data.Model record} to
         * computer summary values (and not use this config). In some cases, however,
         * this config can be useful to isolate summary calculations to only certain grids.
         *
         * To implement a custom summary for a column, use {@link #cfg!summaryRenderer}.
         * @since 6.5.0
         */
        summary: null,

        /**
         * @cfg {Object} summaryCell
         * The config object used to create {@link Ext.grid.cell.Base cells} in
         * {@link Ext.grid.SummaryRow Summary Rows} for this column.
         */
        summaryCell: null,

        /**
         * @cfg {String} summaryDataIndex
         * For {@link Ext.grid.SummaryRow summary rows} this config overrides the normal
         * `dataIndex` to use from the summary record.
         * @since 6.5.0
         */
        summaryDataIndex: null,

        /**
         * @cfg {String} summaryFormatter
         * This summaryFormatter is similar to {@link #formatter} but is called before
         * displaying a value in the SummaryRow. The config is optional, if not specified
         * the default calculated value is shown. The summaryFormatter is called with:
         *
         *  - value: The calculated value.
         *
         * Note that this configuration only works when the grid has the
         * {@link Ext.grid.plugin.Summary gridsummary} plugin enabled.
         */
        summaryFormatter: null,

        /**
         * @cfg {Function/String} summaryRenderer
         * This summaryRenderer is called to render the value to display in a cell of a
         * summary row. If the value of this config is a String, it is the name of the
         * renderer method on the associated {@link Ext.Component#controller controller}.
         *
         * @cfg {Mixed} summaryRenderer.value The summary value to render. This value is
         * retrieved from the summary record based on the {@link #cfg!summaryDataIndex} or
         * {@link #cfg!dataIndex}, or by applying the {@link #cfg!summary} algorithm to
         * the appropriate records. While this value can be useful, it can also be ignored
         * and the renderer method can use the `context` information to determine the value
         * to render entirely on its own.
         *
         * @cfg {Object} summaryRenderer.context The summary context object.
         *
         * @cfg {String} summaryRenderer.context.dataIndex The data field. This will be
         * either the {@link #cfg!summaryDataIndex} if one is specified, or the normal
         * {@link #cfg!dataIndex} if not.
         *
         * @cfg {String} summaryRenderer.context.group The {@link Ext.data.Group group}
         * being summarized. This is `null` if the summary is for the whole `store`.
         *
         * @cfg {String} summaryRenderer.context.store The {@link Ext.data.Store store}
         * being summarized.
         *
         * If this method returns `undefined`, no update is made to the cell. Instead it
         * is assumed that the `summaryRenderer` has made all of the necessary changes.
         *
         * Note that this configuration only works when the grid has the
         * {@link Ext.grid.plugin.Summary gridsummary} plugin enabled.
         */
        summaryRenderer: null,

        /**
         * @cfg {String/Function} summaryType
         * This configuration specifies the type of summary. There are several built in
         * summary types. These call underlying methods on the store:
         *
         *  - {@link Ext.data.Store#count count}
         *  - {@link Ext.data.Store#sum sum}
         *  - {@link Ext.data.Store#min min}
         *  - {@link Ext.data.Store#max max}
         *  - {@link Ext.data.Store#average average}
         *
         * Any other name is assumed to be the name of a method on the associated
         * {@link Ext.app.ViewController view controller}.
         *
         * Note that this configuration only works when the grid has the
         * {@link Ext.grid.plugin.Summary gridsummary} plugin enabled.
         *
         * @deprecated 6.5 Use {@link #cfg!summary} or {@link #cfg!summaryRenderer} instead.
         */
        summaryType: null,

        /**
         * @cfg {Boolean/Function/String} exportSummaryRenderer
         *
         * This config is similar to {@link #exportRenderer} but is applied to summary
         * records.
         */
        exportSummaryRenderer: false,

        minWidth: 40,

        /**
         * @cfg {String/String[]/Ext.XTemplate} tpl
         * An {@link Ext.XTemplate XTemplate}, or an XTemplate *definition string* to use
         * to process a {@link Ext.data.Model records} data to produce a cell's rendered
         * value.
         *
         *     @example
         *     Ext.create('Ext.data.Store', {
         *         storeId:'employeeStore',
         *         fields:['firstname', 'lastname', 'seniority', 'department'],
         *         groupField: 'department',
         *         data:[
         *             { firstname: "Michael", lastname: "Scott",   seniority: 7, department: "Management" },
         *             { firstname: "Dwight",  lastname: "Schrute", seniority: 2, department: "Sales" },
         *             { firstname: "Jim",     lastname: "Halpert", seniority: 3, department: "Sales" },
         *             { firstname: "Kevin",   lastname: "Malone",  seniority: 4, department: "Accounting" },
         *             { firstname: "Angela",  lastname: "Martin",  seniority: 5, department: "Accounting" }
         *         ]
         *     });
         *
         *     Ext.create('Ext.grid.Panel', {
         *         title: 'Column Template Demo',
         *         store: Ext.data.StoreManager.lookup('employeeStore'),
         *         columns: [{
         *             text: 'Full Name',
         *             tpl: '{firstname} {lastname}'
         *         }, {
         *             text: 'Department (Yrs)',
         *             tpl: '{department} ({seniority})'
         *         }],
         *         height: 200,
         *         width: 300,
         *         renderTo: Ext.getBody()
         *     });
         *
         * This config is only processed if the {@link #cell} type is the default of
         * {@link Ext.grid.cell.Cell gridcell}.
         *
         * **Note** See {@link Ext.grid.Grid} documentation for other, better alternatives
         * to rendering cell content.
         */
        tpl: null,

        /**
         * @cfg {Number} computedWidth
         * The computed width for this column, may come from either
         * {@link #width} or {@link #flex}.
         * @readonly
         */
        computedWidth: null,

        /**
         * @cfg {Function/String/Object/Ext.util.Grouper} grouper
         * A grouper config object to apply when the standard grouping user interface is
         * is invoked. This option is, for example, available in the column's header
         * menu.
         *
         * Note that a grouper may also be specified as a function which accepts two
         * records to compare.
         *
         * A `{@link Ext.app.ViewController controller}` method can be used like so:
         *
         *      grouper: 'groupMethodName'
         *
         * This is different then a `sorter` in that the `grouper` method is used to
         * set the {@link Ext.util.Grouper#cfg!groupFn groupFn}. This string returned
         * by this method is used to determine group membership. To specify both the
         * `grpoupFn` and the `sorterFn`:
         *
         *      grouper: {
         *          groupFn: 'groupMethodName'
         *          sorterFn: 'sorterMethodName
         *      }
         *
         * @since 6.5.0
         */
        grouper: {
            lazy: true,
            $value: null
        },

        /**
         * @cfg {String/String[]/Ext.XTemplate} groupHeaderTpl
         * This config allows a column to replace the default template supplied by the
         * grid's {@link Ext.grid.RowHeader#tpl groupHeader.tpl}.
         *
         * @since 6.5.0
         */
        groupHeaderTpl: null,

        /**
         * @cfg {Function/String/Object/Ext.util.Sorter} sorter
         * A sorter config object to apply when the standard sort user interface is
         * is invoked. This is usually clicking this column header, but there are also
         * menu options to sort ascending or descending.
         *
         * Note that a sorter may also be specified as a function which accepts two
         * records to compare.
         *
         * A `{@link Ext.app.ViewController controller}` method can be used like so:
         *
         *      sorter: 'sorterMethodName'
         *
         * Or more explicitly:
         *
         *      sorter: {
         *          sorterFn: 'sorterMethodName'
         *      }
         *
         * By default sorting is based on the `dataIndex` but this can be adjusted
         * like so:
         *
         *      sorter: {
         *          property: 'otherProperty'
         *      }
         *
         * @since 6.5.0
         */
        sorter: {
            lazy: true,
            $value: true
        },

        /**
         * @cfg {Ext.grid.cell.Cell/Object} scratchCell
         * @since 6.5.0
         * @private
         */
        scratchCell: {
            lazy: true,
            $value: true
        },

        /**
         * @cfg {Ext.menu.Menu/Object} menu
         * An optional menu configuration object which is merged with the grid's
         * {@link #cfg!columnMenu} to create this column's header menu. This can be set
         * to `null` to remove the menu from this column. To dynamically change whether
         * the menu should be enabled or not use the `menuDisabled` config.
         *
         * The grid's {@link Ext.grid.Grid#cfg!columnMenu} provides the sort items, this
         * config can be used to add column-specific menu items or override aspects of
         * the common items.
         * @since 6.5.0
         */
        menu: {
            lazy: true,
            $value: {}
        },

        /**
         * @cfg {Boolean} [menuDisabled=false]
         * Set to `true` to disable this column's `menu` containing sort/hide options.
         * This can be useful if the menu will be dynamically available since setting
         * `menu` to `null` will eliminate the menu making dynamic changes to its
         * availability more expensive.
         * @since 6.5.0
         */
        menuDisabled: null,

        /**
         * @cfg {Ext.menu.CheckItem/Object} hideShowMenuItem
         * The {@link Ext.menu.CheckItem menu item} to be used by the owning grid's
         * header menu to hide or show this column.
         * @since 6.5.0
         * @private
         */
        hideShowMenuItem: {
            lazy: true,
            $value: {
                xtype: 'menucheckitem'
            }
        }
    },

    toolDefaults: {
        ui: 'gridcolumn',
        zone: 'tail'
    },

    toolAnchorName: 'titleWrapElement',

    dockTools: false,

    scrollable: false,

    docked: null,

    sortState: null,

    // These are not readable descriptions; the values go in the aria-sort attribute.
    ariaSortStates: {
        ASC: 'ascending',
        DESC: 'descending'
    },

    inheritUi: true,

    classCls: Ext.baseCSSPrefix + 'gridcolumn',
    sortedCls: Ext.baseCSSPrefix + 'sorted',
    secondarySortCls : Ext.baseCSSPrefix + 'secondary-sort',
    auxSortCls : Ext.baseCSSPrefix + 'aux-sort',
    resizableCls: Ext.baseCSSPrefix + 'resizable',
    groupCls: Ext.baseCSSPrefix + 'group',
    leafCls: Ext.baseCSSPrefix + 'leaf',
    menuOpenCls: Ext.baseCSSPrefix + 'menu-open',
    alignCls: {
        left: Ext.baseCSSPrefix + 'align-left',
        center: Ext.baseCSSPrefix + 'align-center',
        right: Ext.baseCSSPrefix + 'align-right'
    },

    /**
     * @event columnmenucreated
     * @member Ext.grid.Grid
     * Fired when a column first creates its column menu. This is to allow plugins
     * to access and manipulate the column menu.
     *
     * There will be the two sort items, and a column hide/show item with a child menu of
     * checkboxes. After this, developers may add custom enu items.
     *
     * Menu items may be configured with a `weight` config, and those with the lowest weight
     * gravitate to the top.
     *
     * The sort ascending, sort descending, and hide columns items have weight -3, -2, and -1
     * @param {Ext.grid.Grid} grid This Grid
     * @param {Ext.grid.Column} column The column creating the menu
     * @param {Ext.menu.Menu} menu The column's new menu
     */

    constructor: function (config) {
        var me = this,
            isHeaderGroup, menu;

        // If we are configured or prototyped as a HeaderGroup
        // TODO - move to updater (me.columns won't work in all cases)
        if (config.columns || me.columns) {
            isHeaderGroup = me.isHeaderGroup = true;
        } else {
            me.isLeafHeader = true;
        }

        me.callParent([config]);

        me.addCls(isHeaderGroup ? me.groupCls : me.leafCls);

        menu = me.getConfig('menu', /*peek=*/true);
        if (!menu && me.getMenuDisabled() === null) {
            me.setMenuDisabled(true);
        }
    },

    getTemplate: function () {
        var me = this,
            beforeTitleTemplate = me.beforeTitleTemplate,
            afterTitleTemplate = me.afterTitleTemplate,
            titleTpl = [];

        // Hook for subclasses to insert extra elements
        if (beforeTitleTemplate) {
            titleTpl.push.apply(titleTpl, beforeTitleTemplate);
        }

        titleTpl.push({
            reference: 'titleElement',
            className: Ext.baseCSSPrefix + 'title-el',
            children: [{
                reference: 'textElement',
                className: Ext.baseCSSPrefix + 'text-el',
                "data-qoverflow": true
            }, {
                reference: 'sortIconElement',
                cls: Ext.baseCSSPrefix + 'sort-icon-el ' +
                Ext.baseCSSPrefix + 'font-icon'
            }]
        });

        // Hook for subclasses to insert extra elements
        if (afterTitleTemplate) {
            titleTpl.push.apply(titleTpl, afterTitleTemplate);
        }

        return [{
            reference: 'headerElement',
            cls: Ext.baseCSSPrefix + 'header-el',
            children: [{
                reference: 'titleWrapElement',
                cls: Ext.baseCSSPrefix + 'title-wrap-el',
                uiCls: 'title-wrap-el',
                children: titleTpl
            }, {
                reference: 'resizerElement',
                cls: Ext.baseCSSPrefix + 'resizer-el ' +
                     Ext.baseCSSPrefix + 'item-no-tap'
            }, {
                reference: 'triggerElement',
                cls: Ext.baseCSSPrefix + 'trigger-el ' +
                     Ext.baseCSSPrefix + 'font-icon ' +
                     Ext.baseCSSPrefix + 'item-no-tap'
            }]
        }, {
            reference: 'bodyElement',
            cls: Ext.baseCSSPrefix + 'body-el',
            uiCls: 'body-el'
        }];
    },

    initialize: function () {
        var me = this;

        if (me.isLeafHeader && !me.getWidth() && me.getFlex() == null) {
            me.setWidth(me.getDefaultWidth());
        }

        me.callParent();

        me.element.on({
            tap: 'onColumnTap',
            longpress: 'onColumnLongPress',
            scope: this
        });
        me.triggerElement.on({
            tap: 'onTriggerTap',
            scope: this
        });
        me.resizerElement.on({
            tap: 'onResizerTap',
            scope: this
        });

        if (me.isHeaderGroup) {
            me.on({
                add: 'doVisibilityCheck',
                remove: 'doVisibilityCheck',
                show: 'onColumnShow',
                hide: 'onColumnHide',
                move: 'onColumnMove',
                delegate: '> column',
                scope: me
            });

            me.on({
                show: 'onShow',
                scope: me
            });
        }
    },

    doDestroy: function () {
        var me = this;

        me.destroyMembers('editor', 'resizeListener', 'menu', 'hideShowMenuItem', 'childColumnsMenu');

        me.setScratchCell(null);

        me.mixins.toolable.doDestroy.call(me);

        me.callParent();
    },

    onAdded: function(parent, instanced) {
        this.visibleIndex = null;
        this.callParent([parent, instanced]);
    },

    /**
     * Returns the index of this column in the list of *visible* columns only if this column is a base level Column. If it
     * is a group column, it returns `false`.
     * @return {Number}
     */
    getVisibleIndex: function() {
        // Note that the visibleIndex property is assigned by the owning HeaderContainer
        // when assembling the visible column set for the view.
        var visibleIndex = this.visibleIndex,
            rootHeaders;

        if (visibleIndex == null) {
            if (this.isHeaderGroup) {
                visibleIndex = false;
            }
            else {
                rootHeaders = this.getRootHeaderCt();

                if (rootHeaders) {
                    visibleIndex = rootHeaders.indexOfLeaf(this);
                }
            }

            this.visibleIndex = visibleIndex;
        }

        return visibleIndex;
    },

    _columnScopeRe: /^column\./,
    _gridScopeRe: /^grid\./,

    applyMenu: function (menu) {
        var me = this,
            grid = me.getGrid(),
            columnScopeRe = me._columnScopeRe,
            gridScopeRe = me._gridScopeRe,
            extraItems, gridColumnMenu, i, item, items, s;

        Ext.destroy(me.sortChangeListener);

        // Allow menu:null to rid the column of all menus... so only merge in the
        // grid's column menu if we have a non-null menu
        if (menu && !menu.isMenu) {
            if (Ext.isArray(menu)) {
                extraItems = menu;
                menu = null;
            }
            else if (!menu.items) {
                menu = {
                    items: menu
                };
            }

            if (!(gridColumnMenu = grid.getColumnMenu())) {
                // if menu was an array it is now null, so just make an empty {}
                menu = menu ? Ext.clone(menu) : {};
            }
            else {
                gridColumnMenu = Ext.clone(gridColumnMenu);
                menu = menu ? Ext.merge(gridColumnMenu, menu) : gridColumnMenu;
            }

            menu.ownerCmp = me;

            menu = Ext.create(menu);

            // This column is informed about group changes
            me.sortChangeListener = menu.on({
                groupchange: 'onColumnMenuGroupChange',
                scope: me
            });

            // We cannot use defaultListenerScope to map handlers in our menu to
            // ourselves because user views would then be blocked from doing so to
            // items they may have added to the same menu.
            //
            // Our trick is to encode special scopes in the handler names and see
            // if they have survived until now. It is possible the user has set
            // the handler to something else...

            for (items = menu.getItems().items, i = items && items.length; i-- > 0; ) {
                item = items[i];

                if (columnScopeRe.test(s = item.getHandler() || '')) {
                    item.setHandler(s.substr(7));  // remove "column."
                    item.scope = me;
                }
                else if (gridScopeRe.test(s)) {
                    item.setHandler(s.substr(5));  // remove "grid."
                    item.scope = grid;
                }
                else if (item.isMenuCheckItem) {
                    if (columnScopeRe.test(s = item.getCheckHandler() || '')) {
                        item.setCheckHandler(s.substr(7));
                        item.scope = me;
                    }
                    else if (gridScopeRe.test(s)) {
                        item.setCheckHandler(s.substr(5));
                        item.scope = grid;
                    }
                }
            }

            if (extraItems) {
                menu.add(extraItems);
            }

            grid.fireEvent('columnmenucreated', grid, me, menu);
        }

        return menu;
    },

    updateMenu: function (menu, oldMenu) {
        if (oldMenu) {
            oldMenu.destroy();
        }
    },

    beforeShowMenu: function (menu) {
        var me = this,
            store = me.getGrid().getStore(),
            isGrouped = store && !!store.getGrouper(),
            groupByThis = menu.getComponent('groupByThis'),
            showInGroups = menu.getComponent('showInGroups'),
            sortAsc = menu.getComponent('sortAsc'),
            sortDesc = menu.getComponent('sortDesc');

        sortAsc.setDisabled(!store);
        sortDesc.setDisabled(!store);

        // We have no store yet, we can't group or ungroup
        if (!store) {
            groupByThis.setHidden(true);
            showInGroups.setHidden(true);
            return;
        }

        // Ensure the checked state of the ascending and descending menu items
        // matches the reality of the Store's sorters.
        //
        // We are syncing the menu state with the reality of the store.
        // Ensure its state change doesn't drive the store state
        // by suspending the groupchange event.
        menu.suspendEvent('groupchange');
        if (sortAsc) {
            me.syncMenuItemState(sortAsc);
        }
        if (sortDesc) {
            me.syncMenuItemState(sortDesc);
        }

        if (groupByThis) {
            groupByThis.setHidden(!(me.canGroup() && !store.isTreeStore));
        }
        menu.resumeEvent('groupchange');

        if (showInGroups) {
            // A TreeStore is never grouped
            showInGroups.setHidden(store.isTreeStore);
            // Disable the "Show in groups" options if we're not already shown in groups
            showInGroups.setChecked(isGrouped);
            showInGroups.setDisabled(!isGrouped);
        }
    },

    showMenu: function () {
        var me = this,
            menu = !me.getMenuDisabled() && me.getMenu(),
            menuOpenCls = me.menuOpenCls,
            columnsMenu, grid;

        // Only try if the menu is not disabled, and there *is* a menu
        if (menu) {
            grid = me.getGrid();
            columnsMenu = grid.getColumnsMenuItem();
            menu.add(columnsMenu);

            if (me.beforeShowMenu(menu) !== false &&
                    grid.beforeShowColumnMenu(me, menu) !== false) {
                menu.showBy(me.triggerElement);

                // Add menu open class to show the trigger element while the menu is open
                me.addCls(menuOpenCls);

                menu.on({
                    single: true,
                    hide: function () {
                        if (!(me.destroyed || me.destroying)) {
                            me.removeCls(menuOpenCls);
                            menu.remove(columnsMenu, /*destroy=*/false);
                        }
                    }
                });
            }
        }
    },

    getCells: function () {
        var cells = [],
            rows = this.getGrid().items.items,
            len = rows.length,
            i, row;

        for (i = 0; i < len; ++i) {
            row = rows[i];
            if (row.isGridRow) {
                cells.push(row.getCellByColumn(this));
            }
        }

        return cells;
    },

    getColumnForField: function (fieldName) {
        if (fieldName === this.getDataIndex()) {
            return this;
        }

        return this.callParent([ fieldName ]);
    },

    /**
     * Determines whether the UI should be allowed to offer an option to hide this column.
     *
     * A column may *not* be hidden if to do so would leave the grid with no visible columns.
     *
     * This is used to determine the enabled/disabled state of header hide menu items.
     */
    isHideable: function() {
        var menuOfferingColumns = [];

        // Collect menu offering columns so that we can assess our hideability.
        // Cannot use CQ because we need to use getConfig with peek flag to
        // check whether there's a menu without instantiating it.
        this.getRootHeaderCt().visitPreOrder('gridcolumn:not([hidden])', function(col) {
            if (!col.getMenuDisabled() && col.getConfig('menu', true)) {
                menuOfferingColumns.push(col);
            }
        });

        return menuOfferingColumns.length > 1 || menuOfferingColumns[0] !== this;
    },

    applyTpl: function (tpl) {
        return Ext.XTemplate.get(tpl);
    },

    applyAlign: function(align, oldAlign) {
        if (align == null) {
            align = this.isHeaderGroup ? 'center' : 'left';
        }

        return align;
    },

    updateAlign: function (align, oldAlign) {
        var me = this,
            alignCls = me.alignCls;

        if (oldAlign) {
            me.removeCls(alignCls[oldAlign]);
        }

        if (align) {
            //<debug>
            if (!alignCls[align]) {
                Ext.raise("Invalid value for align: '" + align + "'");
            }
            //</debug>
            me.addCls(alignCls[align]);
        }

        me.syncToolableAlign();
    },

    updateMenuDisabled: function (menuDisabled) {
        if (this.triggerElement) {
            this.triggerElement.setVisible(!menuDisabled);
        }
    },

    onColumnTap: function (e) {
        var me = this,
            grid = me.getGrid(),
            selModel = grid.getSelectable(),
            store = grid.getStore(),
            sorters = store && store.getSorters(true),
            sorter = store && me.pickSorter(),
            sorterIndex = sorter ? sorters.indexOf(sorter) : -1,
            isSorted = sorter && (sorterIndex !== -1 || sorter === store.getGrouper());

        // Tapping on the trigger or resizer must not sort the column and
        // neither should tapping on any components (e.g. tools) contained
        // in the column.
        if (Ext.Component.from(e) !== me ||
            e.getTarget('.' + Ext.baseCSSPrefix + 'item-no-tap', me)) {
            return;
        }

        // Column tap sorts if we are sortable, and the selection model
        // is not selecting columns
        if (store && me.isSortable() && (!selModel || !selModel.getColumns())) {
            // Special case that our sorter is the grouper
            if (sorter.isGrouper) {
                sorter.toggle();
                store.group(sorter);
            }
            // If we are already the primary sorter
            // then just toggle through the three states
            else if (sorterIndex === 0) {
                me.toggleSortState();
            }
            // We must be a secondary or auxilliary in a multi column sort grid,
            // or unsorted now.
            else {
                // We're secondary or auxilliary, bring top top of sorter stack
                if (isSorted) {
                    store.sort(sorter, 'prepend');
                }
                // Our sorter is unused, go primary, ascending
                else {
                    me.sort('ASC');
                }
            }
        }

        return me.fireEvent('tap', me, e);
    },

    onTriggerTap: function (e) {
        this.fireEvent('triggertap', this, e);
    },

    onResizerTap: function (e) {
        // If they tapped on the resizer without dragging, interpret that as a tap
        // on the trigger, if it's in the correct region.
        if (e.getPoint().isContainedBy(this.triggerElement.getRegion())) {
            this.fireEvent('triggertap', this, e);
        }
    },

    onColumnLongPress: function (e) {
        this.fireEvent('longpress', this, e);
    },

    onGroupByThis: function () {
        var me = this,
            grid = me.getGrid(),
            grouper = me.getGrouper(),
            store = grid.getStore(),
            dataIndex;

        if (!grouper) {
            dataIndex = me.getDataIndex();

            if (dataIndex != null) {
                me.setGrouper({
                    property: dataIndex
                });

                grouper = me.getGrouper();
            }
        }

        if (grouper) {
            store.setGrouper(grouper);
        }
        grid.setGrouped(true);
    },

    /**
     * @private
     * Called as a groupchange handler on the header menu to either set the direction, or
     * remove the sorter.
     */
    onColumnMenuGroupChange: function (menu, groupName, value) {
        if (groupName === 'sortDir') {
            this.setSortDirection(value);
        }
    },

    getSortDirection: function () {
        var sorter = this.pickSorter();

        return sorter && sorter.getDirection();
    },

    setSortDirection: function (direction) {
        var me = this,
            grid = me.getGrid(),
            store = grid.getStore(),
            sorter = me.pickSorter(),
            sorters = store.getSorters(true),
            isSorted = sorter && (sorters.contains(sorter) || sorter.isGrouper);

        // Toggling to checked.
        if (direction) {
            if (isSorted) {
                if (sorter.getDirection() !== direction) {
                    sorter.setDirection(direction);

                    if (sorter.isGrouper) {
                        store.group(sorter);
                    } else {
                        sorters.beginUpdate();
                        sorters.endUpdate();
                    }
                }
            }
            // Either the sorter is not applied, or it's the first time and there's no sorter.
            // Sort by direction as primary
            else {
                return me.sort(direction);
            }
        }
        // Toggled to clear.
        // If we own a sorter, and its in our direction, and it's applied to the store
        // then remove it.
        else if (sorter) {
            sorters.remove(sorter);
        }

        // A locally sorted store will not refresh in response to having a sorter
        // removed, so we must sync the column header arrows now.
        // AbstractStore#onSorterEndUpdate will however always fire the sort event
        // which is what Grid uses to trigger a HeaderContainer sort state sync
        if (!store.getRemoteSort()) {
            me.getRootHeaderCt().setSortState();
        }
    },

    syncMenuItemState: function (menuItem) {
        if (menuItem) {
            var me = this,
                sortable = me.isSortable(),
                store = me.getGrid().getStore(),
                sorter = me.pickSorter(),
                isSorted = sorter && (store.getSorters().contains(sorter) || sorter.isGrouper);

            menuItem.setDisabled(!sortable);
            menuItem.setChecked(sortable && isSorted && sorter.getDirection() === menuItem.getValue());
        }
    },

    onToggleShowInGroups: function (menuItem) {
        var grid = this.getGrid(),
            store = grid.getStore();

        grid.setGrouped(false);
        store.setGrouper(null);
    },

    updateResizable: function (resizable) {
        var me = this,
            widthed = me.getWidth() != null,
            flexed = me.getFlex() != null;

        // Column only drag-resizable if it's widthed, flexed, or a leaf.
        // If it's shrinkwrapping child columns then the child columns must be resized.
        me.toggleCls(me.resizableCls, !!(me.getResizable() && (widthed || flexed ||
            me.isLeafHeader)));
    },

    updateText: function (text) {
        this.setHtml(text || '\xa0');
    },

    onResize: function () {
        if (!this.isHidden(true)) {
            // Update the resizability of this column based on *how* it's just been sized.
            // If we are shrinkwrapping, we are not drag-resizable.
            this.updateResizable(this.getResizable());

            // Computed with needs to be exact so that sub-pixel changes are
            // not rejected by the config system because scrollbars may
            // depend upon the *exact* width of the cells in the view.
            this.measureWidth();
        }
    },

    getComputedWidth: function () {
        return this.isVisible(true) ? this._computedWidth : 0;
    },

    updateColumns: function (columns) {
        this.getItems();
        this.add(columns);
    },

    measureWidth: function () {
        // Computed width must be a real. exact pixel width.
        // It cannot be em or rem etc because it is used to size owned cells
        // and different styles and fonts may be applied to cells.
        var width = this.el.measure('w');

        this.setComputedWidth(width);

        return width;
    },

    updateComputedWidth: function (value, oldValue) {
        var me = this,
            rootHeaderCt = !me.isConfiguring && me.getRootHeaderCt();

        // This is how grid's resize their cells in response. Not through events.
        // Width change events arrive asynchronously through resize listeners
        // and that would cause janky grid resizes.
        //
        // By informing the grid, it can force all flexed columns to republish
        // their computed widths, and correctly update all cells in one pass.
        if (rootHeaderCt) {
            // This updates the cells.
            rootHeaderCt.onColumnComputedWidthChange(me, value);

            // Fire the event after cells have been resized
            me.fireEvent('columnresize', me, value, oldValue);
        }
    },

    updateDataIndex: function (dataIndex) {
        var sorter;

        if (!this.isConfiguring) {
            sorter = this.pickSorter();

            if (sorter) {
                this.setSorter(null);
            }
        }
    },

    applyGroupHeaderTpl: function (tpl) {
        return Ext.XTemplate.get(tpl);
    },

    updateGroupHeaderTpl: function (tpl) {
        var grouper = this.grouper;

        if (grouper) {
            grouper.headerTpl = tpl;
        }
    },

    isSortable: function () {
        var me = this;

        // Only leaf headers are sortable.
        // Only if we are not configured sortable: false.
        // We're not sortable if there's no Sorter configured AND we have no dataIndex.
        // HeaderContainer's sortable config must be honoured dynamically since
        // SelectionModels can change it.
        // And the grid has the final say.
        return me.isLeafHeader &&
            me.getSortable() &&
            (me.pickSorter() || me.getDataIndex()) &&
            me.getRootHeaderCt().getSortable() &&
            me.getGrid().sortableColumns !== false;
    },

    applyEditor: function (value) {
        if (value && !value.isInstance) {
            if (typeof(value) === 'string') {
                value = {
                    xtype: value
                };
            }

            if (!value.xtype) {
                value = Ext.apply({
                    xtype: value.field ? 'celleditor' : 'textfield'
                }, value);
            }

            return Ext.create(value);
        }

        return value;
    },

    applyDefaultEditor: function(editor) {
        var dataIndex = this.getDataIndex(),
            model, field;

        if (dataIndex && !editor.isInstance) {
            // We mutate the config
            editor = Ext.clone(editor);

            // Infer default xtype from data field type
            if (!editor.isInstance && !editor.xtype) {
                model = this.getGrid().getStore().getModel();
                field = model.getField(dataIndex);

                if (field) {
                    switch (field.type) {
                        case 'date':
                            editor.xtype = 'datefield';
                            break;
                        case 'int':
                        case 'integer':
                            editor.xtype = 'numberfield';
                            editor.decimals = 0;
                            break;
                        case 'float':
                        case 'number':
                            editor.xtype = 'numberfield';
                            break;
                        case 'boolean':
                        case 'bool':
                            editor.xtype = 'checkboxfield';
                            break;
                        default:
                            editor.xtype = 'textfield';
                    }
                } else {
                    editor.xtype = 'textfield';
                }
            }
        }
        return editor;
    },

    updateEditor: function (editor, oldEditor) {
        // If we are changing editors destroy the last one
        // but if we are changing from a field to a cell editor make sure we do not destroy
        // the field that is now a child of the cell editor
        if (oldEditor && (!editor || (editor.isCellEditor && editor.getField() !== oldEditor))) {
            oldEditor.destroy();
        }
    },

    applyFormatter: function (format) {
        var me = this,
            fmt = format,
            parser;

        if (fmt) {
            parser = Ext.app.bind.Parser.fly(fmt);
            fmt = parser.compileFormat();
            parser.release();

            return function (v) {
                return fmt(v, me.getScope() || me.resolveListenerScope());
            };
        }

        return fmt;
    },

    applySummaryFormatter: function (format) {
        var me = this,
            fmt = format,
            parser;

        if (fmt) {
            parser = Ext.app.bind.Parser.fly(fmt);
            fmt = parser.compileFormat();
            parser.release();
            return function (v) {
                return fmt(v, me.getScope() || me.resolveListenerScope());
            };
        }

        return fmt;
    },

    applyGrouper: function (grouper) {
        var me = this,
            cfg = grouper;

        if (cfg && !cfg.isInstance) {
            if (typeof cfg === 'string') {
                cfg = {
                    groupFn: cfg
                };
            } else {
                cfg = Ext.apply({}, cfg);
            }

            if (typeof cfg.groupFn === 'string') {
                cfg = me.scopeReplacer(cfg, grouper, 'groupFn', 'setGroupFn');
            }

            if (typeof cfg.sorterFn === 'string') {
                cfg = me.scopeReplacer(cfg, grouper, 'sorterFn', 'setSorterFn');
            }

            grouper = new Ext.util.Grouper(cfg);
        }

        // The owner/headerTpl expandos on our grouper are picked up by the ItemHeader
        // as a means to override the list's groupHeaderTpl...
        if (grouper) {
            grouper.owner = me.getGrid();
            grouper.headerTpl = me.getGroupHeaderTpl();
        }

        // So folks can easily pick this up w/o calling getGrouper which will trigger
        // its creation.
        return grouper;
    },

    updateGrouper: function (grouper, oldGrouper) {
        var store = this.getGrid().getStore();

        if (store && oldGrouper) {
            if (oldGrouper === store.getGrouper()) {
                store.setGrouper(grouper);
            }
        }

        this.grouper = grouper;
    },

    applySorter: function (sorter) {
        var me = this,
            cfg = sorter,
            sortProperty;

        if (cfg && !cfg.isInstance) {
            // The default value is true to indicate use the dataIndex
            if (cfg === true) {
                sortProperty = me.getSortParam();
                if (!sortProperty) {
                    return null;
                }

                cfg = {
                    property: sortProperty,
                    direction: 'ASC'
                };
            } else {
                if (typeof cfg === 'string') {
                    cfg = {
                        sorterFn: cfg
                    };
                }

                if (typeof cfg.sorterFn === 'string') {
                    cfg = me.scopeReplacer(cfg, sorter, 'sorterFn', 'setSorterFn');
                }
            }

            sorter = new Ext.util.Sorter(cfg);
        }

        if (sorter) {
            sorter.owner = me.getGrid();
        }

        return sorter;
    },

    updateSorter: function (sorter, oldSorter) {
        var store = this.getGrid().getStore(),
            sorters = store ? store.getSorters() : null,
            at;

        // If our previous sorter is in the store, replace it with the new one or
        // just remove it if we don't have one.
        if (sorters) {
            if (oldSorter && (at = sorters.indexOf(oldSorter)) > -1) {
                if (sorter) {
                    sorters.splice(at, 1, sorter);
                } else {
                    sorters.remove(oldSorter);
                }
            }
        }

        // So folks can easily pick this up w/o calling getSorter which will trigger
        // its creation.
        this.sorter = sorter;
    },

    pickSorter: function() {
        var me = this,
            store = me.getGrid().getStore(),
            result;

        // Must always use the grouper if our dataIndex is the store's groupField.
        // We have to test dynamically in the getter because of possible store changes
        if (store.isGrouped() && store.getGroupField() === me.getDataIndex()) {
            result = me.getGrouper() || store.getGrouper();

            // The sort state is always the direction of the grouper
            me.sortState = result.getDirection();
        }
        else {
            result = me.getSorter();
        }

        return result;
    },

    applyHideShowMenuItem: function (config, existing) {
        return Ext.updateWidget(existing, config, this, 'createHideShowMenuItem');
    },

    createHideShowMenuItem: function(defaults) {
        return Ext.apply({
            text: this.getText(),
            checked: !this.getHidden(),
            column: this
        }, defaults);
    },

    getHideShowMenuItem: function(deep) {
        var me = this,
            result = me.callParent(),
            items = me.items.items,
            len = items.length,
            childItems = [],
            childColumnsMenu = me.childColumnsMenu,
            i;

        // If we're a header group, we offer our hideable child columns
        // in a submenu.
        if (me.isHeaderGroup && deep !== false) {
            if (!childColumnsMenu) {
                result.setMenu({});
                me.childColumnsMenu = childColumnsMenu = result.getMenu();
            }
            if (!childColumnsMenu.items.length || me.rebuildChildColumnsMenu) {
                for (i = 0; i < len; i++) {
                    if (items[i].getHideable()) {
                        childItems.push(items[i].getHideShowMenuItem());
                    }
                }
                childColumnsMenu.removeAll(false);
                childColumnsMenu.add(childItems);
            }
        }

        // Ensure we're enabled/disabled correctly on first show
        result['set' + (result.getMenu() ? 'CheckChange' : '') + 'Disabled'](!me.isHideable());

        return result;
    },

    getInnerHtmlElement: function () {
        return this.textElement;
    },

    /**
     * Returns the parameter to sort upon when sorting this header. By default this returns
     * the dataIndex and will not need to be overridden in most cases.
     * @return {String}
     */
    getSortParam: function () {
        return this.getDataIndex();
    },

    applyCell: function(cell, oldCell) {
        // Allow the cell config object to be reconfigured.
        if (oldCell) {
            cell = Ext.apply(oldCell, cell);
        }
        return cell;
    },

    createCell: function (row) {
        var me = this,
            cfg = {
                row: row,
                ownerCmp: row || me,
                column: me,
                width: me.rendered ? (me.getComputedWidth() || me.measureWidth()) : me.getWidth(),
                minWidth: me.getMinWidth()
            },
            align = me.getAlign(),
            cellCfg;

        if (row && row.isSummaryRow) {
            cellCfg = me.getSummaryCell();

            if (!cellCfg) {
                cellCfg = me.getCell();

                if (cellCfg.xtype === 'widgetcell') {
                    // We don't default to creating a widgetcell in a summary row, so
                    // fallback to a normal cell
                    cellCfg = Ext.apply({}, cellCfg);
                    cellCfg.xtype = 'gridcell';
                    delete cellCfg.widget;
                }
            }
        }
        else {
            cellCfg = me.getCell();
        }

        if (align) {
            // only put align on the config object if it is not null.  This prevents
            // the column's default value of null from overriding a value set on the
            // cell's class definition (e.g. widgetcell)
            cfg.align = align;
        }

        if (row) {
            cfg.hidden = me.isHidden(row.getGrid().getHeaderContainer());
            cfg.record = row.getRecord();

            if (!(cfg.ui = row.getDefaultCellUI())) {
                delete cfg.ui;
            }
        }

        if (typeof cellCfg === 'string') {
            cfg.xtype = cellCfg;
        }
        else {
            Ext.apply(cfg, cellCfg);
        }

        return cfg;
    },

    applyScratchCell: function(cell, oldCell) {
        var me = this;

        if (cell) {
            cell = Ext.create(me.createCell());

            if (!cell.printValue) {
                // If this cell type (widgetcell) cannot print its value, fallback to
                // default gridcell
                Ext.destroy(cell);
                cell = me.createCell();
                cell.xtype = 'gridcell';
                cell = Ext.create(cell);
            }
            // Add the positioned class to make this position:absolute so that it can be
            // added to the document without breaking the layout.
            cell.addCls(me.floatingCls);
        }

        if (oldCell) {
            oldCell.destroy();
        }

        return cell;
    },

    printValue: function (value) {
        var me = this,
            rows = me.getGrid().dataItems,
            cell;

        if (rows.length) {
            cell = rows[0].getCellByColumn(me);
        }

        cell = (cell && cell.printValue) ? cell : me.getScratchCell();

        return cell.printValue(value);
    },

    privates: {
        // State map for cycling our sortState property
        directionSequence: {
            "null": "ASC",
            "ASC": "DESC",
            "DESC": null
        },

        applySummary: function (summary) {
            if (summary) {
                summary = Ext.Factory.dataSummary(summary);
            }

            return summary;
        },

        beginRefresh: function (context) {
            // This is called by our detached cells
            var me = this,
                grid = me.getGrid();

            context = context || {};

            context.column = me;
            context.grid = grid;
            // record = null
            // row = null
            context.store = grid.store;

            return context;
        },

        canGroup: function() {
            return this.getGroupable() && (this.getDataIndex() || this.getGrouper());
        },

        /**
         * Sorts by this column's sorter in the passed direction.
         * @param direction
         * @param mode
         */
        sort: function(direction, mode) {
            var me = this,
                sorter = me.pickSorter(),
                grid = me.getGrid(),
                store = grid.getStore(),
                sorters = store.getSorters();

            if (!me.isSortable()) {
                return;
            }

            // This is the "group by" column - we have to set the grouper and tell it to
            // recalculate. AbstractStore#group just calls its Collection's updateGrouper
            // if passed a Grouper because *something* in the grouper might have changed,
            // but the config system would reject that as not a change.
            if (sorter.isGrouper) {
                if (sorter.getDirection() !== direction) {
                    sorter.toggle();
                    store.group(sorter);
                }
            }
            // We are moving to a sorted state
            else if (direction) {
                // We have a sorter - set its direction.
                if (sorter) {
                    // Not the primary. We will make it so.
                    // If it's already the primary, SorterCollection#addSort will toggle it
                    if (sorters.indexOf(sorter) !== 0) {
                        sorter.setDirection(direction);
                    }
                }
                // First time in, create a sorter with required direction
                else {
                    me.setSorter({
                        property: me.getSortParam(),
                        direction: 'ASC'
                    });

                    sorter = me.getSorter(); // not pickSorter
                }

                // If the grid is NOT configured with multi column sorting, then specify
                // "replace". Only if we are doing multi column sorting do we insert it as
                // one of a multi set.
                store.sort(sorter, mode || grid.getMultiColumnSort() ? 'multi' : 'replace');
            }
            // We're moving to an unsorted state
            else {
                if (sorter) {
                    sorters.remove(sorter);

                    // A locally sorted store will not refresh in response to having a
                    // sorter removed, so we must sync the column header arrows now.
                    // AbstractStore#onSorterEndUpdate will however always fire the sort
                    // event which is what Grid uses to trigger a HeaderContainer sort
                    // state sync
                    if (!store.getRemoteSort()) {
                        me.getRootHeaderCt().setSortState();
                    }
                }
            }
        },

        /**
         * Called on HeaderTap to toggle the column through three sort states.
         *
         *    Is primary sort?
         *      Yes - Cycle through ASC, DESC, None
         *      No - is sorted?
         *          Yes - Make it primary (leave direction alone)
         *          No - Make it primary ASC
         *
         *  - None -> ASC
         *  - ASC  -> DESC
         *  - DESC -> None
         */
        toggleSortState: function () {
            this.sort(this.directionSequence[this.sortState]);
        },

        /**
         * Sets the column sort state according to the direction of the Sorter passed.
         * @param {Ext.util.Sorter/String} sorter A Sorter, or the direction (`'ASC'` or `'DESC'`) to display in the header.
         */
        setSortState: function (sorter) {
            // Set the UI state to reflect the state of any passed Sorter
            // Called by the grid's HeaderContainer on view refresh
            var me = this,
                store = me.getGrid().getStore(),
                grouper = store.isGrouped() && store.getGrouper(),
                oldDirection = me.sortState,
                direction = null,
                sortedCls = me.sortedCls,
                secondarySortCls = me.secondarySortCls,
                auxSortCls = me.auxSortCls,
                ascCls = sortedCls + '-asc',
                descCls = sortedCls + '-desc',
                ariaDom = me.ariaEl.dom,
                sortPrioClass = '',
                changed, index,
                remove = [
                    secondarySortCls,
                    auxSortCls
                ],
                add;

            if (sorter) {
                if (typeof sorter === 'string') {
                    direction = sorter;
                } else {
                    //<debug>
                    if (!sorter.isSorter) {
                        Ext.raise('Must pass a sorter instance into HeaderContainer#saveState');
                    }
                    //</debug>

                    // The Grouper is always primary
                    if (sorter === grouper) {
                        index = 0;
                    }
                    else {
                        index = store.getSorters().indexOf(sorter);
                    }

                    //<debug>
                    if (index === -1) {
                        Ext.raise("Sorter passed into HeaderContainer#saveState is not used by the grid's store");
                    }
                    //</debug>

                    direction = sorter.getDirection();
                    sortPrioClass = index === 1 ? secondarySortCls : index > 1 ? auxSortCls : '';
                }
            }

            // Detect if we've changed state, then set our state
            changed = direction !== oldDirection;
            me.sortState = direction;

            switch (direction) {
                case 'DESC':
                    add = [sortedCls, descCls, sortPrioClass];
                    remove.push(ascCls);
                    break;

                case 'ASC':
                    add = [sortedCls, ascCls, sortPrioClass];
                    remove.push(descCls);
                    break;

                default:
                    remove.push(sortedCls, ascCls, descCls);
                    break;
            }
            me.replaceCls(remove, add);

            if (ariaDom) {
                if (direction) {
                    ariaDom.setAttribute('aria-sort', me.ariaSortStates[direction]);
                }
                else {
                    ariaDom.removeAttribute('aria-sort');
                }
            }

            // we only want to fire the event if we have actually sorted
            if (changed) {
                me.fireEvent('sort', me, direction, oldDirection);
            }
        },

        getVisibleCount: function () {
            var columns = this.getInnerItems(),
                len = columns.length,
                count = 0,
                i;

            for (i = 0; i < len; ++i) {
                if (columns[i].isHeaderGroup) {
                    count += columns[i].getVisibleCount();
                } else {
                    count += columns[i].isHidden() ? 0 : 1;
                }
            }

            return count;
        },

        onShow: function () {
            var toShow;

            // No visible subcolumns, then show the first child.
            if (!this.getVisibleCount()) {
                toShow = this.getComponent(0);
                if (toShow) {
                    toShow.show();
                }
            }
        },

        doVisibilityCheck: function () {
            var me = this,
                columns = me.getInnerItems(),
                ln = columns.length,
                i, column;

            for (i = 0; i < ln; i++) {
                column = columns[i];

                if (!column.isHidden()) {
                    if (me.isHidden()) {
                        if (me.initialized) {
                            me.show();
                        } else {
                            me.setHidden(false);
                        }
                    }
                    return;
                }
            }

            me.hide();

            // Next time we show our hide/show item, we need to rebuild the submenu
            me.rebuildChildColumnsMenu = true;

            // Update hideable/showable state of column menu items
            me.updateMenuDisabledState();
        },

        onColumnShow: function () {
            var me = this,
                hideShowItem;

            if (me.getVisibleCount() > 0) {
                me.show();
                hideShowItem = me.getHideShowMenuItem(false);
                hideShowItem.setChecked(true);
                hideShowItem.setCheckChangeDisabled(false);
            }

            // Next time we show our hide/show item, we need to rebuild the submenu
            me.rebuildChildColumnsMenu = true;

            // Update hideable/showable state of column menu items
            me.updateMenuDisabledState();
        },

        onColumnHide: function (column) {
            var me = this,
                hideShowItem;

            if (me.getVisibleCount() === 0) {
                me.hide();
                hideShowItem = me.getHideShowMenuItem(false);
                hideShowItem.setChecked(false);
                hideShowItem.setCheckChangeDisabled(true);
            }

            // Next time we show our hide/show item, we need to rebuild the submenu
            me.rebuildChildColumnsMenu = true;

            // Update hideable/showable state of column menu items
            me.updateMenuDisabledState();
        },

        onColumnMove: function(column) {
            // Next time we show our hide/show item, we need to rebuild the submenu
            this.rebuildChildColumnsMenu = true;
        },

        scopeReplacer: function (config, original, prop, setter) {
            var me = this,
                name = config[prop];

            if (typeof name === 'string') {
                prop = prop || 'sorterFn';
                setter = setter || 'setSorterFn';

                if (original === config) {
                    config = Ext.apply({}, config);
                }

                // The goal of this method is to be called only on the first use
                // and then replace itself (using the setter) to direct all future
                // calls to the proper method.
                config[prop] = function () {
                    // NOTE "this" is Sorter or Grouper!
                    var scope = me.resolveListenerScope(),
                        fn = scope && scope[name],
                        ret = 0;

                    if (fn) {
                        this[setter](fn.bind(scope));

                        ret = fn.apply(scope, arguments);
                    }
                    //<debug>
                    else if (!scope) {
                        Ext.raise('Cannot resolve scope for column ' + me.id);
                    }
                    else {
                        Ext.raise('No such method "' + name + '" on ' + scope.$className);
                    }
                    //</debug>

                    return ret;
                };
            }

            return config;
        }
    } // privates
});
