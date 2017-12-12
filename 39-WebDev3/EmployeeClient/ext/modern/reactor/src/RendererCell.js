/**
 * Use this component to render a React element inside of a grid cell.  
 *
 *      <Grid>
 *          <Column text="Actions" dataIndex="name">
 *              <RendererCell
 *                  renderer={(value, record) => (
 *                      <Button text={`Call ${value}`} handler={this.onCallClick.bind(this, record)}/>
 *                  )}
 *              />
 *          </Column>
 *      </Grid>
 *
 * RendererCell is automatically used when a Column contains a renderer prop. The following
 * is equivalent to the example above:
 * 
 *      <Grid>
 *          <Column 
 *              text="Actions" 
 *              dataIndex="name"
 *              renderer={(value, record) => (
 *                  <Button text={`Call ${value}`} handler={this.onCallClick.bind(this, record)}/>
 *              )}
 *          />
 *      </Grid>
 * @since 6.5.1
 */
Ext.define('Ext.reactor.RendererCell', {
    extend: 'Ext.grid.cell.Base',
    xtype: 'renderercell',

    config: {
        /**
         * @cfg {Function} renderer
         * A function that returns a React element or text to render. It is passed 
         * the following arguments:
         * @cfg {Object} renderer.value The data value for the current cell.
         * @cfg {Ext.data.Model} renderer.record The record for the current row.
         * @cfg {Number} renderer.dataIndex The dataIndex of the current column.
         * @cfg {Ext.grid.cell.Base} renderer.cell The current cell.
         * @cfg {Ext.grid.column.Column} renderer.column The current column.
         * @cfg {React.Element/String} renderer.return The React element or text to be
         * rendered.
         */
        renderer: null,

        /**
         * @cfg {Function} summaryRenderer
         * A function that returns a React element or text to render in the summary 
         * row. It is passed the following arguments:
         * @cfg {Object} renderer.value The data value for the current cell.
         * @cfg {React.Element/String} renderer.return The React element or text to be
         * rendered.
         */
        summaryRenderer: null,

        /**
         * @cfg {Boolean} forceWidth
         * `true` to measure the available width of the cell and set that
         * width on the underlying widget. If `false`, the widget width will auto
         * size.
         */
        forceWidth: false
    },

    setValue: function (value) {
        var me = this, 
            context = me.refreshContext, 
            column = context.column,
            needsSizing = false,
            scope = column.getScope(),
            markup, renderer, result;

        if (context.summary) {
            renderer = me.getSummaryRenderer() || column.getSummaryRenderer();
        }

        renderer = renderer || me.getRenderer() || column.getRenderer();

        if (renderer) {
            markup = renderer.call(scope, value, context.record, context.dataIndex, me, column);

            if (typeof markup === 'object') {
                // Ext.reactor.ReactDOM is set by reactor before the app is launched
                result = Ext.reactor.ReactDOM.render(markup, me.bodyElement.dom); 
            
                if (result.isWidget) {
                    needsSizing = result !== me.widget;
                    me.widget = result;
                }
            } else {
                if (markup == null) {
                    markup = '';
                }

                Ext.dom.Helper.overwrite(me.bodyElement, Ext.htmlEncode(markup.toString()));
                me.widget = null;
            }

            if (needsSizing && me.getForceWidth()) {
                me.setWidgetWidth(me.getWidth());
            }
        }

        return me;
    },

    updateWidth: function (width, oldWidth) {
        this.callParent(arguments);

        if (this.getForceWidth()) {
            this.setWidgetWidth(width);
        }
    },

    doDestroy: function () {
        this.widget = null;
        Ext.reactor.ReactDOM.unmountComponentAtNode(this.bodyElement.dom);
        this.callParent();
    },

    privates: {
        setWidgetWidth: function (width) {
            var me = this,
                el = me.bodyElement,
                widget, column, leftPad, rightPad;

            if (!me.rendered) {
                return;
            }

            widget = me.widget;

            if (widget) {
                column = me.getColumn();
                leftPad = parseInt(column.getCachedStyle(el, 'padding-left'), 10) || 0;
                rightPad = parseInt(column.getCachedStyle(el, 'padding-right'), 10) || 0;

                // Give the widget a reference to ourself to allow it to do extra measuring
                widget.measurer = column;
                widget.setWidth(width - leftPad - rightPad);
            }
        }
    }
});
