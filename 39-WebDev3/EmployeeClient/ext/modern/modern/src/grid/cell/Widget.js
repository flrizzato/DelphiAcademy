/**
 * This class is used for {@link Ext.grid.Grid grid} cells that contain a child
 * {@link Ext.Component} or {@link Ext.Widget}. This cell type is typically used by
 * specifying {@link Ext.grid.column.Widget} column type.
 *
 * {@link Ext.grid.Row Rows} create cells based on the {@link Ext.grid.column.Column#cell}
 * config. Application code would rarely create cells directly.
 */
Ext.define('Ext.grid.cell.Widget', {
    extend: 'Ext.grid.cell.Base',
    xtype: 'widgetcell',

    isWidgetCell: true,

    config: {
        /**
         * @cfg {Boolean} forceWidth
         * `true` to measure the available width of the cell and set that
         * width on the underlying widget. If `false`, the widget width will auto
         * size.
         */
        forceWidth: false,

        /**
         * @cfg {Object} widget (required)
         * The config object for a {@link Ext.Component} or {@link Ext.Widget}.
         *
         * @cfg {String} widget.xtype (required) The type of component or widget to create.
         */
        widget: null
    },

    /**
     * @cfg align
     * @inheritdoc
     */
    align: 'center',

    /**
     * @property classCls
     * @inheritdoc
     */
    classCls: Ext.baseCSSPrefix + 'widgetcell',

    /**
     * @cfg selectable
     * @inheritdoc
     */
    selectable: false,

    getRefItems: function(deep) {
        var result = [],
            widget = this.getWidget();

        if (widget) {
            result.push(widget);
            if (deep && widget.getRefItems) {
                result.push.apply(result, widget.getRefItems(deep));
            }
        }

        return result;
    },

    setValue: function (value) {
        // If it's an object, its internals may have changed, but the simple
        // equality test of the config's setter will reject it, so
        // go directly to the updater.
        if (value && typeof value === 'object') {
            // we still need to update _value otherwise the Base cell refresh() will ignore us.
            this._value = value;
            this.updateValue(value);
        } else {
            if (value === undefined) {
                // The config system doesn't do well w/setFoo(undefined)
                value = null;
            }

            this.callParent([value]);
        }

        return this;
    },

    updateValue: function (value) {
        var me = this,
            widget = me.getWidget(), // this may create the widget & set defaultBindCfg
            defaultBindCfg = me.defaultBindCfg;

        if (defaultBindCfg && widget) {
            widget[defaultBindCfg.names.set](value);
        }
    },

    applyWidget: function (widget) {
        var me = this;

        if (widget) {
            widget = Ext.apply({
                ownerCmp: me
            }, widget);

            widget = Ext.create(widget);
        }

        return widget;
    },

    updateWidget: function(widget, oldWidget) {
        var me = this,
            defaultBindCfg;

        if (oldWidget) {
            me.widgetChangeListener = Ext.destroy(me.widgetChangeListener);
            oldWidget.measurer = null;
            oldWidget.destroy();
        }

        if (widget) {
            // in FF/Edge the cell body should only contain the widget canvas and nothing else
            // otherwise the widget is not visible
            me.bodyElement.setHtml('');
            me.bodyElement.appendChild(widget.element);

            if (me.getForceWidth()) {
                me.setWidgetWidth(me.getWidth());
            }

            defaultBindCfg = widget.defaultBindProperty;
            defaultBindCfg = widget.self.getConfigurator().configs[defaultBindCfg];
            me.defaultBindCfg = defaultBindCfg || null;

            //<debug>
            if (!defaultBindCfg || !widget[defaultBindCfg.names.get] ||
                    !widget[defaultBindCfg.names.set]) {
                Ext.raise('Invalid config "' + widget.defaultBindProperty + '" for ' +
                    widget.$className);
            }
            //</debug>

            if (me.dataIndex) {
                me.widgetChangeListener = widget.on({
                    change: 'onWidgetChange',
                    scope: me
                });
            }
        }
    },

    onWidgetChange: function (widget) {
        if (!this.refreshContext) {
            var me = this,
                record = me.getRecord(),
                defaultBindCfg = me.defaultBindCfg,
                dataIndex = me.dataIndex;

            if (record && !record.isSummaryRecord && dataIndex && defaultBindCfg) {
                record.set(dataIndex, widget[defaultBindCfg.names.get]());
            }
        }
    },

    updateWidth: function(width, oldWidth) {
        this.callParent([width, oldWidth]);
        if (this.getForceWidth()) {
            this.setWidgetWidth(width);
        }
    },

    onRender: function() {
        var me = this;

        if (me.getForceWidth()) {
            me.setWidgetWidth(me.getWidth());
        }
    },

    doDestroy: function() {
        this.setWidget(null);
        this.callParent();
    },

    privates: {
        setWidgetWidth: function(width) {
            var me = this,
                el = me.bodyElement,
                widget, column, leftPad, rightPad;

            if (!me.rendered) {
                return;
            }

            widget = me.getWidget();
            if (widget) {
                column = me.getColumn();
                leftPad = parseInt(column.getCachedStyle(el, 'padding-left'), 10) || 0;
                rightPad = parseInt(column.getCachedStyle(el, 'padding-right'), 10) || 0;
                // Give the widget a reference to ourself to allow it to do any extra measuring
                widget.measurer = column;
                widget.setWidth(width - leftPad - rightPad);
            }
        }
    }
});
