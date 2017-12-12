/**
 * This is the base class for {@link Ext.grid.Grid grid} cells that contain only text.
 *
 * {@link Ext.grid.Row Rows} create cells based on the {@link Ext.grid.column.Column#cell}
 * config. Application code would rarely create cells directly.
 */
Ext.define('Ext.grid.cell.Text', {
    extend: 'Ext.grid.cell.Base',
    xtype: 'textcell',

    config: {
        /**
         * @cfg {Boolean} encodeHtml
         * Specify `false` to write HTML directly to the cell. Be aware that doing this
         * can expose your application to security issues if that content is not known to
         * be safe. User input can contain malicious content such as `script` tags and
         * should be scrubbed before directly rendering that HTML.
         */
        encodeHtml: true,

        /**
         * @cfg {String} rawValue
         * The text value of the cell. This value will be written to the cell differently
         * based on the {@link #encodeHtml} config. This config is automatically set as a
         * result of setting the {@link #value} config and is rarely set directly. This is
         * a separate config to avoid writting the same formatted result to the DOM.
         * @protected
         */
        rawValue: null,

        /**
         * @cfg {String} zeroValue
         *
         * A replacement value for 0.
         *
         * If the cell value is 0 and you want to display it or hide it then you can define
         * a not null value here.
         *
         * Set it as an empty string if you want to hide cells that have 0s.
         */
        zeroValue: null
    },

    getTemplate: function() {
        var template = this.callParent();

        template[0]["data-qoverflow"] = true;

        return template;
    },

    formatValue: function (v) {
        var me = this,
            context = me.refreshContext,
            column = context.column,
            zeroValue = me.getZeroValue(),
            format = column.getFormatter(),
            renderer, scope;

        if (context.summary) {
            renderer = column.getSummaryRenderer();

            if (renderer) {
                format = null; // ignore the non-summary formatter
                scope = context.scope;

                if (typeof renderer === 'string') {
                    v = Ext.callback(renderer, scope, [ v, context ], 0, column);
                }
                else {
                    v = renderer.call(scope || me, v, context);
                }
            }

            format = column.getSummaryFormatter() || format;
        }
        else if (v === 0 && zeroValue !== null) {
            v = zeroValue;
            format = null;
        }

        if (format) {
            v = format(v);
        }

        if (v != null) {
            v = String(v);
        } else {
            v = '';
        }

        return v;
    },

    printValue: function (v) {
        var me = this,
            was = me.refreshContext,
            s;

        // This method is mostly called outside row.refresh(), so we need to spin
        // up a context...
        me.refreshContext = me.beginRefresh(was);

        s = me.formatValue(v);

        if (me.getEncodeHtml()) {
            s = Ext.htmlEncode(s);
        }

        me.refreshContext = was;

        return s;
    },

    updateRawValue: function (rawValue) {
        var dom = this.bodyElement.dom,
            value = rawValue == null ? '' : rawValue;

        if (this.getEncodeHtml()) {
            dom.textContent = value;
        } else {
            dom.innerHTML = value;
        }
    },

    updateValue: function () {
            var me = this,
                was = me.refreshContext,
                row = me.row;

        // We may be called by binding after the store has already been nullified.
        // This can happen when binding to an association store if the parent record
        // is dropped.  If that is the case the row will have been removed from the grid
        // and cached for later use, so we can skip updating the dom.
        if (row && row.parent) {
            // We can be called by refresh() or directly such as when binding.
            // Make sure we have a context spun up...
            if (!was) {
                me.refreshContext = me.beginRefresh();
            }

            me.writeValue();

            me.refreshContext = was;
        }
    },

    updateZeroValue: function () {
        if (!this.isConfiguring) {
            this.refresh();
        }
    },

    writeValue: function () {
        var me = this,
            value = me.getValue();

        if (!(value = me.formatValue(value))) {
            // formatValue returns a string, so ! means '' not 0.
            value = me.getColumn().getEmptyText();
        }

        me.setRawValue(value);
    }
});
