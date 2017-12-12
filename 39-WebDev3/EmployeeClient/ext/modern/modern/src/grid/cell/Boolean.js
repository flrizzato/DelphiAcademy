/**
 * This class displays a boolean value in a {@link Ext.grid.Grid grid} cell. This cell type
 * is typically used by specifying {@link Ext.grid.column.Boolean} column type.
 *
 * {@link Ext.grid.Row Rows} create cells based on the {@link Ext.grid.column.Column#cell}
 * config. Application code would rarely create cells directly.
 */
Ext.define('Ext.grid.cell.Boolean', {
    extend: 'Ext.grid.cell.Text',
    xtype: 'booleancell',

    isBooleanCell: true,

    config: {
        /**
         * @cfg {String} falseText
         * The string to display when the value is falsey (but not undefined).
         * @locale
         */
        falseText: 'False',

        /**
         * @cfg {String} trueText
         * The string to display when the value is not falsey.
         * @locale
         */
        trueText: 'True',

        /**
         * @cfg {String} undefinedText
         * The string to display when the column value is `undefined`.
         * @locale
         */
        undefinedText: '\xA0'
    },

    updateColumn: function (column, oldColumn) {
        this.callParent([ column, oldColumn ]);

        if (column && column.isBooleanColumn) {
            var text = column.getFalseText();

            if (text !== null) {
                this.setFalseText(text);
            }

            text = column.getTrueText();
            if (text !== null) {
                this.setTrueText(text);
            }

            text = column.getUndefinedText();
            if (text !== null) {
                this.setUndefinedText(text);
            }
        }
    },

    updateFalseText: function () {
        if (!this.isConfiguring) {
            this.writeValue();
        }
    },


    updateTrueText: function () {
        if (!this.isConfiguring) {
            this.writeValue();
        }
    },

    updateUndefinedText: function () {
        if (!this.isConfiguring) {
            this.writeValue();
        }
    },

    formatValue: function (value) {
        var me = this;

        if (value === undefined) {
            value = me.getUndefinedText();
        }
        else if (!value || value === 'false') {
            value = me.getFalseText();
        }
        else {
            value = me.getTrueText();
        }

        return value;
    }
});
