/**
 * A Column subclass which renders a checkbox in each column cell which toggles the truthiness of the associated data field on click.
 */
Ext.define('Ext.grid.column.Check', {
    extend: 'Ext.grid.column.Column',

    isCheckColumn: true,

    requires: [
        'Ext.util.Format',
        'Ext.grid.cell.Number'
    ],

    xtype: 'checkcolumn',

    /**
     * @event beforecheckchange
     * Fires when the UI requests a change of check status.
     * The change may be vetoed by returning `false` from a listener.
     * @param {Ext.grid.cell.Check} this The cell changing its state.
     * @param {Number} rowIndex The row index.
     * @param {Boolean} checked `true` if the box is to be checked.
     * @param {Ext.data.Model} record The record to be updated.
     * @param {Ext.event.Event} e The underlying event which caused the check change.
     * @param {Ext.grid.CellContext} e.position A {@link Ext.grid.CellContext CellContext} object
     * containing all contextual information about where the event was triggered.
     */

    /**
     * @event checkchange
     * Fires when the UI has successfully changed the checked state of a row.
     * @param {Ext.grid.cell.Check} this The cell changing its state.
     * @param {Number} rowIndex The row index.
     * @param {Boolean} checked `true` if the box is now checked.
     * @param {Ext.data.Model} record The record which was updated.
     * @param {Ext.event.Event} e The underlying event which caused the check change.
     * @param {Ext.grid.CellContext} e.position A {@link Ext.grid.CellContext CellContext} object
     */

    cachedConfig: {
        /**
         * @cfg {'top'/'right'/'bottom'/'left'} headerCheckboxAlign
         * Alignment of the header checkbox relative to the title text.
         */
        headerCheckboxAlign: 'bottom'
    },

    config: {
        /**
         * @cfg {Boolean} stopSelection
         * Prevent grid selection upon tap.
         */
        stopSelection: true,

        /**
         * @cfg {Boolean} [headerCheckbox=false]
         * Configure as `true` to display a checkbox below the header text.
         *
         * Clicking the checkbox will check/uncheck all records.
         */
        headerCheckbox: null
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
    classCls: Ext.baseCSSPrefix + 'checkcolumn',
    noHeaderCheckboxCls: Ext.baseCSSPrefix + 'no-header-checkbox',
    checkedCls: Ext.baseCSSPrefix + 'checked',
    hasTextCls: Ext.baseCSSPrefix + 'has-text',
    checkboxAlignCls: {
        top: Ext.baseCSSPrefix + 'checkbox-align-top',
        right: Ext.baseCSSPrefix + 'checkbox-align-right',
        bottom: Ext.baseCSSPrefix + 'checkbox-align-bottom',
        left: Ext.baseCSSPrefix + 'checkbox-align-left'
    },

    /**
     * @cfg text
     * @inheritdoc
     */
    text: '',

    /**
     * @cfg ignoreExport
     * @inheritdoc
     */
    ignoreExport: true,

    /**
     * @cfg cell
     * @inheritdoc
     */
    cell: {
        xtype: 'checkcell'
    },

    afterTitleTemplate: [{
        reference: 'checkboxElement',
        classList: [
            Ext.baseCSSPrefix + 'checkbox-el',
            Ext.baseCSSPrefix + 'font-icon'
        ]
    }],

    onColumnTap: function (e) {
        var me = this;

        if ((e.target === me.checkboxElement.dom) && !me.getDisabled()) {
            me.toggleAll(e);
        }

        me.callParent([e]);
    },

    toggleAll: function(e) {
        var me = this,
            checked = !me.allChecked;

        if (me.fireEvent('beforeheadercheckchange', me, checked, e) !== false) {
            me.doToggleAll(checked);
            me.setHeaderStatus(checked);
            me.fireEvent('headercheckchange', me, checked, e);
        }
    },

    doToggleAll: function(checked) {
        var me = this,
            store = me.getGrid().getStore();

        store.each(function(record) {
            me.setRecordChecked(record, checked);
        });
    },

    setRecordChecked: function (record, checked, e) {
        checked = !!checked;

        this.doSetRecordChecked(record, checked);

        // Must clear the "all checked" status in the column header
        if (checked) {
            this.updateHeaderState();
        } else {
            this.setHeaderStatus(checked);
        }
    },

    doSetRecordChecked: function(record, checked) {
        var dataIndex = this.getDataIndex();

        // Only proceed if we NEED to change
        if (record.get(dataIndex) != checked) {
            record.set(dataIndex, checked);
        }
    },

    areAllChecked: function() {
        var me = this,
            store = me.getGrid().getStore(),
            records, len, i;

        if (store && !store.isVirtualStore && store.getCount() > 0) {
            records = store.getData().items;
            len = records.length;
            for (i = 0; i < len; ++i) {
                if (!me.isRecordChecked(records[i])) {
                    return false;
                }
            }
            return true;
        }
    },

    isRecordChecked: function (record) {
        return record.get(this.getDataIndex());
    },

    updateHeaderState: function() {
        // This is called on a timer, so ignore if it fires after destruction
        if (!this.destroyed && (this.getHeaderCheckbox() !== false)) {
            this.setHeaderStatus(this.areAllChecked());
        }
    },

    setHeaderStatus: function(checked) {
        var me = this;

        // Will fire initially due to allChecked being undefined and using !==
        if (me.allChecked !== checked) {
            me.allChecked = checked;
            me.el.toggleCls(me.checkedCls, checked);
        }
    },

    updateDisabled: function(disabled, oldDisabled) {
        var me = this,
            grid = me.getGrid(),
            rows,
            len, i;

        me.callParent([disabled, oldDisabled]);

        if (grid) {
            rows = grid.getViewItems();
            len = rows.length;

            for (i = 0; i < len; i++) {
                rows[i].getCellByColumn(me).setDisabled(disabled);
            }
        }
    },

    updateHeaderCheckboxAlign: function (align, oldAlign) {
        var me = this,
            checkboxAlignCls = me.checkboxAlignCls;

        if (oldAlign) {
            me.removeCls(checkboxAlignCls[oldAlign]);
        }

        if (align) {
            //<debug>
            if (!checkboxAlignCls[align]) {
                Ext.raise("Invalid value for checkboxAlign: '" + align + "'");
            }
            //</debug>
            me.addCls(checkboxAlignCls[align]);
        }
    },

    updateHeaderCheckbox: function(headerCheckbox) {
        var me = this,
            grid = me.getGrid();

        me.el.toggleCls(me.noHeaderCheckboxCls, !headerCheckbox);
        me.setSortable(!headerCheckbox);

        // May be called in initialization before we are added to a grid.
        if (grid) {

            // Keep the header checkbox up to date
            me.updateHeaderState();
        }
    },

    updateText: function (text) {
        // Override: We do not want &nbsp; because it uncenters the checkbox.
        this.setHtml(text);
        this.toggleCls(this.hasTextCls, !!text);
    }
});
