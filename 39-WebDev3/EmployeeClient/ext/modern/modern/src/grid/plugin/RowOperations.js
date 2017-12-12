/**
 * The RowOperations plugin enables user to select rows in a {@link Ext.grid.Grid grid} and
 * then perform an operation (e.g. Delete) on them. This plugin adds a "Select" button
 * to the grid's title to start the selection process. This button is replaced by "Done"
 * while in selection mode. When selecting an extra `operation` button is displayed. By
 * default this button can be used to delete records.
 *
 * In addition, this plugin will ensure that the {@link Ext.grid.Grid#cfg!selectable checkbox}
 * selection method is enabled. The {@link #selectionColumn} config of this plugin can be
 * used to control this configuration.
 *
 * Customizing the RowOperations plugin to provide operations other then "Delete" can
 * be done with the `operation` config:
 *
 *      Ext.create({
 *          xtype: 'grid',
 *          fullscreen: true,
 *
 *          title: 'My Grid',
 *
 *          plugins: {
 *              rowoperations: {
 *                  operation: {
 *                      text: 'Archive',
 *                      handler: 'onRowOperation',
 *                      ui: 'alt'
 *                  }
 *              }
 *          },
 *
 *          store: store
 *
 *          columns: [
 *              // columns
 *          ]
 *      });
 *
 * Since the `operation` is a {@link Ext.Button button}, multiple operations can easily be
 * provided using the button's  {@link Ext.Button#cfg!menu menu}.
 */
Ext.define('Ext.grid.plugin.RowOperations', {
    extend: 'Ext.plugin.Abstract',
    alias: [
        'plugin.rowoperations',

        // legacy compat names:
        'plugin.multiselection',
        'plugin.gridmultiselection'
    ],

    alternateClassName: 'Ext.grid.plugin.MultiSelection',

    config: {
        /**
         * @cfg {Object/Ext.Button} operation
         * This button is displayed when the `selectButton` is tapped. By default it is
         * a "Delete" button (see `deleteText`). This button can be customized to perform
         * other operations than delete by replacing the `handler` (or using a `menu`).
         * @since 6.5.0
         */
        operation: {
            lazy: true,
            $value: {
                xtype: 'button',
                ui: 'alt decline',
                align: 'right',
                handler: 'me.onOperationTap',
                margin: '0 0 0 10'
            }
        },

        /**
         * @cfg {Object/Ext.Button} selectButton
         * This config drives the "Select" button added to the grid's title bar. This
         * button's handler is provided by the plugin and toggles the visibility of the
         * `operation` button.
         * @since 6.5.0
         */
        selectButton: {
            lazy: true,
            $value: {
                xtype: 'button',
                ui: 'alt',
                align: 'right',
                margin: '0 0 0 10'
            }
        },

        /**
         * @cfg {Object} selectionColumn
         * The default settings for the selection column. Used as a config object for the
         * {@link Ext.grid.selection.Model#checkbox}. You may provide a value for this
         * config in order to:
         *
         * + Change column width
         * + Show the selectionColumn by default
         * + Change the default cls or cellCls
         * + Etc.
         */
        selectionColumn: {
            sortable: false
        },

        /**
         * @cfg {Boolean} useTriggerButton
         * Determines whether or not the trigger button is show when the grid is loaded.
         * This most commonly be set to false if you wanted to have the selectionColumn
         * shown 100% of the time instead of hidden by default. You could show the {@link #selectionColumn}
         * by modifying its hidden value to be false.
         */
        useTriggerButton: true,

        /**
         * @cfg {String} triggerText
         * The text of the button used to display the `operation` and the `selectionColumn`.
         */
        triggerText: 'Select',

        /**
         * @cfg {String} cancelText
         * The text of the button used to hide the `operation` and the `selectionColumn`.
         */
        cancelText: 'Done',

        /**
         * @cfg {String} deleteText
         * The text of the button used to delete selected rows.
         */
        deleteText: 'Delete',

        /**
         * @cfg {Boolean} disableSelection
         * Set to `true` to disable selection styling on the owning grid when not in
         * selection mode.
         * @since 6.5.0
         */
        disableSelection: true,

        /**
         * @cfg {Boolean} selecting
         * Setting this config to `true` will show the `operation` and `selectionColumn`
         * while settings it `false` will hide them.
         * @private
         * @since 6.5.0
         */
        selecting: null
    },

    init: function (grid) {
        if (!this.useSelectButton()) {
            this.setSelecting(true);
        }

        if (this.getDisableSelection()) {
            grid.setDisableSelection(true);
        }
    },

    destroy: function () {
        this.setOperation(null);
        this.setSelectButton(null);

        this.callParent();
    },

    getRecords: function () {
        var grid = this.cmp;

        return grid.getSelections();
    },

    onOperationTap: function () {
        this.deleteSelectedRecords();
    },

    onTriggerTap: function() {
        this.setSelecting(!this.getSelecting());
    },

    // operation

    applyOperation: function (config, button) {
        return Ext.updateWidget(button, config, this, 'createOperation');
    },

    createOperation: function (config) {
        var me = this,
            ret = Ext.apply({
                text: me.getDeleteText()
            }, config);

        ret.plugin = me;

        if (ret.handler === 'me.onOperationTap') {
            ret.handler = 'onOperationTap';
            ret.scope = me;
        }

        return ret;
    },

    updateOperation: function (operation) {
        if (operation) {
            var selectButton = this.useSelectButton(),
                titleBar = this.cmp.getTitleBar(),
                container;

            if (titleBar) {
                if (selectButton) {
                    container = selectButton.getParent();

                    titleBar.insert(container.indexOf(selectButton), operation);
                }
                else {
                    titleBar.add(operation);
                }
            }
        }
    },

    // selectButton

    applySelectButton: function (config, button) {
        return Ext.updateWidget(button, config, this, 'createSelectButton');
    },

    createSelectButton: function (config) {
        var me = this,
            ret = Ext.apply({
                text: me.getTriggerText()
            }, config);

        ret.handler = 'onTriggerTap';
        ret.scope = me;

        return ret;
    },

    updateSelectButton: function (selectButton) {
        if (selectButton) {
            this.cmp.getTitleBar().add(selectButton);
        }
    },

    updateSelecting: function (selecting) {
        var me = this,
            grid = me.cmp,
            disableSelection = me.getDisableSelection(),
            operation = me.getOperation(),
            selectButton = me.useSelectButton(),
            selectionModel = grid.getSelectable();

        if (operation) {
            operation.setHidden(!selecting);
        }

        if (selectButton) {
            selectButton.setText(selecting ? me.getCancelText() : me.getTriggerText());
        }

        if (disableSelection) {
            grid.setDisableSelection(!selecting);
        }

        selectionModel.setCheckbox(selecting && me.getSelectionColumn());
        selectionModel.setMode(selecting ? 'multi' : 'single');

        if (disableSelection || !selecting) {
            selectionModel.deselectAll();
        }
    },

    privates: {
        deleteSelectedRecords: function () {
            var records = this.getRecords(),
                store = this.cmp.getStore();

            store.remove(records);
        },

        useSelectButton: function () {
            var me = this,
                titleBar = me.cmp.getTitleBar();

            return me.getUseTriggerButton() && titleBar && titleBar.getTitle() &&
                me.getSelectButton();
        }
    }
});
