/**
 * Base class for selections which may be of four subtypes:
 *
 * - {@link Ext.dataview.selection.Records Records} A Collection of {@link Ext.data.Model Records}s.
 * - {@link Ext.dataview.selection.Rows Rows} Ranges of row indices.
 * - {@link Ext.grid.selection.Cells Cells} A rectangular range of cells defined by a start
 *   record/column and an end record/column.
 * - {@link Ext.grid.selection.Columns Columns} An array of columns in which all records
 *   are included.
 *
 */
Ext.define('Ext.dataview.selection.Selection', {
    mixins: [
        'Ext.mixin.Factoryable'
    ],

    factoryConfig: {
        type: 'selection',
        defaultType: 'records',
        instanceProp: 'isSelection'
    },

    /**
     * @property {Boolean} isSelection
     * This property indicates that this is a DataView selection object.
     * @readonly
     */
    isSelection: true,

    config: {
        /**
         * @private
         * The owning SelectionModel
         */
        selectionModel: null
    },

    constructor: function(config) {
        // Allow simple construction passing the view
        if (config.isDataView) {
            config = {
                selectionModel: config.getSelectionModel()
            };
        }
        this.initConfig(config);
        //<debug>
        if (!this.getSelectionModel()) {
            Ext.raise('Selection must be configured with a SelectionModel');
        }
        //</debug>
    },

    destroy: function() {
        this.clear();
        this.callParent();
    },

    /**
     * Clones this selection object.
     * @return {Ext.dataview.selection.Selection} A clone of this instance.
     * @method clone
     */

    /**
     * Clears the selection represented by this selection object.
     * @private
     * @method clear
     */

    /**
     * Calls the passed function for each selected {@link Ext.data.Model record}.
     *
     * @param {Function} fn The function to call. If this returns `false`, the iteration is
     * halted with no further calls.
     * @param {Ext.data.Model} fn.record The current record.
     * @param {Object} [scope] The context (`this` reference) in which the function is executed.
     * Defaults to this Selection object.
     * @method eachRow
     */

    /**
     * Calls the passed function for each selected cell from top left to bottom right
     * iterating over columns within each row.
     *
     * @param {Function} fn The function to call. If this returns `false`, the iteration is
     * halted with no further calls.
     * @param {Ext.grid.Location} fn.context The CellContext representing the current cell.
     * @param {Number} fn.columnIndex The column index of the current cell.
     * @param {Number} fn.rowIndex The row index of the current cell.
     * @param {Object} [scope] The context (`this` reference) in which `fn` is executed.
     * Defaults to this Selection object.
     * @method eachCell
     */

    /**
     * Calls the passed function for each selected column from left to right.
     *
     * @param {Function} fn The function to call. If this returns false, the iteration is
     * halted with no further calls.
     * @param {Ext.grid.column.Column} fn.column The current column.
     * @param {Number} fn.columnIndex The index of the current column. *Note that in a
     * locked grid, this is relative to the outermost grid encompassing both sides*.
     * @param {Object} [scope] The context (`this` reference) in which `fn` is executed.
     * Defaults to this Selection object.
     * @method eachColumn
     */

     /**
      * Called when selection is completed.
      * @method onSelectionFinish
      * @private
      */

     privates: {
         applySelectionModel: function(selectionModel) {
            var view;

             // We use the topmost (possible locking View) view
             // We store an unprefixed view property.
            this.view = view = selectionModel.getView();

            // If we are acting for a grid, we must look at the topmost grid in a locking assembly.
            if (view.isGrid) {
                this.view = view.ownerGrid;
            }

            return selectionModel;
        }
     }
});
