/**
 * This {@link Ext.grid.Grid grid} plugin adds clipboard support to a grid.
 *
 * This class supports the following `{@link Ext.plugin.AbstractClipboard#formats formats}`
 * for grid data:
 *
 *  * `cell` - Complete field data that can be matched to other grids using the same
 *    {@link Ext.data.Model model} regardless of column order.
 *  * `text` - Cell content stripped of HTML tags.
 *  * `html` - Complete cell content, including any rendered HTML tags.
 *  * `raw` - Underlying field values based on `dataIndex`.
 *
 * The `cell` format is not valid for the `{@link Ext.plugin.AbstractClipboard#system system}`
 * clipboard format.
 */
Ext.define('Ext.grid.plugin.Clipboard', {
    extend: 'Ext.plugin.AbstractClipboard',

    alias: 'plugin.clipboard',
    requires: [
        'Ext.util.Format',
        'Ext.util.TSV'
    ],

    formats: {
        cell: {
            get: 'getCells'
        },
        html: {
            get: 'getCellData'
        },
        raw: {
            get: 'getCellData',
            put: 'putCellData'
        }
    },

    gridListeners: {
        initialize: 'onCmpReady'
    },

    getCellData: function (format, erase) {
        var cmp = this.getCmp(),
            selectable = cmp.getSelectable(),
            selection = selectable && selectable.getSelection(),
            ret = [],
            isRaw = format === 'raw',
            isText = format === 'text',
            data, dataIndex, lastRecord, column, record, row;

        if (selection) {
            selection.eachCell(function (location, colIdx, rowIdx) {
                column = location.column;
                record = location.record;

                // Do not copy data from ignored columns
                if(column.getIgnoreExport()) {
                    return;
                }

                if (lastRecord !== record) {
                    lastRecord = record;
                    ret.push(row = []);
                }

                dataIndex = column.getDataIndex();
                data = record.data[dataIndex];

                if (!isRaw) {
                    // printValue takes care of not yet rendered cells
                    data = column.printValue(data);
                    if (isText) {
                        data = Ext.util.Format.stripTags(data);
                    }
                }

                row.push(data);

                if (erase && dataIndex) {
                    record.set(dataIndex, null);
                }
            });
        }

        return Ext.util.TSV.encode(ret);
    },

    getCells: function (format, erase) {
        var cmp = this.getCmp(),
            selectable = cmp.getSelectable(),
            selection = selectable && selectable.getSelection(),
            ret = [],
            dataIndex, lastRecord, record, row;

        if (selection) {
            selection.eachCell(function (location) {
                record = location.record;
                if (lastRecord !== record) {
                    lastRecord = record;
                    ret.push(row = {
                        model: record.self,
                        fields: []
                    });
                }

                dataIndex = location.column.getDataIndex();

                row.fields.push({
                    name: dataIndex,
                    value: record.data[dataIndex]
                });

                if (erase && dataIndex) {
                    record.set(dataIndex, null);
                }
            });
        }

        return ret;
    },

    getTextData: function (format, erase) {
        return this.getCellData(format, erase);
    },

    putCellData: function (data, format) {
        var cmp = this.getCmp(),
            values = Ext.util.TSV.decode(data),
            recCount = values.length,
            colCount = recCount ? values[0].length : 0,
            columns = cmp.getHeaderContainer().getVisibleColumns(),
            store = cmp.getStore(),
            maxRowIdx = store ? store.getCount() - 1 : 0,
            maxColIdx = columns.length - 1,
            selectable = cmp.getSelectable(),
            selection = selectable && selectable.getSelection(),
            row, sourceRowIdx, sourceColIdx, column, record, columnIndex, recordIndex,
            dataObject, destination, dataIndex, startColumnIndex, startRecordIndex;

        if (maxRowIdx <= 0 || maxColIdx <= 0) {
            return;
        }

        if (selection) {
            selection.eachCell(function (c) {
                destination = c;
                return false;
            });
        }

        startColumnIndex = destination ? destination.columnIndex : 0;
        startRecordIndex = destination ? destination.recordIndex : 0;

        for (sourceRowIdx = 0; sourceRowIdx < recCount; sourceRowIdx++) {
            row = values[sourceRowIdx];
            recordIndex = startRecordIndex + sourceRowIdx;
            // If we are at the end of the destination store, break the row loop.
            if (recordIndex > maxRowIdx) {
                break;
            }
            record = store.getAt(recordIndex);

            dataObject = {};
            columnIndex = startColumnIndex;
            sourceColIdx = 0;

            // Collect new values in dataObject
            while (sourceColIdx < colCount && columnIndex <= maxColIdx) {
                column = columns[columnIndex];
                dataIndex = column.getDataIndex();

                // we skip ignored columns
                if (!column.getIgnoreExport()) {
                    // paste the content if the column allows us to do that, otherwise we ignore it
                    if (dataIndex && (format === 'raw' || format === 'text')) {
                        dataObject[dataIndex] = row[sourceColIdx];
                    }
                    sourceColIdx++;
                }
                columnIndex++;
            }

            // Update the record in one go.
            record.set(dataObject);
        }
    },

    putTextData: function (data, format) {
        this.putCellData(data, format);
    },

    getTarget: function(comp) {
        return comp.element;
    },

    privates : {
        validateAction : function(event) {
            var cmp = this.getCmp(),
                viewLocation = cmp.getNavigationModel().getLocation(),
                selectable = cmp.getSelectable(),
                checkColumn = selectable && selectable.getCheckbox();

            // if current location's column is not the checkbox selection column then allow copying
            if (viewLocation && viewLocation.actionable && checkColumn !== viewLocation.column) {
                return false;
            }
        }
    }
});
