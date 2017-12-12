/**
 * A Component used by {@link Ext.dataview.Abstract Data Views},
 * {@link Ext.dataview.List Lists} and {@link Ext.grid.Grid Grids} for displaying
 * {@link Ext.dataview.Abstract#emptyText empty text} when the store contains no records.
 *
 * Instances of this class should not be created directly as they are created automatically
 * as needed by their owning Data View
 */
Ext.define('Ext.dataview.EmptyText', {
    extend: 'Ext.Component',
    xtype: 'emptytext',
    classCls: Ext.baseCSSPrefix + 'emptytext',
    inheritUi: true,
    html: 'No data to display',
    top: 0,
    right: 0,
    bottom: 0,
    left: 0
});