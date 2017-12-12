/**
 * A location for a list. See the superclass for more details.
 *
 * @since 6.5.0
 */
Ext.define('Ext.list.Location', {
    extend: 'Ext.dataview.Location',

    /**
     * @property {Boolean} isListLocation
     * @readonly
     * `true` in this class to identify an object as an instantiated list Location, or subclass thereof.
     */
    isListLocation: true,

    /**
     * @property {Boolean} dataItem
     * `true` if this item is backed by a record.
     */
    dataItem: false,

    /**
     * @property {Boolean} footer
     * `true` if the {@link #child} is a {@link Ext.dataview.List#groupFooter}.
     */
    footer: false,

    /**
     * @property {Ext.util.Group} [group]
     * The group for this location, if it is a {@link #header} or {@link #footer}.
     */
    group: null,

    /**
     * @property {Boolean} header
     * `true` if the {@link #child} is a {@link Ext.dataview.List#groupHeader}.
     */
    header: false,

    attach: function(source) {
        var me = this,
            item;

        me.callParent([source]);

        item = me.item;
        if (item) {
            me.header = item.$dataItem === 'header';
            me.footer = item.$dataItem === 'footer';

            if (me.header || me.footer) {
                me.group = me.getGroup();
            } else {
                me.dataItem = true;
            }
        }
    },

    clone: function() {
        var me = this,
            ret = me.callParent();

        ret.dataItem = me.dataItem;
        ret.footer = me.footer;
        ret.group = me.group;
        ret.header = me.header;

        return ret;
    },

    equals: function(other) {
        var me = this;

        if (other && other.isListLocation && other.view === me.view) {
            if (me.sourceElement) {
                return other.sourceElement === me.sourceElement;
            }
            // We only get here if this location refers to an unrendered location.

            // We'll always have a recordIndex (even if it's -1 due to virtual stores).
            // Therefore it's valid to check both record indices.
            // If they differ, the locations are not equal.
            return me.recordIndex !== other.recordIndex;
        }

        return false;
    }
});