/**
 * @class Ext.chart.interactions.ItemHighlight
 * @extends Ext.chart.interactions.Abstract
 *
 * The 'itemhighlight' interaction allows the user to highlight series items in the chart.
 */
Ext.define('Ext.chart.interactions.ItemHighlight', {

    extend: 'Ext.chart.interactions.Abstract',

    type: 'itemhighlight',
    alias: 'interaction.itemhighlight',

    isItemHighlight: true,

    config: {

        gestures: {
            tap: 'onTapGesture',
            mousemove: 'onMouseMoveGesture',
            mousedown: 'onMouseDownGesture',
            mouseup: 'onMouseUpGesture',
            mouseleave: 'onMouseUpGesture'
        },

        /**
         * @cfg {Boolean} [sticky=false]
         * Disables mouse tracking.
         * Series items will only be highlighted/unhighlighted on mouse click.
         * This config has no effect on touch devices.
         */
        sticky: false
    },

    stickyHighlightItem: null,

    onMouseMoveGesture: function (e) {
        var me = this,
            oldItem = me.oldItem,
            isMousePointer = e.pointerType === 'mouse',
            item, tooltip;

        if (me.getSticky()) {
            return true;
        }

        if (isMousePointer && me.stickyHighlightItem) {
            me.stickyHighlightItem = null;
            me.highlight(null);
        }

        if (me.isDragging) {
            if (oldItem && isMousePointer) {
                oldItem.series.hideTooltip(oldItem);
                me.oldItem = null;
            }
        } else if (!me.stickyHighlightItem) {
            item = me.getItemForEvent(e);
            if (item !== me.getChart().getHighlightItem()) {
                me.highlight(item);
                me.sync();
            }

            if (isMousePointer) {
                // If we detected a mouse hit, show/refresh the tooltip
                if (item) {
                    tooltip = item.series.getTooltip();

                    if (tooltip) {
                        // If there was a different previously active item, ask it to hide its tooltip.
                        // Unless it's the same tooltip instance that we are about to show.
                        // In which case, we are just going to reposition it.
                        if (oldItem && oldItem !== item && oldItem.series.getTooltip() !== tooltip) {
                            oldItem.series.hideTooltip(oldItem, true);
                        }

                        if (tooltip.getTrackMouse()) {
                            item.series.showTooltip(item, e);
                        } else {
                            me.showUntracked(item);
                        }
                        me.oldItem = item;
                    }
                }
                // No mouse hit - schedule a hide for hideDelay ms.
                // If pointer enters another item within that time,
                // there will be no flickery reshow.
                else if (oldItem) {
                    oldItem.series.hideTooltip(oldItem);
                }
            }
            return false;
        }
    },

    highlight: function (item) {
        // This is its own function to make it easier for subclasses
        // to enhance the behavior. An alternative would be to listen
        // for the chart's 'itemhighlight' event.
        this.getChart().setHighlightItem(item);
    },

    showTooltip: function (e, item) {
        item.series.showTooltip(item, e);
        this.oldItem = item;
    },

    showUntracked: function (item) {
        var marker = item.sprite.getMarker(item.category),
            surface, surfaceXY, isInverseY,
            itemBBox, matrix;

        if (marker) {
            surface = marker.getSurface();
            isInverseY = surface.matrix.elements[3] < 0;
            surfaceXY = surface.element.getXY();
            itemBBox = Ext.clone(marker.getBBoxFor(item.index));
            if (isInverseY) {
                // The item.category for bar series will be 'items'.
                // The item.category for line series will be 'markers'.
                // 'items' are in the 'series' surface, which is flipped vertically
                // for cartesian series.
                // 'markers' are in the 'overlay' surface, which isn't flipped.
                // So for 'markers' we already have the bbox in a coordinate system
                // with the origin at the top-left of the surface, but for 'items'
                // we need to do a conversion.
                if (surface.getInherited().rtl) {
                    matrix = surface.inverseMatrix.clone().flipX().translate(item.sprite.attr.innerWidth, 0, true);
                } else {
                    matrix = surface.inverseMatrix;
                }
                itemBBox = matrix.transformBBox(itemBBox);
            }
            itemBBox.x += surfaceXY[0];
            itemBBox.y += surfaceXY[1];
            item.series.showTooltipAt(item,
                itemBBox.x + itemBBox.width * .5,
                itemBBox.y + itemBBox.height * .5
            );
        }
    },

    onMouseDownGesture: function () {
        this.isDragging = true;
    },

    onMouseUpGesture: function () {
        this.isDragging = false;
    },

    isSameItem: function (a, b) {
        return a && b && a.series === b.series && a.field === b.field && a.index === b.index;
    },

    onTapGesture: function (e) {
        var me = this;

        // A click/tap on an item makes its highlight sticky.
        // It requires another click/tap to unhighlight.
        if (e.pointerType === 'mouse' && !me.getSticky()) {
            return;
        }

        var item = me.getItemForEvent(e);

        if (me.isSameItem(me.stickyHighlightItem, item)) {
            item = null; // toggle
        }
        me.stickyHighlightItem = item;
        me.highlight(item);
    }
});