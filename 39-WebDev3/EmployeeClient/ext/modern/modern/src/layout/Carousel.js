/**
 * @private
 */
Ext.define('Ext.layout.Carousel', {
    extend: 'Ext.layout.Auto',
    alias: 'layout.carousel',

    requires: [
        'Ext.Deferred'
    ],

    config: {
        /**
         * @cfg {Number} [visibleChildren=1] Number of children visible simultaneously
         * in the container.
         */
        visibleChildren: 1,

        /**
         * @cfg {Number} [frontIndex] Index of the child considered "front", 0-based.
         *
         * Default value is calculated thusly: for layouts with odd number of
         * {@link #cfg!visibleChildren} the index is calculated to be the center item, for even
         * number of visible children the index is `visibleChildren / 2`.
         */
        frontIndex: {
            $value: true,
            lazy: true
        },

        /**
         * @cfg {Boolean} [animation=true] Set to `false` to disable animated transitions.
         */
        animation: true
    },

    vertical: false,

    targetCls: Ext.baseCSSPrefix + 'layout-carousel',
    wrapCls: Ext.baseCSSPrefix + 'layout-carousel-wrap',
    itemCls: Ext.baseCSSPrefix + 'layout-carousel-item',
    singularCls: Ext.baseCSSPrefix + 'layout-carousel-singular',

    destroy: function() {
        var container = this.getContainer();

        Ext.destroy(container.carouselElement, this.activeAnim);

        this.callParent();
    },

    updateContainer: function(container, oldContainer) {
        var me = this;

        me.callParent([container, oldContainer]);

        container.bodyElement.addCls(me.wrapCls);

        container.carouselElement = container.getRenderTarget().appendChild({
            cls: me.targetCls
        });

        Ext.override(container, {
            privates: {
                getRenderTarget: function() {
                    return this.carouselElement;
                }
            }
        });
    },

    onContainerInitialized: function() {
        var me = this;

        me.callParent();

        // We don't want to stomp on front seat being set during configuration
        // but we need to make sure it's primed if it wasn't yet.
        if (!me.frontItem) {
            me.setFrontItem(me.getFrontIndex(), false);
        }
    },

    updateVisibleChildren: function(count) {
        var me = this,
            target = me.getContainer().getRenderTarget(),
            pct, items, item, i, len;

        items = me.getLayoutItems();
        pct = me.calcItemBasis(count) + '%';

        if (items.length > count) {
            target.setStyle('left', '-' + pct);
            target.setStyle('transform', 'translateX(' + pct + ')');
        }

        for (i = 0, len = items.length; i < len; i++) {
            item = items[i];

            item.el.setStyle('flex-basis', pct);
        }

        target.toggleCls(me.singularCls, count === 1);
    },

    applyFrontIndex: function(itemIdx) {
        var count, index;

        if (typeof itemIdx !== 'number') {
            count = this.getVisibleChildren();
            index = count - 1;

            itemIdx = !index ? index : index % 2 ? Math.floor(index / 2) + 1 : index / 2;
        }

        return itemIdx;
    },

    applyDuration: function(duration) {
        if (typeof duration !== 'number') {
            duration = parseInt(duration, 10) || 500;
        }

        return duration;
    },

    calcItemBasis: function(count) {
        count = count != null ? count : this.getVisibleChildren();

        return count === 1 ? 100 : !(count % 2) ? 100 / count : (100 / count).toFixed(5);
    },

    insertInnerItem: function(item, index) {
        var me = this;

        me.callParent([item, index]);

        if (index === 0) {
            me.frontItem = item;
        }

        item.el.setStyle('order', index + 1);
        item.el.setStyle('flex-basis', me.calcItemBasis() + '%');
    },

    getLayoutItemCount: function() {
        return this.getLayoutItems().length;
    },

    getLayoutItems: function() {
        return this.getContainer().getInnerItems();
    },

    getItemIndex: function(item) {
        return this.getContainer().innerIndexOf(item);
    },

    shiftIndex: function(index, increment) {
        var count = this.getLayoutItemCount();

        index += increment;

        if (increment < 0) {
            index = index < 0 ? count - 1 : index;
        } else if (increment > 0) {
            index = index >= count ? 0 : index;
        }

        return index;
    },

    getVisibleItems: function() {
        return this.visibleItems;
    },

    getEdgeItem: function(increment) {
        var items = this.getOrderedLayoutItems();
        return increment < 0 ? items[0] : items[items.length - 1];
    },

    getFirstVisibleItem: function() {
        return this.getVisibleItems()[0];
    },

    getLastVisibleItem: function() {
        var items = this.getVisibleItems();

        return items[items.length - 1];
    },

    getFrontItem: function() {
        return this.frontItem;
    },

    getFrontItemIndex: function() {
        return this.getItemIndex(this.getFrontItem());
    },

    getOrderedLayoutItems: function() {
        var items = Ext.Array.clone(this.getLayoutItems());
        return items.sort(this.sortByOrder);
    },

    setFrontItem: function(index, animate) {
        var me = this,
            container = me.getContainer(),
            target = container.getRenderTarget(),
            frontIndex = me.getFrontIndex(),
            visibleChildren = me.getVisibleChildren(),
            items, item, frontItem, oldFrontItem, oldFrontIndex,
            visibleItems, direction, basis, i, len, ret, deferred;

        items = me.getLayoutItems();

        if (items.length < visibleChildren) {
            return Ext.Deferred.getCachedResolved();
        }

        if (typeof index !== 'number') {
            index = items.indexOf(index);
        }

        basis = me.calcItemBasis();
        target.setStyle('left', '-' + basis + '%');

        oldFrontItem = me.getFrontItem();
        me.frontItem = frontItem = items[index];

        // Carousel seats are shifted one position to the left to avoid flickering,
        // and frontIndex needs to be adjusted accordingly
        frontIndex++;

        // Normalize items so that desired index is at the beginning
        items = items.slice(index).concat(items.slice(0, index));
        oldFrontIndex = items.indexOf(oldFrontItem);

        // Now shift items right to account for front seat index
        items = items.slice(-frontIndex).concat(items.slice(0, items.length - frontIndex));

        if (animate == null) {
            animate = me.getAnimation();
        }

        if (animate) {
            if (typeof animate === 'boolean') {
                animate = {};
            }
            // If old front item is less than half items away from new front (0 index)
            // then it was in front before, and we're moving backwards
            direction = oldFrontIndex > -1 && oldFrontIndex <= Math.floor(items.length / 2) ? 1 : -1;
            Ext.destroy(me.activeAnim);

            deferred = new Ext.Deferred();
            ret = deferred.promise;

            me.activeAnim = Ext.Animator.run(Ext.apply({
                element: target,
                to: {
                    transform: {
                        translateX: (basis * direction) + '%'
                    }
                },
                callback: function() {
                    me.orderItems(items);
                    deferred.resolve();
                    me.activeAnim = null;
                }
            }, animate));
        }

        me.visibleItems = visibleItems = [];
        for (i = 0, len = items.length; i < len; i++) {
            item = items[i];

            // We always keep one invisible item off the left side
            // so visible items start at index 1 and end at visibleChildren
            if (i > 0 && i <= visibleChildren) {
                visibleItems.push(item);
            }
            item.$carouselOrder = i + 1;
        }

        if (!animate) {
            me.orderItems(items);
            ret = Ext.Deferred.getCachedResolved();
        }

        visibleItems.sort(me.sortByOrder);

        return ret;
    },

    getMoveItem: function(increment) {
        var index = this.getFrontItemIndex();
        index = this.shiftIndex(index, increment);
        return this.getLayoutItems()[index];
    },

    cancelAnimation: function() {
        Ext.destroy(this.activeAnim);
    },

    move: function(increment, animate) {
        return this.setFrontItem(this.getMoveItem(increment), animate);
    },

    prev: function(animate) {
        return this.move(-1, animate);
    },

    next: function(animate) {
        return this.move(1, animate);
    },

    privates: {
        orderItems: function(items) {
            var len = items.length,
                i, item;

            for (i = 0; i < len; ++i) {
                item = items[i];
                item.el.setStyle('order', item.$carouselOrder + 1);
            }
        },

        sortByOrder: function(a, b) {
            return +a.$carouselOrder - b.$carouselOrder;
        }
    }
});
