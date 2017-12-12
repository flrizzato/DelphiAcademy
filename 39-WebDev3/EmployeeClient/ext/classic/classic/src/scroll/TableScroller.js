Ext.define('Ext.scroll.TableScroller', {
    extend: 'Ext.scroll.Scroller',
    alias: 'scroller.table',

    config: {
        lockingScroller: null
    },

    privates: {
        getEnsureVisibleXY: function (el, options) {
            var lockingScroller = this.getLockingScroller(),
                position = this.getPosition(),
                newPosition;

            if (el && el.element && !el.isElement) {
                options = el;
                el = options.element;
            }

            options = options || {};

            if (lockingScroller) {
                position.y = lockingScroller.position.y;
            }

            newPosition = Ext.fly(el).getScrollIntoViewXY(this.getElement(), position.x, position.y);
            newPosition.x = (options.x === false) ? position.x : newPosition.x;

            if (lockingScroller) {
                newPosition.y = (options.y === false) ? position.y : Ext.fly(el).getScrollIntoViewXY(lockingScroller.getElement(), position.x, position.y).y;
            }

            return newPosition;
        },

        doScrollTo: function(x, y, animate) {
            var lockingScroller,
                lockedPromise,
                ret;

            if (y != null) {
                lockingScroller = this.getLockingScroller();

                if (lockingScroller) {
                    lockedPromise = lockingScroller.doScrollTo(null, y, animate);
                    y = null;
                }
            }

            ret = this.callParent([x, y, animate]);

            if (lockedPromise) {
                ret = Ext.Promise.all([ret, lockedPromise]);
            }
            return ret;
        }
    }

});
