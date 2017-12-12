/**
 * IndexBar is a component used to display a list of data (primarily an alphabet) which
 * can then be used to quickly navigate through a list (see {@link Ext.List}) of data.
 *
 * When a user taps on an item in the {@link Ext.IndexBar}, it will fire the {@link #index}
 * event.
 *
 * Here is an example of the usage in a {@link Ext.dataview.List List}:
 *
 *     @example
 *     Ext.define('Contact', {
 *         extend: 'Ext.data.Model',
 *         config: {
 *             fields: ['firstName', 'lastName']
 *         }
 *     });
 *
 *     var store = new Ext.data.JsonStore({
 *        model: 'Contact',
 *        sorters: 'lastName',
 *
 *        grouper: {
 *            groupFn: function(record) {
 *                return record.get('lastName')[0];
 *            }
 *        },
 *
 *        data: [
 *            {firstName: 'Screech', lastName: 'Powers'},
 *            {firstName: 'Kelly',   lastName: 'Kapowski'},
 *            {firstName: 'Zach',    lastName: 'Morris'},
 *            {firstName: 'Jessie',  lastName: 'Spano'},
 *            {firstName: 'Lisa',    lastName: 'Turtle'},
 *            {firstName: 'A.C.',    lastName: 'Slater'},
 *            {firstName: 'Richard', lastName: 'Belding'}
 *        ]
 *     });
 *
 *     var list = new Ext.List({
 *        fullscreen: true,
 *        itemTpl: '<div class="contact">{firstName} <strong>{lastName}</strong></div>',
 *
 *        grouped: true,
 *        indexBar: true,
 *        store: store,
 *        hideOnMaskTap: false
 *     });
 *
 */
Ext.define('Ext.dataview.IndexBar', {
    extend: 'Ext.Component',
    alternateClassName: 'Ext.IndexBar',
    xtype: 'indexbar',

    /**
     * @event index
     * Fires when an item in the index bar display has been tapped.
     * @param {Ext.dataview.IndexBar} this The IndexBar instance
     * @param {String} html The HTML inside the tapped node.
     * @param {Ext.dom.Element} target The node on the indexbar that has been tapped.
     */

    cachedConfig: {
        /**
         * @cfg {String/String[]} letters
         * The letters to show on the index bar.
         */
        letters: 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'
    },

    config: {
        /**
         * @cfg {Boolean/Object} animation
         * Set to `false` to disable animation when scrolling the list to the selected
         * position. This can also be an animation config object.
         */
        animation: true,

        /**
         * @cfg {Boolean/String} autoHide
         * Determines if the indexbar is hidden when not actively in use.
         * Value of 'true' will show/hide the indexbar on Over/Out events.
         * Value of 'press' will show/hide the indexbar on Press/Release events.
         */
        autoHide: false,

        /**
         * @cfg {Boolean} dynamic
         * Set to `true` to scroll the list as the index bar is manipulated, or `false`
         * to position the list when the index drag is complete.
         */
        dynamic: false,

        /**
         * @cfg {String} listPrefix
         * The prefix string to be used at the beginning of the list.
         * E.g: useful to add a "#" prefix before numbers.
         */
        listPrefix: null,

        /**
         * @cfg {Boolean} indicator
         * Determines if a indicator is used to show the current selected index
         */
        indicator: true
    },

    eventedConfig: {
        /**
         * @cfg {'vertical'/'horizontal'} direction
         * The layout direction.
         */
        direction: 'vertical'
    },

    top: 0,
    bottom: 0,
    left: 0,
    right: 0,

    inheritUi: true,

    autoHideCls: Ext.baseCSSPrefix + 'autohide',
    classCls: Ext.baseCSSPrefix + 'indexbar',
    horizontalCls: Ext.baseCSSPrefix + 'horizontal',
    indexedCls: Ext.baseCSSPrefix + 'indexed',
    indexedHorizontalCls: Ext.baseCSSPrefix + 'indexed-horizontal',
    indexedVerticalCls: Ext.baseCSSPrefix + 'indexed-vertical',
    indexedNoAutoHideCls: Ext.baseCSSPrefix + 'indexed-no-autohide',
    indicatorCls: Ext.baseCSSPrefix + 'indexbar-indicator',
    pressedCls: Ext.baseCSSPrefix + 'pressed',
    verticalCls: Ext.baseCSSPrefix + 'vertical',

    element: {
        reference: 'element',
        cls: Ext.baseCSSPrefix + 'unselectable',

        children: [{
            reference: 'bodyElement',
            cls: Ext.baseCSSPrefix + 'body-el'
        }]
    },

    initialize: function() {
        var me = this,
            bodyElement = me.bodyElement;

        me.callParent();

        bodyElement.addClsOnClick(me.pressedCls);

        bodyElement.on({
            touchstart: 'onTouchStart',
            touchend: 'onTouchEnd',
            mouseover: 'onMouseOver',
            mouseout: 'onMouseOut',
            drag: 'onDrag',
            dragEnd: 'onDragEnd',
            scope: me
        });
    },

    getVertical: function() {
        return this.getDirection() === 'vertical';
    },

    setVertical: function (vertical) {
        return this.setDirection(vertical ? 'vertical' : 'horizontal');
    },

    //-----------------------
    // Protected

    onAdded: function(parent, instanced) {
        var me = this;

        parent.el.addCls(me.indexedCls);

        me.parentListeners = parent.on({
            pinnedfooterheightchange: 'onPinnedFooterHeightChange',
            pinnedheaderheightchange: 'onPinnedHeaderHeightChange',
            verticaloverflowchange: 'onVerticalOverflowChange',

            destroyable: true,
            scope: me
        });

        me.callParent([parent, instanced]);
    },

    onRemoved: function (destroying) {
        var me = this,
            parent = me.parent;

        Ext.destroy(me.parentListeners);

        if (parent && !parent.destroying && !parent.destroyed) {
            parent.el.removeCls(me.indexedCls);
        }

        me.callParent([destroying]);
    },

    //-----------------------
    privates: {
        parentListeners: null,

        onDrag: function (e) {
            this.trackMove(e, false);
        },

        onDragEnd: function (e) {
            var me = this,
                indicator = me.getIndicator();

            me.trackMove(e, true);

            if (indicator && me.indicator) {
                me.indicator.hide();
            }
        },

        onMouseOver: function() {
            var me = this;

            me.$isMouseOver = true;

            if (me.shouldAutoHide('over')) {
                me.bodyElement.show();
            }
        },

        onMouseOut: function() {
            var me = this;

            me.$isMouseOver = false;

            if (me.shouldAutoHide('out')) {
                me.bodyElement.hide();
            }
        },

        onPinnedFooterHeightChange: function (list, height) {
            this.setBottom(height);
        },

        onPinnedHeaderHeightChange: function (list, height) {
            this.setTop(height);
        },

        onTouchStart: function(e) {
            var me = this;

            me.$isPressing = true;
            me.pageBox = me.bodyElement.getBox();

            me.onDrag(e);

            if (me.shouldAutoHide('press')) {
                me.bodyElement.show();
            }
        },

        onTouchEnd: function (e) {
            var me = this;

            me.$isPressing = false;

            if (me.shouldAutoHide('release')) {
                me.bodyElement.hide();
            }

            me.onDragEnd(e);
        },

        onVerticalOverflowChange: function (list, verticalOverflow) {
            this.setRight(verticalOverflow ? Ext.getScrollbarSize().width : 0);
        },

        scrollToClosestByIndex: function (index) {
            var me = this,
                list = me.parent,
                key = index.toLowerCase(),
                store = list.getStore(),
                groups = store.getGroups(),
                ln = groups.length,
                group, groupKey, i, closest, item, record;

            for (i = 0; i < ln; i++) {
                group = groups.getAt(i);
                groupKey = group.getGroupKey().toLowerCase();

                if (groupKey >= key) {
                    closest = group;
                    break;
                }

                closest = group;
            }

            if (closest) {
                record = closest.first();

                // Scrolling when infinite will already take the
                // header into account so we only want to get the
                // header when the list is not infinite. Also note,
                // header pinning is only applicable to infinite
                // lists so we don't have to worry about adjusting
                // for pinned headers.
                if (!list.getInfinite()) {
                    item = list.itemFromRecord(record).$header;
                }

                list.ensureVisible(record, {
                    animation: me.getAnimation(),
                    item: item,
                    align: {
                        y: 'start'
                    }
                });
            }
        },

        /**
         *
         * @param {'over'/'out'/'press'/'release'} trigger
         * @private
         */
        shouldAutoHide: function (trigger) {
            var me = this,
                autoHide = me.getAutoHide(),
                ret = false;

            // Automatic autohide detection
            // Desktop (hover events) will use over/out to show/hide
            // Mobile (touch based) will use press/release to show/hide
            if (autoHide) {
                // Press mode only by config or automatic autohide for mobile
                if (autoHide === 'pressed' || !Ext.os.is.Desktop) {
                    ret = trigger === 'press' || trigger === 'release';
                    // Automatic autohide for desktop
                } else {
                    // Over the index bar
                    // out of the index bar but not currently pressing down on the bar
                    // released the mouse and not hovered over the bar
                    ret = trigger === 'over' ||
                         (trigger === 'release' && !me.$isMouseOver) ||
                         (trigger === 'out' && !me.$isPressing);
                }
            }

            return ret;
        },

        syncIndicatorPosition: function (point, target, isValidTarget) {
            var me = this,
                isUsingIndicator = me.getIndicator(),
                direction = me.getDirection(),
                renderElement = me.renderElement,
                bodyElement = me.bodyElement,
                indicator = me.indicator,
                indicatorInner = me.indicatorInner,
                first = bodyElement.getFirstChild(),
                last = bodyElement.getLastChild(),
                indexbarWidth, indexbarHeight,indicatorSpacing,
                firstPosition, lastPosition,
                indicatorSize;

            if (isUsingIndicator && indicator) {
                indicator.show();

                if (direction === 'vertical') {
                    indicatorSize = indicator.getHeight();
                    indexbarWidth = bodyElement.getWidth();
                    indicatorSpacing = bodyElement.getMargin('lr');
                    firstPosition = first.getY();
                    lastPosition = last.getY();

                    if (point.y < firstPosition) {
                        target = first;
                    } else if (point.y > lastPosition) {
                        target = last;
                    }

                    if (isValidTarget) {
                        indicatorInner.setHtml(target.getHtml().toUpperCase());
                    }

                    indicator.setTop(target.getY() - renderElement.getY() - (indicatorSize / 2) + (target.getHeight()/2));
                    indicator.setRight(indicatorSpacing + indexbarWidth);
                } else {
                    indicatorSize = indicator.getWidth();
                    indicatorSpacing = bodyElement.getMargin('tb');
                    indexbarHeight = bodyElement.getHeight();
                    firstPosition = first.getX();
                    lastPosition = last.getX();

                    if (point.x < firstPosition) {
                        target = first;
                    } else if (point.x > lastPosition) {
                        target = last;
                    }

                    indicator.setLeft(target.getX() - renderElement.getX() - (indicatorSize / 2) + (target.getWidth()/2));
                    indicator.setBottom(indicatorSpacing + indexbarHeight);
                }

                indicatorInner.setHtml(target.getHtml().toUpperCase());
            }
        },

        trackMove: function (event, drop) {
            var me = this,
                el = me.bodyElement,
                pageBox = me.pageBox || (me.pageBox = me.el.getBox()),
                point = Ext.util.Point.fromEvent(event),
                target, isValidTarget;

            if (me.getDirection() === 'vertical') {
                if (point.y > pageBox.bottom || point.y < pageBox.top) {
                    return;
                }

                target = Ext.Element.fromPoint(pageBox.left + (pageBox.width / 2),
                    point.y);
                isValidTarget = target && target.getParent() === el;
            } else {
                if (point.x > pageBox.right || point.x < pageBox.left) {
                    return;
                }

                target = Ext.Element.fromPoint(point.x,
                    pageBox.top + (pageBox.height / 2));
                isValidTarget = target && target.getParent() === el;
            }

            if (target && isValidTarget) {
                if (me.getIndicator()) {
                    me.syncIndicatorPosition(point, target, isValidTarget);
                }

                if (drop || me.getDynamic()) {
                    me.scrollToClosestByIndex(target.dom.innerHTML);
                }
            }
        },

        //--------------------------------------------------------
        // Config properties

        // autoHide

        updateAutoHide: function(autoHide) {
            var me = this,
                parentEl = me.parent.el,
                autoHideCls = me.autoHideCls,
                indexedNoAutoHideCls = me.indexedNoAutoHideCls;

            // get this down to our element
            me.bodyElement.setVisibilityMode(Ext.Element.OPACITY);

            if (autoHide) {
                // Autohide requires opacity based visibility for event detection
                me.addCls(autoHideCls);
                me.bodyElement.hide();
                parentEl.removeCls(indexedNoAutoHideCls);
            } else {
                me.removeCls(autoHideCls);
                me.bodyElement.show();
                parentEl.addCls(indexedNoAutoHideCls);
            }
        },

        // direction

        updateDirection: function(direction) {
            var me = this,
                verticalCls = me.verticalCls,
                horizontalCls = me.horizontalCls,
                indexedVerticalCls = me.indexedVerticalCls,
                indexedHorizontalCls = me.indexedHorizontalCls,
                oldCls, newCls, oldIndexedCls, newIndexedCls;

            if (direction === 'vertical') {
                oldCls = horizontalCls;
                newCls = verticalCls;
                oldIndexedCls = indexedHorizontalCls;
                newIndexedCls = indexedVerticalCls;
            } else {
                oldCls = verticalCls;
                newCls = horizontalCls;
                oldIndexedCls = indexedVerticalCls;
                newIndexedCls = indexedHorizontalCls;
            }

            me.element.replaceCls(oldCls, newCls);
            me.bodyElement.replaceCls(oldCls, newCls);
            me.parent.element.replaceCls(oldIndexedCls, newIndexedCls);
        },

        // indicator

        updateIndicator: function (indicator) {
            var me = this,
                config = { cls: me.indicatorCls };

            if (indicator && indicator !== true) {
                config = Ext.apply(config, indicator);
            }

            if (indicator) {
                me.indicator = me.el.appendChild(config);
                me.indicatorInner = me.indicator.appendChild({
                    cls: me.indicatorCls + '-inner'
                });
                me.indicator.hide(false);
            }
            else if (me.indicator) {
                me.indicator.destroy();
                me.indicatorInner.destroy();

                me.indicator = me.indicatorInner = null;
            }
        },

        // letters

        updateLetters: function (letters) {
            var bodyElement = this.bodyElement,
                len = letters.length,
                i;

            bodyElement.setHtml('');

            if (letters) {
                // This loop needs to work for String or String[]
                for (i = 0; i < len; i++) {
                    bodyElement.createChild({
                        cls: Ext.baseCSSPrefix + 'indexbar-item',
                        html: letters[i]
                    });
                }
            }
        },

        // listPrefix

        updateListPrefix: function(listPrefix) {
            if (listPrefix && listPrefix.length) {
                this.bodyElement.createChild({
                    html: listPrefix
                }, 0);
            }
        },

        // ui

        updateUi: function(ui, oldUi) {
            var me = this,
                list = me.parent,
                listElement = list.element,
                indexedCls = me.indexedCls;

            // list element needs the x-indexed-[indexBarUi] class added so that it can pad
            // its items to account for the presence of the index bar
            if (oldUi) {
                listElement.removeCls(oldUi, indexedCls);
            }

            if (ui) {
                listElement.addCls(ui, indexedCls);
            }

            me.callParent([ui, oldUi]);
        }
    } // privates
});
