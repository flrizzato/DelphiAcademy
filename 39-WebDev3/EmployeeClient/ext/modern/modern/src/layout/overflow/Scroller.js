/**
 * Overflow scroller enables scrolling within a box layout.
 *
 * Desktop devices use arrows by default. These arrows scroll the content.
 * Mobile devices use touch scrolling by default.
 *
 * For example usage see {@link Ext.layout.Box#overflow}.
 */
Ext.define('Ext.layout.overflow.Scroller', {
    alias: 'layout.overflow.scroller',

    mixins: [
        'Ext.mixin.Factoryable'
    ],

    requires: [
        'Ext.Tool',
        'Ext.util.ClickRepeater'
    ],

    config: {

        /**
         * @cfg {Boolean}
         * `true` to show the scroller arrow tools
         */
        arrows: Ext.os.is.Desktop,

        /**
         * @cfg {Object/Ext.Tool}
         */
        backwardTool: {
            xtype: 'tool',
            ui: 'boxscroller',
            focusable: false
        },

        /**
         * @cfg {Boolean/Object} [animation=false]
         * Animation to use when scrolling using the scroller tools
         */
        animation: true,

        /**
         * @cfg {Object/Ext.Tool}
         */
        forwardTool: {
            xtype: 'tool',
            ui: 'boxscroller',
            focusable: false
        },

        /**
         * @cfg {'stretch'/'center'/'start'/'end'}
         * The alignment of the {@link #forwardTool forward} and {@link #backwardTool backward}
         * tools on the box layout's cross axis
         */
        toolAlign: 'stretch',

        /**
         * @cfg {Number/'item'/'page'}
         * The number of pixels to scroll when a scroller tool is clicked
         * Also can be the string 'item' to scroll the next tab into view or 'page'
         * to scroll the next full page of items into view.
         * Default is 'item'
         */
        increment: 'item',

        /**
         * @cfg {Number}
         * The time (in milliseconds) to repeat a click when a scroller tool is pressed down
         */
        repeatInterval: 500,

        /**
         * @cfg {Boolean}
         * `true` to allow horizontal scrolling in response to vertical mouse wheel events
         */
        mouseWheel: true,

        /**
         * @cfg {Number}
         * The number of pixels to scroll on each mouse `wheel` event when mouse wheel
         * scrolling is {@link #mouseWheel enabled}.
         */
        wheelIncrement: 10,

        /**
         * @private
         */
        vertical: null,

        /**
         * @private
         * The owning {@link Ext.layout.Box Box Layout}
         */
        owner: null,

        /**
         * @private
         * The owning {@link Ext.layout.Box Box Layout}'s Container
         */
        container: null,

        /**
         * @private
         * @readonly
         */
        wrap: true
    },

    cls: Ext.baseCSSPrefix + 'boxscroller',
    bodyCls: Ext.baseCSSPrefix + 'boxscroller-body',

    toolAlignCls: {
        start: Ext.baseCSSPrefix + 'tool-align-start',
        end: Ext.baseCSSPrefix + 'tool-align-end',
        center: Ext.baseCSSPrefix + 'tool-align-center',
        stretch: Ext.baseCSSPrefix + 'tool-align-stretch'
    },

    orientMap: {
        false: {
            cls: Ext.baseCSSPrefix + 'horizontal',
            crossSize: 'height',
            getSize: 'getWidth',
            setCrossSize: 'setHeight',
            measureCross: 'h',
            scrollSize: 'scrollWidth',
            scrollbarMargin: 'margin-bottom',
            forwardTool: 'scroll-right',
            backwardTool: 'scroll-left',
            startPad: 'l',
            endPad: 'r',
            posProp: 'x'
        },
        true: {
            cls: Ext.baseCSSPrefix + 'vertical',
            crossSize: 'width',
            getSize: 'getHeight',
            setCrossSize: 'setWidth',
            measureCross: 'w',
            scrollSize: 'scrollHeight',
            scrollbarMargin: 'margin-right',
            forwardTool: 'scroll-down',
            backwardTool: 'scroll-up',
            startPad: 't',
            endPad: 'b',
            posProp: 'y'
        }
    },

    constructor: function (config) {
        var me = this;

        me.repeaters = [];

        me.initConfig(config);
        me.syncMouseWheel();

        //<debug>
        if (me.getOwner().getPack() !== 'start') {
            Ext.raise('Overflow scroller requires box layout to be packed \'start\'');
        }

        if (me.getContainer().getAutoSize() === false) {
            Ext.raise('Overflow scroller is not compatible with autoSize: false');
        }
        //</debug>

        me.getContainer().$onScrollerContainerVisible = me.sync.bind(me);
    },

    applyWrap: function () {
        var me = this,
            container = me.getContainer(),
            containerBody = me.getContainer().bodyElement,
            wrap = Ext.Element.create({
                cls: me.cls,
                children: [{
                    cls: me.bodyCls
                }]
            }),
            uiCls = 'boxscroller',
            bodyEl = wrap.first();

        // Add the wrap el as a "reference element" on the Container so it can participate
        // in the Container's "UI"
        container.boxScrollerElement = wrap;

        // Add the boxScrollerElement to the Container's uiReferences map so that it's CSS
        // class names will be automatically synchronized when the container's UI changes.
        container.uiReferences.boxScrollerElement = uiCls;

        container.initUiReference('boxScrollerElement', uiCls);

        if (!container.isConfiguring) {
            // If the container has already been configured its UI classes have already been
            // synchronized on its reference elements.  Since we are adding the boxScrollerElement
            // to the Container references we need too sync its UI cls now.
            container.syncUiCls({
                boxScrollerElement: 'boxscroller'
            });
        }

        this._body = bodyEl;

        wrap.insertBefore(containerBody);
        bodyEl.appendChild(containerBody);

        return wrap;
    },

    getBody: function () {
        var body = this._body;

        if (!body) {
            this.getWrap();
        }

        return this._body;
    },

    onActiveItemChange: function (render, item) {
        var me = this;

        if (me.getContainer().rendered) {
            me.ensureVisible(item);
        }
    },

    onActiveTabChange: function (render, tab) {
        var me = this;

        if (me.getContainer().rendered) {
            me.ensureVisible(tab);
        }
    },

    ensureVisible: function (item, animation) {
        var me = this;

        if (animation === undefined) {
            animation = me.getAnimation();
        }

        me.getOwner().ensureVisible(item, {animation: animation});
    },

    createForwardTool: function (config) {
        var me = this;

        return Ext.apply({
            $initParent: me.getContainer(),
            hidden: true,
            preventRefocus: true
        }, config);
    },

    createBackwardTool: function (config) {
        var me = this;

        return Ext.apply({
            $initParent: me.getContainer(),
            hidden: true,
            preventRefocus: true
        }, config);
    },

    scrollToItemOffset: function (offset, page) {
        var animate = this.getAnimation();

        this.getOwner().ensureVisible({
            offset: offset,
            scroll: page ? 'max' : 'min',
            animation: animate
        });
    },

    applyVertical: function (vertical) {
        return !!vertical;
    },

    updateVertical: function (vertical) {
        var me = this,
            orientMap = me.orientMap[vertical];

        me.getWrap().replaceCls(me.orientMap[!vertical].cls, orientMap.cls);

        if (me.getArrows()) {
            me.getForwardTool().setType(orientMap.forwardTool);
            me.getBackwardTool().setType(orientMap.backwardTool);
        }

        if (!me.isConfiguring) {
            me.syncContainerScrollable();
            me.syncMouseWheel();
        }
    },

    applyForwardTool: function (tool, oldTool) {
        var ct = this.getContainer();

        tool = Ext.updateWidget(oldTool, tool, this, 'createForwardTool');
        delete tool.$initParent;
        tool.ownerCmp = ct;

        tool.doInheritUi();
        tool.addUi('boxscroller-' + ct.xtype);

        return tool;
    },

    applyBackwardTool: function (tool, oldTool) {
        var ct = this.getContainer();

        tool = Ext.updateWidget(oldTool, tool, this, 'createBackwardTool');
        delete tool.$initParent;
        tool.ownerCmp = ct;

        tool.doInheritUi();
        tool.addUi('boxscroller-' + ct.xtype);

        return tool;
    },

    updateArrows: function (arrows) {
        var me = this,
            container = me.getContainer(),
            el = me.getWrap(),
            forward = me.getForwardTool(),
            backward = me.getBackwardTool();

        el.insertFirst(backward.el);
        el.append(forward.el);
        me.addClickListener(forward, me.onForwardClick);
        me.addClickListener(backward, me.onBackwardClick);

        if (container.rendered) {
            me.sync();
        } else {
            container.whenVisible('$onScrollerContainerVisible');
            container.on({
                painted: 'sync',
                scope: me,
                single: true
            });
        }
    },

    updateMouseWheel: function () {
        if (!this.isConfiguring) {
            this.syncMouseWheel();
        }
    },

    updateOwner: function (owner) {
        var me = this,
            container = owner.getContainer();

        container.on({
            activeitemchange: 'onActiveItemChange',
            activetabchange: 'onActiveTabChange', // is this needed?
            scope: me
        });

        Ext.override(container, {
            getRefItems: function (deep) {
                var refItems = this.callParent([deep]), // this, not me!
                    forward = me.getForwardTool(),      // me, not this!
                    backward = me.getBackwardTool();    // me, not this!

                refItems.push(forward, backward);

                return refItems;
            }
        });

        me.syncContainerScrollable();

        container.getScrollable().on('scrollend', 'sync', me);

        container.on('resize', 'sync', me);
        container.bodyElement.on('resize', 'sync', me);
    },

    updateToolAlign: function (toolAlign, oldToolAlign) {
        var map = this.toolAlignCls;

        this.getWrap().replaceCls(map[oldToolAlign], map[toolAlign]);
    },

    getContainer: function () {
        return this.getOwner().getContainer();
    },

    destroy: function () {
        var me = this;

        Ext.destroy(me.repeaters);
        me.getForwardTool().destroy();
        me.getBackwardTool().destroy();
        me.callParent();
    },

    privates: {
        addClickListener: function (tool, clickFn) {
            var me = this,
                repeat = me.getRepeatInterval(),
                repeater;

            if (repeat) {
                repeater = new Ext.util.ClickRepeater(Ext.apply({
                    target: tool,
                    preventDefault: true,
                    listeners: {
                        click: clickFn,
                        scope: me
                    }
                }, repeat));

                me.repeaters.push(repeater);
            } else {
                tool.on({
                    click: clickFn,
                    scope: me
                });
            }
        },

        doMoveFromClick: function(offset) {
            var me = this,
                scrollable = me.getContainer().getScrollable(),
                animate = me.getAnimation(),
                isVertical = me.getVertical(),
                increment = me.getIncrement(),
                dx, dy;

            if (isNaN(increment)) {
                me.scrollToItemOffset(offset, increment === 'page');
            } else {
                dx = !isVertical ? increment : 0;
                dy = isVertical ? increment : 0;
                scrollable.scrollBy(dx * offset, dy * offset, animate);
            }
        },

        onBackwardClick: function () {
            this.doMoveFromClick(-1);
        },

        onForwardClick: function () {
            this.doMoveFromClick(1);
        },

        onMouseWheel: function (e) {
            var me = this,
                scrollable = me.getContainer().getScrollable(),
                isVertical = me.getVertical(),
                delta = e.getWheelDelta() * me.getWheelIncrement() * -1,
                dx = !isVertical ? delta : 0,
                dy = isVertical ? delta : 0;

            scrollable.scrollBy(dx, dy);
        },

        syncMouseWheel: function () {
            var me = this,
                target = me.getContainer().getScrollerTarget(),
                fn = me.getMouseWheel() && !me.getVertical() ? 'on' : 'un';

            target[fn]('wheel', 'onMouseWheel', me);
        },

        syncContainerScrollable: function () {
            var me = this,
                isVertical = me.getVertical(),
                container = me.getContainer();

            container.setScrollable({
                element: me.getBody(),
                x: !isVertical,
                y: isVertical
            });
        },

        sync: function () {
            var me = this,
                container = me.getContainer(),
                target = container.getScrollerTarget(),
                orientMap = me.orientMap[me.getVertical()],
                elementSize = container.el[orientMap.getSize](),
                targetSize = target.dom[orientMap.scrollSize],
                forwardTool = me.getForwardTool(),
                backwardTool = me.getBackwardTool(),
                scrollbarSize = Ext.getScrollbarSize()[orientMap.crossSize],
                hasOverflow = targetSize > elementSize,
                posProp = orientMap.posProp,
                scrollable, maxPos, pos;

            if (me.getArrows() && hasOverflow) {
                forwardTool.show();
                backwardTool.show();

                scrollable = container.getScrollable();
                maxPos = scrollable.getMaxPosition()[posProp];
                pos = scrollable.getPosition()[posProp];
                
                forwardTool.setDisabled(pos + target.getPadding(orientMap.endPad) >= maxPos);
                backwardTool.setDisabled(pos <= target.getPadding(orientMap.startPad));
            } else {
                forwardTool.hide();
                backwardTool.hide();
            }

            // The stylesheet uses padding and negative margin on the x-boxscroller-body
            // element in order to hide the scrollbar or floating scroll indicator, whichever
            // the browser may have. On browsers that have scrollbars we need an additional
            // adjustment to account for the additional height that the scrollbar adds to
            // the x-boxscroller-body element.
            container.bodyElement.setStyle(
                orientMap.scrollbarMargin,
                hasOverflow ? (-scrollbarSize + 'px') : 0
            );

            // Forces the height of the box scroller, this will remove any flicker
            // when changing heights/resizing later
            me.getBody()[orientMap.setCrossSize](
                container.bodyElement.measure(orientMap.measureCross)
            );
        }
    }
});
