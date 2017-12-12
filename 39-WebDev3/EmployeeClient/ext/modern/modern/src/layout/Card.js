/**
 * Sometimes you want to show several screens worth of information but you've only got a small screen to work with.
 * TabPanels and Carousels both enable you to see one screen of many at a time, and underneath they both use a Card
 * Layout.
 *
 * Card Layout takes the size of the Container it is applied to and sizes the currently active item to fill the
 * Container completely. It then hides the rest of the items, allowing you to change which one is currently visible but
 * only showing one at once.
 *
 * Here we create a Panel with a Card Layout and later set the second item active (the active item index is zero-based,
 * so 1 corresponds to the second item). You might consider using a {@link Ext.tab.Panel tab panel} or a
 * {@link Ext.carousel.Carousel carousel}.
 *
 *     var panel = Ext.create('Ext.Panel', {
 *         layout: 'card',
 *         items: [
 *             {
 *                 html: "First Item"
 *             },
 *             {
 *                 html: "Second Item"
 *             },
 *             {
 *                 html: "Third Item"
 *             },
 *             {
 *                 html: "Fourth Item"
 *             }
 *         ]
 *     });
 *
 *     panel.setActiveItem(1); // make "Second Item" card visible
 *
 * You may specify an animation type for how activating cards looks to the user.  Without
 * an animation specified, the  cards will immediately appear as setActiveItem() is called.
 * The animation config may be a string or an object:of the form:
 *      animation: {
 *          type: one of 'cover', 'cube', 'fade', 'flip', 'pop', 'reveal', 'scroll', or 'slide',
 *          direction: (optional) one of 'up', 'down', 'left', or 'right'
 *      }
 *
 * Or the animation config may be a string, one of 'cover', 'cube', 'fade', 'flip', 'pop',
 * 'reveal', 'scroll', or 'slide'
 *
 * If an animation type is specified, the direction of the animation will be chosen based
 * upon the animation's initial direction left/right (horizontal) or up/down (vertical).
 * If the new card setActiveItem index is less than the current card's, then a right (or down)
 * animation will be applied.  If the new card index is greater, then a left (or up) animation
 * is applied.
 *
 * If you call setAnimation() with an animation config or instance, the animation direction will
 * obey what you specify  - the direction will not be chosen automatically.
 */
Ext.define('Ext.layout.Card', {
    extend: 'Ext.layout.Auto',

    alias: 'layout.card',

    type: 'card',

    isCard: true,

    /**
     * @cfg {String/Object} animation
     * The animation to use when switching between cards. The possible animation
     * types are:
     * - `'cover'`
     * - `'cube'`
     * - `'fade'`
     * - `'flip'`
     * - `'pop'`
     * - `'reveal'`
     * - `'scroll'`
     * - `'slide'`
     *
     * If a string, the value should be one of the above types. If an object, the type
     * should be one of the above types.
     *
     * @cfg {Number} [animation.duration]
     * The duration of the animation.
     * 
     * @cfg {String} [animation.direction]
     *
     * For animations that support a direction, the direction of the animation can be specified. The possible values are:
     * - `'horizontal'`
     * - `'vertical'`
     * - `'top'`
     * - `'right'`
     * - `'bottom'`
     * - `'left'`
     *
     * If a particular direction is specified (`top`/`right`/`bottom`/`left`), then the layout
     * will always animate in that direction. If `horizontal`/`vertical` is used, the direction
     * will be determined based on the position in the items collection. If the new item is before the
     * current item, the direction will be "back" (`left`/`top`). If the new item is after the current item,
     * the direction will be "forward" (`right`/`top`).
     */

    /**
     * @event activeitemchange
     * @preventable
     * Fires when an card is made active
     * @param {Ext.layout.Card} this The layout instance
     * @param {Mixed} newActiveItem The new active item
     * @param {Mixed} oldActiveItem The old active item
     */

    config: {
        /**
         * @cfg {Ext.Indicator} indicator
         * Creates an {@link Ext.Indicator} instance that can be used to visualize
         * the number of items and which item is active.
         */
        indicator: {
            lazy: true,
            $value: {
                xtype: 'indicator',
                flex: 1
            }
        }
    },

    /**
     * @cfg {Boolean} [deferRender=true]
     * By default, items not initially shown in the Card layout are rendered when first shown.  This provides
     * a performance benefit, but if the hidden items contain components that are bound, the bindings do not
     * immediately take effect.  If you have a form with bnound fields that spans several cards, the initially
     * hidden items won't have their values bound and validation will not be done properly.  In those cases,
     * you will want to set deferRender to false.
     */
    deferRender: true,

    cls: Ext.baseCSSPrefix + 'layout-card',

    itemCls: Ext.baseCSSPrefix + 'layout-card-item',

    requires: [
        'Ext.Indicator',
        'Ext.layout.card.fx.*'
    ],

    /**
     * @private
     */
    applyAnimation: function(animation) {
        return animation ? new Ext.Factory.layoutCardFx(animation) : null;
    },

    /**
     * @private
     */
    updateAnimation: function(animation, oldAnimation) {
        var me = this,
            direction;

        me.autoDirection = null;

        if (animation && animation.isAnimation) {
            animation.setLayout(me);

            direction = animation.getDirection();
            if (!direction || me.autoDirectionMap[direction]) {
                me.autoDirection = direction || 'horizontal';
                // If we got horizontal or vertical, clear it out
                animation.setDirection(null);
            }
        }

        if (oldAnimation) {
            oldAnimation.destroy();
        }
    },

    applyIndicator: function (indicator, currentIndicator) {
        return Ext.updateWidget(currentIndicator, indicator, this, 'createIndicator');
    },

    createIndicator: function (indicator) {
        return Ext.apply({
            ownerCmp: this.getContainer()
        }, indicator);
    },

    updateIndicator: function (indicator) {
        if (indicator) {
            var container = this.getContainer(),
                innerItems = container.getInnerItems(),
                activeItem = container.getActiveItem();

            indicator
                .sync(innerItems.length, innerItems.indexOf(activeItem))
                .on({
                    indicatortap: 'onIndicatorTap',
                    next: 'next',
                    previous: 'previous',
                    scope: this
                });
        }
    },

    onContainerInitialized: function() {
        var me = this,
            container = me.getContainer(),
            firstItem = container.getInnerAt(0),
            activeItem = container.getActiveItem();

        me.callParent();

        if (activeItem) {
            // Don't call showItem here, since the component will get rendered by the
            // render cycle
            activeItem.show();

            if (firstItem && firstItem !== activeItem) {
                firstItem.hide();
            }
        }

        container.on('activeitemchange', 'onContainerActiveItemChange', me);
    },

    /**
     * @private
     */
    onContainerActiveItemChange: function(container, newItem, oldItem) {
        var me = this,
            innerItems = container.getInnerItems(),
            newIndex = innerItems.indexOf(newItem),
            oldIndex = innerItems.indexOf(oldItem),
            animation = me.getAnimation(),
            autoDirection = me.autoDirection,
            horizontal = autoDirection && autoDirection === 'horizontal',
            direction;

        if (autoDirection && newIndex !== -1 && oldIndex !== -1) {
            if (newIndex < oldIndex) {
                direction = horizontal ? 'right' : 'up';
            } else {
                direction = horizontal ? 'left' : 'down';
            }
            animation.setDirection(direction);
        }

        me.fireEventedAction('activeitemchange', [me, newItem, oldItem],
            'doActiveItemChange', me);
    },

    onItemInnerStateChange: function(item, isInner, destroying) {
        this.callParent([item, isInner, destroying]);

        var container = this.getContainer(),
            activeItem = container.getActiveItem();

        if (isInner) {
            if (activeItem !== container.innerIndexOf(item) && activeItem !== item && item !== container.pendingActiveItem) {
                item.hide();
            }
        } else {
            if (!destroying && !item.destroyed && item.destroying !== true) {
                item.show();
            }
        }
    },

    /**
     * @private
     */
    doActiveItemChange: function(me, newActiveItem, oldActiveItem) {
        var indicator = me.getConfig('indicator', null, true),
            container, innerItems;

        if (oldActiveItem && !oldActiveItem.destroyed) {
            oldActiveItem.hide();
        }

        if (newActiveItem && !newActiveItem.destroyed) {
            me.showItem(newActiveItem);

            if (indicator) {
                container = this.getContainer();
                innerItems = container.getInnerItems();

                indicator.setActiveIndex(innerItems.indexOf(newActiveItem));
            }
        }
    },

    onItemAdd: function (item, index) {
        var indicator,
            style;

        this.callParent([item, index]);

        if (item.isInnerItem()) {
            indicator = this.getConfig('indicator', null, true);

            if (indicator) {
                indicator.add();
            }

            // Clear any styles as they come in, we should be
            // 100% 100%
            style = item.element.dom.style;
            style.width = style.height = '';
        }
    },

    onItemRemove: function (item, index, destroying) {
        var indicator,
            w, h;

        this.callParent([item, index, destroying]);

        if (item.isInnerItem()) {
            indicator = this.getConfig('indicator', null, true);

            if (indicator) {
                indicator.remove();
            }

            // Restore inline sizes on the way out
            w = item.getWidth();
            h = item.getHeight();

            item.setWidth(null).setWidth(w);
            item.setHeight(null).setHeight(w);
        }
    },

    /**
     * Moves to the next item if not on the last item.
     */
    next: function () {
        var container = this.getContainer(),
            activeItem = container.getActiveItem(),
            innerItems = container.getInnerItems(),
            index = innerItems.indexOf(activeItem);

        activeItem = innerItems[index + 1];

        if (activeItem) {
            container.setActiveItem(activeItem);
        }
    },

    /**
     * Moves to the previous item if not on the first item.
     */
    previous: function () {
        var container = this.getContainer(),
            activeItem = container.getActiveItem(),
            innerItems = container.getInnerItems(),
            index = innerItems.indexOf(activeItem);

        activeItem = innerItems[index - 1];

        if (activeItem) {
            container.setActiveItem(activeItem);
        }
    },

    onIndicatorTap: function (indicator, index) {
        var container = this.getContainer();

        container.setActiveItem(index);
    },

    destroy: function() {
        Ext.destroy(this.getAnimation(), this.getIndicator());

        this.callParent();
    },

    privates: {
        autoDirectionMap: {
            horizontal: 1,
            vertical: 1
        },

        renderInnerItem: function(item, asRoot) {
            if (!this.deferRender || this.getContainer().getActiveItem() === item) {
                this.callParent([item, asRoot]);
            }
        },

        showItem: function(item) {
            item.show();
            item.setRendered(true, true);
        }
    }
});
