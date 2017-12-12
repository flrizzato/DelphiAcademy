/**
 * A component to show indication of an active item.
 */
Ext.define('Ext.Indicator', {
    extend: 'Ext.Component',
    xtype: 'indicator',

    config: {
        /**
         * @cfg {Number} activeIndex The index of the active indicator
         * dot.
         */
        activeIndex: null,

        /**
         * @cfg {Number} count The number of indicator dots to show.
         */
        count: null,

        /**
         * @cfg {String} [direction=horizontal] The direction the indicator
         * will be shown. Can be `horizontal` to have the dots be shown
         * in a row or `vertical` to have the dots be shown in a column.
         */
        direction: 'horizontal',

        /**
         * @cfg {String} [tapMode=direction] Controls what happens when clicked
         * on. The following are valid options:
         *
         * - **`direction`** Depending where the click happened, if on the right
         * half of the indicator, the next item will be made active else the
         * previous item will be made active.
         * - **`item`** If the click was on an indicator dot, the associated item
         * to that dot will be made active. If the click was not on an indicator
         * dot, then `direction` mode will be used.
         */
        tapMode: 'direction'
    },

    /**
     * @property {String} [activeCls=active] The CSS class name that will
     * be added to the active indicator dot.
     * @protected
     */
    activeCls: Ext.baseCSSPrefix + 'indicator-active',

    baseCls: Ext.baseCSSPrefix + 'indicator',

    /**
     * @property {String} [itemCls=x-indicator-item] The CSS class name
     * that will be added to the indicator dots.
     * @protected
     */
    itemCls: Ext.baseCSSPrefix + 'indicator-item',

    defaultBindProperty: 'activeIndex',

    twoWayBindable: [
        'activeIndex'
    ],

    /**
     * @private
     */
    isIndicator: true,

    /**
     * @property {Array} indicators An array of indicator dot
     * elements.
     * @private
     */

    /**
     * @event indicatortap
     * Fires when an indicator dot has been tapped and {@link #tapMode}
     * is set to `item`.
     * @param {Ext.Indicator} this
     * @param {Number} index The index of the indicator dot.
     * @param {Ext.dom.Elemnet} item The indicator dot item.
     */

    /**
     * @event next
     * Fires when this indicator is tapped on the right half
     * @param {Ext.Indicator} this
     */

    /**
     * @event previous
     * Fires when this indicator is tapped on the left half
     * @param {Ext.Indicator} this
     */

    constructor: function (config) {
        this.indicators = [];

        this.callParent([config]);
    },

    initialize: function () {
        this.callParent();

        this.element.on({
            tap: 'onTap',
            scope: this
        });
    },

    doDestroy: function () {
        Ext.destroy(this.indicators);

        this.callParent();
    },

    //<debug>
    applyActiveIndex: function (index) {
        var indicators = this.indicators,
            max = indicators.length - 1;

        if (index > max) {
            Ext.raise('Cannot set the active index greater than the number of indicators');
        }

        return index;
    },
    //</debug>

    updateActiveIndex: function (index, oldIndex) {
        var activeCls = this.activeCls,
            baseCls = this.baseCls,
            indicators = this.indicators,
            currentActiveItem = indicators[oldIndex],
            activeItem = indicators[index];

        if (currentActiveItem) {
            currentActiveItem.removeCls(activeCls);
        }

        if (activeItem) {
            activeItem.addCls(activeCls);
        }
    },

    updateCount: function (count) {
        var indicators = this.indicators;

        while (indicators.length < count) {
             this.doAdd()
        }

        while (indicators.length > count) {
            this.doRemove();
        }
    },

    //<debug>
    applyDirection: function (direction) {
        if (direction !== 'vertical' && direction !== 'horizontal') {
            Ext.raise('Invalid indicator direction provided: ' + direction);

            direction = 'horizontal';
        }

        return direction;
    },
    //</debug>

    updateDirection: function (newDirection, oldDirection) {
        this.element.replaceCls(oldDirection, newDirection, this.baseCls);
    },

    /**
     * Syncs the number of indicators and sets the active index.
     * @param {Number} count The number of indicators that needs to be shown. If `null`,
     * will skip syncing the number of indicators
     * @param {Number} activeIndex If specified, will set the {@link #activeIndex}.
     * @return {Ext.Indicator} this
     */
    sync: function (count, activeIndex) {
        if (Ext.isNumber(count)) {
            this.setCount(count);
        }

        if (Ext.isNumber(activeIndex)) {
            this.setActiveIndex(activeIndex);
        }

        return this;
    },

    /**
     * Adds an indicator dot at the end.
     * @return {Ext.Indicator} this
     */
    add: function () {
        var count = this.getCount();

        return this.setCount(++count);
    },

    /**
     * Removes all indicator dots.
     * @return {Ext.Indicator} this
     */
    removeAll: function () {
        return this.setCount(0);
    },

    /**
     * Removes the last indicator dot.
     * @return {Ext.Indicator} this
     */
    remove: function () {
        var count = this.getCount();

        return this.setCount(--count);
    },

    /**
     * Creates an indicator dot and addes it to {@link #indicators}.
     * @private
     * @return {Ext.Indicator} this
     */
    doAdd: function () {
        var indicators = this.indicators;

        indicators.push(this.element.createChild({
            tag: 'span',
            cls: this.itemCls
        }));

        return this;
    },

    /**
     * Removes and destroys the last indicator dot.
     * @private
     * @return {Ext.Indicator} this
     */
    doRemove: function () {
        var indicators = this.indicators,
            indicator = indicators.pop();

        if (indicator) {
            indicator.destroy();
        }

        return this;
    },

    /**
     * @private
     */
    onTap: function (e) {
        var mode = this.getTapMode();

        if (mode === 'item') {
            this.onTapItem(e);
        } else {
            this.onTapDirection(e);
        }
    },

    /**
     * Handles the tap when {@link #tapMode} is set to `item`.
     * @private
     */
    onTapItem: function (e) {
        var me = this,
            item = e.getTarget('.' + me.itemCls, 1, true),
            index;

        if (item) {
            index = me.indicators.indexOf(item);

            if (index !== -1) {
                me.fireEvent('indicatortap', me, index, item);
            }
        } else {
            //tap wasn't on a dot, go with direction
            me.onTapDirection(e);
        }
    },

    /**
     * Handles the tap when {@link #tapMode} is set to `direction`.
     * @private
     */
    onTapDirection: function (e) {
        var me = this,
            direction = me.getDirection(),
            touch = e.touch,
            box = me.element.getBox(),
            centerX = box.left + (box.width / 2),
            centerY = box.top + (box.height / 2),
            event = (direction === 'horizontal' && touch.pageX >= centerX) ||
                    (direction === 'vertical' && touch.pageY >= centerY) ? 'next' : 'previous';

        me.fireEvent(event, me);
    }
});
