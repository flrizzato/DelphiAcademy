/**
 * The slider component utilized by `Ext.field.Slider`.
 *
 * The slider is a way to allow the user to select a value from a given numerical range.
 * You might use it for choosing a percentage, combine two of them to get min and max
 * values, or use three of them to specify the hex values for a color.
 *
 * Each slider contains a single 'thumb' that can be dragged along the slider's length to
 * change the value.
 *
 * ## Simple Slider
 *
 *     @example
 *     Ext.create({
 *         xtype: 'container',
 *         fullscreen: true,
 *         padding: 20,
 *
 *         items: [{
 *             xtype: 'slider',
 *             value: 42
 *         }]
 *     });
 *
 * ## Slider with Bound Value
 *
 * This slider does not incorporate two-way binding by default.  Please utilize `publishes` or
 * `twoWayBindable` in order to publish bound values.  You can also use Ext.field.Slider
 * directly for a more feature-rich component.
 *
 *     @example
 *     Ext.create({
 *         xtype: 'container',
 *         fullscreen: true,
 *         padding: 20,
 *
 *         layout: {
 *             type: 'hbox',
 *             pack: 'center'
 *         },
 *         
 *         viewModel: {
 *             data: {
 *                 value: 42
 *             }
 *         },
 *
 *         items: [{
 *             xtype: 'slider',
 *             value: '{value}'
 *         }, {
 *             xtype: 'label',
 *             bind: '{value}'
 *         }]
 *     })
 *
 */
Ext.define('Ext.slider.Slider', {
    extend: 'Ext.Component',
    xtype: 'slider',

    requires: [
        'Ext.slider.Thumb',
        'Ext.fx.easing.EaseOut'
    ],

    /**
    * @event change
    * Fires when the value changes
    * @param {Ext.slider.Slider} this
    * @param {Ext.slider.Thumb} thumb The thumb being changed
    * @param {Number} newValue The new value
    * @param {Number} oldValue The old value
    */

    /**
    * @event dragstart
    * Fires when the slider thumb starts a drag
    * @param {Ext.slider.Slider} this
    * @param {Ext.slider.Thumb} thumb The thumb being dragged
    * @param {Array} value The start value
    * @param {Ext.event.Event} e
    */

    /**
    * @event drag
    * Fires when the slider thumb starts a drag
    * @param {Ext.slider.Slider} this
    * @param {Ext.slider.Thumb} thumb The thumb being dragged
    * @param {Ext.event.Event} e
    */

    /**
    * @event dragend
    * Fires when the slider thumb starts a drag
    * @param {Ext.slider.Slider} this
    * @param {Ext.slider.Thumb} thumb The thumb being dragged
    * @param {Array} value The end value
    * @param {Ext.event.Event} e
    */
    config: {
        // @cmd-auto-dependency { defaultType: "Ext.slider.Thumb", aliasPrefix:'widget.',typeProperty: 'xtype' }
        /**
         * @cfg {Object} thumbDefaults The config object to factory {@link Ext.slider.Thumb} instances
         * @accessor
         */
        thumbDefaults: {
            xtype: 'thumb',
            inheritUi: true,
            translatable: {
                easingX: {
                    duration: 300,
                    type: 'ease-out'
                }
            }
        },

        /**
         * @cfg {Number} increment The increment by which to snap each thumb when its value changes. Any thumb movement
         * will be snapped to the nearest value that is a multiple of the increment (e.g. if increment is 10 and the user
         * tries to move the thumb to 67, it will be snapped to 70 instead)
         * @accessor
         */
        increment: 1,

        /**
         * @cfg {Number/Number[]} value The value(s) of this slider's thumbs. If you pass
         * a number, it will assume you have just 1 thumb.
         * @accessor
         */
        value: 0,

        /**
         * @cfg {Boolean} [valueIsArray=false]
         * If `false` the {@link #value} will be a number when a single value is given
         * (even if it's an array containing that single value), and an array,
         * when an array of values was given.
         * If `true`, the {@link #value} will always be converted to an array.
         */
        valueIsArray: false,

        /**
         * @cfg {Number} minValue The lowest value any thumb on this slider can be set to.
         * @accessor
         */
        minValue: 0,

        /**
         * @cfg {Number} maxValue The highest value any thumb on this slider can be set to.
         * @accessor
         */
        maxValue: 100,

        /**
         * @cfg {Boolean} allowThumbsOverlapping Whether or not to allow multiple thumbs to overlap each other.
         * Setting this to true guarantees the ability to select every possible value in between {@link #minValue}
         * and {@link #maxValue} that satisfies {@link #increment}
         * @accessor
         */
        allowThumbsOverlapping: false,

        /**
         * @cfg {Boolean/Object} animation
         * The animation to use when moving the slider. Possible properties are:
         *
         * - duration
         * - easingX
         * - easingY
         *
         * @accessor
         */
        animation: true,

        /**
         * Will make this field read only, meaning it cannot be changed from the user interface.
         * @cfg {Boolean} readOnly
         * @accessor
         */
        readOnly: false
    },

    defaultBindProperty: 'value',
    twoWayBindable: {
        value: 1
    },

    /**
     * @cfg {Number/Number[]} values Alias to {@link #value}
     */

    classCls: Ext.baseCSSPrefix + 'slider',

    elementWidth: 0,

    offsetValueRatio: 0,

    activeThumb: null,

    isThumbAnimating: 0,

    template: [{
        reference: 'trackElement',
        cls: Ext.baseCSSPrefix + 'track-el'
    }, {
        reference: 'thumbWrapElement',
        cls: Ext.baseCSSPrefix + 'thumb-wrap-el'
    }],

    fillSelector: '.' + Ext.baseCSSPrefix + 'fill-el',

    constructor: function(config) {
        if (config && config.hasOwnProperty('values')) {
            config = Ext.apply({
                value: config.values
            }, config);

            delete config.values;
        }

        this.thumbs = [];

        this.callParent([config]);
    },

    /**
     * @private
     */
    initialize: function() {
        this.callParent();
        this.element.on('tap', 'onTap', this);
    },

    onRender: function() {
        this.callParent();
        this.whenVisible('refreshSizes');
    },

    applyThumbDefaults: function (defaults) {
        return Ext.apply({
            slider: this,
            ownerCmp: this
        }, defaults);
    },

    /**
     * @private
     */
    factoryThumb: function() {
        var thumb = Ext.create(this.getThumbDefaults());

        thumb.doInheritUi();

        return thumb;
    },

    /**
     * Returns the Thumb instances bound to this Slider
     * @return {Ext.slider.Thumb[]} The thumb instances
     */
    getThumbs: function() {
        return this.thumbs;
    },

    /**
     * Returns the Thumb instance bound to this Slider
     * @param {Number} [index=0] The index of Thumb to return.
     * @return {Ext.slider.Thumb} The thumb instance
     */
    getThumb: function(index) {
        if (typeof index !== 'number') {
            index = 0;
        }

        return this.thumbs[index];
    },

    refreshOffsetValueRatio: function() {
        var me = this,
            valueRange = me.getMaxValue() - me.getMinValue(),
            trackWidth = me.elementWidth - me.thumbWidth;

        me.offsetValueRatio = valueRange === 0 ? 0 : trackWidth / valueRange;
    },

    onThumbResize: function (thumb, thumbWidth) {
        this.thumbWidth = thumbWidth;

        this.refresh();
    },

    onResize: function(width) {
        this.elementWidth = width;
        this.refresh();
    },

    refresh: function() {
        this.refreshing = true;
        this.refreshValue();
        this.refreshing = false;
    },

    setActiveThumb: function(thumb) {
        var oldActiveThumb = this.activeThumb;

        if (oldActiveThumb && oldActiveThumb !== thumb) {
            oldActiveThumb.setZIndex(null);
        }

        this.activeThumb = thumb;
        thumb.setZIndex(2);

        return this;
    },

    onThumbBeforeDragStart: function(thumb, e) {
        if (this.offsetValueRatio === 0 || e.absDeltaX <= e.absDeltaY || this.getReadOnly()) {
            return false;
        }
    },

    onThumbDragStart: function (thumb, e) {
        var me = this;

        if (me.getAllowThumbsOverlapping()) {
            me.setActiveThumb(thumb);
        }

        me.dragStartValue = me.getArrayValues()[me.getThumbIndex(thumb)];
        me.fireEvent('dragstart', me, thumb, me.dragStartValue, e);
    },

    onThumbDragMove: function (thumb, e, offsetX) {
        var me = this,
            index = me.getThumbIndex(thumb),
            offsetValueRatio = me.offsetValueRatio,
            constrainedValue = me.constrainValue(me.getMinValue() + offsetX / offsetValueRatio);

        e.stopPropagation();

        me.setIndexValue(index, constrainedValue);

        me.fireEvent('drag', me, thumb, me.getArrayValues(), e);

        return false;
    },

    setIndexValue: function (index, value, animation) {
        var me = this,
            thumb = me.thumbs[index],
            values = me.getArrayValues(),
            minValue = me.getMinValue(),
            offsetValueRatio = me.offsetValueRatio,
            increment = me.getIncrement(),
            pos = (value - minValue) * offsetValueRatio;

        // draggable.setOffset((value - minValue) * offsetValueRatio, null, animation);
        thumb.setXY(pos, null, animation);

        values[index] = minValue + Math.round((pos / offsetValueRatio) / increment) * increment;

        me.setValue(values);
        me.refreshAdjacentThumbConstraints(thumb);
    },

    onChange: function(thumb, newValue, oldValue) {
        var me = this;
        if (me.hasListeners.change) {
            me.fireEvent('change', me, thumb, newValue, oldValue);
        }
    },

    onThumbDragEnd: function (thumb, e) {
        var me = this,
            index = me.getThumbIndex(thumb),
            newValue = me.getArrayValues()[index],
            oldValue = me.dragStartValue;

        me.snapThumbPosition(thumb, newValue);
        me.fireEvent('dragend', me, thumb, me.getArrayValues(), e);
        if (oldValue !== newValue) {
            me.onChange(thumb, newValue, oldValue);
        }
    },

    getThumbIndex: function(thumb) {
        return this.thumbs.indexOf(thumb);
    },

    refreshAdjacentThumbConstraints: function (thumb) {
        var me = this,
            offsetX = thumb.getLeft(),
            thumbs = me.thumbs,
            index = me.getThumbIndex(thumb),
            previousThumb = thumbs[index - 1],
            nextThumb = thumbs[index + 1],
            thumbWidth = me.getAllowThumbsOverlapping() ? 0 : me.thumbWidth;

        if (previousThumb) {
            previousThumb.setDragMax(offsetX - thumbWidth);
        }

        if (nextThumb) {
            nextThumb.setDragMin(offsetX + thumbWidth);
        }
    },

    /**
     * @private
     */
    onTap: function(e) {
        var me = this,
            element = me.element,
            minDistance = Infinity,
            i, absDistance, testValue, closestIndex, oldValue, thumb,
            ln, values, value, offset, elementX, targetElement, touchPointX;

        if (me.offsetValueRatio === 0 || me.isDisabled() || me.getReadOnly()) {
            return;
        }

        targetElement = Ext.get(e.target);

        if (!targetElement || (Ext.browser.engineName == 'WebKit' && targetElement.hasCls('x-thumb'))) {
            return;
        }

        touchPointX = e.touch.point.x;
        elementX = element.getX();
        offset = touchPointX - elementX - (me.thumbWidth / 2);
        value = me.constrainValue(me.getMinValue() + offset / me.offsetValueRatio);
        values = me.getArrayValues();
        ln = values.length;

        if (ln === 1) {
            closestIndex = 0;
        } else {
            for (i = 0; i < ln; i++) {
                testValue = values[i];
                absDistance = Math.abs(testValue - value);

                if (absDistance < minDistance) {
                    minDistance = absDistance;
                    closestIndex = i;
                }
            }
        }

        oldValue = values[closestIndex];
        thumb = me.thumbs[closestIndex];

        me.setIndexValue(closestIndex, value, me.getAnimation());

        if (oldValue !== value) {
            me.onChange(thumb, value, oldValue);
        }
    },

    /**
     * @private
     */
    updateThumbs: function(newThumbs) {
        this.add(newThumbs);
    },

    applyValue: function(value, oldValue) {
        var me = this,
            values = Ext.Array.from(value || 0),
            valueIsArray = me.getValueIsArray(),
            filteredValues = [],
            previousFilteredValue = me.getMinValue(),
            filteredValue, i, ln;

        for (i = 0,ln = values.length; i < ln; i++) {
            filteredValue = me.constrainValue(values[i]);

            if (filteredValue < previousFilteredValue) {
                //<debug>
                Ext.log.warn("Invalid values of '" + Ext.encode(values) +
                    "', values at smaller indexes must " +
                    "be smaller than or equal to values at greater indexes");
                //</debug>
                filteredValue = previousFilteredValue;
            }

            filteredValues.push(filteredValue);
            previousFilteredValue = filteredValue;
        }

        if (!me.refreshing && oldValue && Ext.Array.equals(values, oldValue)) {
            filteredValues = undefined;
        }
        else {
            me.values = filteredValues;

            if (!valueIsArray && filteredValues.length === 1) {
                filteredValues = filteredValues[0];
            }
        }

        return filteredValues;
    },

    /**
     * Updates the sliders thumbs with their new value(s)
     */
    updateValue: function() {
        var me = this,
            thumbs = me.thumbs,
            values = me.getArrayValues(),
            len = values.length,
            i;

        me.setThumbsCount(len);

        if (!this.isThumbAnimating) {
            for (i = 0; i < len; i++) {
                me.snapThumbPosition(thumbs[i], values[i]);
            }
        }
    },

    /**
     * @private
     */
    refreshValue: function() {
        this.refreshOffsetValueRatio();

        this.updateValue();
    },

    /**
     * @private
     * Takes a desired value of a thumb and returns the nearest snap value. e.g if minValue = 0, maxValue = 100, increment = 10 and we
     * pass a value of 67 here, the returned value will be 70. The returned number is constrained within {@link #minValue} and {@link #maxValue},
     * so in the above example 68 would be returned if {@link #maxValue} was set to 68.
     * @param {Number} value The value to snap
     * @return {Number} The snapped value
     */
    constrainValue: function(value) {
        var me = this,
            minValue  = me.getMinValue(),
            maxValue  = me.getMaxValue(),
            increment = me.getIncrement(),
            remainder;

        value = parseFloat(value);

        if (isNaN(value)) {
            value = minValue;
        }

        remainder = (value - minValue) % increment;
        value -= remainder;

        if (Math.abs(remainder) >= (increment / 2)) {
            value += (remainder > 0) ? increment : -increment;
        }

        value = Math.max(minValue, value);
        value = Math.min(maxValue, value);

        return value;
    },

    setThumbsCount: function(count) {
        var me = this,
            thumbs = me.thumbs,
            thumbsCount = thumbs.length,
            i, thumb;

        while (count < thumbs.length) {
            thumb = thumbs.pop();
            thumb.destroy();
        }

        while (count > thumbs.length) {
            thumb = me.factoryThumb();
            thumbs.push(thumb);

            me.trackElement.appendChild(thumb.fillElement);
            me.thumbWrapElement.appendChild(thumb.element);
            me.element.appendChild(thumb.sizerElement);
        }

        if (thumbsCount !== count) {
            for (i = 0; i < count; i++) {
                // Default fill behavior is as follows:
                // - if only one thumb, fill is on
                // - if 2 thumbs, fill is off for first thumb, on for 2nd thumb
                // - 3 or more thumbs - no fill
                // TODO: allow the user to configure thumbs in initial slider config
                thumb = thumbs[i];

                if (count > 2) {
                    thumb.setFillTrack(false);
                } else if (count === 2) {
                    thumb.setFillTrack(i === 1);
                } else {
                    thumb.setFillTrack(true);
                }
            }
        }

        return this;
    },

    /**
     * Convenience method. Calls {@link #setValue}.
     */
    setValues: function(value) {
        this.setValue(value);
    },

    /**
     * Convenience method. Calls {@link #getValue}.
     * @return {Object}
     */
    getValues: function() {
        return this.getValue();
    },

    /**
     * @private
     */
    getArrayValues: function () {
        return this.values;
    },

    /**
     * Sets the {@link #increment} configuration.
     * @param {Number} increment
     * @return {Number}
     */
    applyIncrement: function(increment) {
        if (increment === 0) {
            increment = 1;
        }

        return Math.abs(increment);
    },

    /**
     * @private
     */
    updateAllowThumbsOverlapping: function(newValue, oldValue) {
        if (typeof oldValue != 'undefined') {
            this.refreshValue();
        }
    },

    /**
     * @private
     */
    updateMinValue: function(newValue, oldValue) {
        if (typeof oldValue != 'undefined') {
            this.refreshValue();
        }
    },

    /**
     * @private
     */
    updateMaxValue: function(newValue, oldValue) {
        if (typeof oldValue != 'undefined') {
            this.refreshValue();
        }
    },

    /**
     * @private
     */
    updateIncrement: function(newValue, oldValue) {
        if (typeof oldValue != 'undefined') {
            this.refreshValue();
        }
    },

    updateDisabled: function(disabled) {
        this.callParent(arguments);

        var thumbs = this.thumbs,
            ln = thumbs.length,
            i;

        for (i = 0; i < ln; i++) {
            thumbs[i].setDisabled(disabled);
        }
    },

    doDestroy: function() {
        this.thumbs = Ext.destroy(this.thumbs);
        this.callParent();
    },

    getRefItems: function(deep) {
        return this.thumbs;
    },

    privates: {
        /**
         * This method is called by the `thumb` before a drag gets going. We are still
         * allowed to adjust the constraints at this point so we fix them all up.
         * @private
         */
        refreshAllThumbConstraints: function () {
            var me = this,
                thumbs = me.thumbs,
                len = thumbs.length,
                thumbWidth = me.getAllowThumbsOverlapping() ? 0 : me.thumbWidth,
                i;

            for (i = 0; i < len; i++) {
                me.refreshAdjacentThumbConstraints(thumbs[i]);
            }

            thumbs[0].setDragMin(0);
            thumbs[len - 1].setDragMax(me.elementWidth - thumbWidth);
        },

        refreshSizes: function() {
            var me = this,
                thumb = me.thumbs[0];

            me.elementWidth = me.element.measure('w');
            if (thumb) {
                me.thumbWidth = thumb.element.measure('w');
            }
            me.refresh();
        },

        snapThumbPosition: function(thumb, value) {
            var ratio = this.offsetValueRatio,
                offset;

            if (isFinite(ratio)) {
                offset = Ext.Number.correctFloat((value - this.getMinValue()) * ratio);
                thumb.setXY(offset, null);
            }
        },

        syncFill: function(thumb, offset) {
            var me = this,
                thumbs = me.thumbs,
                values = me.getArrayValues(),
                ln = values.length,
                prevOffset = 0,
                fillElements = me.trackElement.query(me.fillSelector, false),
                thumbIndex = thumbs.indexOf(thumb),
                thumbOffset, fillElement, i;

            offset = offset + Math.ceil(thumb.element.getWidth() / 2);

            for (i = 0; i < ln; i++) {
                thumb = thumbs[i];
                fillElement = fillElements[i];
                thumbOffset = (i === thumbIndex) ? offset :
                    thumb.getLeft() + (thumb.element.getWidth() / 2);

                fillElement.setWidth(thumbOffset - prevOffset);
                fillElement.setLocalX(prevOffset);

                prevOffset = thumbOffset;
            }
        },

        onThumbAnimationStart: function() {
            this.isThumbAnimating++;
        },

        onThumbAnimationEnd: function() {
            this.isThumbAnimating--;
        }
    }
});
