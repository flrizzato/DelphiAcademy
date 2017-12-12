/**
 * This is a specialized field which shows a {@link Ext.picker.Date} when tapped. If it has a predefined value,
 * or a value is selected in the {@link Ext.picker.Date}, it will be displayed like a normal {@link Ext.field.Text}
 * (but not selectable/changable).
 *
 *     Ext.create('Ext.field.Date', {
 *         label: 'Birthday',
 *         value: new Date()
 *     });
 *
 * {@link Ext.field.Date} fields are very simple to implement, and have no required configurations.
 *
 * ## Examples
 *
 * It can be very useful to set a default {@link #value} configuration on {@link Ext.field.Date} fields. In
 * this example, we set the {@link #value} to be the current date. You can also use the {@link #setValue} method to
 * update the value at any time.
 *
 *     @example
 *     var form = Ext.create('Ext.form.Panel', {
 *         fullscreen: true,
 *         referenceHolder: true,
 *         items: [{
 *             xtype: 'fieldset',
 *             items: [{
 *                 xtype: 'datefield',
 *                 label: 'Birthday',
 *                 reference: 'birthday',
 *                 value: new Date()
 *             }]
 *         }, {
 *             xtype: 'toolbar',
 *             docked: 'bottom',
 *             items: [{
 *                 text: 'setValue',
 *                 handler: function() {
 *                     var field = form.lookup('birthday'),
 *                         y = Ext.Number.randomInt(1980, 2011),
 *                         m = Ext.Number.randomInt(0, 11),
 *                         d = Ext.Number.randomInt(1, 28);
 *
 *                     field.setValue(new Date(y, m, d));
 *                 }
 *             }]
 *         }]
 *     });
 *
 * When you need to retrieve the date from the {@link Ext.field.Date}, you can either use the {@link #getValue} or
 * {@link #getFormattedValue} methods:
 *
 *     @example
 *     var form = Ext.create('Ext.form.Panel', {
 *         fullscreen: true,
 *         referenceHolder: true,
 *         items: [{
 *             xtype: 'fieldset',
 *             items: [{
 *                 xtype: 'datefield',
 *                 label: 'Birthday',
 *                 reference: 'birthday',
 *                 value: new Date()
 *             }]
 *         }, {
 *             xtype: 'toolbar',
 *             docked: 'bottom',
 *             items: [{
 *                 text: 'getValue',
 *                 handler: function() {
 *                     var field = form.lookup('birthday');
 *                     console.log(field.getValue());
 *                 }
 *             }, {
 *                 text: 'getFormattedValue',
 *                 handler: function() {
 *                     var field = form.lookup('birthday');
 *                     console.log(field.getFormattedValue());
 *                 }
 *             }]
 *         }]
 *     });
 *
 *
 */
Ext.define('Ext.field.Date', {
    extend: 'Ext.field.Picker',

    alternateClassName: [
        'Ext.form.DatePicker',
        'Ext.field.DatePicker'
    ],

    xtype: ['datefield', 'datepickerfield'],

    requires: [
        'Ext.data.validator.Date',
        'Ext.field.trigger.Date',
        'Ext.picker.Date',
        'Ext.panel.Date'
    ],

    /**
     * @event change
     * Fires when a date is selected
     * @param {Ext.field.Date} this
     * @param {Date} newDate The new date
     * @param {Date} oldDate The old date
     */

    config: {
        /**
         * @cfg {Object/Date} value
         * Default value for the field and the internal {@link Ext.picker.Date} component.
         * Accepts an object of 'year', 'month' and 'day' values, all of which should be
         * numbers, or a {@link Date}.
         *
         * Example: {year: 1989, day: 1, month: 5} = 1st May 1989 or new Date()
         */

        /**
         * @cfg {Boolean} destroyPickerOnHide
         * Whether or not to destroy the picker widget on hide. This save memory if it's
         * not used frequently, but increase delay time on the next show due to
         * re-instantiation.
         */
        destroyPickerOnHide: false,

        /**
         * @cfg {String} [dateFormat=Ext.util.Format.defaultDateFormat] The format to be
         * used when displaying the date in this field. Accepts any valid date format. You
         * can view formats over in the {@link Ext.Date} documentation.
         */
        dateFormat: '',

        /**
         * @cfg {Date/String} [minDate] The minimum allowed date value for this field.
         * String value should conform to {@link #cfg!dateFormat}.
         */
        minDate: null,

        /**
         * @cfg {Date/String} [maxDate] The maximum allowed date value for this field.
         * String value should conform to {@link #cfg!dateFormat}.
         */
        maxDate: null,

        triggers: {
            expand: {
                type: 'date'
            }
        }
    },

    classCls: Ext.baseCSSPrefix + 'datepickerfield',
    matchFieldWidth: false,

    /**
     * @property {String}
     * The error message when the {@link #cfg!minDate} constraint has been violated.
     * @locale
     */
    minDateMessage: "The date in this field must be equal to or after {0}",

    /**
     * @property {String}
     * The error message when the {@link #cfg!maxDate} constraint has been violated.
     * @locale
     */
    maxDateMessage: "The date in this field must be equal to or before {0}",

    floatedPicker: {
        xtype: 'datepanel',
        autoConfirm: true,
        floated: true,
        listeners: {
            tabout: 'onTabOut',
            select: 'onPickerChange',
            scope: 'owner'
        },
        keyMap: {
            ESC: 'onTabOut',
            scope: 'owner'
        }
    },

    edgePicker: {
        xtype: 'datepicker',
        cover: true
    },

    parseValidator: 'date',

    applyValue: function(value, oldValue) {
        if (!(value || value === 0)) {
            value = null;
        }

        value = this.callParent([value, oldValue]);

        if (value) {
            if (this.isConfiguring) {
                this.originalValue = value;
            }

            // The same date value may not be the same reference, so compare them by time.
            // If we have dates for both, then compare the time. If they're the same we
            // don't need to do anything.
            if (Ext.isDate(value) && Ext.isDate(oldValue) && value.getTime() === oldValue.getTime()) {
                return;
            }
        }

        return value;
    },

    updateValue: function(value, oldValue) {
        var picker = this._picker;

        if (picker && picker.isPicker && Ext.isDate(value)) {
            this.updatePickerValue(picker, value);
        }

        this.callParent([value, oldValue]);
    },

    updatePickerValue: function (picker, value) {
        picker.setValue(value);
    },

    applyInputValue: function(value, oldValue) {
        if (Ext.isDate(value)) {
            value = Ext.Date.format(value, this.getDateFormat());
        }

        return this.callParent([value, oldValue]);
    },

    applyDateFormat: function(dateFormat) {
        return dateFormat || Ext.util.Format.defaultDateFormat;
    },

    /**
     * Updates the date format in the field.
     * @private
     */
    updateDateFormat: function() {
        var me = this,
            value;

        if (!me.isConfiguring && !me.hasFocus) {
            value = me.getValue();
            if (Ext.isDate(value)) {
                me.setInputValue(value);
            }
        }
    },

    applyMinDate: function(minDate) {
        if (typeof minDate === 'string') {
            minDate = Ext.Date.parse(minDate, this.getDateFormat());
        }

        //<debug>
        if (!Ext.isDate(minDate)) {
            Ext.raise("Date object or string in dateFormat required");
        }
        //</debug>

        return Ext.Date.clearTime(minDate, true);
    },

    applyMaxDate: function(maxDate) {
        if (typeof maxDate === 'string') {
            maxDate = Ext.Date.parse(maxDate, this.getDateFormat());
        }

        //<debug>
        if (!Ext.isDate(maxDate)) {
            Ext.raise("Date object or string in dateFormat required");
        }
        //</debug>

        return Ext.Date.clearTime(maxDate, true);
    },

    /**
     * Returns the value of the field formatted using the specified format. If it is not
     * specified, it will default to {@link #dateFormat} and then
     * {@link Ext.util.Format#defaultDateFormat}.
     * @param {String} format The format to be returned.
     * @return {String} The formatted date.
     */
    getFormattedValue: function(format) {
        var value = this.getValue();
        return Ext.isDate(value) ? Ext.Date.format(value, format || this.getDateFormat()) : '';
    },

    applyPicker: function(picker, oldPicker) {
        var me = this,
            type;

        picker = me.callParent([picker, oldPicker]);

        me.pickerType = type = picker.xtype === 'datepicker' ? 'edge' : 'floated';
        picker.ownerCmp = me;

        return picker;
    },

    createFloatedPicker: function() {
        return this.getFloatedPicker();
    },

    createEdgePicker: function() {
        var me = this,
            minDate = this.getMinDate(),
            maxDate = this.getMaxDate();

        return Ext.merge({
            yearFrom: minDate ? minDate.getFullyear() : (new Date().getFullYear() - 20),
            yearTo: maxDate ? maxDate.getFullyear() : (new Date().getFullYear() + 20)
        }, me.getEdgePicker());
    },

    setPickerLocation: function(fromKeyboard) {
        var me = this,
            pickerType = me.pickerType,
            picker = me.getPicker(),
            value = me.getValue(),
            limit;

        me.$ignorePickerChange = true;
        if (value != null) {
            picker.setValue(value);
        }
        else if (pickerType === 'edge') {
            picker.setValue(new Date());
        }
        delete me.$ignorePickerChange;

        if (pickerType === 'floated') {
            picker.el.dom.tabIndex = -1;

            limit = me.getMinDate();

            if (limit) {
                picker.setMinDate(limit);
            }

            limit = me.getMaxDate();

            if (limit) {
                picker.setMaxDate(limit);
            }

            value = value || new Date();

            // Ensure the carousel is at the correct position wth no animation.
            picker.navigateTo(value, false);

            if (fromKeyboard) {
                picker.focusDate(value);
            }
        }
    },

    doValidate: function (value, errors, skipLazy) {
        var me = this,
            format = me.getDateFormat(),
            limit, t;

        me.callParent([ value, errors, skipLazy ]);

        limit = me.getMinDate();
        t = +value;  // doValidate is only passed values that have been parsed

        if (limit && t < +limit) {
            limit = Ext.Date.format(limit, format);
            errors.push(Ext.String.format(me.minDateMessage, limit));
        }

        limit = me.getMaxDate();

        if (limit && t > +limit) {
            limit = Ext.Date.format(limit, format);
            errors.push(Ext.String.format(me.maxDateMessage, limit));
        }
    },

    /**
     * Called when the picker changes its value.
     * @param {Ext.picker.Date} picker The date picker.
     * @param {Object} value The new value from the date picker.
     * @private
     */
    onPickerChange: function(picker, value) {
        var me = this;

        if (me.$ignorePickerChange) {
            return;
        }

        me.forceInputChange = true;
        me.setValue(value);
        me.forceInputChange = false;
        me.fireEvent('select', me, value);

        // Focus the inputEl first and then collapse. We configure
        // the picker not to revert focus which is a normal thing to do
        // for floaters; in our case when the picker is focusable it will
        // lead to unexpected results on Tab key presses.
        // Note that this focusing might happen synchronously during Tab
        // key handling in the picker, which is the way we want it.
        me.onTabOut(picker);
    },

    onTabOut: function() {
        // Automatic focus reversion will move focus back to the owning field if necessary.
        this.collapse();
    },

    parseValue: function(value, errors) {
        var date;

        if (value) {
            date = Ext.Date.parse(value, this.getDateFormat());
            if (date !== null) {
                return date;
            }
        }
        return this.callParent([value, errors]);
    },

    transformValue: function (value) {
        if (Ext.isObject(value)) {
            value = new Date(value.year, value.month, value.day);

            if (isNaN(value.getTime())) {
                value = null;
            }
        }

        return value;
    },

    doDestroy: function() {
        var picker = this._picker;

        if (picker && picker.isPicker) {
            picker.destroy();
        }

        this.callParent();
    },

    privates: {
        setShowPickerValue: function(picker) {
            this.updatePickerValue(picker, this.getValue() || new Date());
        }
    },

    deprecated: {
        '6.5': {
            configs: {
                format: 'dateFormat'
            }
        }
    }
});
