/**
 * A date picker component which shows a Date Picker on the screen. This class extends from {@link Ext.picker.Picker}
 * and {@link Ext.Sheet} so it is a popup.
 *
 * This component has no required configurations.
 *
 * ## Examples
 *
 *     @example
 *     var datePicker = Ext.create('Ext.picker.Date');
 *     Ext.Viewport.add(datePicker);
 *     datePicker.show();
 *
 * You may want to adjust the {@link #yearFrom} and {@link #yearTo} properties:
 *
 *     @example
 *     var datePicker = Ext.create('Ext.picker.Date', {
 *         yearFrom: 2000,
 *         yearTo  : 2015
 *     });
 *     Ext.Viewport.add(datePicker);
 *     datePicker.show();
 *
 * You can set the value of the {@link Ext.picker.Date} to the current date using `new Date()`:
 *
 *     @example
 *     var datePicker = Ext.create('Ext.picker.Date', {
 *         value: new Date()
 *     });
 *     Ext.Viewport.add(datePicker);
 *     datePicker.show();
 *
 * And you can hide the titles from each of the slots by using the {@link #useTitles} configuration:
 *
 *     @example
 *     var datePicker = Ext.create('Ext.picker.Date', {
 *         useTitles: false
 *     });
 *     Ext.Viewport.add(datePicker);
 *     datePicker.show();
 */
Ext.define('Ext.picker.Date', {
    extend: 'Ext.picker.Picker',
    xtype: 'datepicker',
    alternateClassName: 'Ext.DatePicker',
    requires: ['Ext.util.InputBlocker'],

    /**
     * @event change
     * Fired when the value of this picker has changed and the done button is pressed.
     * @param {Ext.picker.Date} this This Picker
     * @param {Date} value The date value
     */

    config: {
        /**
         * @cfg {Number} yearFrom
         * The start year for the date picker. If {@link #yearFrom} is greater than
         * {@link #yearTo} then the order of years will be reversed.
         * @accessor
         */
        yearFrom: 1980,

        /**
         * @cfg {Number} [yearTo=new Date().getFullYear()]
         * The last year for the date picker. If {@link #yearFrom} is greater than
         * {@link #yearTo} then the order of years will be reversed.
         * @accessor
         */
        yearTo: new Date().getFullYear(),

        /**
         * @cfg {String} monthText
         * The label to show for the month column.
         * @accessor
         */
        monthText: 'Month',

        /**
         * @cfg {String} dayText
         * The label to show for the day column.
         * @accessor
         */
        dayText: 'Day',

        /**
         * @cfg {String} yearText
         * The label to show for the year column.
         * @accessor
         */
        yearText: 'Year',

        /**
         * @cfg {Array} slotOrder
         * An array of strings that specifies the order of the slots.
         * @accessor
         */
        slotOrder: ['month', 'day', 'year'],

        /**
         * @cfg {Object/Date} value
         * Default value for the field and the internal {@link Ext.picker.Date} component. Accepts an object of 'year',
         * 'month' and 'day' values, all of which should be numbers, or a {@link Date}.
         *
         * Examples:
         *
         * - `{year: 1989, day: 1, month: 5}` = 1st May 1989
         * - `new Date()` = current date
         * @accessor
         */

        /**
         * @cfg {Array} slots
         * @hide
         * @accessor
         */

        /**
         * @cfg {String/Mixed} doneButton
         * Can be either:
         *
         * - A {String} text to be used on the Done button.
         * - An {Object} as config for {@link Ext.Button}.
         * - `false` or `null` to hide it.
         * @accessor
         */
        doneButton: true
    },

    initialize: function() {
        var me = this;

        me.callParent();

        me.on({
            scope: me,
            delegate: '> slot',
            slotpick: me.onSlotPick
        });

        me.on({
            scope: me,
            show: me.onSlotPick
        });
    },

    setValue: function(value, animated) {
        var me = this;

        if (Ext.isDate(value)) {
            value = {
                day  : value.getDate(),
                month: value.getMonth() + 1,
                year : value.getFullYear()
            };
        }

        me.callParent([value, animated]);
        if (me.rendered) {
            me.onSlotPick();
        }

        return me;
    },

    getValue: function(useDom) {
        var values = {},
            items = this.getItems().items,
            ln = items.length,
            daysInMonth, day, month, year, item, i;

        for (i = 0; i < ln; i++) {
            item = items[i];
            if (item.isSlot) {
                values[item.getName()] = item.getValue(useDom);
            }
        }

        //if all the slots return null, we should not return a date
        if (values.year === null && values.month === null && values.day === null) {
            return null;
        }

        year = Ext.isNumber(values.year) ? values.year : 1;
        month = Ext.isNumber(values.month) ? values.month : 1;
        day = Ext.isNumber(values.day) ? values.day : 1;

        if (month && year && month && day) {
            daysInMonth = this.getDaysInMonth(month, year);
        }
        day = (daysInMonth) ? Math.min(day, daysInMonth): day;

        return new Date(year, month - 1, day);
    },

    /**
     * Updates the yearFrom configuration
     */
    updateYearFrom: function() {
        if (this.initialized) {
            this.createSlots();
        }
    },

    /**
     * Updates the yearTo configuration
     */
    updateYearTo: function() {
        if (this.initialized) {
            this.createSlots();
        }
    },

    /**
     * Updates the monthText configuration
     */
    updateMonthText: function(newMonthText, oldMonthText) {
        var innerItems = this.getInnerItems,
            ln = innerItems.length,
            item, i;

        //loop through each of the current items and set the title on the correct slice
        if (this.initialized) {
            for (i = 0; i < ln; i++) {
                item = innerItems[i];

                if ((typeof item.title == "string" && item.title == oldMonthText) || (item.title.html == oldMonthText)) {
                    item.setTitle(newMonthText);
                }
            }
        }
    },

    /**
     * Updates the {@link #dayText} configuration.
     */
    updateDayText: function(newDayText, oldDayText) {
        var innerItems = this.getInnerItems,
            ln = innerItems.length,
            item, i;

        //loop through each of the current items and set the title on the correct slice
        if (this.initialized) {
            for (i = 0; i < ln; i++) {
                item = innerItems[i];

                if ((typeof item.title == "string" && item.title == oldDayText) || (item.title.html == oldDayText)) {
                    item.setTitle(newDayText);
                }
            }
        }
    },

    /**
     * Updates the yearText configuration
     */
    updateYearText: function(yearText) {
        var innerItems = this.getInnerItems,
            ln = innerItems.length,
            item, i;

        //loop through each of the current items and set the title on the correct slice
        if (this.initialized) {
            for (i = 0; i < ln; i++) {
                item = innerItems[i];

                if (item.title == this.yearText) {
                    item.setTitle(yearText);
                }
            }
        }
    },

    /**
     * @private
     */
    constructor: function() {
        this.callParent(arguments);
        this.createSlots();
    },

    /**
     * Generates all slots for all years specified by this component, and then sets them on the component
     * @private
     */
    createSlots: function() {
        var me        = this,
            slotOrder = me.getSlotOrder(),
            yearsFrom = me.getYearFrom(),
            yearsTo   = me.getYearTo(),
            years     = [],
            days      = [],
            months    = [],
            slots     = [],
            reverse   = yearsFrom > yearsTo,
            ln, i, daysInMonth;

        while (yearsFrom) {
            years.push({
                text  : yearsFrom,
                value : yearsFrom
            });

            if (yearsFrom === yearsTo) {
                break;
            }

            if (reverse) {
                yearsFrom--;
            } else {
                yearsFrom++;
            }
        }

        daysInMonth = me.getDaysInMonth(1, new Date().getFullYear());

        for (i = 0; i < daysInMonth; i++) {
            days.push({
                text  : i + 1,
                value : i + 1
            });
        }

        for (i = 0, ln = Ext.Date.monthNames.length; i < ln; i++) {
            months.push({
                text  : Ext.Date.monthNames[i],
                value : i + 1
            });
        }

        slotOrder.forEach(function (item) {
            slots.push(me.createSlot(item, days, months, years));
        });

        me.setSlots(slots);
    },

    /**
     * Returns a slot config for a specified date.
     * @private
     */
    createSlot: function(name, days, months, years) {
        var me = this,
            result;

        switch (name) {
            case 'year':
                result = {
                    name: 'year',
                    align: 'center',
                    data: years,
                    title: me.getYearText(),
                    flex: 3
                };
                break;
            case 'month':
                result = {
                    name: name,
                    align: 'right',
                    data: months,
                    title: me.getMonthText(),
                    flex: 4
                };
                break;
            case 'day':
                result = {
                    name: 'day',
                    align: 'center',
                    data: days,
                    title: me.getDayText(),
                    flex: 2
                };
        }
        if (me._value) {
            result.value = me._value[name];
        }
        return result;
    },

    onSlotPick: function() {
        var me = this,
            addDays = [],
            value, daySlot, valueField, dayStore, dayData, 
            daysInMonth, slotCount, year, month, i, spliceArgs;

        if (me.isConfiguring) {
            return;
        }
        
        value = me.getValue(true);
        daySlot = me.getDaySlot();
        
        me.callParent(arguments);

        // This method only deals with number of days adjustments
        // if we have no daySlot there is nothing to be done.
        if (!daySlot) {
            return;
        }

        valueField = daySlot.getValueField();
        dayStore = daySlot.getStore();
        dayData = dayStore.getData();
        slotCount = dayStore.getCount();
        year = value.getFullYear();
        month = value.getMonth();

        //get the new days of the month for this new date
        daysInMonth = me.getDaysInMonth(month + 1, year);

        // We don't need to update the slot days unless it has changed
        if (slotCount === daysInMonth) {
            return;
        }

        // Need a few more days, eg, we've gone from Feb to Mar
        // We need to create 29, 30 and 31
        if (daysInMonth > slotCount) {
            for (i = slotCount; i < daysInMonth; i++) {
                addDays.push(dayStore.createModel({
                    text: i + 1,
                    value: i + 1
                }));
            }
            spliceArgs = [slotCount, 0, addDays];
        } else {
            spliceArgs = [daysInMonth, 5];
        }
        // Splice day store to correct length
        dayData.splice.apply(dayData, spliceArgs);

        // Now we have the correct amount of days for the day slot, lets update it
        i = dayStore.find(valueField, value.getDate());
        if (i == -1) {
            return;
        }

        daySlot.selectedIndex = i;
        daySlot.scrollToItem(daySlot.mapToItem(i));
        daySlot.setValue(daySlot.getValue(true));
    },

    getDaySlot: function() {
        var innerItems = this.getInnerItems(),
            ln = innerItems.length,
            i, slot;

        if (this.daySlot) {
            return this.daySlot;
        }

        for (i = 0; i < ln; i++) {
            slot = innerItems[i];
            if (slot.isSlot && slot.getName() == "day") {
                this.daySlot = slot;
                return slot;
            }
        }

        return null;
    },

    /**
     * @private
     */
    getDaysInMonth: function(month, year) {
        var daysInMonth = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
        return month == 2 && this.isLeapYear(year) ? 29 : daysInMonth[month-1];
    },

    /**
     * @private
     */
    isLeapYear: function(year) {
        return !!((year & 3) === 0 && (year % 100 || (year % 400 === 0 && year)));
    },

    onDoneButtonTap: function() {
        var me = this,
            oldValue = me._value,
            newValue = me.getValue(true),
            testValue = newValue;

        if (Ext.isDate(newValue)) {
            testValue = newValue.toDateString();
        }
        if (Ext.isDate(oldValue)) {
            oldValue = oldValue.toDateString();
        }

        if (testValue != oldValue) {
            me.ownerField.onPickerChange(me, newValue);
            me.fireEvent('change', me, newValue);
        }

        me.hide();
        Ext.util.InputBlocker.unblockInputs();
    }
});
