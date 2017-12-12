/**
 * This component presents a month calendar and allows the user to browse and select a valid
 * date. It is used as a `floated` popup by {@link Ext.field.Date datefield} but can be created
 * and used directly.
 * @since 6.5.0
 */
Ext.define('Ext.panel.Date', {
    extend: 'Ext.Panel',
    xtype: 'datepanel',

    requires: [
        'Ext.layout.Carousel',
        'Ext.panel.DateView',
        'Ext.panel.DateTitle',
        'Ext.panel.YearPicker'
    ],

    config: {
        /**
         * @cfg {Boolean} [animation=true]
         * Set to `false` to disable animations.
         */
        animation: true,

        /**
         * @cfg {Boolean} [autoConfirm=false]
         * When set to `true`, clicking or tapping on
         * a date cell in the calendar will confirm selection and dismiss the picker.
         * When set to `false`, user will have to click OK button after selecting the date.
         */
        autoConfirm: false,

        /**
         * @cfg {String} [captionFormat="F Y"]
         * Date format for calendar pane captions.
         */
        captionFormat: {
            $value: 'F Y',
            cached: true
        },

        /**
         * @cfg {String} dateCellFormat
         * The date format to use for date cells, compatible with {@link Ext.Date#format} method.
         * This format usually includes only day of month information.
         * @locale
         */
        dateCellFormat: {
            $value: 'j',
            cached: true
        },

        /**
         * @cfg {Date[]/String[]/RegExp} disabledDates
         * An array of dates to disable. This array can contain Date objects, stringified dates
         * in {@link #format}, or RegExp patterns that would match strings in {@link #format}.
         * Date objects can be used to disable specific dates, while strings will be used to build
         * a regular expression to match dates against.
         * Some examples:
         *
         *   - ['03/08/2003', new Date(2003, 8, 16)] would disable those exact dates
         *   - ['03/08', '09/16'] would disable those days for every year
         *   - ['^03/08'] would only match the beginning (useful if you are using short years)
         *   - [/03\/..\/2006/] would disable every day in March 2006
         *   - /^03/ would disable every day in every March
         *
         * Note that the format of the dates included in the array should exactly match the
         * {@link #format} config.
         */
        disabledDates: null,

        /**
         * @cfg {Number[]} [disabledDays]
         * An array of days to disable, 0-based. For example, [0, 6] disables Sunday and Saturday.
         * See {@link #disabledDates}.
         */
        disabledDays: null,

        /**
         * @cfg {Date} focusableDate
         * The date that is currently focusable.
         *
         * @private
         * @since 6.5.1
         */
        focusableDate: null,

        /**
         * @cfg {String} format
         * The default date format string which can be overriden for localization support.
         * The format must be valid according to {@link Ext.Date#parse}
         * (defaults to {@link Ext.Date#defaultFormat}).
         * @locale
         */
        format: {
            $value: Ext.Date.defaultFormat,
            cached: true
        },

        /**
         * @cfg {Function} [handler]
         * A function that will handle the change in value.
         * The function will receive the following parameters:
         *
         * @param {Ext.panel.Date} handler.this this
         * @param {Date} handler.date The selected date
         */
        handler: null,

        /**
         * @cfg {String} headerFormat
         * The format to display the current value in the title.
         * The format must be valid according to {@link Ext.Date#parse}.
         *
         * @locale
         */
        headerFormat: {
            $value: 'D, M j Y',
            cached: true
        },

        /**
         * @cfg {Number} [headerLength=1]
         * Length of day names in header cells.
         */
        headerLength: 1,

        /**
         * @cfg {Boolean} hideCaptions
         * Set to `true` to hide calendar pane captions displaying
         * the month and year shown in each pane.
         */
        hideCaptions: true,

        /**
         * @cfg {Boolean} hideOutside
         * `true` to hide dates outside of the current month. This means no classes
         * (other than the base cell class) will be used on the cells.
         *
         * @since 6.5.1
         */
        hideOutside: false,

        /**
         * @cfg {Date/String} [maxDate]
         * Maximum allowable date as Date object or a string in {@link #format}.
         */
        maxDate: null,

        /**
         * @cfg {Date/String} [minDate]
         * Minimum allowable date as Date object or a string in {@link #format}.
         */
        minDate: null,

        /**
         * @cfg {'header'/'caption'} navigationPosition
         * The position for the {@link #tools}.
         *
         * @since 6.5.1
         */
        navigationPosition: 'header',

        /**
         * @cfg {String} nextText
         * The next month navigation button tooltip.
         * @locale
         */
        nextText: 'Next Month (Control+Right)',

        /**
         * @cfg {Number} [panes=1]
         * Number of calendar panes to display in the picker.
         */
        panes: 1,

        /**
         * @cfg {String} prevText
         * The previous month navigation button tooltip.
         * @locale
         */
        prevText: 'Previous Month (Control+Left)',

        /**
         * @cfg {Boolean} selectOnNavigate
         * `true` to keep the selection on the current pane.
         *
         * @since  6.5.1
         */
        selectOnNavigate: true,

        /**
         * @cfg {Boolean} [showAfterMaxDate]
         * Set to `true` to allow navigating to months coming after {@link #maxDate}.
         * This has no effect when `maxDate` is not set.
         */
        showAfterMaxDate: false,

        /**
         * @cfg {Boolean} [showBeforeMinDate]
         * Set to `true` to allow navigating to months preceding {@link #minDate}.
         * This has no effect when `minDate` is not set.
         */
        showBeforeMinDate: false,

        /**
         * @cfg {Boolean} [showFooter]
         * Set to `true` to always show footer bar with OK,
         * Cancel, and Today buttons. If this config is not provided, footer will be shown
         * or hidden automatically depending on {@link #autoConfirm}.
         */
        showFooter: null,

        /**
         * @cfg {Boolean} [showTodayButton]
         * Set to `true` to show the Today button. Location
         * will depend on {@link #showFooter} config: if the footer is shown, Today button
         * will be placed in the footer; otherwise the button will be placed in picker header.
         */
        showTodayButton: null,

        /**
         * @cfg {Date[]/String[]/RegExp[]} [specialDates]
         * An array of Date objects, strings, or
         * RegExp patterns designating special dates like holidays. These dates will have
         * 'x-special-day' CSS class added to their cells, allowing for visually distinct styling.
         *
         * If you want to disallow selecting these dates you would need to include them in
         * {@link #disabledDates} config as well.
         */
        specialDates: null,

        /**
         * @cfg {Number[]} [specialDays]
         * An array of days to mark as special, 0-based. For example, [0, 6] disables Sunday and Saturday.
         * See {@link #specialDates}.
         *
         * @since 6.5.1
         */
        specialDays: null,

        /**
         * @cfg {Boolean} splitTitle
         * `true` to split the year vertically from the main title. The {@link #headerFormat}
         * should be modified to reflect this.
         *
         * @since 6.5.1
         */
        splitTitle: false,

        /**
         * @cfg {Number} [startDay]
         * Day index at which the week should begin, 0-based.
         *
         * Defaults to the value of {@link Ext.Date.firstDayOfWeek}.
         * @locale
         */
        startDay: {
            $value: Ext.Date.firstDayOfWeek,
            cached: true
        },

        /**
         * @cfg {Boolean/Object} titleAnimation
         * The animation for the title. If not specified, the default
         * {@link #animation} configuration is used. This is not compatible
         * when using splitting titles using {@link #splitTitle}.
         *
         * @since 6.5.1
         */
        titleAnimation: null,

        /**
         * @cfg {Date} [value]
         * The value of this picker. Defaults to today.
         */
        value: undefined,

        /**
         * @cfg {Number[]} [weekendDays]
         * Array of weekend day indices, 0-based.
         *
         * Defaults to the value of {@link Ext.Date.weekendDays}
         * @locale
         */
        weekendDays: {
            $value: Ext.Date.weekendDays,
            cached: true
        },

        /**
         * @cfg {Object} yearPicker
         * A configuration for the {@link Ext.panel.YearPicker}. `null` to
         * disable the year picker.
         *
         * @since 6.5.1
         */
        yearPicker: {
            lazy: true,
            $value: {}
        },

        /**
         * @cfg {Object} yearPickerDefaults
         * The default configuration options for the {@link #yearPicker}.
         *
         * @since 6.5.1
         */
        yearPickerDefaults: null
    },

    /**
     * @cfg {Object} scope
     * The scope in which {@link #handler} function will be called.
     */

    /**
     * @cfg {Function} [transformCellCls]
     * A function that will be called during cell rendering
     * to allow modifying CSS classes applied to the cell.
     *
     * @param {Date} transformCellCls.date
     * Date for which a cell is being rendered.
     * @param {String[]} transformCellCls.classes Array of standard CSS classes for this cell,
     * including class names for {@link #specialDates}, {@link #disabledDates}, etc.
     * You can add custom classes or remove some standard class names as desired.
     */

    focusable: true,
    tabIndex: 0,

    border: false,
    mouseWheelBuffer: 500,

    autoSize: null,

    headerCls: Ext.baseCSSPrefix + 'datepanelheader',
    titleCls: Ext.baseCSSPrefix + 'datetitle',
    toolCls: [
        Ext.baseCSSPrefix + 'paneltool',
        Ext.baseCSSPrefix + 'datepaneltool'
    ],

    header: {
        title: {
            xtype: 'datetitle'
        }
    },

    tools: {
        previousMonth: {
            reference: 'navigatePrevMonth',
            iconCls: 'x-fa fa-angle-left',
            cls: Ext.baseCSSPrefix + 'left-year-tool ',
            weight: -100,
            increment: -1,
            focusable: false,
            tabIndex: null,
            forceTabIndex: true,
            listeners: {
                click: 'onMonthToolClick'
            }
        },
        previousYear: {
            reference: 'navigatePrevYear',
            iconCls: 'x-fa fa-angle-double-left',
            cls: Ext.baseCSSPrefix + 'left-month-tool',
            weight: -90,
            increment: -12,
            focusable: false,
            tabIndex: null,
            forceTabIndex: true,
            listeners: {
                click: 'onMonthToolClick'
            }
        },
        nextYear: {
            reference: 'navigateNextYear',
            iconCls: 'x-fa fa-angle-double-right',
            cls: Ext.baseCSSPrefix + 'right-month-tool',
            weight: 90,
            increment: 12,
            focusable: false,
            tabIndex: null,
            forceTabIndex: true,
            listeners: {
                click: 'onMonthToolClick'
            }
        },
        nextMonth: {
            reference: 'navigateNextMonth',
            iconCls: 'x-fa fa-angle-right',
            cls: Ext.baseCSSPrefix + 'right-year-tool',
            weight: 100,
            increment: 1,
            focusable: false,
            tabIndex: null,
            forceTabIndex: true,
            listeners: {
                click: 'onMonthToolClick'
            }
        }
    },

    keyMapTarget: 'bodyElement',

    // Ctrl-PageUp and Ctrl-PageDown are often used in browser to switch tabs
    // so we support both Shift- and Ctrl-PageUp/PageDown for switching years
    keyMap: {
        '*+LEFT': 'onLeftArrowKey',
        '*+RIGHT': 'onRightArrowKey',
        UP: 'onUpArrowKey',
        DOWN: 'onDownArrowKey',
        "*+PAGE_UP": 'onPageUpKey',
        "*+PAGE_DOWN": 'onPageDownKey',
        HOME: 'onHomeKey',
        END: 'onEndKey',
        ENTER: 'onEnterKey',
        SPACE: 'onSpaceKey',
        BACKSPACE: 'onBackspaceKey',
        "*+TAB": 'onTabKey',

        scope: 'this'
    },

    paneXtype: 'dateview',

    classCls: Ext.baseCSSPrefix + 'datepanel',

    layout: {
        type: 'carousel',
        animation: {
            duration: 100
        }
    },

    defaultListenerScope: true,
    referenceHolder: true,

    buttonToolbar: {
        enableFocusableContainer: false,
        cls: Ext.baseCSSPrefix + 'datepanel-footer',
        reference: 'footer'
    },

    buttons: {
        footerTodayButton: {
            text: 'Today',
            tabIndex: -1,
            hidden: true,
            weight: -20,
            handler: 'onTodayButtonClick',
            reference: 'footerTodayButton'
        },
        spacer: {
            xtype: 'component',
            weight: -10,
            flex: 1
        },
        ok: {
            tabIndex: -1,
            handler: 'onOkButtonClick'
        },
        cancel: {
            tabIndex: -1,
            handler: 'onCancelButtonClick'
        }
    },

    initialize: function() {
        var me = this,
            value = me.getValue();

        me.callParent();

        me.setToolText('navigatePrevMonth', me.getPrevText());
        me.setToolText('navigateNextMonth', me.getNextText());

        me.bodyElement.on({
            click: {
                delegate: me.cellSelector,
                fn: 'onDateClick'
            },
            focus: 'onBodyFocus',

            // Some browsers/platforms like desktop Mac will send a lot of
            // wheel events in sequence, causing very rapid calendar transitions.
            // Throttled functions begin executing immediately upon call and
            // thereafter, repeated calls are throttled to the passed buffer quantum.
            wheel: Ext.Function.createThrottled(me.onMouseWheel, me.mouseWheelBuffer),
            scope: me
        });

        // Make sure the panes are refreshed
        me.getShowFooter();

        me.preventAnim = true;
        me.setFocusableDate(value);
        me.preventAnim = false;
        me.setTitleByDate(value);
        Ext.fly(me.getCellByDate(value)).addCls(me.selectedCls);
    },

    onRender: function() {
        this.callParent();
        this.measurePaneSize();
    },

    doDestroy: function() {
        var me = this;

        Ext.destroy(me.animTitle, me.animBody);

        me.callParent();
    },

    focusDate: function(date) {
        var me = this;

        me.doFocus = true;
        me.setFocusableDate(date);
        me.doFocus = false;
    },

    updateAnimation: function(animate) {
        this.getLayout().setAnimation(animate);
    },

    updateAutoConfirm: function(autoConfirm) {
        var me = this;

        me.getButtons();

        if (!autoConfirm) {
            me.setShowFooter(true);
        } else {
            me.setShowFooter(me.initialConfig.showFooter);
        }
    },

    updateCaptionFormat: function(format) {
        this.broadcastConfig('captionFormat', format);
    },

    updateDateCellFormat: function(format) {
        this.broadcastConfig('dateCellFormat', format);
    },

    applyDisabledDates: function(dates) {
        if (!dates) {
            return dates;
        }

        var cfg = {
                dates: {}
            },
            re = [],
            item, i, len;

        if (dates instanceof RegExp) {
            cfg.re = dates;
        }
        else {
            if (!Ext.isArray(dates)) {
                dates = [dates];
            }

            for (i = 0, len = dates.length; i < len; i++) {
                item = dates[i];

                if (item instanceof Date) {
                    item = Ext.Date.clearTime(item);
                    cfg.dates[item.getTime()] = true;
                }
                else if (item instanceof RegExp) {
                    re.push(item.source);
                }
                else {
                    re.push(Ext.String.escapeRegex(item));
                }
            }

            if (re.length) {
                cfg.re = new RegExp('(?:' + re.join('|') + ')');
            }
        }

        return cfg;
    },

    updateDisabledDates: function() {
        this.refreshPanes();
    },

    applyDisabledDays: function(days) {
        return days ? Ext.Array.toMap(days) : days;
    },

    updateDisabledDays: function() {
        this.refreshPanes();
    },

    updateFormat: function(format) {
        this.broadcastConfig('format', format);
    },

    updateHeader: function(header, oldHeader) {
        this.callParent([header, oldHeader]);
        header.getTitle().on({
            scope: this,
            yeartap: 'onYearTitleTap',
            titletap: 'onTitleTap'
        });
    },

    applyMaxDate: function(date) {
        if (typeof date === 'string') {
            date = Ext.Date.parse(date, this.getFormat());
        }

        return date;
    },

    updateMaxDate: function() {
        this.refreshPanes();
    },

    applyMinDate: function(date) {
        if (typeof date === 'string') {
            date = Ext.Date.parse(date, this.getFormat());
        }

        return date;
    },

    updateMinDate: function() {
        this.refreshPanes();
    },

    updateNavigationPosition: function(pos) {
        var me = this,
            toolList = me.toolList,
            len = toolList.length,
            isHeader = pos === 'header',
            ct = isHeader ? me.toolCt : me.getHeader(),
            tools, i, c;

        if (isHeader && me.isConfiguring) {
            return;
        }

        me.getTools();

        tools = [];
        for (i = 0; i < len; ++i) {
            c = me.lookup(toolList[i]);
            if (c) {
                tools.push(c);
                ct.remove(c, false);
                c.toggleCls(me.toolCls, isHeader);
            }
        }

        me.toolCt = Ext.destroy(me.toolCt);

        if (pos === 'header') {
            me.getHeader().add(tools);
        } else {
            tools.push({
                xtype: 'component',
                flex: 1,
                weight: 0
            });

            me.toolCt = me.add({
                xtype: 'container',
                cls: Ext.baseCSSPrefix + 'navigation-tools',
                defaultType: 'tool',
                weighted: true,
                layout: 'hbox',
                // Used to make the box positioned
                bottom: 'auto',
                items: tools
            });
        }
    },

    updateNextText: function(text) {
        this.setToolText('navigateNextMonth', text);
    },

    updatePrevText: function(text) {
        this.setToolText('navigatePrevMonth', text);
    },

    //<debug>
    applyPanes: function(count) {
        if (count < 1) {
            Ext.raise("Cannot configure less than 1 pane for Calendar picker");
        }
        return count;
    },
    //</debug>

    updatePanes: function(count) {
        var me = this;

        me.getLayout().setVisibleChildren(count);
        me.initPanes(0);
        me.singlePane = count === 1;
        me.toggleCls(Ext.baseCSSPrefix + 'single', me.singlePane);
    },

    updateShowFooter: function(showFooter) {
        this.lookup('footer').setHidden(!showFooter);
        this.getShowTodayButton();
    },

    updateShowTodayButton: function(showButton) {
        var footerBtn;

        this.getButtons();

        footerBtn = this.lookup('footerTodayButton');

        if (footerBtn) {
            footerBtn.setHidden(!showButton);
        }
    },

    applySpecialDates: function(dates) {
        return this.applyDisabledDates(dates);
    },

    updateSpecialDates: function(cfg) {
        this.broadcastConfig('specialDates', cfg);
    },

    applySpecialDays: function(days) {
        return days ? Ext.Array.toMap(days) : days;
    },

    updateSpecialDays: function(daysMap) {
        this.broadcastConfig('specialDays', daysMap);
    },

    updateSplitTitle: function(splitTitle) {
        this.getHeader().getTitle().setSplit(splitTitle);
    },

    updateStartDay: function(day) {
        this.broadcastConfig('startDay', day);
    },

    applyValue: function(date) {
        if (typeof date === 'string') {
            date = Ext.Date.parse(date, this.getFormat());
        }
        // This is to make sure the default value doesn't get stale
        // in long running apps
        else if (!date) {
            date = new Date();
        }

        return Ext.isDate(date) ? Ext.Date.clearTime(date, true) : null;
    },

    updateValue: function(value, oldValue) {
        var me = this,
            handler = me.getHandler(),
            selectedCls = me.selectedCls,
            cell;

        if (oldValue) {
            cell = me.getCellByDate(oldValue);
            if (cell) {
                Ext.fly(cell).removeCls(selectedCls);
            }
        }

        if (!me.isConfiguring) {
            if (me.hasFocus) {
                me.focusDate(value);
            } else {
                me.setFocusableDate(value);
            }
            cell = me.getCellByDate(value);
            if (cell) {
                Ext.fly(cell).addCls(selectedCls);
            }
            me.setTitleByDate(value);

            me.fireEvent('change', me, value, oldValue);

            if (handler) {
                Ext.callback(handler, me.scope, [me, value, oldValue]);
            }
        }
    },

    applyWeekendDays: function(days) {
        return Ext.Array.toMap(days);
    },

    updateWeekendDays: function(daysMap) {
        this.broadcastConfig('weekendDays', daysMap);
    },

    applyYearPicker: function(yearPicker, oldYearPicker) {
        return Ext.updateWidget(oldYearPicker, yearPicker, this, 'createYearPicker', 'yearPickerDefaults');
    },

    updateYearPicker: function(yearPicker) {
        if (yearPicker) {
            this.add(yearPicker);
        }
    },

    replacePanes: function(increment, animate) {
        var me = this,
            panes, cb, direction, ret;

        if (me.destroying || me.destroyed) {
            return;
        }

        panes = me.getLayout().getVisibleItems();

        cb = function() {
            var pane, offset, j, jlen;

            for (j = 0, jlen = panes.length; j < jlen; j++) {
                pane = panes[j];
                offset = pane.getMonthOffset();
                pane.setMonthOffset(offset + increment);
            }
        };

        if (animate == null) {
            animate = me.getAnimation();
        }

        if (animate) {
            direction = increment < 0 ? 'up' : 'down';
            ret = me.animateVertical(me.carouselElement, direction, 0, cb, 'animBody');
        } else {
            cb();
            ret = Ext.Deferred.getCachedResolved();
        }
        return ret;
    },

    initPanes: function(offset) {
        var me = this,
            count = me.getPanes() + 2,
            panes = [],
            oldPanes, index, center, i;

        index = count - 1;
        center = !index ? index : index % 2 ? Math.floor(index / 2) + 1 : Math.floor(index / 2);

        for (i = 0; i < count; i++) {
            panes.push(me.getPaneTemplate((i + offset) - center));
        }

        oldPanes = me.getInnerItems();

        for (i = 0; i < oldPanes.length; i++) {
            me.remove(oldPanes[i], true);
        }

        me.add(panes);
        me.getLayout().setFrontItem(center, false);
    },

    getPaneByDate: function(date) {
        var me = this,
            panes = me.getInnerItems(),
            month, pane, i, len;

        month = Ext.Date.getFirstDateOfMonth(date);

        for (i = 0, len = panes.length; i < len; i++) {
            pane = panes[i];

            if (Ext.Date.isEqual(pane.getMonth(), month)) {
                return pane;
            }
        }

        return null;
    },

    getCellByDate: function(date) {
        var pane = this.getPaneByDate(date);

        return pane ? pane.getCellByDate(date) : null;
    },

    updateCellTabIndex: function(date, tabIndex) {
        var cell = date && this.getCellByDate(date);

        if (cell) {
            Ext.fly(cell).setTabIndex(tabIndex);
        }
        return cell;
    },

    canSwitchTo: function(date, offset) {
        var me = this,
            boundary, prevent;

        if (offset < 0) {
            boundary = me.getMinDate();
            prevent = !me.getShowBeforeMinDate();

            if (boundary && prevent) {
                if (date.getTime() < Ext.Date.getFirstDateOfMonth(boundary).getTime()) {
                    return false;
                }
            }
        }
        else if (offset > 0) {
            boundary = me.getMaxDate();
            prevent = !me.getShowAfterMaxDate();

            if (boundary && prevent) {
                if (date.getTime() > Ext.Date.getLastDateOfMonth(boundary).getTime()) {
                    return false;
                }
            }
        }

        return true;
    },

    navigateTo: function(date, animate) {
        var me = this,
            layout = me.getLayout(),
            month, increment, boundary, prevent;

        // Offset is only known beforehand for pointer/touch interaction, where
        // clicking month/year tool switches panes as an action. Keyboard interaction
        // is different; moving focused date might result in not switching panes at all
        // so we have to calculate increment here as a difference between the new date
        // and visible panes.
        // Assignment is intentional
        if (date.getTime() < (month = layout.getFirstVisibleItem().getMonth()).getTime()) {
            boundary = month;
        } else if (date.getTime() > (month = layout.getLastVisibleItem().getMonth()).getTime()) {
            boundary = month;
        } else {
            boundary = date;
        }

        increment = (date.getFullYear() * 12 + date.getMonth()) -
                    (boundary.getFullYear() * 12 + boundary.getMonth());

        if (increment < 0) {
            boundary = me.getMinDate();
            prevent = !me.getShowBeforeMinDate();

            if (boundary && prevent) {
                if (date.getTime() < Ext.Date.getFirstDateOfMonth(boundary).getTime()) {
                    increment = 0;
                }
            }
        } else if (increment > 0) {
            boundary = me.getMaxDate();
            prevent = !me.getShowAfterMaxDate();

            if (boundary && prevent) {
                if (date.getTime() > Ext.Date.getLastDateOfMonth(boundary).getTime()) {
                    increment = 0;
                }
            }
        }

        return me.navigateByIncrement(increment, animate, 0);
    },

    switchPanes: function(increment, animate) {
        var me = this,
            layout = me.getLayout(),
            edgePane, pane;

        edgePane = increment < 0 ? layout.getFirstVisibleItem() : layout.getLastVisibleItem();

        pane = layout.getEdgeItem(increment);
        pane.setMonthOffset(edgePane.getMonthOffset() + increment);

        return layout.move(increment, animate);
    },

    onMonthToolClick: function(tool) {
        var me = this,
            panes = me.getInnerItems(),
            D = Ext.Date,
            increment = tool.increment,
            date = D.add(me.getFocusableDate(), D.MONTH, increment),
            hasFocus = me.hasFocus,
            index, pane, month;

        index = me.getCenterIndex();
        pane = panes[index];

        month = D.add(pane.getMonth(), D.MONTH, increment);

        if (!me.canSwitchTo(month, increment)) {
            return;
        }

        me.navIncrement = me.singlePane ? 0 : increment;
        if (hasFocus || me.getSelectOnNavigate()) {
            me.setValue(date);
        } else {
            me.doFocus = hasFocus;
            me.setFocusableDate(date);
            me.doFocus = false;
        }
        me.navIncrement = 0;
    },

    onDateClick: function(e) {
        var me = this,
            cell = e.getTarget(me.cellSelector, me.bodyElement),
            date = cell && cell.date,
            focus = true,
            disabled = cell && cell.disabled;

        // Click could land on element other than date cell
        if (!date || me.getDisabled()) {
            return;
        }

        if (!disabled) {
            me.setValue(date);

            if (me.getAutoConfirm()) {
                // Touch events change focus on tap.
                // Prevent this as we are just about to hide.
                // PickerFields revert focus to themselves in a beforehide handler.
                if (e.pointerType === 'touch') {
                    e.preventDefault();
                }
                focus = false;
                me.fireEvent('select', me, date);
            }
        }

        if (focus) {
            // Even though setValue might focus the date, we may
            // either be in a position where the date is disabled
            // or already set.
            me.focusDate(date);
        }
    },

    onMouseWheel: function (e) {
        var dy = e.browserEvent.deltaY;

        if (dy && !this.pickerVisible) {
            this.onMonthToolClick({
                increment: Math.sign(dy)
            });
        }
    },

    onOkButtonClick: function() {
        this.setValue(this.getFocusableDate());
    },

    onCancelButtonClick: function() {
        this.fireEventArgs('tabout', [this]);
    },

    onTodayButtonClick: function() {
        var me = this,
            offset;

        offset = me.getLayout().getFrontItem().getMonthOffset();

        if (offset !== 0) {
            // This looks smoother if switchPane is used
            if (Math.abs(offset) === 1) {
                me.switchPanes(-offset);
            } else {
                me.replacePanes(-offset);
            }
        }

        me.setValue(Ext.Date.clearTime(new Date()));
    },

    getFocusEl: function() {
        if (!this.initialized) {
            return null;
        }

        return this.getCellByDate(this.getFocusableDate());
    },

    onLeftArrowKey: function(e) {
        this.walkCells(e.target.date, e.ctrlKey ? Ext.Date.MONTH : Ext.Date.DAY, -1);

        // We need to prevent default to avoid scrolling the nearest container
        // which in case of a floating Date picker will be the document body.
        // This applies to all navigation keys and Space key.
        e.preventDefault();
    },

    onRightArrowKey: function(e) {
        this.walkCells(e.target.date, e.ctrlKey ? Ext.Date.MONTH : Ext.Date.DAY, 1);

        e.preventDefault();
    },

    onUpArrowKey: function(e) {
        this.walkCells(e.target.date, Ext.Date.DAY, -7);

        e.preventDefault();
    },

    onDownArrowKey: function(e) {
        this.walkCells(e.target.date, Ext.Date.DAY, 7);

        e.preventDefault();
    },

    onPageUpKey: function(e) {
        var unit = e.ctrlKey || e.shiftKey ? Ext.Date.YEAR : Ext.Date.MONTH;

        this.walkCells(e.target.date, unit, -1);

        e.preventDefault();
    },

    onPageDownKey: function(e) {
        var unit = e.ctrlKey || e.shiftKey ? Ext.Date.YEAR : Ext.Date.MONTH;

        this.walkCells(e.target.date, unit, 1);

        e.preventDefault();
    },

    onHomeKey: function(e) {
        this.walkCells(Ext.Date.getFirstDateOfMonth(e.target.date));

        e.preventDefault();
    },

    onEndKey: function(e) {
        this.walkCells(Ext.Date.getLastDateOfMonth(e.target.date));

        e.preventDefault();
    },

    onBackspaceKey: function(e) {
        this.walkCells(new Date());

        e.preventDefault();
    },

    onEnterKey: function(e) {
        var target = e.target,
            date = target && target.date;

        if (date && !target.disabled) {
            this.setValue(date);
            this.fireEvent('select', this, target.date);
        }
    },

    onSpaceKey: function(e) {
        this.onEnterKey(e);

        // Space key scrolls as well
        e.preventDefault();
    },

    onTabKey: function(e) {
        // When the picker is floating and attached to an input field, its
        // 'select' handler will focus the inputEl so when navigation happens
        // it does so as if the input field was focused all the time.
        // This is the desired behavior and we try not to interfere with it
        // in the picker itself, see below.
        this.handleTabKey(e);

        // Allow default behaviour of TAB - it MUST be allowed to navigate.
        return true;
    },

    handleTabKey: function(e) {
        var me = this,
            target = e.target,
            picker = me.pickerField;

        // We're only setting the value if autoConfirm == true; if it's not then pressing
        // Enter key or clicking OK button is required to confirm date selection
        if (!me.getDisabled() && me.getAutoConfirm() && target && target.date && !target.disabled) {
            me.setValue(target.date);

            // If the ownerfield is part of an editor we must preventDefault and let
            // the navigationModel handle the tab event.
            if (picker && picker.isEditorComponent) {
                e.preventDefault();
            }
        }
        // Even if the above condition is not met we have to let the field know
        // that we're tabbing out; that's user action we can do nothing about
        else {
            me.fireEventArgs('tabout', [me]);
        }
    },

    walkCells: function(date, unit, increment) {
        var me = this,
            newDate;

        if (!me.getDisabled()) {
            date = me.getFocusableDate();
            newDate = unit ? Ext.Date.add(date, unit, increment) : date;

            if (me.isDateDisabled(newDate)) {
                me.focusDate(newDate);
            } else {
                me.setValue(newDate);
            }
        }
    },

    onBodyFocus: function(e) {
        var me = this,
            date = me.getFocusableDate(),
            cell = me.getCellByDate(date);

        // Make sure there is a focusable cell in the view
        if (!cell) {
            me.navigateTo(date, false);
        }

        cell = me.updateCellTabIndex(date, me.getTabIndex());
        cell.focus();
    },

    getTabIndex: function() {
        // We want this method to always return configured tabIndex value
        // instead of trying to read it off the `focusEl`.
        return this.getConfig('tabIndex', true);
    },

    getFocusClsEl: function() {
        return this.bodyElement;
    },

    onFocusEnter: function(e) {
        if (this.bodyElement.contains(e.target)) {
            this.onFocus(e);
        }

        this.callParent([e]);
    },

    onFocusLeave: function(e) {
        this.onBlur(e);
        this.callParent([e]);
    },

    privates: {
        cellSelector: '.' + Ext.baseCSSPrefix + 'cell',
        clonedCls: Ext.baseCSSPrefix + 'cloned',
        lastNavigate: 0,
        hideFocusCls: Ext.baseCSSPrefix + 'hide-focus',
        selectedCls: Ext.baseCSSPrefix + 'selected',
        toolList: ['navigatePrevMonth', 'navigatePrevYear', 'navigateNextYear', 'navigateNextMonth'],

        paneWidthMap: {},
        pickerVisible: false,

        applyFocusableDate: function(date) {
            var me = this,
                D = Ext.Date,
                boundary;

            // Null is a valid value to set onFocusLeave in order to clear the focused cell
            // and allow the value to be set the next time the panel is displayed.
            if (date) {
                // Should check default value (today) as well, it could be that
                // allowed selection is in the past or in the future.
                date = D.clearTime(date || new Date());

                if ((boundary = me.getMinDate()) && !me.getShowBeforeMinDate() &&
                    date.getTime() < boundary.getTime()) {
                    date = boundary;
                }
                else if ((boundary = me.getMaxDate()) && !me.getShowAfterMaxDate() &&
                    date.getTime() > boundary.getTime()) {
                    date = boundary;
                }
            }

            return date;
        },

        updateFocusableDate: function(date, oldDate) {
            var me = this,
                focus = me.doFocus,
                layout = me.getLayout(),
                cls = me.hideFocusCls,
                increment = me.navIncrement,
                visibleItems, toPane, anim, navigate, oldCell, p;

            if (me.destroying || me.destroyed) {
                return;
            }

            if (oldDate) {
                oldCell = me.getCellByDate(oldDate);
                me.updateCellTabIndex(oldDate, -1);
            }

            if (date) {
                toPane = me.getPaneByDate(date);

                if (!me.preventAnim) {
                    anim = me.getAnimation();
                }

                visibleItems = layout.getVisibleItems();

                // New date will be immediately visible, or is in same pane.
                // Simply activate the pane and focus. Do not animate title change.
                me.lastNavigate = navigate = Date.now();
                if (!increment && (!anim || visibleItems.indexOf(toPane) > -1)) {
                    me.navigateTo(date, false);
                    if (focus) {
                        me.getCellByDate(date).focus();
                    }
                } else {
                    // Temporarily remove the focus styling while we're moving away
                    if (oldCell) {
                        Ext.fly(oldCell).addCls(cls);
                    }
                    p = increment ? me.navigateByIncrement(increment) : me.navigateTo(date);
                    p.then(function() {
                        oldCell = me.getCellByDate(oldDate);
                        if (oldCell) {
                            Ext.fly(oldCell).removeCls(cls);
                        }
                        // Make sure the frontItem hasn't changed
                        if (focus && me.lastNavigate === navigate) {
                            me.getCellByDate(date).focus();
                        }
                    });
                }
                me.updateCellTabIndex(date, me.getTabIndex());
            }
        },

        animateVertical: function(el, direction, offset, beforeFn, prop) {
            var me = this,
                clone = el.dom.cloneNode(true),
                ret = new Ext.Deferred();

            clone.id = '';

            Ext.fly(clone).addCls(me.clonedCls);

            el.parent().appendChild(clone);

            if (beforeFn) {
                beforeFn();
            }

            Ext.destroy(me[prop]);

            me[prop] = Ext.Animator.run([{
                offset: offset,
                type: 'slide',
                direction: direction,
                element: el
            }, {
                offset: offset,
                type: 'slideOut',
                direction: direction,
                element: clone,
                callback: function() {
                    Ext.fly(clone).destroy();
                    me[prop] = null;
                    ret.resolve();
                }
            }]);

            return ret.promise;
        },

        broadcastConfig: function(config, value) {
            if (this.isConfiguring) {
                return;
            }

            var panes = this.getInnerItems(),
                setter, pane, i, len;

            setter = Ext.Config.get(config).names.set;

            for (i = 0, len = panes.length; i < len; i++) {
                pane = panes[i];

                if (pane[setter]) {
                    pane[setter](value);
                }
            }
        },

        createYearPicker: function(config) {
            return Ext.apply({
                xtype: 'yearpicker',
                hidden: true,
                top: 0,
                right: 0,
                bottom: 0,
                left: 0,
                listeners: {
                    yeartap: 'onYearPickerTap'
                }
            }, config);
        },

        getCenterIndex: function() {
            var count = this.getPanes(),
                index = count - 1;

            return !index ? index : index % 2 ? Math.floor(index / 2) + 1 : Math.floor(index / 2);
        },

        getPaneTemplate: function(offset) {
            var me = this;

            return {
                xtype: me.paneXtype,
                monthOffset: offset,
                hideOutside: me.getHideOutside(),
                hideCaption: me.getHideCaptions(),
                startDay: me.getStartDay(),
                weekendDays: me.getWeekendDays(),
                specialDates: me.getSpecialDates(),
                specialDays: me.getSpecialDays(),
                format: me.getFormat(),
                captionFormat: me.getCaptionFormat(),
                dateCellFormat: me.getDateCellFormat(),
                headerLength: me.getHeaderLength(),
                transformCellCls: me.transformCellCls
            };
        },

        getPositionedItemTarget: function () {
            return this.bodyElement;
        },

        isDateDisabled: function(date) {
            var me = this,
                ms = date.getTime(),
                minDate = me.getMinDate(),
                maxDate = me.getMaxDate(),
                disabled = false,
                disabledDays, disabledDates, formatted, re;

            disabled = (minDate && ms < minDate.getTime()) || (maxDate && ms > maxDate.getTime());

            if (!disabled) {
                disabledDays = me.getDisabledDays();
                if (disabledDays) {
                    disabled = disabledDays[date.getDay()];
                }
            }

            if (!disabled) {
                disabledDates = me.getDisabledDates();
                if (disabledDates) {
                    disabled = disabledDates.dates[ms];
                    re = disabledDates.re;
                    if (!disabled && re) {
                        formatted = Ext.Date.format(date, me.getFormat());
                        disabled = re.test(formatted);
                    }
                }
            }

            return !!disabled;
        },

        measurePaneSize: function() {
            var me = this,
                count = me.getPanes(),
                ui = me.getUi() || 'default',
                map = me.paneWidthMap,
                borderWidth;

            // Okay this is a hack but will do for now because Carousel layout
            // needs the container to be widthed
            if (!map.hasOwnProperty(ui)) {
                map[ui] = this.getLayout().getFrontItem().measurePaneSize();
            }

            borderWidth = me.el.getBorderWidth('lr');
            me.setWidth(borderWidth + count * map[ui]);
        },

        navigateByIncrement: function(increment, animate) {
            var ret;

            if (Math.abs(increment) === 1) {
                ret = this.switchPanes(increment, animate);
            } else {
                if (increment !== 0) {
                    ret = this.replacePanes(increment, animate);
                } else if (!animate) {
                    this.getLayout().cancelAnimation();
                    ret = Ext.Deferred.getCachedResolved();
                }
            }
            return ret;
        },

        onTitleTap: function() {
            var visible;

            if (this.getSplitTitle()) {
                visible = false;
            } else {
                visible = !this.pickerVisible;
            }
            this.togglePicker(visible);
        },

        onYearPickerTap: function(picker, year) {
            this.togglePicker(false);
            var d = Ext.Date.clone(this.getFocusableDate());
            d.setFullYear(year);
            this.setValue(d);
        },

        onYearTitleTap: function() {
            this.togglePicker(!this.pickerVisible);
        },

        refreshPanes: function() {
            if (this.isConfiguring) {
                return;
            }

            var panes = this.getPanes(),
                len = panes.length,
                i;

            for (i = 0; i < len; ++i) {
                panes[i].refresh();
            }
        },

        setTitleByDate: function(date) {
            var me = this,
                prev = me.lastTitleDate,
                anim;

            if (prev && prev.getTime() === date.getTime()) {
                anim = false;
            }

            me.setTitleText(Ext.Date.format(date, me.getHeaderFormat()), date, prev, anim);

            me.lastTitleDate = date;
        },

        setTitleText: function(text, date, oldDate, animate) {
            var me = this,
                title, direction, titleAnim;

            if (me.destroying || me.destroyed) {
                return;
            }

            if (animate === undefined) {
                titleAnim = me.getTitleAnimation();
                if (titleAnim !== null) {
                    animate = titleAnim;
                } else {
                    animate = me.getAnimation();
                }
            }

            animate = me.rendered ? animate : false;

            title = me.getHeader().getTitle();

            if (animate) {
                //<debug>
                if (me.getSplitTitle()) {
                    Ext.raise('Animation is not supported with title split');
                }
                //</debug>
                direction = (oldDate || date).getTime() < date.getTime() ? 'bottom' : 'top';
                me.animateVertical(title.textElement, direction, '150%', function() {
                    title.setText(text);
                }, 'animTitle');
            } else {
                if (me.getSplitTitle()) {
                    title.setYear(date.getFullYear());
                    title.setText(text);
                } else {
                    title.setText(text);
                }
            }
        },

        setToolText: function(type, text) {
            var tool = this.lookup(type);

            if (tool) {
                tool.setTooltip(text);
            }
        },

        togglePicker: function(visible) {
            var me = this,
                picker = me.getYearPicker();

            if (picker) {
                if (me.getSplitTitle()) {
                    me.getHeader().getTitle().setTitleActive(!visible);
                }
                picker.setHidden(!visible);
                if (visible) {
                    picker.focusYear(me.getFocusableDate().getFullYear());
                }
                me.pickerVisible = visible;
            }
        }
    }
});