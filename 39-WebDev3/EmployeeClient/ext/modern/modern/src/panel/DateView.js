Ext.define('Ext.panel.DateView', {
    extend: 'Ext.Widget',
    xtype: 'dateview',

    config: {
        specialDates: null,
        specialDays: null,
        monthOffset: 0
    },

    cachedConfig: {
        captionFormat: null,
        dateCellFormat: null,
        format: null,
        headerLength: null,
        hideCaption: true,
        hideOutside: true,
        startDay: null,
        weekendDays: null
    },

    element: {
        reference: 'element'
    },

    tableTpl: {
        reference: 'tableElement',
        tag: 'table',
        cellspacing: '0',
        cellpadding: '0',
        children: []
    },

    captionTpl: {
        reference: 'captionElement',
        tag: 'caption'
    },

    headTpl: {
        tag: 'thead',
        reference: 'headElement'
    },

    headRowTpl: {
        tag: 'tr'
    },

    headCellTpl: {
        tag: 'th',
        cls: Ext.baseCSSPrefix + 'cell',
        children: [{
            tag: 'div',
            cls: Ext.baseCSSPrefix + 'inner ' + Ext.dom.Element.unselectableCls
        }]
    },

    bodyTpl: {
        tag: 'tbody',
        reference: 'bodyElement'
    },

    bodyRowTpl: {
        tag: 'tr'
    },

    bodyCellTpl: {
        tag: 'td',
        cls: Ext.baseCSSPrefix + 'cell',
        tabIndex: -1,
        children: [{
            tag: 'div',
            cls: Ext.baseCSSPrefix + 'inner ' +  Ext.dom.Element.unselectableCls
        }]
    },

    rows: 6,
    columns: 7,

    cellCls: Ext.baseCSSPrefix + 'cell',
    emptyCls: Ext.baseCSSPrefix + 'empty',
    weekendDayCls: Ext.baseCSSPrefix + 'weekend',
    disabledDayCls: Ext.baseCSSPrefix + 'disabled',
    specialDateCls: Ext.baseCSSPrefix + 'special',
    todayCls: Ext.baseCSSPrefix + 'today',
    outsideCls: Ext.baseCSSPrefix + 'outside',
    prevMonthCls: Ext.baseCSSPrefix + 'prev-month',
    nextMonthCls: Ext.baseCSSPrefix + 'next-month',
    currentMonthCls: Ext.baseCSSPrefix + 'current-month',

    constructor: function(config) {
        this.firstOfMonth = Ext.Date.getFirstDateOfMonth(new Date());
        this.callParent([config]);
    },

    initElement: function() {
        var me = this;

        me.callParent();

        me.headCells = me.headElement.query('th');
        me.bodyCells = me.bodyElement.query('td');
        me.cellMap = {};
    },

    getMonth: function() {
        return Ext.Date.add(this.firstOfMonth, Ext.Date.MONTH, this.getMonthOffset());
    },

    getTemplate: function() {
        var me = this,
            table = me.tableTpl,
            headRow = me.headRowTpl,
            headCell = me.headCellTpl,
            bodyRow = me.bodyRowTpl,
            bodyCell = me.bodyCellTpl,
            rows = me.rows,
            columns = me.columns,
            headTpl, bodyTpl, i, len;

        headRow = Ext.apply({
            children: []
        }, headRow);

        bodyRow = Ext.apply({
            children: []
        }, bodyRow);

        for (i = 0, len = columns; i < len; i++) {
            headRow.children.push(headCell);
            bodyRow.children.push(bodyCell);
        }

        headTpl = Ext.apply({
            children: []
        }, me.headTpl);

        headTpl.children.push(headRow);

        bodyTpl = Ext.apply({
            children: []
        }, me.bodyTpl);

        for (i = 0, len = rows; i < len; i++) {
            bodyTpl.children.push(bodyRow);
        }

        table.children = [me.captionTpl, headTpl, bodyTpl];

        return [table];
    },

    getCellByDate: function(date) {
        return date ? this.cellMap[date.getTime()] : null;
    },

    updateWeekendDays: function() {
        if (!this.isConfiguring) {
            this.refresh();
        }
    },

    updateStartDay: function(dayIndex) {
        var cells = this.headCells,
            weekendDays = this.getWeekendDays(),
            weekendCls = this.weekendDayCls,
            headerLength = this.getHeaderLength(),
            cell, i, len, offsetIdx;

        // We want to do this even during initial config
        for (i = 0, len = cells.length; i < len; i++) {
            cell = cells[i];
            offsetIdx = (i + dayIndex) % 7;
            cell.firstChild.innerHTML =
                Ext.Date.getShortDayName(offsetIdx).substr(0, headerLength);

            Ext.fly(cell).toggleCls(weekendCls, !!weekendDays[offsetIdx]);
        }
    },

    updateSpecialDates: function() {
        if (!this.isConfiguring) {
            this.refresh();
        }
    },

    updateSpecialDays: function() {
        if (!this.isConfiguring) {
            this.refresh();
        }
    },

    applyMonthOffset: function(offset) {
        return !isNaN(offset) ? offset : 0;
    },

    updateMonthOffset: function() {
        this.refresh();
    },

    updateCaptionFormat: function(format) {
        var month = this.getMonth();

        if (month) {
            this.captionElement.setHtml(Ext.Date.format(month, format));
        }
    },

    updateHideCaption: function(hide) {
        this.toggleCls(Ext.baseCSSPrefix + 'hide-caption', hide);
    },

    refresh: function() {
        var me = this,
            ExtDate = Ext.Date,
            cells = me.bodyCells,
            monthStart, startOffset, startDate, startDay, date,
            cellMap, cell, params, i, len, outPrev, outNext,
            currentMonth, month;

        // Calling getters might cause recursive refresh() calls, we don't want that
        if (me.refreshing) {
            return;
        }

        me.refreshing = true;

        monthStart = me.getMonth();
        startDay = me.getStartDay();
        startOffset = startDay - monthStart.getDay();

        if (startOffset > 0) {
            startOffset -= 7;
        }

        startDate = ExtDate.add(monthStart, ExtDate.DAY, startOffset);

        cellMap = me.cellMap = {};

        currentMonth = monthStart.getMonth();

        params = {
            today: Ext.Date.clearTime(new Date()),
            weekendDays: me.getWeekendDays(),
            specialDates: me.getSpecialDates(),
            specialDays: me.getSpecialDays(),
            format: me.getFormat(),
            dateCellFormat: me.getDateCellFormat(),
            hideOutside: me.getHideOutside()
        };

        for (i = 0, len = cells.length; i < len; i++) {
            cell = cells[i];

            date = ExtDate.add(startDate, ExtDate.DAY, i);

            month = date.getMonth();
            outPrev = month < currentMonth;
            outNext = month > currentMonth;

            cellMap[date.getTime()] = cell;

            params.cell = cell;
            params.date = date;

            params.outside = outPrev || outNext;
            params.outsidePrevious = outPrev;
            params.outsideNext = outNext;

            me.refreshCell(params);
        }

        me.captionElement.setHtml(Ext.Date.format(monthStart, me.getCaptionFormat()));

        me.refreshing = false;
    },

    refreshCell: function(params) {
        var me = this,
            cell = params.cell,
            date = params.date,
            dayOfWeek = date.getDay(),
            ms = date.getTime(),
            specialDates = params.specialDates,
            specialDays = params.specialDays,
            cls = [me.cellCls],
            formatted = Ext.Date.format(date, params.format),
            empty = params.outside && params.hideOutside,
            html, special, disabled;

        if (!empty) {
            if (params.outsidePrevious) {
                cls.push(me.outsideCls, me.prevMonthCls);
            } else if (params.outsideNext) {
                cls.push(me.outsideCls, me.nextMonthCls);
            } else {
                cls.push(me.currentMonthCls);

                // Today should not be marked in previous or next month
                if (Ext.Date.isEqual(date, params.today)) {
                    cls.push(me.todayCls);
                }
            }

            if (params.weekendDays[dayOfWeek]) {
                cls.push(me.weekendDayCls);
            }

            if (!special && specialDays) {
                special = specialDays[dayOfWeek];
            }

            if (specialDates) {
                special = specialDates.dates[ms] || (specialDates.re && specialDates.re.test(formatted));
            }

            if (special) {
                cls.push(me.specialDateCls);
            }
        } else {
            cls.push(me.emptyCls);
        }

        disabled = me.getParent().isDateDisabled(date);
        if (!empty && disabled) {
            cls.push(me.disabledDayCls);
        }

        cell.tabIndex = -1;
        if (empty) {
            html = '&#160;';
        } else {
            html = Ext.Date.format(date, params.dateCellFormat);
        }

        cell.firstChild.innerHTML = html;

        if (me.transformCellCls) {
            me.transformCellCls(date, cls);
        }

        cell.className = cls.join(' ');

        // We need this in event handlers
        cell.date = date;
        cell.disabled = disabled;
    },

    ownsDate: function(d) {
        var curr = this.getMonth();
        return d.getFullYear() === curr.getFullYear() && d.getMonth() === curr.getMonth();
    },

    privates: {
        measurePaneSize: function() {
            var el = this.element.first();
            return el.measure('w') + el.getMargin('lr');
        }
    }
});
