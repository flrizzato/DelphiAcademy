/**
 * @private
 *
 * A general {@link Ext.picker.Picker} slot class.  Slots are used to organize multiple scrollable slots into
 * a single {@link Ext.picker.Picker}.
 *
 *     {
 *         name : 'limit_speed',
 *         title: 'Speed Limit',
 *         data : [
 *             {text: '50 KB/s', value: 50},
 *             {text: '100 KB/s', value: 100},
 *             {text: '200 KB/s', value: 200},
 *             {text: '300 KB/s', value: 300}
 *         ]
 *     }
 *
 * See the {@link Ext.picker.Picker} documentation on how to use slots.
 */
Ext.define('Ext.picker.Slot', {
    extend: 'Ext.dataview.DataView',
    xtype: 'pickerslot',
    requires: [
        'Ext.dataview.BoundListNavigationModel'
    ],

    /**
     * @event slotpick
     * Fires whenever an slot is picked
     * @param {Ext.picker.Slot} this
     * @param {Mixed} value The value of the pick
     * @param {HTMLElement} node The node element of the pick
     */

    isSlot: true,

    config: {
        /**
         * @cfg {String} title The title to use for this slot, or `null` for no title.
         * @accessor
         */
        title: null,

        /**
         * @private
         * @cfg {Boolean} showTitle
         * @accessor
         */
        showTitle: true,

        /**
         * @private
         * @cfg {String} cls The main component class
         * @accessor
         */
        cls: Ext.baseCSSPrefix + 'picker-slot',

        /**
         * @cfg {String} name (required) The name of this slot.
         * @accessor
         */
        name: null,

        /**
         * @cfg {Number} value The value of this slot
         * @accessor
         */
        value: null,

        /**
         * @cfg {Number} flex
         * @accessor
         * @private
         */
        flex: 1,

        /**
         * @cfg {String} align The horizontal alignment of the slot's contents.
         *
         * Valid values are: "left", "center", and "right".
         * @accessor
         */
        align: 'left',

        /**
         * @cfg {String} displayField The display field in the store.
         * @accessor
         */
        displayField: 'text',

        /**
         * @cfg {String} valueField The value field in the store.
         * @accessor
         */
        valueField: 'value',

        /**
         * @cfg {String} itemTpl The template to be used in this slot.
         * If you set this, {@link #displayField} will be ignored.
         */
        itemTpl: null,

        /**
         * @cfg {Object} scrollable
         * @accessor
         * @hide
         */
        scrollable: {
            x: false,
            y: true,
            scrollbars: false
        },

        /**
         * @cfg {Boolean} verticallyCenterItems
         * @private
         */
        verticallyCenterItems: true
    },

    tabIndex: null,
    focusEl: null,
    itemsFocusable: false,

    scrollToTopOnRefresh: false,

    snapSelector: '.' + Ext.baseCSSPrefix + 'dataview-item',

    /**
     * @property selectedIndex
     * @type Number
     * The current `selectedIndex` of the picker slot.
     * @private
     */
    selectedIndex: 0,

    deselectable: false,

    navigationModel: {
        type: 'boundlist',
        keyboard: false
    },

    onFocusEnter: Ext.emptyFn,
    onFocusLeave: Ext.emptyFn,

    /**
     * @cfg {'tap'} triggerEvent
     * @hide
     * BoundLists always use tap. This is ignored.
     */

    /**
     * Sets the title for this dataview by creating element.
     * @param {String} title
     * @return {String}
     */
    applyTitle: function(title) {
        //check if the title isnt defined
        if (title) {
            //create a new title element
            title = Ext.create('Ext.Component', {
                cls: Ext.baseCSSPrefix + 'picker-slot-title',
                docked: 'top',
                html: title
            });
        }

        return title;
    },

    updateTitle: function(newTitle, oldTitle) {
        if (newTitle) {
            this.add(newTitle);
            this.setupBar();
        }

        if (oldTitle) {
            this.remove(oldTitle);
        }
    },

    updateShowTitle: function(showTitle) {
        var title = this.getTitle(),
            mode = showTitle ? 'show' : 'hide';
        if (title) {
            title.on(mode, this.setupBar, this, { single: true, delay: 50 });
            title[showTitle ? 'show' : 'hide']();
        }
    },

    updateDisplayField: function(newDisplayField) {
        if (!this.config.itemTpl) {
            this.setItemTpl('<div class="' + Ext.baseCSSPrefix + 'picker-item {cls} <tpl if="extra">' + Ext.baseCSSPrefix + 'picker-invalid</tpl>">{' + newDisplayField + '}</div>');
        }
    },

    /**
     * Updates the {@link #align} configuration
     */
    updateAlign: function(newAlign, oldAlign) {
        var element = this.element;
        element.addCls(Ext.baseCSSPrefix + 'picker-' + newAlign);
        element.removeCls(Ext.baseCSSPrefix + 'picker-' + oldAlign);
    },

    /**
     * Looks at the {@link #data} configuration and turns it into {@link #store}.
     * @param {Object} data
     * @return {Object}
     */
    applyData: function(data) {
        var parsedData = [],
            ln = data && data.length,
            i, item, obj;

        if (data && Ext.isArray(data) && ln) {
            for (i = 0; i < ln; i++) {
                item = data[i];
                obj = {};
                if (Ext.isArray(item)) {
                    obj[this.valueField] = item[0];
                    obj[this.displayField] = item[1];
                }
                else if (Ext.isString(item)) {
                    obj[this.valueField] = item;
                    obj[this.displayField] = item;
                }
                else if (Ext.isObject(item)) {
                    obj = item;
                }
                parsedData.push(obj);
            }
        }

        return data;
    },

    /**
     * @private
     */
    initialize: function() {
        var me = this;

        me.callParent();

        me.on({
            scope: me,
            painted: 'onPainted',
            single: true
        });

        me.picker.on({
            scope: me,
            beforehiddenchange: 'onBeforeHiddenChange'
        });
    },

    /**
     * @private
     */
    onPainted: function() {
        this.setupBar();
    },

    /**
     * @private
     */
    onResize: function() {
        var value = this.getValue();
        if (value) {
            this.doSetValue(value);
        }
    },

    /**
     * @private
     */
    onBeforeHiddenChange: function (picker, hidden) {
        if (!hidden) {
            this.doSetValue(this.getValue());
        }        
    },

    /**
     * Returns an instance of the owner picker.
     * @return {Object}
     * @private
     */
    getPicker: function() {
        if (!this.picker) {
            this.picker = this.getParent();
        }

        return this.picker;
    },

    /**
     * @private
     */
    setupBar: function() {
        if (!this.isPainted()) {
            //if the component isn't rendered yet, there is no point in calculating the padding just yet
            return;
        }

        var me = this,
            title = me.getTitle(),
            titleHeight = me.getShowTitle() && title ? title.el.measure('h') : 0,
            barHeight = me.getPicker().bar.measure('h'),
            offset;

        if (me.getVerticallyCenterItems()) {
            offset = Math.ceil((me.el.measure('h') - titleHeight - barHeight) / 2);
            me.bodyElement.setStyle({
                'padding-top' : offset + 'px'
            });
            // Due to a change on how browsers set the element now, padding is applied
            // at the content edge, not after any overflow. So the padding-bottom will
            // be clipped if the content becomes scrollable.
            // For more info see: https://bugzilla.mozilla.org/show_bug.cgi?id=74851
            if (!me.bottomSpacer) {
                me.bottomSpacer = me.add({
                    xtype: 'component',
                    scrollDock: 'end',
                    height: offset,
                    style: 'pointer-events: none'
                });
            } else {
                me.bottomSpacer.setHeight(offset);
            }
        }

        me.doSetValue(me.getValue());
    },

    /**
     * This method is required by the Scroller to return the scrollable client region
     * @return {Ext.util.Region} The scrolling viewport region.
     *
     * It's overridden here because the region required for scrollIntoView to work
     * is the bar of the picker.
     * @private
     */
    getScrollableClientRegion: function() {
        return this.picker.bar.getClientRegion();
    },

    /**
     * @private
     */
    scrollToItem: function(item, animated) {
        // Scrollable will scroll into the bar region because of our getScrollableClientRegion
        // implementation above.
        this.getScrollable().scrollIntoView(item.el, false, animated);
    },

    /**
     * @private
     * Called directly by our scroller when scrolling has stopped.
     */
    onScrollEnd: function(x, y) {
        var me = this,
            viewItems = me.getViewItems(),
            index = Ext.Number.constrain(Math.round(y / me.picker.bar.measure('h')), 0, viewItems.length - 1),
            item = viewItems[index];

        if (item) {
            me.selectedIndex = index;
            me.selectedNode = item;

            me.setValueAnimated(me.getValue(true));
            me.fireEvent('slotpick', me, me.getValue(), me.selectedNode);
        }
    },

    /**
     * Returns the value of this slot
     * @private
     */
    getValue: function(useDom) {
        var store = this.getStore(),
            record, value;

        if (!store) {
            return;
        }

        if (!useDom) {
            return this._value;
        }

        //if the value is ever false, that means we do not want to return anything
        if (this._value === false) {
            return null;
        }

        record = store.getAt(this.selectedIndex);

        value = record ? record.get(this.getValueField()) : null;

        return value;
    },

    /**
     * Sets the value of this slot
     * @private
     */
    setValue: function(value) {
        return this.doSetValue(value);
    },

    /**
     * Sets the value of this slot
     * @private
     */
    setValueAnimated: function(value) {
        return this.doSetValue(value, true);
    },

    doSetValue: function(value, animated) {
        var me = this,
            hasSelection = true,
            store, index, item;
        
        // Store can be null
        store = me.getStore();
        
        index = store ? store.findExact(me.getValueField(), value) : -1;

        if (index === -1) {
            hasSelection = false;
            index = 0;
        }

        me.selectedIndex = index;

        if (me.refreshCounter) {
            item = Ext.get(me.getViewItems()[index]);
            if (item) {
                me.scrollToItem(item, animated);
                if (hasSelection) {
                    // only set selection if an item is actually selected
                    me.select(me.selectedIndex);
                }
            }
        }

        me._value = value;
    },

    privates: {
        forceRefreshOnRender: true
    }
});
