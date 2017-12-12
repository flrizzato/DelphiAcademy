/**
 * @private
 * @since 6.5.0
 */
Ext.define('Ext.dataview.Abstract', {
    extend: 'Ext.Container',

    mixins: [
        'Ext.mixin.ConfigProxy',
        'Ext.mixin.ItemRippler'
    ],

    /**
     * @property {Boolean} isDataView
     * `true` to identify an object as an instantiated DataView, or subclass thereof.
     */
    isDataView: true,

    requires: [
        'Ext.LoadMask',
        'Ext.XTemplate',
        'Ext.data.StoreManager',
        'Ext.dataview.NavigationModel',
        'Ext.dataview.selection.Model',
        'Ext.dataview.EmptyText'
    ],

    /**
     * @event itemtouchstart
     * Fires whenever an item is touched
     * @param {Ext.dataview.DataView} this
     * @param {Number} index The index of the item touched
     * @param {Ext.Element/Ext.dataview.DataItem} target The element or DataItem touched
     * @param {Ext.data.Model} record The record associated to the item
     * @param {Ext.event.Event} e The event object
     *
     * @deprecated 6.5.0 Use {@link #childtouchstart}
     */

    /**
     * @event itemtouchmove
     * Fires whenever an item is moved
     * @param {Ext.dataview.DataView} this
     * @param {Number} index The index of the item moved
     * @param {Ext.Element/Ext.dataview.DataItem} target The element or DataItem moved
     * @param {Ext.data.Model} record The record associated to the item
     * @param {Ext.event.Event} e The event object
     *
     * @deprecated 6.5.0 Use {@link #childtouchmove}
     */

    /**
     * @event itemtouchend
     * Fires whenever an item is touched
     * @param {Ext.dataview.DataView} this
     * @param {Number} index The index of the item touched
     * @param {Ext.Element/Ext.dataview.DataItem} target The element or DataItem touched
     * @param {Ext.data.Model} record The record associated to the item
     * @param {Ext.event.Event} e The event object
     *
     * @deprecated 6.5.0 Use {@link #childtouchend}
     */

    /**
     * @event itemtouchcancel
     * Fires whenever an item touch is cancelled
     * @param {Ext.dataview.DataView} this
     * @param {Number} index The index of the item touched
     * @param {Ext.Element/Ext.dataview.DataItem} target The element or DataItem touched
     * @param {Ext.data.Model} record The record associated to the item
     * @param {Ext.event.Event} e The event object
     *
     * @deprecated 6.5.0 Use {@link #childtouchcancel}
     */

    /**
     * @event itemtap
     * Fires whenever an item is tapped. Add `x-item-no-tap` CSS class to a child of list
     * item to suppress `itemtap` events on that child. This can be useful when items
     * contain components such as Buttons.
     * @param {Ext.dataview.DataView} this
     * @param {Number} index The index of the item tapped
     * @param {Ext.Element/Ext.dataview.DataItem} target The element or DataItem tapped
     * @param {Ext.data.Model} record The record associated to the item
     * @param {Ext.event.Event} e The event object
     *
     * @deprecated 6.5.0 Use {@link #childtap}
     */

    /**
     * @event itemlongpress
     * Fires whenever an item's longpress event fires
     * @param {Ext.dataview.DataView} this
     * @param {Number} index The index of the item touched
     * @param {Ext.Element/Ext.dataview.DataItem} target The element or DataItem touched
     * @param {Ext.data.Model} record The record associated to the item
     * @param {Ext.event.Event} e The event object
     *
     * @deprecated 6.5.0 Use {@link #childlongpress}
     */

    /**
     * @event itemtaphold
     * Fires whenever an item's taphold event fires
     * @param {Ext.dataview.DataView} this
     * @param {Number} index The index of the item touched
     * @param {Ext.Element/Ext.dataview.DataItem} target The element or DataItem touched
     * @param {Ext.data.Model} record The record associated to the item
     * @param {Ext.event.Event} e The event object
     *
     * @deprecated 6.5.0 Use {@link #childtaphold}
     */

    /**
     * @event itemsingletap
     * Fires whenever an item is singletapped
     * @param {Ext.dataview.DataView} this
     * @param {Number} index The index of the item singletapped
     * @param {Ext.Element/Ext.dataview.DataItem} target The element or DataItem singletapped
     * @param {Ext.data.Model} record The record associated to the item
     * @param {Ext.event.Event} e The event object
     *
     * @deprecated 6.5.0 Use {@link #childsingletap}
     */

    /**
     * @event itemdoubletap
     * Fires whenever an item is doubletapped
     * @param {Ext.dataview.DataView} this
     * @param {Number} index The index of the item doubletapped
     * @param {Ext.Element/Ext.dataview.DataItem} target The element or DataItem doubletapped
     * @param {Ext.data.Model} record The record associated to the item
     * @param {Ext.event.Event} e The event object
     *
     * @deprecated 6.5.0 Use {@link #childdoubletap}
     */

    /**
     * @event itemswipe
     * Fires whenever an item is swiped
     * @param {Ext.dataview.DataView} this
     * @param {Number} index The index of the item swiped
     * @param {Ext.Element/Ext.dataview.DataItem} target The element or DataItem swiped
     * @param {Ext.data.Model} record The record associated to the item
     * @param {Ext.event.Event} e The event object
     *
     * @deprecated 6.5.0 Use {@link #childswipe}
     */

    /**
     * @event itemmouseenter
     * Fires whenever the mouse pointer moves over an item
     * @param {Ext.dataview.DataView} this
     * @param {Number} index The index of the item
     * @param {Ext.Element/Ext.dataview.DataItem} target The element or DataItem
     * @param {Ext.data.Model} record The record associated to the item
     * @param {Ext.event.Event} e The event object
     *
     * @deprecated 6.5.0 Use {@link #childmouseenter}
     */

    /**
     * @event itemmouseleave
     * Fires whenever the mouse pointer leaves an item
     * @param {Ext.dataview.DataView} this
     * @param {Number} index The index of the item
     * @param {Ext.Element/Ext.dataview.DataItem} target The element or DataItem
     * @param {Ext.data.Model} record The record associated to the item
     * @param {Ext.event.Event} e The event object
     *
     * @deprecated 6.5.0 Use {@link #childmouseleave}
     */

    /**
     * @event select
     * Fires whenever an item is selected
     * @param {Ext.dataview.DataView} this
     * @param {Ext.data.Model/Ext.data.Model[]} selected
     * The selected record(s). If {@link #selectable} {@link Ext.dataview.selection.Model#mode mode}
     * is `single`, this will be a single {@link Ext.data.Model record}. If
     * {@link Ext.dataview.selection.Model#mode mode} is `simple` or `multi`, this will be an array
     * of {@link Ext.data.Model records}.
     */

    /**
     * @event deselect
     * Fires whenever an item is deselected
     * @param {Ext.dataview.DataView} this
     * @param {Ext.data.Model[]} records The records being deselected
     */

    /**
     * @event refresh
     * @preventable
     * Fires whenever the DataView is refreshed
     * @param {Ext.dataview.DataView} this
     */

    /**
     * @event navigate
     * Fires whenever the user navigates to a new location.
     *
     * In regular dataviews, a location encapsulates one view item, and its associated record.
     *
     * In grids, a location encapsulates one cell, and its associated data field.
     *
     * @param {Ext.dataview.DataView} this
     * @param {Ext.dataview.Location} to The location navigated to.
     * @param {Ext.dataview.Location} from The location where navigation came from.
     */

    /**
     * @hide
     * @event add
     */

    /**
     * @hide
     * @event remove
     */

    /**
     * @hide
     * @event move
     */

    cachedConfig: {
        /**
         * @cfg {Boolean/Object} [associatedData=true]
         * Set this config to `false` to limit rendering data to just the record's data
         * or to an object to describe the desired associated data. This data is used to
         * satisfy the `itemTpl`. The default of `true` will gather all associated data
         * that is currently loaded. This can be expensive. If only a small amount of the
         * available data is needed, this config can speed up the rendering process.
         *
         * For example, if an `OrderItem` needs the `Item` data but not its parent `Order`,
         * this config can be set like so:
         *
         *      associatedData: {
         *          item: true
         *      }
         *
         * Given the above, only the `item` association (to the `Item` record) will be
         * gathered into the render data.
         *
         * For more details, see {@link Ext.data.Model#getData getData}.
         * @since 6.5.0
         */
        associatedData: null,

        /**
         * @cfg {Boolean} deferEmptyText
         * Set to `false` to not defer `emptyText` being applied until the store's first
         * load.
         */
        deferEmptyText: true,

        /**
         * @cfg {Boolean} deselectOnContainerClick
         * When set to true, tapping on the DataView's background (i.e. not on
         * an item in the DataView) will deselect any currently selected items.
         */
        deselectOnContainerClick: true,

        /**
         * @cfg {Boolean} disableSelection
         * Set to `true` to disable selection styling. This only affects the presentation
         * of the selection not the internal selection state.
         */
        disableSelection: false,

        /**
         * @cfg {Object/Ext.Component} emptyTextDefaults
         * This component config object is used to create the `emptyText` component.
         * @since 6.5.0
         */
        emptyTextDefaults: {
            xtype: 'emptytext'
        },

        /**
         * @cfg {String}
         * The text to render when the rendering of the item via `itemTpl` produces no
         * text.
         */
        emptyItemText: '\xA0',

        /**
         * @cfg {Boolean} itemsFocusable
         * For use by subclasses, not applications.
         *
         * By default the dataview items are focusable, and navigable using an
         * {@link Ext.dataview.NavigationModel}.
         *
         * {@link Ext.grid.Grid grids} set this to false to make rows non-focusable in
         * favour of cells.
         * @private
         */
        itemsFocusable: true,

        /**
         * @cfg {String/String[]/Ext.XTemplate} itemTpl
         * The `tpl` to use for each of the items displayed in this DataView. This template
         * produces HTML and can use the follow CSS class names to influence the response
         * to tapping/clicking child elements:
         *
         *  - `x-no-ripple` - Disables `itemRipple` (primarily for theme-material)
         *  - `x-item-no-select` - Disables item selection
         *  - `x-item-no-tap` - Disables all click or tap processing
         *
         * For example:
         *
         *      itemTpl: '<div>' +
         *                   '...' +
         *                   '<div class="x-item-no-select x-fa fa-gear"></div>' +
         *                   '...' +
         *               '</div>'
         *
         * Because this template produces HTML from record data it can expose applications
         * to security issues if user-provided data is not properly encoded. For example,
         * in previous releases this template was:
         *
         *      itemTpl: '<div>{text}</div>'
         *
         * If the 'text' field contained HTML scripts, these would be evaluated into
         * the application. The `itemTpl` in version 6.5 is now:
         *
         *      itemTpl: '<div>{text:htmlEncode}</div>'
         */
        itemTpl: '<div>{text:htmlEncode}</div>',

        /**
         * @cfg {String/Boolean} loadingText
         * A string to display during data load operations. This text will be displayed
         * in a loading div and the view's contents will be cleared while loading,
         * otherwise the view's contents will continue to display normally until the new
         * data is loaded and the contents are replaced.
         * @locale
         */
        loadingText: 'Loading...',

        /**
         * @cfg {Number} pressedDelay
         * The amount of delay between the `tapstart` and adding the `pressedCls`.
         */
        pressedDelay: 100,

        /**
         * @cfg {Boolean} scrollToTopOnRefresh
         * Scroll the DataView to the top when the DataView is refreshed.
         * @accessor
         */
        scrollToTopOnRefresh: true,

        storeEventListeners: {
            add: 'onStoreAdd',
            beforeload: 'onStoreBeforeLoad',
            clear: 'onStoreClear',
            load: 'onStoreLoad',
            refresh: 'onStoreRefresh',
            remove: 'onStoreRemove',
            update: 'onStoreUpdate'
            // check derived classes before adding new event handlers
        },

        /**
         * @cfg {'childtap'/'childsingletap'/'childdoubletap'/'childswipe'/'childtaphold'/'childlongpress'} triggerEvent
         * Determines what type of touch event causes an item to be selected.
         */
        triggerEvent: 'childtap',

        /**
         * @cfg {'tap'/'singletap'} triggerCtEvent
         * Determines what type of touch event is recognized as a touch on the container.
         */
        triggerCtEvent: 'tap'

    }, // cachedConfig

    config: {

        /**
         * @cfg {boolean} itemButtonMode
         * True to cause items to act like buttons for interaction styling.
         * in ButtonMode items will maintain pressed state whenever pressed down.
         * they will not remove this state for tap distance cancellation or mouse out.
         */
        itemButtonMode: false,

        /**
         * @cfg data
         * @inheritdoc
         */
        data: null,

        /**
         * @cfg {Boolean} emptyState
         * @private
         */
        emptyState: null,

        /**
         * @cfg {String/Boolean} emptyText
         * The text to display in the view when there is no data to display.
         * Set this to `true` to display the default message.
         */
        emptyText: null,

        /**
         * @cfg {Boolean} enableTextSelection
         * True to enable text selection inside this view.
         *
         * @deprecated 6.5.1 Use {@link Ext.Component#userSelectable} instead.
         */
        enableTextSelection: null,

        /**
         * @cfg {Boolean/Object} inline
         * When set to `true` the items within the DataView will have their display set to
         * inline-block and be arranged horizontally. By default the items will wrap to
         * the width of the DataView. Passing an object with `{ wrap: false }` will turn
         * off this wrapping behavior and overflowed items will need to be scrolled to
         * horizontally.
         */
        inline: null,

        /**
         * @cfg {String} itemCls
         * An additional CSS class to apply to items within the DataView.
         */
        itemCls: null,

        /**
         * @cfg {Number} loadingHeight
         * If specified, gives an explicit height for a {@link #cfg!floated} data view
         * when it is showing the {@link #loadingText}, if that is specified. This is
         * useful to prevent the view's height from collapsing to zero when the loading
         * mask is applied and there are no other contents in the data view.
         */
        loadingHeight: null,

        /**
         * @cfg {Boolean} [markDirty=false]
         * `true` to mark items as dirty when the underlying record has been modified.
         *
         * By default there is no special styling for dirty items in data views and
         * {@link Ext.dataview.List Lists}.  When this config is set to `true` each item's
         * element will have a CSS class name of `x-mark-dirty` added to it.  When the
         * underlying record for an item has been modified the item will have the `x-dirty`
         * CSS class.
         *
         * {@link Ext.grid.Grid Grids} style "dirty" cells using a red triangle icon in
         * the corner of the cell.  See
         * {@link Ext.grid.cell.Base#$gridcell-dirty-icon $gridcell-dirty-icon}
         *
         * @since 6.5.1
         */
        markDirty: null,

        navigationModel: {
            type: 'dataview'
        },

        /**
         * @cfg {Object} selectable
         * A configuration object which allows passing of configuration options to create or
         * reconfigure a {@link Ext.dataview.selection.Model selection model}.
         *
         * May contain the following options:
         *
         *     - mode `'single'`, '`simple'` or `'multi'` Simple and Multi are similar in that
         *     click toggle selection. Multi allows SHIFT+click and CTRL+click. Single simply
         *     toggles an item between selected and unselected (unless `deselectable` is set to `false`)
         *     - deselectable Configure as false to disallow deselecting down to zero selections.
         */
        selectable: true
    },

    /**
     * @cfg autoSize
     * @inheritdoc
     */
    autoSize: null,

    /**
     * @cfg publishes
     * @inheritdoc
     */
    publishes: {
        selection: 1
    },

    /**
     * @cfg twoWayBindable
     * @inheritdoc
     */
    twoWayBindable: {
        selection: 1
    },

    eventedConfig: {
        /**
         * @cfg {Ext.data.Store/Object} store (required)
         * Can be either a Store instance or a configuration object that will be turned
         * into a Store. The Store is used to populate the set of items that will be
         * rendered in the DataView. See the DataView intro documentation for more
         * information about the relationship between Store and DataView.
         */
        store: undefined
    },

    /**
     * @cfg {'start'/'emd'} scrollDock
     * This property is placed on the _child items_ added to this container. The value
     * placed on the child items determines the position of that item with respect to
     * the data items.
     *
     *      Ext.Viewport.add({
     *          xtype: 'dataview',
     *          itemTpl: '{firstName}',
     *          data: [
     *              { firstName: 'Peter'},
     *              { firstName: 'Raymond'},
     *              { firstName: 'Egon'},
     *              { firstName: 'Winston'}
     *          ],
     *          items: [{
     *               xtype: 'component',
     *               html: 'Always At End!',
     *               scrollDock: 'end'
     *          }]
     *      });
     *
     * Note, a value of `'top'` is equivalent to `'start'` while `'bottom'` is
     * equivalent to `'end'`. The `'top'` and `'bottom'` values originated from the
     * `Ext.dataview.List` class.
     */

    /**
     * @cfg {Ext.data.Model} selection
     * The selected record.
     * @readonly
     */

    proxyConfig: {
        selectable: {
            configs: [
                'mode',
                'deselectable',
                'lastSelected',
                'selected'
            ],
            methods: [
                'isSelected',
                'select',
                'selectAll',
                'deselectAll',
                'getSelections',
                'hasSelection',
                'getSelectionCount'
            ]
        }
    },

    /**
     * @cfg {String} emptyTextProperty
     * The config to set on the `emptyText` component to contain the desired text.
     * @since 6.5.0
     */
    emptyTextProperty: 'html',

    /**
     * @property {Boolean} restoreFocus
     * By default, using the TAB key to *re*enter a grid restores focus to the cell which was last focused.
     *
     * Setting this to `false` means that `TAB` from above focuses the first *rendered* cell
     * and `TAB` from below focuses the last *rendered* cell.
     *
     * Be aware that due to buffered rendering, the last row of a 1,000,000 row grid may not
     * be available to receive immediate focus.
     */
    restoreFocus: true,

    /**
     * @readonly
     * @property {Number} refreshCounter
     * The number of refreshes this DataView has had.
     */
    refreshCounter: 0,

    /**
     * @property {String} selectionModel
     * @private
     * @readonly
     * The selection model type to create. Defaults to `'dataview'` for DataViews and Lists.
     */
    selectionModel: 'dataview',

    /**
     * @property defaultBindProperty
     * @inheritdoc
     */
    defaultBindProperty: 'store',
    
    /**
     * @property
     * @inheritdoc
     */
    focusable: true,

    /**
     * @cfg scrollable
     * @inheritdoc
     */
    scrollable: true,

    /**
     * @cfg tabIndex
     * @inheritdoc
     */
    tabIndex: 0,

    /**
     * @property classCls
     * @inheritdoc
     */
    classCls: Ext.baseCSSPrefix + 'dataview',
    focusedCls: Ext.baseCSSPrefix + 'focused',
    hoveredCls: Ext.baseCSSPrefix + 'hovered',
    inlineCls: Ext.baseCSSPrefix + 'inline',
    noWrapCls: Ext.baseCSSPrefix + 'nowrap',
    pressedCls: Ext.baseCSSPrefix + 'pressed',
    scrollDockCls: Ext.baseCSSPrefix + 'scrolldock',
    selectedCls: Ext.baseCSSPrefix + 'selected',

    hasLoadedStore: false,

    scrollDockedItems: null,

    beforeInitialize: function (config) {
        /**
         * @property {Ext.dom.Element[]/Ext.Component[]} dataItems
         * The array of data items. This array is maintained in store order. The type of
         * objects in this array depend on the type of this dataview. Further, infinite
         * lists only put the actually rendered portion of the store in this array.
         *
         * **NOTE:** This is not the same thing as the items maintained by this `Container`
         * since there could be items in the container that are not associated to any
         * record in the store.
         * @private
         * @readonly
         */
        this.dataItems = [];

        this.callParent([ config ]);
    },

    initialize: function() {
        var me = this;

        me.generateSelectorFunctions();
        me.callParent();

        // Must use the bodyElement here, because we may want to listen to things like pinned headers or
        // other floating pieces.
        me.bodyElement.on({
            touchstart: '_onChildTouchStart',
            touchend: '_onChildTouchEnd',
            touchcancel: '_onChildTouchCancel',
            tap: '_onChildTap',
            tapcancel: '_onChildTapCancel',
            longpress: '_onChildLongPress',
            taphold: '_onChildTapHold',
            singletap: '_onChildSingleTap',
            doubletap: '_onChildDoubleTap',
            swipe: '_onChildSwipe',
            mouseover: '_onChildMouseOver',
            mouseout: '_onChildMouseOut',
            contextmenu: '_onChildContextMenu',
            delegate: me.eventDelegate,
            scope: me
        });

        // If there are space-taking scrollbars, prevent mousedown on a scrollbar
        // from focusing the view.
        if (Ext.getScrollbarSize().width) {
            me.bodyElement.on('touchstart', '_onContainerTouchStart', me);
        }

        me.on(me.getTriggerCtEvent(), 'onContainerTrigger', me);
    },

    onRender: function() {
        var me = this;

        me.callParent();
        if (me.forceRefreshOnRender) {
            me.runRefresh();
        } else {
            me.refresh();
        }
    },

    doDestroy: function() {
        var me = this;

        me.destroyAllRipples();
        me.clearPressedTimer();
        me.setStore(null);
        me.setNavigationModel(null);
        me.setSelectable(null);
        me.lastPressedLocation = null;

        me.callParent();
    },

    createEmptyText: function (emptyText) {
        var ret = Ext.apply({}, this.getEmptyTextDefaults());

        if (typeof emptyText === 'string') {
            ret[this.emptyTextProperty] = emptyText;
        }
        else if (emptyText) {
            Ext.apply(ret, emptyText);
        }

        ret.isEmptyText = ret.hidden = true;
        ret.showInEmptyState = null;

        return ret;
    },

    /**
     * Scrolls the specified record into view.
     *
     * @param {Number/Ext.data.Model} [record] The record or the 0-based position
     * to which to scroll. If this parameter is not passed, the `options` argument must
     * be passed and contain either `record` or `recordIndex`.
     *
     * @param {Object} [options] An object containing options to modify the operation.
     *
     * @param {Boolean} [options.animation] Pass `true` to animate the row into view.
     *
     * @param {Boolean} [options.focus] Pass as `true` to focus the specified row.
     *
     * @param {Boolean} [options.highlight] Pass `true` to highlight the row with a glow
     * animation when it is in view.
     *
     * @param {Ext.data.Model} [options.record] The record to which to scroll.
     *
     * @param {Number} [options.recordIndex] The 0-based position to which to scroll.
     *
     * @param {Boolean} [options.select] Pass as `true` to select the specified row.
     */
    ensureVisible: function (record, options) {
        var me = this,
            plan = me.ensureVisiblePlan(record, options),
            step;

        //TODO highlight
        for (;;) {
            if (!(step = plan.steps.pop())) {
                break;
            }

            me[step](plan);
        }

        return plan.promise;
    },

    gatherData: function (record, recordIndex) {
        var me = this,
            data = record && record.getData(me.associatedData);

        if (data) {
            if (recordIndex === undefined) {
                recordIndex = me.store.indexOf(record);
            }

            data = me.prepareData(data, recordIndex, record);
        }

        return data || null;
    },

    getFirstDataItem: function() {
        return this.dataItems[0] || null;
    },

    getFirstItem: function() {
        return this.getFastItems()[0] || null;
    },

    /**
     * Returns an item at the specified view `index`. This may return items that do not
     * correspond to a {@link Ext.data.Model record} in the store if such items have been
     * added to this container.
     *
     * Negative numbers are treated as relative to the end such that `-1` is the last
     * item, `-2` is the next-to-last and so on.
     *
     * The `mapToItem` method recommended over this method as it is more flexible and can
     * also handle a {@link Ext.data.Model record} as the parameter. To handle store
     * index values, use `mapToViewIndex`:
     *
     *      item = view.mapToItem(view.mapToViewIndex(storeIndex));
     *
     * @param {Number} index The index of the item in the view.
     * @return {HTMLElement/Ext.Component}
     */
    getItemAt: function (index) {
        var items = this.getFastItems();

        if (index < 0) {
            index += items.length;
        }

        return items[index] || null;
    },

    /**
     * Returns the item's index in the store, or -1 if the item does not correspond to a
     * {@link Ext.data.Model record}.
     *
     * **Deprecated** Historically this method has always returned the record's index in
     * the `store`. In most uses this was assumed to match the view index. But this is
     * not always the case, especially for the `Ext.List` subclass. To be clear about
     * which index is being requested, new code should instead call `mapToViewIndex` or
     * `mapToRecordIndex`.
     *
     * @param {Ext.dom.Element/HTMLElement/Ext.Component} item The item to locate.
     * @return {Number} Index for the specified item.
     * @deprecated 6.5.0 Use `mapToViewIndex` or `mapToRecordIndex` instead.
     */
    getItemIndex: function (item) {
        return this.mapToRecordIndex(item);
    },

    getItem: function(record) {
        var ret = null,
            idx;

        if (record) {
            idx = record.isEntity ? this.store.indexOf(record) : record;
            if (idx > -1) {
                ret = this.getItemAt(idx);
            }
        }

        return ret;
    },

    getLastDataItem: function() {
        var dataItems = this.dataItems;

        return dataItems[dataItems.length - 1] || null;
    },

    getLastItem: function() {
        var items = this.getFastItems();
        return items[items.length - 1];
    },

    /**
     * Returns all the items that are docked at the ends of the items.
     * @param {'start'/'end'} which The set of desired `scrollDock` items.
     * @return {Ext.Component[]} An array of the `scrollDock` items.
     */
    getScrollDockedItems: function (which) {
        var scrollDock = this.scrollDockedItems;

        if (scrollDock) {
            if (which) {
                which = this.scrollDockAliases[which] || which;
                scrollDock = scrollDock[which].slice();
            }
            else {
                scrollDock = scrollDock.start.items.concat(scrollDock.end.items);
            }
        }

        return scrollDock || [];
    },

    /**
     * Returns an array of the current items in the DataView. Depends on the {@link #cfg-useComponents}
     * configuration.
     * @return {HTMLElement[]/Ext.dataview.DataItem[]} The items.
     * @method getViewItems
     */

    isItemSelected: function(item) {
        var record = this.mapToRecord(item);
        return record ? this.isSelected(record) : false;
    },

    isFirstItem: function(item) {
        return Ext.getDom(item) === this.getFirstItem();
    },

    isFirstDataItem: function(item) {
        return Ext.getDom(item) === this.getFirstDataItem();
    },

    isLastItem: function(item) {
        return Ext.getDom(item) === this.getLastItem();
    },

    isLastDataItem: function(item) {
        return Ext.getDom(item) === this.getLastDataItem();
    },

    /**
     * Converts the given `indexOrRecord` to an "item".
     *
     * An "item" can be either an `Ext.dom.Element` or an `Ext.Component` depending on the
     * type of dataview. For convenience the `as` parameter can be used to convert the
     * returned item to a common type such as `Ext.dom.Element` or `HTMLElement`.
     *
     * Be aware that the `Ext.List` subclass can optionally render only some records, in
     * which case not all records will have an associated item in the view and this method
     * will return `null`.
     *
     * An index value is a view index. These will only match the record's index in the
     * `store` when no extra items are added to this dataview (so called "non-record"
     * items). These are often unaligned in `Ext.List` due to group headers as well as
     * `infinite` mode where not all records are rendered into the view at one time.
     *
     * Negative index values are treated as relative to the end such that `-1` is the last
     * item, `-2` is the next-to-last and so on.
     *
     * For example:
     *
     *      // Add "foo" class to the last item in the view
     *      view.mapToItem(-1, 'el').addCls('foo');
     *
     *      // Add "foo" class to the last data item in the view
     *      view.mapToItem(view.getStore().last(), 'el').addCls('foo');
     *
     * To handle a record's index in the `store`:
     *
     *      item = view.mapToItem(view.mapToViewIndex(storeIndex));
     *
     * @param {Number/Ext.data.Model/Ext.event.Event} value The event, view index or
     * {@link Ext.data.Model record}.
     *
     * @param {"dom"/"el"} [as] Pass `"dom"` to always return an `HTMLElement` for the item.
     * For component dataviews this is the component's main element. Pass `"el"` to return
     * the `Ext.dom.Element` form of the item. For component dataviews this will be the
     * component's main element. For other dataviews the returned instance is produced by
     * {@link Ext#fly Ext.fly()} and should not be retained.
     *
     * @return {HTMLElement/Ext.dom.Element/Ext.Component}
     * @since 6.5.0
     */
    mapToItem: function (value, as) {
        var me = this,
            el = me.element,
            item, items;

        if (value && value.isEvent) {
            item = value.getTarget(me.itemSelector, el);
        }
        else if (value && (value.isElement || value.nodeType === 1)) {
            item = Ext.fly(value).findParent(me.itemSelector, el);
        }
        else if (value && value.isEntity) {
            item = me.itemFromRecord(value);
        }
        else {
            if (value && value.isComponent && me.items.contains(value)) {
                item = value;
            }
            else {
                // Only map it if it is not already one of our items
                items = me.getFastItems();

                if (value < 0) {
                    value += items.length; // -1 is last, -2 next-to-last, etc
                }

                item = items[value || 0];
            }
        }

        if (item) {
            item = me.itemAs(item, as || (me.isElementDataView ? 'el' : 'cmp'));
        }

        return item || null;
    },

    /**
     * Converts the given parameter to a {@link Ext.data.Model record}. Not all items
     * in a dataview correspond to records (such as group headers in `Ext.List`). In these
     * cases `null` is returned.
     *
     * An "item" can be simply an element or a component depending on the type of dataview.
     *
     * An index value is a view index. These will only match the record's index in the
     * `store` when no extra items are added to this dataview (so called "non-record"
     * items). These are often unaligned in `Ext.List` due to group headers as well as
     * `infinite` mode where not all records are rendered into the view at one time.
     *
     * Negative index values are treated as relative to the end such that `-1` is the last
     * item, `-2` is the next-to-last and so on.
     *
     * @param {Ext.event.Event/Number/HTMLElement/Ext.dom.Element/Ext.Component} value
     * @return {Ext.data.Model} The associated record or `null` if there is none.
     * @since 6.5.0
     */
    mapToRecord: function (value) {
        var me = this,
            item = value,
            el = me.element,
            dom, rec;

        if (item && item.isEvent) {
            item = item.getTarget(me.itemSelector, el);
        }
        else if (item && (item.isElement || item.nodeType === 1)) {
            item = Ext.fly(item).findParent(me.itemSelector, el);
        }
        else if (typeof item === 'number') {
            item = me.mapToItem(item);
        }

        if (item) {
            // Items are either components or elements
            dom = item.isWidget ? item.el : item;
            dom = dom.dom || dom;  // unwrap Ext.Elements

            if (this.itemSelector(dom)) {
                rec = dom.getAttribute('data-recordid');
                rec = rec && me.store.getByInternalId(+rec);
            }
        }

        return rec || null;
    },

    /**
     * Converts the given parameter to the record's index in the `store`. Not all items
     * in a dataview correspond to records (such as group headers in `Ext.List`). In these
     * cases `-1` is returned.
     *
     * An "item" can be simply an element or a component depending on the type of dataview.
     *
     * An input index value is a view index. These will only match the record's index in
     * the `store` when no extra items are added to this dataview (so called "non-record"
     * items). These are often unaligned in `Ext.List` due to group headers as well as
     * `infinite` mode where not all records are rendered into the view at one time.
     *
     * Negative index values are treated as relative to the end such that `-1` is the last
     * item, `-2` is the next-to-last and so on.
     *
     * @param {Ext.event.Event/Number/HTMLElement/Ext.dom.Element/Ext.Component/Ext.data.Model} value
     * @return {Number} The record's index in the store or -1 if not found.
     * @since 6.5.0
     */
    mapToRecordIndex: function (value) {
        var me = this,
            item = value,
            index = -1,
            el = me.element,
            dom;

        if (item && item.isEntity) {
            index = me.store.indexOf(item);
        }
        else {
            if (item && item.isEvent) {
                item = item.getTarget(me.itemSelector, el);
            }
            else if (item && (item.isElement || item.nodeType === 1)) {
                item = Ext.fly(item).findParent(me.itemSelector, el);
            }
            else if (typeof item === 'number') {
                item = me.mapToItem(item);
            }

            if (item) {
                // Items are either components or elements
                dom = item.isWidget ? item.el : item;
                dom = dom.dom || dom;  // unwrap Ext.Elements

                // If we have been handed a detached DOM, ignore it.
                if (me.itemSelector(dom)) {
                    index = dom.getAttribute('data-recordindex');
                    index = index ? +index : -1;
                }
            }
        }

        return index;
    },

    /**
     * Converts the given parameter to the equivalent record index in the `store`.
     *
     * In this method alone, the index parameter is a *store index* not a *view index*.
     *
     * Be aware that the `Ext.List` subclass can optionally render only some records, in
     * which case not all records will have an associated item in the view and this method
     * will return `-1`.
     *
     * Negative index values are treated as relative to the end such that `-1` is the last
     * record, `-2` is the next-to-last and so on.
     *
     * An "item" can be simply an element or a component depending on the type of dataview.
     *
     * The view index will only match the record's index in the `store` when no extra
     * items are added to this dataview (so called "non-record" items). These are often
     * unaligned in `Ext.List` due to group headers as well as `infinite` mode where not
     * all records are rendered into the view at one time.
     *
     * @param {Ext.event.Event/Number/HTMLElement/Ext.dom.Element/Ext.Component/Ext.data.Model} value
     * @param {Number} [indexOffset] (private) This is passed by an infinite list.
     * @return {Number} The view index or -1 if not found.
     * @since 6.5.0
     */
    mapToViewIndex: function (value, indexOffset) {
        var me = this,
            index = -1,
            item = value,
            el = me.element,
            items = me.getFastItems(),
            dom;

        if (typeof item === 'number') {
            indexOffset = indexOffset || 0;

            // We start looking for the matching item at the record index. If there
            // are no special items in the view, that will be the item we want. If
            // not, the item must follow it so we advance along looking for a match.
            for (; item < items.length; ++item) {
                dom = items[item];

                if (dom.isWidget) {
                    dom = dom.el.dom;
                }

                // Infinite lists pass the record index of the top of the rendered
                // range as well as subtract that value from the index we are looking
                // for. This aligns the first index with the view items and then we
                // add back that offset when comparing record index values.
                //
                if (+dom.getAttribute('data-recordindex') === item + indexOffset) {
                    index = item;
                    break;
                }
            }
        }
        else if (item) {
            if (item.isEntity) {
                item = me.itemFromRecord(item);
            }
            else if (item.isEvent) {
                item = item.getTarget(me.itemSelector, el);
            }
            else if (item.isElement || item.nodeType === 1) {
                item = Ext.fly(item).findParent(me.itemSelector, el);
            }

            if (item && items.length) {
                if (items[0].isWidget) {
                    if (!item.isWidget) {
                        item = Ext.Component.from(item);
                    }
                }
                else {
                    // raw DOM nodes...
                    item = item.nodeType ? item : item.el.dom;  // "el" is a loopback on Ext.Element
                }

                // For component dataviews and lists, fastItems is an array, but for
                // element dataviews it is a NodeList (which has no indexOf method)
                // Fortunately we can hoist the one from Array.prototype
                //
                index = Array.prototype.indexOf.call(items, item);
            }
        }

        return index;
    },

    /**
     * Returns the item following the passed `item` in the view. For `infinite` lists, this
     * traversal can encounter unrendered records. In this case, the record index of the
     * unrendered record is returned.
     *
     * If `as` is specified, the item is converted to the desired form, if possible. If
     * that conversion cannot be performed, `null` is returned.
     *
     * @param {Ext.dom.Element/Ext.Component} item The item from which to navigate.
     *
     * @param {"cmp"/"dom"/"el"} [as] Pass `"dom"` to always return an `HTMLElement` for
     * the item. For component dataviews this is the component's main element. Pass `"el"`
     * to return the `Ext.dom.Element` form of the item. For component dataviews this will
     * be the component's main element. For other dataviews the returned instance is
     * produced by {@link Ext#fly Ext.fly()} and should not be retained. Pass `"cmp"` to
     * return the `Ext.Component` reference for the item (if one exists).
     *
     * @return {Number/HTMLElement/Ext.dom.Element/Ext.Component}
     */
    nextItem: function (item, as) {
        var next = this.traverseItem(item, 1);
        return as ? this.itemAs(next, as) : next;
    },

    /**
     * Returns the item preceding the passed `item` in the view. For `infinite` lists, this
     * traversal can encounter unrendered records. In this case, the record index of the
     * unrendered record is returned.
     *
     * If `as` is specified, the item is converted to the desired form, if possible. If
     * that conversion cannot be performed, `null` is returned.
     *
     * @param {Ext.dom.Element/Ext.Component} item The item from which to navigate.
     *
     * @param {"cmp"/"dom"/"el"} [as] Pass `"dom"` to always return an `HTMLElement` for
     * the item. For component dataviews this is the component's main element. Pass `"el"`
     * to return the `Ext.dom.Element` form of the item. For component dataviews this will
     * be the component's main element. For other dataviews the returned instance is
     * produced by {@link Ext#fly Ext.fly()} and should not be retained. Pass `"cmp"` to
     * return the `Ext.Component` reference for the item (if one exists).
     *
     * @return {Number/HTMLElement/Ext.dom.Element/Ext.Component}
     */
    previousItem: function (item, as) {
        var prev = this.traverseItem(item, -1);
        return as ? this.itemAs(prev, as) : prev;
    },

    /**
     * Function which can be overridden to provide custom formatting for each Record that is used
     * by this DataView's {@link #tpl template} to render each node.
     * @param {Object/Object[]} data The raw data object that was used to create the Record.
     * @param {Number} index the index number of the Record being prepared for rendering.
     * @param {Ext.data.Model} record The Record being prepared for rendering.
     * @return {Array/Object} The formatted data in a format expected by the internal
     * {@link #tpl template}'s `overwrite()` method.
     * (either an array if your params are numeric (i.e. `{0}`) or an object (i.e. `{foo: 'bar'}`))
     */
    prepareData: function (data, index, record) {
        return data;
    },

    /**
     * Refreshes the view by reloading the data from the store and re-rendering the template.
     */
    refresh: function () {
        this.whenVisible('runRefresh');
    },

    //---------------------------------------------------
    // Event handlers

    onFocusEnter: function(e) {
        var me = this;

        me.callParent([e]);

        // Not inside the view on on our focus catching el, Component's handling
        // will be enough, so return;
        if (!(e.within(me.getRenderTarget()) || e.target === me.getFocusEl().dom)) {
            return;
        }

        // We are entering the view items
        return me.onInnerFocusEnter(e);
    },

    onInnerFocusEnter: function(e) {
        var me = this,
            navigationModel = me.getNavigationModel(),
            focusPosition, itemCount;

        // This is set on mousedown on the scrollbar.
        // IE/Edge focuses the element on mousedown on a scrollbar.
        // which is not what we want, so throw focus back in this
        // situation.
        // See this#_onContainerTouchStart for this being set.
        if (navigationModel.lastLocation === 'scrollbar') {
            if (e.relatedTarget) {
                e.relatedTarget.focus();
            }
            
            return;
        }

        // TAB onto the view
        if (e.target === me.getFocusEl().dom) {
            focusPosition = me.restoreFocus && navigationModel.getPreviousLocation();
            if (focusPosition) {
                // In case the record has been moved or deleted, refresh resyncs the location
                // with reality. In the case of a gone record, this reorientates on the
                // same rowIndex.
                // Convert that last location back to the default Location class.
                // Subclasses may implement different Location subclasses to encapsulate
                // different location types. eg: Grid's Actionlocation
                focusPosition = focusPosition.refresh();
            }
            // SHIFT+TAB focuses last rendered position.
            // Locations understand Components AND Element as inputs into their Record property.
            else if (e.backwards) {
                focusPosition = me.getLastDataItem();
            }
            // TAB focuses first rendered position.
            // Locations understand Components AND Element as inputs into their Record property.
            else {
                focusPosition = me.getFirstDataItem();
            }
        }
        // Click/tap on an item, or focus being restored into an inner element
        // NavMode#setLocation must be able to understand an event.
        else {
            focusPosition = e;
        }

        // Disable tabbability of elements within this view.
        me.toggleChildrenTabbability(false);

        itemCount = me.getFastItems().length;  //TODO should this be dataItems?

        if (itemCount) {
            // If useComponents is set, an item will be a component.
            // Use a widget's focusEl by preference in case it implements an inner
            // element as focusable. If not, List#createItem uses the encapsulating el.
            if (focusPosition.isWidget) {
                focusPosition = focusPosition.getFocusEl() || focusPosition.el;
            }

            // Focus entered from after the view.
            navigationModel.setLocation(focusPosition, {
                event: e,
                navigate: false
            });
        }

        // View's main el should be kept untabbable, otherwise pressing
        // Shift-Tab key in the view would move the focus to the main el
        // which will then bounce it back to the last focused item.
        // That would effectively make Shift-Tab unusable.
        if (navigationModel.getLocation()) {
            me.el.dom.setAttribute('tabIndex', -1);
        }
    },

    onFocusLeave: function(e) {
        var me = this,
            navModel = me.getNavigationModel();

        // Ignore this event if we do not actually contain focus,
        // or if the reason for focus exiting was that we are refreshing.
        if (navModel.getLocation()) {
            // Blur the focused cell
            navModel.setLocation(null, {
                event: e
            });

            me.el.dom.setAttribute('tabIndex', 0);
        }

        me.callParent([e]);
    },

    // Moved into a docked item.
    onInnerFocusLeave: function(e) {
        // Blur the focused cell
        this.getNavigationModel().setLocation(null, {
            event: e
        });
    },

    onFocusMove: function(e) {
        var me = this,
            el = me.el,
            renderTarget = me.getRenderTarget(),
            toComponent = e.event.toComponent,
            fromComponent = e.event.fromComponent;

        /*
         * This little bit of horror is because the grid is not a pure view.
         * It may contain docked items such as the HeaderContainer, or
         * TitleBar which may contain focusable items, and which will result
         * in focusmove events.
         * We need to filter focusmove events which involve moving into or out of the
         * view, and also those which are fully outside the view.
         */

        // The focus is within the component's tree, but to an outside element.
        // This does not affect navigation's location
        if (!el.contains(e.toElement)) {
            return me.callParent([e]);
        }
        // Focus moved out of row container into docked items.
        // The toElement may be outside of this.el, in a descendant floated.
        // This would represent an internal focusMove.
        if (el.contains(e.toElement) && !renderTarget.contains(e.toElement) && 
                renderTarget.contains(e.fromElement)) {
            return me.onInnerFocusLeave(e.event);
        }
        // Focus from docked items into row container.
        if (el.contains(e.fromElement) && !renderTarget.contains(e.fromElement) &&
                renderTarget.contains(e.toElement)) {
            return me.onInnerFocusEnter(e.event);
        }
        // Focus move within docked items
        if (!renderTarget.contains(e.fromElement) && !renderTarget.contains(e.toElement)) {
            return me.callParent([e]);
        }

        // Only process a focus move if we are the owner of the focusmove.
        // If it's inside a nested dataview, we are not responsible, we're just seeing
        // the bubble phase of this event.
        if ((toComponent === me || toComponent.up('dataview,componentdataview') === me) &&
            (fromComponent === me || fromComponent.up('dataview,componentdataview') === me)) {
            me.getNavigationModel().onFocusMove(e.event);
        }
        return me.callParent([e]);
    },

    onItemAdd: function (item, index) {
        var me = this,
            scrollDock = item.scrollDock,
            scrollDockCls = me.scrollDockCls,
            scrollDockedItems;

        if (!item.$dataItem && item.isInner) {

            if (scrollDock !== null) {
                scrollDock = scrollDock || 'end';
            }

            if(scrollDock) {
                if (!(scrollDockedItems = me.scrollDockedItems)) {
                    me.scrollDockedItems = scrollDockedItems = {
                        start: {
                            items: [],
                            height: 0,
                            filter: me.filterScrollDockStart,
                            name: scrollDock
                        },
                        end: {
                            items: [],
                            height: 0,
                            filter: me.filterScrollDockEnd,
                            name: scrollDock
                        }
                    };
                }

                scrollDock = me.scrollDockAliases[scrollDock] || scrollDock;

                //<debug>
                if (!scrollDockedItems[scrollDock]) {
                    Ext.raise('Invalid value for scrollDock: ' + item.scrollDock);
                }
                //</debug>

                item.scrollDock = scrollDock;  // follow the alias remap
                scrollDock = scrollDockedItems[scrollDock];
                scrollDock.items = me.innerItems.filter(scrollDock.filter);

                if (item.showInEmptyState === undefined) {
                    item.showInEmptyState = false;
                }

                item.addCls(scrollDockCls + ' ' + scrollDockCls + '-' + scrollDock.name);

                if (me.getItemsFocusable()) {
                    item.el.set({
                        tabIndex: -1
                    });
                }

                if (me.addScrollDockedItem) {
                    me.addScrollDockedItem(item);
                }
            }
        }

        me.callParent([item, index]);
    },

    // invoked by the selection model to maintain visual UI cues
    onItemDeselect: function(records, suppressEvent) {
        var me = this;

        if (!me.isConfiguring && !me.destroyed) {
            if (suppressEvent) {
                me.setItemSelection(records, false);
            }
            else {
                me.fireEventedAction('deselect', [me, records], 'setItemSelection',
                    me, [records, false]);
            }
        }
    },

    // invoked by the selection model to maintain visual UI cues
    onItemSelect: function(records, suppressEvent) {
        var me = this;

        if (suppressEvent) {
            me.setItemSelection(records, true);
        } else {
            me.fireEventedAction('select', [me, records], 'setItemSelection',
                me, [records, true]);
        }
    },

    onChildTouchStart: function (location) {
        var me = this,
            child = location.item,
            e = location.event,
            hasListeners = me.hasListeners,
            curLocation = me.getNavigationModel().getLocation(),
            actionable = curLocation && curLocation.actionable,
            name, skip;

        // Don't ripple if we're clicking on an actionable location, or if we're clicking
        // in the location where we are already focused.
        if (!location.actionable && !(location.equalCell || location.equals)(curLocation)) {
            me.rippleItem(child, e);
        }

        // Because this has to fire both the deprecated/new events we can't use fireEventedAction
        name = 'beforechildtouchstart';
        skip = hasListeners[name] && me.fireEvent(name, me, location) === false;
        if (!skip) {
            name = 'beforeitemtouchstart';
            skip = hasListeners[name] &&
                me.fireEvent(name, me, location.viewIndex, child, location.record, e) === false;
        }

        if (!skip) {
            // Don't do the item press if we're in an actionable location
            if (!actionable) {
                me.doChildTouchStart(location);
            }
            me.fireChildEvent('touchstart', location);
        }
    },

    onChildTouchEnd: function(location) {
        var me = this,
            child = location.item,
            curLocation = me.getNavigationModel().getLocation(),
            e = location.event;

        // Don't ripple if our location is actionable.
        if (!(curLocation && curLocation.actionable)) {
            me.rippleItem(child, e);
        }
        this.clearPressedCls('touchend', location);
    },

    onChildTouchCancel: function(location) {
        this.clearPressedCls('touchcancel', location);
    },

    onChildTouchMove: function(location) {
        this.fireChildEvent('touchmove', location);
    },

    onChildTap: function(location) {
        this.fireChildEvent('tap', location);
    },

    onChildTapCancel: function(location) {
        var me = this,
            itemButtonMode = me.getItemButtonMode();

        if (!itemButtonMode) {
            this.clearPressedCls('tapcancel', location);
        }
    },

    onChildContextMenu: function(location) {
        this.fireChildEvent('contextmenu', location);
    },

    onChildLongPress: function(location) {
        this.fireChildEvent('longpress', location);
    },

    onChildTapHold: function(location) {
        this.fireChildEvent('taphold', location);
    },

    onChildSingleTap: function(location) {
        this.fireChildEvent('singletap', location);
    },

    onChildDoubleTap: function(location) {
        this.fireChildEvent('doubletap', location);
    },

    onChildSwipe: function(location) {
        this.fireChildEvent('swipe', location);
    },

    onChildMouseOver: function(location) {
        var me = this,
            child = location.item;

        if (me.mouseOverItem !== child) {
            me.mouseOverItem = child;

            if (me.doHover) {
                me.toggleHoverCls(true);
            }

            me.fireChildEvent('mouseenter', location);
        }
    },

    onChildMouseOut: function(location) {
        var me = this,
            itemButtonMode = me.getItemButtonMode(),
            child = location.item,
            relatedTarget = location.event.getRelatedTarget(me.itemSelector);

        if (child && child.dom !== relatedTarget) {
            if (me.doHover) {
               me.toggleHoverCls(false);
            }

            if (!itemButtonMode) {
                this.clearPressedCls('mouseleave', location);
            } else {
                me.fireChildEvent('mouseleave', location);
            }
            me.mouseOverItem = null;
        }
    },

    /**
     * This method is called by the {@link #cfg!navigationModel} when navigation events are
     * detected within this DataView.
     *
     * It may be overridden to control the linkage of navigation events such as
     * taps, clicks or keystrokes detected by the {@link #cfg!navigationModel} to
     * the {@link #cfg!selectionModel}.
     *
     * `callParent` if you wish selection to proceed from the passed event.
     * @param {Ext.event.Event} e The UI event which caused the navigation.
     *
     * @protected
     */
    onNavigate: function(e) {
        var me = this,
            selectable = !me.destroyed && me.getSelectable();

        if (selectable && me.shouldSelectItem(e)) {
            selectable.onNavigate(e);
        }
    },

    shouldSelectItem: function (e) {
        var me = this,
            selectable = me.getSelectable(),
            no = e.stopSelection || !selectable || selectable.getDisabled(),
            target = !no && e.getTarget('.' + Ext.baseCSSPrefix + 'item-no-select,.' +
                Ext.baseCSSPrefix + 'item-no-tap', this.element);

        if (target) {
            no = me.el.contains(target);
        }

        return !no;
    },

    // Store events

    onStoreAdd: function () {
        this.syncEmptyState();
    },

    onStoreBeforeLoad: function () {
        this.handleBeforeLoad();
    },

    onStoreClear: function () {
        this.doClear();
    },

    onStoreLoad: function () {
        this.hasLoadedStore = true;
        this.clearMask();
        this.syncEmptyState();
    },

    onStoreRefresh: function () {
        this.refresh();
    },

    onStoreRemove: function () {
        this.syncEmptyState();
    },

    onStoreUpdate: function (store, record, type, modifiedFieldNames, info) {
        var me = this,
            item;

        // Index changing will be handled by the Store's refresh event fired in case of
        // a splice causing an atomic remove+add sequence. See Store#onCollectionAddItems
        if (!info || !(info.indexChanged || info.filtered)) {
            // If, due to filtering or node collapse, the updated record is not
            // represented in the rendered structure, this is a no-op.
            item = me.itemFromRecord(record);

            if (item) {
                me.syncItemRecord({
                    item: item,
                    modified: me.indexModifiedFields(modifiedFieldNames),
                    record: record
                });
            }
        }

        if (me.isSelected(record)) {
            me.setItemSelection(record, true);
        }
    },

    //-------------------------
    // Public Configs

    // associatedData

    updateAssociatedData: function (assocData) {
        this.associatedData = {
            associated: assocData
        };
    },

    // data
    updateData: function (data) {
        var store = this.store;

        if (!store) {
            this.setStore({
                data: data,
                autoDestroy: true
            });
        } else {
            store.loadData(data);
        }
    },

    // disableSelection

    updateDisableSelection: function (value) {
        var el = this.getRenderTarget();

        el.toggleCls(this.showSelectionCls, !value);
    },

    // emptyText

    updateEmptyText: function (emptyText) {
        var me = this,
            config = emptyText,
            emptyTextCmp = me.emptyTextCmp;

        if (emptyTextCmp) {
            if (!emptyText || typeof emptyText === 'string') {
                config = {};
                config[me.emptyTextProperty] = emptyText || '\xA0';
            }

            emptyTextCmp.setConfig(config);
        }
        if (!me.isConfiguring) {
            me.syncEmptyState();
        }
    },
    
    // enableTextSelection

    updateEnableTextSelection: function (enableTextSelection) {
        this.setUserSelectable({ bodyElement: !!enableTextSelection });
    },

    // inline
    updateInline: function (inline) {
        var me = this;

        me.toggleCls(me.inlineCls, !!inline);
        me.toggleCls(me.noWrapCls, inline && inline.wrap === false);
    },

    // itemCls
    updateItemCls: function (newCls, oldCls) {
        if (!this.isConfiguring) {
            var items = this.dataItems,  //TODO confirm - was getFastItems()
                len = items.length,
                i, item;

            for (i = 0; i < len; i++) {
                item = items[i];
                item = item.isWidget ? item.el : Ext.fly(item);

                item.replaceCls(oldCls, newCls);
            }
        }
    },

    // itemTpl
    applyItemTpl: function (config) {
        return Ext.XTemplate.get(config);
    },

    updateItemTpl: function () {
        if (!this.isConfiguring) {
            this.refresh();
        }
    },

    // markDirty

    updateMarkDirty: function (markDirty) {
        var dataItems = this.dataItems,
            i, ln, dataItem;

        markDirty = !!markDirty;
        for (i = 0, ln = dataItems.length; i < ln; i++) {
            dataItem = dataItems[i];
            (dataItem.el || Ext.fly(dataItem)).toggleCls(this.markDirtyCls, markDirty);
        }
    },

    // masked
    updateMasked: function (masked) {
        var me = this,
            loadingHeight = me.getLoadingHeight();

        if (masked) {
            if (loadingHeight && loadingHeight > me.el.getHeight()) {
                me.hasLoadingHeight = true;
                me.oldMinHeight = me.getMinHeight();
                me.setMinHeight(loadingHeight);
            }
        } else {
            if (!me.destroying && me.hasLoadingHeight) {
                me.setMinHeight(me.oldMinHeight);
                delete me.hasLoadingHeight;
            }
        }
    },
    
    // selectable

    applySelectable: function(selectable, oldSelectable) {
        var me = this,
            record = me.selection;

        if (selectable === false) {
            selectable = {
                disabled: true
            };
        }
        if (selectable) {
            if (typeof selectable === 'string') {
                selectable = {
                    type: me.selectionModel,
                    mode: selectable.toLowerCase(),
                    view: me
                };
            } else {
                selectable = Ext.apply({
                    type: me.selectionModel,
                    view: me
                }, selectable);
            }

            // If we already have a Selectable, reconfigure it with incoming values
            if (oldSelectable) {
                //<debug>
                if (selectable.isSelectionModel || selectable.type !== oldSelectable.type) {
                    Ext.raise('Switching out selectables dynamically is not supported');
                }
                //</debug>
                selectable = oldSelectable.setConfig(selectable);
            }
            // Create a Selectable
            else {
                selectable = Ext.Factory.selmodel(me.mergeProxiedConfigs('selectable', selectable));
            }

            // Set the initially configured selection record into the selection model
            if (record) {
                // Only the first time in.
                delete me.selection;

                //<debug>
                if (!record.isEntity) {
                    Ext.raise('DataView selection config must be single record');
                }
                if (selectable.getRecords && !selectable.getRecords()) {
                    Ext.raise('DataView configured with selection when selectable not configured to accept records');
                }
                //</debug>
                selectable.select(record);
            }
        }

        return selectable;
    },

    // store
    applyStore: function (store) {
        return store ? Ext.data.StoreManager.lookup(store) : null;
    },

    updateStore: function (newStore, oldStore) {
        var me = this,
            storeEvents = Ext.apply({scope: me}, me.getStoreEventListeners()),
            mask = me.autoMask,
            newLoad;

        if (oldStore) {
            if (!oldStore.destroyed) {
                if (oldStore.getAutoDestroy()) {
                    oldStore.destroy();
                } else {
                    oldStore.un(storeEvents);
                }
            }

            me.dataRange = me.store = Ext.destroy(me.dataRange);

            // If we are not destroying, refresh is triggered below if there is a newStore
            if (!me.destroying && !me.destroyed && !newStore) {
                me.doClear();
            }
        }

        if (newStore) {
            me.store = newStore;
            if (me.destroying) {
                return;
            }

            newStore.on(storeEvents);
            if (newStore.isLoaded()) {
                me.hasLoadedStore = true;
            }

            // Ignore TreeStore pending loads. They kick off loads while
            // content is still perfecty valid and renderable.
            newLoad = !newStore.isTreeStore && newStore.hasPendingLoad();

            me.bindStore(newStore);

            if (me.initialized) {
                me.refresh();
            }
        }

        // Bind/unbind the selection model if we are rebinding to a new store.
        if (!me.isConfiguring) {
            me.getSelectable().setStore(newStore);
        }

        if (mask && !newLoad) {
            me.setMasked(false);
            me.autoMask = false;
        } else if (!mask && newLoad) {
            me.handleBeforeLoad();
        }
    },

    updateHidden: function (hidden, oldHidden) {
        this.callParent([hidden, oldHidden]);
        this.destroyAllRipples();
    },

    //-----------------------------------------------------------------------

    privates: {
        // This is maintained by updateAssociatedData
        associatedData: true,
        doHover: true,
        showSelectionCls: Ext.baseCSSPrefix + 'show-selection',
        multiSelectCls: Ext.baseCSSPrefix + 'multi-select',
        markDirtyCls: Ext.baseCSSPrefix + 'mark-dirty',

        scrollDockAliases: {
            top: 'start',
            bottom: 'end'
        },

        getSelection: function() {
            // Preserve the Selectable API which offered a getSelection method.
            // The SelectionModel base class uses the "selection" property
            // to store an object which encapsulates a selection of any
            // of several subtypes.
            return this.getSelectable().getSelectedRecord();
        },

        setSelection: function(record) {
            // Preserve the Selectable API which offered a setSelection method.
            // The SelectionModel base class uses the "selection" property
            // to store an object which encapsulates a selection of any
            // of several subtypes.
            return this.getSelectable().setSelectedRecord(record);
        },

        generateSelectorFunctions: function() {
            var renderTarget = this.getRenderTarget(),
                bodyElement = this.bodyElement;

            // eventDelegate is used solely by the view event listener to filter the event reactions
            // to the level of granularity needed. At the DataView level, this means item elements.
            // At the Grid level, this will be cell elements.
            //
            // itemSelector is used by the Navigation and Location classes to find a dataview item from
            // a passed element.
            // They are identical at this level
            this.eventDelegate = this.itemSelector = function (candidate) {
                return candidate && (
                        candidate.parentNode === bodyElement.dom ||
                        candidate.parentNode === renderTarget.dom
                    );
            };
        },

        bindStore: function (store) {
            this.dataRange = store.createActiveRange();
        },

        clearMask: function() {
            this.setMasked(false);
            this.autoMask = false;
        },

        clearPressedCls: function(type, location) {
            var me = this,
                record = location.record,
                child = location.child,
                el;

            me.clearPressedTimer();

            if (record && child) {
                el = child.isWidget ? child.element : Ext.fly(child);
                el.removeCls(me.pressedCls);
            }

            me.fireChildEvent(type, location);
        },

        clearPressedTimer: function() {
            var timeout = this.pressedTimeout;

            if (timeout) {
                Ext.undefer(timeout);
                delete this.pressedTimeout;
            }
        },

        doAddPressedCls: function(record) {
            var me = this,
                item = me.itemFromRecord(record);

            if (item) {
                item = item.isWidget ? item.element : Ext.fly(item);
                item.addCls(me.pressedCls);
            }
        },

        doClear: function() {
            this.syncEmptyState();
        },

        doChildTouchStart: function(location) {
            var me = this,
                record = location.record,
                itemButtonMode = me.getItemButtonMode(),
                pressedDelay = me.getPressedDelay();

            me.clearPressedTimer();

            if (record) {
                if (pressedDelay > 0) {
                    me.pressedTimeout = Ext.defer(me.doAddPressedCls, pressedDelay,
                        me, [record]);
                } else {
                    me.doAddPressedCls(record);
                }

                if (itemButtonMode) {
                    me.lastPressedLocation = location;
                    Ext.GlobalEvents.setPressedComponent(me, location);
                }
            }
        },

        /**
         * Called by {@link Ext.GlobalEvents#setPressedComponent} when the global
         * mouseup event fires and there's a registered pressed component.
         * @private
         */
        onRelease: function() {
            var me = this;

            if (me.lastPressedLocation) {
                me.clearPressedCls('release', me.lastPressedLocation);
            }
            me.lastPressedLocation = null;
        },

        /**
         * This method builds up a plan object with flags and a pop-off "steps" array of
         * method names to be called in order to fullfil the passed options of an ensureVisible call.
         *
         * @param {Number/Ext.data.Model} [record] The record or the 0-based position
         * to which to scroll. If this parameter is not passed, the `options` argument must
         * be passed and contain either `record` or `recordIndex`.
         *
         * @param {Object} [plan] An object containing options to modify the operation.
         *
         * @param {Boolean} [plan.animation] Pass `true` to animate the row into view.
         *
         * @param {Boolean} [plan.focus] Pass as `true` to focus the specified row.
         *
         * @param {Boolean} [plan.highlight] Pass `true` to highlight the row with a glow
         * animation when it is in view.
         *
         * @param {Ext.data.Model} [plan.record] The record to which to scroll.
         *
         * @param {Number} [plan.recordIndex] The 0-based position to which to scroll.
         *
         * @param {Boolean} [plan.select] Pass as `true` to select the specified row.
         * @private
         */
        ensureVisiblePlan: function (record, plan) {
            var store = this.store,
                recIndex;

            // record was passed as an options object
            if (record.record) {
                plan = Ext.apply({}, record);
                record = plan.record;
                delete plan.record;
            } else {
                plan = Ext.apply({}, plan);
            }

            if (record.isEntity) {
                recIndex = store.indexOf(record);
            }
            else if (typeof record === 'number') {
                recIndex = record;
                record = store.getAt(record);
            }
            //<debug>
            else {
                Ext.raise('ensureVisible first parameter must be record or recordIndex ' +
                          'or an options object with a record property');
            }
            //</debug>

            plan.record = record;
            plan.recordIndex = recIndex;

            plan.animation = plan.animation || plan.animate; // classic compat
            plan.async = !!plan.animation;
            plan.steps = [];

            // In an infinite list we can have a record w/no item but it then must
            // exist in the store...
            if (recIndex < 0 || recIndex >= store.getCount()) {
                //<debug>
                Ext.raise('Invalid record passed to List#ensureVisible');
                //</debug>

                plan.promise = Ext.Deferred.getCachedRejected();
            }
            else {
                // These will be pop()ed and dispatched so they are in LIFO order
                // here:
                plan.steps.push(
                    'ensureVisibleFocus',
                    'ensureVisibleSelect',
                    'ensureVisiblePrep'
                );
            }

            return plan;
        },

        ensureVisibleFocus: function (plan) {
            if (plan.focus) {
                var item = plan.item;

                if (plan.async) {
                    plan.promise = plan.promise.then(function (o) {
                        item = o.item;

                        if (item) {
                            item.focus();
                        }

                        return o;
                    });
                }
                else if (item) {
                    item.focus();
                }
            }
        },

        ensureVisiblePrep: function (plan) {
            var me = this,
                dataRange = me.dataRange,
                cleanup = function () {
                    delete dataRange.goto;

                    if (args) {
                        dataRange.goto(args[0], args[1]);
                    }
                },
                args, promise;

            if (plan.async) {
                // We do *not* want the spray goto() calls all down the virtual store
                // as we animate, so replace the method and capture the most current
                // call arguments...
                dataRange.goto = function (begin, end) {
                    if (args) {
                        args[0] = begin;
                        args[1] = end;
                    }
                    else {
                        args = [begin, end];
                    }
                };

                promise = me.ensureVisibleScroll(plan);

                // Once the scroll is done, we can allow the last goto() call through.
                // This method is called to add the range unlock at the proper point
                // in the promise chain.
                promise = promise.then(function (v) {
                    cleanup();
                    return v;
                }, function (ex) {
                    cleanup();
                    throw ex;
                });
            }
            else {
                promise = me.ensureVisibleScroll(plan);
            }

            plan.promise = promise;
        },

        ensureVisibleScroll: function(plan) {
            var item = plan.item || (plan.item = this.itemFromRecord(plan.recIndex));

            return this.getScrollable().ensureVisbile(item.el, {
                animation: plan.animation
            });
        },

        ensureVisibleSelect: function (plan) {
            if (plan.select) {
                var me = this;

                if (plan.async) {
                    plan.promise = plan.promise.then(function (o) {
                        //TODO select rec
                        //TODO we may need to wait for it unless we can select by index
                        //TODO if (o.record) ...
                        //TODO if (o.recordIndex) ...
                        return o;
                    });
                }
                else {
                    //TODO select opts.record or opts.recordIndex
                }
            }
        },

        filterScrollDockStart: function (item) {
            var scrollDock = item.scrollDock;

            return scrollDock === 'start' || scrollDock === 'top';
        },

        filterScrollDockEnd: function (item) {
            var scrollDock = item.scrollDock;

            return scrollDock === 'end' || scrollDock === 'bottom';
        },

        findTailItem: function (rawElements) {
            var me = this,
                items = rawElements ? me.innerItems : me.items.items,
                at = -1,
                tail = null,
                i, item, scrollDock;

            for (i = items.length; i-- > 0; ) {
                item = items[i];
                scrollDock = item.scrollDock;

                if (scrollDock === 'end') {
                    tail = items[at = i];
                }
                else {
                    break;
                }
            }

            return rawElements ? tail : at;
        },

        fireChildEvent: function(type, location) {
            var me = this,
                deprecatedName = 'item' + type,
                name = 'child' + type,
                hasListeners = me.hasListeners;

            if (hasListeners[name]) {
                me.fireEvent(name, me, location);
            }

            // Deprecated style only fire for things backed by records.
            if (hasListeners[deprecatedName] && location.record) {
                me.fireEvent(deprecatedName, me, location.viewIndex, location.item, 
                             location.record, location.event);
            }
        },

        getEmptyTextCmp: function () {
            var me = this,
                cmp = me.emptyTextCmp;

            if (!cmp) {
                me.emptyTextCmp = cmp = me.add(me.createEmptyText(me.getEmptyText()));
            }

            return cmp;
        },

        getRecordIndexFromPoint: function (x, y) {
            var item = this.getItemFromPoint(x, y);

            return item ? this.mapToRecordIndex(item) : -1;
        },

        getItemFromPoint: function (x, y) {
            var me = this,
                scroller = me.getScrollable(),
                scrollPosition = scroller.getPosition(),
                scrollSize = scroller.getSize(),
                offset = me.getScrollerTarget().getXY();

            return me.getItemFromPagePoint(
                Math.max(Math.min(x, scrollSize.x), 0) + offset[0] - scrollPosition.x,
                Math.max(Math.min(y, scrollSize.y), 0) + offset[1] - scrollPosition.y
            );
        },

        getItemFromPagePoint: function(x, y) {
            var items = this.getFastItems(),
                len = items.length,
                point = new Ext.util.Point(x, y),
                ret = null,
                i, item, el;

            for (i = 0; i < len; i++) {
                item = items[i];
                el = item.isWidget ? item.element : Ext.fly(item);
                if (el.getRegion().contains(point)) {
                    ret = item;
                    break;
                }
            }

            return ret;
        },

        handleBeforeLoad: function() {
            var me = this,
                loadingText = me.getLoadingText();

            if (loadingText) {
                me.autoMask = true;
                me.setMasked({
                    xtype: 'loadmask',
                    message: loadingText
                });
            }

            me.hideEmptyText();
        },

        hideEmptyText: function() {
            var cmp = this.emptyTextCmp;
            if (cmp) {
                cmp.hide();
            }
        },

        /**
         * This method is called to convert the modified field names array received from
         * the `store` when records are modified. Grids want to convert that array into an
         * object keyed by modified name for efficient decisions about which cells need to
         * be refreshed.
         *
         * @param {String[]} modified
         * @return {String[]/Object}
         * @template
         * @private
         * @since 6.5.1
         */
        indexModifiedFields: function (modified) {
            return modified;
        },

        /**
         * @param {Ext.dom.Element/Ext.Component} item The item from which to navigate.
         *
         * @param {"cmp"/"dom"/"el"} as Pass `"dom"` to always return an `HTMLElement` for
         * the item. For component dataviews this is the component's main element. Pass `"el"`
         * to return the `Ext.dom.Element` form of the item. For component dataviews this will
         * be the component's main element. For other dataviews the returned instance is
         * produced by {@link Ext#fly Ext.fly()} and should not be retained. Pass `"cmp"` to
         * return the `Ext.Component` reference for the item (if one exists).
         *
         * @return {Number/HTMLElement/Ext.dom.Element/Ext.Component}
         * @private
         */
        itemAs: function (item, as) {
            var ret = item;

            //<debug>
            if (as !== 'cmp' && as !== 'dom' && as !== 'el') {
                Ext.raise('Invalid "as" value "' + as + '" to mapToItem()');
            }
            //</debug>

            if (typeof ret === 'number') {
                // traversal can hit edge conditions in infinite lists...
                ret = null;
            }
            else if (ret) {
                if (as === 'cmp') {
                    if (!ret.isWidget) {
                        ret = Ext.getCmp(ret.id);
                    }
                }
                else {
                    if (ret.isWidget) {
                        ret = ret.el; // we're digging down at least this far...
                    }

                    if (ret) {
                        if (ret.isElement) {
                            if (as === 'dom') {
                                ret = ret.dom;
                            }
                        }
                        else if (as === 'el') {
                            ret = Ext.fly(ret);
                        }
                    }
                }
            }

            return ret;
        },

        itemFromRecord: function (rec) {
            var index = rec.isEntity ? this.store.indexOf(rec) : rec;

            // Only valid if the store contains the record
            return ((index > -1) && this.dataItems[index]) || null;
        },

        onContainerTrigger: function(e) {
            var me = this;

            if (e.target === me.element.dom) {
                if (me.getDeselectOnContainerClick() && me.store) {
                    me.getSelectable().deselectAll();
                }
            }
        },

        runRefresh: function() {
            var me = this,
                store = me.store;

            me.syncEmptyState();

            // Ignore TreeStore loading state. They kick off loads while
            // content is still perfecty valid and renderable.
            if (store && !me.isConfiguring && (store.isTreeStore || !store.hasPendingLoad())) {
                me.fireEventedAction('refresh', [me], 'doRefresh', me, [me.getScrollToTopOnRefresh()]);
            }
        },

        /**
         * @private
         * Called prior to an operation which mey remove focus from this view by some kind of DOM operation.
         *
         * If this view contains focus, this method returns a function which, when called after
         * the disruptive DOM operation will restore focus to the same record, or, if the record has
         * been removed to the same item index..
         *
         * @returns {Function} A function that will restore focus if focus was within this view,
         * or a function which does nothing is focus is not in this view.
         */
        saveFocusState: function() {
            var me = this,
                navModel = me.getNavigationModel(),
                location = navModel.location,
                lastFocusedViewIndex, lastFocusedRecord, itemCount, focusItem;

            // If there is a position to restore...
            if (location) {
                lastFocusedRecord = location.record;
                lastFocusedViewIndex = location.viewIndex;

                // The following function will attempt to refocus back to the same viewIndex if
                // it is still there
                return function() {
                    itemCount = me.getFastItems().length;

                    // If we still have data, attempt to refocus at the same record, or the same
                    // viewIndex.
                    if (itemCount) {
                        // Adjust expectations of where we are able to refocus according to what
                        // kind of destruction might have been wrought on this view's DOM since
                        // focus save.
                        if (lastFocusedRecord) {
                            focusItem = me.mapToItem(lastFocusedRecord);
                        }

                        if (!focusItem) {
                            focusItem = me.mapToItem(Math.min(lastFocusedViewIndex || 0, itemCount - 1));
                        }

                        navModel.setLocation(null);
                        navModel.setLocation(focusItem);
                    }
                };
            }
            return Ext.emptyFn;
        },

        setItemHidden: function (item, hide) {
            if (hide) {
                if (!item.$hidden) {
                    item.hide();
                    item.$hidden = true;
                }
            }
            else if (item.$hidden) {
                item.$hidden = false;
                item.show();
            }
        },

        setItemSelection: function(records, selected) {
            // Ensure it's an array.
            records = Ext.Array.from(records);

            var me = this,
                len = records.length,
                pressedCls = me.pressedCls,
                selectedCls = me.selectedCls,
                toRemove = pressedCls,
                i, record, item, toAdd;

            if (!selected) {
                toRemove = [pressedCls, selectedCls];
            } else {
                toAdd = selectedCls;
            }

            if (!me.isConfiguring && !me.destroyed) {
                for (i = 0; i < len; i++) {
                    record = records[i];
                    item = me.itemFromRecord(record);

                    if (item) {
                        item = item.isWidget ? item.element : Ext.fly(item);
                        item.removeCls(toRemove);
                        if (toAdd) {
                            item.addCls(toAdd);
                        }
                    }
                }
            }
        },

        shouldRippleItem: function (item, e) {
            var disableSelection = this.getDisableSelection();
            if (!disableSelection && this.isItemSelected(item)) {
                return false;
            }

            return this.mixins.itemrippler.shouldRippleItem.call(this, item, e);
        },

        syncEmptyState: function() {
            var me = this,
                store = me.store,
                empty = !store || !store.getCount() && me.getEmptyText(),
                emptyTextCmp = me.emptyTextCmp;

            if (!empty) {
                if (emptyTextCmp) {
                    emptyTextCmp.hide();
                }
            }
            else if ((me.hasLoadedStore || !me.getDeferEmptyText()) && !(store && store.hasPendingLoad())) {
                emptyTextCmp = emptyTextCmp || me.getEmptyTextCmp();
                emptyTextCmp.show();
            }

            me.setEmptyState(empty);

            return empty;
        },

        toggleChildrenTabbability: function(enableTabbing) {
            var focusEl = this.getRenderTarget();

            if (enableTabbing) {
                focusEl.restoreTabbableState({
                    skipSelf: true
                });
            } else {
                // Do NOT includeSaved
                // Once an item has had tabbability saved, do not increment its save level
                focusEl.saveTabbableState({
                    skipSelf: true,
                    includeSaved: false
                });
            }
        },

        toggleHoverCls: function(on) {
            var target = this.mouseOverItem,
                el;

            if (target) {
                el = target.isWidget ? target.element : Ext.fly(target);
                el.toggleCls(this.hoveredCls, on);
            }
        },

        _onChildEvent: function(fn, e) {
            var me = this,
                last = me.lastPressedLocation,
                location = me.getNavigationModel().createLocation(e);

            if (location.child) {
                location.pressing = !!(last && last.child === location.child);
                me[fn](location);
            }

            return location;
        },

        _onChildTouchStart: function(e) {
            var child = this._onChildEvent('onChildTouchStart', e).child,
                el = child && (child.element || Ext.get(child));

            if (el) {
                el.on('touchmove', '_onChildTouchMove', this);
            }
        },

        _onChildTouchMove: function(e) {
            this._onChildEvent('onChildTouchMove', e);
        },

        _onChildTouchEnd: function(e) {
            var child = this._onChildEvent('onChildTouchEnd', e).child,
                el = child && (child.element || Ext.get(child));

            if (el) {
                el.un('touchmove', '_onChildTouchMove', this);
            }
        },

        _onChildTouchCancel: function(e) {
            var child = this._onChildEvent('onChildTouchCancel', e).child,
                el = child && (child.element || Ext.get(child));

            if (el) {
                el.un('touchmove', '_onChildTouchMove', this);
            }
        },

        _onChildTap: function(e) {
            var target = e.getTarget('.' + Ext.baseCSSPrefix + 'item-no-tap', this.element);

            if (!target) {
                this._onChildEvent('onChildTap', e);
            }
        },

        _onChildTapCancel: function(e) {
            this._onChildEvent('onChildTapCancel', e);
        },

        _onChildContextMenu: function(e) {
            this._onChildEvent('onChildContextMenu', e);
        },

        _onChildLongPress: function(e) {
            this._onChildEvent('onChildLongPress', e);
        },

        _onChildTapHold: function(e) {
            this._onChildEvent('onChildTapHold', e);
        },

        _onChildSingleTap: function(e) {
            this._onChildEvent('onChildSingleTap', e);
        },

        _onChildDoubleTap: function(e) {
            this._onChildEvent('onChildDoubleTap', e);
        },

        _onChildSwipe: function(e) {
            this._onChildEvent('onChildSwipe', e);
        },

        _onChildMouseOver: function(e) {
            var fromItem = e.getRelatedTarget(this.itemSelector),
                toItem = e.getTarget(this.itemSelector);

            if (toItem !== fromItem) {
                this._onChildEvent('onChildMouseOver', e);
            }
        },

        _onChildMouseOut: function(e) {
            var toItem = e.getRelatedTarget(this.itemSelector),
                fromItem = e.getTarget(this.itemSelector);

            if (toItem !== fromItem || !e.getRelatedTarget(this.eventDelegate)) {
                this._onChildEvent('onChildMouseOut', e);
            }
        },

        _onContainerTouchStart: function(e) {
            // If it's not a click within an item, then it's a click on the scrollbar
            if (!e.getTarget(this.itemSelector)) {
                e.preventDefault();

                if (!this.bodyElement.getClientRegion().contains(e.getPoint())) {
                    this.getNavigationModel().lastLocation = 'scrollbar';
                }
            }
        },

        setupChildEvent: Ext.privateFn,

        //-------------------------
        // Private Configs

        // emptyState

        updateEmptyState: function (empty) {
            var me = this,
                items = me.items.items,
                showInEmptyState, hide, i, item, show;

            for (i = 0; i < items.length; ++i) {
                item = items[i];
                showInEmptyState = item.showInEmptyState;
                hide = show = false;

                if (showInEmptyState === false) {
                    // Bound the emptyState of false, which means show when !empty
                    hide = !(show = !empty);
                }
                else if (showInEmptyState) {
                    if (typeof showInEmptyState === 'function') {
                        hide = !(show = item.showInEmptyState(empty));
                        if (show == null) {
                            continue;
                        }
                    }
                    else {
                        hide = !(show = empty);
                    }
                }

                if (hide) {
                    if (item.isInner) {
                        me.setItemHidden(item, true);
                    }
                    else {
                        item.hide();
                    }
                }
                else if (show) {
                    if (item.isInner) {
                        me.setItemHidden(item, false);
                    }
                    else {
                        item.show();
                    }
                }
            }
        },

        // navigationModel
        applyNavigationModel: function(navigationModel) {
            if (navigationModel) {
                if (typeof navigationModel === 'string') {
                    navigationModel = {
                        type: navigationModel
                    };
                }

                navigationModel = Ext.Factory.navmodel(Ext.apply({
                    view: this
                }, navigationModel));
            }
            return navigationModel;
        },

        updateNavigationModel: function(navigationModel, oldNavigationModel) {
            Ext.destroy(oldNavigationModel);
        },

        getUseComponents: function () {
            return this.isComponentDataView;  // for backwards compat
        }
    } // privates
});
