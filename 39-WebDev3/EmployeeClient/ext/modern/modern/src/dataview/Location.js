/**
 * Instances of this class encapsulate a focusable item in a DataView.
 *
 * DataView addresses are configured using the owning {@link #property!item}.
 *
 * Be careful not to make `Location` objects persistent. If the associated record
 * is removed or filtered out, or is scrolled out of the rendered range in an infinite
 * list, the reference will be stale. Its referenced item may be used by a different record.
 *
 * Freshly created Location objects, such as those exposed by events from the
 * {@link Ext.dataview.selection.Model selection model} are safe to use until your
 * application mutates the store, changes the column set (for grids), or scrolls
 * the referenced item out of the rendered block (for infinite lists).
 *
 * @since 6.5.0
 */
Ext.define('Ext.dataview.Location', {
    /**
     * @property {Boolean} isDataViewLocation
     * @readonly
     * `true` in this class to identify an object as an instantiated dataview Location, or subclass thereof.
     */
    isDataViewLocation: true,

    /**
     * @property {Ext.Component/Ext.dom.Element} child
     * @readonly
     * The child in the view.
     */
    child: null,

    /**
     * @property {Ext.event.Event} [event]
     * @readonly
     * The event that led to the creation of this event. This may be null.
     */
    event: null,

    /**
     * @property {Ext.Component/Ext.dom.Element} item
     * @readonly
     * The item in the view, if it backed by a record.
     */
    item: null,

    /**
     * @property {Ext.data.Model} [record]
     * @readonly
     * The record.
     */
    record: null,

    /**
     * @property {Number} [recordIndex]
     * @readonly
     * The record index.
     */
    recordIndex: -1,

    /**
     * @private
     */
    sourceElement: null,

    /**
     * @property {Ext.dataview.Abstract} view
     * @readonly
     * The view.
     */
    view: null,

    /**
     * @property {Number} viewIndex
     * @readonly
     * The index of the {@link #child} in the view.
     */
    viewIndex: -1,

    /**
     * Create a new Location
     * @param {Ext.dataview.Abstract} view The view.
     * @param {Object} source The source for the location. It can be:
     *
     * - `Ext.event.Event` - An event from the view.
     * - `Ext.dom.Element/HTMLElement` - An element from the view.
     * - `Ext.Widget` - A child component from the view.
     * - `Ext.data.Model` - A record from the view.
     * - `Number` - The record index.
     */
    constructor: function(view, source) {
        this.view = view;

        if (source != null) {
            this.attach(source);
        }
    },

    attach: function(source) {
        var me = this,
            view = me.view,
            store = view.store,
            record, child, sourceElement;

        //<debug>
        if (me.source) {
            Ext.raise('DataView Locations cannot be modified');
        }
        //</debug>
        if (source.isEvent) {
            me.event = source;
            sourceElement = source.target;
        }
        if (source.isElement || source.nodeType === 1) {
            sourceElement = source;
        }

        me.source = source;
        if (source.isWidget) {
            sourceElement = source.getFocusEl();
            source = source.element;
        }

        if (typeof source === 'number') {
            child = view.itemFromRecord(source);
            me.recordIndex = source;

            // If the view is not yet bound to a store, we cannot find the record
            record = store && store.getAt(source);
        } else {
            if (source.isModel) {
                record = source;
            } else {
                record = view.mapToRecord(source);
            }
            child = view.mapToItem(source);

            // If the view is not yet bound to a store, we cannot find the record
            me.recordIndex = store ? store.indexOf(record) : -1;
        }

        if (child && !sourceElement) {
            sourceElement = child.isWidget ? child.getFocusEl() : child;
        }

        me.child = child;
        me.record = record;

        // Item property only present if the child firing the event is a record-based item.
        if (record && child) {
            me.item = child;
        }

        if (child) {
            me.viewIndex = view.mapToViewIndex(child);
        }

        me.sourceElement = Ext.getDom(sourceElement);
    },

    /**
     * Creates a clone of this Location.
     * @return {Ext.dataview.Location} A clone of this Location.
     */
    clone: function() {
        var me = this,
            ret = new this.self(me.view);

        ret.event = me.event;
        ret.sourceElement = me.sourceElement;
        ret.item = me.item;
        ret.record = me.record;
        ret.recordIndex = me.recordIndex;
        ret.viewIndex = me.viewIndex;

        return ret;
    },

    equals: function(other) {
        // There will never be unrendered locations at this level.
        // Location will always be able to resolve a sourceElement, so
        // Locations are only equal if their sourceElements are equal.
        return other &&
               other.view === this.view &&
               other.isDataViewLocation &&
               other.sourceElement === this.sourceElement;
    },

    /**
     * Returns the location's referenced `focusEl` *at the time of calling*.
     *
     * @param {"dom"/"el"} [as=el] Pass `"dom"` to always return the item's `HTMLElement`.
     * Pass `"el"` to return the item's `Ext.dom.Element`.
     * @return {HTMLElement/Ext.dom.Element} The item focusable *element* referenced by this location.
     */
    getFocusEl: function(as) {
        var item = this.get(),
            ret = null;

        if (item && item.isWidget) {
            item = item.element;
        }

        if (item) {
            ret = (as === 'dom' || as === true) ? Ext.getDom(item) : Ext.get(item);
        }

        return ret;
    },

    /**
     * Returns the focusable element/component which this location represents.
     *
     * An {@link Ext.dataview.Location} will return the view's item element (or Component if an
     * {@link Ext.dataview.Component Component Dataview} or {@link Ext.dataview.List List) is used).
     *
     * An {@link Ext.grid.Location} will return the {@link Ext.grid.cell.Cell cell} component or
     * an focusable element if navigation has moved with a cell, or into non-record view items.
     * @returns {Ext.Element|Ext.Component}
     *
     * Contrast with {@link #getItem} which slways returns the DataView item, even in a Grid.
     */
    get: function() {
        return this.child;
    },

    isFirstDataItem: function() {
        return this.recordIndex === 0;
    },

    isFirstViewItem: function() {
        var view = this.view;

        if (view.infinite) {
            return view.previous(this.child == null);
        }

        return this.viewIndex === 0;
    },

    isLastDataItem: function() {
        return this.recordIndex === this.view.store.getCount() - 1;
    },

    isLastViewItem: function() {
        var view = this.view;

        if (view.infinite) {
            return view.next(this.child == null);
        }

        return this.viewIndex === view.innerItems.length - 1;
    },

    /**
     * Re-orientates this Location according to the existing settings. If for example
     * a row has been deleted, or moved by a sort, this will
     * resync internal values with reality.
     */
    refresh: function() {
        var me = this,
            view = me.view,
            item = me.child,

            // The most important anchor is the record. Try to access its corresponding
            // item first. Failing that, try our item directly, and if that has gone,
            // fall back to our recorded viewIndex.
            newSource = view.mapToItem(me.record) ||
                (view.items.contains(item) ? item
                    : view.mapToItem(Math.min(me.viewIndex, view.dataItems.length - 1)));

        return new this.self(view, newSource);
    },

    isFirst: function() {
        return this.view.isFirstItem(this.child);
    },

    isLast: function() {
        return this.view.isLastItem(this.child);
    },

    /**
     * Navigates to the next navigable Location.
     * @param {Boolean/Object} [options] An options object or a boolean flag meaning wrap
     * @param {Boolean} [options.wrap] `true` to wrap from the last to the first Location.
     * @param {Number} [options.column] The column to move to if not the current column.
     * @returns {Ext.dataview.Location} A *new* Location object representing the new location.
     */
    next: function(options) {
        var me = this,
            candidate = me.nextItem(options),
            item = candidate && candidate.get();

        while (candidate && (!item || !item.el.isFocusable())) {
            // Wrapped round. Give up.
            if (candidate.equals(me)) {
                return me;
            }

            candidate = candidate.nextItem(options);
            item = candidate && candidate.get();
        }

        return candidate || me;
    },

    /**
     * Navigates to the previous visible Location.
     * @param {Boolean/Object} [options] An options object or a boolean flag meaning wrap
     * @param {Boolean} [options.wrap] `true` to wrap from the first to the last Location.
     * @param {Number} [options.column] The column to move to if not the current column.
     * @returns {Ext.dataview.Location} A *new* Location object representing the new location.
     */
    previous: function(options) {
        var me = this,
            candidate = me.previousItem(options),
            item = candidate && candidate.get();

        while (candidate && (!item || !item.el.isFocusable())) {
            // Wrapped round. Give up.
            if (candidate.equals(me)) {
                return me;
            }

            candidate = candidate.previousItem(options);
            item = candidate && candidate.get();
        }

        return candidate || me;
    },

    /**
     * Returns a new Location object encapsulating the next item in the DataView.
     * @param {Boolean/Object} [options] An options object or a boolean flag meaning wrap
     * @param {Boolean} [options.wrap] `true` to wrap from the last to the first Location.
     * @returns {Ext.dataview.Location} A *new* Location object representing the new location.
     */
    nextItem: function(options) {
        var view = this.view,
            item = this.child,
            wrap = (typeof options === 'boolean') ? options : !!(options && options.wrap),
            nextItem;

        if (view.isLastItem(item)) {
            if (wrap) {
                nextItem = view.getFirstItem();
            } else {
                return null;
            }
        } else {
            nextItem = view.nextItem(item);
        }

        return new this.self(view, nextItem);
    },

    /**
     * Returns a new Location object encapsulating the previous item in the DataView.
     * @param {Boolean/Object} [options] An options object or a boolean flag meaning wrap
     * @param {Boolean} [options.wrap] `true` to wrap from the first to the last Location.
     * @returns {Ext.dataview.Location} A *new* Location object representing the new location.
     */
    previousItem: function(options) {
        var view = this.view,
            item = this.child,
            wrap = (typeof options === 'boolean') ? options : !!(options && options.wrap),
            prevItem;

        if (view.isFirstItem(item)) {
            if (wrap) {
                prevItem = view.getLastItem();
            } else {
                return null;
            }
        } else {
            prevItem = view.previousItem(item);
        }

        return new this.self(view, prevItem);
    }
});
