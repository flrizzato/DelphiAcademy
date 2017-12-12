/**
 * Instances of this class encapsulate a position in a grid's row/column coordinate system.
 *
 * Cell addresses are configured using the owning {@link #property!record} and {@link #property!column}
 * for robustness.
 *
 * The column may be moved, the store may be sorted, and the grid Location will still reference
 * the same *logical* cell. Be aware that due to buffered rendering the *physical* cell may
 * be recycled and used by another record.
 *
 * Be careful not to make `grid Location` objects *too* persistent. If the owning record
 * is removed or filtered out, or the owning column is removed, the reference will be stale.
 *
 * Note that due to buffered rendering, a valid `location` object might be scrolled out of
 * visibility, and therefore derendered, and may not not have a corresponding rendered
 * row.
 *
 * Also, when using {@link Ext.data.virtual.Store virtual stores}, the record referenced may
 * not be present in the store, and may require an asynchronous load to bring it into the
 * store before the location can be realized.
 *
 * Freshly created Location objects, such as those exposed by events from the
 * {@link Ext.grid.Grid#cfg!selectable grid selection model} are safe to use until your
 * application mutates the store, or changes the column set.
 *
 * @since 6.5.0
 */
Ext.define('Ext.grid.Location', {
    extend: 'Ext.dataview.Location',

    /**
     * @property {Boolean} isGridLocation
     * @readonly
     * `true` in this class to identify an object as an instantiated grid Location, or subclass thereof.
     */
    isGridLocation: true,

    /**
     * @property {Boolean} actionable
     * `true` if this is an actionable location.
     */
    actionable: false,

    /**
     * @property {Ext.grid.cell.Base} [cell]
     * The cell.
     */
    cell: null,

    /**
     * @property {Ext.grid.column.Column} [column]
     * The column.
     */
    column: null,

    /**
     * @property {Number} [columnIndex]
     * The visible column index.
     */
    columnIndex: -1,

    /**
     * @property {Boolean} summary
     * `true` if this is a {@link Ext.grid.SummaryRow summary row}.
     */
    summary: false,

    /**
     * @property {Ext.grid.Row} [row]
     * The row.
     */
    row: null,

    /**
     * @property {Ext.grid.RowBody} [rowBody]
     * The row body.
     */
    rowBody: null,

    /**
     * @property {Boolean} [isTreeLocation]
     * `true` if this is a {@link Ext.grid.cell.Tree tree cell} location.
     */
    isTreeLocation: false,

    inheritableStatics: {
        /**
         * @private
         */
        defineProtoProperty: function(propName, getterName) {
            Object.defineProperty(this.prototype, propName, {
                get: function () {
                    // This refers to the class instance
                    var v = this[getterName]();
                    // Mask the proto defineProperty with the instance value
                    Object.defineProperty(this, propName, {
                        value: v,
                        configurable: true
                    });

                    return v;
                }
            });
        }
    },

    /**
     * @method constructor
     *
     * Create a new Location
     * @param {Ext.grid.Grid} view The view.
     * @param {Object} source The source for the location. It can be:
     *
     * - `Ext.event.Event` - An event from the view.
     * - `Ext.dom.Element/HTMLElement` - An element from the view.
     * - `Ext.Widget` - A child component from the view.
     * - `Ext.data.Model` - A record from the view.
     * - `Number` - The record index.
     * - `Object` - An object with 2 properties, the `record` and `column`.
     */

    attach: function(source) {
        var me = this,
            view = me.view,
            store = view.store,
            item, cell, column, columns, sourceRec, sourceCol, first;

        if (source.constructor === Object) {
            sourceRec = source.record;
            if (typeof sourceRec === 'number') {
                sourceRec = store.getAt(Math.max(Math.min(sourceRec, store.getCount() - 1), 0));
            }
            sourceCol = source.column;
            if (typeof sourceCol === 'number') {
                columns = view.getVisibleColumns();
                sourceCol = columns[Math.max(Math.min(sourceCol, columns.length - 1), 0)];
            }

            if (!(sourceRec && sourceCol)) {
                if (sourceRec) {
                    sourceCol = view.getFirstVisibleColumn();
                } else {
                    sourceRec = store.getAt(0);
                }
            }
            cell = view.mapToCell(sourceRec, sourceCol);
            if (cell) {
                source = cell.element;
            }
            else {
                me._setColumn(sourceCol);
                source = sourceRec;
            }
        }

        me.callParent([source]);

        item = me.item;

        if (item && item.isGridRow) {
            me.row = item;
            me.summary = item.isSummaryRow;
            if (!cell) {
                cell = view.mapToCell(source);
                if (!cell) {
                    columns = view.getVisibleColumns();
                    first = columns[0];
                    if (first) {
                        cell = item.getCellByColumn(first);
                    }
                }
            }
            me.cell = cell;
            if (cell) {
                me.column = column = cell.getColumn();
                columns = columns || view.getVisibleColumns();
                me.columnIndex = columns.indexOf(column);
                me.isTreeLocation = !!cell.isTreeCell;
            } else {
                me.rowBody = view.mapToRowBody(source);
            }
        }
    },

    /**
     * Creates a clone of this Location, optionally moving either the record or column
     * to a different position.
     * @param {Object} [options] The different `record` or `column` to attach the clone to.
     * @param {Ext.data.Model/Number/Ext.dom.Element} [options.record] The different `record` or DataView item to attach the clone to.
     * @param {Ext.grid.column.Column/Number} [options.column] The different `column` to attach the clone to.
     * @return {Ext.grid.Location} The clone, optionally repositioned using the options parameter.
     */
    clone: function(options) {
        var me = this,
            ret = me.callParent(),
            record, column, cell;

        if (options) {
            if (options.record !== undefined) {
                record = options.record;
            }
            if (options.column !== undefined) {
                column = options.column;
            }
            delete ret.sourceElement;
        }

        if (record) {
            delete me.source;
            me.superclass.attach.call(ret, record);
            ret.row = ret.item;
        } else {
            ret.row = ret.child = me.row;
            ret.summary = me.summary;
            ret.rowBody = me.rowBody;
        }

        if (column) {
            ret._setColumn(column);
        } else {
            ret.cell = cell = me.cell;
            ret.column = me.column;
            ret.columnIndex = me.columnIndex;
            me.isTreeLocation = !!(cell && cell.isTreeCell);
        }

        return ret;
    },

    cloneForColumn: function(column) {
        return this.clone({
            column: column
        });
    },

    /**
     * Compares this grid Location object to another grid Location to see if they refer to the same cell
     * *and*, if actionable, the same element in the same cell. So an actionable location focused upon
     * a {@link Ext.Tool} inside a cell will not be equal to the raw Location of that cell.
     * @param {Ext.grid.Location} other The grid Location to compare.
     * @return {Boolean} `true` if the other cell Location references the same cell in the same
     * action/navigation mode as this.
     */
    equals: function(other) {
        var me = this;

        if (other && other.view === me.view && other.isGridLocation) {
            // Actionable state must match.
            if (me.actionable !== other.actionable) {
                return false;
            }

            // Only actionable locations depend on the source element for equivalence
            if (me.sourceElement && me.actionable) {
                return other.sourceElement === me.sourceElement;
            }
            // We only get here if this location refers to an unrendered location,
            // or we are in navigation mode. Which means that we just test
            // recordIndex and column identity.

            // We'll always have a recordIndex (even if it's -1 due to virtual stores).
            // Therefore it's valid to check both record indices.
            // If they differ, the locations can never be equal.
            if (me.recordIndex !== other.recordIndex) {
                return false;
            }

            // If we get here, it is an unrendered location which can only be created
            // by encapsulating a record or recordIndex, so we will not be comparing
            // locations such as headers or footers or rowbodies.
            // We must be talking about cells.
            return me.column === other.column;
        }

        return false;
    },

    /**
     * Compares this grid Location object to another grid Location to see if they refer to the same cell address.
     * @param {Ext.grid.Location} other The grid Location to compare.
     * @return {Boolean} `true` if the other cell Location references the same cell address as this.
     */
    equalCell: function(other) {
        var me = this;

        return other && other.view === me.view && other.isGridLocation &&
               me.recordIndex === other.recordIndex &&
               me.column === other.column;
    },

    getFocusEl: function(as) {
        var cell = this.get(),
            ret;

        if (this.actionable) {
            ret = this.sourceElement;
        } else {
            ret = cell && !cell.destroyed && cell.el.dom;
        }

        // Only return the element if it is still in the document.
        return Ext.getBody().contains(ret) ?
            (as === 'dom' || as === true) ? ret : Ext.get(ret) :
            null;
    },

    /**
     * Returns the {@link Ext.grid.Cell cell} Component referenced *at the time of calling*.
     * Note that grid DOM is recycled, and the cell referenced may be repurposed for use by
     * another record.
     *
     * @param {"cmp"/"dom"/"el"} [as=el] Pass `"dom"` to always return the cell's `HTMLElement`.
     * Pass `"el"` to return the cell's `Ext.dom.Element` . Pass `"cmp"` to
     * return the cell `Ext.Component` reference for this location (if one exists).
     * @return {Ext.grid.cell.Cell} The Cell component referenced by this Location.
     */
    getCell: function(as) {
        var result = this.cell,
            ret = null;

        if (result) {
            ret = (as === 'dom' || as === true) ? result.el.dom : (as === 'cmp' ? result : result.el);
        }
        return ret;
    },

    /**
     * Returns the {@link Ext.grid.cell.Cell cell} Component referenced *at the time of
     * calling*. Note that grid DOM is recycled, and the cell referenced may be repurposed
     * for use by another record.
     *
     * @return {Ext.grid.cell.Cell} The Cell component referenced by this Location.
     */
    get: function() {
        return this.cell;
    },

    isFirstColumn: function() {
        var column = this.column,
            ret = false;

        if (column) {
            ret = this.view.isFirstVisibleColumn(column);
        }

        return ret;
    },

    isLastColumn: function() {
        var column = this.column,
            ret = false;

        if (column) {
            ret = this.view.isLastVisibleColumn(column);
        }

        return ret;
    },

    /**
     * Re-orientates this Location according to the existing settings. If for example
     * a row has been deleted, or moved by a sort, or a column has been moved, this will
     * resync internal values with reality.
     */
    refresh: function() {
        var me = this,
            column = me.column,
            oldColumnIndex = me.columnIndex,
            newColumnIndex = me.view.getHeaderContainer().indexOfLeaf(column),
            location;

        if (newColumnIndex === -1) {
            newColumnIndex = (oldColumnIndex === -1) ? 0 : oldColumnIndex;
        }

        // Restore connection to an item using superclass.
        location = me.callParent();

        // Reconnect to the column
        return location._setColumn(newColumnIndex);
    },

    /**
     * Navigates to the next visible Location.
     * @param {Boolean/Object} [options] An options object or a boolean flag meaning wrap
     * @param {Boolean} [options.wrap] `true` to wrap from the last to the first Location.
     * @return {Ext.grid.Location} A Location object representing the new location.
     */
    next: function(options) {
        var me = this,
            candidate;

        if (me.actionable) {
            return me.navigate();
        } else {
            // Maintainer: This is an empty loop. It just skips non-focusable Locations
            for (
                candidate = me.nextCell(options);
                candidate && !candidate.get().el.isFocusable();
                candidate = candidate.nextCell(options));
            return candidate || me;
        }
    },

    /**
     * Navigates to the previous visible Location.
     * @param {Boolean/Object} [options] An options object or a boolean flag meaning wrap
     * @param {Boolean} [options.wrap] `true` to wrap from the first to the last Location.
     * @returns {Ext.grid.Location} A Location object representing the new location.
     */
    previous: function(options) {
        var me = this,
            candidate;

        if (me.actionable) {
            return me.navigate(true);
        } else {
            // Maintainer: This is an empty loop. It just skips non-focusable Locations
            for (
                candidate = me.previousCell(options);
                candidate && !candidate.get().el.isFocusable();
                candidate = candidate.previousCell(options));
            return candidate || me;
        }
    },

    /**
     * Navigates to the next visible Location.
     * @param {Boolean/Object} [options] An options object or a boolean flag meaning wrap
     * @param {Boolean} [options.wrap] `true` to wrap from the last to the first Location.
     * @param {Number} [options.column] The column to move to if not the current column.
     * @returns {Ext.dataview.Location} A *new* Location object representing the new location.
     */
    down: function(options) {
        var me = this,
            column = options && options.column || me.column,
            candidate = me.nextItem(options),
            cell;

        if (candidate) {
            candidate._setColumn(column);
            cell = candidate.get();
            while (candidate && (!cell || !cell.el.isFocusable())) {
                candidate = candidate.nextItem(options);
                if (candidate) {
                    candidate._setColumn(column);
                    cell = candidate.get();
                }
            }
            if (candidate && !candidate.equals(me)) {
                return candidate;
            }
        }
        return me;
    },

    /**
     * Navigates to the previous visible Location.
     * @param {Boolean/Object} [options] An options object or a boolean flag meaning wrap
     * @param {Boolean} [options.wrap] `true` to wrap from the first to the last Location.
     * @param {Number} [options.column] The column to move to if not the current column.
     * @returns {Ext.dataview.Location} A *new* Location object representing the new location.
     */
    up: function(options) {
        var me = this,
            column = options && options.column || me.column,
            candidate = me.previousItem(options),
            cell;

        if (candidate) {
            candidate._setColumn(column);
            cell = candidate.get();
            while (candidate && (!cell || !cell.el.isFocusable())) {
                candidate = candidate.previousItem(options);
                if (candidate) {
                    candidate._setColumn(column);
                    cell = candidate.get();
                }
            }
        }
        if (candidate && !candidate.equals(me)) {
            return candidate;
        }
        return me;
    },

    privates: {
        determineActionable: function() {
            var target = this.sourceElement,
                cell = this.cell,
                actionable = false;

            // If targetElement is *not* one of our cells, then if it's focusable
            // this is an actionable location.
            //
            // For example, a floated menu or any focusable thing popped up by a widget inside a cell.
            if (target && (!cell || cell.destroyed || cell.element.dom !== target)) {
                actionable = Ext.fly(target).isFocusable(true);
            }

            return actionable;
        },

        /**
         * Navigates from the current position in actionable mode.
         * @param {Boolean} reverse
         * @return {Ext.grid.Location} The new actionable Location
         * @private
         */
        navigate: function(reverse) {
            var me = this,
                activeEl = me.sourceElement,
                view = me.view,
                scrollable = view.getScrollable(),
                actionables = view.getNavigationModel().actionables,
                len = actionables && actionables.length,
                candidate = me.clone(),
                previousCandidate = me.clone(),
                testEl,
                visitOptions = {
                    callback: function(el) {
                        testEl = Ext.fly(el);
                        if (!testEl.$isFocusTrap && testEl.isFocusable()) {
                            component = Ext.Component.from(el);

                            // If it's the focusEl of a disabled component, or the
                            // focusTrap of a PickerField, skip it
                            if (!component || !component.getDisabled()) {
                                focusables.push(el);
                            }
                        }
                    },
                    reverse: reverse,
                    skipSelf: true
                },
                i, result, component, focusables = [];

            // Loop in the passed direction until we either find a result, or we walk off the end of
            // the rendered block, in which case we have to give up.
            while (candidate && !result && candidate.get()) {
                // Collect focusables from the candidate cell arranged in the direction of travel.
                focusables.length = 0;
                candidate.get().el.visit(visitOptions);

                // See if there's a next focusable element in the current candidate
                activeEl = focusables[
                    activeEl ? (Ext.Array.indexOf(focusables, activeEl) + 1) : 0
                    ];

                if (activeEl) {
                    result = candidate;
                    result.source = result.sourceElement = activeEl;
                    delete result.actionable;

                    // We want to see the whole item which *contains* the activeEl, so we must scroll
                    // vertically to bring that into view, then scroll the activeEl into view (which
                    // will most likely do nothing unless it's a horizontal scroll needed)
                    if (candidate.child) {
                        scrollable.ensureVisible(candidate.child.el);
                    }
                    scrollable.ensureVisible(activeEl);
                    activeEl.focus();
                }

                // If we found no next focusable internal element, move to next cell and ask
                // the Actionables whether they are interested in it.
                else {
                    candidate = candidate[reverse ? 'previousCell' : 'nextCell']();

                    // The previousCell/nextCell methods return the same logical location to indicate
                    // inability to navigate that direction so we're done, the loop through candidates
                    // was unable to settle on an actionable location, so the "new" location is me.
                    if (candidate.equals(previousCandidate)) {
                        return me;
                    }

                    // If we cannot navigate, previousCell and nextCell return the same Location
                    if (candidate && len) {
                        for (i = 0; !result && i < len; i++) {
                            result = actionables[i].activateCell(candidate);
                        }
                    }
                }
                previousCandidate = candidate;
            }
            return result || me;
        },

        activate: function() {
            var me = this,
                view = me.view,
                scrollable = view.getScrollable(),
                actionables = view.getNavigationModel().actionables,
                len = actionables && actionables.length,
                candidate = me.clone(),
                activeEl, i, result;

            // Find the first focusable internal element
            candidate.get().el.visit({
                callback: function(el) {
                    if (Ext.fly(el).isFocusable()) {
                        activeEl = el;
                        return false;
                    }
                },
                skipSelf: true
            });

            if (activeEl) {
                result = candidate;
                result.source = result.sourceElement = activeEl;
                delete result.actionable;

                // We want to see the whole item which *contains* the activeEl, so we must scroll
                // vertically to bring that into view, then scroll the activeEl into view (which
                // will most likely do nothing unless it's a horizontal scroll needed)
                if (candidate.child) {
                    scrollable.ensureVisible(candidate.child.el);
                }
                scrollable.ensureVisible(activeEl);
                activeEl.focus();
            }

            // If we found no tabbable internal elements, ask the Actionables whether they
            // are interested in it.
            else {
                for (i = 0; !result && i < len; i++) {
                    result = actionables[i].activateCell(candidate);
                }
            }
            return result;
        },

        getFocusables: function() {
            var focusables = [],
                element = this.sourceElement;

            if (element) {
                Ext.fly(element).visit({
                    callback: function (el) {
                        if (Ext.fly(el).isFocusable()) {
                            focusables.push(el);
                        }
                    },
                    skipSelf: true
                });
            }
            return focusables;
        },

        /**
         * Navigates to the next visible *cell* Location.
         * @param {Boolean/Object} options An options object or a boolean flag meaning wrap
         * @param {Boolean} [options.wrap] `true` to wrap from the last to the first Location.
         * @return {Ext.grid.Location} A Location object representing the new location.
         * @private
         */
        nextCell: function(options) {
            var me = this,
                view = me.view,
                startPoint = me.clone(),
                result = me.clone(),
                columns = view.getVisibleColumns(),
                len = columns.length,
                wrap;

            if (options) {
                if (typeof options === 'boolean') {
                    wrap = options;
                } else {
                    wrap = options.wrap;
                }
            }

            do {
                // If we are at the end of the row, or it's not a grid row,
                // go down to column 0
                if (result.column === columns[len - 1] || !me.child.isGridRow) {
                    result = me.down(Ext.apply({
                        column: columns[0]
                    }, options));
                } else {
                    result._setColumn(result.columnIndex + 1);
                }

                // We've wrapped all the way round without finding a visible location.
                // Quit now.
                if (result && result.equals(startPoint)) {
                    break;
                }
            } while (result && !result.sourceElement);

            return result;
        },

        /**
         * Navigates to the previous visible *cell* Location.
         * @param {Boolean/Object} options An options object or a boolean flag meaning wrap
         * @param {Boolean} [options.wrap] `true` to wrap from the first to the last Location.
         * @returns {Ext.grid.Location} A Location object representing the new location.
         * @private
         */
        previousCell: function(options) {
            var me = this,
                view = me.view,
                startPoint = me.clone(),
                result = me.clone(),
                columns = view.getVisibleColumns(),
                wrap;

            if (options) {
                if (typeof options === 'boolean') {
                    wrap = options;
                } else {
                    wrap = options.wrap;
                }
            }

            do {
                // If we are at the start of the row, or it's not a grid row,
                // Ggo up to the last column.
                if (result.column === columns[0] || !me.child.isGridRow) {
                    result = me.up(Ext.apply({
                        column: columns.length - 1
                    }, options));
                } else {
                    result._setColumn(result.columnIndex - 1);
                }

                // We've wrapped all the way round without finding a visible location.
                // Quit now.
                if (result && result.equals(startPoint)) {
                    break;
                }
            } while (result && !result.sourceElement);

            return result;
        },

        /**
         * Internal function to allow the clone method to mutate the clone
         * as requested in the options.
         * @private
        */
        _setColumn: function(column) {
            var me = this,
                columns = me.view.getVisibleColumns(),
                index;

            if (typeof column === 'number') {
                index = column;
                column = columns[index];
            } else {
                index = columns.indexOf(column);
            }

            delete me.event;
            delete me.actionable;
            me.column = column;
            me.columnIndex = index;
            me.cell = me.row && me.row.getCellByColumn(column);
            if (me.cell) {
                me.isTreeLocation = !!me.cell.isTreeCell;
                me.sourceElement = me.cell.el.dom;
            }

            return me;
        }
    }
}, function(Cls) {
    Cls.defineProtoProperty('actionable', 'determineActionable');
});
