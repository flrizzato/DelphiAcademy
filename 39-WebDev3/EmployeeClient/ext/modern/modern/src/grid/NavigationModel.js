/**
 * @class Ext.grid.NavigationModel
 * @private
 * This class listens for events fired from a {@link Ext.grid.Panel GridPanel}, and
 * tracks the currently focused cell.
 */
Ext.define('Ext.grid.NavigationModel', {
    extend: 'Ext.dataview.NavigationModel',
    alias: 'navmodel.grid',

    requires: [
        'Ext.grid.Location'
    ],

    locationClass: 'Ext.grid.Location',
    
    statics: {
        /**
         * We should ignore keydown events for certain keys pressed in an input field
         * e.g. while editing, to allow for native arrow key navigation, Home/End keys,
         * etc. However we can't ignore all keydown events in input fields wholesale,
         * that breaks Enter/Esc/Tab key processing.
         * This map defines what key names should be ignored if target is an input field.
         * @private
         * @since 6.5.1
         */
        ignoreInputFieldKeys: {
            PAGE_UP: true,
            PAGE_DOWN: true,
            END: true,
            HOME: true,
            LEFT: true,
            UP: true,
            RIGHT: true,
            DOWN: true
        }
    },

    /**
     * Focuses the passed position, and optionally selects that position.
     * @param {Ext.grid.Location/Ext.data.Model/Number} location The location to focus.
     * This may be
     *     - A `{@link Ext.grid.Location grid Location}` object;
     *     - A `[column, row]` array; column is `X` coordinate, record is `Y` coordinate.
     *     - An element which is used to locate the referenced cell.
     *     - A number to locate column zero on the passed row
     *     - A record to indicate column zero on the passed record's row.
     * @param {Object} [options]
     * @param {Object} [options.event] The UI event which caused the navigation if any.
     * @param {Object} [options.select] Pass as `true` to also select the location.
     */
    setLocation: function(location, options) {
        var me = this,
            view = me.getView(),
            event = options && options.event;

        me.columnIndex = -1;

        // Massage the incoming location into a form the base class can work with
        if (location != null && !location.isGridLocation) {
            if (Ext.isArray(location)) {
                location = {
                    column: location[0],
                    record: location[1]
                };
            } else if (typeof location === 'number') {
                location = view.store.getAt(location);
            }
            location = me.createLocation(location);
            if (event) {
                location.event = event;
            }
        }
        return me.callParent([location, options]);
    },

    clearLocation: function() {
        var me = this,
            item;

        if (me.location) {
            me.previousLocation = me.location;
            item = me.location.sourceElement;
            if (item) {
                Ext.fly(item).removeCls(me.focusedCls);
            }
            me.location = null;
        }
    },

    registerActionable: function(actionable) {
        var me = this,
            view = me.getView(),
            actionables = me.actionables || (me.actionables = []),
            triggerEvent, listeners;

        if (!Ext.Array.contains(actionables, actionable)) {
            actionables.push(actionable);
            triggerEvent = actionable.getTriggerEvent();
            if (triggerEvent) {

                // create {click: triggerActionable, scope: me, args:[theActionable]}
                listeners = {
                    scope: me,
                    args: [actionable]
                };
                listeners[triggerEvent] = 'triggerActionable';

                // Trigger the actionable on the requested event.
                // Also, per ARIA standards:
                // "Enter or F2 pressed while focus is on a cell containing
                // an actionable item enters Actionable Mode"
                actionable.triggerEventListener = view.bodyElement.on(listeners);
            }
        }
    },

    unregisterActionable: function(actionable) {
        var actionables = this.actionables;

        if (actionables) {
            Ext.Array.remove(actionables, actionable);
        }
    },

    privates: {
        // In the case of a focus move invoked by assistive technologies,
        // we have to react to that and maintain correct state.
        onFocusMove: function(e) {
            var me = this,
                view = me.getView(),
                location = me.getLocation();

            // Focus moved to the view from an inner location.
            // This can happen on mousedown on non-focusable parts of the grid
            // such as row lines (which are not part of a cell) and non-focusable
            // non-data items. We revert to the current location.
            if (e.toElement === view.el.dom && location) {
                me.clearLocation();
                return me.setLocation(location);
            }

            location = me.createLocation(e);

            // If a setLocation call has been called with the synchronous option
            // handleLocationChange will already have been called.
            if (!location.equals(me.location)) {
                me.handleLocationChange(location, {
                    event: e,
                    navigate: false // We just navigated
                });
            }
        },

        processViewEvent: function(e) {
            var me = this,
                view = me.getView(),
                cell = view.mapToCell(e);
            
            if (Ext.fly(e.target).isInputField() && me.self.ignoreInputFieldKeys[e.getKeyName()]) {
                return false;
            }

            // We found our grid cell which contained the event.
            if (cell && cell.row.grid === view) {
                return e;
            }
        },

        /**
         * Enters actionable mode at the passed location
         */
        activateCell: function(location) {
            // Attempt to activate a Location based on the current location.
            location.clone().activate();
        },

        triggerActionable: function(actionable, e) {
            var actionLocation;

            // Request the Actionable to activate a *cell* Location based on the event location.
            actionLocation = actionable.activateCell(this.createLocation(e));

            // If we successfully started actionable mode, set our location.
            // Note that this may pass the already active location if the trigger
            // event was inside the actionable, such as clicking in a cell editor.
            // This will be a no-op, so harmless.
            if (actionLocation) {
                this.setLocation(actionLocation);
            }
        },

        onChildTouchStart: function(view, location) {
            var e = location.event;

            // Not a navigable child - do not disturb focus
            if (location.header || location.footer) {
                e.preventDefault();
            } else {
                // Only react if we already have a location that is elsewhere in the grid,
                // so that we can move.
                //
                // If we do not, this means that there will be an impending
                // onFocusEnter call, and that event will be decoded to focus
                // the targeted location.
                if (this.location && !this.location.equalCell(location)) {
                    this.setLocation(location, {
                        event: location.event,
                        navigate: this.getView().getTriggerEvent() === 'childtouchstart'
                    });
                }
            }
        },

        onKeyUp: function(e) {
            // Do not scroll
            e.preventDefault();

            if (!this.location.actionable) {
                if (this.location) {
                    this.moveUp(e);
                } else {
                    this.setLocation(0);
                }
            }
        },

        onKeyDown: function(e) {
            // Do not scroll
            e.preventDefault();

            if (!this.location.actionable) {
                if (this.location) {
                    this.moveDown(e);
                } else {
                    this.setLocation(0);
                }
            }
        },

        onKeyLeft: function(e) {
            var location = this.location,
                isSimpleTree = location.isLastColumn() && location.isFirstColumn();

            if (!location.actionable) {
                // Do not scroll
                e.preventDefault();

                // On an expanded non-leaf tree cell.
                if (location.isTreeLocation && !location.record.isLeaf() && location.record.isExpanded()) {

                    // If a simple Tree (just one column), or its a ctrl+RIGHT
                    // expand the node.
                    if (isSimpleTree === !e.ctrlKey) {
                        return location.cell.collapse();
                    }
                }

                // Do not allow SHIFT+(left|right) to wrap.
                if (!(e.shiftKey && location.isFirstColumn())) {
                    this.movePrevious({
                        event: e
                    });
                }
            }
            // From an input field - return true to *not* stop the event.
            else if (Ext.fly(e.target).isInputField()) {
                return true;
            }
        },

        onKeyRight: function(e) {
            var location = this.location,
                isSimpleTree = location.isLastColumn() && location.isFirstColumn();

            if (!location.actionable) {
                // Do not scroll
                e.preventDefault();

                // On a collapsed non-leaf tree cell.
                if (location.isTreeLocation && !location.record.isLeaf() && !location.record.isExpanded()) {

                    // If a simple Tree (just one column), or its a ctrl+RIGHT
                    // expand the node.
                    if (isSimpleTree === !e.ctrlKey) {
                        return location.cell.expand();
                    }
                }

                // Do not allow SHIFT+(left|right) to wrap.
                if (!(e.shiftKey && location.isLastColumn())) {
                    this.moveNext({
                        event: e
                    });
                }
            }
            // From an input field - return true to *not* stop the event.
            else if (Ext.fly(e.target).isInputField()) {
                return true;
            }
        },

        onKeyF2: function(e) {
            // Events are tagged with information about the event target.
            // If the target is *within* the cell, it's actionable mode.
            // If the target *is* the cell, it's navigation mode.
            if (this.location.actionable) {
                this.onKeyEsc();
            } else {
                this.activateCell(this.location);
            }
        },

        onKeyEsc: function(e) {
            // Focus the item which the current location encpsulates, regardless of whether it
            // also encpsulates an inner, focusable targetElement..
            if (this.location.actionable) {
                this.location.get().el.focus();
            }
        },

        onKeyTab: function(e) {
            var me = this,
                view = me.getView(),
                location = me.location,
                navigate;

            if (location.actionable) {
                navigate = function() {
                    me.location = e.shiftKey ? location.previous() : location.next();
                };
                // Now ensure that item is visible beore tabbing.
                view.ensureVisible(location.record).then(function() {
                    // TODO: ensureVisible does not ensure the item is present - it just scrolls
                    // and does not wait for the resulting List adjustment.
                    // TODO: workaround is a 100ms delay. Remove this when ensureVisible guarantees item presence.
                    if (view.mapToItem(location.record)) {
                        navigate();
                    } else {
                        Ext.defer(navigate, 100);
                    }
                });
            }
            // Navigation mode - return true to *not* stop the event
            else {
                return true;
            }
        },

        onKeyPageDown: function(e) {
            // Do not scroll
            e.preventDefault();

            if (!this.location.actionable) {
                var me = this,
                    view = me.getView(),
                    y = (view.infinite ? view.getItemTop(me.location.child) : me.location.child.el.dom.offsetTop) + view.getVisibleHeight(),
                    candidate = view.getRecordIndexFromPoint(0, y);

                view.ensureVisible(candidate).then(function() {
                    candidate = new Ext.grid.Location(view, {
                        record: candidate,
                        column: me.location.column
                    });

                    // Might have landed on a non-focusable item.
                    // The up method moves to a focusable location.
                    if (!(candidate.sourceElement && Ext.fly(candidate.sourceElement).isFocusable())) {
                        candidate = candidate.up();
                    }
                    // Go down by the visible page size
                    me.setLocation(candidate, {
                        event: e
                    });
                });
            }
        },

        onKeyPageUp: function(e) {
            // Do not scroll
            e.preventDefault();

            if (!this.location.actionable) {
                var me = this,
                    view = me.getView(),
                    y = (view.infinite ? view.getItemTop(me.location.child) : me.location.child.el.dom.offsetTop) - view.getVisibleHeight(),
                    candidate = view.getRecordIndexFromPoint(0, y);

                view.ensureVisible(candidate).then(function() {
                    candidate = new Ext.grid.Location(view, {
                        record: candidate,
                        column: me.location.column
                    });

                    // Might have landed on a non-focusable item.
                    // The down method advances to a focusable location.
                    if (!(candidate.sourceElement && Ext.fly(candidate.sourceElement).isFocusable())) {
                        candidate = candidate.down();
                    }
                    // Go up by the visible page size
                    me.setLocation(candidate, {
                        event: e
                    });
                });
            }
        },

        onKeyHome: function(e) {
            // Do not scroll
            e.preventDefault();

            if (!this.location.actionable) {
                // Go to first cell in current column
                if (e.ctrlKey) {
                    this.setLocation({
                        record: this.getView().getStore().first(),
                        column: this.location.column
                    }, {
                        event: e
                    });
                }
                // Go to first cell in row
                else {
                    this.setLocation({
                        record: this.location.record,
                        column: this.getView().getFirstVisibleColumn()
                    }, {
                        event: e
                    });
                }
            }
        },

        onKeyEnd: function(e) {

            // Do not scroll
            e.preventDefault();

            if (!this.location.actionable) {
                // Go to last cell in current column
                if (e.ctrlKey) {
                    this.setLocation({
                        record: this.getView().getStore().last(),
                        column: this.location.column
                    }, {
                        event: e
                    });
                }
                // Go to last cell in row
                else {
                    this.setLocation({
                        record: this.location.record,
                        column: this.getView().getLastVisibleColumn()
                    }, {
                        event: e
                    });
                }
            }
        },

        onKeySpace: function(e) {
            var target = Ext.fly(e.target),
                events, focusables, result;

            // SPACE hits up the Selection Model.
            // But also click the first focusable inner el.
            this.onNavigate(e);

            if (!this.location.actionable) {
                focusables = this.location.getFocusables();
                if (focusables.length) {
                    events = Ext.get(focusables[0]).events;
                }
            }
            // In actionable mode, SPACE clicks the focused el
            // if it is not an input field.
            else {
                if (target.isInputField()) {
                    result = true;
                } else {
                    events = target.events;
                }
            }

            // Fire any click or tap handlers on the discovered action element.
            if (events) {
                if (events.tap) {
                    events.tap.fire(e);
                }
                if (events.click) {
                    events.click.fire(e);
                }
            }
            return result;
        },

        // ENTER emulates an childtap event at the View level
        onKeyEnter: function(e) {
            var l = this.location;

            // Stop the keydown event so that an ENTER keyup does not get delivered to
            // any element which focus is transferred to in a click handler.
            e.stopEvent();

            // Navigation mode drops into actionable mode
            if (!l.actionable) {
                // Enter on a CheckNode, toggles it.
                if (l.isTreeLocation && l.record.data.checked != null) {
                    l.record.set('checked', !l.record.data.checked);
                } else {
                    this.activateCell(l);
                }
            }
            // Actionable mode clicks the target, same as SPACE
            else {
                this.onKeySpace(e);
            }
        },

        onSelectAllKeyPress: function(e) {
            // Return true to not stop the event if it's in an input field
            if (Ext.fly(e.target).isInputField()) {
                return true;
            }
            // Superclass selects all
            else {
                return this.callParent([e]);
            }
        },

        moveUp: function(e) {
            var location = this.getLocation();

            if (location) {
                location = location.up();
                if (location) {
                    this.setLocation(location, {
                        event: e
                    });
                }
            }
        },

        moveDown: function(e) {
            var location = this.getLocation();

            if (location) {
                location = location.down();
                if (location) {
                    this.setLocation(location, {
                        event: e
                    });
                }
            }
        }
    }
});
