/**
 * @class Ext.dataview.NavigationModel
 * @private
 * This class listens for events fired from a {@link Ext.dataview.DataView DataView},
 * and tracks the currently focused item.
 */
Ext.define('Ext.dataview.NavigationModel', {
    extend: 'Ext.Evented',
    alias: 'navmodel.dataview',

    mixins: [
        'Ext.mixin.Factoryable',
        'Ext.mixin.Bufferable'
    ],

    requires: [
        'Ext.dataview.Location'
    ],

    factoryConfig: {
        type: 'navmodel',
        defaultType: 'dataview',
        instanceProp: 'isNavigationModel'
    },

    isNavigationModel: true,

    config: {
        view: null,

        disabled: false
    },

    bufferableMethods: {
        // buffer response to view's triggerEvent when that event is a focusing gesture
        // to allow focus to be processed before we go into selection.
        handleChildTrigger: 1
    },

    /**
     * @protected
     * @property {String} [locationClass=Ext.dataview.Location]
     * The name of the location class for this NavigationModel. This may be overridden in
     * subclasses.
     */
    locationClass: 'Ext.dataview.Location',

    /**
     * @property {Ext.dataview.Location} lastLocation
     * This is the location that we last positively focused upon, whether or not focus
     * has been lost from the view, and the location has been cleared.
     *
     * Contrast this with {@link #property!previousLocation).
     */

    /**
     * @property {Ext.dataview.Location} prevLocation
     * This is the location that we previously *`set`*, whether it was `null` or not.
     * So if focus is not currently in the view, this will be null.
     *
     * Contrast this with {@link #property!lastLocation).
     */

    /**
     * Focuses the passed position, and optionally selects that position.
     * @param {Ext.dataview.Location/Ext.data.Model/Number/Ext.dom.Element} location The location to focus.
     * @param {Object} [options]
     * @param {Object} [options.event] The UI event which caused the navigation if any.
     * @param {Object} [options.select] Pass as `true` to also select the location.
     * @param {Object} [options.animation] Pass as `true` or an animation config to animate to the location.
     */
    setLocation: function(location, options) {
        var me = this,
            view = me.getView(),
            oldLocation = me.location,
            animation = options && options.animation,
            scroller, child, record, itemContainer, childFloatStyle;

        if (location == null) {
            return me.clearLocation();
        }
        if (!location.isDataViewLocation) {
            location = this.createLocation(location);
        }

        // If it's a valid location, focus it.
        // Handling the consquences will happen in the onFocusMove
        // listener unless the synchronous options is passed.
        if (!location.equals(oldLocation)) {
            record = location.record;
            child = location.child;

            // If the record is not rendered, ask to scroll to it and try again
            if (record && !child) {
                // TODO: column?
                return view.ensureVisible(record, {
                    animation: animation
                }).then(function() {
                    if (!me.destroyed) {
                        me.setLocation({
                            record: record,
                            column: location.column
                        }, options);
                    }
                });
            }

            // Work out if they are using any of the ways to get the items
            // to flow inline. In which case, moving up requires extra work.
            if (child && me.floatingItems == null) {
                child = child.isComponent ? child.el : Ext.fly(child);
                itemContainer = child.up();
                childFloatStyle = child.getStyleValue('float');

                me.floatingItems = (
                    view.getInline && view.getInline()) ||
                    child.isStyle('display', 'inline-block') ||
                    childFloatStyle === 'left' || childFloatStyle === 'right' ||
                    (itemContainer.isStyle('display', 'flex') && itemContainer.isStyle('flex-direction', 'row')
                );
            }

            // Use explicit scrolling rather than relying on the browser's focus behaviour.
            // Scroll on focus overscrolls. ensureVisible scrolls exactly correctly.
            scroller = view.getScrollable();
            if (scroller) {
                scroller.ensureVisible(location.sourceElement, {
                    animation: options && options.animation
                });
            }
            // Handling the impending focus event is separated because it also needs to
            // happen in case of a focus move caused by assistive technologies.
            me.handleLocationChange(location, options);

            // Event handlers may have destroyed the view (and this)
            if (!me.destroyed) {
                me.doFocus();
            }
        }
    },

    clearLocation: function() {
        var me = this,
            targetElement;

        if (me.location) {
            me.previousLocation = me.location;
            targetElement = me.location.getFocusEl();
            if (targetElement && !targetElement.destroyed) {
                Ext.fly(targetElement).removeCls(me.focusedCls);
            }
            me.location = null;
        }
    },

    getLocation: function () {
        return this.location;
    },

    getPreviousLocation: function() {
        var result = this.previousLocation;

        if (result && (!result.sourceElement || !result.sourceElement.destroyed)) {
            result.refresh();
        }
        return result;
    },

    disable: function() {
        this.setDisabled(true);
    },

    enable: function() {
        this.setDisabled(false);
    },

    privates: {
        createLocation: function(source, options) {
            return Ext.create(this.locationClass, this.getView(), source, options);
        },

        getKeyNavCfg: function(view) {
            var me = this;

            return {
                target: view.getFocusEl(),
                processEvent: me.processViewEvent,
                processEventScope: me,
                eventName: 'keydown',
                defaultEventAction: 'stopEvent',
                esc: me.onKeyEsc,
                f2: me.onKeyF2,
                up: me.onKeyUp,
                down: me.onKeyDown,
                right: me.onKeyRight,
                left: me.onKeyLeft,
                pageDown: me.onKeyPageDown,
                pageUp: me.onKeyPageUp,
                home: me.onKeyHome,
                end: me.onKeyEnd,
                space: me.onKeySpace,
                enter: me.onKeyEnter,
                tab: me.onKeyTab,
                A: {
                    ctrl: true,
                    // Need a separate function because we don't want the key
                    // events passed on to selectAll (causes event suppression).
                    handler: me.onSelectAllKeyPress
                },
                scope: me
            };
        },

        updateView: function (view) {
            var me = this,
                keyNavCfg = me.getKeyNavCfg(view);

            me.focusedCls = view.focusedCls;

            // Drive the KeyNav off the View's itemkeydown event so that beforeitemkeydown listeners may veto.
            // By default KeyNav uses defaultEventAction: 'stopEvent', and this is required for movement keys
            // which by default affect scrolling.
            if (keyNavCfg) {
                me.keyNav = new Ext.util.KeyNav(keyNavCfg);
            }

            me.viewListeners = view.on(me.getViewListeners(view));
        },

        getViewListeners: function(view) {
            var result = {
                scope: this
            };
            result[view.getTriggerEvent()] = 'onChildTrigger';
            return result;
        },

        // We ignore input fields.
        processViewEvent: function(e) {
            var location = this.getLocation(),
                component;

            if (location && e.keyCode) {
                component = Ext.fly(e.target).component;

                // This flag indicates that the key event source is the dataview item.
                // Some key handlers only react in navigable mode.
                // TODO: implement actionable mode in DataViews.
                e.navigationMode = component && component.parent === this.getView();

                e.setCurrentTarget(location.sourceElement);
                if (!Ext.fly(e.target).isInputField()) {
                    return e;
                }
            }
        },

        /**
         * @private
         * Focuses the passed location
         * May be overridden in subclasses which do not focus the targets
         */
        doFocus: function(location) {
            location = location || this.location;

            // getElement returns the focusEl.
            // So for navigation mode, that's the navigation level element, ie
            // dataview item or grid cell.
            // For actionable mode, that's the focused sub-element.
            if (location && location.getFocusEl()) {
                location.getFocusEl().focus();
            }
        },

        // In the case of a focus move invoked by assistive technologies,
        // we have to react to that and maintain correct state.
        onFocusMove: function(e) {
            var location = this.createLocation(e);

            // If a setLocation call has been called with the synchronous option
            // handleLocationChange will already have been called.
            if (!location.equals(this.location)) {
                this.handleLocationChange(location, {
                    event: e,
                    navigate: false // we just navigated
                });
            }
        },

        handleLocationChange: function(location, options) {
            var me = this,
                oldLocation = me.location,
                view = me.getView(),
                target, item;

            // There is a subtle difference between previousLocation and lastLocation.
            //
            // previousLocation is where we focused previously whether null or not. So
            // when the location is cleared, for instance on view focusLeave, previousLocation
            // is cleared.
            //
            //
            // lastLocation is the last location that was positively focused.
            me.previousLocation = oldLocation;
            if (oldLocation) {
                me.lastLocation = oldLocation;

                // getFocusEl returns the focusEl.
                // So for navigation mode, that's the navigation level element, ie
                // dataview item or grid cell.
                // For actionable mode, that's the focused sub-element.
                // It may have been destroyed (eg 31 when month switches from Jan to Feb).
                target = oldLocation.getFocusEl();
                if (target && !target.destroyed) {
                    Ext.fly(target).removeCls(me.focusedCls);
                }
            }

            me.location = location;

            // If we are navigating to one of our navigable items, add our focused class to it.
            target = location && location.getFocusEl('dom');
            if (target) {
                item = location.get();
                if (item) {
                    if (item.isWidget) {
                        item = item.el;
                    } else {
                        item = Ext.get(item);
                    }
                    if (item && target === item.dom) {
                        item.addCls(me.focusedCls);
                    }

                    if (options && (options.event || options.select) && options.navigate !== false) {
                        me.onNavigate(options.event);
                    }
                }
            }

            // Event handlers may destroy the view
            if (!view.destroyed) {
                view.fireEvent('navigate', view, location, oldLocation);
            }
        },

        onKeyUp: function(e) {
            // Do not scroll
            e.preventDefault();

            if (this.location) {
                if (this.floatingItems) {
                    this.moveUp(e);
                } else {
                    this.movePrevious({
                        event: e
                    });
                }
            } else {
                this.setLocation(0);
            }
        },

        onKeyDown: function(e) {
            // Do not scroll
            e.preventDefault();

            if (this.location) {
                if (this.floatingItems) {
                    this.moveDown(e);
                } else {
                    this.moveNext({
                        event: e
                    });
                }
            } else {
                this.setLocation(0);
            }
        },

        onKeyLeft: function(e) {
            // Do not scroll
            e.preventDefault();

            this.movePrevious({
                event: e
            });
        },

        onKeyRight: function(e) {
            // Do not scroll
            e.preventDefault();

            this.moveNext({
                event: e
            });
        },

        onKeyF2: function(e) {
            return false;
        },

        onKeyEsc:function(e) {
            return false;
        },

        onKeyTab: function(e) {
            return !this.location.actionable;
        },

        onKeyPageDown: function(e) {
            // Do not scroll
            e.preventDefault();

            if (!this.location.actionable && !this.floatingItems) {
                var me = this,
                    view = me.getView(),
                    y = (view.infinite ? view.getItemTop(me.location.child) : me.location.child.el.dom.offsetTop) + view.el.getClientRegion().height,
                    candidate = me.createLocation(view.getItemFromPoint(0, y));

                // Might have landed on a non-focusable item.
                // The previous item moves to a focusable location.
                if (!(candidate.child && candidate.child.el.isFocusable())) {
                    candidate = candidate.previous();
                }
                // Go down by the visible page size
                me.setLocation(candidate, {
                    event: e
                });
            }
        },

        onKeyPageUp: function(e) {
            // Do not scroll
            e.preventDefault();

            if (!this.location.actionable && !this.floatingItems) {
                var me = this,
                    view = me.getView(),
                    y = (view.infinite ? view.getItemTop(me.location.child) : me.location.child.el.dom.offsetTop) - view.el.getClientRegion().height,
                    candidate = me.createLocation(view.getItemFromPoint(0, y));

                // Might have landed on a non-focusable item.
                // The next method advances to a focusable location.
                if (!(candidate.child && candidate.child.el.isFocusable())) {
                    candidate = candidate.next();
                }
                // Go up by the visible page size
                me.setLocation(candidate, {
                    event: e
                });
            }
        },


        onKeyHome: function(e) {
            this.setLocation(0, {
                event: e
            });
        },

        onKeyEnd: function(e) {
            this.setLocation(this.getView().getStore().last(), {
                event: e
            });
        },

        onKeySpace: function(e) {
            this.onNavigate(e);
        },

        // ENTER emulates an childtap event at the View level
        onKeyEnter: function(e) {
            // Stop the keydown event so that an ENTER keyup does not get delivered to
            // any element which focus is transferred to in a click handler.
            e.stopEvent();
            this.getView()._onChildTap(e);
        },

        onSelectAllKeyPress: function(e) {
            var view = this.getView(),
                selModel = view.getSelectable();

            // If there are items to select, select them, and do not allow any other
            // consequences to flow from CTRL/A, it would be confusing to the user.
            if (selModel && view.getStore().getCount()) {
                selModel[selModel.allSelected ? 'deselectAll' : 'selectAll']();
                e.preventDefault();
                return false;
            }
        },

        // For use with inline DataViews, such as the KS.
        // We must see what's above
        moveUp: function(e) {
            var view = this.getView(),
                location = this.location,
                el = this.location.sourceElement,
                topCentre = Ext.fly(el).getAnchorXY('t'),
                item;

            // Look above the top centre of this item's element
            // Move 10pixels past any top/bottom padding;
            topCentre[1] -= (Ext.fly(el).getMargin('tb') + 10);
            item = this.getView().getItemFromPagePoint(topCentre[0], topCentre[1], true);

            // Nothing above us, move to first, unless we are first, in which case,
            // wrap to last.
            if (!item || !item.isFocusable()) {
                item = location.isFirst() ? view.getLastItem() : view.getFirstItem();
            }
            if (item) {
                this.setLocation(item, {
                    event: e
                });
            }
        },

        // For use with inline DataViews, such as the KS.
        // We must see what's below
        moveDown: function(e) {
            var view = this.getView(),
                location = this.location,
                el = location.sourceElement,
                bottomCentre = Ext.fly(el).getAnchorXY('b'),
                item;

            // Look above the top centre of this item's element
            // Move 10pixels past any top/bottom padding;
            bottomCentre[1] += Ext.fly(el).getMargin('tb') + 10;
            item = view.getItemFromPagePoint(bottomCentre[0], bottomCentre[1]);

            // If we're on the last line, above blank space, go to last
            if (!item || !item.isFocusable()) {
                item = location.isLast() ? view.getFirstItem() : view.getLastItem();
            }
            if (item) {
                this.setLocation(item, {
                    event: e
                });
            }
        },

        moveNext: function(options) {
            var location = this.getLocation();

            if (location) {
                location = location.next(options);
                if (location) {
                    this.setLocation(location, options);
                }
            }
        },

        movePrevious: function(options) {
            var location = this.getLocation();

            if (location) {
                location = location.previous(options);
                if (location) {
                    this.setLocation(location, options);
                }
            }
        },

        onChildTrigger: function(view, location) {
            var e = location.event,
                isFocusingEvent = (e.pointerType === 'touch') ? e.type ==='tap' : e.type ==='touchstart';

            // The selection event handler must run after any navigation caused by the
            // event has been processed.
            // For mouse click events this won't have an effect, mousedown will have focused and
            // navigated before the click. If the triggerEvent is ever configured to 'childtouchstart'
            // then on a mousedown event, focus will not have moved, so this will become important.
            // For touch gestures, its's the tap that focuses, so we must wait until
            // the impending focusMove notification has done the navigation.
            if (isFocusingEvent) {
                this.handleChildTrigger(view, location);
            } else {
                this.doHandleChildTrigger(view, location);
            }
        },

        doHandleChildTrigger: function(view, location) {
            var myLocation = this.location,
                event = location.event,
                compareMethod = location.isGridLocation ? 'equalCell' : 'equals';

            // This is the selection gesture for the view.
            // We do not navigate on this gesture, navigation is driven by response to focus.
            // If that gesture results in fous, well and good - it will find the
            // location already selected.
            // We just call onNavigate which is how we go into selection
            // in response to navigation. Unless we have navigated already to this
            // location.
            if (myLocation && myLocation[compareMethod](location)) {
                this.onNavigate(event);
            }
            // If we ever get here and there has been no focus-driven navigation
            // navigate now. Synthetic events can do this.
            else {
                this.setLocation(location, {
                    event: event
                });
            }
        },

        onNavigate: function(event) {
            var me = this,
                location = me.location;

            // Fake up an event if we have no event, but are just being commanded to select
            if (!event) {
                event = new Ext.event.Event({
                    target: location.sourceElement
                });
            }
            Ext.apply(event, {
                navigationModel: me,
                from: me.previousLocation,
                to: location
            });
            me.getView().onNavigate(event);
        },

        updateDisabled: function(disabled) {
            // If the view is not focusable, (or, in the case of a BoundList, if it does
            // not have access to its ownerField - eg unit tests) then there will be no key event source
            // and so no keyNav.
            if (this.keyNav) {
                if (disabled) {
                    this.keyNav.disable();
                } else {
                    this.keyNav.enable();
                }
            }
        }
    }
 });
