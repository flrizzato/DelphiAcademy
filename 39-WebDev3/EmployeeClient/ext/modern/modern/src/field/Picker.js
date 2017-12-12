/**
 * An abstract class for fields that have a single trigger which opens a "picker" popup
 * above the field. It provides a base implementation for toggling the picker's
 * visibility when the trigger is tapped.
 *
 * You would not normally use this class directly, but instead use it as the parent
 * class for a specific picker field implementation.
 */
Ext.define('Ext.field.Picker', {
    extend: 'Ext.field.Text',
    xtype: 'pickerfield',

    requires: [
        'Ext.field.trigger.Expand'
    ],

    config: {

        /**
         * @cfg {String/Object} [picker='auto']
         *
         * A string representing the type of picker to use.  Can be one of the following values.
         *
         * - `'edge'` to use the {@link #edgePicker}, generally used on small formfactor devices.
         * - `'floated'` to use the {@link #floatedPicker}, generally used on tablets or desktops.
         * - `'auto'` to allow the framework to select the appropriate picker for the device.
         *
         * Can also be a config object for the picker.
         *
         */
        picker: {
            lazy: true,
            $value: 'auto'
        },

        /**
         * A configuration object, containing an {@link cfg#xtype} property which specifies the widget to
         * create if `{@link #cfg!picker}: 'floated'` (or if it's '`auto'` and the app is *not* on a phone)
         * Replaces `defaultTabletPicker`
         * @since 6.5.0
         */
        floatedPicker: {
            lazy: true,
            $value: null
        },

        /**
         * A configuration object, containing an {@link cfg#xtype} property which specifies the widget to
         * create if `{@link #cfg!picker}: 'edge'` (or if it's '`auto'` and the app is on a phone)
         * Replaces `defaultPhonePicker`
         * @since 6.5.0
         */
        edgePicker: {
            lazy: true,
            $value: null
        },

        clearable: false,

        /**
         * @cfg {Boolean} [matchFieldWidth=true]
         * *Only valid when the `{@link #cfg!picker}: 'floated'` is used.
         * Whether the {@link #cfg!floatedPicker}'s width should be explicitly set to match the width of the input element.
         */
        matchFieldWidth: true,

        /**
         * @cfg {String} [floatedPickerAlign=tl-bl?]
         * *Only valud when the {@link #cfg!floatedPicker} is used.
         * The {@link Ext.Component#method!showBy} alignment string to use when showing the floated picker
         * by the input field.
         */
        floatedPickerAlign: 'tl-bl?',

        /**
         * @cfg {String} pickerSlotAlign
         * The alignment of text in the picker created by this Select
         * @private
         */
        pickerSlotAlign: 'center',

        /**
         * @cfg {Boolean} hideTrigger
         * `true` to hide the expand {@link #triggers trigger}.
         */
        hideTrigger: false,

        /**
         * @private
         */
        focusTrap: {
            lazy: true,
            $value: {
                tabIndex: -1,
                cls: 'x-hidden-clip'
            }
        }
    },

    triggers: {
        expand: {
            type: 'expand'
        }
    },

    /**
     * @cfg {String} alignTarget
     * The element reference to which the {@link #cfg!picker floated picker} aligns
     * and sizes to. By default, it sizes to the `bodyElement` which encapsulates the
     * input field and triggers.
     *
     * An alternate value which may be useful if using `floated` pickers on phone platforms
     * could be `el`, to align the picker to the field's encapsulating element.
     */
    alignTarget: 'bodyElement',

    keyMap: {
        scope: 'this',
        DOWN: 'onDownArrow',
        ESC: 'onEsc'
    },
    keyMapTarget: 'inputElement',

    /**
     * @cfg {Boolean} [autoComplete=false]
     * Autocomplete is disabled on Picker fields by default.
     */
    autoComplete: false,

    classCls: Ext.baseCSSPrefix + 'pickerfield',

    /**
     * @event expand
     * Fires when the field's picker is expanded.
     * @param {Ext.form.field.Picker} field This field instance
     */

    /**
     * @event collapse
     * Fires when the field's picker is collapsed.
     * @param {Ext.form.field.Picker} field This field instance
     */

    /**
     * @private
     */
    initialize: function () {
        var me = this;

        me.callParent();

        Ext.on('hide', 'onGlobalHide', me);
        me.inputElement.on('click', 'onInputElementClick', me);
    },

    onFocus: function (e) {
        this.callParent([e]);

        if (Ext.isTouchMode()) {
            this.getFocusTrap().focus();
            this.expand();
        }
    },

    onFocusMove: function (info) {
        var me = this,
            focusTrap;

        me.callParent([info]);

        if (Ext.isTouchMode()) {
            // Avoid creating the focus trap if not in touch mode
            focusTrap = me.getFocusTrap();

            if (info.fromElement === focusTrap.dom && info.toElement === me.getFocusEl().dom) {
                if (me.getEditable()) {
                    // virtual keyboard is about to display. collapse the picker
                    me.collapse();
                } else {
                    // No keyboard can be displayed, so ensure the picker
                    // is always visible
                    focusTrap.focus();
                    me.expand();
                }
            }
        }
    },

    onFocusLeave: function (e) {
        this.callParent([e]);

        // Callparent first; collapse listener needs to read correct containsFocus state
        this.collapse();
    },

    /**
     * @private
     */
    onEsc: function (e) {
        if (Ext.isIE) {
            // Stop the esc key from "restoring" the previous value in IE
            // For example, type "foo". Highlight all the text, hit backspace.
            // Hit esc, "foo" will be restored. This behaviour doesn't occur
            // in any other browsers
            e.preventDefault();
        }

        if (this.expanded) {
            this.collapse();
            e.stopEvent();
        }
    },

    onDownArrow: function (e) {
        var me = this;

        if ((e.time - me.lastDownArrow) > 150) {
            delete me.lastDownArrow;
        }

        if (!me.expanded) {
            // Do not let the down arrow event propagate into the picker
            e.stopEvent();

            // Don't call expand() directly as there may be additional processing involved before
            // expanding, e.g. in the case of a ComboBox query.
            me.onExpandTap();

            // Tell setPickerLocation that it's invoked from the keyboard so
            // that it may set the location regardless of other settings.
            // For example, ComboBox has autoSelect and autoSelectLast which *may*
            // be set to false for some applications. This information
            // can override that.
            me.setPickerLocation(true);

            me.lastDownArrow = e.time;
        }
        else if (!e.stopped && (e.time - me.lastDownArrow) < 150) {
            delete me.lastDownArrow;
        }
    },

    /**
     * @template
     * @method
     * @param {Boolean} [fromKeyboard=false] Passed as `true` if the keyboard was used
     * to open the picker. This usually means that picker location should be set.
     *
     * A function which may be implemented in subclasses which moves the focus
     * to the value in the {@link #cfg!picker} which matches this field's value.
     *
     * For example, if you were writing a time picker, this method would be where
     * you synced the picker's value with the field's value.
     */
    setPickerLocation: Ext.emptyFn,

    updateHideTrigger: function(hideTrigger) {
        var triggers = this.getTriggers(),
            expand = triggers && triggers.expand;

        if (expand) {
            expand.setHidden(hideTrigger);
        }
    },

    applyPicker: function (picker) {
        var me = this,
            pickerListeners = {
                show: 'onPickerShow',
                hide: 'onPickerHide',
                scope: me
            },
            type = picker,
            config;

        if (!type) {
            type = 'auto';
        }
        else if (Ext.isObject(picker)) {
            type = null;

            if (!picker.isWidget && !picker.xtype) {
                config = picker;
                type = 'auto';
            }
        }

        if (type) {
            if (type === 'auto') {
                type = Ext.platformTags.phone ? 'edge' : 'floated';
            }

            if (type === 'edge') {
                picker = me.createEdgePicker(config);
            }
            //<debug>
            else if (type !== 'floated') {
                Ext.raise('Picker type must be "edge" or "floated" not "' + type + '"');
            }
            //</debug>
            else {
                picker = me.createFloatedPicker(config);
                pickerListeners.resize = pickerListeners.hiddenchange = 'realignFloatedPicker';
            }
        }

        if (picker.isWidget) {
            picker.ownerField = me;
        } else {
            picker = Ext.apply({
                ownerField: me
            }, picker);
            
            // Allow mutation of the picker configuration
            me.fireEvent('beforepickercreate', me, picker);

            picker = Ext.create(picker);
        }

        // Detect whether we are using a floated or edge picker.
        me.pickerType = type || (picker.isViewportMenu ? 'edge' : 'floated');

        // Allow configuration of the instantiated picker
        me.fireEvent('pickercreate', me, picker);

        picker.on(pickerListeners);
        return picker;
    },

    updatePicker: function (picker) {
        var value = this.getValue();

        if (picker && picker.setValue && value != null) {
            if (this.pickerType === 'floated' || picker.isPicker) {
                picker.setValue(value);
            }
        }
    },

    applyFocusTrap: function (focusTrap) {
        var result = this.el.appendChild(Ext.dom.Element.create(focusTrap));

        // Flag to indicate that it should not be considered for programmatic focus.
        // For example Grid Location actionable navigation ignores elements
        // with this property set when searching for actionable elements.
        result.$isFocusTrap = true;
        return result;
    },

    onResize: function () {
        // See if the picker has been created
        var picker = this.getConfig('picker', false, true);

        if (picker && picker.isVisible()) {
            this.realignFloatedPicker();
        }
    },

    /**
     * @private
     */
    realignFloatedPicker: function (picker) {
        var me = this;

        picker = me.getConfig('picker', false, true);

        if (picker && picker.isVisible()) {
            if (me.getMatchFieldWidth()) {
                picker.setWidth(me[me.alignTarget].getWidth());
            }
            picker.realign(me[me.alignTarget], me.getFloatedPickerAlign(), {
                minHeight: 100
            });
            me.setPickerLocation();
        }
    },

    onInputElementClick: function (e) {
        var me = this;

        if (e.pointerType === 'mouse' && (!me.getEditable() && !me.getReadOnly())) {
            me[me.expanded ? 'collapse' : 'expand']();
        }
    },

    onExpandTap: function () {
        if (this.expanded) {
            this.collapse();
        } else {
            this.expand();
        }

        return false;
    },

    expand: function () {
        if (!this.expanded && !this.getDisabled()) {
            this.showPicker();
        }
    },

    collapse: function () {
        var picker;

        if (this.expanded) {
            picker = this.getPicker();

            // If we are collapsing an edge picker, we must not leave it as the default
            // edge swipe menu for that side. It must only be shown by the trigger (or
            // touch-tapping the unfocused field)
            if (this.pickerType === 'edge') {
                Ext['Viewport'].removeMenu(picker.getSide(), true);
            } else {
                picker.hide();
            }
        }
    },

    /**
     * @private
     * Runs on touchstart of doc to check to see if we should collapse the picker.
     */
    collapseIf: function (e) {
        var me = this;

        // If what was mousedowned on is outside of this Field, then collapse.
        if (!me.destroyed && (!e.within(me.bodyElement, false, true) && !me.owns(e.target))) {

            // If they have clicked on a focusable, we will let the default browser behaviour
            // take its course.
            // If they clicked on non-focusable content, then do not blur the input field, but
            // allow automatic focus reversion to jump safely back into the field.
            // TODO: wtf?
            // if (!Ext.fly(e.target).isFocusable()) {
            //     // Don't blur the input field
            //     e.preventDefault();
            // }
            me.collapse();
        }
    },

    showPicker: function () {
        var me = this,
            alignTarget = me[me.alignTarget],
            picker = me.getPicker(),
            value;

        // TODO: what if virtual keyboard is present

        if (me.pickerType === 'floated') {
            if (me.getMatchFieldWidth()) {
                picker.setWidth(alignTarget.getWidth());
            }
            picker.showBy(alignTarget, me.getFloatedPickerAlign(), {
                minHeight: 100
            });

            // Collapse on touch outside this component tree.
            // Because touch platforms do not focus document.body on touch
            // so no focusleave would occur to trigger a collapse.
            me.touchListeners = Ext.getDoc().on({
                // Do not translate on non-touch platforms.
                // mousedown will blur the field.
                translate: false,
                touchstart: me.collapseIf,
                scope: me,
                delegated: false,
                destroyable: true
            });
        } else {
            picker.show();
            me.setShowPickerValue(picker);
        }
    },

    updatePickerValue: function (picker, value) {
        var slot = picker.getSlots()[0],
            name = slot.name || slot.getName(),
            pickerValue = {};

        pickerValue[name] = value;

        picker.setValue(pickerValue);
    },

    onPickerShow: function () {
        var me = this;

        me.expanded = true;

        // If there's an edge picker encroaching, then ensure this field is still visible.
        if (me.pickerType === 'edge') {
            me.el.dom.scrollIntoView();
        }

        // We have to explicitly hide on any pointer event outside the field's component tree
        // relying on focus is not enough because you can mousedown on a window header and
        // drag it, and the default will be prevented.
        // Scrolling of anything which causes this field to move should collapse
        me.hideEventListeners = Ext.on({
            mousedown: 'collapseIf',
            scroll: 'onGlobalScroll',
            scope: me,
            destroyable: true
        });

        me.fireEvent('expand', me);
    },

    onPickerHide: function () {
        var me = this;

        me.expanded = false;
        Ext.destroy(me.hideEventListeners, me.touchListeners);
        me.fireEvent('collapse', me);
    },

    doDestroy: function () {
        this.destroyMembers('picker', 'hideEventListeners', 'touchListeners', 'focusTrap');
        this.callParent();
    },

    privates: {
        onGlobalHide: function(cmp) {
            // hide picker if ancestor is hidden
            if (this === cmp || cmp.isAncestor(this)) {
                this.collapse();
            }
        },

        onGlobalScroll: function (scroller, x, y) {
            var me = this,
                scrollingEl = scroller.getElement();

            if (me.expanded) {

                // Edge pickers are modal and anchored. We do not care if other
                // parts of the app scroll.
                if (me.pickerType === 'edge') {
                    return;
                }

                // Collapse if the scroll is anywhere but inside the picker
                // Also ignore body element scrolling, that won't affect the alignment.
                if (!me.getPicker().owns(scrollingEl) && scrollingEl.dom !== document.body) {
                    me.collapse();
                }
            }
        },

        revertFocusTo: function (target) {
            if (Ext.isTouchMode()) {
                this.getFocusTrap().focus();
            } else {
                target.focus();
            }
        },

        setShowPickerValue: function(picker) {
            var value = this.getValue();

            if (value != null) {
                this.updatePickerValue(picker, value);
            }
        }
    }
});
