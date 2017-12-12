/**
 * The Editor class is used to provide inline editing for elements on the page. The editor
 * is backed by a {@link Ext.field.Field} that will be displayed to edit the underlying content.
 * The editor is a floating Component, when the editor is shown it is automatically aligned to
 * display over the top of the bound element it is editing. The Editor contains several options
 * for how to handle key presses:
 *
 * - {@link #completeOnEnter}
 * - {@link #cancelOnEsc}
 * - {@link #swallowKeys}
 *
 * It also has options for how to use the value once the editor has been activated:
 *
 * - {@link #revertInvalid}
 * - {@link #ignoreNoChange}
 * - {@link #updateEl}
 *
 * Sample usage:
 *
 *     @example
 *     var form = Ext.create('Ext.form.Panel', {
 *         renderTo: Ext.getBody(),
 *         width: 380,
 *         height: 400,
 *         title: 'User Details',
 *         bodyPadding: 10,
 *         items: [{
 *             html: 'Double-Click on the header title, this, or the field label to edit',
 *             height:30
 *         },{
 *             label: 'First Name',
 *             name: 'firstname',
 *             xtype: 'textfield'
 *         }]
 *     });
 *
 *     var editor = new Ext.Editor({
 *         // update the innerHTML of the bound element 
 *         // when editing completes
 *         updateEl: true,
 *         alignment: 'l-l',
 *         autoSize: {
 *             width: 'boundEl'
 *         },
 *         field: {
 *             xtype: 'textfield'
 *         }
 *     });
 *
 *     form.header.getTitle().textEl.on('dblclick', function(e, t) {
 *         editor.startEdit(t);
 *     });
 *
 *     form.getTargetEl().on('dblclick', function(e, t) {
 *         editor.startEdit(t);
 *         // Manually focus, since clicking on the label will focus the text field
 *         editor.getField().focus(50, true);
 *     });
 *
 * {@img Ext.Editor/Ext.Editor.png Ext.Editor component}
 *
 */
Ext.define('Ext.Editor', {
    extend: 'Ext.Container',
    xtype: 'editor',

    isEditor: true,

    config: {
        /**
         * @cfg {Object} field
         * Config object for a {@link Ext.field.Field}
         */
        field: {
            xtype: 'textfield'
        }
    },

    floated: true,

    /**
     * @cfg {Boolean} allowBlur
     * True to {@link #completeEdit complete the editing process} if in edit mode when focus exits from this Editor's hierarchy.
     */
    allowBlur: true,

    /**
     * @cfg {Boolean} revertInvalid
     * True to automatically revert the field value and cancel the edit when the user completes an edit and the field
     * validation fails
     */
    revertInvalid: true,

    /**
     * @cfg {Boolean} [ignoreNoChange=false]
     * True to skip the edit completion process (no save, no events fired) if the user completes an edit and
     * the value has not changed.  Applies only to string values - edits for other data types
     * will never be ignored.
     */

    /**
     * @cfg {Boolean} [hideEl=true]
     * False to keep the bound element visible while the editor is displayed
     */
    hideEl: true,

    /**
     * @cfg {Object} value
     * The data value of the underlying field
     */
    value : '',

    /**
     * @cfg {String} [alignment=c-c]
     * The position to align to (see {@link Ext.util.Positionable#alignTo} for more details).
     */
    alignment: 'c-c?',

    /**
     * @cfg {Number[]} offset
     * The offset to use when aligning (see {@link Ext.util.Positionable#alignTo} for more details.
     */
    offset: [0, 0],

    /**
     * @cfg {Boolean/String} shadow
     * "sides" for sides/bottom only, "frame" for 4-way shadow, and "drop" for bottom-right shadow.
     */
    shadow : 'frame',

    /**
     * @cfg {Boolean} constrain
     * True to constrain the editor to the viewport
     */
    constrain : false,

    /**
     * @cfg {Boolean} swallowKeys
     * Handle the keydown/keypress events so they don't propagate
     */
    swallowKeys : true,

    /**
     * @cfg {Boolean} completeOnEnter
     * True to complete the edit when the enter key is pressed.
     */
    completeOnEnter : true,

    /**
     * @cfg {Boolean} cancelOnEsc
     * True to cancel the edit when the escape key is pressed.
     */
    cancelOnEsc : true,

    /**
     * @cfg {Boolean} cancelOnClear
     * True to cancel edit when the clear icon of a field is pressed
     */
    cancelOnClear: false,

    /**
     * @cfg {Boolean} updateEl
     * True to update the innerHTML of the bound element when the update completes
     */
    updateEl : false,

    // Do not participate in the ZIndexManager's focus switching operations.
    // When an editor is hidden, the ZIndexManager will not automatically activate
    // the last visible floater on the stack.
    focusOnToFront: false,

    /**
     * @cfg {String/HTMLElement/Ext.dom.Element} [parentEl=document.body]
     * An element to render to.
     */

    baseCls: Ext.baseCSSPrefix + 'editor',

    /**
     * @property {Boolean} editing
     * True if this editor is currently active.
     * @readonly
     */
    editing: false,

    /**
     * @event beforestartedit
     * Fires when editing is initiated, but before the value changes.  Editing can be canceled by returning
     * false from the handler of this event.
     * @param {Ext.Editor} this
     * @param {Ext.dom.Element} boundEl The underlying element bound to this editor
     * @param {Object} value The field value being set
     */

    /**
     * @event startedit
     * Fires when this editor is displayed
     * @param {Ext.Editor} this
     * @param {Ext.dom.Element} boundEl The underlying element bound to this editor
     * @param {Object} value The starting field value
     */

    /**
     * @event beforecomplete
     * Fires after a change has been made to the field, but before the change is reflected in the underlying
     * field.  Saving the change to the field can be canceled by returning false from the handler of this event.
     * Note that if the value has not changed and ignoreNoChange = true, the editing will still end but this
     * event will not fire since no edit actually occurred.
     * @param {Ext.Editor} this
     * @param {Object} value The current field value
     * @param {Object} startValue The original field value
     */

    /**
     * @event complete
     * Fires after editing is complete and any changed value has been written to the underlying field.
     * @param {Ext.Editor} this
     * @param {Object} value The current field value
     * @param {Object} startValue The original field value
     */

    /**
     * @event canceledit
     * Fires after editing has been canceled and the editor's value has been reset.
     * @param {Ext.Editor} this
     * @param {Object} value The user-entered field value that was discarded
     * @param {Object} startValue The original field value that was set back into the editor after cancel
     */

    /**
     * @event specialkey
     * Fires when any key related to navigation (arrows, tab, enter, esc, etc.) is pressed.  You can check
     * {@link Ext.event.Event#getKey} to determine which key was pressed.
     * @param {Ext.Editor} this
     * @param {Ext.form.field.Field} field The field attached to this editor
     * @param {Ext.event.Event} event The event object
     */

    preventDefaultAlign: true,
    useBoundValue: true,
    specialKeyDelay: 1,

    /**
     * @cfg {Boolean} matchFont
     * Determines if the editor input should match the font style of the target element
     */
    matchFont: false,

    applyField: function (config) {
        return Ext.widget(config);
    },

    updateField: function(newField, oldField) {
        var me = this, inputEl;

        if (oldField) {
            me.remove(oldField, true);
            oldField.un({
                specialkey: 'onSpecialKey',
                clearicontap: 'onFieldClear',
                scope: this
            });

            me._fieldSwallower = Ext.destroy(me._fieldSwallower);
        }

        if (newField) {
            inputEl = newField.inputElement;
            me.add(newField);
            newField.on({
                specialkey: 'onSpecialKey',
                clearicontap: 'onFieldClear',
                scope: this
            });

            if (me.swallowKeys) {
                me._fieldSwallower = inputEl.swallowEvent([
                    'keypress', // *** Opera
                    'keydown'   // *** all other browsers
                ]);
            }
        }
    },

    onAdded: function (container) {
        // Editors are floaters and shouldn't have an ownerCt, so use ownerCmp as
        // the upward link.
        this.ownerCmp = container;
        this.callParent(arguments);
    },

    /**
     * @private
     */
    onSpecialKey: function(field, event) {
        var me = this,
            key = event.getKey(),
            complete = me.completeOnEnter && key === event.ENTER,
            cancel = me.cancelOnEsc && key === event.ESC,
            task = me.specialKeyTask;

        if (!event.fromBoundList && (complete || cancel)) {
            event.stopEvent();
            if (!task) {
                me.specialKeyTask = task = new Ext.util.DelayedTask();
            }
            // Must defer this slightly to prevent exiting edit mode before the field's own
            // key nav can handle the enter key, e.g. selecting an item in a combobox list
            task.delay(me.specialKeyDelay, complete ? me.completeEdit : me.cancelEdit, me);
            //<debug>
            // Makes unit testing easier
            if (me.specialKeyDelay === 0) {
                task.cancel();
                if (complete) {
                    me.completeEdit();
                } else {
                    me.cancelEdit();
                }
            }
            //</debug>
        }

        me.fireEvent('specialkey', me, field, event);
    },

    /**
     * Starts the editing process and shows the editor.
     * @param {String/HTMLElement/Ext.dom.Element} el The element to edit
     * @param {String} value (optional) A value to initialize the editor with. If a value is not provided, it defaults
     * to the innerHTML of el.
     * @param doFocus (private)
     */
    startEdit: function(el, value, doFocus) {
        var me = this,
            field = me.getField(),
            dom, font;

        if (!this.allowBlur && this.editing) {
            me.toggleBoundEl(true);
        }

        me.completeEdit(true);
        me.boundEl = el = Ext.get(el);
        dom = me.boundEl.dom;

        if (me.useBoundValue && !Ext.isDefined(value)) {
            value = Ext.String.trim(dom.textContent || dom.innerText || dom.innerHTML);
        }

        if (me.fireEvent('beforestartedit', me, el, value) !== false) {
            if (me.context) {
                // Grab the value again, may have changed in beforestartedit
                value = me.context.value;
            }

            if (this.matchFont) {
                font = el.getStyle('font');

                if (!font) {
                    font = el.getStyle('fontWeight') + ' ' + el.getStyle('fontSize') + '/' + el.getStyle('lineHeight') + ' ' + el.getStyle('fontFamily');
                }

                field.inputElement.setStyle('font', font);
            }

            me.startValue = value;
            me.show();

            // forces editor to use absolute position for proper alignment
            if (!me.getFloated()) {
                me.setTop(0);
            }
            me.realign();

            // temporarily suspend events on field to prevent the "change" event from firing when resetOriginalValue() and setValue() are called
            field.suspendEvents();
            field.setValue(value);
            field.resetOriginalValue();
            field.resumeEvents();

            if (doFocus !== false) {
                field.focus(field.selectOnFocus ? true : [Number.MAX_VALUE]);
            }

            me.toggleBoundEl(false);
            me.editing = true;
        }
    },

    /**
     * Realigns the editor to the bound field based on the current alignment config value.
     */
    realign: function() {
        var me = this;

        // Editors must not be affected by the Ext.Widget.floatInset setting, so calculate
        // a constraining Region.
        me.setConstrainAlign(Ext.getBody().getConstrainRegion());
        me.alignTo(me.boundEl, me.alignment, {offset: me.offset});
    },

    /**
     * Ends the editing process, persists the changed value to the underlying field, and hides the editor.
     * @param {Boolean} [remainVisible=false] Override the default behavior and keep the editor visible after edit
     */
    completeEdit: function(remainVisible) {
        var me = this,
            field = me.getField(),
            startValue = me.startValue,
            cancel = me.context && me.context.cancel,
            value;

        if (!me.editing) {
            return;
        }

        value = me.getValue();

        // Check isValid first to check any explicitly set errors, then fallback to re-validation
        // this will prevent the possible overwriting of server set errors
        if (!field.isValid() || !field.validate()) {
            if (me.revertInvalid !== false) {
                me.cancelEdit(remainVisible);
            }
            return;
        }

        if (me.ignoreNoChange && !field.didValueChange(value, startValue)) {
            me.onEditComplete(remainVisible);
            return;
        }

        if (me.fireEvent('beforecomplete', me, value, startValue) !== false) {
            // Grab the value again, may have changed in beforecomplete
            value = me.getValue();
            if (me.updateEl && me.boundEl) {
                me.boundEl.setHtml(value);
            }
            me.onEditComplete(remainVisible, cancel);
            me.fireEvent('complete', me, value, startValue);
        }
    },

    afterShow: function() {
        var me = this;

        me.callParent(arguments);
        me.fireEvent('startedit', me, me.boundEl, me.startValue);
    },

    onFieldClear: function() {
        if (this.cancelOnClear) {
            this.cancelEdit();
        }
    },

    /**
     * Cancels the editing process and hides the editor without persisting any changes.  The field value will be
     * reverted to the original starting value.
     * @param {Boolean} [remainVisible=false] Override the default behavior and keep the editor visible after cancel
     */
    cancelEdit: function(remainVisible) {
        var me = this,
            startValue = me.startValue,
            field = me.getField(),
            value;

        if (me.editing) {
            if (field) {
                value = me.editedValue = me.getValue();
                // temporarily suspend events on field to prevent the "change" event from firing when setValue() is called
                field.suspendEvents();
                me.setValue(startValue);
                field.resumeEvents();
            }
            me.onEditComplete(remainVisible, true);
            me.fireEvent('canceledit', me, value, startValue);
            delete me.editedValue;
        }
    },

    /**
     * @private
     */
    onEditComplete: function(remainVisible, cancelling) {
        var me = this,
            field = me.getField();

        me.editing = false;
        if (remainVisible !== true) {
            me.hide();
            me.toggleBoundEl(true);
        }

        field.inputElement.setStyle('font', null);
    },

    onFocusLeave: function(e) {
        var me = this;

        if (me.allowBlur === true && me.editing) {
            me.completeEdit();
        }
        me.callParent([e]);
    },

    updateHidden: function(hidden, oldHidden) {
        var me = this,
            field;

        if (hidden && !me.destroying) {
            field = me.getField();
            if (me.editing) {
                me.completeEdit();
            } else if (field.collapse) {
                field.collapse();
            }
        }
        me.callParent([hidden, oldHidden]);
    },

    /**
     * Gets the data value of the editor
     * @return {Object} The data value
     */
    getValue: function() {
        var field = this.getField();
        return field.getValue();
    },

    /**
     * Sets the data value of the editor
     * @param {Object} value Any valid value supported by the underlying field
     */
    setValue: function(value) {
        var field = this.getField();
        field.setValue(value);
    },

    toggleBoundEl: function(visible) {
        if (this.hideEl) {
            this.boundEl.setVisibilityMode(Ext.Element.VISIBILITY);
            this.boundEl.setVisible(visible);
        }
    },

    doDestroy: function() {
        var me = this,
            task = me.specialKeyTask;

        if (task) {
            task.cancel();
        }

        me.callParent();
    }
});
