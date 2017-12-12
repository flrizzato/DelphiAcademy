/**
 * The text field is the basis for most of the input fields. It provides a baseline of shared
 * functionality such as input validation, standard events, state management and look and feel. Typically we create
 * text fields inside a form, like this:
 *
 *     @example
 *     Ext.create('Ext.form.Panel', {
 *         fullscreen: true,
 *         items: [
 *             {
 *                 xtype: 'fieldset',
 *                 title: 'Enter your name',
 *                 items: [
 *                     {
 *                         xtype: 'textfield',
 *                         label: 'First Name',
 *                         name: 'firstName'
 *                     },
 *                     {
 *                         xtype: 'textfield',
 *                         label: 'Last Name',
 *                         name: 'lastName'
 *                     }
 *                 ]
 *             }
 *         ]
 *     });
 *
 * This creates two text fields inside a form. Text Fields can also be created outside of a Form, like this:
 *
 *     Ext.create('Ext.field.Text', {
 *         label: 'Your Name',
 *         value: 'Ed Spencer'
 *     });
 *
 * ## Configuring
 *
 * Text field offers several configuration options, including {@link #placeholder}, {@link #maxLength},
 * {@link #autoComplete}, {@link #autoCapitalize} and {@link #autoCorrect}. For example, here is how we would configure
 * a text field to have a maximum length of 10 characters, with placeholder text that disappears when the field is
 * focused:
 *
 *     Ext.create('Ext.field.Text', {
 *         label: 'Username',
 *         maxLength: 10,
 *         placeholder: 'Enter your username'
 *     });
 *
 * The autoComplete, autoCapitalize and autoCorrect configs simply set those attributes on the text field and allow the
 * native browser to provide those capabilities. For example, to enable auto complete and auto correct, simply
 * configure your text field like this:
 *
 *     Ext.create('Ext.field.Text', {
 *         label: 'Username',
 *         autoComplete: true,
 *         autoCorrect: true
 *     });
 *
 * These configurations will be picked up by the native browser, which will enable the options at the OS level.
 *
 * Text field inherits from {@link Ext.field.Field}, which is the base class for all fields and provides
 * a lot of shared functionality for all fields, including setting values, clearing and basic validation. See the
 * {@link Ext.field.Field} documentation to see how to leverage its capabilities.
 */
Ext.define('Ext.field.Text', {
    extend: 'Ext.field.Input',
    xtype: 'textfield',
    alternateClassName: 'Ext.form.Text',

    requires: [
        'Ext.field.trigger.Clear',
        'Ext.Deferred'
    ],

    /**
     * @event focus
     * Fires when this field receives input focus
     * @param {Ext.field.Text} this This field
     * @param {Ext.event.Event} e
     */

    /**
     * @event blur
     * Fires when this field loses input focus
     * @param {Ext.field.Text} this This field
     * @param {Ext.event.Event} e
     */

    /**
     * @event paste
     * Fires when this field is pasted.
     * @param {Ext.field.Text} this This field
     * @param {Ext.event.Event} e
     */

    /**
     * @event mousedown
     * Fires when this field receives a mousedown
     * @param {Ext.field.Text} this This field
     * @param {Ext.event.Event} e
     */

    /**
     * @event keyup
     * @preventable
     * Fires when a key is released on the input element
     * @param {Ext.field.Text} this This field
     * @param {Ext.event.Event} e
     */

    /**
     * @event clearicontap
     * @preventable
     * Fires when the clear icon is tapped
     * @param {Ext.field.Text} this This field
     * @param {Ext.field.Input} input The field's input component.
     * @param {Ext.event.Event} e
     */

    /**
     * @event action
     * @preventable
     * Fires whenever the return key or go is pressed. FormPanel listeners
     * for this event, and submits itself whenever it fires. Also note
     * that this event bubbles up to parent containers.
     * @param {Ext.field.Text} this This field
     * @param {Mixed} e The key event object
     */

    config: {
        /**
         * @cfg {Boolean} clearable
         * `true` to show a clear trigger in this field when it has a non-empty value
         */
        clearable: true,

        /**
         * @cfg labelAlign
         * When value is `'placeholder'`, the label text will be rendered as placeholder
         * text inside the empty input and will animated to "top" alignment when the input
         * is focused or contains text.
         * @inheritdoc
         */

        /**
         * @cfg {String} placeholder
         * A string value displayed in the input when the control is empty.
         */
        placeholder: null,

        /**
         * @cfg {Number} maxLength
         * The maximum number of permitted input characters.
         */
        maxLength: null,

        /**
         * @cfg {Boolean} [autoComplete=true]
         * `false to disable autocomplete on this text field.  Autocomplete is enabled by
         * default on text fields, but disabled on picker fields.
         */
        autoComplete: null,

        /**
         * @cfg {Boolean} autoCapitalize
         * True to set the field's DOM element autocapitalize attribute to "on", false to set to "off".
         */
        autoCapitalize: null,

        /**
         * @cfg {Boolean} autoCorrect
         * True to set the field DOM element autocorrect attribute to "on", false to set to "off".
         */
        autoCorrect: null,

        /**
         * @cfg {Boolean} [autoHideInputMask=true]
         * Specify as `false` to always show the `inputMask`.
         * @since 6.5.0
         */
        autoHideInputMask: null,

        /**
         * @cfg {String/Ext.field.InputMask} inputMask
         *
         * **Important:** To use this config you must require `Ext.field.InputMask` or
         * use a complete framework build. The logic to implement an `inputMask` is not
         * automatically included in a build.
         * @since 6.5.0
         */
        inputMask: null,

        /**
         * @cfg {String} pattern
         * The value for the HTML5 `pattern` attribute. You can use this to change which
         * keyboard layout will be used.
         *
         *     Ext.define('Ux.field.Pattern', {
         *         extend : 'Ext.field.Text',
         *         xtype  : 'patternfield',
         *
         *         config : {
         *             component : {
         *                 pattern : '[0-9]*'
         *             }
         *         }
         *     });
         *
         * Even though it extends {@link Ext.field.Text}, it will display the number keyboard.
         */
        pattern: null,

        // @cmd-auto-dependency {aliasPrefix: "trigger.", isKeyedObject: true}
        /**
         * @cfg {Object} triggers
         * {@link Ext.field.trigger.Trigger Triggers} to use in this field.  The keys in
         * this object are unique identifiers for the triggers. The values in this object
         * are {@link Ext.field.trigger.Trigger Trigger} configuration objects.
         *
         *     Ext.create('Ext.field.Text', {
         *         label: 'My Custom Field',
         *         triggers: {
         *             foo: {
         *                 cls: 'my-foo-trigger',
         *                 handler: function() {
         *                     console.log('foo trigger clicked');
         *                 }
         *             },
         *             bar: {
         *                 cls: 'my-bar-trigger',
         *                 handler: function() {
         *                     console.log('bar trigger clicked');
         *                 }
         *             }
         *         }
         *     });
         *
         * The weight value may be a negative value in order to position custom triggers
         * ahead of default triggers like that of a DatePicker field.
         *
         *     Ext.create('Ext.form.DatePicker', {
         *         label: 'Pick a Date',
         *         triggers: {
         *             foo: {
         *                 cls: 'my-foo-trigger',
         *                 weight: -2, // negative to place before default triggers
         *                 handler: function() {
         *                     console.log('foo trigger clicked');
         *                 }
         *             },
         *             bar: {
         *                 cls: 'my-bar-trigger',
         *                 weight: -1,
         *                 handler: function() {
         *                     console.log('bar trigger clicked');
         *                 }
         *             }
         *         }
         *     });
         */
        triggers: {
            clear: {
                type: 'clear'
            }
        },

        /**
         * @cfg {Boolean} editable
         * Configure as `false` to prevent the user from typing text directly into the
         * field; the field can only have its value set programmatically or via an action
         * invoked by a trigger.
         *
         * Contrast with {@link #cfg!readOnly} which disables all mutation via the UI.
         */
        editable: true,

        bubbleEvents: ['action'],

        /**
         * @cfg bodyAlign
         * @hide
         */
        bodyAlign: 'stretch',

        /**
         * @private
         */
        labelInPlaceholder: {
            lazy: true,
            $value: true
        },

        /**
         * @cfg {'left'/'center'/'right'} [textAlign='left']
         * The text alignment of this field.
         */
        textAlign: null
    },

    cachedConfig: {
        /**
         * @cfg {Boolean} animateUnderline
         * 'true' to animate the underline of a field when focused
         */
        animateUnderline: false,

        /**
         * @cfg {Ext.data.validator.Validator} parseValidator
         * @private
         * @since 6.5.1
         */
        parseValidator: null
    },

    /**
     * @cfg {String} badFormatMessage
     * The error message that will be displayed if the value cannot be parsed (for some
     * derived types) or if the value does not match a configured {@link #inputMask}.
     * @locale
     * @since 6.5.0
     */
    badFormatMessage: 'Value does not match the required format',

    /**
     * @cfg bodyAlign
     * @hide
     */

    /**
     * @property defaultBindProperty
     * @inheritdoc
     */
    defaultBindProperty: 'value',

    /**
     * @cfg twoWayBindable
     * @inheritdoc
     */
    twoWayBindable: {
        value: 1
    },

    /**
     * @cfg publishes
     * @inheritdoc
     */
    publishes: {
        value: 1
    },

    /**
     * @cfg inputType
     * @inheritdoc
     */
    inputType: 'text',

    /**
     * @property classCls
     * @inheritdoc
     */
    classCls: Ext.baseCSSPrefix + 'textfield',
    focusedCls: Ext.baseCSSPrefix + 'focused',
    emptyCls: Ext.baseCSSPrefix + 'empty',
    webkitBorderBoxBugCls: Ext.baseCSSPrefix + 'webkit-border-box-bug',

    requiredIndicator: '*',

    getBodyTemplate: function () {
        var template = [{
            reference: 'beforeInputElement',
            cls: Ext.baseCSSPrefix + 'before-input-el'
        }];

        template.push(this.getInputTemplate());

        template.push({
            reference: 'afterInputElement',
            cls: Ext.baseCSSPrefix + 'after-input-el'
        });

        return [{
            reference: 'inputWrapElement',
            cls: Ext.baseCSSPrefix + 'input-wrap-el' +
                (Ext.supports.WebKitInputTableBoxModelBug ? (' ' + this.webkitBorderBoxBugCls) : ''),
            children: template
        }, {
            reference: 'underlineElement',
            cls: Ext.baseCSSPrefix + 'underline-el'
        }];
    },

    initialize: function () {
        var me = this;

        if (Ext.isRobot) {
            me.focusedInputDelay = 0;
        }

        me.callParent();

        me.inputElement.on({
            keyup: 'onKeyUp',
            keydown: 'onKeyDown',
            keypress: 'onKeyPress',
            paste: 'onPaste',
            mousedown: 'onMouseDown',
            scope: me
        });

        me.syncEmptyState();
    },

    /**
     * Clears the value of this field.
     */
    clearValue: function () {
        var me = this,
            inputMask = me.getInputMask();

        if (inputMask) {
            // show empty mask and move caret to first editable position
            inputMask.showEmptyMask(me, true);
        } else {
            me.forceInputChange = true;
            me.setValue('');
            me.forceInputChange = false;
        }

        me.syncEmptyState();
    },

    transformValue: function (value) {
        if (value == null) {
            value = '';
        }

        return value;
    },

    applyInputMask: function (value, instance) {
        var InputMask = Ext.field['InputMask']; // prevent Cmd detection

        //<debug>
        if (value) {
            if (!InputMask) {
                Ext.raise('Missing Ext.field.InputMask (required to use inputMask)');
            }
        }
        //</debug>

        return value ? InputMask.from(value, instance) : null;
    },

    updateInputMask: function (inputMask, previous) {
        this.hasMask = false;
        if (previous) {
            previous.release();
        }

        if (inputMask) {
            this.hasMask = true;
            //Synchronize pattern in case we have changed it and ensure that initial mask is being shown
            inputMask.syncPattern(this);
        }
    },

    doValidate: function (value, errors, skipLazy) {
        this.callParent([ value, errors, skipLazy ]);

        if (!skipLazy) {
            var inputMask = this.getInputMask();

            // Field will be marked invalid if user has entered some chars.
            if (inputMask && !inputMask.isFilled(value) && value !== inputMask._mask) {
                errors.push(this.badFormatMessage);
            }
        }
    },

    /**
     * Parses the given `value` and returns it in the desired representation. By default
     * this will return the `value` given (no change). Derived classes (such as `datefield`
     * and `numberfield`) will override this method and return a Date or a Number,
     * respectively.
     *
     * If `value` cannot be parsed, this method will return `null`, otherwise it will
     * return the parsed value. It is the parsed value that will become the field's
     * {@link #cfg!value value}.
     *
     * This method is not called directly but is called internally by the
     * {@link #method!validate validate method}. This call to parse a value is not made on
     * empty or null values.
     *
     * *Note:* It is not expected that applications will need to override this method
     * because an application can achieve value parsing by virtue of the `validators` it
     * defines. These can come from this component or from a bound model field (using
     * {@link #cfg!modelValidation}). In addition, `datefield` and `numberfield` both
     * provide default parsing for their values.
     *
     * @param {String} value The value to parse (never `null`).
     *
     * @param {String[]} errors The set of validation errors. If the value cannot be
     * parsed, the error message should be added to this array.
     *
     * @return {Mixed} The parsed value.
     *
     * @template
     * @protected
     * @since 6.5.1
     */
    parseValue: function (value, errors) {
        var me = this,
            parser = me.getParseValidator(),
            field, i, k, len, v, validators;

        if (parser) {
            // If the derived class has specified a default parseValidator, then this
            // pass is needed. Consult the component's validators first for a more
            // specific validator, followed by the bound model field (if we have one).
            field = me._validationField;

            for (k = 2; k-- > 0; ) {
                validators = k ? me.getValidators() : (field && field.getValidators());
                len = validators && validators.length;

                for (i = 0; i < len; ++i) {
                    v = validators[i];

                    if (v.parse) {
                        v = v.parse(value);

                        // The first parse validator to achieve a parse wins. Returns
                        // its result.
                        if (v !== null) {
                            return v;
                        }
                    }
                }
            }

            // No user-defined parse validator found, so run the default one. It must
            // succeed or the value is invalid.
            value = parser.parse(value);

            if (value === null && errors) {
                errors.push(me.badFormatMessage);
            }
        }

        return value;
    },

    applyValue: function (value, oldValue) {
        // This converts the raw, textual value into whatever form the field uses
        // So Number and Date subclasses convert to number or date here.
        // If the validation fails, undefined return will abort the setter.
        if (value && typeof value === 'string') {
            value = this.parseValue(value);
            if (value === null) {
                return;
            }
        }

        return this.transformValue(this.callParent([value, oldValue]));
    },

    updateInputValue: function (value, oldValue) {
        var me = this,
            inputMask = me.getInputMask();

        me.callParent([value, oldValue]);

        me.syncEmptyState();
        me.syncLabelPlaceholder(false);

        if (inputMask) {
            inputMask.onChange(this, value, oldValue);
        }
    },

    updateTextAlign: function (newAlign, oldAlign) {
        var element = this.element;

        if (oldAlign) {
            element.removeCls(Ext.baseCSSPrefix + 'text-align-' + oldAlign);
        }

        if (newAlign) {
            element.addCls(Ext.baseCSSPrefix + 'text-align-' + newAlign);
        }
    },

    updatePlaceholder: function (value) {
        this.setInputAttribute('placeholder', value);
    },

    //<debug>
    applyMaxLength: function (maxLength) {
        if (maxLength !== null && typeof maxLength != 'number') {
            throw new Error("Ext.field.Text: [applyMaxLength] trying to pass a value which is not a number");
        }
        return maxLength;
    },
    //</debug>

    updateMaxLength: function (newMaxLength) {
        this.setInputAttribute('maxlength', newMaxLength);
    },

    applyAutoComplete: function (value) {
        return value === true || value === 'on';
    },

    updateAutoComplete: function (value) {
        this.setInputAttribute('autocomplete', value ? 'on' : 'off');
    },

    applyAutoCapitalize: function (value) {
        return value === true || value === 'on';
    },

    updateAutoCapitalize: function (value) {
        this.setInputAttribute('autocapitalize', value ? 'on' : 'off');
    },

    applyAutoCorrect: function (value) {
        return value === true || value === 'on';
    },

    updateAutoCorrect: function (value) {
        this.setInputAttribute('autocorrect', value ? 'on' : 'off');
    },

    updateReadOnly: function (newReadOnly) {
        var me = this,
            triggers = me.getTriggers(),
            isEditable = me.getEditable(),
            triggerName, trigger;

        me.callParent([newReadOnly || !isEditable]);

        for (triggerName in triggers) {
            trigger = triggers[triggerName];

            if (trigger.disableOnReadOnly !== false) {
                trigger.setDisabled(newReadOnly);
            }
        }

        me.syncEmptyState();
    },

    updateEditable: function (newEditable) {
        var triggers = this.getTriggers(),
            isReadOnly = this.getReadOnly(),
            triggerName, trigger;

        this.updateReadOnly(!newEditable);
        for (triggerName in triggers) {
            trigger = triggers[triggerName];

            if (trigger.disableOnReadOnly !== false) {
                triggers[triggerName].setDisabled(isReadOnly);
            }
        }
    },

    updatePattern: function (pattern) {
        this.setInputAttribute('pattern', pattern);
    },

    updateDisabled: function (disabled, oldDisabled) {
        this.callParent([disabled, oldDisabled]);
        this.syncEmptyState();
    },

    updateClearable: function (clearable, oldClearable) {
        var me = this,
            triggers, clearTrigger;

        if (!me.isConfiguring) {
            triggers = me.getTriggers();
            clearTrigger = triggers && triggers.clear;

            if (clearable) {
                if (!clearTrigger) {
                    me.addTrigger('clear', 'clear');
                }
            } else if (clearTrigger) {
                me.removeTrigger('clear');
            }
        }
    },

    applyTriggers: function (triggers, oldTriggers) {
        var me = this,
            instances = oldTriggers || {},
            clearable = me.getClearable(),
            name, trigger, oldTrigger;

        for (name in triggers) {
            trigger = triggers[name];
            oldTrigger = instances[name];

            // Any key that exists on the incoming object should cause destruction of
            // the existing trigger for that key, if one exists.  This is true for both
            // truthy values (triggers and trigger configs) and falsy values. Falsy values
            // cause destruction of the existing trigger without replacement.
            if (oldTrigger) {
                oldTrigger.destroy();
            }

            if (trigger) {
                if (!clearable && (name === 'clear')) {
                    continue;
                }

                instances[name] = me.createTrigger(name, trigger);
            }
        }

        return instances;
    },

    updateTriggers: function () {
        this.syncTriggers();
    },

    /**
     * Adds a trigger to this text field.
     * @param {String} name Unique name (within this field) for the trigger.  Cannot be the
     * same as the name of an existing trigger for this field.
     * @param {Ext.field.trigger.Trigger/Object} trigger The trigger instance or a config
     * object for a trigger to add
     * @return {Ext.field.trigger.Trigger} The trigger that was added
     */
    addTrigger: function (name, trigger) {
        var me = this,
            triggers = me.getTriggers(),
            triggerConfig;

        //<debug>
        if (triggers && triggers[name]) {
            Ext.raise('Trigger with name "' + name + '" already exists.');
        }
        if (typeof name !== 'string') {
            Ext.raise('Cannot add trigger. Key must be a string.');
        }
        if (typeof trigger !== 'string' && !Ext.isObject(trigger)) {
            Ext.raise('Cannot add trigger "' + name + '". A trigger config or instance is required.');
        }
        //</debug>

        trigger = me.createTrigger(name, trigger);

        if (triggers) {
            triggers[name] = trigger;
            me.syncTriggers();
        } else {
            triggerConfig = {};
            triggerConfig[name] = trigger;
            me.setTriggers(triggerConfig);
        }

        return trigger;
    },

    /**
     * Removes a trigger from this text field.
     * @param {String/Ext.field.trigger.Trigger} trigger The name of the trigger to remove,
     * or a trigger reference.
     * @param {Boolean} [destroy=true] False to prevent the trigger from being destroyed
     * on removal.  Only use this option if you want to reuse the trigger instance.
     * @return {Ext.field.trigger.Trigger} The trigger that was removed
     */
    removeTrigger: function (trigger, destroy) {
        var me = this,
            triggers = me.getTriggers(),
            name = trigger,
            triggerEl;

        if (name.isTrigger) {
            name = trigger.getName();
        } else {
            trigger = triggers[name];
        }

        //<debug>
        if (!name) {
            Ext.raise('Trigger not found.');
        } else if (!triggers[name]) {
            Ext.raise('Cannot remove trigger. Trigger with name "' + name + '" not found.');
        }
        //</debug>

        delete triggers[name];

        if (destroy !== false) {
            trigger.destroy();
        } else {
            triggerEl = trigger.el.dom;
            triggerEl.parentNode.removeChild(triggerEl);
        }

        this.syncTriggers();

        return trigger;
    },

    onKeyDown: function (event) {
        var me = this,
            inputMask = me.getInputMask();

        me.lastKeyTime = Date.now();
        if (inputMask) {
            inputMask.onKeyDown(me, me.getValue(), event);
        }

        // tell the class to ignore the input event. this happens when we want to listen
        // to the field change when the input autocompletes
        me.ignoreInput = true;

        if (Ext.supports.SpecialKeyDownRepeat) {
            me.fireKey(event);
        }
    },

    onInput: function (e) {
        var me = this,
            inputEl = me.inputElement.dom,
            value = inputEl.value,
            inputMask = me.getInputMask(),
            parseErrors, oldValue;

        if (inputMask) {
            inputMask.processAutocomplete(this, value);
            value = inputEl.value;
        }

        // Keep our config up to date:
        me._inputValue = value;

        // If the value is empty don't try and parse it, use the result
        // of parseValue as the default. For text fields it will be empty string,
        // for other typed fields (number/date) it will be null
        if (value) {
            parseErrors = [];
            value = me.parseValue(value, parseErrors);
        }
        if (parseErrors && parseErrors.length) {
            me.setError(parseErrors);
        } else {
            oldValue = me.getValue();
            me.setValue(value);

            // If the value did not change, revalidate.
            // The user may have just erased into a valid state from an invalid state.
            if (me.getValue() === oldValue) {
                me.validate();
            }
        }

        me.syncEmptyState();

        // if we should ignore input, stop now.
        if (me.ignoreInput) {
            me.ignoreInput = false;
            return;
        }

        // set a timeout for 10ms to check if we want to stop the input event.
        // if not, then continue with the event (keyup)
        Ext.defer(function () {
            if (!me.ignoreInput && !me.destroyed) {
                me.fireEvent('keyup', e);
                me.ignoreInput = false;
            }
        }, 10);
    },

    /**
     * @private
     */
    fireKey: function (e) {
        if (e.isSpecialKey()) {
            this.fireEvent('specialkey', this, e);
        }
    },

    onKeyPress: function (event) {
        var me = this,
            inputMask = me.getInputMask();

        if (inputMask) {
            inputMask.onKeyPress(me, me.getValue(), event);
        }

        me.fireEvent('keypress', me, event);

        if (!Ext.supports.SpecialKeyDownRepeat) {
            me.fireKey(event);
        }
    },

    onKeyUp: function (e) {
        this.fireAction('keyup', [this, e], 'doKeyUp');
    },

    /**
     * Called when a key has been pressed in the `<input>`
     * @private
     */
    doKeyUp: function (me, e) {
        me.syncEmptyState();

        if (e.browserEvent.keyCode === 13) {
            me.fireAction('action', [me, e], 'doAction');
        }
    },

    doAction: function () {
        // Blur fields on enter on virtual keyboard platforms.
        // The virtual keyboard pushes the document up outside the viewport bounds.
        if (document.documentElement.getBoundingClientRect().top < 0) {
            this.blur();
        }
    },

    onClearIconTap: function (input, e) {
        this.fireAction('clearicontap', [this, input, e], 'doClearIconTap');
    },

    /**
     * @private
     */
    doClearIconTap: function () {
        this.clearValue();
    },

    onFocusEnter: function (event) {
        var me = this,
            inputMask = me.getInputMask();

        me.callParent([event]);

        me.addCls(me.focusedCls);
        me.syncLabelPlaceholder(true);

        if (inputMask) {
            inputMask.onFocus(me, me.getValue());
        }
    },

    onFocusLeave: function (event) {
        var me = this,
            inputMask = me.getInputMask();

        me.callParent([event]);

        me.removeCls(me.focusedCls);
        me.syncLabelPlaceholder(true);

        if (inputMask) {
            inputMask.onBlur(me, me.getValue());
        }
    },

    onPaste: function (e) {
        this.forceInputChange = true;
        this.handlePaste(e);
        this.forceInputChange = false;
    },

    getCaretPos: function () {
        return this.inputElement.getCaretPos();
    },

    setCaretPos: function (pos) {
        this.inputElement.setCaretPos(pos);
    },

    /**
     * Returns the selection range of an input element as an array of three values:
     *
     *      [ start, end, direction ]
     *
     * These have the same meaning as the parameters to `select`.
     * @return {Array}
     * @since 6.5.0
     */
    getTextSelection: function () {
        return this.inputElement.getTextSelection();
    },

    /**
     * Select the specified contents of the input field (all by default).
     * @param {Number} [start=0]
     * @param {Number} [end]
     * @param {"f"/"b"/"forward"/"backward"} [direction="f"] Pass "f" for forward,
     * "b" for backwards.
     * @return {Ext.field.Text} this
     * @chainable
     */
    select: function (start, end, direction) {
        // Safari has a bug where selecting text in an input element focuses that
        // input element. If we do not contain focus, do nothing. We select on focus
        // anyway.
        if (this.containsFocus) {
            this.inputElement.selectText(start, end, direction);
        }

        return this;
    },

    reset: function () {
        this.callParent();

        this.syncEmptyState();
    },

    onClick: function (e) {
        this.fireEvent('click', e);
    },

    onMouseDown: function (e) {
        this.fireEvent('mousedown', e);
    },

    trimValueToMaxLength: function () {
        var me = this,
            maxLength = me.getMaxLength(),
            value = me.getValue();

        if (maxLength && value.length > maxLength) {
            me.setValue(value.slice(0, maxLength));
        }
    },

    doDestroy: function () {
        var me = this,
            triggers = me.getTriggers(),
            triggerGroups = me.triggerGroups,
            name, animation;

        animation = me.labelElement && me.labelElement.getActiveAnimation();

        if (animation) {
            animation.stop();
        }

        if (triggerGroups) {
            for (name in triggerGroups) {
                triggerGroups[name].destroy();
            }
            me.triggerGroups = null;
        }

        for (name in triggers) {
            triggers[name].destroy();
        }

        me.setTriggers(null);
        me.setInputMask(null);

        me.callParent();
    },

    onRender: function () {
        this.callParent();

        this.syncLabelPlaceholder();
    },

    getRefItems: function (deep) {
        var me = this,
            triggers = me.getTriggers(),
            items = [],
            triggerName, trigger;

        for (triggerName in triggers) {
            trigger = triggers[triggerName];
            items.push(trigger);

            // component triggers have ref items of their own
            if (deep && trigger.getRefItems) {
                items.push.apply(items, trigger.getRefItems(deep));
            }
        }

        return items;
    },

    privates: {
        focusedInputDelay: 300,
        forceInputChange: false,
        hasMask: false,
        lastKeyTime: 0,

        applyParseValidator: function (config) {
            return this.decodeValidator(config);
        },

        updateLabelInPlaceholder: function (inside) {
            var me = this,
                placeHolder = me.getPlaceholder() || '',
                anim = me._animPlaceholderLabel;

            if (me.getLabelAlign() !== 'placeholder' || !me.getLabel()) {
                me.clearWhenVisible('doPositionPlaceholder');
                me.setInputAttribute('placeholder', placeHolder);
                return;
            }

            me.whenVisible('doPositionPlaceholder', [inside, anim]);
            me.el.toggleCls(Ext.baseCSSPrefix + 'label-inside', inside);
        },

        updateAnimateUnderline: function (value) {
            this.toggleCls(Ext.baseCSSPrefix + 'animate-underline', value);
        },

        canSetInputValue: function () {
            var me = this;
            // If we're using an inputMask, the field is updated dynamically
            // as typing occurs. forceInputChange is for when the component wants
            // to force the value to change, for example selecting from a picker,
            // or after consuming a paste. If we are focused, make sure enough
            // of a delay has passed so that we're not overwriting the value
            // as the user is typing, which typically means the value will
            // have come from a setValue call elsewhere, as opposed to
            // from typing.
            return me.hasMask || me.forceInputChange || !me.hasFocus ||
                Date.now() - me.lastKeyTime > me.focusedInputDelay;
        },

        doPositionPlaceholder: function (inside, doAnimate) {
            var me = this,
                labelElement = me.labelElement,
                anim, animation, info, insideInfo, outsideInfo;

            animation = labelElement.getActiveAnimation();
            if (animation) {
                animation.stop();
            }

            info = me.lastPlaceholderAnimInfo;
            if (!info) {
                me.lastPlaceholderAnimInfo = info = me.getPlaceholderAnimInfo();
            }
            insideInfo = info.inside;
            outsideInfo = info.outside;

            anim = {
                from: inside ? outsideInfo : insideInfo,
                to: inside ? insideInfo : outsideInfo,
                preserveEndState: true,
                duration: 250,
                easing: 'ease-out'
            };

            if (doAnimate) {
                labelElement.animate(anim);
            } else {
                labelElement.setStyle(anim.to);
            }
        },

        getPlaceholderLabel: function () {
            var me = this,
                label = me.getLabel();

            if (label && me.getRequired()) {
                label += ' ' + me.requiredIndicator;
            }

            return label;
        },

        getPlaceholderAnimInfo: function () {
            var me = this,
                element = me.element,
                labelElement = me.labelElement,
                inputElement = me.inputElement,
                labelOffsets = labelElement.getOffsetsTo(element),
                inputOffsets = inputElement.getOffsetsTo(element),
                labelLeftPadding = labelElement.getPadding('l'),
                inputLeftPadding = inputElement.getPadding('l'),
                labelTopPadding = labelElement.getPadding('t'),
                inputTopPadding = inputElement.getPadding('t'),
                translateX = inputOffsets[0] - labelOffsets[0] + (inputLeftPadding - labelLeftPadding),
                translateY = inputOffsets[1] - labelOffsets[1] + (inputTopPadding - labelTopPadding);

            return {
                inside: {
                    transform: {
                        translateX: translateX + 'px',
                        translateY: translateY + 'px'
                    },
                    'font-size': inputElement.getStyle('font-size')
                },
                outside: {
                    transform: {
                        translateX: '0px',
                        translateY: '0px'
                    },
                    'font-size': labelElement.getStyle('font-size')
                }
            };
        },

        handlePaste: function (e) {
            var me = this,
                inputMask = me.getInputMask();

            if (inputMask) {
                inputMask.onPaste(me, me.getValue(), e);
            }

            me.fireEvent('paste', me, e);
        },

        /**
         * @private
         */
        createTrigger: function (name, trigger) {
            if (!trigger.isTrigger) {
                if (trigger === true) {
                    trigger = {
                        type: name
                    };
                } else if (typeof trigger === 'string') {
                    trigger = {
                        type: trigger
                    };
                }

                trigger = Ext.apply({
                    name: name,
                    field: this
                }, trigger);

                trigger = trigger.xtype ? Ext.create(trigger) : Ext.Factory.trigger(trigger);
            }

            return trigger;
        },

        syncLabelPlaceholder: function (animate) {
            var me = this,
                inputEl = me.inputElement,
                value = inputEl && inputEl.dom.value,
                inside;

            me._animPlaceholderLabel = animate;

            if (me.rendered) {
                if (value !== '') {
                    // The cardinal rule: if there is a value do NOT put the label on
                    // top of it!
                    inside = false;
                }
                else {
                    // Otherwise, if the input is not focused we would generally put
                    // the label on top to act as the placeholder. Or flipped, when
                    // the input is focused we want to move the label outside. That
                    // is UNLESS the field is disabled or readonly since doing that
                    // move-to-outside would make it seem like the field is editable
                    // in some way (which it is not).
                    inside = !me.hasFocus || me.getDisabled() || me.getReadOnly();
                }

                me.setLabelInPlaceholder(inside);
            }

            me._animPlaceholderLabel = false;
        },

        /**
         * Synchronizes the DOM to match the triggers' configured weight, side, and grouping
         * @private
         */
        syncTriggers: function () {
            var me = this,
                triggers = me.getTriggers(),
                beforeInputElement = me.beforeInputElement,
                afterInputElement = me.afterInputElement,
                triggerGroups = me.triggerGroups || (me.triggerGroups = {}),
                beforeTriggers = [],
                afterTriggers = [],
                triggersByGroup = {},
                TriggerBase = Ext.field.trigger['Base'],
                name, trigger, groupName, triggerGroup, i, ln;

            for (name in triggers) {
                trigger = triggers[name];

                groupName = trigger.getGroup();

                if (groupName) {
                    (triggersByGroup[groupName] || (triggersByGroup[groupName] = [])).push(trigger);
                } else if (trigger.getSide() === 'left') {
                    beforeTriggers.push(trigger);
                } else {
                    afterTriggers.push(trigger);
                }
            }

            for (groupName in triggersByGroup) {
                triggerGroup = triggerGroups[groupName];

                if (!triggerGroup) {
                    triggerGroup = triggers[groupName]; // just in case the user configured a group trigger

                    if (!triggerGroup) {
                        triggerGroup = new TriggerBase();
                    }

                    triggerGroups[groupName] = triggerGroup;
                }

                triggerGroup.setTriggers(TriggerBase.sort(triggersByGroup[groupName]));

                if (triggerGroup.getSide() === 'left') {
                    beforeTriggers.push(triggerGroup);
                } else {
                    afterTriggers.push(triggerGroup);
                }
            }

            TriggerBase.sort(beforeTriggers);
            TriggerBase.sort(afterTriggers);

            for (i = 0, ln = beforeTriggers.length; i < ln; i++) {
                beforeInputElement.appendChild(beforeTriggers[i].element);
            }

            for (i = 0, ln = afterTriggers.length; i < ln; i++) {
                afterInputElement.appendChild(afterTriggers[i].element);
            }

            for (groupName in triggerGroups) {
                if (!(groupName in triggersByGroup)) {
                    // group no longer has any triggers. it can be removed.
                    triggerGroup = triggerGroups[groupName];
                    triggerGroup.setTriggers(null);
                    triggerGroup.destroy();
                    delete triggerGroups[groupName];
                }
            }
        },

        syncEmptyState: function () {
            var me = this,
                triggers = me.getTriggers(),
                inputMask = me.getInputMask(),
                clearTrigger = triggers && triggers.clear,
                value = me.inputElement.dom.value,
                visible;

            me.toggleCls(me.emptyCls, !value);

            if (clearTrigger) {
                if (me.getClearable()) {
                    if (value !== '' && !me.getDisabled() && !me.getReadOnly()) {
                        visible = true;
                    }

                    if (inputMask) {
                        // Don't show clear trigger if there is no real input in the field
                        if (value === inputMask._mask) {
                            visible = false;
                        }
                    }
                }

                if (visible) {
                    clearTrigger.show();
                } else {
                    clearTrigger.hide();
                }
            }
        }
    },

    deprecated: {
        '6.5': {
            configs: {
                /**
                 * @cfg {String} placeHolder
                 * A string value displayed in the input when the control is empty.
                 * @deprecated 6.5.0 Use the all lowercase {@link #placeholder} config.
                 */
                placeHolder: 'placeholder',
                clearIcon: 'clearable'
            },
            methods: {
                getPlaceHolder: 'getPlaceholder',
                setPlaceHolder: 'setPlaceholder'
            }
        }
    }
},
function() {
    // Fix for android active field not scrolled into view when keyboard is shown
    if (Ext.os.is.Android) {
        window.addEventListener('resize', function () {
            var el = document.activeElement,
                tag = el && el.tagName;
            
            if (tag === 'INPUT' || tag === 'TEXTAREA') {
                el.scrollIntoView();
            }
        });
    }
});
