/**
 * Field is the base class for all form fields. It provides a lot of shared functionality to all
 * field subclasses (for example labels, simple validation, {@link #clearable clearing} and tab index management), but
 * is rarely used directly. Instead, it is much more common to use one of the field subclasses:
 *
 *     xtype            Class
 *     ---------------------------------------
 *     textfield        {@link Ext.field.Text}
 *     numberfield      {@link Ext.field.Number}
 *     textareafield    {@link Ext.field.TextArea}
 *     hiddenfield      {@link Ext.field.Hidden}
 *     radiofield       {@link Ext.field.Radio}
 *     filefield        {@link Ext.field.File}
 *     checkboxfield    {@link Ext.field.Checkbox}
 *     selectfield      {@link Ext.field.Select}
 *     togglefield      {@link Ext.field.Toggle}
 *     fieldset         {@link Ext.form.FieldSet}
 *
 * Fields are normally used within the context of a form and/or fieldset. See the {@link Ext.form.Panel FormPanel}
 * and {@link Ext.form.FieldSet FieldSet} docs for examples on how to put those together, or the list of links above
 * for usage of individual field types. If you wish to create your own Field subclasses you can extend this class,
 * though it is sometimes more useful to extend {@link Ext.field.Text} as this provides additional text entry
 * functionality.
 */
Ext.define('Ext.field.Field', {
    extend: 'Ext.Component',
    alternateClassName: 'Ext.form.Field',
    xtype: 'field',
    
    /**
     * Set to `true` on all Ext.field.Field subclasses. This is used by {@link Ext.form.Panel#getValues} to determine which
     * components inside a form are fields.
     * @property isField
     * @type Boolean
     */
    isField: true,

    /**
     * @private
     */
    isFormField: true,

    /**
     * @event click
     * Fires whenever the input is clicked.
     * @param {Ext.event.Event} e The event object.
     */

    /**
     * @event keyup
     * Fires whenever keyup is detected.
     * @param {Ext.event.Event} e The event object.
     */

    /**
     * @event paste
     * Fires whenever paste is detected.
     * @param {Ext.event.Event} e The event object.
     */

    /**
     * @event mousedown
     * Fires whenever the input has a mousedown occur.
     * @param {Ext.event.Event} e The event object.
     */

    /**
     * @event errorchange
     * Fires when the active error message changes.
     * @param {Ext.field.Field} this
     * @param {String} error The current error message
     * @since 6.5.0
     */

    /**
     * @event change
     * Fires when the value has changed.
     * @param {Ext.field.Field} this This field
     * @param {String} newValue The new value
     * @param {String} oldValue The original value
     */

    cachedConfig: {
        /**
         * @cfg {'start'/'center'/'end'/'stretch'}
         *
         * The horizontal alignment the contents of this field's body element.
         */
        bodyAlign: 'start',

        /**
         * @cfg {'top'/'left'/'bottom'/'right'}
         * The position to render the label relative to the field body.
         */
        labelAlign: 'left',

        /**
         * @cfg {String}
         * Optional CSS class to add to the Label element.
         */
        labelCls: null,

        /**
         * @cfg {'top'/'right'/'bottom'/'left'}
         *
         * Text alignment of this field's label
         */
        labelTextAlign: 'left',

        /**
         * @cfg {Number/String}
         * Width of this field's label. Can be a number of pixels or any valid CSS value,
         * such as `'30%'`. To size the label to its text, use `labelWidth: 'auto'`
         */
        labelWidth: null,

        /**
         * @cfg {Number/String}
         * Min-width of this field's label.
         */
        labelMinWidth: null,

        /**
         * @cfg {Boolean}
         * `true` to allow the label to wrap. If set to `false`, the label will be truncated
         * with an ellipsis.
         */
        labelWrap: false
    },

    config: {
        /**
         * @cfg {String}
         * The field's name.  Used by form panels to gather data to be submitted.
         *
         * For {@link Ext.field.Input Input Fields} this name is set as the `name` attribute
         * of the `inputElement`
         */
        name: null,

        /**
         * The label of this field
         * @cfg {String} label
         * @accessor
         */
        label: null,

        /**
         * @cfg {Boolean}
         * `true` to make this field required.
         */
        required: false,

        /**
         * @cfg {String}
         * The error message to display when {@link #required} is `true` and the field's
         * value is "empty" (`null`, `undefined`, or empty string).
         */
        requiredMessage: 'This field is required',

        /**
         * @cfg {Mixed}
         * The field's value
         */
        value: null,

        /**
         * @cfg {Mixed}
         * A validator or array of validators to be applied to the field.  
         *
         * When the field is validated, each validator is applied and if any one of them 
         * determines the field  is invalid, the field will be marked as invalid.  If you 
         * examine the field's validators, you will get an array of Ext.data.Validators.
         *
         * Validation currently is synchronous.  If you need to validate a field with interaction
         * with a server, you would do this when the form is submitted.
         *
         * A validator may be:
         *
         * * A regexp - if the field fails to match the regexp, it is invalid.
         * * A function - the function will be called to validate the field; it should return false if invalid.`
         * * An object - an object with a member fn that is a function to be called to validate the field.
         * * An instantiated Validator {@link  Ext.data.validator}
         */
        validators: null,

        /**
         * @cfg {String} validationMessage
         * For validation, regex, etc., this is the error message returned if field is invalid.
         */
        validationMessage: 'Is in the wrong format',

        /**
         * @cfg {Boolean} [validateDisabled=false]
         * `true` to validate the field, even if it is disabled.
         */
        validateDisabled: null,

        /**
         * @cfg {Boolean} [disabled=false] `true` to disable the field.
         *
         * Be aware that conformant with the [HTML specification](http://www.w3.org/TR/html401/interact/forms.html),
         * disabled Fields will not be {@link Ext.form.Panel#method-submit submitted}.
         * @accessor
         */

        /**
         * @cfg {Boolean} fullscreen
         * @hide
         */

        /**
         * @cfg {Boolean} [autoFitErrors=true]
         * Whether to adjust the field's body width to make room for the
         * {@link #errorTarget error messages}.
         */
        autoFitErrors: null,

        /**
         * @cfg {Boolean} [inline=false]
         * `true` to cause this field to layout with inline element behavior.
         * An inline field's width is determined by the total width of its label and body
         * elements instead of automatically expanding to fill the width of its container.
         */
        inline: null,

        /**
         * @cfg {String/String[]} error
         * The error (or errors) to display for this field. This config is typically set
         * by the field's {@link #cfg!validators validators} but can be set directly if
         * an error needs to be associated with a field manually.
         *
         * Errors will be HTML encoded as necessary and {@link #cfg!errorTpl formatted}
         * before becoming the current {@link #cfg!errorMessage}.
         * @since 6.5.0
         */
        error: null,

        /**
         * @cfg {String} errorMessage
         * The field's error message to display as {@link #cfg!errorTarget specified}.
         * This message must already be properly formatted and encoded as appropriate
         * for the `errorTarget`.
         * @since 6.5.0
         */
        errorMessage: null,

        /**
         * @cfg {String} errorTarget
         * The location where the error message text should display.
         *
         * The following are values that have predefined meaning:
         *
         * - `qtip` Display a {@link Ext.tip.Manager quick tip} containing the message
         *  when the user hovers (or taps) the field. For this option to work, quick tips
         *  must be enabled by calling {@link Ext.tip.Manager#init}.
         * - `side` Add an error icon to the right of the field, displaying the message
         *  in a popup on hover or tap.
         * - `title` Display the message in a default browser `title` attribute.
         * - `under` Add a `div` beneath the field containing the error message.
         * @since 6.5.0
         */
        errorTarget: 'qtip',

        /**
         * @cfg {String/String[]/Ext.XTemplate} errorTpl
         * The template used to format the {@link #cfg!error error} set for this field.
         * By default, the {@link #cfg!errorTarget errorTarget} is used to determine
         * whether the error(s) are formatted as an HTML list or as plain text.
         * @since 6.5.0
         */
        errorTpl: null,

        /**
         * @cfg {Object} errorTip
         * The default config that will be used to display errors in the tooltip.
         * @since 6.5.0
         */
        errorTip: null,

        /**
         * @cfg {String}
         * @private
         */
        sideError: null,

        /**
         * @cfg {String}
         * @private
         */
        tipError: null,

        /**
         * @cfg {String}
         * @private
         */
        titleError: null,

        /**
         * @cfg {String}
         * @private
         */
        underError: null
    },

    /**
     * @property {Ext.XTemplate} htmlErrorsTpl
     * The default template used to format errors in HTML. This property is promoted
     * to an `Ext.XTemplate` instance on first use.
     * @private
     * @readonly
     * @since 6.5.0
     */
    htmlErrorsTpl: [
        '<tpl if="count == 1">',
            '<tpl for="errors">{.:htmlEncode}</tpl>',
        '<tpl elseif="count">',
            '<ul class="{listCls}">',
                '<tpl for="errors"><li>{.:htmlEncode}</li></tpl>',
            '</ul>',
        '</tpl>'
    ],

    /**
     * @property {Ext.XTemplate} plainErrorsTpl
     * The default template used to format errors as plain text. This property is promoted
     * to an `Ext.XTemplate` instance on first use.
     * @private
     * @readonly
     * @since 6.5.0
     */
    plainErrorsTpl: [
        '<tpl if="count">',
            '<tpl for="errors" between="\\n">{.}</tpl>',
        '</tpl>'
    ],

    _errorTplMap: {
        title: 'plainErrorsTpl'
    },

    /**
     * @property {Mixed} originalValue
     * The original value when the field was instantiated.
     * @private
     */
    originalValue: null,

    focusable: true,

    classCls: Ext.baseCSSPrefix + 'field',
    requiredCls: Ext.baseCSSPrefix + 'required',
    noLabelWrapCls: Ext.baseCSSPrefix + 'no-label-wrap',
    invalidCls: Ext.baseCSSPrefix + 'invalid',
    noAutoFitErrorsCls: Ext.baseCSSPrefix + 'no-auto-fit-errors',
    inlineCls: Ext.baseCSSPrefix + 'inline',
    labelAlignVerticalCls: Ext.baseCSSPrefix + 'label-align-vertical',
    labelAlignHorizontalCls: Ext.baseCSSPrefix + 'label-align-horizontal',
    labeledCls: Ext.baseCSSPrefix + 'labeled',

    verticalLabelMap: {
        top: 1,
        placeholder: 1,
        bottom: 1
    },

    horizontalLabelMap: {
        left: 1,
        right: 1
    },

    nameable: true,

    /**
     * @property {"none"/"auto"/"all"} validateOnInit
     * Determines how initial values will handle validation
     * - none: Will not validate any initial values
     * - auto: Will only validate non-empty initial values
     * - all: Will validate all initial values
     */
    validateOnInit: 'auto',

    errorElement: null,
    errorIconElement: null,
    errorMessageElement: null,

    element: {
        reference: 'element',
        classList: [
            Ext.supports.CSSMinContent ? '' : (Ext.baseCSSPrefix + 'no-min-content'),
            Ext.supports.PercentageSizeFlexBug ? (Ext.baseCSSPrefix + 'has-percentage-size-flex-bug') : ''
        ]
    },

    initialize: function () {
        var me = this;

        me.callParent();

        if (me.getValue() === '' && me.validateOnInit === 'all') {
            me.validate();
        }
    },

    /**
     * @private
     * Checks if the value has changed. Allows subclasses to override for
     * any more complex logic.
     */
    didValueChange: function (newVal, oldVal){
        return !this.isEqual(newVal, oldVal);
    },

    getTemplate: function () {
        return [{
            reference: 'labelElement',
            cls: Ext.baseCSSPrefix + 'label-el',
            tag: 'label',
            children: [{
                reference: 'labelTextElement',
                cls: Ext.baseCSSPrefix + 'label-text-el',
                tag: 'span'
            }]
        }, {
            reference: 'bodyWrapElement',
            cls: Ext.baseCSSPrefix + 'body-wrap-el',
            children: [{
                reference: 'bodyElement',
                cls: Ext.baseCSSPrefix + 'body-el',
                children: this.getBodyTemplate()
            }, {
                reference: 'errorElement',
                cls: Ext.baseCSSPrefix + 'error-el',
                children: [{
                    reference: 'errorIconElement',
                    cls: Ext.baseCSSPrefix + 'error-icon-el ' +
                    Ext.baseCSSPrefix + 'font-icon'
                }, {
                    reference: 'errorMessageElement',
                    cls: Ext.baseCSSPrefix + 'error-message-el'
                }]
            }]
        }];
    },

    getBodyTemplate: Ext.emptyFn,

    initElement: function () {
        this.callParent();

        // alias for backward compatibility
        this.innerElement = this.innerElement || this.bodyElement;
    },

    onFocusLeave: function(e) {
        this.callParent([e]);

        this.completeEdit();
    },

    /**
     * @method
     * @protected
     * Called when focus leaves this input field.
     * Used to postprocess raw values and perform conversion and validation.
     */
    completeEdit: Ext.emptyFn,

    updateBodyAlign: function (bodyAlign, oldBodyAlign) {
        var element = this.element;

        if (oldBodyAlign) {
            element.removeCls(Ext.baseCSSPrefix + 'body-align-' + oldBodyAlign);
        }

        if (bodyAlign) {
            element.addCls(Ext.baseCSSPrefix + 'body-align-' + bodyAlign);
        }
    },

    updateAutoFitErrors: function (autoFitErrors) {
        this.toggleCls(this.noAutoFitErrorsCls, autoFitErrors === false);
    },

    applyErrorTpl: function (tpl) {
        if (tpl && !tpl.isTemplate) {
            tpl = Ext.XTemplate.get(tpl);
        }

        return tpl;
    },

    /**
     * Formats the given error(s) based on the given {@link #cfg!errorTpl} and the
     * specified {@link #cfg!errorTarget}.
     * @param {String[]} errors
     * @return {String}
     * @protected
     * @since 6.5.0
     */
    formatErrors: function (errors) {
        var me = this,
            tpl = me.getErrorTpl();

        if (!tpl) {
            tpl = me.lookupTpl(me._errorTplMap[me.getErrorTarget()] || 'htmlErrorsTpl');
        }

        return tpl.apply({
            count: errors ? errors.length : 0,
            label: me.getLabel(),
            errors: errors
        });
    },

    updateError: function (value) {
        var msg = this.formatErrors(Ext.Array.from(value));

        this.setErrorMessage(msg);
    },

    updateErrorMessage: function (msg) {
        var me = this,
            errorTarget;

        me.fireEvent('errorchange', me, msg);

        if (me.preventMark) {
            return;
        }

        me.toggleInvalidCls(!!msg);

        errorTarget = me.getErrorTarget();

        switch (errorTarget) {
            case 'side':
                me.setSideError(msg);
                break;

            case 'qtip':
                me.setTipError(msg);
                break;

            case 'title':
                me.setTitleError(msg);
                break;

            case 'under':
                me.setUnderError(msg);
                break;

            case 'parent':
                var owner = me.up('[onFieldErrorChange]');

                if (owner) {
                    owner.onFieldErrorChange(me, msg);
                }
                break;

            //default:
                //TODO
                // @method ==> controller
                // .foo ==> DQ
        }
    },

    updateErrorTarget: function (target, oldTarget) {
        var me = this,
            error, owner;

        if (oldTarget) {
            me.removeCls(Ext.baseCSSPrefix + 'error-target-' + oldTarget);

            if (oldTarget === 'qtip') {
                me.setTipError(null);
            } else if (oldTarget === 'title') {
                me.setTitleError(null);
            } else if (oldTarget === 'side') {
                me.setSideError(null);
            } else if (oldTarget === 'under') {
                me.setUnderError(null);
            } else if (oldTarget === 'parent') {
                owner = me.up('[onFieldErrorChange]');

                if (owner) {
                    owner.onFieldErrorChange(me);
                }
            }
        }

        if (target) {
            me.addCls(Ext.baseCSSPrefix + 'error-target-' + target);

            if (!me.isConfiguring) {
                error = me.getError();

                if (error) {
                    if (target === 'qtip') {
                        me.setTipError(error);
                    } else if (target === 'title') {
                        me.setTitleError(error);
                    } else if (target === 'side') {
                        me.setSideError(error);
                    } else if (target === 'under') {
                        me.setUnderError(error);
                    } else if (target === 'parent') {
                        owner = me.up('[onFieldErrorChange]');

                        if (owner) {
                            owner.onFieldErrorChange(me, error);
                        }
                    }
                }
            }
        }
    },

    updateInline: function (inline) {
        this.toggleCls(this.inlineCls, inline);
    },

    updateSideError: function (error) {
        if (error) {
            error = Ext.apply({
                html: error
            }, this.getErrorTip());
        }

        this.errorElement.getData().qtip = error;
    },

    updateTipError: function (error) {
        if (error) {
            error = Ext.apply({
                html: error
            }, this.getErrorTip());
        }

        // Using the bodyElement as the target of the qtip ensures that the tip is visually
        // aligned to the field body, regardless of label positioning or bottom padding added
        // by the stylesheet to support $field-vertical-spacing
        this.bodyElement.getData().qtip = error;
    },

    updateTitleError: function (error) {
        var dom = this.el.dom;

        if (error) {
            dom.setAttribute('title', error);
        } else {
            dom.removeAttribute('title');
        }
    },

    updateUnderError: function (error) {
        this.errorMessageElement.dom.innerHTML = error || '';
    },

    updateLabel: function (label) {
        this.labelTextElement.setHtml(label);
        this.el.toggleCls(this.labeledCls, !!label);
    },

    updateLabelAlign: function (newLabelAlign, oldLabelAlign) {
        var me = this,
            element = me.element;

        if (oldLabelAlign) {
            element.removeCls(Ext.baseCSSPrefix + 'label-align-' + oldLabelAlign);
        }

        if (newLabelAlign) {
            element.addCls(Ext.baseCSSPrefix + 'label-align-' + newLabelAlign);
        }

        element.toggleCls(me.labelAlignVerticalCls, newLabelAlign in me.verticalLabelMap);
        element.toggleCls(me.labelAlignHorizontalCls, newLabelAlign in me.horizontalLabelMap);
    },

    updateLabelTextAlign: function (labelTextAlign, oldLabelTextAlign) {
        var element = this.element;

        if (oldLabelTextAlign) {
            element.removeCls(Ext.baseCSSPrefix + 'label-text-align-' + oldLabelTextAlign);
        }

        if (labelTextAlign) {
            element.addCls(Ext.baseCSSPrefix + 'label-text-align-' + labelTextAlign);
        }
    },

    updateLabelCls: function (newLabelCls, oldLabelCls) {
        var labelElement = this.labelElement;

        if (newLabelCls) {
            labelElement.addCls(newLabelCls);
        }

        if (oldLabelCls) {
            labelElement.removeCls(oldLabelCls);
        }
    },

    updateLabelWidth: function (labelWidth) {
        this.labelElement.setWidth(labelWidth);
    },

    updateLabelMinWidth: function (labelMinWidth) {
        this.labelElement.setStyle('min-width', Ext.Element.addUnits(labelMinWidth));
    },

    updateLabelWrap: function (labelWrap) {
        this.element.toggleCls(this.noLabelWrapCls, !labelWrap);
    },

    updateName: function (newName) {
        this.name = newName;
    },

    updateRequired: function (required) {
        var me = this;

        me.element.toggleCls(me.requiredCls, required);

        if (!me.isConfiguring) {
            me.validate();
        }
    },

    updateRequiredMessage: function () {
        if (!this.isConfiguring) {
            this.validate();
        }
    },

    updateDisabled: function (disabled, oldDisabled) {
        this.callParent([disabled, oldDisabled]);

        if (!this.isConfiguring) {
            this.validate();
        }
    },

    updateValidateDisabled: function () {
        if (!this.isConfiguring) {
            this.validate();
        }
    },

    applyValue: function (value) {
        if (this.isConfiguring) {
            this.originalValue = value;
        }

        return value;
    },

    updateValue: function (value, oldValue) {
        var me = this;

        // Don't try to validate the field if the value transitions between empty values (null,
        // undefined, '', etc.). This can happen after initialization when binding value to an
        // empty record field (e.g while building a creation form, which is initially empty).
        if (!(Ext.isEmpty(value) && Ext.isEmpty(oldValue))) {
            me.validate();
        }

        if (!me.isConfiguring && value !== oldValue) {
            me.fireEvent('change', me, value, oldValue);
        }
    },

    /**
     * Resets the current field value back to the original value on this field when it was created.
     *
     *     // This will create a field with an original value
     *     var field = Ext.Viewport.add({
     *         xtype: 'textfield',
     *         value: 'first value'
     *     });
     *
     *     // Update the value
     *     field.setValue('new value');
     *
     *     // Now you can reset it back to the `first value`
     *     field.reset();
     *
     * @return {Ext.field.Field} this
     */
    reset: function() {
        this.setValue(this.originalValue);

        return this;
    },

    /**
     * Resets the field's {@link #originalValue} property so it matches the current {@link #getValue value}. This is
     * called by {@link Ext.form.Panel}.{@link Ext.form.Panel#setValues setValues} if the form's
     * {@link Ext.form.Panel#trackResetOnLoad trackResetOnLoad} property is set to true.
     */
    resetOriginalValue: function () {
        this.originalValue = this.getValue();
    },

    /**
     * Returns `true` if the value of this Field has been changed from its {@link #originalValue}.
     * Will return `false` if the field is disabled or has not been rendered yet.
     *
     * @return {Boolean} `true` if this field has been changed from its original value (and
     * is not disabled), `false` otherwise.
     */
    isDirty: function () {
        return this.getValue() !== this.originalValue;
    },

    /**
     * @private
     * Add/remove invalid class(es)
     * @param {Boolean} hasError 
     */
    toggleInvalidCls: function (hasError) {
        this.el[hasError ? 'addCls' : 'removeCls'](this.invalidCls);
    },

    /**
     * Mark field as invalid.
     * @deprecated 6.5.0 Use {@link #setError} instead. (for classic compatibility)
     * @since 6.5.0
     */
    markInvalid: function (messages) {
        this.setError(messages);
    },

    /**
     * Mark field as valid.
     * @deprecated 6.5.0 Use {@link #setError setError(null)} instead. (for classic compatibility)
     * @since 6.5.0
     */
    clearInvalid: function () {
        this.setError(null);
    },

    /**
     * Returns true if field is valid.
     */
    isValid: function () {
        return !this.getError();
    },

    /**
     * Returns whether two field {@link #getValue values} are logically equal. Field implementations may override this
     * to provide custom comparison logic appropriate for the particular field's data type.
     * @param {Object} value1 The first value to compare
     * @param {Object} value2 The second value to compare
     * @return {Boolean} True if the values are equal, false if inequal.
     */
    isEqual: function (value1, value2) {
        return String(value1) === String(value2);
    },

    /**
     * @private
     */
    applyValidators: function (validators) {
        var me = this,
            i, len, ret;

        validators = (validators && !Ext.isArray(validators)) ? [validators] : validators;
        len = validators && validators.length;
        ret = len ? [] : null;

        for (i = 0; i < len; ++i) {
            ret.push(me.decodeValidator(validators[i]));
        }

        return ret;
    },

    wrapValidatorFn: function (fn, validator) {
        var me = this,
            scope = validator && validator.scope;

        return new Ext.data.validator['Validator'](function(value) {
            return Ext.callback(fn, scope, [value], 0, me);
        });
    },

    /**
     * This method is called by {@link #method!validate validate} if the value is both
     * non-empty (not `null`, `undefined` or `''`) and if the value can be parsed by the
     * {@link #method!parseValue parseValue} method. This parsing concern is technically
     * only in play for `Ext.field.Text` and derived classes (such as `Ext.field.Date` and
     * `Ext.field.Number`) but the guarantee here is that the `value` will be a parsed
     * value and not the raw string and if the value cannot be parsed, this method will
     * not be called.
     *
     * @param {Mixed} value The (parsed) value
     * @param {String[]} errors The array of validation errors
     * @param {Boolean} [skipLazy] `false` (the default) to run all validators.
     * @private
     */
    doValidate: function (value, errors, skipLazy) {
        var validators = this.getValidators(),
            len = validators && validators.length,
            i, result, validator;

        for (i = 0; i < len; ++i) {
            validator = validators[i];

            if (!skipLazy || !validator.lazy) {
                result = validator.validate(value);

                if (result !== true) {
                    //<debug>
                    if (!result || typeof result !== 'string') {
                        Ext.raise('Validator did not return a valid result.');
                    }
                    //</debug>

                    errors.push(result);
                }
            }
        }
    },

    parseValue: Ext.identityFn, // documented on textfield

    /**
     * Validate the field and return it's validity state. 
     * To get the existing validity state without re-validating current value,
     * use {@link isValid}.
     *
     * @param {Boolean} [skipLazy] (private) Pass `true` to skip validators marked as `lazy`.
     * @return {Boolean} The new validity state.
     */
    validate: function (skipLazy) {
        var me = this,
            empty, errors, field, record, validity, value;

        // If we are in configuration and not validating any values, skip out of here
        if (me.isConfiguring && me.validateOnInit === 'none') {
            return true;
        }

        // if field is disabled and cfg not set to validate if disabled, skip out of here
        if (!me.getDisabled() || me.getValidateDisabled()) {
            errors = [];

            // If we are a textual input field, get the input element's value.
            // Check the DOM validity state first in case a type="number"
            // check has failed.
            if (me.isInputField && !me.isSelectField) {
                value = me.getInputValue();
                empty = !value;
                validity = empty && me.inputElement.dom.validity;

                if (validity && validity.badInput) {
                    errors.push(me.badFormatMessage);
                    empty = false;
                }
            }
            else {
                value = me.getValue();
                empty = value === '' || value == null;
            }

            if (empty && me.getRequired()) {
                errors.push(me.getRequiredMessage());
            }
            else if (!errors.length) {
                if (!empty) {
                    // Pass non-empty values along to parseValue to handle things like
                    // datefield and numberfield. Technically this concern is more of a
                    // textfield family issue, but it is awkward to leap out of this
                    // sequence in such a way as to make a surgical override practical...
                    // So we simply provide identityFn as the default parseValue impl
                    value = me.parseValue(value, errors);
                }

                if (!errors.length) {
                    field = me._validationField;
                    record = me._validationRecord;

                    if (field && record) {
                        field.validate(value, null, errors, record);
                    }

                    if (!empty) {
                        me.doValidate(value, errors, skipLazy);
                    }
                }
            }

            if (errors.length) {
                me.setError(errors);
                return false;
            }
        }

        me.setError(null);
        return true;
    },

    getFocusClsEl: function () {
        return this.element;
    },

    updateHeight: function(height, oldHeight) {
        this.callParent([height, oldHeight]);
        this.syncFormLayoutHeight();
    },

    onAdded: function(parent, instanced) {
        this.callParent([parent, instanced]);
        this.syncFormLayoutHeight();
        this.validateLayout();
    },

    onRemoved: function(destroying) {
        this.callParent([destroying]);
        this.syncFormLayoutHeight();
    },

    privates: {
        syncFormLayoutHeight: function() {
            var me = this,
                parent = me.parent,
                height = me.getHeight();

            if (!(height && parent && parent.getLayout().isFormLayout)) {
                height = null;
            }

            me.bodyElement.setHeight(height);
        },

        validateLayout: function () {
            var errorTarget = this.getErrorTarget(),
                parent = this.parent;

            if (this.isInner && parent && parent.getLayout().isFormLayout) {
                // Form layout only supports left aligned labels
                this.setLabelAlign('left');

                // Form layout does not support "under" error target
                if (errorTarget === 'under') {
                    this.setErrorTarget('side');
                }
            }
        },

        applyBind: function (bind, currentBindings) {
            var me = this,
                valueBinding = currentBindings && currentBindings.value,
                bindings, newValueBind;

            bindings = me.callParent([ bind, currentBindings ]);

            if (bindings) {
                newValueBind = bindings.value;
                me.hasBindingValue = !!newValueBind;

                if (newValueBind !== valueBinding && me.getInherited().modelValidation) {
                    me.updateValueBinding(bindings);
                }
            }

            return bindings;
        },

        updateValueBinding: function (bindings) {
            var me = this,
                newBinding = bindings.value,
                fieldBinding = bindings.$fieldBinding;

            if (fieldBinding) {
                fieldBinding.destroy();
                bindings.$fieldBinding = null;
            }

            if (newBinding && newBinding.bindValidationField) {
                me.fieldBinding = newBinding.bindValidationField('setValidationField', me);
            }
        },

        setValidationField: function (field, record) {
            this._validationField = field;
            this._validationRecord = record;
        },

        decodeValidator: function(validator) {
            var type = Ext.typeOf(validator),
                result = validator.fn;

            if (type === 'function') {
                result = this.wrapValidatorFn(validator);
            }
            else if (type === 'regexp') {
                result = Ext.Factory.validator({
                    type: 'format',
                    matcher: validator
                });
            }
            else if (type === 'object' && result && !validator.isValidator) {
                result = this.wrapValidatorFn(result, validator);
            }
            else {
                result = Ext.Factory.validator(validator);
            }

            return result;
        }
    }
});
