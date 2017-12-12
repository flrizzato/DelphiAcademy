/**
 * A mixin that contains functionality common to {@link Ext.field.Panel} and
 * {@link Ext.form.FieldSet}.
 *
 * @private
 * @since 6.5.0
 */
Ext.define('Ext.form.Borders', {
    mixinId: 'formborders',

    config: {
        /**
         * @cfg {Boolean} [fieldSeparators=false]
         * Set to `true` to show separators between the fields in this form.  Typically
         * used together with {@link #inputBorders} to create iOS-like forms
         *
         * Defaults to `true` in the iOS theme
         */
        fieldSeparators: null,

        /**
         * @cfg {Boolean} [inputBorders=true]
         *
         * `false` to suppress borders on the input elements of fields in this form.
         * Typically used together with {@link #fieldSeparators} to create iOS-like forms.
         *
         * Defaults to `false` in the iOS theme
         */
        inputBorders: null
    },

    fieldSeparatorsCls: Ext.baseCSSPrefix + 'form-field-separators',
    noInputBordersCls: Ext.baseCSSPrefix + 'form-no-input-borders',

    updateFieldSeparators: function(fieldSeparators, oldFieldSeparators) {
        var bodyElement = this.bodyElement,
            cls = this.fieldSeparatorsCls;

        if (fieldSeparators) {
            bodyElement.addCls(cls);
        } else if (oldFieldSeparators) {
            bodyElement.removeCls(cls);
        }
    },

    updateInputBorders: function(inputBorders, oldInputBorders) {
        var bodyElement = this.bodyElement,
            cls = this.noInputBordersCls;

        if (inputBorders === false) {
            bodyElement.addCls(cls);
        } else if (oldInputBorders === false) {
            bodyElement.removeCls(cls);
        }
    }
});
