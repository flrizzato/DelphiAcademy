/**
 * Creates an HTML textarea field on the page. This is useful whenever you need the user to enter large amounts of text
 * (i.e. more than a few words). Typically, text entry on mobile devices is not a pleasant experience for the user so
 * it's good to limit your use of text areas to only those occasions when free form text is required or alternative
 * input methods like select boxes or radio buttons are not possible. Text Areas are usually created inside forms, like
 * this:
 *
 *     @example
 *     Ext.create('Ext.form.Panel', {
 *         fullscreen: true,
 *         items: [
 *             {
 *                 xtype: 'fieldset',
 *                 title: 'About you',
 *                 items: [
 *                     {
 *                         xtype: 'textfield',
 *                         label: 'Name',
 *                         name: 'name'
 *                     },
 *                     {
 *                         xtype: 'textareafield',
 *                         label: 'Bio',
 *                         maxRows: 4,
 *                         name: 'bio'
 *                     }
 *                 ]
 *             }
 *         ]
 *     });
 *
 * In the example above we're creating a form with a {@link Ext.field.Text text field} for the user's name and a text
 * area for their bio. We used the {@link #maxRows} configuration on the text area to tell it to grow to a maximum of 4
 * rows of text before it starts using a scroll bar inside the text area to scroll the text.
 *
 * We can also create a text area outside the context of a form, like this:
 *
 * This creates two text fields inside a form. Text Fields can also be created outside of a Form, like this:
 *
 *     Ext.create('Ext.field.TextArea', {
 *         label: 'About You',
 *         placeHolder: 'Tell us about yourself...'
 *     });
 */
Ext.define('Ext.field.TextArea', {
    extend: 'Ext.field.Text',
    xtype: 'textareafield',
    
    alternateClassName: 'Ext.form.TextArea',

    config: {
        /**
         * @cfg autoCapitalize
         * @inheritdoc
         */
        autoCapitalize: false,

        /**
         * @cfg {Number} maxRows
         * The maximum number of lines made visible by the input.
         */
        maxRows: null,

        /**
         * @cfg clearable
         * @inheritdoc
         */
        clearable: false
    },

    /**
     * @property tag
     * @inheritdoc
     */
    tag: 'textarea',

    /**
     * @property classCls
     * @inheritdoc
     */
    classCls: Ext.baseCSSPrefix + 'textareafield',

    //<debug>
    applyMaxRows: function(maxRows) {
        if (maxRows !== null && typeof maxRows !== 'number') {
            throw new Error("Ext.field.TextArea: [applyMaxRows] trying to pass a value which is not a number");
        }

        return maxRows;
    },
    //</debug>

    updateMaxRows: function(newRows) {
        this.setInputAttribute('rows', newRows);
    },

    doKeyUp: function(me) {
        // Do not call parent - we don't want to fire action on enter key press
        this.syncEmptyState();
    }
});
