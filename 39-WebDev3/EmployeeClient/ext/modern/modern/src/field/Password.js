/**
 * The Password field creates a password input and is usually created inside a form. Because it creates a password
 * field, when the user enters text it will show up as stars. Aside from that, the password field is just a normal text
 * field. Here's an example of how to use it in a form:
 *
 *     @example
 *     Ext.create('Ext.form.Panel', {
 *         fullscreen: true,
 *         items: [
 *             {
 *                 xtype: 'fieldset',
 *                 title: 'Register',
 *                 items: [
 *                     {
 *                         xtype: 'emailfield',
 *                         label: 'Email',
 *                         name: 'email'
 *                     },
 *                     {
 *                         xtype: 'passwordfield',
 *                         label: 'Password',
 *                         name: 'password'
 *                     }
 *                 ]
 *             }
 *         ]
 *     });
 *
 * Or on its own, outside of a form:
 *
 *     Ext.create('Ext.field.Password', {
 *         label: 'Password',
 *         value: 'existingPassword'
 *     });
 *
 * Because the password field inherits from {@link Ext.field.Text textfield} it gains all of the functionality that text
 * fields provide, including getting and setting the value at runtime, validations and various events that are fired as
 * the user interacts with the component. Check out the {@link Ext.field.Text} docs to see the additional functionality
 * available.
 */
Ext.define('Ext.field.Password', {
    extend: 'Ext.field.Text',
    xtype: 'passwordfield',
    alternateClassName: 'Ext.form.Password',

    requires: [
        'Ext.field.trigger.Reveal'
    ],

    config: {
        /**
         * @cfg autoCapitalize
         * @inheritdoc
         */
        autoCapitalize: false,

        /**
         * @cfg {Boolean} revealable
         * Enables the reveal toggle button that will show the password in clear text.
         */
        revealable: false,

        /**
         * @cfg {Boolean} revealed
         * A value of 'true' for this config will show the password from clear text
         */
        revealed: {
            $value: false,
            lazy: true
        }
    },
    
    /**
     * @cfg inputType
     * @inheritdoc
     */
    inputType: 'password',

    /**
     * @property classCls
     * @inheritdoc
     */
    classCls: Ext.baseCSSPrefix + 'passwordfield',
    revealedCls: Ext.baseCSSPrefix + 'revealed',

    isPassword: true,

    applyTriggers: function(triggers, oldTriggers) {
        if (triggers && this.getRevealable() && !triggers.reveal) {
            triggers = Ext.apply({
                reveal: {
                    type: 'reveal'
                }
            }, triggers);
        }

        return this.callParent([triggers, oldTriggers]);
    },

    updateRevealed: function(newValue, oldValue) {
        var me = this;

        if (newValue) {
            me.element.addCls(me.revealedCls);
            me.setInputType("text");
        }
        else {
            me.element.removeCls(me.revealedCls);
            me.setInputType("password");
        }
    },

    updateValue: function(value, oldValue) {
        this.syncRevealTrigger();
        this.callParent([value, oldValue]);
    },

    doKeyUp: function(me, e) {
        this.callParent([me, e]);

        this.syncRevealTrigger();
    },

    onRevealTap: function(e) {
        this.fireAction('revealicontap', [this, e], 'doRevealTap');
    },

    /**
     * @private
     */
    doRevealTap: function(me, e) {
        this.setRevealed(!this.getRevealed());
    },

    privates: {
        isValidTextValue: function(value) {
            // allows newValue to be zero but not undefined or null (other falsey values)
            return (value !== undefined && value !== null && value !== '');
        },

        syncRevealTrigger: function () {
            var me = this,
                triggers = me.getTriggers(),
                revealTrigger = triggers && triggers.reveal,
                visible, value;

            if (revealTrigger) {
                if (me.getRevealable()) {
                    value = me.getValue();

                    if (value != null && value !== '' && !me.getDisabled() && !me.getReadOnly()) {
                        visible = true
                    }
                }

                if (visible) {
                    revealTrigger.show();
                } else {
                    revealTrigger.hide();
                }
            }
        }
    }
});
