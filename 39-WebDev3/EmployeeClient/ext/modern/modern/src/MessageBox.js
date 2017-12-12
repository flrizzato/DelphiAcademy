/**
 * Utility class for generating different styles of message boxes. The framework provides a global singleton
 * {@link Ext.Msg} for common usage which you should use in most cases.
 *
 * If you want to use {@link Ext.MessageBox} directly, just think of it as a modal {@link Ext.Container}.
 *
 * Note that the MessageBox is asynchronous. Unlike a regular JavaScript `alert` (which will halt browser execution),
 * showing a MessageBox will not cause the code to stop. For this reason, if you have code that should only run _after_
 * some user feedback from the MessageBox, you must use a callback function (see the `fn` configuration option parameter
 * for the {@link #method-show show} method for more details).
 *
 *     @example
 *     Ext.Msg.alert('Title', 'The quick brown fox jumped over the lazy dog.', Ext.emptyFn);
 *
 * Checkout {@link Ext.Msg} for more examples.
 */
Ext.define('Ext.MessageBox', {
    extend: 'Ext.Dialog',
    xtype: 'messagebox',
    requires: [
        'Ext.util.InputBlocker'
    ],

    config: {
        /**
         * @cfg iconCls
         * @inheritdoc Ext.Button#cfg-iconCls
         * @accessor
         */
        iconCls: null,

        /**
         * @cfg {Number} defaultTextHeight
         * The default height in pixels of the message box's multiline textarea if displayed.
         * @accessor
         */
        defaultTextHeight: 75,

        // @cmd-auto-dependency {defaultType: 'Ext.Button', requires: ['Ext.Toolbar']}
        /**
         * @cfg {Array/Object} buttons
         * An array of buttons, or an object of a button to be displayed in the toolbar of this {@link Ext.MessageBox}.
         */
        buttons: null,

        /**
         * @cfg {String} message
         * The message to be displayed in the {@link Ext.MessageBox}.
         * @accessor
         */
        message: null,

        /**
         * @cfg {String} msg
         * The message to be displayed in the {@link Ext.MessageBox}.
         * @removed 2.0.0 Please use {@link #message} instead.
         */

        // @cmd-auto-dependency { requires: ['Ext.field.Text', 'Ext.field.TextArea'] }
        /**
         * @cfg {Object} prompt
         * The configuration to be passed if you want an {@link Ext.field.Text} or {@link Ext.field.TextArea} field
         * in your {@link Ext.MessageBox}.
         *
         * Pass an object with the property `multiLine` with a value of `true`, if you want the prompt to use a TextArea.
         *
         * Alternatively, you can just pass in an object which has an xtype/xclass of another component.
         *
         *     prompt: {
         *         xtype: 'textareafield',
         *         value: 'test'
         *     }
         *
         * @accessor
         */
        prompt: null,

        /**
         * @cfg layout
         * @inheritdoc
         */
        layout: {
            type: 'vbox',
            pack: 'center'
        },

        multiLine: null
    },

    /**
     * @property classCls
     * @inheritdoc
     */
    classCls: Ext.baseCSSPrefix + 'messagebox',

    /**
     * @cfg closeAction
     * @inheritdoc
     */
    closeAction: 'hide',

    headerCls: [
        Ext.baseCSSPrefix + 'dialogheader',
        Ext.baseCSSPrefix + 'messageboxheader'
    ],

    titleCls: [
        Ext.baseCSSPrefix + 'dialogtitle',
        Ext.baseCSSPrefix + 'messageboxtitle'
    ],

    toolCls: [
        Ext.baseCSSPrefix + 'paneltool',
        Ext.baseCSSPrefix + 'dialogtool',
        Ext.baseCSSPrefix + 'messageboxtool'
    ],

    statics: {
        INFO    : Ext.baseCSSPrefix + 'msgbox-info',
        WARNING : Ext.baseCSSPrefix + 'msgbox-warning',
        QUESTION: Ext.baseCSSPrefix + 'msgbox-question',
        ERROR   : Ext.baseCSSPrefix + 'msgbox-error',

        OK    : { ok: 'me.onClick' },
        YES   : { yes: 'me.onClick' },
        NO    : { no: 'me.onClick' },
        CANCEL: { cancel: 'me.onClick' },

        OKCANCEL: {
            ok: 'me.onClick',
            cancel: 'me.onClick'
        },

        YESNOCANCEL: {
            yes: 'me.onClick',
            no: 'me.onClick',
            cancel: 'me.onClick'
        },

        YESNO: {
            yes: 'me.onClick',
            no: 'me.onClick'
        }
    },

    /**
     * @private
     */
    constructor: function(config) {
        config = config || {};

        if (config.hasOwnProperty('multiline') || config.hasOwnProperty('multiLine')) {
            config.prompt = config.prompt || {};
            Ext.applyIf(config.prompt, {
                multiLine: config.multiline || config.multiLine
            });

            delete config.multiline;
            delete config.multiLine;
        }

        this.defaultAllowedConfig = {};
        var allowedConfigs = ['ui', 'showAnimation', 'hideAnimation', 'title', 'message', 'prompt', 'iconCls', 'buttons', 'defaultTextHeight'],
            ln = allowedConfigs.length,
            i, allowedConfig;

        for (i = 0; i < ln; i++) {
            allowedConfig = allowedConfigs[i];
            this.defaultAllowedConfig[allowedConfig] = this.defaultConfig[allowedConfig];
        }

        this.callParent([config]);
    },

    /**
     * Creates a new {@link Ext.Toolbar} instance using {@link Ext#factory}.
     * @private
     */
    applyTitle: function(config) {
        if (typeof config === "string") {
            return config;
        }

        return config.title;
    },

    /**
     * Adds the new {@link Ext.Toolbar} instance into this container.
     * @private
     */
    updateTitle: function(newTitle) {
        var header = this.getHeader() || {};

        if (Ext.isSimpleObject(header)) {
            header.title = newTitle;
            this.setHeader(header);
        } else if (Ext.isFunction(header.setTitle)) {
            header.setTitle(newTitle);
        }
    },

    /**
     * @private
     */
    applyMessage: function(config) {
        config = {
            html : config,
            cls  : this.baseCls + '-text'
        };

        return Ext.factory(config, Ext.Component, this._message);
    },

    /**
     * @private
     */
    updateMessage: function(newMessage) {
        if (newMessage) {
            this.add(newMessage);
        }
    },

    getMessage: function() {
        if (this._message) {
            return this._message.getHtml();
        }

        return null;
    },

    /**
     * @private
     */
    applyIconCls: function(config) {

        if (config) {
            config = {
                xtype: 'component',
                docked: 'left',
                width: 40,
                height: 40,
                hidden: (config) ? false : true,
                cls: Ext.baseCSSPrefix + 'icon ' + config
            };
            return Ext.factory(config, Ext.Component, this._iconCls);
        }

        return config;
    },

    /**
     * @private
     */
    updateIconCls: function(newIconCls, oldIconCls) {
        //ensure the title and button elements are added first
        this.getTitle();
        this.getButtons();

        if (newIconCls) {
            this.add(newIconCls);
        }
        else {
            this.remove(oldIconCls);
        }
    },

    getIconCls: function() {
        var icon = this._iconCls,
            iconCls;

        if (icon) {
            iconCls = icon.getCls();
            return (iconCls) ? iconCls[0] : null;
        }

        return null;
    },

    /**
     * @private
     */
    applyPrompt: function(prompt) {
        if (prompt) {
            var config = {
                label: false
            };

            if (Ext.isObject(prompt)) {
                Ext.apply(config, prompt);
            }

            if (config.multiLine) {
                config.height = Ext.isNumber(config.multiLine) ? parseFloat(config.multiLine) : this.getDefaultTextHeight();
                return Ext.factory(config, Ext.field['TextArea'], this.getPrompt());
            } else {
                return Ext.factory(config, Ext.field['Text'], this.getPrompt());
            }
        }

        return prompt;
    },

    /**
     * @private
     */
    updatePrompt: function(newPrompt, oldPrompt) {
        if (newPrompt) {
            this.add(newPrompt);
        }

        if (oldPrompt) {
            this.remove(oldPrompt);
        }
    },

    /**
     * @private
     * Pass `fn` config to show method instead.
     */
    onClick: function (button) {
        var me = this,
            msgBoxOptions = me.msgBoxOptions,
            prompt = me.getPrompt(),
            fn = msgBoxOptions.fn,
            which;

        if (button) {
            if (typeof fn == 'function') {
                button.disable();

                prompt = prompt ? prompt.getValue() : null;
                which = button.getItemId() || button.getText();

                me.on({
                    single: true,
                    hiddenchange: function () {
                        fn.call(msgBoxOptions.scope || me, which, prompt, msgBoxOptions);
                        button.enable();
                    }
                });
            }
        }

        me.hide();
    },

    /**
     * Displays the {@link Ext.MessageBox} with a specified configuration. All
     * display functions (e.g. {@link #method-prompt}, {@link #alert}, {@link #confirm})
     * on MessageBox call this function internally, although those calls
     * are basic shortcuts and do not support all of the config msgBoxOptions allowed here.
     *
     * Example usage:
     *
     *     @example
     *     Ext.Msg.show({
     *        title: 'Address',
     *        message: 'Please enter your address:',
     *        width: 300,
     *        buttons: Ext.MessageBox.OKCANCEL,
     *        multiLine: true,
     *        prompt : { maxlength : 180, autocapitalize : true },
     *        fn: function(buttonId) {
     *            alert('You pressed the "' + buttonId + '" button.');
     *        }
     *     });
     *
     * @param {Object} msgBoxOptions An object with the following config msgBoxOptions:
     *
     * @param {Object/Array} [msgBoxOptions.buttons=false]
     * A button config object or Array of the same(e.g., `Ext.MessageBox.OKCANCEL` or `{text:'Foo', itemId:'cancel'}`),
     * or false to not show any buttons.
     *
     * @param {String} msgBoxOptions.cls
     * A custom CSS class to apply to the message box's container element.
     *
     * @param {Function} msgBoxOptions.fn
     * A callback function which is called when the dialog is dismissed by clicking on the configured buttons.
     *
     * @param {String} msgBoxOptions.fn.buttonId The `itemId` of the button pressed, one of: 'ok', 'yes', 'no', 'cancel'.
     * @param {String} msgBoxOptions.fn.value Value of the input field if either `prompt` or `multiline` option is `true`.
     * @param {Object} msgBoxOptions.fn.opt The config object passed to show.
     *
     * @param {Number} [msgBoxOptions.width=auto]
     * A fixed width for the MessageBox.
     *
     * @param {Number} [msgBoxOptions.height=auto]
     * A fixed height for the MessageBox.
     *
     * @param {Object} msgBoxOptions.scope
     * The scope of the callback function
     *
     * @param {String} msgBoxOptions.icon
     * A CSS class that provides a background image to be used as the body icon for the dialog
     * (e.g. Ext.MessageBox.WARNING or 'custom-class').
     *
     * @param {Boolean} [msgBoxOptions.modal=true]
     * `false` to allow user interaction with the page while the message box is displayed.
     *
     * @param {String} [msgBoxOptions.message=&#160;]
     * A string that will replace the existing message box body text.
     * Defaults to the XHTML-compliant non-breaking space character `&#160;`.
     *
     * @param {Number} [msgBoxOptions.defaultTextHeight=75]
     * The default height in pixels of the message box's multiline textarea if displayed.
     *
     * @param {Boolean} [msgBoxOptions.prompt=false]
     * `true` to prompt the user to enter single-line text. Please view the {@link Ext.MessageBox#method-prompt} documentation in {@link Ext.MessageBox}.
     * for more information.
     *
     * @param {Boolean} [msgBoxOptions.multiline=false]
     * `true` to prompt the user to enter multi-line text.
     *
     * @param {String} msgBoxOptions.title
     * The title text.
     *
     * @param {String} msgBoxOptions.value
     * The string value to set into the active textbox element if displayed.
     *
     * @param {Object} [options] Options for {@link Ext.Panel#method!show}.
     *
     * @return {Ext.MessageBox} this
     */
    show: function (msgBoxOptions, options) {
        var me = this,
            buttons, config, prompt;

        Ext.util.InputBlocker.blockInputs();

        if (!msgBoxOptions) {
            return me.callParent(arguments);
        }

        config = Ext.apply({
            buttons: Ext.MessageBox.OK,
            draggable: false,
            prompt: null,
            defaultFocus: null
        }, msgBoxOptions);

        if (config.multiLine) {
            config.prompt = config.prompt || {};
            config.prompt.multiLine = config.multiLine;
            delete config.multiLine;
        }

        delete config.value;
        delete config.fn;
        delete config.scope;

        config = Ext.merge({}, me.defaultAllowedConfig, config);

        me.setConfig(config);
        me.msgBoxOptions = msgBoxOptions;

        buttons = me.getButtons();
        buttons.items.each(function (btn) {
            if (btn.isButton) {
                var value = btn.getScope();

                if (btn.fn && value) {
                    btn.fn = btn.fn.bind(value);
                }

                value = btn.getHandler();

                if (!value || value === 'me.onClick') {
                    btn.setHandler('onClick');
                    btn.setScope(me);
                }
            }
        });

        prompt = me.getPrompt();
        if (prompt) {
            prompt.setValue(msgBoxOptions.value || '');
        }

        me.callParent([null, options]);

        return me;
    },

    /**
     * Displays a standard read-only message box with an OK button (comparable to the basic JavaScript alert prompt). If
     * a callback function is passed it will be called after the user clicks the button, and the `itemId` of the button
     * that was clicked will be passed as the only parameter to the callback.
     *
     * @param {String} title The title bar text.
     * @param {String} message The message box body text.
     * @param {Function} [fn] A callback function which is called when the dialog is dismissed by clicking on the configured buttons.
     * @param {String} fn.buttonId The `itemId` of the button pressed, one of: 'ok', 'yes', 'no', 'cancel'.
     * @param {String} fn.value Value of the input field if either `prompt` or `multiLine` option is `true`.
     * @param {Object} fn.opt The config object passed to show.
     * @param {Object} [scope] The scope (`this` reference) in which the callback is executed.
     * Defaults to: the browser window
     *
     * @return {Ext.MessageBox} this
     */
    alert: function(title, message, fn, scope) {
        return this.show({
            title: title || null,
            message: message || null,
            buttons: Ext.MessageBox.OK,
            defaultFocus: '#ok',
            prompt: false,
            fn: function() {
                if (fn) {
                    Ext.callback(fn, scope, arguments);
                }
            },
            scope: scope
        });
    },

    /**
     * Displays a confirmation message box with Yes and No buttons (comparable to JavaScript's confirm). If a callback
     * function is passed it will be called after the user clicks either button, and the id of the button that was
     * clicked will be passed as the only parameter to the callback (could also be the top-right close button).
     *
     * @param {String} title The title bar text.
     * @param {String} message The message box body text.
     * @param {Function} fn A callback function which is called when the dialog is dismissed by clicking on the configured buttons.
     * @param {String} fn.buttonId The `itemId` of the button pressed, one of: 'ok', 'yes', 'no', 'cancel'.
     * @param {String} fn.value Value of the input field if either `prompt` or `multiLine` option is `true`.
     * @param {Object} fn.opt The config object passed to show.
     * @param {Object} [scope] The scope (`this` reference) in which the callback is executed.
     *
     * Defaults to: the browser window
     *
     * @return {Ext.MessageBox} this
     */
    confirm: function(title, message, fn, scope) {
        return this.show({
            title: title || null,
            message: message || null,
            buttons: Ext.MessageBox.YESNO,
            defaultFocus: '#yes',
            prompt: false,
            scope: scope,

            fn: function() {
                if (fn) {
                    Ext.callback(fn, scope, arguments);
                }
            }
        });
    },

    /**
     * Displays a message box with OK and Cancel buttons prompting the user to enter some text (comparable to
     * JavaScript's prompt). The prompt can be a single-line or multi-line textbox. If a callback function is passed it
     * will be called after the user clicks either button, and the id of the button that was clicked (could also be the
     * top-right close button) and the text that was entered will be passed as the two parameters to the callback.
     *
     * Example usage:
     *
     *     @example
     *     Ext.Msg.prompt(
     *         'Welcome!',
     *         'What\'s your name going to be today?',
     *         function (buttonId, value) {
     *             console.log(value);
     *         },
     *         null,
     *         false,
     *         null,
     *         {
     *             autoCapitalize: true,
     *             placeHolder: 'First-name please...'
     *         }
     *     );
     *
     * @param {String} title The title bar text.
     * @param {String} message The message box body text.
     * @param {Function} fn A callback function which is called when the dialog is dismissed by clicking on the configured buttons.
     * @param {String} fn.buttonId The `itemId` of the button pressed, one of: 'ok', 'yes', 'no', 'cancel'.
     * @param {String} fn.value Value of the input field if either `prompt` or `multiLine` option is `true`.
     * @param {Object} fn.opt The config object passed to show.
     * @param {Object} scope The scope (`this` reference) in which the callback is executed.
     *
     * Defaults to: the browser window.
     *
     * @param {Boolean/Number} [multiLine=false] `true` to create a multiline textbox using the `defaultTextHeight` property,
     * or the height in pixels to create the textbox.
     *
     * @param {String} [value] Default value of the text input element.
     *
     * @param {Object} [prompt=true]
     * The configuration for the prompt. See the {@link Ext.MessageBox#cfg-prompt prompt} documentation in {@link Ext.MessageBox}
     * for more information.
     *
     * @return {Ext.MessageBox} this
     */
    prompt: function(title, message, fn, scope, multiLine, value, prompt) {
        return this.show({
            title    : title || null,
            message  : message || null,
            buttons  : Ext.MessageBox.OKCANCEL,
            scope    : scope,
            prompt   : prompt || true,
            defaultFocus: 'textfield',
            multiLine: multiLine,
            value    : value,

            fn: function() {
                if (fn) {
                    Ext.callback(fn, scope, arguments);
                }
            }
        });
    }
}, function(MessageBox) {
    Ext.onInternalReady(function() {
        // #define Ext.Msg
        /**
         * @class Ext.Msg
         * @extends Ext.MessageBox
         * @singleton
         *
         * A global shared singleton instance of the {@link Ext.MessageBox} class.
         *
         * Allows for simple creation of various different alerts and notifications.
         *
         * To change any configurations on this singleton instance, you must change the
         * `defaultAllowedConfig` object.  For example to remove all animations on `Msg`:
         *
         *     Ext.Msg.defaultAllowedConfig.showAnimation = false;
         *     Ext.Msg.defaultAllowedConfig.hideAnimation = false;
         *
         * ## Examples
         *
         * ### Alert
         * Use the {@link #alert} method to show a basic alert:
         *
         *     @example
         *     Ext.Msg.alert('Title', 'The quick brown fox jumped over the lazy dog.', Ext.emptyFn);
         *
         * ### Prompt
         * Use the {@link #method-prompt} method to show an alert which has a textfield:
         *
         *     @example
         *     Ext.Msg.prompt('Name', 'Please enter your name:', function(text) {
         *         // process text value and close...
         *     });
         *
         * ### Confirm
         * Use the {@link #confirm} method to show a confirmation alert (shows yes and no buttons).
         *
         *     @example
         *     Ext.Msg.confirm("Confirmation", "Are you sure you want to do that?", Ext.emptyFn);
         */
        Ext.Msg = new Ext.MessageBox({
            id: 'ext-messagebox'
        });
    });
});

