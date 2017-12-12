/**
 * This class provides a push button with several presentation options. There are various
 * different styles of Button you can create by using the {@link #icon}, {@link #iconCls},
 * {@link #iconAlign}, {@link #ui}, and {@link #text} configurations.
 *
 * ## Simple Button
 *
 * Here is a Button in it's simplest form:
 *
 *     @example
 *     var button = Ext.create('Ext.Button', {
 *         text: 'Button'
 *     });
 *     Ext.Viewport.add({ xtype: 'container', padding: 10, items: [button] });
 *
 * ## Icons
 *
 * You can also create a Button with just an icon using the {@link #iconCls} configuration:
 *
 *     @example
 *     var button = Ext.create('Ext.Button', {
 *         iconCls: 'refresh'
 *     });
 *     Ext.Viewport.add({ xtype: 'container', padding: 10, items: [button] });
 *
 * Sencha provides the "Font" and "PNG" icons packs from http://wwww.pictos.cc.
 * Use icons with the {@link Global_CSS#icon icon} mixin in your Sass.
 *
 * ## Badges
 *
 * Buttons can also have a badge on them, by using the {@link #badgeText} configuration:
 *
 *     @example
 *     Ext.create('Ext.Container', {
 *         fullscreen: true,
 *         padding: 10,
 *         items: {
 *             xtype: 'button',
 *             text: 'My Button',
 *             badgeText: '2'
 *         }
 *     });
 *
 * ## Menus
 *
 * You can assign a menu to a button by using the {@link #cfg!menu} config. This config can be
 * either a reference to a {@link Ext.menu.Menu menu} instance or a {@link Ext.menu.Menu menu}
 * config object.
 *
 * When assigning a menu to a button, an arrow is automatically added to the button. You can
 * change the alignment of the arrow using the {@link #cfg!arrowAlign} config.
 *
 * Example usage:
 *
 *     @example
 *     Ext.create('Ext.Button', {
 *         text: 'Menu button',
 *         renderTo: Ext.getBody(),
 *         arrowAlign: 'bottom',
 *         menu: [
 *             { text: 'Item 1' },
 *             { text: 'Item 2' },
 *             { text: 'Item 3' },
 *             { text: 'Item 4' }
 *         ]
 *     });
 *
 * ## UI
 *
 * Buttons also come with a range of different default UIs. Here are the included UIs
 * available (if {@link #$include-button-uis $include-button-uis} is set to `true`):
 *
 * - **normal** - a basic gray button
 * - **back** - a back button
 * - **forward** - a forward button
 * - **round** - a round button
 * - **action** - shaded using the {@link Global_CSS#$active-color $active-color} (dark blue by default)
 * - **decline** - shaded using the {@link Global_CSS#$alert-color $alert-color} (red by default)
 * - **confirm** - shaded using the {@link Global_CSS#$confirm-color $confirm-color} (green by default)
 *
 * You can also append `-round` to each of the last three UI's to give it a round shape:
 *
 * - **action-round**
 * - **decline-round**
 * - **confirm-round**
 *
 * And setting them is very simple:
 *
 *     var uiButton = Ext.create('Ext.Button', {
 *         text: 'My Button',
 *         ui: 'action'
 *     });
 *
 * And how they look:
 *
 *     @example
 *     Ext.create('Ext.Container', {
 *         fullscreen: true,
 *         padding: 4,
 *         defaults: {
 *             xtype: 'button',
 *             margin: 5
 *         },
 *         layout: {
 *             type: 'vbox',
 *             align: 'center'
 *         },
 *         items: [
 *             { ui: 'normal', text: 'normal' },
 *             { ui: 'round', text: 'round' },
 *             { ui: 'action', text: 'action' },
 *             { ui: 'decline', text: 'decline' },
 *             { ui: 'confirm', text: 'confirm' }
 *         ]
 *     });
 *
 * Note that the default {@link #ui} is **normal**.
 *
 * You can also use the {@link #sencha-button-ui sencha-button-ui} CSS Mixin to create your own UIs.
 *
 * ## Example
 *
 * This example shows a bunch of icons on the screen in two toolbars. When you click on the center
 * button, it switches the {@link #iconCls} on every button on the page.
 *
 *     @example
 *     Ext.createWidget('container', {
 *         fullscreen: true,
 *         layout: {
 *             type: 'vbox',
 *             pack:'center',
 *             align: 'center'
 *         },
 *         items: [
 *             {
 *                 xtype: 'button',
 *                 text: 'Change iconCls',
 *                 handler: function() {
 *                     // classes for all the icons to loop through.
 *                     var availableIconCls = [
 *                         'action', 'add', 'arrow_down', 'arrow_left',
 *                         'arrow_right', 'arrow_up', 'compose', 'delete',
 *                         'organize', 'refresh', 'reply', 'search',
 *                         'settings', 'star', 'trash', 'maps', 'locate',
 *                         'home'
 *                     ];
 *                     // get the text of this button,
 *                     // so we know which button we don't want to change
 *                     var text = this.getText();
 *
 *                     // use ComponentQuery to find all buttons on the page
 *                     // and loop through all of them
 *                     Ext.Array.forEach(Ext.ComponentQuery.query('button'), function(button) {
 *                         // if the button is the change iconCls button, continue
 *                         if (button.getText() === text) {
 *                             return;
 *                         }
 *
 *                         // get the index of the new available iconCls
 *                         var index = availableIconCls.indexOf(button.getIconCls()) + 1;
 *
 *                         // update the iconCls of the button with the next iconCls, if one exists.
 *                         // if not, use the first one
 *                         button.setIconCls(availableIconCls[(index === availableIconCls.length) ? 0 : index]);
 *                     });
 *                 }
 *             },
 *             {
 *                 xtype: 'toolbar',
 *                 docked: 'top',
 *                 items: [
 *                     { xtype: 'spacer' },
 *                     { iconCls: 'action' },
 *                     { iconCls: 'add' },
 *                     { iconCls: 'arrow_down' },
 *                     { iconCls: 'arrow_left' },
 *                     { iconCls: 'arrow_up' },
 *                     { iconCls: 'compose' },
 *                     { iconCls: 'delete' },
 *                     { iconCls: 'organize' },
 *                     { iconCls: 'refresh' },
 *                     { xtype: 'spacer' }
 *                 ]
 *             },
 *             {
 *                 xtype: 'toolbar',
 *                 docked: 'bottom',
 *                 ui: 'light',
 *                 items: [
 *                     { xtype: 'spacer' },
 *                     { iconCls: 'reply' },
 *                     { iconCls: 'search' },
 *                     { iconCls: 'settings' },
 *                     { iconCls: 'star' },
 *                     { iconCls: 'trash' },
 *                     { iconCls: 'maps' },
 *                     { iconCls: 'locate' },
 *                     { iconCls: 'home' },
 *                     { xtype: 'spacer' }
 *                 ]
 *             }
 *         ]
 *     });
 *
 */
Ext.define('Ext.Button', {
    extend: 'Ext.Component',
    xtype: 'button',

    isButton: true,

    /**
     * @event tap
     * @preventable
     * Fires whenever a button is tapped.
     * @param {Ext.Button} this The item added to the Container.
     * @param {Ext.EventObject} e The event object.
     */

    /**
     * @event release
     * @preventable
     * Fires whenever the button is released.
     * @param {Ext.Button} this The item added to the Container.
     * @param {Ext.EventObject} e The event object.
     */

    cachedConfig: {
        /**
         * @cfg {String} buttonType
         * By default, all buttons have `type="button"`. If a button is intended to be invoked as
         * the default action button inside an {@link Ext.form.Panel}, then setting this to `'submit'`
         * will cause the button to be clicked whenever the `ENTER` key is pressed.
         *
         * @since 6.5.0
         */
        buttonType: 'button',

        /**
         * @cfg {String} iconCls
         * One or more space separated CSS classes to be applied to the icon element.
         * The CSS rule(s) applied should specify a background image to be used as the
         * icon.
         *
         * An example of specifying a custom icon class would be something like:
         *
         *     // specify the property in the config for the class:
         *     iconCls: 'my-home-icon'
         *
         *     // css rule specifying the background image to be used as the icon image:
         *     .my-home-icon {
         *         background-image: url(../images/my-home-icon.gif) !important;
         *     }
         *
         * In addition to specifying your own classes, you can use the font icons
         * provided in the SDK using the following syntax:
         *
         *     // using Font Awesome
         *     iconCls: 'x-fa fa-home'
         *
         *     // using Pictos
         *     iconCls: 'pictos pictos-home'
         *
         * Depending on the theme you're using, you may need include the font icon
         * packages in your application in order to use the icons included in the
         * SDK.  For more information see:
         *
         *  - [Font Awesome icons](http://fontawesome.io/cheatsheet/)
         *  - [Pictos icons](../guides/core_concepts/font_ext.html)
         *  - [Theming Guide](../guides/core_concepts/theming.html)
         * @accessor
         */
        iconCls: null,

        /**
         * @cfg {"left"/"right"/"center"} [textAlign="center"]
         * @since 6.0.1
         */
        textAlign: null,

        /**
         * @cfg {String} menuAlign
         * The position to align the menu to (see {@link Ext.util.Positionable#alignTo} for more details).
         */
        menuAlign: 'tl-bl?',

        /**
         * @cfg {Boolean} destroyMenu
         * Whether or not to destroy any associated menu when this button is destroyed.
         * In addition, a value of `true` for this config will destroy the currently bound menu
         * when a new menu is set in {@link #setMenu} unless overridden by that method's destroyMenu
         * function argument.
         */
        destroyMenu: true,

        /**
         * @cfg {Boolean} stretchMenu
         * Configure as `true` if the cfg of this button's. {@link #cfg!menu} should
         * at least match the width of this button. An {@link #minWidth} explicit `minWidth` on
         * the menu will override this.
         * @since 6.5.1
         */
        stretchMenu: false,

        /**
         * @cfg eventHandlers
         * @inheritdoc
         */
        eventHandlers: {
            click: 'onClick'
        }
    },

    config: {
        /**
         * @cfg {Boolean} allowDepress
         * `true` to allow user interaction to set {@link #pressed} to `false` when
         * the button is in the {@link #pressed} state. Only valid when {@link #pressed}
         * is configured.
         *
         * @since 6.0.2
         */
        allowDepress: true,

        /**
         * @cfg {String} badgeText
         * Optional badge text.  Badges appear as small numbers, letters, or icons that sit on top of your button.  For instance, a small red number indicating how many updates are available.
         * @accessor
         */
        badgeText: null,

        /**
         * @cfg {String} text
         * The Button text.
         * @accessor
         */
        text: null,

        /**
         * @cfg {String} icon
         * Url to the icon image to use if you want an icon to appear on your button.
         * @accessor
         */
        icon: false,

        /**
         * @cfg {'top'/'right'/'bottom'/'left'} iconAlign
         * The position of the icon relative to the button text
         */
        iconAlign: 'left',

        /**
         * @cfg {Number/Boolean} pressedDelay
         * The amount of delay between the `mousedown` or `touchstart` and the moment the
         * button receives "pressed" styling.
         * Settings this to `true` defaults to 100ms.
         */
        pressedDelay: 0,

        // @cmd-auto-dependency { defaultType: "Ext.menu.Menu", requires: ["Ext.menu.Menu"] }
        /**
         * @cfg {Ext.menu.Menu/String/Object} menu
         * A menu or menu configuration. This can be a reference to a menu instance, a menu
         * config object or the `xtype` alias of a {@link Ext.menu.Menu menu}-derived class.
         */
        menu: {
            lazy: true,
            $value: null
        },

        /**
         * @cfg {Boolean} [arrow=true]
         * By default, if the button has a {@link #cfg!menu}, an arrow icon is shown to indicate this.
         *
         * Configure `arrow: false` to hide the menu arrow.
         */
        arrow: null,

        /**
         * @cfg {"right"/"bottom"} arrowAlign
         * The side of the Button box to render the arrow if the button has an associated
         * {@link #cfg!menu}.
         */
        arrowAlign: 'right',

        /**
         * @cfg {Function} handler
         * @cfg {Ext.Button} handler.button This Button.
         * @cfg {Ext.event.Event} handler.e The triggering event.
         * The handler function to run when the Button is tapped on.
         * @accessor
         */
        handler: null,

        /**
         * @cfg {Function} toggleHandler
         * @cfg {Ext.Button} toggleHandler.button This Button.
         * @cfg {Boolean} toggleHandler.pressed This Button's new pressed state.
         * The handler function to run when the Button is toggled. Supplying this
         * configuration implies `{@link #cfg!enableToggle}` is `true`.
         * @accessor
         */
        toggleHandler: null,

        /**
         * @cfg {Object} scope
         * The scope (`this` refeence) in which the configured {@link #handler} will be executed,
         * unless the scope is a ViewController method nmame.
         * @accessor
         */
        scope: null,

        /**
         * @cfg {String} autoEvent
         * Optional event name that will be fired instead of `tap` when the Button is tapped on.
         * @accessor
         */
        autoEvent: null,

        /**
         * @cfg {String} ui
         * The ui style to render this button with. The valid default options are:
         *
         * - `null` - a basic gray button (default).
         * - `'back'` - a back button.
         * - `'forward'` - a forward button.
         * - `'round'` - a round button.
         * - `'plain'`
         * - `'action'` - shaded using the {@link Global_CSS#$active-color $active-color} (dark blue by default).
         * - `'decline'` - shaded using the {@link Global_CSS#$alert-color $alert-color} (red by default).
         * - `'confirm'` - shaded using the {@link Global_CSS#$confirm-color $confirm-color} (green by default).
         *
         * You can also append `-round` to each of the last three UI's to give it a round shape:
         *
         * - **action-round**
         * - **decline-round**
         * - **confirm-round**
         *
         * @accessor
         */
        ui: null,

        /**
         * @cfg {String} html The HTML to put in this button.
         *
         * If you want to just add text, please use the {@link #text} configuration.
         */

        /**
         * @cfg {Boolean} enableToggle
         * Allows this button to have the pressed state toggled via user
         * interaction.
         *
         * @since 6.0.2
         */
        enableToggle: false,

        /**
         * @cfg {String/Number} value
         * The value of this button.  Only applicable when used as an item of a {@link Ext.SegmentedButton Segmented Button}.
         */
        value: null
    },

    eventedConfig: {
        /**
         * @cfg {Boolean} pressed
         * Sets the pressed state of the button.
         *
         * @since 6.0.2
         */
        pressed: false
    },

    /**
     * @private
     */
    preventDefaultAction: true,

    isMenuOwner: true,

    /**
     * @property baseCls
     * @inheritdoc
     */
    baseCls: Ext.baseCSSPrefix + 'button',
    hasMenuCls: Ext.baseCSSPrefix + 'has-menu',
    hoveredCls: Ext.baseCSSPrefix + 'hovered',
    pressedCls: Ext.baseCSSPrefix + 'pressed',
    pressingCls: Ext.baseCSSPrefix + 'pressing',
    hasBadgeCls: Ext.baseCSSPrefix + 'has-badge',
    hasIconCls: Ext.baseCSSPrefix + 'has-icon',
    hasTextCls: Ext.baseCSSPrefix + 'has-text',
    hasArrowCls: Ext.baseCSSPrefix + 'has-arrow',
    noArrowCls: Ext.baseCSSPrefix + 'no-arrow',

    /**
     * @property defaultBindProperty
     * @inheritdoc
     */
    defaultBindProperty: 'text',
    
    /**
     * @cfg publishes
     * @inheritdoc
     */
    publishes: ['pressed'],

    /**
     * @property element
     * @inheritdoc
     */
    element: {
        reference: 'element',
        onclick: 'return Ext.doEv(this, event);'
    },

    /**
     * @property focusable
     * @inheritdoc
     */
    focusable: true,
    
    /**
     * @property focusEl
     * @inheritdoc
     */
    focusEl: 'buttonElement',
    
    /**
     * @property ariaEl
     * @inheritdoc
     */
    ariaEl: 'buttonElement',
    backgroundColorEl: 'innerElement',
    
    /**
     * @property focusClsEl
     * @inheritdoc
     */
    focusClsEl: 'el',

    initialize: function() {
        var me = this,
            el = me.el;

        me.callParent();

        // The menu config is lazy
        if (me.getConfig('menu', true)) {
            me.addCls(me.hasMenuCls);
        }

        el.on({
            scope: me,
            touchstart: 'onPress'
        });

        el.addClsOnOver(me.hoveredCls, me.isEnabled, me);
    },

    getTemplate: function() {
        return [{
            reference: 'innerElement',
            cls: Ext.baseCSSPrefix + 'inner-el',
            children: [{
                reference: 'bodyElement',
                cls: Ext.baseCSSPrefix + 'body-el',
                children: [{
                    cls: Ext.baseCSSPrefix + 'icon-el ' + Ext.baseCSSPrefix + 'font-icon',
                    reference: 'iconElement'
                }, {
                    reference: 'textElement',
                    cls: Ext.baseCSSPrefix + 'text-el'
                }]
            }, {
                reference: 'arrowElement',
                cls: Ext.baseCSSPrefix + 'arrow-el ' + Ext.baseCSSPrefix + 'font-icon'
            }]
        }, {
            reference: 'badgeElement',
            cls: Ext.baseCSSPrefix + 'badge-el'
        },
            this.getButtonTemplate()];
    },

    /**
     * @private
     * Returns a for an absolutely positioned transparent button element that overlays the
     * entire component and captures tabs and clicks for accessibility purposes.
     *
     * Overridden by {@link Ext.field.FileButton} to replace the `<button` element with
     * an `<input type="file">` element.
     */
    getButtonTemplate: function() {
        return {
            tag: 'button',
            reference: 'buttonElement',
            cls: Ext.baseCSSPrefix + 'button-el',
            onfocus: 'return Ext.doEv(this, event);',
            onblur: 'return Ext.doEv(this, event);'
        };
    },

    /**
     * @private
     * Intercept ripple config to return unbound ripples for icon only buttons
     */
    shouldRipple: function(e) {
        var me = this,
            ui = me.getUi(),
            ripple = me.getRipple(),
            isFab = ui ? ui.split(" ").indexOf("fab") >= 0 : false,
            text, icon;


        if (!isFab && ripple && ripple.bound === undefined) {
            text = me.getText();
            icon = me.getIconCls();

            if ((!text || text.length === 0) && icon) {
                ripple = Ext.clone(ripple);
                ripple.bound = false;
                ripple.measureSelector = 'x-icon-el';
            }
        }

        return ripple;
    },

    /**
     * `true` if this button is currently in a pressed state. See {@link #pressed}.
     * @return {Boolean} The pressed state.
     *
     * @since 6.0.2
     */
    isPressed: function() {
        return Boolean(this.getPressed());
    },

    /**
     * Toggles the {@link #pressed} state.
     *
     * @since 6.0.2
     */
    toggle: function() {
        this.setPressed(!this.isPressed());
    },

    updateBadgeText: function(badgeText) {
        var me = this,
            el = me.el,
            badgeElement = me.badgeElement,
            hasBadgeCls = me.hasBadgeCls;

        if (badgeText) {
            badgeElement.setText(badgeText);
            el.addCls(hasBadgeCls);
        } else {
            el.removeCls(hasBadgeCls);
        }
    },

    updateButtonType: function(buttonType) {
        this.buttonElement.dom.setAttribute('type', buttonType);
    },

    updateText: function(text) {
        this.textElement.setHtml(text);
        this.toggleCls(this.hasTextCls, !!text);
    },

    updateHtml: function(html) {
        this.setText(html);
    },

    applyPressed: function(pressed) {
        return Boolean(pressed);
    },

    updatePressed: function(pressed) {
        var me = this,
            toggleHandler = me.getToggleHandler();

        if (toggleHandler && !me.isConfiguring) {
            Ext.callback(toggleHandler, me.getScope(), [me, pressed], 0, me);
        }
        me.element.toggleCls(me.pressedCls, pressed);
    },

    updateIcon: function(icon) {
        var me = this,
            element = me.iconElement,
            hasIconCls = me.hasIconCls;

        if (icon) {
            me.addCls(hasIconCls);
            element.setStyle('background-image', 'url(' + icon + ')');
        } else {
            element.setStyle('background-image', '');
            if (!me.getIconCls()) {
                me.removeCls(hasIconCls);
            }
        }
    },

    updateIconCls: function(iconCls, oldIconCls) {
        var me = this,
            element = me.iconElement,
            hasIconCls = me.hasIconCls;

        if (iconCls) {
            me.addCls(hasIconCls);
            element.replaceCls(oldIconCls, iconCls);
        } else {
            element.removeCls(oldIconCls);
            if (!me.getIcon()) {
                me.removeCls(hasIconCls);
            }
        }
    },

    updateIconAlign: function(iconAlign, oldIconAlign) {
        var el = this.el,
            prefix = Ext.baseCSSPrefix + 'icon-align-';

        el.removeCls(prefix + oldIconAlign);
        el.addCls(prefix + iconAlign);
    },

    _textAlignCls: {
        left: Ext.baseCSSPrefix + 'text-align-left',
        right: Ext.baseCSSPrefix + 'text-align-right',
        center: ''
    },

    updateTextAlign: function(textAlign, oldTextAlign) {
        var textAlignClasses = this._textAlignCls,
            add = textAlignClasses[textAlign || 'center'],
            remove = textAlignClasses[oldTextAlign || 'center'];

        this.replaceCls(remove, add);
    },

    updateArrowAlign: function(align, oldAlign) {
        var element = this.element,
            cls = Ext.baseCSSPrefix + 'arrow-align-';

        if (oldAlign) {
            element.removeCls(cls + oldAlign);
        }

        element.addCls(cls + align);
    },

    applyMenu: function(menu) {
        if (menu) {
            if (!menu.isMenu) {
                if (Ext.isArray(menu)) {
                    menu = {
                        items: menu
                    };
                }

                if (!menu.xtype) {
                    menu.xtype = 'menu';
                }

                menu.ownerCmp = this;
                menu = Ext.widget(menu);
            }

            this.menuMinWidth = menu.getMinWidth();
        }

        return menu;
    },

    updateMenu: function(menu, oldMenu) {
        var listener = {
            scope: this,
            hide: 'onMenuHide'
        };

        if (oldMenu && !oldMenu.destroyed) {
            if (this.getDestroyMenu()) {
                oldMenu.destroy();
            } else if (oldMenu.isMenu) {
                oldMenu.un(listener);
            }
        }

        this.toggleCls(this.hasMenuCls, !!menu);

        if (menu && menu.isMenu) {
            menu.on(listener);
        }
    },

    updateArrow: function(arrow) {
        this.toggleCls(this.noArrowCls, !arrow);
        this.toggleCls(this.hasArrowCls, !!arrow);
    },

    applyAutoEvent: function(autoEvent) {
        var me = this;

        if (typeof autoEvent == 'string') {
            autoEvent = {
                name: autoEvent,
                scope: me.scope || me
            };
        }

        return autoEvent;
    },

    updateAutoEvent: function(autoEvent) {
        var name = autoEvent.name,
            scope = autoEvent.scope;

        this.setHandler(function() {
            scope.fireEvent(name, scope, this);
        });

        this.setScope(scope);
    },

    applyPressedDelay: function(delay) {
        if (Ext.isNumber(delay)) {
            return delay;
        }
        return (delay) ? 100 : 0;
    },

    enableFocusable: function() {
        this.buttonElement.dom.disabled = false;

        this.callParent();
    },

    disableFocusable: function() {
        this.callParent();

        this.buttonElement.dom.disabled = true;
    },

    /**
     * @private
     */
    onPress: function(e) {
        var me = this,
            element = me.element,
            pressedDelay = me.getPressedDelay(),
            pressingCls = me.pressingCls;

        // Do not react if disabled, or it's a contextmenu event (right click)
        if (!me.getDisabled() && !e.button) {
            if (pressedDelay > 0) {
                me.pressedTimeout = Ext.defer(function() {
                    delete me.pressedTimeout;
                    if (element) {
                        element.addCls(pressingCls);
                    }
                }, pressedDelay);
            } else {
                element.addCls(pressingCls);
            }
            Ext.GlobalEvents.setPressedComponent(me, e);
        }
    },

    /**
     * Called by {@link Ext.GlobalEvents#setPressedComponent} when the global
     * mouseup event fires and there's a registered pressed component.
     * @private
     */
    onRelease: function(e) {
        this.fireAction('release', [this, e], 'doRelease');
    },

    /**
     * @private
     */
    doRelease: function(me, e) {
        if (!me.getDisabled()) {
            if (me.hasOwnProperty('pressedTimeout')) {
                Ext.undefer(me.pressedTimeout);
                delete me.pressedTimeout;
            } else {
                me.element.removeCls(me.pressingCls);
            }
        }
    },

    onClick: function(e) {
        return this.onTap(e);
    },

    /**
     * @private
     */
    onTap: function(e) {
        if (this.getDisabled()) {
            return false;
        }

        this.fireAction('tap', [this, e], 'doTap');
    },

    /**
     * @private
     */
    doTap: function(me, e) {
        var menu = me.getMenu(),
            handler = me.getHandler();

        // this is done so if you hide the button in the handler, the tap event will not fire
        // on the new element where the button was.
        if (e && e.preventDefault && me.preventDefaultAction) {
            e.preventDefault();
        }

        if (menu) {
            me.toggleMenu(e, menu);
        }
        else {
            if ((me.getToggleHandler() || me.getEnableToggle()) && (me.getAllowDepress() || !me.isPressed())) {
                me.toggle();
            }

            if (handler) {
                Ext.callback(handler, me.getScope(), [me, e], 0, me);
            }
        }
    },

    onEnterKey: function(e) {
        this.onTap(e);

        e.stopEvent();

        return false;
    },

    onDownKey: function(e) {
        var menu = this.getMenu();

        if (menu && !this.getDisabled()) {
            this.showMenu(e, menu);

            e.stopEvent();

            return false;
        }
    },

    onEscKey: function(e) {
        var menu = this.getMenu();

        if (menu && !this.getDisabled() && menu.isVisible()) {
            menu.hide();

            e.stopEvent();

            return false;
        }
    },

    onFocus: function(e) {
        if (!this.keyHandlersAdded) {
            this.setKeyMap({
                scope: 'this',
                SPACE: 'onEnterKey',
                ENTER: 'onEnterKey',
                DOWN: 'onDownKey',
                ESC: 'onEscKey'
            });
            this.keyHandlersAdded = true;
        }
        this.callParent([e]);
    },

    onMenuHide: function (menu) {
        if (menu.isMenu && !this.$buttonWasPressed) {
            this.setPressed(false);
        }
    },

    toggleMenu: function (e, menu) {
        var me = this;

        menu = menu || me.getMenu();

        if (menu) {
            if (menu.isVisible()) {
                me.hideMenu(e, menu);
            } else {
                me.showMenu(e, menu);
            }
        }
    },

    hideMenu: function (e, menu) {
        menu = menu || this.getMenu();

        if (menu) {
            menu.hide();
        }
    },

    showMenu: function(e, menu) {
        var me = this,
            isPointerEvent = !e || e.pointerType,
            pressed;

        menu = menu || me.getMenu();

        me.setupMenuStretch(menu);

        if (menu) {
            if (menu.isVisible()) {
                // Click/tap toggles the menu visibility.
                if (isPointerEvent) {
                    menu.hide();
                }
                else {
                    menu.focus();
                }
            }
            else {
                menu.autoFocus = !isPointerEvent;

                if (menu.isMenu) {
                    /*
                     * We need to keep track if this button was already
                     * pressed when the menu was being shown so when the
                     * menu hides, we don't unpress the button when it should
                     * stay pressed.
                     */
                    me.$buttonWasPressed = pressed = me.getPressed();

                    menu.showBy(me.element, me.getMenuAlign());

                    if (!pressed) {
                        me.setPressed(true);
                    }
                } else if (menu.isViewportMenu) {
                    menu.setDisplayed(!menu.getDisplayed());
                } else {
                    menu.show();
                }
            }
        }
    },

    doDestroy: function() {
        var me = this;

        if (me.hasOwnProperty('pressedTimeout')) {
            Ext.undefer(me.pressedTimeout);
        }

        me.setMenu(null);

        me.callParent();
    },

    getFocusClsEl: function() {
        return this.element;
    },

    privates: {
        setupMenuStretch: function(menu) {
            var me = this;
            // Only stretch to our width if the menu doesn't already have a minWidth
            if (!me.menuMinWidth) {
                if (me.getStretchMenu()) {
                    menu.setMinWidth(me.el.measure('w'));
                } else {
                    menu.setMinWidth(null);
                }
            }
        }
    }
});
