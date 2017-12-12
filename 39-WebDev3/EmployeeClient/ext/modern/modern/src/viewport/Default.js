/**
 * @private
 * Base class for iOS and Android viewports.
 */
Ext.define('Ext.viewport.Default', function() {
    var TOP = 1,
        RIGHT = 2,
        BOTTOM = 4,
        LEFT = 8,
        sideMap = {
            top: TOP,
            right: RIGHT,
            bottom: BOTTOM,
            left: LEFT
        },
        oppositeSide = {
            "1": BOTTOM,
            "2": LEFT,
            "4": TOP,
            "8": RIGHT
        },
        oppositeSideNames = {
            left: 'right',
            right: 'left',
            top: 'bottom',
            bottom: 'top',
            up: 'bottom',
            down: 'top'
        };

    return {
        extend: 'Ext.Container',

        xtype: 'viewport',

        PORTRAIT: 'portrait',

        LANDSCAPE: 'landscape',

        requires: [
            'Ext.GlobalEvents',
            'Ext.layout.Card',
            'Ext.util.InputBlocker'
        ],

        nameHolder: true,

        /**
         * @event ready
         * Fires when the Viewport is in the DOM and ready.
         * @param {Ext.Viewport} this
         */

        /**
         * @event maximize
         * Fires when the Viewport is maximized.
         * @param {Ext.Viewport} this
         */

        /**
         * @event orientationchange
         * Fires when the Viewport orientation has changed.
         * @param {Ext.Viewport} this
         * @param {String} newOrientation The new orientation.
         * @param {Number} width The width of the Viewport.
         * @param {Number} height The height of the Viewport.
         */

        config: {
            /**
             * @private
             */
            autoMaximize: false,

            /**
             * @private
             *
             * Auto blur the focused element when touching on a non-input. This is used to work around Android bugs
             * where the virtual keyboard is not hidden when tapping outside an input.
             */
            autoBlurInput: true,

            /**
             * @cfg {Boolean} preventZooming
             * `true` to attempt to stop zooming when you double tap on the screen on mobile devices,
             * typically HTC devices with HTC Sense UI.
             * @accessor
             */
            preventZooming: false,

            /**
             * @cfg
             * @private
             */
            autoRender: true,

            /**
             * @cfg {Object/String} layout Configuration for this Container's layout. Example:
             *
             *     Ext.create('Ext.Container', {
             *         layout: {
             *             type: 'hbox',
             *             align: 'middle'
             *         },
             *         items: [
             *             {
             *                 xtype: 'panel',
             *                 flex: 1,
             *                 style: 'background-color: red;'
             *             },
             *             {
             *                 xtype: 'panel',
             *                 flex: 2,
             *                 style: 'background-color: green'
             *             }
             *         ]
             *     });
             *
             * @accessor
             */
            layout: 'card',

            /**
             * @cfg
             * @private
             */
            width: '100%',

            /**
             * @cfg
             * @private
             */
            height: '100%',

            /**
             * An object of all the menus on this viewport.
             * @private
             */
            menus: {},

            /**
             * @private
             */
            orientation: null,

            /**
             * @cfg {Number} swipeThreshold
             * The minimum distance an edge swipe must traverse in order to trigger showing
             * an edge menu.
             *
             * Note that reversing an edge swipe gesture back towards the edge aborts showing
             * that side's edge menu.
             */
            swipeThreshold: 30
        },

        classCls: Ext.baseCSSPrefix + 'viewport',

        getTemplate: function() {
            var template = this.callParent();

            // Used in legacy browser that do not support matchMedia. Hidden element is used for checking of orientation
            if (!Ext.feature.has.MatchMedia) {
                template.unshift({
                    reference: 'orientationElement',
                    className: Ext.baseCSSPrefix + 'orientation-inspector',
                    children: [{
                        className: Ext.baseCSSPrefix + 'orientation-inspector-landscape'
                    }]
                });
            }

            return template;
        },

        /**
         * @property {Boolean} isReady
         * `true` if the DOM is ready.
         */
        isReady: false,

        isViewport: true,

        isMaximizing: false,

        id: 'ext-viewport',

        isInputRegex: /^(input|textarea|select|a)$/i,

        isInteractiveWebComponentRegEx: /^(audio|video)$/i,

        notScalableRe: /user-scalable=no/,

        focusable: false,
        focusEl: null,
        ariaEl: null,

        allSidesCls: [
            Ext.baseCSSPrefix + 'top',
            Ext.baseCSSPrefix + 'right',
            Ext.baseCSSPrefix + 'bottom',
            Ext.baseCSSPrefix + 'left'
        ],

        sideClsMap: {
            top: Ext.baseCSSPrefix + 'top',
            right: Ext.baseCSSPrefix + 'right',
            bottom: Ext.baseCSSPrefix + 'bottom',
            left: Ext.baseCSSPrefix + 'left'
        },

        hasViewportCls: Ext.baseCSSPrefix + 'has-viewport',
        fixedCls: Ext.baseCSSPrefix + 'fixed-viewport',

        /**
         * @private
         */
        fullscreenItemCls: Ext.baseCSSPrefix + 'fullscreen',

        constructor: function(config) {
            var me = this;

            me.doPreventPanning = me.doPreventPanning.bind(me);
            me.doPreventZooming = me.doPreventZooming.bind(me);

            me.maximizeOnEvents = [
              'ready',
              'orientationchange'
            ];

            // set default devicePixelRatio if it is not explicitly defined
            window.devicePixelRatio = window.devicePixelRatio || 1;

            me.callParent([config]);

            me.updateSize();
            me.windowOuterHeight = me.getWindowOuterHeight();

            // The global scroller is our scroller.
            // We must provide a non-scrolling one if we are not configured to scroll,
            // otherwise the deferred ready listener in Scroller will create
            // one with scroll: true
            Ext.setViewportScroller(me.getScrollable() || Ext.getViewportScroller().setConfig({
                x: false,
                y: false,
                component: me
            }));

            // The body has to be overflow:hidden
            Ext.getBody().setStyle('overflow', 'hidden');

            Ext.get(document.documentElement).addCls(me.hasViewportCls);

            me.stretchHeights = me.stretchHeights || {};

            if (Ext.feature.has.OrientationChange) {
                me.addWindowListener('orientationchange', me.onOrientationChange.bind(me));
            }

            if (!Ext.os.is.iOS || !me.isScalable()) {
                Ext.get(document.documentElement).addCls(me.fixedCls);
            }

            // Tale over firing the resize event to sync the Viewport first, then fire the event.
            Ext.GlobalEvents.on('resize', 'onWindowResize', me, {priority: 1000});

            Ext.onDocumentReady(me.onDomReady, me);

            return me;
        },

        initialize: function() {
            var me = this;

            me.addMeta('apple-mobile-web-app-capable', 'yes');
            me.addMeta('apple-touch-fullscreen', 'yes');

            me.callParent();
        },

        getRefItems: function(deep) {
            var menus = this.getMenus(),
                result = this.callParent([deep]),
                side, menu;

            for (side in menus) {
                menu = menus[side];

                if (menu) {
                    Ext.Array.include(result, menu);
                }
            }

            return result;
        },

        initInheritedState: function (inheritedState, inheritedStateInner) {
            var me = this,
                root = Ext.rootInheritedState;

            if (inheritedState !== root) {
                // We need to go at this again but with the rootInheritedState object. Let
                // any derived class poke on the proper object!
                me.initInheritedState(me.inheritedState = root,
                    me.inheritedStateInner = Ext.Object.chain(root));
            } else {
                me.callParent([inheritedState, inheritedStateInner]);
            }
        },

        onAppLaunch: function() {
            var me = this;
            if (!me.isReady) {
                me.onDomReady();
            }
        },

        onDomReady: function() {
            var me = this;

            if (me.isReady) {
                return;
            }

            me.isReady = true;
            me.updateSize();
            me.onReady();
            me.fireEvent('ready', me);
            Ext.GlobalEvents.fireEvent('viewportready', me);
        },

        onReady: function() {
            if (this.getAutoRender()) {
                this.render();
            }
        },

        render: function() {
            var me = this,
                body = Ext.getBody();

            if (!me.rendered) {
                // Render ourself *before* any existing floatRoot so that floateds
                // are always on top.
                me.callParent([body, Ext.floatRoot]);

                me.setOrientation(me.determineOrientation());
                Ext.getBody().addCls(Ext.baseCSSPrefix + me.getOrientation());
            }
        },

        applyAutoMaximize: function(autoMaximize) {
            return Ext.browser.is.WebView ? false : autoMaximize;
        },

        updateAutoMaximize: function(autoMaximize) {
            var me = this;

            if (autoMaximize) {
                me.on('ready', 'doAutoMaximizeOnReady', me, { single: true });
                me.on('orientationchange', 'doAutoMaximizeOnOrientationChange', me);
            } else {
                me.un('ready', 'doAutoMaximizeOnReady', me);
                me.un('orientationchange', 'doAutoMaximizeOnOrientationChange', me);
            }
        },

        updatePreventPanning: function(preventPanning) {
            this.toggleWindowListener(preventPanning, 'touchmove', this.doPreventPanning, false);
        },

        updatePreventZooming: function(preventZooming) {
            var touchstart = Ext.feature.has.TouchEvents ? 'touchstart' : 'mousedown';
            this.toggleWindowListener(preventZooming, touchstart, this.doPreventZooming, false);
        },

        doAutoMaximizeOnReady: function() {
            var me = this;

            me.isMaximizing = true;

            me.on('maximize', function() {
                me.isMaximizing = false;

                me.updateSize();

                me.fireEvent('ready', me);
            }, me, { single: true });

            me.maximize();
        },

        doAutoMaximizeOnOrientationChange: function() {
            var me = this;

            me.isMaximizing = true;

            me.on('maximize', function() {
                me.isMaximizing = false;

                me.updateSize();
            }, me, { single: true });

            me.maximize();
        },

        doPreventPanning: function(e) {
            var target = e.target, 
                touch;

            // If we have an interaction on a WebComponent we need to check the actual shadow dom element selected
            // to determine if it is an input before preventing default behavior
            // Side effect to this is if the shadow input does not do anything with 'touchmove' the user could pan
            // the screen.
            if (this.isInteractiveWebComponentRegEx.test(target.tagName) && e.touches && e.touches.length > 0) {
                touch = e.touches[0];
                if (touch && touch.target && this.isInputRegex.test(touch.target.tagName)) {
                    return;
                }
            }

            if (target && target.nodeType === 1 && !this.isInputRegex.test(target.tagName)) {
                e.preventDefault();
            }
        },

        doPreventZooming: function(e) {
            // Don't prevent right mouse event
            if ('button' in e && e.button !== 0) {
                return;
            }

            var target = e.target, 
                inputRe = this.isInputRegex,
                touch;

            if (this.isInteractiveWebComponentRegEx.test(target.tagName) && e.touches && e.touches.length > 0) {
                touch = e.touches[0];
                if (touch && touch.target && inputRe.test(touch.target.tagName)) {
                    return;
                }
            }

            if (target && target.nodeType === 1 && !inputRe.test(target.tagName)) {
                e.preventDefault();
            }
        },

        addWindowListener: function(eventName, fn, capturing) {
            window.addEventListener(eventName, fn, Boolean(capturing));
        },

        removeWindowListener: function(eventName, fn, capturing) {
            window.removeEventListener(eventName, fn, Boolean(capturing));
        },

        supportsOrientation: function() {
            return Ext.feature.has.Orientation;
        },

        supportsMatchMedia: function() {
            return Ext.feature.has.MatchMedia;
        },

        onOrientationChange: function() {
            this.setOrientation(this.determineOrientation());
        },

        determineOrientation: function() {
            var me = this,
                orientationElement = me.orientationElement,
                nativeOrientation, visible;

            // First attempt will be to use Native Orientation information
            if (me.supportsOrientation()) {
                nativeOrientation = me.getWindowOrientation();
                // 90 || -90 || 270 is landscape
                if (Math.abs(nativeOrientation) === 90 || nativeOrientation === 270) {
                    return me.LANDSCAPE;
                } else {
                    return me.PORTRAIT;
                }
                // Second attempt will be to use MatchMedia and a media query
            } else if (me.supportsMatchMedia()) {
                return window.matchMedia('(orientation : landscape)').matches ? me.LANDSCAPE : me.PORTRAIT;
                // Fall back on hidden element with media query attached to it (media query in Base Theme)
            } else if (orientationElement) {
                visible = orientationElement.first().isVisible();
                return visible ? me.LANDSCAPE : me.PORTRAIT;
            }

            return null;
        },

        updateOrientation: function(newValue, oldValue) {
            if (oldValue) {
                this.fireOrientationChangeEvent(newValue, oldValue);
            }
        },

        fireOrientationChangeEvent: function(newOrientation, oldOrientation) {
            var me = this,
                newSize = me.updateSize();

            Ext.getBody().replaceCls(Ext.baseCSSPrefix + oldOrientation, Ext.baseCSSPrefix + newOrientation);

            me.fireEvent('orientationchange', me, newOrientation, newSize.width, newSize.height);
        },

        onWindowResize: function(width, height) {
            var me = this,
                oldWidth = me.lastSize.width,
                oldHeight = me.lastSize.height;

            me.updateSize(width, height);

            // On devices that do not support native orientation we use resize.
            // orientationchange events are only dispatched when there is an actual change in orientation value
            // so in cases on devices with orientation change events, the setter is called an extra time, but stopped after
            me.setOrientation(me.determineOrientation());

            // Only fire the event if we have actually resized.
            if (width != null) {
                me.fireEvent('resize', this, width, height, oldWidth, oldHeight);
            }
        },

        updateSize: function(width, height) {
            var lastSize = this.lastSize;

            lastSize.width  = width  !== undefined ? width  : this.getWindowWidth();
            lastSize.height = height !== undefined ? height : this.getWindowHeight();

            return lastSize;
        },

        waitUntil: function(condition, onSatisfied, onTimeout, delay, timeoutDuration) {
            if (!delay) {
                delay = 50;
            }

            if (!timeoutDuration) {
                timeoutDuration = 2000;
            }

            var scope = this,
                elapse = 0;

            Ext.defer(function repeat() {
                elapse += delay;

                if (condition.call(scope) === true) {
                    if (onSatisfied) {
                        onSatisfied.call(scope);
                    }
                }
                else {
                    if (elapse >= timeoutDuration) {
                        if (onTimeout) {
                            onTimeout.call(scope);
                        }
                    }
                    else {
                        Ext.defer(repeat, delay);
                    }
                }
            }, delay);
        },

        maximize: function() {
            this.fireMaximizeEvent();
        },

        fireMaximizeEvent: function() {
            this.updateSize();
            this.fireEvent('maximize', this);
        },

        updateHeight: function(height, oldHeight) {
            Ext.getBody().setHeight(height);
            this.callParent([height, oldHeight]);
        },

        updateWidth: function(width, oldWidth) {
            Ext.getBody().setWidth(width);
            this.callParent([width, oldWidth]);
        },

        scrollToTop: function() {
            window.scrollTo(0, -1);
        },

        /**
         * Retrieves the document width.
         * @return {Number} width in pixels.
         */
        getWindowWidth: function() {
            return window.innerWidth;
        },

        /**
         * Retrieves the document height.
         * @return {Number} height in pixels.
         */
        getWindowHeight: function() {
            return window.innerHeight;
        },

        getWindowOuterHeight: function() {
            return window.outerHeight;
        },

        getWindowOrientation: function() {
            return window.orientation;
        },

        getSize: function() {
            return this.lastSize;
        },

        setItemFullScreen: function(item) {
            item.addCls(this.fullscreenItemCls);
            item.setTop(0);
            item.setRight(0);
            item.setBottom(0);
            item.setLeft(0);
            this.add(item);
        },

        /**
         * Sets a menu for a given side of the Viewport.
         *
         * Adds functionality to show the menu by swiping from the side of the screen from the given side.
         *
         * If a menu is already set for a given side, it will be replaced by the passed menu.
         *
         * Available sides are: `left`, `right`, `top`, and `bottom`.
         *
         * **Note:** The `cover` and `reveal` animation configs are mutually exclusive.
         * Include only one animation config or omit both to default to `cover`.
         *
         * @param {Ext.Menu/Object} menu The menu instance or config to assign to the viewport.
         * @param {Object} config The configuration for the menu.
         * @param {'top'/'bottom'/'left'/'right'} config.side The side to put the menu on.
         * @param {Boolean} config.cover True to cover the viewport content. Defaults to `true`.
         * @param {Boolean} config.reveal True to push the menu alongside the viewport
         * content. Defaults to `false`.
         *
         * @return {Ext.Menu} The menu set for the passed side.
         */
        setMenu: function(menu, config) {
            config = config || {};

            //<debug>
            if (config.reveal && config.cover) {
                Ext.raise('[Ext.Viewport] setMenu(): Only one of reveal or cover allowed in config');
            }
            //</debug>

            var me = this,
                side, menus, oldMenu;

            // Temporary workaround for body shifting issue
            if (Ext.os.is.iOS && !me.hasiOSOrientationFix) {
                me.hasiOSOrientationFix = true;
                me.on('orientationchange', function() {
                    window.scrollTo(0, 0);
                }, me);
            }

            //<debug>
            if (!menu) {
                Ext.raise("You must specify a menu configuration.");
            }
            //</debug>

            menus = me.getMenus();

            if (!me.addedSwipeListener) {
                me.attachSwipeListeners();
                me.addedSwipeListener = true;
            }

            // Either create the menu, or reconfigure a passed instance
            // according to the config settings for side, cover, and reveal.
            menu = me.configureMenu(menu, config);
            side = menu.getSide();

            // We we already have a menu for this side, ensure it's hidden
            // and reconfgure it using setConfig which will either use
            // the config in the default situation, that the menu is a Sheet,
            // or update the private property.
            oldMenu = menus[side];
            if (oldMenu && !oldMenu.destroyed && oldMenu !== menu) {
                me.hideMenu(side);
                oldMenu.setSide(null);
            }

            menus[side] = menu;

            me.setMenus(menus);

            return menu;
        },

        attachSwipeListeners: function() {
            var me = this;

            me.element.on({
                tap: me.onTap,
                swipestart: me.onSwipeStart,
                edgeswipestart: me.onEdgeSwipeStart,
                edgeswipe: me.onEdgeSwipe,
                edgeswipeend: me.onEdgeSwipeEnd,
                scope: me
            });
        },

        configureMenu: function(menu, config) {
            // We may be creating or reconfiguring a menu here.
            // If reconfiguring, only change configs that are present in the passed config.
            var isInstanced = menu.isComponent,

                // If an instance is being reconfigured, and the config is silent about
                // reveal, cover, or side, we must use the instance's current setting.
                reveal = isInstanced && !('reveal' in config) ? menu.getReveal() : !!config.reveal,
                cover  = (isInstanced && !('cover' in config)  ? menu.getCover()  : config.cover) !== false && !reveal,
                side   = isInstanced && !('side' in config)   ? menu.getSide()   : config.side,
                wasFloated;

            //<debug>
            if (!side) {
                Ext.raise("You must specify a side to dock the menu.");
            }

            if (!sideMap[side]) {
                Ext.raise("You must specify a valid side (left, right, top or bottom) to dock the menu.");
            }
            //</debug>

            // Upgrade the config object to have the correct configurations as defaulted
            // in from the existing instance if it is an instance.
            config = {
                hideAnimation: null,
                showAnimation: null,
                hidden: true,
                floated: cover,
                zIndex : cover ? null : 5,
                reveal: reveal,
                cover: cover,
                side: side
            };
            config[oppositeSideNames[side]] = null;

            if (isInstanced) {
                wasFloated = menu.getFloated();

                // Flipping modes - the menu has to be derendered.
                if (config.floated !== wasFloated) {
                    if (menu.rendered) {
                        // If menu was covering the viewport, then it was a floated child
                        // just remove it non-destructively. It will be derendered and lose
                        // its parent reference.
                        if (wasFloated) {
                            this.remove(menu, false);
                        }
                        // If we're in the non-standard menu insertion mode, we need to derender.
                        // Floated setRender(false) does unwrap the component from its floatWrap.
                        else {
                            menu.el.dom.parentNode.removeChild(menu.el.dom);
                            menu.setRendered(false);
                        }
                    }

                    // Clear down old positioning
                    menu.setConfig({
                        top: null,
                        right: null,
                        bottom: null,
                        left: null
                    });
                }

                // Reconfigure instance according to config.
                // Use strict: false because if the menu is not an instance of Sheet,
                // the reveal, cover and side configs are merely private properties.
                menu.setConfig(config, null, {
                    strict: false
                });
            } else {
                config.xtype = 'actionsheet';
                menu = Ext.create(Ext.apply(config, menu));
            }

            // Update the positioning configs *after* the floatedness has been fully setttled
            // If applied during configuration, these imply positioned, and *not* floated.
            config = {
                left: 0,
                right: 0,
                top: 0,
                bottom: 0
            };
            config[oppositeSideNames[side]] = null;
            menu.setConfig(config);

            menu.toggleCls(menu.floatingCls, !menu.getFloated());
            menu.removeCls(this.getLayout().itemCls);
            menu.toggleCls(Ext.baseCSSPrefix + 'menu-cover', cover);
            menu.toggleCls(Ext.baseCSSPrefix + 'menu-reveal', reveal);
            menu.replaceCls(this.allSidesCls, this.sideClsMap[side]);
            menu.isViewportMenu = true;

            return menu;
        },

        /**
         * Removes a menu from a specified side.
         * @param {'top'/'bottom'/'left'/'right'} side The side to remove the menu from
         * @param {Boolean} animation Pass `true` to animate the menu out of view
         */
        removeMenu: function(side, animation) {
            var me = this,
                menus = me.getMenus() || {},
                menu = menus[side];

            if (menu) {
                me.hideMenu(side, animation);
                menu.removeCls(me.sideClsMap[side]);
            }
            delete menus[side];
            me.setMenus(menus);
        },

        /**
         * Shows the menu that has been {@link #method!setMenu set} on the passed side.
         *
         * If no menu has been set on the passed side, nothing hapens.
         * @param {'top'/'bottom'/'left'/'right'} side The side to show the menu for.
         */
        showMenu: function(side) {
            var me = this,
                sideValue = sideMap[side],
                menu = me.getMenus()[side],
                viewportAfter = {
                    translateX: 0,
                    translateY: 0
                }, size;

            if (!menu || !menu.isHidden()) {
                return;
            }

            // Ensures Menu is rendered, and positioned for animation
            me.beforeMenuAnimate(menu);

            size = menu.element.measure(sideValue & (LEFT | RIGHT) ? 'w' : 'h');

            if (sideValue === LEFT) {
                viewportAfter.translateX = size;
            } else if (sideValue === RIGHT) {
                viewportAfter.translateX = -size;
            } else if (sideValue === TOP) {
                viewportAfter.translateY = size;
            } else if (sideValue === BOTTOM) {
                viewportAfter.translateY = -size;
            }

            // Menu always animates in. Animate to an unstranslated state.
            menu.translate(0, 0, {
                duration: 200
            });

            // Viewport only animates out of its way if menu is not not floated.
            if (!menu.getFloated()) {
                me.translate(viewportAfter.translateX, viewportAfter.translateY, {
                    duration: 200
                });
            }
        },

        /**
         * Hides a menu specified by the menu's side.
         * @param {'top'/'bottom'/'left'/'right'} side The side which the menu is placed.
         * @param {Boolean} animate if false, the menu will be hidden without animation.
         */
        hideMenu: function(side, animate) {
            var me = this,
                sideValue = sideMap[side],
                menu = me.getMenus()[side],
                after = {
                    translateX: 0,
                    translateY: 0
                },
                size;

            animate = animate !== false;

            if (!menu || menu.isHidden()) {
                return;
            }

            size = menu.element.measure(sideValue & (LEFT | RIGHT) ? 'w' : 'h');

            if (sideValue === LEFT) {
                after.translateX = -size;
            } else if (sideValue === RIGHT) {
                after.translateX = size;
            } else if (sideValue === TOP) {
                after.translateY = -size;
            } else if (sideValue === BOTTOM) {
                after.translateY = size;
            }

            // Animate menu out of view if told to
            if (animate) {
                menu.revertFocus();
                menu.translate(after.translateX, after.translateY, {
                    duration: 200,
                    callback: function () {
                        if (!menu.destroyed) {
                            menu.translate(0, 0);
                            menu.setHidden(true);
                        }
                    }
                });
            }
            // Otherwise hide it immediately
            else {
                menu.getTranslatable().stopAnimation();
                menu.setHidden(true);
            }

            // Viewport only has to move back into place if menu is not not floated
            // Return it to an unstranslated state
            if (!menu.getFloated()) {
                me.translate(0, 0, animate ? {
                    duration: 200
                } : null);
            }
        },

        /**
         * Hides all visible menus.
         */
        hideAllMenus: function(animation) {
            var menus = this.getMenus(),
                side;

            for (side in menus) {
                this.hideMenu(side, animation);
            }
        },

        /**
         * Hides all menus except for the side specified
         * @param {'top'/'bottom'/'left'/'right'} side Side not to hide.
         * @param {Boolean} animate if false, the menu will be hidden without animation.
         */
        hideOtherMenus: function(side, animate){
            var menus = this.getMenus(),
                menu;

            for (menu in menus) {
                if (side !== menu) {
                    this.hideMenu(menu, animate);
                }
            }
        },

        /**
         * Toggles the menu specified by side
         * @param {'top'/'bottom'/'left'/'right'} side The side which the menu is placed.
         */
        toggleMenu: function(side) {
            var menus = this.getMenus(), 
                menu;

            if (menus[side]) {
                menu = menus[side];

                menu.setDisplayed(menu.isHidden());
            }
        },

        applyScrollable: function (scrollable) {
            return this.callParent([ scrollable, Ext.getViewportScroller() ]);
        },

        doDestroy: function() {
            var me = this,
                docEl = Ext.get(document.documentElement),
                scroller = me._scrollable;

            docEl.removeCls(me.hasViewportCls);
            docEl.removeCls(me.fixedCls);

            // We acquired usage of the global body scroller through our applyScrollable.
            // Just relinquish it here and allow it to live on.
            if (scroller) {
                // Return the body scroller to default; X and Y scrolling
                scroller.setConfig({
                    x: true,
                    y: true
                });
                me._scrollable = null;
            }

            Ext.un('resize', 'onWindowResize', me);

            me.callParent();

            Ext.Viewport = null;
        },

        privates: {
            addMeta: function(name, content) {
                var meta = document.createElement('meta');

                meta.setAttribute('name', name);
                meta.setAttribute('content', content);
                Ext.getHead().append(meta);
            },

            /**
             * Sets up the before conditions to begin animating a menu into view
             * whether from {@link #method!showMenu}, or {@link #method!beginEdgeSwipe}
             * @param {Ext.Sheet} menu The menu to setup the animation.
             * @private
             */
            beforeMenuAnimate: function(menu) {
                var me = this,
                    side = menu.getSide(),
                    sideValue = sideMap[side],
                    before = {
                        translateX: 0,
                        translateY: 0
                    },
                    size, modal;

                me.hideOtherMenus(side);

                // Ensure the menu is in place as a floated child, or programatically render it.
                if (menu.getFloated()) {
                    me.add(menu);
                } else {
                    // We're going off the reservation by programatically adding to the document body
                    // Usually onAdded does this stuff. We must render the menu and its modal mask.
                    Ext.getBody().insertFirst(menu.element);
                    if (!menu.rendered) {
                        menu.setRendered(true);
                    }
                    modal = menu.getModal();
                    if (modal) {
                        Ext.getBody().insertFirst(modal.element);
                        if (!modal.rendered) {
                            modal.setRendered(true);
                        }
                    }
                    // Must initialize the translatable config to be able to animate
                    me.translate(0, 0);
                }
                menu.removeCls(me.getLayout().itemCls);

                // Now show the edge menu through the normal component show pathway
                // which will fire the expected template methods and events.
                menu.show(false, {
                    side: null
                });

                size = menu.element.measure(sideValue & (LEFT | RIGHT) ? 'w' : 'h');

                if (sideValue === LEFT) {
                    before.translateX = -size;
                } else if (sideValue === RIGHT) {
                    before.translateX = size;
                } else if (sideValue === TOP) {
                    before.translateY = -size;
                } else if (sideValue === BOTTOM) {
                    before.translateY = size;
                }
                menu.translate(before.translateX, before.translateY);
            },

            doAddListener: function(eventName, fn, scope, options, order, caller, manager) {
                var me = this;
                if (eventName === 'ready' && me.isReady && !me.isMaximizing) {
                    fn.call(scope);
                    return me;
                }

                me.callParent([eventName, fn, scope, options, order, caller, manager]);
            },

            /**
             * Returns true if the user can zoom the viewport
             * @private
             */
            isScalable: function () {
                var me = this,
                    metas = document.querySelectorAll('meta[name="viewport"]'),
                    // if there are multiple viewport tags the last one wins.
                    meta = metas.length && metas[metas.length - 1],
                    scalable = true,
                    content;

                if (meta) {
                    content = meta.getAttribute('content');

                    scalable = !(content && me.notScalableRe.test(content));
                }

                return scalable;
            },

            /**
             * @private
             */
            onTap: function(e) {
                // this.hideAllMenus();
            },

            /**
             * @private
             */
            onSwipeStart: function(e) {
                var side = this.sideForSwipeDirection(e.direction),
                    menu = this.getMenus()[side];

                // preventing menu scrolling from being captured as viewport swiping
                if (menu && !menu.owns(e)) {
                    this.hideMenu(side);
                }
            },

            /**
             * @private
             */
            onEdgeSwipeStart: function(e) {
                var me = this,
                    menus = me.getMenus(),
                    menu = menus[oppositeSideNames[e.direction]],
                    menuSide, checkMenu;

                if (!menu || !menu.isHidden()) {
                    return;
                }

                // Claim the gesture to prevent viewport panning.
                e.claimGesture();

                for (menuSide in menus) {
                    checkMenu = menus[menuSide];
                    if (checkMenu.isVisible()) {
                        return;
                    }
                }

                me.$swiping = true;

                // Ensures Menu is rendered, and positioned for animation
                me.beforeMenuAnimate(menu);
            },

            /**
             * @private
             */
            onEdgeSwipe: function(e) {
                var me = this,
                    side = me.sideForDirection(e.direction),
                    menu = me.getMenus()[oppositeSideNames[e.direction]],
                    size, after, viewportAfter,
                    movement, viewportMovement;

                if (!menu || !me.$swiping) {
                    return;
                }

                // Claim the gesture to prevent viewport panning.
                e.claimGesture();

                // See if the swipe has been reversed
                if (e.distance !== me.lastSwipeDistance) {
                    me.reverseSwiping = e.distance < me.lastSwipeDistance;
                }

                me.lastSwipeDistance = e.distance;
                size = menu.element.measure(side & (LEFT | RIGHT) ? 'w' : 'h');
                movement = Math.min(e.distance - size, 0);
                viewportMovement = Math.min(e.distance, size);

                after = {
                    translateX: 0,
                    translateY: 0
                };

                viewportAfter = {
                    translateX: 0,
                    translateY: 0
                };

                if (side === LEFT) {
                    after.translateX = movement;
                    viewportAfter.translateX = viewportMovement;
                } else if (side === RIGHT) {
                    after.translateX = -movement;
                    viewportAfter.translateX = -viewportMovement;
                } else if (side === TOP) {
                    after.translateY = movement;
                    viewportAfter.translateY = viewportMovement;
                } else if (side === BOTTOM) {
                    after.translateY = -movement;
                    viewportAfter.translateY = -viewportMovement;
                }

                menu.translate(after.translateX, after.translateY);

                // Viewport only animates out of its way if menu is not not floated.
                if (!menu.getFloated()) {
                    me.translate(viewportAfter.translateX, viewportAfter.translateY);
                }
            },

            /**
             * @private
             */
            onEdgeSwipeEnd: function(e) {
                var me = this,
                    side = me.sideForDirection(e.direction),
                    menu = me.getMenus()[oppositeSideNames[e.direction]],
                    shouldRevert = me.reverseSwiping || (e.distance < me.getSwipeThreshold()),
                    after = {
                        translateX: 0,
                        translateY: 0
                    }, viewportAfter = {
                        translateX: 0,
                        translateY: 0
                    },
                    size, velocity, movement, viewportMovement;

                if (!menu) {
                    return;
                }

                size = menu.element.measure(side & (LEFT | RIGHT) ? 'w' : 'h');
                velocity = (e.flick) ? e.flick.velocity : 0;

                // check if continuing in the right direction
                if (side === RIGHT) {
                    if (velocity.x > 0) {
                        shouldRevert = true;
                    }
                } else if (side === LEFT) {
                    if (velocity.x < 0) {
                        shouldRevert = true;
                    }
                } else if (side === TOP) {
                    if (velocity.y < 0) {
                        shouldRevert = true;
                    }
                } else if (side === BOTTOM) {
                    if (velocity.y > 0) {
                        shouldRevert = true;
                    }
                }

                movement = shouldRevert ? size : 0;
                viewportMovement = shouldRevert ? 0 : -size;

                if (side === LEFT) {
                    after.translateX = -movement;
                    viewportAfter.translateX = -viewportMovement;
                } else if (side === RIGHT) {
                    after.translateX = movement;
                    viewportAfter.translateX = viewportMovement;
                } else if (side === TOP) {
                    after.translateY = -movement;
                    viewportAfter.translateY = -viewportMovement;
                } else if (side === BOTTOM) {
                    after.translateY = movement;
                    viewportAfter.translateY = viewportMovement;
                }

                // Menu always animates in (or out if reverting)
                menu.translate(after.translateX, after.translateY, {
                    duration: 200,
                    callback: function() {
                        if (shouldRevert) {
                            menu.setHidden(true);
                        }
                    }
                });

               // Viewport only animates out of menu's way if menu is not not floated.
               if (!menu.getFloated()) {
                    me.translate(viewportAfter.translateX, viewportAfter.translateY, {
                        duration: 200
                    });
                }

                me.$swiping = false;
            },

            /**
             * @private
             */
            sideForDirection: function(direction) {
                if (direction === 'up') {
                    direction = 'top';
                } else if (direction === 'down') {
                    direction = 'bottom';
                }
                return oppositeSide[sideMap[direction]];
            },

            /**
             * @private
             */
            sideForSwipeDirection: function(direction) {
                if (direction === 'up') {
                    return  'top';
                } else if (direction === 'down') {
                    return 'bottom';
                }
                return direction;
            },

            toggleWindowListener: function(on, eventName, fn, capturing) {
                if (on) {
                    this.addWindowListener(eventName, fn, capturing);
                } else {
                    this.removeWindowListener(eventName, fn, capturing);
                }
            }
        }
    };
});
