/**
 * A Container has all of the abilities of {@link Ext.Component Component}, but lets you nest other Components inside
 * it. Applications are made up of lots of components, usually nested inside one another. Containers allow you to
 * render and arrange child Components inside them. Most apps have a single top-level Container called a Viewport,
 * which takes up the entire screen. Inside of this are child components, for example in a mail app the Viewport
 * Container's two children might be a message List and an email preview pane.
 *
 * Containers give the following extra functionality:
 *
 * - Adding child Components at instantiation and run time
 * - Removing child Components
 * - Specifying a Layout
 *
 * Layouts determine how the child Components should be laid out on the screen. In our mail app example we'd use an
 * HBox layout so that we can pin the email list to the left hand edge of the screen and allow the preview pane to
 * occupy the rest. There are several layouts, each of which help you achieve your desired
 * application structure.
 *
 * ## Adding Components to Containers
 *
 * As we mentioned above, Containers are special Components that can have child Components arranged by a Layout. One of
 * the code samples above showed how to create a Panel with 2 child Panels already defined inside it but it's easy to
 * do this at run time too:
 *
 *     @example
 *     var mainPanel = Ext.create({
 *         xtype: 'panel',
 *         fullscreen: true,
 *         layout: 'hbox',
 *         defaults: {
 *             flex: 1
 *         },
 *         items: [{
 *             html: 'First Panel',
 *             style: 'background-color: #5E99CC'
 *         }]
 *     });
 *
 *     mainPanel.add({
 *         xtype: 'panel',
 *         html: 'About this App'
 *     });
 *
 * Here we created three Panels in total. First we create mainPanel, which
 * already contains another Panel in its {@link Ext.Container#cfg-items items}
 * configuration, with some dummy text ("First Panel"). Finally, we add the third
 * panel to the second by calling the {@link Ext.Container#method-add add} method on `mainPanel`.
 *
 * In this case we gave our mainPanel another hbox layout, but we also introduced some
 * {@link Ext.Container#defaults defaults}. These are applied to every item in the Panel, so in this case every child
 * inside `mainPanel` will be given a `flex: 1` configuration. The effect of this is that when we first render the screen
 * only a single child is present inside `mainPanel`, so that child takes up the full width available to it. Once the
 * `mainPanel.add` line is called though, the `aboutPanel` is rendered inside of it and also given a `flex` of 1, which will
 * cause it and the first panel to both receive half the full width of the `mainPanel`.
 *
 * Likewise, it's easy to remove items from a Container:
 *
 *     mainPanel.remove(aboutPanel);
 *
 * After this line is run everything is back to how it was, with the first child panel once again taking up the full
 * width inside `mainPanel`.
 */
Ext.define('Ext.Container', {
    extend: 'Ext.Component',

    alternateClassName: ['Ext.lib.Container', 'Ext.container.Container'],

    requires: [
        'Ext.util.ItemCollection'
    ],

    xtype: 'container',
    isContainer: true,

    mixins: [
        'Ext.mixin.Queryable',
        'Ext.mixin.Container',
        'Ext.mixin.FocusableContainer'
    ],

    uses: [
        // This is a uses here because layout.Auto requires container, to ensure
        // component styling rules come in earlier than layout rules and we don't
        // want to create a circular dependency
        'Ext.layout.Auto'
    ],

    /**
     * @event add
     * Fires whenever item added to the Container.
     * @param {Ext.Container} this The Container instance.
     * @param {Object} item The item added to the Container.
     * @param {Number} index The index of the item within the Container.
     */

    /**
     * @event remove
     * Fires whenever item removed from the Container.
     * @param {Ext.Container} this The Container instance.
     * @param {Object} item The item removed from the Container.
     * @param {Number} index The index of the item that was removed.
     */

    /**
     * @event move
     * Fires whenever item moved within the Container.
     * @param {Ext.Container} this The Container instance.
     * @param {Object} item The item moved within the Container.
     * @param {Number} toIndex The new index of the item.
     * @param {Number} fromIndex The old index of the item.
     */

    /**
     * @private
     * @event renderedchange
     * Fires whenever an item is rendered into a container or derendered
     * from a Container.
     * @param {Ext.Container} this The Container instance.
     * @param {Object} item The item in the Container.
     * @param {Boolean} rendered The current rendered status of the item.
     */

    /**
     * @event activate
     * Fires whenever item within the Container is activated.
     * @param {Object} newActiveItem The new active item within the container.
     * @param {Ext.Container} this The Container instance.
     * @param {Object} oldActiveItem The old active item within the container.
     */

    /**
     * @event deactivate
     * Fires whenever item within the Container is deactivated.
     * @param {Object} oldActiveItem The old active item within the container.
     * @param {Ext.Container} this The Container instance.
     * @param {Object} newActiveItem The new active item within the container.
     */

    eventedConfig: {
        /**
         * @cfg {Object/String/Number} activeItem The item from the {@link #cfg-items} collection that will be active first. This is
         * usually only meaningful in a {@link Ext.layout.Card card layout}, where only one item can be active at a
         * time. If passes a string, it will be assumed to be a {@link Ext.ComponentQuery} selector.
         * @accessor
         * @evented
         */
        activeItem: 0
    },


    config: {
        activeItemIndex: null,

        /**
         * @cfg {Boolean} [autoSize=true]
         * May be set to `false` for improved layout performance if auto-sizing is not required.
         *
         * Some versions of Safari, both desktop and mobile, have very slow performance
         * if the application has deeply nested containers due to the following WebKit
         * bug: https://bugs.webkit.org/show_bug.cgi?id=150445
         *
         * Applications that experience performance issues in the affected versions of
         * Safari may need to turn off autoSizing globally for all `Ext.Container` instances
         * by placing the following override in the application's "overrides" directory:
         *
         *     Ext.define('MyApp.overrides.Container', {
         *         override: 'Ext.Container',
         *         config: {
         *             autoSize: false
         *         }
         *     });
         *
         * Once auto-sizing support has turned off by default, it can be selectively
         * turned back on only on those container instances that explicitly need auto-sizing
         * behavior by setting `autoSize` to `true`.
         *
         * This option can also be used to allow items to be sized in percentage
         * units as a workaround for the following browser bug:
         * https://bugs.webkit.org/show_bug.cgi?id=137730
         *
         * To illustrate, the following example should render a 200px by 200px green box
         * (the container) with a yellow box inside of it (the child item).  The child
         * item's height and width are both set to `'50%'` so the child should render
         * exactly 100px by 100px in size.
         *
         *     @example
         *     Ext.create({
         *         xtype: 'container',
         *         renderTo: Ext.getBody(),
         *         height: 200,
         *         width: 200,
         *         style: 'background: green',
         *         items: [{
         *             xtype: 'component',
         *             style: 'background: yellow',
         *             height: '50%',
         *             width: '50%'
         *         }]
         *     });
         *
         * All browsers except for Safari render the previous example correctly, but
         * Safari does not assign a height to the component.  To make percentage-sized
         * items work in Safari, simply set `autoSize` to `false` on the container.
         *
         * Since the underlying implementation works by absolutely positioning the container's
         * body element, this option can only be used when the container is not
         * "shrink wrapping" the content in either direction.  When `autoSize` is
         * set to `false`, shrink wrapped dimension(s) will collapse to 0.
         */
        autoSize: null,

        /**
         * @cfg {String/Object/Boolean} cardSwitchAnimation
         * Animation to be used during transitions of cards.
         * @removed 2.0.0 Please use {@link Ext.layout.Card#animation} instead
         */

        // @cmd-auto-dependency { aliasPrefix : "layout."}
        /**
         * @cfg {Object/String} layout Configuration for this Container's layout. Example:
         *
         *     @example
         *     Ext.create({
         *         xtype: 'container',
         *         layout: {
         *             type: 'hbox',
         *             align: 'middle'
         *         },
         *         items: [{
         *             xtype: 'panel',
         *             flex: 1,
         *             bodyStyle: {
         *                 background: "#000",
         *                 color:"#fff"
         *             }
         *         }, {
         *            xtype: 'panel',
         *            flex: 2,
         *            bodyStyle: {
         *                background: "#f00",
         *                color:"#fff"
         *            }
         *         }]
         *     });
         *
         * @accessor
         */
        layout: 'auto',

        /**
         * @cfg {Object} control Enables you to easily control Components inside this Container by listening to their
         * events and taking some action. For example, if we had a container with a nested Disable button, and we
         * wanted to hide the Container when the Disable button is tapped, we could do this:
         *
         *     @example
         *     Ext.create({
         *         xtype: 'container',
         *         control: {
         *            'button[text=Disable]': {
         *                tap: 'hideMe'
         *            }
         *         },
         *
         *         hideMe: function () {
         *             this.hide();
         *         }
         *     });
         *
         * We used a {@link Ext.ComponentQuery} selector to listen to the {@link Ext.Button#tap tap} event on any
         * {@link Ext.Button button} anywhere inside the Container that has the {@link Ext.Button#text text} 'Disable'.
         * Whenever a Component matching that selector fires the `tap` event our `hideMe` function is called. `hideMe` is
         * called with scope: `this` (e.g. `this` is the Container instance).
         *
         */
        control: null,

        /**
         * @cfg {Object} defaults A set of default configurations to apply to all child Components in this Container.
         * It's often useful to specify defaults when creating more than one items with similar configurations. For
         * example here we can specify that each child is a panel and avoid repeating the xtype declaration for each
         * one:
         *
         *     @example
         *     Ext.create({
         *         xtype: 'container',
         *         defaults: {
         *             xtype: 'panel'
         *         },
         *         items: [
         *             {
         *                 html: 'Panel 1'
         *             },
         *             {
         *                 html: 'Panel 2'
         *             }
         *         ]
         *     });
         *
         * @accessor
         */
        defaults: null,

        // @cmd-auto-dependency { aliasPrefix: "widget.", typeProperty: "xtype", defaultTypeProperty: "defaultType", defaultsProperty: "defaults" }
        /**
         * @cfg {Array/Object} items The child items to add to this Container. This is usually an array of Component
         * configurations or instances, for example:
         *
         *     @example
         *     Ext.create({
         *         xtype: 'container',
         *         items: [{
         *             xtype: 'panel',
         *             html: 'This is an item'
         *         }]
         *     });
         *
         * This may also be specified as an object, the property names of which are `itemId`s, and the property values
         * are child Component config objects, for example:
         *
         *     @example
         *     Ext.create({
         *         xtype: 'tabpanel',
         *         items: {
         *             panel1: {
         *                 xtype: 'panel',
         *                 title: 'First panel'
         *             },
         *             panel2: {
         *                 xtype: 'panel',
         *                 title: 'Second panel'
         *             }
         *         }
         *     });
         *
         * @accessor
         */
        items: null,

        /**
         * @cfg {Boolean} autoDestroy
         * If `true`, child items will be destroyed as soon as they are {@link #method-remove removed}
         * from this container.
         * @accessor
         */
        autoDestroy: true,

        /**
         * @cfg {String} [defaultType=container]
         * The default {@link Ext.Component xtype} of child Components to create in this Container
         * when a child item is specified as a raw configuration object, rather than as an instantiated
         * Component.
         * @accessor
         */
        defaultType: null,
        
        /**
         * @cfg {String} defaultFocus
         *
         * Specifies a child Component to receive focus when this Container's {@link #method-focus}
         * method is called. Should be a valid {@link Ext.ComponentQuery query} selector.
         */
        defaultFocus: {
            $value: null,
            lazy: true
        },

        /**
         * @cfg {String} innerCls
         * A string to add to the immediate parent element of the inner items of this
         * container. That is, items that are not `docked`, `positioned` or `floated`. In
         * some containers, `positioned` items may be in this same element.
         * @since 6.5.0
         */
        innerCls: null,

        // @cmd-auto-dependency {defaultType: "Ext.Mask"}
        /**
         * @cfg {Boolean/Object/Ext.Mask/Ext.LoadMask} masked
         * A configuration to allow you to mask this container.
         * You can optionally pass an object block with and xtype of `loadmask`, and an optional `message` value to
         * display a loading mask. Please refer to the {@link Ext.LoadMask} component to see other configurations.
         *
         *     @example
         *     Ext.create({
         *         xtype: 'container',
         *         fullscreen: true,
         *         html: 'Hello World',
         *         masked: {
         *             xtype: 'loadmask',
         *             message: 'My Message'
         *         }
         *     });
         *
         * Alternatively, you can just call the setter at any time with `true`/`false` to show/hide the mask:
         *
         *     setMasked(true); //show the mask
         *     setMasked(false); //hides the mask
         *
         * There are also two convenient methods, {@link #method-mask} and {@link #unmask}, to allow you to mask and unmask
         * this container at any time.
         *
         * Remember, the {@link Ext.Viewport} is always a container, so if you want to mask your whole application at anytime,
         * can call:
         *
         *     Ext.Viewport.setMasked({
         *         xtype: 'loadmask',
         *         message: 'Hello'
         *     });
         *
         * @accessor
         */
        masked: null
    },

    /**
     * @cfg {Boolean} [weighted=false]
     * If set to `true`, then child {@link #cfg!items} may be specified as a object,
     * with each property name specifying an {@link #cfg!itemId}, and the property
     * value being the child item configuration object.
     *
     * When using this scheme, each child item may contain a {@link #cfg!weight}
     * configuration value which affects its order in this container. Lower weights
     * are towards the start, higher weights towards the end.
     */
    weighted: false,

    /**
     * @cfg {Boolean}
     * @protected
     * `true` to enable border management of docked items.  When enabled, borders of docked
     * items will collapse where they meet to avoid duplicated borders.
     */
    manageBorders: false,

    classCls: Ext.baseCSSPrefix + 'container',

    managedBordersCls: Ext.baseCSSPrefix + 'managed-borders',

    template: [{
        reference: 'bodyElement',
        cls: Ext.baseCSSPrefix + 'body-el',
        uiCls: 'body-el'
    }],

    constructor: function(config) {
        var me = this;

        me._items = me.items = new Ext.util.ItemCollection();
        me.innerItems = [];

        me.getReferences = me.getFirstReferences;
        me.onItemAdd = me.onFirstItemAdd;

        me.callParent(arguments);

        delete me.getReferences;
    },

    initialize: function() {
        var me = this;

        me.reference = me.setupReference(me.reference);
        me.callParent();

        if (me.manageBorders) {
            me.addCls(me.managedBordersCls);
        }

        // Ensure the container's layout instance is created, even if the container
        // has no items.  This ensures border management is handled correctly on empty
        // panels.
        me.getLayout();
    },

    /**
     * Changes the {@link #masked} configuration when its setter is called, which will convert the value
     * into a proper object/instance of {@link Ext.Mask}/{@link Ext.LoadMask}. If a mask already exists,
     * it will use that instead.
     * @param {Boolean/Object/Ext.Mask/Ext.LoadMask} masked
     * @return {Object}
     */
    applyMasked: function (masked) {
        var isVisible = true,
            currentMask;

        if (masked === false) {
            masked = true;
            isVisible = false;
        }

        // Subscript notation is used to reference Ext.Mask to prevent creation of an auto-dependency
        currentMask = Ext.factory(masked, Ext['Mask'], this.getMasked());

        if (currentMask) {
            currentMask.setHidden(!isVisible);

            //\\ TODO: Reliable render pathway and rendered transition.
            // was: this.el.append(currentMask.el);
            currentMask.render(this.el);
        }

        return currentMask;
    },

    /**
     * Convenience method which calls {@link #setMasked} with a value of `true` (to show the mask). For additional
     * functionality, call the {@link #setMasked} function direction (See the {@link #masked} configuration documentation
     * for more information).
     */
    mask: function (mask) {
        this.setMasked(mask || true);
    },

    /**
     * Convenience method which calls {@link #setMasked} with a value of false (to hide the mask). For additional
     * functionality, call the {@link #setMasked} function direction (See the {@link #masked} configuration documentation
     * for more information).
     */
    unmask: function () {
        this.setMasked(false);
    },

    initInheritedState: function(inheritedState, inheritedStateInner) {
        this.callParent([inheritedState, inheritedStateInner]);
        this.initContainerInheritedState(inheritedState, inheritedStateInner);
    },

    onAdded: function(parent, instanced) {
        this.callParent([parent, instanced]);

        this.containerOnAdded(parent, instanced);
    },

    onRemoved: function(destroying) {
        this.containerOnRemoved(destroying);
        this.callParent([destroying]);
    },

    afterItemShow: function(item) {
        var layout;

        if (item.getDocked()) {
            layout = this.getLayout();
            this.items.generation++;
            layout.handleDockedItemBorders();
        }
    },

    afterItemHide: function(item) {
        var layout;

        if (item.getDocked()) {
            layout = this.getLayout();
            this.items.generation++;
            layout.handleDockedItemBorders();
        }
    },

    applyItems: function(items, collection) {
        if (items) {
            var me = this,
                activeItem;

            me.getDefaultType();
            me.getDefaults();

            if (me.initialized && collection.length > 0) {
                me.removeAll();
            }

            // Read items from object properties back into the newItems array
            // unless the item is a Widget or is a config object with an xtype.
            if (me.weighted && !items.isWidget && !items.xtype) {
                items = Ext.convertKeyedItems(items);
            }

            me.add(items);

            //Don't need to call setActiveItem when Container is first initialized
            if (me.initialized) {
                activeItem = me.initialConfig.activeItem || me.config.activeItem || 0;

                me.setActiveItem(activeItem);
            }
        }
    },

    /**
     * @private
     */
    applyControl: function(selectors) {
        var selector, key, listener, listeners;

        for (selector in selectors) {
            listeners = selectors[selector];

            for (key in listeners) {
                listener = listeners[key];

                if (Ext.isObject(listener)) {
                    listener.delegate = selector;
                }
            }

            listeners.delegate = selector;

            this.addListener(listeners);
        }

        return selectors;
    },
    
    updateDisabled: function(disabled) {
        var me = this;
        
        me.callParent([disabled]);
        
        if (me.focusableContainer) {
            me.getItems();
            
            if (disabled) {
                me.element.saveTabbableState();
            }
            else {
                me.element.restoreTabbableState();
            }

            me.activateFocusableContainer(!disabled);
            
            if (!disabled) {
                me.initDefaultFocusable();
            }
        }
    },

    /**
     * Initialize layout and event listeners the very first time an item is added
     * @private
     */
    onFirstItemAdd: function(item) {
        var me = this;

        delete me.onItemAdd;

        if (item.isInner && me.innerHtmlElement && !me.getHtml() && !me.getTpl()) {
            me.innerHtmlElement.destroy();
            delete me.innerHtmlElement;
        }

        return me.onItemAdd.apply(me, arguments);
    },

    applyLayout: function (layout, oldLayout) {
        if (typeof layout === 'string') {
            layout = {
                type: layout
            };
        }

        if (oldLayout) {
            if (layout) {
                if (!layout.isLayout) {
                    oldLayout.setConfig(layout);
                }
                //<debug>
                else {
                    Ext.raise('Cannot change layout instances on ' + this.$className);
                }
                //</debug>
            }

            return oldLayout;
        }

        // Container has to be stamped into the layout as soon as its created.
        if (!(layout && layout.isLayout)) {
            layout = Ext.Factory.layout(Ext.apply({
                container: this
            }, layout), Ext.layout.Auto);
        }

        this.link('layout', layout);
        return layout;
    },

    updateDefaultType: function(defaultType) {
        // Cache the direct reference to the default item class here for performance
        this.defaultItemClass = Ext.ClassManager.getByAlias('widget.' + defaultType);

        //<debug>
        if (!this.defaultItemClass) {
            Ext.Logger.error("Invalid defaultType of: '" + defaultType + "', must be a valid component xtype");
        }
        //</debug>
    },

    /**
     * Called when an item is added to this container either during initialization of the {@link #cfg-items} config,
     * or when new items are {@link #method-add added), or {@link #method-insert inserted}.
     *
     * If the passed object is *not* an instanced component, it converts the passed object into an instanced
     * child component.
     *
     * It applies {@link #cfg-defaults} applied for contained child items - that is items
     * which are not positiond using {@link Ext.Component#cfg-left left},  {@link Ext.Component#cfg-top top},
     * {@link Ext.Component#cfg-bottom bottom}, {@link Ext.Component#cfg-right right},
     * {@link Ext.Component#cfg-centered centered} or {@link Ext.Component#cfg-docked docked}.
     *
     * Derived classes can override this method to process context appropriate short-hands
     * such as {@link Ext.Toolbar} and "->" to insert a spacer.
     *
     * @param {Mixed} item The item being added. May be a raw config object or an instanced
     * Component or some other short-hand understood by the container.
     * @return {Ext.Component} The component to be added.
     * @protected
     */
    factoryItem: function (item) {
        //<debug>
        if (!item) {
            Ext.Logger.error("Invalid item given: " + item + ", must be either the config object to factory a new item, " +
                "or an existing component instance");
        }
        //</debug>

        var me = this;

        item = me.applyItemDefaults(item);

        if (!item.isComponent) {
            // This forces default type to be resolved prior to any other configs that
            // may be using it to create children
            if (!me.$hasCachedDefaultItemClass) {
                me.getDefaultType();
                me.$hasCachedDefaultItemClass = true;
            }

            item = Ext.factory(item, me.defaultItemClass);
        }

        return item;
    },

    /**
     * Adds one or more Components to this Container. Example:
     *
     *     var myPanel = Ext.create({
     *         xtype: 'panel',
     *         html : 'This will be added to a Container'
     *     });
     *
     *     var items = myContainer.add([myPanel]); // Array returned
     *     var item  = myContainer.add(myPanel);   // One item is returned
     *
     * @param {Object/Object[]/Ext.Component/Ext.Component[]} newItems The new item(s) to add
     * to the Container. Note that if an array of items to add was passed in, an array of added
     * items will be returned as well even if there was only one item.
     *
     * @return {Ext.Component/Ext.Component[]} The Component(s) that were added.
     */
    add: function(newItems) {
        var me = this,
            items = me.getItems(),
            weighted = me.weighted,
            addingArray = true,
            addedItems = [],
            doWeightedInsert, i, ln, item, instanced;

        if (!Ext.isArray(newItems)) {
            newItems = [newItems];
            addingArray = false;
        }

        // If we are maintaining child items in weight order, then we only
        // have to do a calculated insert if there are existing items.
        // If no existing items, we can just sort the incoming items
        // and add them in that order.
        if (weighted) {
            if (items.length) {
                doWeightedInsert = true;
            } else {
                Ext.Array.sort(newItems, Ext.weightSortFn);
            }
        }

        for (i = 0, ln = newItems.length; i < ln; i++) {
            item = newItems[i];

            if (item) {
                instanced = item.isWidget;

                if (!instanced) {
                    item.$initParent = me;
                }

                item = me.factoryItem(item);

                // If we are a weighted container, and we're not empty, and we're adding multiple
                // items, then insert items according to weighting.
                if (doWeightedInsert) {
                    me.doInsert(items.findInsertionIndex(item, Ext.weightSortFn), item, instanced);
                } else {
                    me.doAdd(item, instanced);
                }
                delete item.$initParent;

                if (me.focusableContainer) {
                    me.onFocusableChildAdd(item);
                }

                addedItems.push(item);
            }
            //<debug>
            else if (item !== null) {
                Ext.raise('Invalid item passed to add');
            }
            //</debug>
        }

        if ((me.isConfiguring || !me.getActiveItem()) && me.innerItems.length > 0) {
            me.setActiveItem(me.initialConfig.activeItem || 0);
        }

        if (me.rendered && ln && me.focusableContainer) {
            me.initFocusableContainer();
        }

        return addingArray ? addedItems : addedItems[0];
    },

    onItemWeightChange: function(item) {
        var items = this.getItems(),
            oldIndex = items.indexOf(item),
            index;

        items.remove(item);
        index = items.findInsertionIndex(item, Ext.weightSortFn);
        items.insert(index, item);

        this.insertInner(item, index);
        this.onItemMove(item, index, oldIndex);
    },

    /**
     * @private
     * @param {Ext.Component} item
     * @param {Boolean} instanced
     * when received.
     */
    doAdd: function(item, instanced) {
        var me = this,
            items = me.getItems(),
            index;

        if (!items.has(item)) {
            index = items.length;
            items.add(item);

            if (item.isInnerItem()) {
                me.insertInner(item, index);
            }

            item.onAdded(me, !!instanced);
            
            if (me.focusableContainer) {
                me.onFocusableChildAdd(item);
            }

            me.onItemAdd(item, index);
        }
    },

    /**
     * Removes an item from this Container, optionally destroying it.
     * @param {Ext.Component/String/Number/Array} which The component instance, id or
     * index to remove or an array of these.
     * @param {Boolean} [destroy] `true` to automatically call Component's
     * {@link Ext.Component#method-destroy destroy} method.
     *
     * @return {Ext.Component} The Component that was removed.
     */
    remove: function (which, destroy) {
        var me = this,
            component = me.getComponent(which), // fails for []'s
            activeItem, index, innerItems, item, wasActive;

        if (destroy === undefined) {
            destroy = me.getAutoDestroy();
        }

        if (!component) {
            //<debug>
            if (!Ext.isArray(which)) {
                Ext.raise('Invalid first argument to Ext.Container#remove() - ',
                    Ext.typeOf(which));
            }
            //</debug>

            activeItem = me.getActiveItem();

            for (index = 0; index < which.length; ++index) {
                item = me.getComponent(which[index]);

                if (item === activeItem) {
                    wasActive = true;
                }
                else if (item) {
                    me.remove(item, destroy);
                }
            }

            // If we are removing the activeItem, save it for last
            if (wasActive) {
                me.remove(activeItem, destroy);
            }

            return which; // the same array we were given
        }

        index = me.indexOf(component);
        innerItems = me.getInnerItems();

        if (index !== -1) {
            if (!me.removingAll && innerItems.length > 1 && component === me.getActiveItem()) {
                me.on({
                    activeitemchange: 'doRemove',
                    scope: me,
                    single: true,
                    order: 'after',
                    args: [component, index, destroy]
                });

                me.doResetActiveItem(innerItems.indexOf(component));
            }
            else {
                me.doRemove(component, index, destroy);
                if (innerItems.length === 0) {
                    me.setActiveItem(null);
                }
            }
        }

        return component;
    },

    doResetActiveItem: function(innerIndex) {
        if (innerIndex === 0) {
            this.setActiveItem(1);
        }
        else {
            this.setActiveItem(0);
        }
    },

    doRemove: function(item, index, destroy) {
        var me = this;

        // Don't bother removing from these collections during destroy, since
        // they will just be nulled out
        if (!me.destroying) {
            me.items.remove(item);

            if (item.isInnerItem()) {
                me.removeInner(item);
            }
            me.onItemRemove(item, index, destroy);
        }

        if (!item.destroyed) {
            item.onRemoved(item.destroying || destroy);
        }

        if (me.focusableContainer && !me.destroying && !me.destroyed) {
            me.onFocusableChildRemove(item, destroy);
        }

        if (destroy && !item.destroyed) {
            item.destroy();
        }
    },

    /**
     * Removes all items currently in the Container, optionally destroying them all.
     *
     * @param {Boolean} destroy If `true`, {@link Ext.Component#method-destroy destroys}
     * each removed Component.
     * @param {Boolean} everything If `true`, completely remove all items including
     * docked / centered and positioned items.
     *
     * @return {Ext.Component[]} Array of the removed Components
     */
    removeAll: function(destroy, everything) {
        var me = this,
            destroying = me.destroying,
            items = me.items,
            removed = destroying ? null : [],
            ln = items.length,
            i, item;

        if (typeof destroy !== 'boolean') {
            destroy = this.getAutoDestroy();
        }

        // removingAll flag is used so we don't unnecessarily change activeItem while
        // removing all items.
        me.removingAll = true;

        for (i = 0; i < ln; i++) {
            item = items.getAt(i);

            if (item && (everything || item.isInnerItem())) {
                me.doRemove(item, i, destroy);
                // When we are destroying, the items will not be removed from the collection
                // so the count won't be modified
                if (!destroying) {
                    i--;
                    ln--;
                }
            }

            if (removed) {
                removed.push(item);
            }
        }

        if (!destroying) {
            me.setActiveItem(null);
        }

        me.removingAll = false;

        return removed;
    },

    /**
     * Returns the Component for a given index in the Container's {@link #property-items}.
     * @param {Number} index The index of the Component to return.
     * @return {Ext.Component} The item at the specified `index`, if found.
     */
    getAt: function(index) {
        return this.items.getAt(index);
    },

    getInnerAt: function(index) {
        return this.innerItems[index];
    },

    /**
     * Removes the Component at the specified index:
     *
     *     myContainer.removeAt(0); // removes the first item
     *
     * @param {Number} index The index of the Component to remove.
     *
     * @param {Boolean} [destroy] `true` to automatically call Component's
     * {@link Ext.Component#method-destroy destroy} method.
     *
     * @return {Ext.Component} The removed Component
     */
    removeAt: function(index, destroy) {
        var item = this.getAt(index);

        if (item) {
            this.remove(item, destroy);
        }

        return item;
    },

    /**
     * Removes an inner Component at the specified index:
     *
     *     myContainer.removeInnerAt(0); // removes the first item of the innerItems property
     *
     * @param {Number} index The index of the Component to remove.
     * @return {Ext.Component} The removed Component
     */
    removeInnerAt: function(index) {
        var item = this.getInnerItems()[index];

        if (item) {
            this.remove(item);
        }

        return item;
    },

    /**
     * @private
     */
    has: function(item) {
        return this.getItems().indexOf(item) != -1;
    },

    /**
     * @private
     */
    hasInnerItem: function(item) {
        return this.innerItems.indexOf(item) != -1;
    },

    /**
     * @private
     */
    indexOf: function(item) {
        return this.getItems().indexOf(item);
    },

    innerIndexOf: function(item) {
        return this.innerItems.indexOf(item);
    },

    /**
     * @private
     * @param {Ext.Component} item
     * @param {Number} index
     */
    insertInner: function(item, index) {
        var items = this.getItems().items,
            innerItems = this.innerItems,
            currentInnerIndex = innerItems.indexOf(item),
            newInnerIndex = -1,
            nextSibling;

        if (currentInnerIndex !== -1) {
            innerItems.splice(currentInnerIndex, 1);
        }

        if (typeof index == 'number') {
            do {
                nextSibling = items[++index];
            } while (nextSibling && !nextSibling.isInnerItem());

            if (nextSibling) {
                newInnerIndex = innerItems.indexOf(nextSibling);
                innerItems.splice(newInnerIndex, 0, item);
            }
        }

        if (newInnerIndex === -1) {
            innerItems.push(item);
            newInnerIndex = innerItems.length - 1;
        }

        if (currentInnerIndex !== -1) {
            this.onInnerItemMove(item, newInnerIndex, currentInnerIndex);
        }

        return this;
    },

    onInnerItemMove: Ext.emptyFn,

    /**
     * @private
     * @param {Ext.Component} item
     */
    removeInner: function(item) {
        Ext.Array.remove(this.innerItems, item);

        return this;
    },

    /**
     * Adds a child Component at the given index. For example, here's how we can add a new item, making it the first
     * child Component of this Container:
     *
     *     myContainer.insert(0, {xtype: 'panel', html: 'new item'});
     *
     * @param {Number} index The index to insert the Component at.
     * @param {Object} item The Component to insert.
     */
    insert: function(index, item) {
        var me = this,
            instanced,
            i;

        //<debug>
        if (typeof index != 'number') {
            Ext.Logger.error("Invalid index of '" + index + "', must be a valid number");
        }
        //</debug>

        if (Ext.isArray(item)) {
            for (i = item.length - 1; i >= 0; i--) {
                me.insert(index, item[i]);
            }

            return me;
        }

        instanced = item.isWidget;
        if (!instanced) {
            item.$initParent = me;
        }
        item = me.factoryItem(item);
        me.doInsert(index, item, instanced);
        delete item.$initParent;

        return item;
    },

    /**
     * @private
     * @param {Number} index
     * @param {Ext.Component} item
     * @param {Boolean} instanced
     */
    doInsert: function(index, item, instanced) {
        var me = this,
            items = me.items,
            itemsLength = items.length,
            currentIndex, isInnerItem;

        isInnerItem = item.isInnerItem();

        if (index > itemsLength) {
            index = itemsLength;
        }

        if (items[index - 1] === item) {
            return;
        }

        currentIndex = me.indexOf(item);

        if (currentIndex !== -1) {
            items.removeAt(currentIndex);
        }

        items.insert(index, item);

        if (currentIndex === -1) {
            item.onAdded(me, !!instanced);
        }

        if (isInnerItem) {
            me.insertInner(item, index);
        }

        if (currentIndex !== -1) {
            me.onItemMove(item, index, currentIndex);
        } else {
            me.onItemAdd(item, index);
        }
    },

    /**
     * @private
     */
    insertFirst: function(item) {
        return this.insert(0, item);
    },

    /**
     * @private
     */
    insertLast: function(item) {
        return this.insert(this.getItems().length, item);
    },

    /**
     * @private
     */
    insertBefore: function(item, relativeToItem) {
        var index = this.indexOf(relativeToItem);

        if (index !== -1) {
            this.insert(index, item);
        }
        return this;
    },

    /**
     * @private
     */
    insertAfter: function(item, relativeToItem) {
        var index = this.indexOf(relativeToItem);

        if (index !== -1) {
            this.insert(index + 1, item);
        }
        return this;
    },

    /**
     * @private
     */
    onItemAdd: function(item, index) {
        var me = this;

        me.doItemLayoutAdd(item, index);

        if (me.initialized) {
            if (item.hasListeners.added) {
                item.fireEvent('added', item, me, index);
            }
            if (me.hasListeners.add) {
                me.fireEvent('add', me, item, index);
            }
        }
    },

    doItemLayoutAdd: function (item, index) {
        var layout = this.getLayout();

        if (this.rendered && !item.rendered) {
            item.fireAction('renderedchange', [this, item, true], 'onItemAdd', layout, {args: [item, index]});
        } else {
            layout.onItemAdd(item, index);
        }
    },

    /**
     * @private
     */
    onItemRemove: function(item, index, destroying) {
        var me = this;

        me.doItemLayoutRemove(item, index, destroying);

        if (item.hasListeners.removed) {
            item.fireEvent('removed', item, me, index);
        }
        if (me.hasListeners.remove) {
            me.fireEvent('remove', me, item, index);
        }
    },

    doItemLayoutRemove: function(item, index, destroying) {
        var layout = this.getLayout();

        if (item.rendered) {
            item.setRendered(false);
            item.fireAction('renderedchange', [this, item, false], 'onItemRemove', layout, {args: [item, index, destroying]});
        }
        else {
            layout.onItemRemove(item, index, destroying);
        }
    },

    /**
     * @private
     */
    onItemMove: function(item, toIndex, fromIndex) {
        var me = this;

        me.doItemLayoutMove(item, toIndex, fromIndex);

        if (item.hasListeners.moved) {
            item.fireEvent('moved', item, me, toIndex, fromIndex);
        }
        if (me.hasListeners.move) {
            me.fireEvent('move', me, item, toIndex, fromIndex);
        }
    },

    doItemLayoutMove: function(item, toIndex, fromIndex) {
        this.getLayout().onItemMove(item, toIndex, fromIndex);
    },

    onItemInnerStateChange: function(item, isInner) {
        var layout = this.getLayout();

        if (isInner) {
            this.insertInner(item, this.items.indexOf(item));
        }
        else {
            this.removeInner(item);
        }

        layout.onItemInnerStateChange.apply(layout, arguments);
    },

    onItemFloatedChange: function(item, floated) {
        var layout = this.getLayout();

        layout.onItemFloatedChange(item, floated);
    },

    /**
     * Returns all inner {@link #property-items} of this container. `inner` means that the item is not `docked` or
     * `positioned`.
     * @return {Array} The inner items of this container.
     */
    getInnerItems: function() {
        return this.innerItems;
    },

    /**
     * Returns all the {@link Ext.Component#docked} items in this container.
     * @return {Array} The docked items of this container.
     */
    getDockedItems: function() {
        var items = this.getItems().items,
            dockedItems = [],
            ln = items.length,
            item, i;

        for (i = 0; i < ln; i++) {
            item = items[i];
            if (item.isDocked()) {
                dockedItems.push(item);
            }
        }

        return dockedItems;
    },

    /**
     * @private
     */
    applyActiveItem: function(activeItem, currentActiveItem) {
        var me = this,
            innerItems = me.getInnerItems(),
            initialConfig = me.initialConfig,
            initialActive = initialConfig.activeItem || activeItem;

        // Make sure the items are already initialized
        me.getItems();

        if (me.isConfiguring && !initialConfig.activeItem) {
            activeItem = initialActive;
        }

        // No items left to be active, reset back to 0 on falsy changes
        if (!activeItem && innerItems.length === 0) {
            return 0;
        } else if (typeof activeItem == 'number') {
            activeItem = Math.max(0, Math.min(activeItem, innerItems.length - 1));
            activeItem = innerItems[activeItem];

            if (activeItem) {
                return activeItem;
            }
            else if (currentActiveItem) {
                return null;
            }
        } else if (activeItem) {
            var item;

            //ComponentQuery selector?
            if (typeof activeItem == 'string') {
                item = me.child(activeItem);

                activeItem = {
                    xtype: activeItem
                };
            }

            if (!item || !item.isComponent) {
                activeItem.$initParent = me;
                item = me.factoryItem(activeItem);
            }
            me.pendingActiveItem = item;

            //<debug>
            if (!item.isInnerItem()) {
                Ext.Logger.error("Setting activeItem to be a non-inner item");
            }
            //</debug>

            if (!me.has(item)) {
                me.add(item);
            }
            delete item.$initParent;

            return item;
        }
    },

    /**
     * Animates to the supplied `activeItem` with a specified animation. Currently this only works
     * with a Card layout.  This passed animation will override any default animations on the
     * container, for a single card switch. The animation will be destroyed when complete.
     * @param {Object/Number} activeItem The item or item index to make active.
     * @param {Object/Ext.layout.card.fx.Abstract} animation Card animation configuration or instance.
     */
    animateActiveItem: function(activeItem, animation) {
        var layout = this.getLayout(),
            defaultAnimation;

        if (this.activeItemAnimation) {
            this.activeItemAnimation.destroy();
        }

        this.activeItemAnimation = animation = new Ext.Factory.layoutCardFx(animation);
        
        if (animation && layout.isCard) {
            animation.setLayout(layout);
            defaultAnimation = layout.getAnimation();
            if (defaultAnimation) {
                defaultAnimation.disable();
            }
            animation.on('animationend', function() {
                if (defaultAnimation) {
                    defaultAnimation.enable();
                }
                animation.destroy();
            }, this);
        }

        return this.setActiveItem(activeItem);
    },

    updateActiveItem: function(newActiveItem, oldActiveItem) {
        delete this.pendingActiveItem;

        if (oldActiveItem && !oldActiveItem.destroyed) {
            oldActiveItem.fireEvent('deactivate', oldActiveItem, this, newActiveItem);
        }

        if (newActiveItem) {
            newActiveItem.fireEvent('activate', newActiveItem, this, oldActiveItem);
        }
        this.setActiveItemIndex(this.innerItems.indexOf(newActiveItem));
    },

    updateActiveItemIndex: function (index) {
        this.setActiveItem(this.innerItems[index]);
    },

    /**
     * Used by ComponentQuery to retrieve all of the items
     * which can potentially be considered a child of this Container.
     * This should be overridden by components which have child items
     * that are not contained in items. For example `dockedItems`, `menu`, etc
     * @private
     */
    getRefItems: function(deep) {
        var items = this.getItems().items,
            result, ln, i, item;

        if (items) {
            // If deep, we have to collect descendants in tree walking order.
            if (deep) {
                result = [];
                for (i = 0, ln = items.length; i < ln; i++) {
                    item = items[i];
                    result[result.length] = item;
                    if (item.getRefItems) {
                        result.push.apply(result, item.getRefItems(true));
                    }
                }
            }
            // Not deep, just return a copy of the items array.
            else {
                result = items.slice();
            }
        }

        // Subclasses might push items into this array, so use a new empty array when
        // there are no results, not Ext.emptyArray.
        return result || [];
    },

    /**
     * Examines this container's `{@link #property-items}` property
     * and gets a direct child component of this container.
     * @param {String/Number} component This parameter may be any of the following:
     *
     * - {String} : representing the `itemId`
     * or `{@link Ext.Component#getId id}` of the child component.
     * - {Number} : representing the position of the child component
     * within the `{@link #property-items}` property.
     *
     * For additional information see {@link Ext.util.MixedCollection#get}.
     * @return {Ext.Component} The component (if found).
     */
    getComponent: function(component) {
        if (typeof component === 'number') {
            return this.getItems().getAt(component);
        }

        if (Ext.isObject(component)) {
            component = component.getItemId();
        }

        return this.getItems().get(component);
    },

    /**
     * Finds a docked item of this container using a reference, `id `or an `index` of its location
     * in {@link #getDockedItems}.
     * @param {String/Number} component The `id` or `index` of the component to find.
     * @return {Ext.Component/Boolean} The docked component, if found.
     */
    getDockedComponent: function(component) {
        if (Ext.isObject(component)) {
            component = component.getItemId();
        }

        var dockedItems = this.getDockedItems(),
            ln = dockedItems.length,
            item, i;

        if (Ext.isNumber(component)) {
            return dockedItems[component];
        }

        for (i = 0; i < ln; i++) {
            item = dockedItems[i];
            if (item.id == component) {
                return item;
            }
        }

        return false;
    },

    doDestroy: function() {
        var me = this;
        
        if (me.focusableContainer) {
            me.destroyFocusableContainer();
        }

        me.removeAll(true, true);
        Ext.destroy(
            me.items,
            me.getMasked()
        );
        me.items = null;

        // We don't want to create one
        if (me._layout) {
            me._layout = Ext.destroy(me._layout);
        }

        me.callParent();
    },

    /**
     * @protected
     * Returns the focus holder element associated with this Container.
     * By default, this is the Container's {@link #focusEl} element;
     * however if {@link #cfg!defaultFocus} is defined, the child component
     * referenced by that property will be found and returned instead.
     *
     * @return {Ext.dom.Element} the focus holding element.
     */
    getFocusEl: function() {
        var delegate = this.findDefaultFocus();
        
        if (delegate) {
            return delegate.isWidget ? delegate.getFocusEl() : delegate;
        }
        else if (this.focusable) {
            return this.focusEl;
        }

        // Containers that are not focusable should not return a focusEl
        return undefined;
    },

    /**
     * Finds the configured default focus item. See {@link #cfg!defaultFocus}.
     */
    findDefaultFocus: function() {
        var result = this.getDefaultFocus();

        // If we have not been configured with a Widget instance, look for a focusable
        // by selector. Then check whether it is focusable. It may be disabled,
        // destroying, or simply set to focusable: false, and the element could
        // still be focusable amd therefore be focused by calling code.
        if (result && !result.isWidget) {
            result = this.down(result);
            if (result && !result.canFocus()) {
                return;
            }
        }

        // Returning undefined is ok
        return result;
    },

    onFocusEnter: function(e) {
        var me = this;
        
        me.callParent([e]);
        
        // We DO NOT check if `me` is focusable here. The reason is that
        // non-focusable containers need to track focus entering their
        // children so that revertFocus would work if these children
        // become unavailable.
        if (me.focusableContainer && !me.destroying && !me.destroyed) {
            me.mixins.focusablecontainer.onFocusEnter.call(me, e);
        }
    },
    
    onFocusLeave: function(e) {
        var me = this;
        
        me.callParent([e]);
        
        // Ditto
        if (me.focusableContainer && !me.destroying && !me.destroyed) {
            me.mixins.focusablecontainer.onFocusLeave.call(me, e);
        }
    },

    updateInnerCls: function (innerCls, old) {
        var el = this.getRenderTarget();

        el.replaceCls(old, innerCls);
    },

    updateAutoSize: function(autoSize) {
        var me = this,
            bodySizerElement = me.bodySizerElement;

        if (autoSize === false) {
            if (!bodySizerElement) {
                me.bodySizerElement = me.bodyElement.wrap({
                    cls: Ext.baseCSSPrefix + 'body-sizer-el'
                });
            }
        } else if (bodySizerElement) {
            me.bodyElement.unwrap();
            bodySizerElement.destroy();
            me.bodySizerElement = null;
        }
    },

    updateMaxHeight: function(maxHeight, oldMaxHeight) {
        var me = this,
            height, stashedHeight, maxHeightElement;

        me.callParent([maxHeight, oldMaxHeight]);

        if (Ext.isIE11 && (maxHeight != null) && (me.getAutoSize() !== false)) {
            me.getMaxHeightElement().setMaxHeight(maxHeight);
            me.addCls(Ext.baseCSSPrefix + 'max-height-wrapped');
        }
    },

    privates: {
        /**
         * This method is in place on the instance during construction to ensure that any
         * {@link #lookup} or {@link #getReferences} calls have the {@link #items} initialized
         * prior to the lookup.
         * @private
         */
        getFirstReferences: function() {
            var me = this;

            delete me.getReferences;
            me.getItems(); // create our items if we haven't yet

            return me.getReferences.apply(me, arguments);
        },

        /**
         * Similar to `getRenderTarget` but for `positioned` items.
         * @param {Ext.Component} item The positioned item being added.
         * @return {Ext.dom.Element}
         * @private
         * @since 6.5.0
         */
        getPositionedItemTarget: function () {
            return this.getRenderTarget();
        },

        /**
         * Applies the container's {@link #defaults} onto a child item. The item
         * can be a config object or an instance but has to be an inner item.
         * @param {Object/Ext.Component} item The item to apply the defaults to.
         * @return {Object/Ext.Component} The item that was passed in
         */
        applyItemDefaults: function (item) {
            var defaults = this.getDefaults();

            if (defaults && !item.ignoreDefaults) {
                if (item.isComponent) {
                    if (item.isInnerItem() && !this.has(item)) {
                        if (Ext.isFunction(defaults)) {
                            defaults = defaults(item);
                        }

                        item.setConfig(defaults, null, {
                            defaults: true
                        });
                    }
                }
                // TODO: revisit this when we have a better story for how to apply defaults
                /*else if (
                    !(
                        //check if config has a config that will make it floating or docked
                        item.hasOwnProperty('left') ||
                        item.hasOwnProperty('right') ||
                        item.hasOwnProperty('top') ||
                        item.hasOwnProperty('bottom') ||
                        item.hasOwnProperty('docked') ||
                        item.hasOwnProperty('centered')
                    )
                ) */
                else {
                    if (Ext.isFunction(defaults)) {
                        defaults = defaults(item);
                    }

                    //make a new object so the config object stays intact
                    item = Ext.merge({}, defaults, item);
                }
            }

            return item;
        },

        setChildRendered: function(rendered, item) {
            if (item.isInnerItem()) {
                this.getLayout().renderInnerItem(item);
            } else if (!rendered || !item.getFloated()) {
                // We do not flag floateds as rendered - they flag themselves as rendered
                // on first show. However, we MUST UNrender and extract floateds from
                // their floatRoot ready to be rendered anew when they are next shown.
                item.setRendered(rendered);
            }
        },

        /**
         * @private
         * In IE11 vertically flexed elements (such as container body-el or panel body-wrap-el)
         * are not flexed properly when the container has a max-height, but no height.
         * We can workaround the issue by wrapping the vertical box in a horizontal box.
         * See EXTJS-24498
         */
        getMaxHeightElement: function() {
            var el = this.el,
                maxHeightElement = this.maxHeightElement,
                selector = '.x-dock,.x-panelheader,.x-body-el,.x-body-wrap-el,.x-tab-guard-el',
                childNodes, node, i, ln;

            if (!maxHeightElement) {
                this.maxHeightElement = maxHeightElement = el.insertFirst({
                    cls: Ext.baseCSSPrefix + 'max-height-el'
                });

                childNodes = Ext.Array.clone(el.dom.childNodes);

                for (i = 1, ln = childNodes.length; i < ln; i++) {
                    node = childNodes[i];

                    if (Ext.fly(node).is(selector)) {
                        maxHeightElement.appendChild(node);
                    }
                }
            }

            return maxHeightElement;
        }
    }

}, function() {
    this.prototype.defaultItemClass = this;
});
