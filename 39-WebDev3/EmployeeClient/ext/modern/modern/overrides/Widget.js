/**
 * @class Ext.Widget
 */

Ext.define('Ext.overrides.Widget', {
    override: 'Ext.Widget',

    mixins: [
        'Ext.mixin.Traversable'
    ],

    requires: [
        'Ext.util.translatable.Abstract'
    ],

    statics: {
        /**
         * @property {Number} floatInset
         * The inset from document edges within which floated components are constrained for readability so that
         * they do not merge into the document edge.
         * The default value is 8.
         */
        floatInset: 8,

        onModalMaskTap: function (e) {
            var top = this.topModal;

            if (top && top.onModalMaskTap && top.onModalMaskTap(e)) {
                this.topModal = null;
            }
        },

        range: document.createRange()
    },

    config: {
        /**
         * @cfg {Number/String/Object} flex
         * The flex of this item *if* this item item is inside a {@link Ext.layout.HBox} or {@link Ext.layout.VBox}
         * layout.
         *
         * You can also update the flex of a component dynamically using the {@link Ext.layout.FlexBox#setItemFlex}
         * method.
         *
         * When supplied as a string or number this option supports the same syntax
         * as CSS [flex](https://developer.mozilla.org/en-US/docs/Web/CSS/flex).
         * For example:
         *
         *     flex: '1 2 auto'
         *
         * sets `flex-grow` property to `0`, `flex-shrink` to `2` and `flex-basis` to
         * `'auto'`.
         *
         * The default `flex-shrink` value for box layout items is set to `0` in the
         * stylesheet, which is different from the browser's default `flex-shrink` value
         * of `1`.  This accommodates the majority use case for applications since where
         * non-flexed components are typically not expected to shrink smaller than their
         * default size.
         *
         * For convenience when only a single number is supplied it is used as the value
         * for both `flex-grow` and `flex-shrink`, for example `flex: 3` is the same as
         * `flex: '3 3'`
         *
         * An object form is also accepted:
         *
         *     flex: {
         *         grow: 1,
         *         shrink: 2,
         *         basis: 'auto'
         *     }
         *
         * When the object form is supplied `shrink` always defaults to `0` regardless
         * of the value of `grow`.
         *
         * Although `'auto'` is the default value for flex-basis, flex-basis defaults to 0%
         * when flex is supplied as a single numeric or string value (e.g. `flex: 1`). If
         * this behavior is not desired either explicitly set flex-basis to `'auto'` or use
         * the object form to set only grow and/or shrink:
         *
         *     flex: {
         *         grow: 2
         *     }
         */
        flex: {
            evented: true,
            $value: null
        },

        /**
         * @cfg {String} id
         * The **unique id of this component instance.**
         *
         * It should not be necessary to use this configuration except for singleton objects in your application. Components
         * created with an id may be accessed globally using {@link Ext#getCmp Ext.getCmp}.
         *
         * Instead of using assigned ids, use the {@link #itemId} config, and {@link Ext.ComponentQuery ComponentQuery}
         * which provides selector-based searching for Sencha Components analogous to DOM querying. The
         * {@link Ext.Container} class contains {@link Ext.Container#down shortcut methods} to query
         * its descendant Components by selector.
         *
         * Note that this id will also be used as the element id for the containing HTML element that is rendered to the
         * page for this component. This allows you to write id-based CSS rules to style the specific instance of this
         * component uniquely, and also to select sub-elements using this component's id as the parent.
         *
         * **Note**: to avoid complications imposed by a unique id also see `{@link #itemId}`.
         *
         * Defaults to an auto-assigned id.
         */

        /**
         * @cfg {String} itemId
         * An itemId can be used as an alternative way to get a reference to a component when no object reference is
         * available. Instead of using an `{@link #id}` with {@link Ext#getCmp}, use `itemId` with
         * {@link Ext.Container#getComponent} which will retrieve `itemId`'s or {@link #id}'s. Since `itemId`'s are an
         * index to the container's internal MixedCollection, the `itemId` is scoped locally to the container - avoiding
         * potential conflicts with {@link Ext.ComponentManager} which requires a **unique** `{@link #id}`.
         *
         * Also see {@link #id}, {@link Ext.Container#query}, {@link Ext.Container#down} and {@link Ext.Container#child}.
         *
         * @accessor
         */
        itemId: undefined,

        /**
         * @cfg {Boolean} [floated=false]
         * A Component may be floated above all other components in the application. This means that the component is absolutely
         * positioned, and will move to the front and occlude other sibling floated component if clicked.
         *
         * A Floated component may have floated descendants. It will bring these decendants to the front with it when brought 
         * to the front of its sibling floated components.
         *
         * By default, descendant floated components are all positioned using the viewport coordinate system. To make a floating 
         * component a positioning parent for descendants, and have the ancestors positioned relatively, configure the parent
         * floated component with `{@link #cfg-relative}: true`.
         *
         * @since 6.2.0
         */
        floated: null,

        /**
         * @cfg {Boolean} [relative=false]
         * *Only valid when a component is `{@link #cfg-floated}`*
         *
         * Configure this as `true` if you require descendant floated components to be positioned  relative to this
         * component's coordinate space, not the viewport's coordinate space.
         * 
         * *Note:* The coordinate space is this Component's encapsulating element's area. Not that of the inner
         * element in which static child items are rendered by the layout.
         *
         * @since 6.2.0
         */
        relative: null,
        
        /**
         * @cfg {Number} [x=0]
         * *Only valid when a component is `{@link #cfg-floated}`*
         *
         * The x position at which to position this component. This is usually viewport-relative. But if there is a
         * `{@link #relative}: true` ancestor, it will be relative to that.
         */
        x: null,
        
        /**
         * @cfg {Number} [y=0]
         * *Only valid when a component is `{@link #cfg-floated}`*
         *
         * The x position at which to position this component. This is usually viewport-relative. But if there is a
         * `{@link #relative}: true` ancestor, it will be relative to that.
         */
        y: null,

        /**
         * @cfg {Boolean} [shadow]
         * Configure as `true` for the component to have a drop shadow. 'false' will suppress any default shadow.
         * By default the theme will determine the presence of a shadow.
         *
         * @since 6.2.0
         */
        shadow: null,

        /**
         * @cfg {Boolean} [shim=false]
         * *Only valid when a component is `{@link #cfg-floated}`*
         *
         * Configure as `true` for the component to use an `<iframe>` as an underlay to ensure certain non-standard
         * browser plugins are occluded by this component.
         *
         * @since 6.2.0
         */
        shim: null,

        /**
         * @cfg {Boolean/Number} [alwaysOnTop=false] A flag indicating that this component should be above its floated siblings.
         *
         * This may be a positive number to prioritize the ordering of multiple visible always on top components.
         *
         * This may be set to a *negative* number to prioritize a component to the *bottom* of the z-index stack.
         *
         * @since 6.2.0
         */
        alwaysOnTop: null,

        /**
         * @cfg {Boolean} [toFrontOnShow=true]
         * True to automatically call {@link #toFront} when a {@link #cfg-floated} Component is shown.
         */
        toFrontOnShow: true,

        // @cmd-auto-dependency { aliasPrefix : "translatable." }
        /**
         * @cfg {Object} translatable
         * @private
         * @accessor
         */
        translatable: {
            lazy: true,
            $value: null
        },

        /**
         * @cfg {String/Ext.util.Region/Ext.dom.Element} [constrainAlign]
         * A specification of the constraint to apply when {@link #showBy} or {@link #alignTo}
         * is called to align a {@link #floated} or positioned component.
         *
         * Defaults to the parent container for *positioned* components (components
         * which have their {@link #cfg!top}, {@link #cfg!right}, {@link #cfg!bottom} or {@link #cfg!left} set
         * to move them out of their container's layout flow).
         *
         * Defaults to the viewport for {@link #floated} components.
         *
         * May be a {@link Ext.ComponentQuery ComponentQuery} selector to find an ancestor
         * component to constrain within.
         *
         * May be `false` to specify that constraining is not applied.
         *
         * You may also specify an element, or a {@link Ext.util.Region Region}
         */
        constrainAlign: null,

        /**
         * @cfg {String} [selfAlign]
         * Specifies the self alignment of this widget in a box layout
         */
        alignSelf: null
    },

    /**
     * @property {Boolean/String}
     * Set to `true` on widgets that should inherit {@link #ui} from their parent container.
     * This property is typically set on the class body, but can be set on an instance as long
     * as it is set prior to the instance being added to its container.  This property is
     * inspected at the moment a widget is added to a container, and any UIs on the container
     * are added to the widget at that time.  Inherited UIs are in addition to the widget's
     * own {@link #ui}, and are updated when the container's UI changes.
     */
    inheritUi: false,

    /**
     * @property {String} [floatingCls="x-floated"] The CSS class to add to this component when it is floated at the viewport level.
     * @private
     * @readonly
     */
    floatedCls: Ext.baseCSSPrefix + 'floated',

    /**
     * @property {String} [floatedSelector=".x-floated"] The CSS selector to match floated elements.
     * @private
     * @readonly
     */
    floatedSelector: '.' + Ext.baseCSSPrefix + 'floated',

    /**
     * @property {String} [shadowCls] The CSS class to add to this component when it has a shadow.
     * @private
     * @readonly
     */
    shadowCls: Ext.baseCSSPrefix + 'shadow',

    /**
     * @property {String} [shadowCls] The CSS class to add to this component should not have a shadow.
     * @private
     * @readonly
     */
    noShadowCls: Ext.baseCSSPrefix + 'no-shadow',
    
    /**
     * @property {String} [floatWrapCls="x-float-wrap"] The CSS class to add to this component's floatWrap when it's created.
     * @private
     * @readonly
     */
    floatWrapCls: Ext.baseCSSPrefix + 'float-wrap',
    
    /**
     * @property {String} [shimCls="x-shim"] The CSS class to add to this component's shim element if enabled.
     * @private
     * @readonly
     */
    shimCls: Ext.baseCSSPrefix + 'shim',

    rootCls: Ext.baseCSSPrefix + 'root',

    /**
     * @event beforetofront
     * Fires before a {@link #cfg-floated} component is brought to the front of the visual stack.
     * @param {Ext.Component} this The component instance
     */

    /**
     * @event tofront
     * Fires when a {@link #cfg-floated} component has been brought to the front of the visual stack.
     * @param {Ext.Component} this The component instance
     */

    /**
     * @private
     */
    isInner: true,

    clearPropertiesOnDestroy: 'async',

    beforeHide: Ext.emptyFn,

    afterHide: function() {
        var me = this,
            parent = me.getParent();

        if (parent && parent.afterItemHide) {
            parent.afterItemHide(me);
        }

        if (me.getFloated()) {
            me.syncShim();
        }
    },

    beforeShow: Ext.emptyFn,

    afterShow: function() {
        var me = this,
            parent = me.getParent();

        if (parent && parent.afterItemShow) {
            parent.afterItemShow(me);
        }
    },

    applyItemId: function(itemId) {
        return itemId || this.getId();
    },

    doDestroy: function() {
        var me = this,
            parent = me.getParent(),
            fw = me.floatWrap;

        if (parent && parent.remove) {
            parent.remove(me, false);
        }
        
        me.setShim(false);
        Ext.destroy(me.getTranslatable());

        if (fw) {
            me.un('resize', 'syncFloatWrap', me);
            fw.destroy();
            me.floatWrap = null;
        }

        me.removeBindings();

        me.callParent();
    },

    isInnerItem: function() {
        return this.isInner;
    },

    isCentered: function() {
        return false;
    },

    isDocked: function() {
        return Boolean(this.getDocked());
    },

    isPositioned: function() {
        return false;
    },

    getDocked: function() {
        return this._docked;
    },

    /**
     * Returns `true` if this Component is currently hidden.
     * @param {Boolean/Ext.Widget} [deep=false] `true` to check if this component
     * is hidden because a parent container is hidden. Alternatively, a reference to the
     * top-most parent at which to stop climbing.
     * @return {Boolean} `true` if currently hidden.
     */
    isHidden: function(deep) {
        var me = this,
            hidden;

        // While configuring, this is a local operation
        // and we peek at the incoming configuration values.
        // Floated components and non-floated components have
        // different defaults in response to an initial hidden
        // state of null. Normalize that here.
        if (me.isConfiguring) {
            hidden = me.getConfig('hidden', true);

            // Floateds default to hidden: true
            if (me.getConfig('floated', true)) {
                return hidden !== false;
            }
            // Non floateds default to hidden: false
            else {
                return !!hidden;
            }
        } else {
            return this.callParent([deep]);
        }
        return me.callParent([deep]);
    },

    /**
     * @private
     */
    onAdded: function(parent, instanced) {
        var me = this,
            currentParent = me.parent;

        if (currentParent && currentParent !== parent) {
            currentParent.remove(me, false);
        }

        me.parent = parent;

        me.onInheritedAdd(parent, instanced);

        // this component is no longer detached from the body
        me.isDetached = false;

        // If we are floated, register with a floatParent
        if (me.getFloated()) {
            me.findFloatParent();
        }

        if (me.inheritUi) {
            me.doInheritUi();
        }
    },

    onRemoved: function(destroying) {
        var me = this;

        if (me.inheritUi && !destroying) {
            me.doUninheritUi();
        }

        me.onInheritedRemove(destroying);

        me.parent = null;
    },

    setIsInner: function(isInner) {
        var parent;

        if (isInner !== this.isInner) {
            this.isInner = isInner;

            parent = this.initialized && this.getParent();
            if (parent) {
                parent.onItemInnerStateChange(this, isInner);
            }
        }
    },

    refreshInnerState: function() {
        this.setIsInner(!this.getFloated() && !this.isCentered() && !this.isPositioned() && !this.isDocked());
    },

    /**
     * Brings a {@link #cfg-floated} Component to the front of any other visible, floated
     * Components while honoring all {@link #cfg!alwaysOnTop} settings. This may not become
     * topmost if another visible floated component has a higher {@link #cfg!alwaysOnTop} value.
     *
     * If this Component becomes the topmost *modal* floated component, the the shared modal
     * mask is moved to just below this Component.
     * @param {Boolean} [fromMousedown] (private)
     * @return {Ext.Component} this
     */
    toFront: function(fromMousedown) {
        //<debug>
        if (!this.getFloated()) {
            Ext.raise('Cannot use toFront on a non-floated component');
        }
        //</debug>
        var me = this,
            floatParent = me.getFloatParent();

        if (!me.hasListeners.beforetofront || me.fireEvent('beforetofront', me) !== false) {
            me.syncAlwaysOnTop(fromMousedown);

            // All floatParents must move to the front of their own floatWraps
            // If we hit the floatRoot, it's not associated with a floated component
            // which could need moving, so there will be no component
            if (floatParent && floatParent.getFloated()) {
                floatParent.toFront(fromMousedown);
            }

            if (me.hasListeners.tofront) {
                me.fireEvent('tofront', me);
            }
        }

        return me;
    },

    applyTranslatable: function (config, translatable) {
        return Ext.Factory.translatable.update(translatable, config, this,
            'createTranslatable');
    },

    createTranslatable: function (config) {
        var me = this,
            listeners = config.listeners;

        config = Ext.apply({
            type: 'cssposition',
            ownerCmp: me,
            element: me.renderElement
        }, config);

        // The resolveListenerScope will handle this case, but this saves many
        // function calls during mousemove...
        if (listeners && listeners.scope === 'this') {
            config.listeners = listeners = Ext.apply({}, listeners);
            listeners.scope = me;
        }

        return config;
    },

    ensureTranslatable: function () {
        var me = this,
            translatable = me.getTranslatable();

        if (!translatable) {
            me.setTranslatable(true);
            translatable = me.getTranslatable();
        }

        return translatable;
    },

    translate: function() {
        var translatable = this.ensureTranslatable();

        translatable.translate.apply(translatable, arguments);
    },

    /**
     * Prepares information on aligning this to component using alignment.
     * Also checks to see if this is already aligned to component according to alignment.
     * @protected
     */
    getAlignmentInfo: function (component, alignment){
        var me = this,
            alignToBox = component.isRegion ? component : (component.isWidget ? component.renderElement : Ext.fly(component)).getBox(),
            element = me.renderElement,
            box = element.getBox(),
            stats = {
                alignToBox: alignToBox,
                alignment: alignment,
                top: alignToBox.top,
                left: alignToBox.left,

                // Might be an Ext.util.Point which does not have dimensions.
                alignToWidth: alignToBox.width || 0,
                alignToHeight: alignToBox.height || 0,

                width: box.width,
                height: box.height,

                // getAnchor returns the element, so cast to boolean
                // otherwise the comparison below rejects it.
                anchor: !!(me.getAnchor && me.getAnchor())
            },
            currentAlignmentInfo = me.getCurrentAlignmentInfo(),
            isAligned = true;

        if (!Ext.isEmpty(currentAlignmentInfo)) {
            Ext.Object.each(stats, function(key, value) {
                if (!Ext.isObject(value) && currentAlignmentInfo[key] !== value) {
                    isAligned = false;
                    return false;
                }
                return true;
            });
        } else {
            isAligned = false;
        }

        return {isAligned: isAligned, stats: stats};
    },

    /**
     * Current Alignment information from the last alignTo call
     * @private
     */
    getCurrentAlignmentInfo: function() {
        return this.$currentAlignmentInfo;
    },

    /**
     * Sets the current Alignment information, called by alignTo
     * @private
     */
    setCurrentAlignmentInfo: function(alignmentInfo) {
        this.$currentAlignmentInfo = Ext.isEmpty(alignmentInfo) ? null : Ext.merge({}, alignmentInfo.stats ? alignmentInfo.stats : alignmentInfo);
    },

    /**
     * @private
     */
    alignTo: function(component, alignment, options) {
        var me = this,
            alignmentInfo = me.getAlignmentInfo(component, alignment),
            config = me.initialConfig,
            positioned = !me.getFloated(),
            setX = positioned ? me.setLeft : me.setX,
            setY = positioned ? me.setTop : me.setY,
            oldHeight, resultRegion;

        if (alignmentInfo.isAligned) {
            return;
        }

        if ('unconstrainedWidth' in me) {
            me.setWidth(me.unconstrainedWidth);
        }
        if ('unconstrainedHeight' in me) {
            me.setHeight(me.unconstrainedHeight);
        }
        resultRegion = me.getAlignRegion(component, alignment, options);

        setX.call(me, resultRegion.x);
        setY.call(me, resultRegion.y);
        if (resultRegion.constrainWidth) {
            me.unconstrainedWidth = config.width || me.self.prototype.width;

            // We must deal with height changeing if we restrict width and we are aliging above
            oldHeight = me.el.getHeight();
            me.setWidth(alignmentInfo.stats.width = resultRegion.getWidth());

            // We are being positioned above, bump upwards by how much the
            // element has expanded as a result of width restriction.
            if (resultRegion.align.position === 0) {
                setY.call(me, resultRegion.y + (oldHeight - me.el.getHeight()));
            }
        }
        if (resultRegion.constrainHeight) {
            me.unconstrainedHeight = config.height || me.self.prototype.height;
            me.setHeight(alignmentInfo.stats.height = resultRegion.getHeight());
        }

        // Cache the alignment options for any realign call which might happen on
        // viewport resize or configuration change.
        me.alignToArgs = [component, alignment, options];

        me.setCurrentAlignmentInfo(alignmentInfo);
    },

    /**
     * @private
     */
    realign: function(component, alignment, options) {
        var args = this.alignToArgs;

        if (this.isVisible()) {
            this.alignTo.call(this, component || args[0], alignment || args[1], options || args[2]);
        }
    },

    /**
     * @private
     */
    getAlignRegion: function(component, alignment, options) {
        var me = this,
            alignmentInfo = me.getAlignmentInfo(component, alignment),
            constrainModifier,
            inside;

        if (alignmentInfo.isAligned) {
            return;
        }

        var alignToBox = alignmentInfo.stats.alignToBox,
            constrainBox = me.getConstrainAlignRegion(),
            height = alignmentInfo.stats.height,
            width = alignmentInfo.stats.width;

        if (constrainBox && (!alignment || alignment === 'auto')) {
            if (constrainBox.bottom - alignToBox.bottom < height) {
                if (alignToBox.top - constrainBox.top < height) {
                    if (alignToBox.left - constrainBox.left < width) {
                        alignment = 'l-r?';
                    }
                    else {
                        alignment = 'r-l?';
                    }
                }
                else {
                    alignment = 'b-t?';
                }
            }
            else {
                alignment = 't-b?';
            }
        }

        constrainModifier = alignment[alignment.length - 1];

        // If the assertive form was used (like "tl-bl!"), constrain to the align to component.
        if (constrainModifier === '!') {
            inside = component.isRegion ? component : (component.isWidget ? component.renderElement : Ext.fly(component)).getBox();
            alignment = alignment.substr(0, alignment.length - 1);
        }
        // Constraining is always applied if the constrainAlign config is set
        else {
            inside = constrainBox;
            if (constrainModifier === '?') {
                alignment = alignment.substr(0, alignment.length - 1);
            }
        }

        return me.el.getRegion().alignTo(Ext.apply({
            target: Ext.util.Region.from(alignmentInfo.stats.alignToBox),
            align: alignment,
            inside: inside,
            minWidth: me.getMinWidth && me.getMinWidth(),
            minHeight: me.getMinHeight && me.getMinHeight()
        }, options));
    },

    //<debug>
    render: function(container, insertBeforeElement) {
        if (this.getFloated()) {
            Ext.raise('floated: true components cannot be rendered. They render themselves on first show');
            return;
        }
        this.callParent([container, insertBeforeElement]);
    },
    //</debug>

    /**
     * This method is called after the component is initially added to the DOM. If this
     * component {@link Ext.Container contains} other components, the `afterRender` method
     * for child components is called *before* the parent's `afterRender`.
     *
     * Implementations of this method should avoid reading from the DOM but are free to
     * write to the DOM as needed. To read the DOM, consider implementing
     * {@link #onRender onRender} instead.
     *
     * This method is not generally needed because components always have their own DOM
     * {@link #property!element elements} and these are maintained by config property
     * updaters prior to insertion in the DOM. In general, it is always best to manipulate
     * the component's elements outside the DOM where there is no associated reflow or
     * layout cost. This method is useful for situations where the component's elements
     * must be in the DOM in order to be manipulated correctly.
     *
     * @template
     * @method afterRender
     * @since 6.5.0
     */
    afterRender: Ext.emptyFn,

    /**
     * @method
     * This method is called the first time a component is inserted into the DOM. If this
     * component {@link Ext.Container contains} other components, the `onRender` method
     * for child components is called *after* the parent's `onRender`.
     *
     * Implementations of this method should avoid modifying the DOM but are free to read
     * from and measure elements as needed. To adjust the DOM, consider implementing
     * {@link #afterRender afterRender} instead.
     *
     * If this method is overridden, be sure to use `callParent` to call the base class
     * version.
     *
     *      onRender: function () {
     *          this.callParent();
     *
     *          // custom actions
     *      }
     *
     * This method is not generally needed because components always have their own DOM
     * {@link #property!element elements} and these are maintained by config property
     * updaters prior to insertion in the DOM. In general, it is always best to interrogate
     * the component's elements outside the DOM where there is no associated reflow or
     * layout cost. This method is useful for situations where the component's elements
     * must be in the DOM. For example to be measured correctly.
     *
     * @template
     * @since 6.5.0
     */
    onRender: Ext.emptyFn,

    // floated

    applyFloated: function (floated) {
        return Boolean(floated);
    },

    updateFloated: function (floated, oldFloated) {
        var me = this,
            fw = me.floatWrap,
            modal, sibling;

        if (floated) {
            me.refreshInnerState = Ext.emptyFn;

            if (me.isPositioned()) {
                me.resetPositioned();
            }

            if (me.isDocked()) {
                me.setDocked(false);
            }

            delete me.refreshInnerState;
        } else {
            // If transitioning to an inner component, unwrap ourself.
            if (fw) {
                fw.dom.removeChild(me.el.dom);
                me.un('resize', 'syncFloatWrap', me);
                fw.destroy();
                me.floatWrap = null;
                me.setRendered(false);
            }
        }

        // Will pull a newly floated item out of the container
        // or put a newly unfloated item in the container.
        // So if moving from oldFloated===false to floated===true
        // we will efectively be derendered.
        me.refreshInnerState();

        me.el.toggleCls(me.floatedCls, floated);

        // If we are *changing* floatedness, then if the modal config has been
        // processed, we need to clear it and re-evaluate it for the new mode.
        //
        // So for floated to non-floated, the _modal property will have
        // to cycle from true to an instance of Ext.Mask. And vice versa
        // for non-floated - floated.
        if (me.hasOwnProperty('_modal')) {
            modal = me.getModal && me.getModal();

            if (modal) {
                me.setModal(false);
                if (floated) {
                    // If we are being changed *to* floated, then the modal
                    // will be the mask instance, so destroy it.
                    // And if we're visible, show the shared mask.
                    Ext.destroy(modal);
                    if (me.isVisible()) {
                        me.showModalMask();
                    }
                } else {
                    // We're being changed to non-floated.
                    // The shared modal mask must be moved to below the topmost
                    // modal sibling, or if none, hidden.
                    sibling = me.getModalSibling();

                    if (sibling) {
                        sibling.showModalMask();
                    } else {
                        me.hideModalMask();
                    }
                }
                me.setModal(true);
            }
            if (me.getHideOnMaskTap && me.getHideOnMaskTap()) {
                me.setHideOnMaskTap(false);
                me.setHideOnMaskTap(true);
            }
        }

        // Update in-DOM state.
        // If component *was* floated, it will have been hoiked out of
        // the DOM, so rendered will be passed in as false
        me.syncFloatedState(floated, oldFloated, me.rendered && oldFloated === false);
    },
        
    applyUi: function(ui, oldUi) {
        var me = this,
            inheritedUi = me._inheritedUi;

        if (inheritedUi) {
            // if some of our UI's are inherited from our parent, let's not remove those
            ui = me.doAddUi(inheritedUi, ui);
        }

        return ui;
    },

    updateUi: function(ui, oldUi) {
        var me = this,
            item, refItems, i, n;

        me.callParent([ui, oldUi]);

        if (me.$inheritUiCount) {
            refItems = me.getRefItems();

            for (i = 0, n = refItems.length; i < n; i++) {
                item = refItems[i];

                if (item.inheritUi) {
                    item.doUninheritUi();
                    item.doInheritUi();
                }
            }
        }
    },

    updateHidden: function(hidden, oldHidden) {
        var globals = Ext.GlobalEvents,
            event = hidden ? 'hide' : 'show';

        this.callParent([hidden, oldHidden]);

        if (!this.isConfiguring && globals.hasListeners[event]) {
            globals.fireEvent(event, this);
        }
    },

    updateAlignSelf: function (align) {
        this.el.setStyle({
            'align-self': align
        });
    },

    privates: {
        /**
         * All Components need a potentially recursive setRendered because some are
         * pseudo containers, such as grid {@link Ext.grid.Row rows}, and some mix in
         * {@link Ext.mixin.Toolable Toolable}.
         * @param {Boolean} rendered
         * @param {Boolean} [root]
         * @private
         */
        setRendered: function (rendered, root) {
            var me = this,
                afterRenderQueue, item, items, ln, i;

            if (!rendered && me.rendered && me.getFloated()) {
                // A floated being derendered must return to pre render state which is out of the document.
                // Component#preprocessShow will ensure that a floated finds its correct floatParent
                // and inserts itself into the floatRoot.
                me.floatWrap.dom.parentNode.removeChild(me.floatWrap.dom);
            }

            me.rendered = rendered;

            if (!me.destroying && !me.destroyed) {
                if (rendered && me.onRender) {
                    me.initBindable();
                    me.initKeyMap();

                    if (!me.onRender.$nullFn) {
                        me.onRender();
                    }

                    me.onRender = null;  // only call onRender() the first time we render

                    // Queue the comp before descent since we pop() from this queue
                    // to call afterRender ... This yields bottom-up order on afterRender
                    // and top-down order for onRender.
                    if (!me.afterRender.$nullFn) {
                        (Ext._afterRenderQueue || (Ext._afterRenderQueue = [])).push(me);
                    }
                }

                // Call getRefItems after onRender in case it changes the result
                items = me.getRefItems && me.getRefItems();
                ln = items && items.length;
                for (i = 0; i < ln; i++) {
                    item = items[i];
                    // Some getRefItems (eg: D3/Charts) return non-Component child items
                    // so detect the presence of setRendered.
                    if (item.setRendered) {
                        me.setChildRendered(rendered, item);
                    }
                }

                if (me.focusableContainer && me.initFocusableContainer) {
                    me.initFocusableContainer();
                }

                if (root) {
                    afterRenderQueue = Ext._afterRenderQueue;

                    if (afterRenderQueue) {
                        while (afterRenderQueue.length) {
                            item = afterRenderQueue.pop();

                            if (!item.destroyed) {
                                item.afterRender();
                            }
                        }
                    }
                }
            }
        },

        setChildRendered: function(rendered, item) {
            // We do not flag floateds as rendered - they flag themselves as rendered
            // on first show. However, we MUST UNrender and extract floateds from
            // their floatRoot ready to be rendered anew when they are next shown.
            if (!rendered || !item.getFloated()) {
                item.setRendered(rendered);
            }
        },

        hideFromModal: function() {
            this.hide();
        },

        /**
         * @private
         * Returns `true` if the passed element is within the container tree of this component.
         *
         * For example if a menu's submenu contains an {@link Ext.form.field.Date}, that top level
         * menu owns the elements of the date picker. Using this method, you can tell if an event took place
         * within a certain component tree.
         */
        owns: function(element) {
            var result = false,
                cmp;

            if (element.isEvent) {
                element = element.target;
            } else if (element.isElement) {
                element = element.dom;
            }

            cmp = Ext.Component.from(element);

            if (cmp) {
                result = (cmp === this) || (!!cmp.up(this));
            }

            return result;
        },

        /**
         * @private
         */
        doInheritUi: function() {
            var me = this,
                owner, ownerUi;

            if (me.inheritUi) {
                me._ownUi = me.getUi();
                owner = me.getRefOwner();
                ownerUi = owner.getUi();

                if (ownerUi) {
                    me.addUi(ownerUi);
                    me._inheritedUi = ownerUi;
                }

                owner.$inheritUiCount = (owner.$inheritUiCount || 0) + 1;
            }
        },

        /**
         * @private
         */
        doUninheritUi: function () {
            var me = this,
                inheritUi = me.inheritUi,
                ownUi, owner;

            if (inheritUi)  {
                owner = me.getRefOwner();
                ownUi = me._ownUi;
                me._ownUi = null;
                me._inheritedUi = null;

                me.setUi(ownUi || null);

                if (owner.$inheritUiCount) {
                    --owner.$inheritUiCount;
                }
            }
        },

        getBubbleTarget: function() {
            return this.getParent();
        },

        getConstrainAlignRegion: function() {
            var me = this,
                isFloated = me.getFloated(),
                constrainAlign = me.getConstrainAlign(),
                parent, isViewport, docInsets;

            if (constrainAlign !== false) {
                if (typeof constrainAlign === 'string') {
                    constrainAlign = parent = me.up(constrainAlign);
                }
                if (!constrainAlign) {
                    // If we're floated, find the floatRoot's owning component.
                    // If not floated, use parent component.
                    parent = isFloated ? me.floatParentNode.getData().component : me.getParent();
                }

                // If there is an owning component, constrain into its renderTarget
                // if it's a container, or it's bodyElement or element if not.
                if (parent) {
                    // If the owner is a non-relative floated, the constraint is the viewport
                    if (parent.getFloated() && !parent.getRelative()) {
                        constrainAlign = Ext.getBody();
                        isViewport = true;
                    } else {
                        constrainAlign = parent.getRenderTarget ? parent.getRenderTarget() : (parent.bodyElement || parent.element);
                        isViewport = parent.isViewport;
                    }
                }
                // No owning component and no constrainAlign; use viewport for floated, parent el for positioned.
                else if (!constrainAlign) {
                    if (isFloated) {
                        isViewport = true;
                        constrainAlign = Ext.getBody();
                    } else {
                        constrainAlign = me.element.parent();
                    }
                }

                if (!constrainAlign.isRegion) {
                    constrainAlign = Ext.fly(constrainAlign).getConstrainRegion();

                    // Inset from viewport edge for readability
                    if (isViewport) {
                        // Round to avoid subpixel errors
                        docInsets = Math.round(Ext.Widget.floatInset);
                        constrainAlign.adjust(docInsets, -docInsets, -docInsets, docInsets);
                    }
                }
                return constrainAlign;
            }
        },

        /**
         * *For {@link #cfg-floated} components only. *
         *
         * Finds the owning {@link #cfg-floated} component (if any) responsible for
         * the base z-index stack position of this compoonent, and, if that component
         * is {@link #cfg-relative}, for the coordinate system in which this component
         * is positioned.
         *
         * If this is a top level floated component, this method will return `null`
         * @return {Ext.Component} The owning floated component or `null` if this
         * component is top level floated.
         * @private
         */
        getFloatParent: function() {
            var result = this.floatParentNode.getData().component;
            return result && result.getFloated() ? result : null;
        },

        syncFloatedState: function(floated, oldFloated, rendered) {
            var me = this,
                isHidden = me.isHidden();

            if (floated) {
                if (rendered) {
                    if (me.isCentered()) {
                        // Ensure any configs are pulled through
                        me.getWidth();
                        me.getHeight();
                        me.center();
                    } else {
                        me.syncXYPosition();
                    }
                    if (!isHidden) {
                        me.showModalMask();
                    }
                }
                // Not rendered - insert into correct floatParentNode unless we are hidden
                else {
                    if (!isHidden) {
                        me.findFloatParent();
                    }
                    else {
                        // Floated components start hidden but hidden-ness is not
                        // normally reflected in an x-hidden class... they just
                        // aren't inserted in the DOM. The problem comes when we
                        // add a floated comp to a container. It gets put into the
                        // DOM early, but it will appear because we don't have the
                        // styles in place to prevent it. Since we are "effectively
                        // hidden" here we just make sure we have the right styles.
                        me.setHidden(true);
                    }
                }
            }
            // Moving back into a container.
            // undo any translation
            else {
                me.translate(0, 0, 0);
            }
        },

        /**
         * The method finds this floated component's floatParent. That means a DOM positioning container
         * which acts as a root element for sibling floated components, and allows allows floated components
         * to be absolutely positioned, and their encapsulating elements to be reordered to produce a visual
         * stacking effect.
         *
         * This component's element is appended to its floatParent.
         *
         * There is a global floatParent element, created on demand when the first top level floated component
         * is shown. This may be an item child of a container configured with `{@link #cfg-floated}: true`,
         * or a free `floated` component which is programatically {@link Ext.Component#show shown}.
         *
         * Child items of components inside a floated component may also be configured `floated`. These
         * are give a floatParent which is created on demand wrapping the nearest `floated` ancestor.
         * This means that when that ancestor's element is brought to the top of the stack (by moving its
         * element to the end of its own floatParent), the descendant elements will automatically remain above.
         *
         * @private
         */
        findFloatParent: function(needsShow) {
            var me = this,
                parent = me.getRefOwner();

            // Climb to the nearest floated if possible
            while (parent && !parent.getFloated()) {
                parent = parent.getRefOwner();
            }

            // Hit the top seeing no floateds; use the global floatRoot
            // The property floatParentNode is an Element.
            // It cannot be called floatParent because that is used by getRefOwner in the case
            // of no ownerCt/parent/$initParent etc.
            if (!parent) {
                me.floatParentNode = Ext.getFloatRoot();
            }
            // Use the nearest floating ancestor's floatRoot wrapper.
            else {
                me.floatParentNode = parent.getFloatWrap();
            }

            me.insertFloatedDom(needsShow);
        },

        /**
         * This method returns, or creates on demand the floatWrap element which wraps the passed
         * floated component. It enables that floated component to act as a host for descendant floated
         * components.
         *
         * @return {Ext.Element} The passed component's floatWrap element.
         * @private
         */
        getFloatWrap: function() {
            var me = this,
                fw = me.floatWrap;

            if (!fw) {
                me.floatWrap = fw = Ext.get(Ext.DomHelper.createDom({
                    cls: me.floatWrapCls,
                    id: me.id + '-floatWrap',
                    "data-componentId": me.id
                }));

                me.on('resize', 'syncFloatWrap', me);

                // Need a link to the owning component so that floateds which are hosted
                // in this element can easily find their floatParent component to move it to
                // front.
                fw.getData().component = me;

                if (me.isContainer && (!me.isWidthed() || !me.isHeighted())) {
                    me.setAutoSize(true);
                }

                // We wrap ourselves in this, and it becomes the hosting element for
                // child floaters.
                fw.dom.appendChild(me.element.dom);

                // alwaysOnTop flag duplicated in the floatWrap so syncAlwaysOnTop can sort.
                fw.getData().alwaysOnTop = me.element.getData().alwaysOnTop;

                me.syncFloatWrap();
            }
            return fw;
        },

        //<debug>
        applyRenderTo: function(renderTo) {
            if (renderTo && this.getFloated()) {
                Ext.raise('floated: true components cannot be rendered. They render themselves on first show');
                return;
            }
            return renderTo;
        },
        //</debug>

        /**
         * Synchronizes the size and position of the {@link #getFloatWrap floatWrap}
         * when this Widget is resized
         * @private
         */
        syncFloatWrap: function() {
            var me = this,
                floatWrap = me.floatWrap,
                mySize = me.el.getSize(),
                mask;

            // If we are positioning child floateds in our address space,
            // size the floatWrap in which child floateds are rendered.
            if (me.getRelative()) {
                floatWrap.setSize(mySize);
            } else {
                mask = floatWrap.getData().modalMask;
                if (mask) {
                    mask.setSize(mySize);
                }
            }
        },

        /**
         * This method inserts this floated component's DOM into its owning floatParent.
         * @private
         */
        insertFloatedDom: function(needsShow) {
            var me = this,
                fw = me.getFloatWrap(),
                floatParentNode = me.floatParentNode;

            if (fw.dom.parentNode !== floatParentNode.dom) {
                floatParentNode.dom.appendChild(me.getFloatWrap().dom);
                if (needsShow) {
                    // The following operations require that the component be
                    // temporarily visible for measurement purposes. They will
                    // be undone by outside callers
                    me.setVisibility(true);
                    me._hidden = false;
                }
                me.setRendered(true, true);
                me.syncXYPosition();
            }
        },

        applyShim: function(shim) {
            //<debug>
            if (shim && !this.getFloated()) {
                Ext.raise('Cannot use setShim on a non-floated component');
            }
            //</debug>
            if (shim) {
                // Allow shim config options to be passed.
                return Ext.getBody().createChild(Ext.apply({
                    cls: this.shimCls
                }, shim));
            } else {
                Ext.destroy(this.shim);
                return null;
            }
        },

        updateShim: function(shim, oldShim) {
            var me = this;

            if (shim) {
                me.syncShim();

                if (!oldShim) {
                    me.on('resize', 'syncShim', me);
                }
            } else if (oldShim) {
                me.un('resize', 'syncShim', me);
            }
        },

        hideModalMask: function() {
            var me = this,
                floatRoot = Ext.getFloatRoot(),
                floatParentNode = me.floatParentNode,
                data, mask;
            
            // If we're hidden there may not be a parent node
            if (floatParentNode) {
                data = floatParentNode.getData();

                // If our floatParent is not relative we will be using the root's mask.
                if (floatParentNode !== floatRoot && !data.component.getRelative()) {
                    data = floatRoot.getData();
                }

                mask = data.modalMask;
                if (mask && mask.dom.parentNode) {
                    mask = mask.dom;
                    Ext.getDetachedBody().appendChild(mask);
                }
            }
        },

        showModalMask: function() {
            var me = this,
                Widget = Ext.Widget,
                floatRoot = Ext.getFloatRoot(),
                positionEl = me.getFloatWrap(),
                parent = me.getParent(),
                floatParentNode = me.floatParentNode,
                data = floatParentNode.getData(),
                mask;

            if (me.getFloated() && me.getModal && me.getModal()) {

                // If our floatParent is not relative we use the root's mask.
                // Obviously, if we are a top level floated, this will already
                // be the case.
                if (floatParentNode !== floatRoot && !data.component.getRelative()) {
                    data = floatRoot.getData();
                }

                // getModal might cause this to be created.
                mask = data.modalMask;
                if (mask) {
                    // IE11 freaks out on insertBefore if positionEl is not yet
                    // in the dom
                    if (positionEl.dom.parentElement === floatParentNode.dom) {
                        floatParentNode.dom.insertBefore(mask.dom, positionEl.dom);
                    }
                    else {
                        floatParentNode.dom.appendChild(mask.dom);
                    }
                }
                else {
                    mask = data.modalMask = floatParentNode.createChild({
                        cls: 'x-mask'
                    }, positionEl);

                    mask.on({
                        tap: Widget.onModalMaskTap,
                        scope: Widget
                    });

                    // A know issue with Safari Mobile causes a body with overflow: hidden
                    // to be scrollable on iOS.
                    // https://bugs.webkit.org/show_bug.cgi?id=153852
                    if (Ext.isiOS && floatParentNode === floatRoot) {
                        mask.on({
                            touchmove: function (e) {
                                e.preventDefault();
                            }
                        });
                    }
                }
                Widget.topModal = me;

                // Ensure that the mask is sized and positioned if
                // parent is not relative
                if (parent && parent.getFloated() && !parent.getRelative()) {
                    parent.syncXYPosition();
                }
            }
        },

        syncShim: function() {
            var me = this,
                shim = me.getShim();

            if (shim) {
                if (me.isVisible(true)) {
                    shim.show();
                    me.getFloatWrap().dom.insertBefore(shim.dom, me.el.dom);
                    shim.setSize(me.getSize());
                } else {
                    shim.hide();
                }
            }
        },
        
        updateAlwaysOnTop: function(alwaysOnTop) {
            this.getFloatWrap().getData().alwaysOnTop = Number(alwaysOnTop);
            this.syncAlwaysOnTop();
        },

        /**
         * @private
         * Fixes up the alwaysOnTop order of this floated widget within its siblings.
         * @param fromMousedown (private)
         * @return {Boolean} `true` if this was the topmost widget among its siblings.
         */
        syncAlwaysOnTop: function(fromMousedown) {
            var me = this,
                positionEl = me.getFloatWrap().dom,
                parentEl = me.floatParentNode,
                nodes = parentEl.dom.childNodes,
                len = nodes.length,
                i, startIdx,
                alwaysOnTop = Number(me.getAlwaysOnTop()),
                range = me.statics().range,
                refNode, isTopModal;

            // If already at end, no node movement necessary
            if (positionEl.nextSibling) {

                // Fastest way of seeing whether we need to move the modal mask
                // to just below our positionEl.
                isTopModal = me.getModal() && positionEl.previousSibling && Ext.fly(positionEl.previousSibling).hasCls(Ext.Mask.prototype.baseCls);

                // Start from 1.
                // All elements if floatRoot are considered, The first element in child floatWraps
                // is the child floated which owns that floatWrap.
                startIdx = parentEl === Ext.floatRoot ? 0 : 1;
                for (i = len - 1; i >= startIdx; i--) {
                    // Do not include shim elements in the comparison
                    // Do not include our own element in the comparison.
                    if (!Ext.fly(nodes[i]).is('.' + me.shimCls) && nodes[i] !== positionEl) {
                        // If we've gone back to find a node that should be below us,
                        // grab its next sibling as the refNode to insertBefore.
                        if (alwaysOnTop >= (Ext.get(nodes[i]).getData().alwaysOnTop || 0)) {
                            refNode = nodes[i].nextSibling;
                            break;
                        }
                    }
                }
                // Already in correct position
                if (refNode === positionEl) {
                    return;
                }

                // If we didn't find a node we are greater than, go to bottom of stack
                if (i < startIdx) {
                    refNode = nodes[0];
                }

                // If we contain focus, or this is triggered by a mousedown,
                // then preserve this element's DOM, and move siblings around it.
                if (me.containsFocus || fromMousedown) {

                    // Nodes to move to before our positionEl
                    range.setStartAfter(positionEl);
                    range.setEndAfter(refNode || nodes[len - 1]);

                    // Move before nodes to before the positionEl
                    parentEl.dom.insertBefore(range.extractContents(), positionEl);
                } else {
                    parentEl.dom.insertBefore(positionEl, refNode);
                }

                // Keep shims in line.
                if (isTopModal) {
                    me.showModalMask();
                }
                me.syncShim();
            }

            if (refNode) {
                Ext.Component.from(refNode).syncShim();
            } else {
                return true;
            }
        },

        updateRelative: function() {
            this.syncXYPosition();
        },

        updateShadow: function(shadow) {
            this.el.toggleCls(this.shadowCls, shadow);
            this.el.toggleCls(this.noShadowCls, shadow === false);
        },

        updateX: function() {
            //<debug>
            if (!this.getFloated()) {
                Ext.raise('Cannot use setX on a non-floated component');
            }
            //</debug>

            if (!this.$updatingXY) {  // set Ext.Compoentn#setXY
                this.syncXYPosition();
            }
        },

        updateY: function() {
            //<debug>
            if (!this.getFloated()) {
                Ext.raise('Cannot use setY on a non-floated component');
            }
            //</debug>

            if (!this.$updatingXY) {
                this.syncXYPosition();
            }
        },

        /*
         * Only applicable to floated components.
         * Ensures correct position after either {@link #cfg-x} or {@link #cfg-x} have been set.
         * If we are positioning descendant floateds relatively, then the
         * wrapping floatWrap is used to position both us and our descendant floateds
         * @private
         */
        syncXYPosition: function (animation) {
            var me = this,
                floatWrap = me.getFloatWrap(),
                maskAnim = animation,
                mask,
                x = me.getX() || 0,
                y = me.getY() || 0;

            // If we are configured to relatively position our descendants, then we ourselves
            // are positioned by our floatWrap element
            if (me.getRelative()) {
                floatWrap.translate(x, y, animation);
                floatWrap.setWidth(me.el.getWidth());
                floatWrap.setHeight(me.el.getHeight());

                me.translate(0, 0);

                mask = floatWrap.getData().modalMask;
                if (mask) {
                    mask.translate(0, 0);
                }
            }
            // Descendants to be positioned absolutely, just position our element.
            else {
                me.translate(x, y, animation);

                mask = floatWrap.getData().modalMask;
                if (mask) {
                    if (maskAnim) {
                        maskAnim = Ext.apply({}, maskAnim);
                        delete maskAnim.callback;
                    }

                    mask.translate(x, y, maskAnim);
                }
            }

            me.syncShim();
        }
    }
}, function(Widget) {
    
    this.borrow(Ext.util.Positionable, ['clipTo', 'clearClip']);

    // Convenience shorthand sibling traverse methods.
    // Maintainer: These methods are brought in by the Traversable mixin/
    Widget.createAlias({
        prev: 'previousSibling',
        next: 'nextSibling'
    });

    /**
     * This method returns, or creates on demand the global floatParent element into which top
     * level floated components are inserted.
     *
     * @return {Ext.Element} The global floatRoot element.
     * @member Ext
     * @method getFloatRoot
     * @private
     */
    Ext.getFloatRoot = function() {
        var fp = Ext.floatRoot,
            viewport = Ext['Viewport'], // Hide from Cmd dependency checking
            prototype = Widget.prototype,
            range = Widget.range;

        if (fp) {
            // Always ensure it's on top so that floateds are above inline components
            if (fp.el.dom.nextSibling) {
                // If the floatRoot contains focus, move following elements
                // to before it by grabbing the whole bunch with a Range.
                if (fp.el.contains(document.activeElement)) {
                    range.setStartBefore(fp.el.dom.nextSibling);
                    range.setEndAfter(fp.el.dom.parentNode.lastChild);
                    fp.el.dom.parentNode.insertBefore(range.extractContents(), fp.el.dom);
                } else {
                    fp.el.dom.parentNode.appendChild(fp.dom);
                }
            }
        } else {
            fp = Ext.getBody().createChild({
                cls: prototype.floatWrapCls + ' ' + prototype.rootCls,
                id: 'ext-global-floatWrap',
                "data-sticky": true
            });

            //<debug>
            fp.$skipResourceCheck = true;
            //</debug>
            
            Ext.floatRoot = fp;
        }
        return fp;
    };
});
