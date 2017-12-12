/**
 *
 */
Ext.define('Ext.layout.Auto', {
    alias: ['layout.default', 'layout.auto'],
    alternateClassName: 'Ext.layout.Default',

    requires: [
        // Require container here so any component styling is included before
        // any layout styling so layouts take precedence
        'Ext.container.Container',
        'Ext.util.Wrapper',
        'Ext.layout.wrapper.BoxDock',
        'Ext.layout.wrapper.Inner'
    ],

    mixins: [
        'Ext.mixin.Observable',
        'Ext.mixin.Factoryable'
    ],

    factoryConfig: {
        type: 'layout',
        defaultType: 'auto',
        instanceProp: 'isLayout'
    },

    isLayout: true,

    config: {
        /**
         * @cfg {Ext.layout.card.fx.Abstract} animation Layout animation configuration
         * Controls how layout transitions are animated.  Currently only available for
         * Card Layouts.
         *
         * Possible values are:
         *
         * - cover
         * - cube
         * - fade
         * - flip
         * - pop
         * - reveal
         * - scroll
         * - slide
         * @accessor
         */
        animation: null,

        /**
         * @cfg {Ext.Container} container The container that this layout manages
         */
        container: null
    },

    centerCls: Ext.baseCSSPrefix + 'center',

    cls: Ext.baseCSSPrefix + 'layout-auto',

    itemCls: Ext.baseCSSPrefix + 'layout-auto-item',

    spaceRe: /\s+/,

    positionMap: {
        top: 'start',
        left: 'start',
        middle: 'center',
        bottom: 'end',
        right: 'end'
    },

    positionDirectionMap: {
        top: 'vertical',
        bottom: 'vertical',
        left: 'horizontal',
        right: 'horizontal'
    },

    constructor: function(config) {
        this.mixins.observable.constructor.call(this, config);
    },

    updateContainer: function(container,  oldContainer) {
        var me = this;

        me.dockedItems = [];

        container.getRenderTarget().addCls(me.cls);

        if (container.initialized) {
            me.onContainerInitialized();
        } else {
            container.onInitialized('onContainerInitialized', me);
        }
    },

    onContainerInitialized: function() {
        var me = this;

        me.handleDockedItemBorders();

        me.getContainer().on({
            delegate: '> component',

            beforecenteredchange: 'onItemCenteredChange',
            positionedchange: 'onItemPositionedChange',
            afterdockedchange: 'onAfterItemDockedChange', // see Component#updateDocked

            scope: me
        });
    },

    onItemAdd: function(item) {
        var me = this,
            container = me.getContainer(),
            floated = item.getFloated();

        if (item.getDocked() != null) {
            me.dockItem(item);
        } else if (item.isCentered()) {
            me.onItemCenteredChange(item, true);
        } else if (item.isPositioned()) {
            me.onItemPositionedChange(item, true);
        } else if (!floated) {
            me.onItemInnerStateChange(item, true);
        }

        if (container.rendered && !floated) {
            if (item.isInnerItem()) {
                me.renderInnerItem(item, true);
            } else {
                item.setRendered(true, true);
            }
        }
    },

    /**
     * @param {Ext.Component} item
     * @param {Boolean} isInner
     * @param {Boolean} [destroying]
     */
    onItemInnerStateChange: function(item, isInner, destroying) {
        var itemCls = this.itemCls;

        if (isInner) {
            this.insertInnerItem(item, this.getContainer().innerIndexOf(item));
            item.addCls(itemCls);
        } else {
            this.removeInnerItem(item);
            item.removeCls(itemCls);
        }
    },

    insertInnerItem: function(item, index) {
        var itemDom = item.element.dom,
            container = this.getContainer(),
            renderTarget = container.getRenderTarget(item),
            nextSibling = null;

        if (index !== -1) {
            if (renderTarget === container.getRenderTarget()) {
                nextSibling = container.getInnerAt(index + 1);
                nextSibling = nextSibling ? nextSibling.element.dom : null;
            } else {
                nextSibling = renderTarget.dom.childNodes[index];
            }
        }

        renderTarget.dom.insertBefore(itemDom, nextSibling);
    },

    insertPositionedItem: function(item) {
        var me = this,
            renderTarget = me.getContainer().getPositionedItemTarget(item).dom;

        if (item.getZIndex() === null) {
            item.setZIndex((me.getContainer().indexOf(item) + 1) * 2);
        }

        renderTarget.insertBefore(item.element.dom, renderTarget.firstChild);

        return me;
    },

    removeInnerItem: function(item) {
        item.element.detach();
    },

    removePositionedItem: function(item) {
        item.setZIndex(null);
        item.element.detach();
    },

    onItemRemove: function(item, index, destroying) {
        var me = this;

        if (item.getDocked()) {
            me.undockItem(item);
        } else if (item.isCentered()) {
            me.onItemCenteredChange(item, false);
        } else if (item.isPositioned()) {
            me.onItemPositionedChange(item, false);
        } else if (!item.getFloated()) {
            me.onItemInnerStateChange(item, false, destroying);
        }
    },

    onItemMove: function(item, toIndex, fromIndex) {
        if (item.isCentered() || item.isPositioned()) {
            item.setZIndex((toIndex + 1) * 2);
        }
        else if (item.isInnerItem()) {
            this.insertInnerItem(item, this.getContainer().innerIndexOf(item));
        }
        else {
            this.undockItem(item);
            this.dockItem(item);
        }
    },

    onItemCenteredChange: function(item, centered) {
        var wrapperName = '$centerWrapper';

        if (item.getFloated()) {
            item.center();
        } else {
            if (centered) {
                this.insertPositionedItem(item);
                item.link(wrapperName, new Ext.util.Wrapper({
                    className: this.centerCls
                }, item.element));
            }
            else {
                item.unlink([wrapperName]);
                this.removePositionedItem(item);
            }
        }
    },

    onItemPositionedChange: function(item, positioned) {
        if (positioned) {
            this.insertPositionedItem(item);
        }
        else {
            this.removePositionedItem(item);
        }
    },

    onAfterItemDockedChange: function(item, docked, oldDocked) {
        // Prevent this from being called during initialization of child items, the
        // setting of docked on the component will occur before add to the container
        if (item.initialized) {
            if (oldDocked) {
                this.undockItem(item, oldDocked);
            }
            if (docked) {
                this.dockItem(item);
            }
        }
    },

    dockItem: function(item) {
        var me = this,
            BoxDock = Ext.layout.wrapper.BoxDock,
            dockedItems = me.dockedItems,
            ln = dockedItems.length,
            container = me.getContainer(),
            itemIndex = container.indexOf(item),
            positionDirectionMap = me.positionDirectionMap,
            direction = positionDirectionMap[item.getDocked()],
            dockInnerWrapper = me.dockInnerWrapper,
            needsInnerWrapper = !dockInnerWrapper,
            referenceDirection, i, dockedItem, index, previousItem, slice,
            referenceItem, referenceDocked, referenceWrapper, newWrapper, nestedWrapper, oldInnerWrapper;

        if (needsInnerWrapper) {
            dockInnerWrapper = new Ext.layout.wrapper.Inner({
                container: container
            });
        }

        if (ln === 0) {
            dockedItems.push(item);

            newWrapper = new BoxDock({
                container: container,
                direction: direction,
                manageBorders: container.manageBorders
            });

            newWrapper.getElement().replace(dockInnerWrapper.getElement(), false);
            newWrapper.setInnerWrapper(dockInnerWrapper);
            newWrapper.addItem(item);
        }
        else {
            for (i = 0; i < ln; i++) {
                dockedItem = dockedItems[i];
                index = container.indexOf(dockedItem);

                if (index > itemIndex) {
                    referenceItem = previousItem || dockedItems[0];
                    dockedItems.splice(i, 0, item);
                    break;
                }

                previousItem = dockedItem;
            }

            if (!referenceItem) {
                referenceItem = dockedItems[ln - 1];
                dockedItems.push(item);
            }

            referenceDocked = referenceItem.getDocked();
            referenceWrapper = referenceItem.$dockWrapper;
            referenceDirection = positionDirectionMap[referenceDocked];

            if (direction === referenceDirection) {
                referenceWrapper.addItem(item);
            }
            else {
                slice = referenceWrapper.getItemsSlice(itemIndex);

                newWrapper = new BoxDock({
                    container: container,
                    direction: direction
                });

                if (slice.length > 0) {
                    if (slice.length === referenceWrapper.itemsCount) {
                        nestedWrapper = referenceWrapper;
                        newWrapper.getElement().replace(nestedWrapper.getElement(), false);
                        newWrapper.setInnerWrapper(nestedWrapper);
                    }
                    else {
                        nestedWrapper = new BoxDock({
                            container: container,
                            direction: referenceDirection
                        });
                        oldInnerWrapper = referenceWrapper.getInnerWrapper();
                        newWrapper.setInnerWrapper(nestedWrapper);
                        referenceWrapper.setInnerWrapper(newWrapper);
                        nestedWrapper.setInnerWrapper(oldInnerWrapper);
                        nestedWrapper.addItems(slice);
                    }
                }
                else {
                    oldInnerWrapper = referenceWrapper.getInnerWrapper();
                    referenceWrapper.setInnerWrapper(newWrapper);
                    newWrapper.setInnerWrapper(oldInnerWrapper);
                }

                newWrapper.addItem(item);
            }
        }

        if (newWrapper) {
            me.link('dockOuterWrapper', newWrapper);
        }

        if (needsInnerWrapper) {
            me.link('dockInnerWrapper', dockInnerWrapper);
        }

        if (container.initialized) {
            me.handleDockedItemBorders();
        }
    },

    getDockWrapper: function() {
        var dockedItems = this.dockedItems;

        if (dockedItems.length > 0) {
            return dockedItems[0].$dockWrapper;
        }

        return null;
    },

    undockItem: function(item, oldDocked) {
        var me = this,
            dockedItems = me.dockedItems,
            lastBorderMask, lastBorderCollapse,
            dockWrapper = item.$dockWrapper;

        if (dockWrapper) {
            dockWrapper.removeItem(item, oldDocked);
        }

        if (me.getContainer().initialized) {
            lastBorderMask = item.lastBorderMask;
            lastBorderCollapse = item.lastBorderCollapse;

            if (lastBorderMask) {
                item.lastBorderMask = 0;
                item.removeCls(me.noBorderClassTable[lastBorderMask]);
            }
            if (lastBorderCollapse) {
                item.lastBorderCollapse = 0;
                item.removeCls(me.getBorderCollapseTable()[lastBorderCollapse]);
            }

            me.handleDockedItemBorders();
        }

        Ext.Array.remove(dockedItems, item);
    },

    destroy: function() {
        this.dockedItems = null;

        Ext.destroy(this.getAnimation());

        this.callParent();
    },

    /**
     * This table contains the border removal classes indexed by the sum of the edges to
     * remove. Each edge is assigned a value:
     *
     *  * `left` = 1
     *  * `bottom` = 2
     *  * `right` = 4
     *  * `top` = 8
     *
     * @private
     */
    noBorderClassTable: [
        0,                                      // TRBL
        Ext.baseCSSPrefix + 'noborder-l',       // 0001 = 1
        Ext.baseCSSPrefix + 'noborder-b',       // 0010 = 2
        Ext.baseCSSPrefix + 'noborder-bl',      // 0011 = 3
        Ext.baseCSSPrefix + 'noborder-r',       // 0100 = 4
        Ext.baseCSSPrefix + 'noborder-rl',      // 0101 = 5
        Ext.baseCSSPrefix + 'noborder-rb',      // 0110 = 6
        Ext.baseCSSPrefix + 'noborder-rbl',     // 0111 = 7
        Ext.baseCSSPrefix + 'noborder-t',       // 1000 = 8
        Ext.baseCSSPrefix + 'noborder-tl',      // 1001 = 9
        Ext.baseCSSPrefix + 'noborder-tb',      // 1010 = 10
        Ext.baseCSSPrefix + 'noborder-tbl',     // 1011 = 11
        Ext.baseCSSPrefix + 'noborder-tr',      // 1100 = 12
        Ext.baseCSSPrefix + 'noborder-trl',     // 1101 = 13
        Ext.baseCSSPrefix + 'noborder-trb',     // 1110 = 14
        Ext.baseCSSPrefix + 'noborder-trbl'     // 1111 = 15
    ],

    /**
     * The numeric values assigned to each edge indexed by the `dock` config value.
     * @private
     */
    edgeMasks: {
        top: 8,
        right: 4,
        bottom: 2,
        left: 1
    },

    handleDockedItemBorders: function(force) {
        var me = this,
            edges = 0,
            maskT = 8,
            maskR = 4,
            maskB = 2,
            maskL = 1,
            container = me.getContainer(),
            bodyBorder = container.getBodyBorder && container.getBodyBorder(),
            containerBorder = container.getBorder(),
            collapsed = me.collapsed,
            edgeMasks = me.edgeMasks,
            noBorderCls = me.noBorderClassTable,
            dockedItemsGen = container.items.generation,
            bodyClsEl = container.boxScrollerElement || container.bodyElement,
            b, borderCls, docked, edgesTouched, i, ln, item, dock, lastValue, mask,
            addCls, removeCls, header;

        if ((!force && (me.initializedBorders === dockedItemsGen)) || !container.manageBorders) {
            return;
        }

        addCls = [];
        removeCls = [];

        borderCls   = me.getBorderCollapseTable();
        noBorderCls = me.getBorderClassTable ? me.getBorderClassTable() : noBorderCls;

        me.initializedBorders = dockedItemsGen;

        // Borders have to be calculated using expanded docked item collection.
        me.collapsed = false;
        docked = container.getDockedItems();
        me.collapsed = collapsed;
        header = container.getHeader && container.getHeader();

        if (header) {
            docked = ([header]).concat(docked);
        }

        for (i = 0, ln = docked.length; i < ln; i++) {
            item = docked[i];

            if (item.getHidden()) {
                continue;
            }

            dock = item.isPanelHeader ? item.getPosition() : item.getDocked();
            mask = edgesTouched = 0;
            addCls.length = 0;
            removeCls.length = 0;

            if (dock !== 'bottom') {
                if (edges & maskT) { // if (not touching the top edge)
                    b = item.border;
                } else {
                    b = containerBorder;
                    if (b !== false) {
                        edgesTouched += maskT;
                    }
                }
                if (b === false) {
                    mask += maskT;
                }
            }
            if (dock !== 'left') {
                if (edges & maskR) { // if (not touching the right edge)
                    b = item.border;
                } else {
                    b = containerBorder;
                    if (b !== false) {
                        edgesTouched += maskR;
                    }
                }
                if (b === false) {
                    mask += maskR;
                }
            }
            if (dock !== 'top') {
                if (edges & maskB) { // if (not touching the bottom edge)
                    b = item.border;
                } else {
                    b = containerBorder;
                    if (b !== false) {
                        edgesTouched += maskB;
                    }
                }
                if (b === false) {
                    mask += maskB;
                }
            }
            if (dock !== 'right') {
                if (edges & maskL) { // if (not touching the left edge)
                    b = item.border;
                } else {
                    b = containerBorder;
                    if (b !== false) {
                        edgesTouched += maskL;
                    }
                }
                if (b === false) {
                    mask += maskL;
                }
            }

            if ((lastValue = item.lastBorderMask) !== mask) {
                item.lastBorderMask = mask;
                if (lastValue) {
                    removeCls[0] = noBorderCls[lastValue];
                }
                if (mask) {
                    addCls[0] = noBorderCls[mask];
                }
            }

            if ((lastValue = item.lastBorderCollapse) !== edgesTouched) {
                item.lastBorderCollapse = edgesTouched;
                if (lastValue) {
                    removeCls.push.apply(removeCls, borderCls[lastValue]);
                }
                if (edgesTouched) {
                    addCls.push.apply(addCls, borderCls[edgesTouched]);
                }
            }

            if (removeCls.length) {
                item.removeCls(removeCls);
            }
            if (addCls.length) {
                item.addCls(addCls);
            }

            // mask can use += but edges must use |= because there can be multiple items
            // on an edge but the mask is reset per item

            edges |= edgeMasks[dock]; // = T, R, B or L (8, 4, 2 or 1)
        }

        mask = edgesTouched = 0;
        addCls.length = 0;
        removeCls.length = 0;

        if (edges & maskT) { // if (not touching the top edge)
            b = bodyBorder;
        } else {
            b = containerBorder;
            if (b !== false) {
                edgesTouched += maskT;
            }
        }
        if (b === false) {
            mask += maskT;
        }

        if (edges & maskR) { // if (not touching the right edge)
            b = bodyBorder;
        } else {
            b = containerBorder;
            if (b !== false) {
                edgesTouched += maskR;
            }
        }
        if (b === false) {
            mask += maskR;
        }

        if (edges & maskB) { // if (not touching the bottom edge)
            b = bodyBorder;
        } else {
            b = containerBorder;
            if (b !== false) {
                edgesTouched += maskB;
            }
        }
        if (b === false) {
            mask += maskB;
        }

        if (edges & maskL) { // if (not touching the left edge)
            b = bodyBorder;
        } else {
            b = containerBorder;
            if (b !== false) {
                edgesTouched += maskL;
            }
        }
        if (b === false) {
            mask += maskL;
        }

        if ((lastValue = me.lastBodyBorderMask) !== mask) {
            me.lastBodyBorderMask = mask;
            if (lastValue) {
                removeCls[0] = noBorderCls[lastValue];
            }
            if (mask) {
                addCls[0] = noBorderCls[mask];
            }
        }

        if ((lastValue = me.lastBodyBorderCollapse) !== edgesTouched) {
            me.lastBodyBorderCollapse = edgesTouched;
            if (lastValue) {
                removeCls.push.apply(removeCls, borderCls[lastValue]);
            }
            if (edgesTouched) {
                addCls.push.apply(addCls, borderCls[edgesTouched]);
            }
        }

        if (removeCls.length) {
            bodyClsEl.removeCls(removeCls);
        }
        if (addCls.length) {
            bodyClsEl.addCls(addCls);
        }
    },

    /**
     * This object is indexed by a component's `classCls` to yield another object which
     * is then indexed by the component's `ui` to produce an array of CSS class names.
     * This array is indexed in the same manner as the `noBorderClassTable` and indicates
     * the a particular edge of a docked item or the body element is actually "collapsed"
     * with the component's outer border.
     * @private
     */
    borderCollapseMap: {
        /*
         'x-panel': {
         'default': []
         }
         */
    },

    /**
     * Returns the array of class names to add to a docked item or body element when for
     * the edges that should collapse with the outer component border. Basically, the
     * panel's outer border must look visually like a contiguous border but may need to
     * be realized by using the border of docked items and/or the body. This class name
     * allows the border color and width to be controlled accordingly and distinctly from
     * the border of the docked item or body element when it is not having its border
     * collapsed.
     * @private
     */
    getBorderCollapseTable: function () {
        var me = this,
            map = me.borderCollapseMap,
            container = me.getContainer(),
            classCls = container.classCls,
            ui = container.getUi(),
            uiKey = ui || 'default',
            uiList, table, classClsList, baseCls, uiCls, i, ln, j, uiLen;

        map = map[classCls] || (map[classCls] = {});
        table = map[uiKey];

        if (!table) {
            classClsList = container.classClsList;
            map[uiKey] = table = [[],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]];

            uiList = [0];

            if (ui) {
                uiList = uiList.concat(ui.split(me.spaceRe));
            }

            uiLen = uiList.length;

            for (i = 0, ln = classClsList.length; i < ln; i++) {
                classCls = classClsList[i];

                for (j = 0; j < uiLen; j++) {
                    ui = uiList[j];
                    uiCls = (ui ? ('-' + ui) : '');
                    baseCls = classCls + uiCls + '-outer-border-';

                    table[1].push(baseCls + 'l');       // 0001 = 1
                    table[2].push(baseCls + 'b');       // 0010 = 2
                    table[3].push(baseCls + 'bl');      // 0011 = 3
                    table[4].push(baseCls + 'r');       // 0100 = 4
                    table[5].push(baseCls + 'rl');      // 0101 = 5
                    table[6].push(baseCls + 'rb');      // 0110 = 6
                    table[7].push(baseCls + 'rbl');     // 0111 = 7
                    table[8].push(baseCls + 't');       // 1000 = 8
                    table[9].push(baseCls + 'tl');      // 1001 = 9
                    table[10].push(baseCls + 'tb');    // 1010 = 10
                    table[11].push(baseCls + 'tbl');   // 1011 = 11
                    table[12].push(baseCls + 'tr');    // 1100 = 12
                    table[13].push(baseCls + 'trl');   // 1101 = 13
                    table[14].push(baseCls + 'trb');   // 1110 = 14
                    table[15].push(baseCls + 'trbl');  // 1111 = 15
                }
            }
        }

        return table;
    },

    setConfig: function (name, value, options) {
        var config = name,
            alias = this.alias,
            type = config.type;

        if (name) {
            if (typeof name === 'string') {
                config = {};
                config[name] = value;
            }
            else {
                options = value;
            }

            if (!type || (alias && alias.indexOf('layout.' + type) > -1)) {
                this.callParent([ config, options ]);
            }
            //<debug>
            else {
                Ext.raise('Cannot change layout from '+this.$className+' to "'+type+'"');
            }
            //</debug>
        }

        return this;
    },

    privates: {
        renderInnerItem: function(item, asRoot) {
            item.setRendered(true, asRoot);
        }
    }
});
