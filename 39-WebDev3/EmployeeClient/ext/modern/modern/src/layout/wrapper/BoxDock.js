/**
 *
 */
Ext.define('Ext.layout.wrapper.BoxDock', {
    config: {
        direction: 'horizontal',
        element: {
            className: Ext.baseCSSPrefix + 'dock'
        },
        innerWrapper: null,
        container: null,
        manageBorders: null
    },

    positionMap: {
        top: 'start',
        left: 'start',
        bottom: 'end',
        right: 'end'
    },

    managedBordersCls: Ext.baseCSSPrefix + 'managed-borders',

    constructor: function(config) {
        this.items = {
            start: [],
            end: []
        };

        this.itemsCount = 0;

        this.initConfig(config);
    },

    addItems: function(items) {
        var i, ln, item;

        for (i = 0, ln = items.length; i < ln; i++) {
            item = items[i];
            this.addItem(item);
        }
    },

    addItem: function(item) {
        var docked = item.getDocked(),
            position = this.positionMap[docked],
            wrapper = item.$dockWrapper,
            container = this.getContainer(),
            index = container.indexOf(item),
            element = item.element,
            items = this.items,
            sideItems = items[position],
            i, ln, sibling, referenceElement, siblingIndex;

        if (wrapper) {
            wrapper.removeItem(item);
        }

        item.$dockWrapper = this;
        item.addCls(Ext.baseCSSPrefix + 'dock-item');
        item.addCls(Ext.baseCSSPrefix + 'docked-' + docked);

        for (i = 0, ln = sideItems.length; i < ln; i++) {
            sibling = sideItems[i];
            siblingIndex = container.indexOf(sibling);

            if (siblingIndex > index) {
                referenceElement = sibling.element;
                sideItems.splice(i, 0, item);
                break;
            }
        }

        if (!referenceElement) {
            sideItems.push(item);
            referenceElement = this.getInnerWrapper().getElement();
        }

        this.itemsCount++;

        if (position === 'start') {
            element.insertBefore(referenceElement);
        }
        else {
            element.insertAfter(referenceElement);
        }
    },

    removeItem: function(item, oldDocked) {
        var me = this,
            position = oldDocked || item.getDocked(),
            items = me.items[me.positionMap[position]];

        Ext.Array.remove(items, item);
        item.element.detach();
        delete item.$dockWrapper;
        item.removeCls(Ext.baseCSSPrefix + 'dock-item');
        item.removeCls(Ext.baseCSSPrefix + 'docked-' + position);

        if (--me.itemsCount === 0) {
            me.destroy();
        }
    },

    getItemsSlice: function(index) {
        var container = this.getContainer(),
            items = this.items,
            slice = [],
            sideItems, i, ln, item;

        for (sideItems = items.start, i = 0, ln = sideItems.length; i < ln; i++) {
            item = sideItems[i];
            if (container.indexOf(item) > index) {
                slice.push(item);
            }
        }

        for (sideItems = items.end, i = 0, ln = sideItems.length; i < ln; i++) {
            item = sideItems[i];
            if (container.indexOf(item) > index) {
                slice.push(item);
            }
        }

        return slice;
    },

    applyElement: function(element) {
        return Ext.Element.create(element);
    },

    updateElement: function(element) {
        element.addCls(Ext.baseCSSPrefix + 'dock-' + this.getDirection());
    },

    updateInnerWrapper: function(innerWrapper, oldInnerWrapper) {
        if (oldInnerWrapper) {
            innerWrapper.getElement().replace(oldInnerWrapper.getElement(), false);
            oldInnerWrapper.$outerWrapper = null;
        } else {
            this.getElement().append(innerWrapper.getElement());
        }

        innerWrapper.setManageBorders(this.getManageBorders());
        innerWrapper.$outerWrapper = this;
    },

    updateManageBorders: function(manageBorders) {
        var me = this,
            innerWrapper = me.getInnerWrapper();

        me.getElement().toggleCls(me.managedBordersCls, manageBorders);

        if (innerWrapper) {
            innerWrapper.setManageBorders(manageBorders);
        }
    },

    destroy: function() {
        var me = this,
            innerWrapper = me.getInnerWrapper(),
            outerWrapper = me.$outerWrapper,
            innerWrapperElement;

        if (innerWrapper) {
            if (outerWrapper) {
                outerWrapper.setInnerWrapper(innerWrapper);
            } else {
                innerWrapperElement = innerWrapper.getElement();
                if (!innerWrapperElement.destroyed) {
                    innerWrapperElement.replace(me.getElement());
                }
                delete innerWrapper.$outerWrapper;
            }
        }

        delete me.$outerWrapper;

        me.unlink(['_element']);

        me.callParent();
    }
});
