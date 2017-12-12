/**
 * @class Ext.list.TreeItem
 */

Ext.define('Ext.overrides.list.TreeItem', {
    override: 'Ext.list.TreeItem',

    runAnimation: function(animation) {
        return this.itemContainer.animate(animation);
    },

    stopAnimation: function(animation) {
        animation.end();
    },

    refreshInnerState: Ext.emptyFn,

    applyFloated: function (floated, wasFloated) {
        this.initialized = true;
        this.callParent([floated, wasFloated]);
        return floated;
    },

    updateFloated: function (floated, wasFloated) {
        var me = this,
            ownerTree,
            toolElement = me.getToolElement(),
            node, wasExpanded;

        if (floated) {
            me.wasExpanded = me.getExpanded();
            me.nextElementSibling = me.el.dom.nextSibling;
            me.setExpanded(true);
        } else {
            wasExpanded = me.wasExpanded;
            node = me.getNode();
            me.setExpanded(me.wasExpanded);
            if (!wasExpanded && node.isExpanded()) {
                me.preventAnimation = true;
                node.collapse();
                me.preventAnimation = false;
            }
        }
        me.callParent([floated, wasFloated]);
        if (floated) {
            // Add the necessary CSS classes for the theming to apply to the item.
            ownerTree = me.getOwner();
            me.floatWrap.addCls([
                Ext.baseCSSPrefix + 'treelist',
                ownerTree.uiPrefix + ownerTree.getUi(),
                Ext.baseCSSPrefix + 'treelist-float-wrap'
            ]);
            me.floatWrap.alignTo(toolElement, 'tl-tr');
            me.floatWrap.on({
                click: ownerTree.onClick,
                mouseover: ownerTree.onMouseOver,
                scope: ownerTree
            });
        } else {
            // Reinsert this el back into the tree
            me.getOwner().rootItem.el.dom.insertBefore(me.el.dom, me.nextElementSibling);
        }
        toolElement.toggleCls(me.floatedToolCls, floated);
    }
});
