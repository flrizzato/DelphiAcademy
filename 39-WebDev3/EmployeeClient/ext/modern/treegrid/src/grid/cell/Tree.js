/**
 *
 */
Ext.define('Ext.grid.cell.Tree', {
    extend: 'Ext.grid.cell.Cell',
    xtype: 'treecell',
    
    isTreeCell: true,

    /**
     * @property classCls
     * @inheritdoc
     */
    classCls: Ext.baseCSSPrefix + 'treecell',

    collapsedCls: Ext.baseCSSPrefix + 'collapsed',
    expandedCls: Ext.baseCSSPrefix + 'expanded',
    leafCls: Ext.baseCSSPrefix + 'leaf',
    expandableCls: Ext.baseCSSPrefix + 'expandable',
    withIconCls: Ext.baseCSSPrefix + 'with-icon',
    withoutIconCls: Ext.baseCSSPrefix + 'no-icon',
    loadingCls: Ext.baseCSSPrefix + 'loading',
    selectedCls: Ext.baseCSSPrefix + 'selected',

    config: {
        /**
         * @cfg {String} iconClsProperty
         * The property from the associated record to map for the {@link #iconCls} config.
         */
        iconClsProperty: 'iconCls',

        /**
         * @cfg iconCls
         * @inheritdoc Ext.panel.Header#cfg-iconCls
         * @localdoc **Note:** This value is taken from the underlying {@link #node}.
         */
        iconCls: null,

        indent: null,

        /**
         * @cfg {String} text
         * The text for this item. This value is taken from
         * the underlying {@link #node}.
         */
        text: {
            lazy: true,
            $value: ''
        }
    },

    // See theme-base/src/grid/cell/Tree.scss when maintaining this structure.
    // Ancestor classes on containing elements are used to style elements in this structure.
    // This involves nested child selectors which rely on this structure.
    /**
     * @property element
     * @inheritdoc
     */
    element: {
        reference: 'element',
        children: [{
            reference: 'innerElement',
            cls: Ext.baseCSSPrefix + 'inner-el',
            children: [{
                reference: 'indentElement',
                cls: Ext.baseCSSPrefix + 'indent-el'
            }, {
                reference: 'expanderElement',
                cls: Ext.baseCSSPrefix + 'expander-el ' +
                Ext.baseCSSPrefix + 'font-icon'
            }, {
                reference: 'iconElement',
                cls: Ext.baseCSSPrefix + 'icon-el ' +
                Ext.baseCSSPrefix + 'font-icon'
            }, {
                reference: 'bodyElement',
                cls: Ext.baseCSSPrefix + 'body-el',
                uiCls: 'body-el'
            }]
        }]
    },

    /**
     * @cfg toolDefaults
     * @inheritdoc
     */
    toolDefaults: {
        zone: 'tail'
    },

    constructor: function (config) {
        this.callParent([ config ]);

        this.element.on({
            scope: this,
            tap: 'maybeToggle'
        });
    },

    /**
     * Expand this tree node if collapse, collapse it if expanded.
     */
    toggle: function() {
        var me = this,
            record = me.getRecord();

        if (record.isExpanded()) {
            me.collapse();
        } else if (record.isExpandable()) {
            me.expand();
        }
    },

    /**
     * Collapse this tree node.
     */
    collapse: function() {
        var me = this,
            record = me.getRecord();

        me.getGrid().fireEventedAction('nodecollapse', [me.parent, record, 'collapse'], 'doToggle', this);
    },

    /**
     * Expand this tree node.
     */
    expand: function() {
        var me = this,
            record = me.getRecord(),
            tree = me.getGrid(),
            siblings, i, len, sibling;

        tree.fireEventedAction('nodeexpand', [me.parent, record, 'expand'], 'doToggle', me);

        // Collapse any other expanded sibling if tree is singleExpand
        if (record.isExpanded && !record.isRoot() && tree.getSingleExpand()) {
            siblings = record.parentNode.childNodes;
            for (i = 0, len = siblings.length; i < len; ++i) {
                sibling = siblings[i];
                if (sibling !== record) {
                    sibling.collapse();
                }
            }
        }
    },

    refresh: function (ctx) {
        this.callParent([ctx]);

        var record = this.getRecord();
        if (record) {
            this.doNodeUpdate(record);
        }
    },

    updateIconCls: function (iconCls, oldIconCls) {
        var me = this,
            el = me.element,
            noIcon = !iconCls;

        me.iconElement.replaceCls(oldIconCls, iconCls);

        el.toggleCls(me.withIconCls, !noIcon);
        el.toggleCls(me.withoutIconCls, noIcon);
    },

    updateUi: function (ui, oldUi) {
        this.callParent([ui, oldUi]);

        // ensure indent is measured from the dom when syncIndent() is called
        this._indent = null;

        this.syncIndent();
    },

    privates: {
        /**
         * Update properties after a record update.
         *
         * @param {Ext.data.TreeModel} record The node.
         *
         * @private
         */
        doNodeUpdate: function (record) {
            var me = this,
                iconClsProperty = me.getIconClsProperty(),
                el = me.element;

            if (iconClsProperty) {
                me.setIconCls(record.data[iconClsProperty]);
            }

            el.toggleCls(me.loadingCls, record.data.loading);
            el.toggleCls(me.leafCls, record.isLeaf());
            me.syncExpandCls();
            me.syncIndent();
        },

        getGrid: function() {
            return this.row.grid;
        },

        syncExpandCls: function () {
            if (!this.updatingExpandCls) {
                var me = this,
                    record = me.getRecord(),
                    expandable = record.isExpandable(),
                    element = me.element,
                    expanded = record.isExpanded(),
                    expandedCls = me.expandedCls,
                    collapsedCls = me.collapsedCls;

                me.updatingExpandCls = true;

                element.toggleCls(me.expandableCls, expandable);

                if (expandable) {
                    element.toggleCls(expandedCls, expanded);
                    element.toggleCls(collapsedCls, !expanded);
                } else {
                    element.removeCls([expandedCls, collapsedCls]);
                }

                me.updatingExpandCls = false;
            }
        },

        syncIndent: function() {
            var me = this,
                column = me.getColumn(),
                indentSize, record, depth;

            if (column) {
                indentSize = column._indentSize;
                record = me.getRecord();

                if (!indentSize) {
                    column._indentSize = indentSize = parseInt(me.el.getStyle('background-position'), 10);
                }

                if (record) {
                    depth = record.getTreeStore().rootVisible ? record.data.depth : record.data.depth - 1;
                    me.indentElement.dom.style.width = (depth * indentSize)  + 'px';
                }
            }
        },

        /**
         * @private
         */
        maybeToggle: function(e) {
            var me = this,
                record = me.getRecord(),
                wasExpanded = record.isExpanded();

            if (!record.isLeaf() && (!me.getGrid().getExpanderOnly() || e.target === me.expanderElement.dom)) {
                me.toggle();
            }

            // Toggling click does not continue to bubble the event to the view.
            // TODO: When NavigationModel implemented, that still has to recieve the events.
            if (record.isExpanded() !== wasExpanded) {
                e.nodeToggled = true;
                e.stopEvent();
            }
        },

        doToggle: function(row, record, fn) {
            record[fn]();
        }
    }
});
