/**
 * The TreeGrid provides a tree-structured UI representation of tree-structured data.
 * TreeGrids must be bound to a {@link Ext.data.TreeStore}.
 *
 * TreeGrid supports multiple columns through the {@link #columns} configuration.
 *
 * By default a TreeGrid contains a single column that uses the `text` field of
 * the store's nodes.
 *
 * Here is a simple TreeGrid using inline data:
 *
 *     @example
 *     var ts = Ext.create('Ext.data.TreeStore', {
 *         root: {
 *             text: 'Genre',
 *             expanded: true,
 *             children: [
 *                 {
 *                     text: 'Comedy',
 *                     children: [
 *                         { leaf: true, text: '30 Rock' },
 *                         { leaf: true, text: 'Arrested Development' },
 *                         { leaf: true, text: 'Bob\'s Burgers' },
 *                         { leaf: true, text: 'Curb your Enthusiasm' },
 *                         { leaf: true, text: 'Futurama' }
 *                     ]
 *                 },
 *                 {
 *                     text: 'Drama',
 *                     children: [
 *                         { leaf: true, text: 'Breaking Bad', },
 *                         { leaf: true, text: 'Game of Thrones' },
 *                         { leaf: true, text: 'Lost' },
 *                         { leaf: true, text: 'Preacher' },
 *                         { leaf: true, text: 'The Wire' }
 *                     ]
 *                 },
 *                 {
 *                     text: 'Science Fiction',
 *                     children: [
 *                         { leaf: true, text: 'Black Mirror' },
 *                         { leaf: true, text: 'Doctor Who' },
 *                         { leaf: true, text: 'Eureka' },
 *                         { leaf: true, text: 'Futurama' },
 *                         { leaf: true, text: 'The Twilight Zone' },
 *                         { leaf: true, text: 'X-Files' }
 *                     ]
 *                 }
 *             ]
 *         }
 *     });
 *
 *     Ext.create({
 *         fullscreen: true,
 *         xtype: 'panel',
 *
 *         items: [{
 *             xtype: 'tree',
 *             height: 600,
 *             width: 400,
 *             store: ts,
 *             title: 'Favorite Shows by Genre'
 *         }]
 *     });
 */
Ext.define('Ext.grid.Tree', {
    extend: 'Ext.grid.Grid',
    xtype: 'tree',
    alternateClassName: 'Ext.tree.Tree',

    classCls: Ext.baseCSSPrefix + 'tree',
    expanderLastCls: Ext.baseCSSPrefix + 'expander-last',
    expanderFirstCls: Ext.baseCSSPrefix + 'expander-first',
    expanderOnlyCls: Ext.baseCSSPrefix + 'expander-only',
    cellExpanderCls: Ext.baseCSSPrefix + 'cell-expander',

    /**
     * @event beforenodeexpand
     * Fires before an row is visually expanded. May be vetoed by returning false from a handler.
     * @param {Ext.grid.Row} row                    The row to be expanded
     * @param {Ext.data.NodeInterface} record       The record to be expanded
     */

    /**
     * @event nodeexpand
     * Fires after an row has been visually expanded and its child nodes are visible in the tree.
     * @param {Ext.grid.Row} row                    The row that was expanded
     * @param {Ext.data.NodeInterface} record       The record that was expanded
     */

    /**
     * @event beforenodecollapse
     * Fires before an row is visually collapsed. May be vetoed by returning false from a handler.
     * @param {Ext.grid.Row} node                   The row to be collapsed
     * @param {Ext.data.NodeInterface} record       The record to be collapsed
     */

    /**
     * @event nodecollapse
     * Fires after an row has been visually collapsed and its child nodes are no longer visible in the tree.
     * @param {Ext.grid.Row} node                   The row that was collapsed
     * @param {Ext.data.NodeInterface} record       The record that was collapsed
     */

    cachedConfig: {

        /**
         * @cfg {Boolean} expanderFirst
         * `true` to display the expander to the left of the item text.  
         * `false` to display the expander to the right of the item text.
         */
        expanderFirst: true,

        /**
         * @cfg {Boolean} expanderOnly
         * `true` to expand only on the click of the expander element. Setting this to
         * `false` will allow expansion on click of any part of the element.
         */
        expanderOnly: true
    },

    config: {
        root: {},

        /**
         * @cfg {Boolean} selectOnExpander
         * `true` to select the node when clicking the expander.
         */
        selectOnExpander: false,

        /**
         * @cfg {Boolean} [singleExpand]
         * `true` if only 1 node per branch may be expanded.
         */
        singleExpand: false,

        rootVisible: true,

        displayField: 'text',

        columns: false, // Non-null to force running the applier.

        rowLines: false,
        
        /**
         * @cfg {Boolean} [folderSort=false]
         * True to automatically prepend a leaf sorter to the store.
         */
        folderSort: false
    },

    eventsSelector: '.' + Ext.baseCSSPrefix + 'grid-cell',

    applyColumns: function(columns) {
        if (!columns) {
            this.setHideHeaders(true);
            columns = [{
                xtype: 'treecolumn',
                text: 'Name',
                dataIndex: this.getDisplayField(),
                minWidth: 100,
                flex: 1
            }];
        }
        return columns;
    },

    onRootChange: function(newRoot, oldRoot) {
        var me = this,
            fireEventArgs;

        if (oldRoot) {
            delete oldRoot.fireEventArgs;
        }
        
        // We take over from event firing so we can relay.
        // Cannot use Function.createSequence. That does not return the return values
        if (newRoot) {
            fireEventArgs = newRoot.fireEventArgs;
            newRoot.fireEventArgs = function(eventName) {
                // Fire on the original firer
                var ret = fireEventArgs.apply(newRoot, arguments);

                // If not stopped, fire through this Tree
                if (ret !== false) {
                    arguments[0] = me.rootEventsMap[eventName] || ('item' + eventName);
                    ret = me.fireEventArgs.apply(me, arguments);
                }
                return ret;
            };
        }
    },

    updateExpanderFirst: function (expanderFirst) {
        var el = this.element;

        el.toggleCls(this.expanderFirstCls, expanderFirst);
        el.toggleCls(this.expanderLastCls, !expanderFirst);
    },

    updateExpanderOnly: function (expanderOnly) {
        var el = this.element;

        el.toggleCls(this.expanderOnlyCls, expanderOnly);
        el.toggleCls(this.cellExpanderCls, !expanderOnly);
    },

    /**
     * Sets root node of this tree. All trees *always* have a root node. It may be {@link #rootVisible hidden}.
     *
     * If the passed node has not already been loaded with child nodes, and has its expanded field set, this triggers
     * the {@link #cfg-store} to load the child nodes of the root.
     * @param {Ext.data.TreeModel/Object} root
     * @return {Ext.data.TreeModel} The new root
     */
    setRootNode: function(root) {
        var store = this.getStore();

        root = store.setRoot(root);

        return root;
    },

    /**
     * Returns the root node for this tree.
     * @return {Ext.data.TreeModel}
     */
    getRootNode: function() {
        var store = this.getStore();
        return store ? store.getRoot() : null;
    },

    
    /**
     * Expands a record that is loaded in the tree.
     * @param {Ext.data.Model} record The record to expand
     * @param {Boolean} [deep] True to expand nodes all the way down the tree hierarchy.
     * @param {Function} [callback] The function to run after the expand is completed
     * @param {Object} [scope] The scope of the callback function.
     */
    expandNode: function(record, deep, callback, scope) {
        return record.expand(deep, callback, scope || this);
    },

    /**
     * Collapses a record that is loaded in the tree.
     * @param {Ext.data.Model} record The record to collapse
     * @param {Boolean} [deep] True to collapse nodes all the way up the tree hierarchy.
     * @param {Function} [callback] The function to run after the collapse is completed
     * @param {Object} [scope] The scope of the callback function.
     */
    collapseNode: function(record, deep, callback, scope) {
        return record.collapse(deep, callback, scope || this);
    },

    /**
     * Expand all nodes
     * @param {Function} [callback] A function to execute when the expand finishes.
     * @param {Object} [scope] The scope of the callback function
     */
    expandAll: function(callback, scope) {
        var me = this,
            root = me.getRootNode();

        if (root) {
            Ext.suspendLayouts();
            root.expand(true, callback, scope || me);
            Ext.resumeLayouts(true);
        }
    },

    /**
     * Collapse all nodes
     * @param {Function} [callback] A function to execute when the collapse finishes.
     * @param {Object} [scope] The scope of the callback function
     */
    collapseAll: function(callback, scope) {
        var me = this,
            root = me.getRootNode();

        if (root) {
            Ext.suspendLayouts();
            scope = scope || me;
            if (me.getStore().rootVisible) {
                root.collapse(true, callback, scope);
            } else {
                root.collapseChildren(true, callback, scope);
            }
            Ext.resumeLayouts(true);
        }
    },

    privates: {
        rootEventsMap: {
            beforeappend: 'beforeitemappend',
            beforeremove: 'beforeritememove',
            beforemove: 'beforeitemmove',
            beforeinsert: 'beforeiteminsert',
            beforeexpand: 'beforeitemexpand',
            beforecollapse: 'beforeitemcollapse'
        },

        doChildTouchStart: function(location) {
            var cell = location.cell;

            if (cell && (!cell.isTreeCell || this.getSelectOnExpander() || location.event.target !== cell.expanderElement.dom)) {
                this.callParent([location]);
            }
        },

        updateStore: function(newStore, oldStore) {
            var me = this,
                newRoot;

            // We take over from event firing so we can relay
            if (oldStore) {
                Ext.destroy(me.storeListeners, me.storeRelayers);
            }

            if (newStore) {
                me.store = newStore;

                // If there is no root node defined, then create one.
                // Ensure a first onRootChange is called so we can hook into the event firing
                if (newRoot = newStore.getRoot()) {
                    me.onRootChange(newRoot);
                } else {
                    newStore.setRoot(me.getRoot());
                    newRoot = newStore.getRoot();
                }

                // Store must have the same idea about root visibility as us before callParent binds it.
                if (!('rootVisible' in newStore.initialConfig)) {
                    newStore.setRootVisible(me.getRootVisible());
                }
                // TreeStore must have an upward link to the TreePanel so that nodes can find their owning tree in NodeInterface.getOwnerTree
                // TODO: NodeInterface.getOwnerTree is deprecated. Data class must not be coupled to UI. Remove this link
                // when that method is removed.
                newStore.ownerTree = me;

                me.callParent([newStore, oldStore]);

                newStore.folderSort = me.getFolderSort();

                // Monitor the TreeStore for the root node being changed. Return a Destroyable object
                me.storeListeners = me.mon(newStore, {
                    destroyable: true,
                    rootchange: me.onRootChange,
                    scope: me
                });

                // Relay store events. relayEvents always returns a Destroyable object.
                me.storeRelayers = me.relayEvents(newStore, [
                    /**
                     * @event beforeload
                     * @inheritdoc Ext.data.TreeStore#beforeload
                     */
                    'beforeload',

                    /**
                     * @event load
                     * @inheritdoc Ext.data.TreeStore#load
                     */
                    'load'
                ]);

                // If rootVisible is false, we *might* need to expand the node.
                // If store is autoLoad, that will already have been kicked off.
                // If its already expanded, or in the process of loading, the TreeStore
                // has started that at the end of updateRoot
                if (!newStore.rootVisible && !newStore.autoLoad && !(newRoot.isExpanded() || newRoot.isLoading())) {
                    // A hidden root must be expanded, unless it's overridden with autoLoad: false.
                    // If it's loaded, set its expanded field (silently), and skip ahead to the onNodeExpand callback.
                    if (newRoot.isLoaded()) {
                        newRoot.data.expanded = true;
                        newStore.onNodeExpand(newRoot, newRoot.childNodes);
                    }
                    // Root is not loaded; go through the expand mechanism to force a load
                    // unless we were told explicitly not to load the store by setting
                    // autoLoad: false. This is useful with Direct proxy in cases when
                    // Direct API is loaded dynamically and may not be available at the time
                    // when TreePanel is created.
                    else if (newStore.autoLoad !== false && !newStore.hasPendingLoad()) {
                        newRoot.data.expanded = false;
                        newRoot.expand();
                    }
                }
            }
        }
    }
});
