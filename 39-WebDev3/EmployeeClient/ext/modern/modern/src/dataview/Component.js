/**
 * This class is similar to `Ext.dataview.DataView` except it renders components for each
 * record instead of simple chunks of HTML. The `itemTpl` can still be used for components
 * but it is more typical to use the component's config properties
 *
 * The type of component can be controlled using the `itemConfig` and record's fields can
 * be mapped to config properties using `itemDataMap`.
 *
 *      Ext.create({
 *          xtype: 'componentdataview',
 *
 *          store: [
 *              { name: 'Peter',  age: 26 },
 *              { name: 'Ray',   age: 21 },
 *              { name: 'Egon', age: 24 },
 *              { name: 'Winston', age: 24 }
 *          ],
 *
 *          itemConfig: {
 *              xtype: 'button',
 *              cls: 'x-item-no-tap' // Prevent childtap events
 *          },
 *
 *          itemDataMap: {
 *              '#': {
 *                  text: 'name'
 *              }
 *          }
 *      });
 *
 * The `itemDataMap` is a simple and efficient means for mapping fields to configs, but
 * can only apply fields stored in the records' data to configs on the target component.
 * While this can be dynamic by using {@link Ext.data.field.Field#cfg!calculate calculated}
 * fields, more complex mappings should use {@link Ext.data.ViewModel ViewModels} and
 * {@link Ext.Component#cfg!bind data binding}.
 *
 * For example:
 *
 *      Ext.create({
 *          xtype: 'componentdataview',
 *
 *          store: [
 *              { name: 'Peter',  age: 26 },
 *              { name: 'Ray',   age: 21 },
 *              { name: 'Egon', age: 24 },
 *              { name: 'Winston', age: 24 }
 *          ],
 *
 *          itemConfig: {
 *              xtype: 'button',
 *
 *              viewModel: true, // enable per-record binding
 *
 *              bind: 'Go {record.name}!'
 *          }
 *      });
 *
 * ### Historical Note
 *
 * In previous releases, the `useComponents` config allowed any `Ext.dataview.DataView` to
 * switch to using components instead of pure HTML for items. This feature was replaced by
 * this class in version 6.5 as part of the numerous {@link Ext.dataview.List List} and
 * {@link Ext.grid.Grid Grid} additions.
 *
 * @since 6.5.0
 */
Ext.define('Ext.dataview.Component', {
    extend: 'Ext.dataview.Abstract',
    xtype: 'componentdataview',

    requires: [
        'Ext.dataview.DataItem'
    ],

    isComponentDataView: true,

    config: {
        /**
         * @cfg {String}
         * A class to add to the inner element of items.
         * @since 6.5.0
         */
        itemInnerCls: null,

        /**
         * @cfg {Object/Ext.Component} itemConfig
         * The object is used to configure the data items created by this data view. The
         * `xtype` property of this config overrides the container's `defaultType`.
         */
        itemConfig: {
            xtype: 'dataitem'
        },

        /**
         * @cfg {String} itemContentCls
         * A class to add to the element that immediate wraps the item content produced
         * by the `itemTpl` (the "inner-html" element).
         * @since 6.5.0
         */
        itemContentCls: null,

        /**
         * @cfg {Object} itemDataMap
         * This object allows you to map {@link Ext.data.Model record} fields to specific
         * configs on component items.
         *
         * The `itemDataMap` object's keys describe the target objects to receive data
         * from the associated {@link #cfg!record record}. These keys are either `'#'`
         * (for the item itself) or a {@link Ext.Component#cfg!reference reference} to
         * a component contained in the item.
         *
         * For each target listed in `itemDataMap`, the value is another map describing
         * the config name (in the key) and the data field name (as the value).
         *
         * For example:
         *
         *      itemDataMap: {
         *          '#': {
         *              title: 'fullName'
         *          },
         *          text: {
         *              html: 'name'
         *          }
         *      }
         *
         * The above is equivalent to:
         *
         *      item.setTitle(item.getRecord().get('fullName'));
         *      item.lookup('text').setHtml(item.getRecord().get('name'));
         *
         * For more complex mapping of data to item, you should use the data binding as
         * described in the class documentation.
         *
         * @since 6.5.0
         */
        itemDataMap: null,

        /**
         * @cfg {Number} maxItemCache
         * The number of components to cache when no longer needed (as opposed to calling
         * `destroy` on them).
         */
        maxItemCache: 20,

        /**
         * @cfg {Boolean} [striped=false]
         * Set this to `true` if you want the items in this DataView to be zebra striped.
         * alternating their background color.
         * Only applicable if the stylesheet provides styling for alternate items.
         *
         * By default the stylesheet does not provide styling for DataView items, but it
         * can be enabled by setting the `ui` to `'basic'`.
         *
         * Lists and Grids provide default styling for striped items
         */
        striped: null,

        // --------------------
        // Private

        itemCount: 0
    },

    /**
     * @event childtouchstart
     * Fires when a child is first touched.
     * @param {Ext.dataview.Component} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childtouchmove
     * Fires when a touch move occurs on a child.
     * @param {Ext.dataview.Component} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childtouchend
     * Fires when a touch ends on a child.
     * @param {Ext.dataview.Component} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childtouchcancel
     * Fires when a touch is cancelled.
     * @param {Ext.dataview.Component} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childtap
     * Fires when a child is tapped. Add `x-item-no-tap` CSS class to a child
     * of list item to suppress `childtap` events on that child. This can be
     * useful when items contain components such as Buttons.
     * @param {Ext.dataview.Component} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childlongpress
     * Fires when a child is long-pressed.
     * @param {Ext.dataview.Component} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childtaphold
     * Fires when a child is tap-held.
     * @param {Ext.dataview.Component} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childsingletap
     * Fires when a child is single tapped.
     * @param {Ext.dataview.Component} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childdoubletap
     * Fires when a child is double tapped.
     * @param {Ext.dataview.Component} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childmouseenter
     * Fires when the mouse pointer enters a child.
     * @param {Ext.dataview.Component} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childmouseleave
     * Fires when the mouse pointer leaves a child.
     * @param {Ext.dataview.Component} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @cfg {Ext.enums.Widget} defaultType
     * As a {@link Ext.Container container}, this config controls the default type of
     * items that are added.
     *
     * Non-data items can also be added to this container, and these will pick up this
     * default. This config will only apply to data items if `itemConfig` does not contain
     * an `xtype` property (which it does by default). This means that data items will
     * *not* be affected by this config unless an `itemConfig` is set that nulls out the
     * `xtype` (not recommended).
     */

    firstCls: Ext.baseCSSPrefix + 'first',
    lastCls: Ext.baseCSSPrefix + 'last',
    oddCls: Ext.baseCSSPrefix + 'odd',

    beforeInitialize: function (config) {
        /**
         * @property {Ext.Component[]} itemCache
         * The array of component items previously created for this view but not in
         * current use. This array will contain no more then `maxItemCache` items.
         * @private
         */
        this.itemCache = [];

        this.callParent([ config ]);
    },

    isFirstItem: function(item) {
        return item === this.getFirstItem();
    },

    isFirstDataItem: function(item) {
        return item === this.getFirstDataItem();
    },

    isLastItem: function(item) {
        return item === this.getLastItem();
    },

    isLastDataItem: function(item) {
        return item === this.getLastDataItem();
    },

    doDestroy: function() {
        // dataItems are also in this container, so they will be handled...
        Ext.destroy(this.itemCache, this.dataRange);

        this.callParent();
    },

    onRender: function() {
        var me = this,
            itemConfig = me.getItemConfig();

        // If we have a viewmodel on our items, then ensure we have a single entry point
        // to allow us to notify all of them when required
        if (itemConfig.viewModel) {
            me.hasItemVm = true;

            if (!me.lookupViewModel()) {
                me.setViewModel(true);
            }
        }

        me.callParent();
    },

    getViewItems: function() {
        return this.getInnerItems().slice();
    },

    onStoreAdd: function (store, records, index) {
        var me = this;
        
        me.callParent(arguments);

        me.setItemCount(store.getCount());
        me.syncItemRange(me.getStoreChangeSyncIndex(index));
    },

    onStoreRemove: function (store, records, index) {
        var me = this,
            len = records.length,
            dataItems = me.dataItems.splice(index, len),
            itemCount = me.getItemCount(),
            i;

        me.callParent(arguments);

        if (!dataItems.length) {
            return;
        }

        for (i = len; i-- > 0; ) {
            me.removeDataItem(dataItems[i]); // less ripple-down cost...
        }

        // The update will have nothing to do now, but the property must be updated.
        me.setItemCount(itemCount - len);
        me.syncItemRange(me.getStoreChangeSyncIndex(index));
    },

    //--------------------------------------------
    // Private Configs

    // itemInnerCls

    updateItemInnerCls: function (cls) {
        if (!this.isConfiguring) {
            var items = this.dataItems,
                len = items.length,
                i, item;

            for (i = 0; i < len; i++) {
                item = items[i];

                if (item.setInnerCls) {
                    item.setInnerCls(cls);
                }
            }
        }
    },

    // itemConfig

    applyItemConfig: function (itemConfig, oldItemConfig) {
        // If the itemConfig is being set after creation, preserve the original
        // xtype/xclass if one wasn't provided
        itemConfig = itemConfig || {};

        if (oldItemConfig && !itemConfig.xtype && !itemConfig.xclass) {
            var xtype = oldItemConfig.xtype,
                xclass = oldItemConfig.xclass;

            if (xtype || xclass) {
                itemConfig = Ext.apply({}, itemConfig);
                itemConfig[xclass ? 'xclass' : 'xtype'] = xclass || xtype;
            }
        }

        return itemConfig;
    },

    updateItemConfig: function () {
        if (!this.isConfiguring) {
            this.clearItems();
            this.refresh();
        }
    },

    // itemContentCls

    updateItemContentCls: function (cls) {
        if (!this.isConfiguring) {
            var items = this.dataItems,
                len = items.length,
                i, item;

            for (i = 0; i < len; i++) {
                item = items[i];

                if (item.setContentCls) {
                    item.setContentCls(cls);
                }
            }
        }
    },

    // itemDataMap

    applyItemDataMap: function (dataMap) {
        return Ext.dataview.DataItem.parseDataMap(dataMap);
    },

    // striped

    updateStriped: function (striped) {
        var me = this,
            dataItems = me.dataItems,
            oddCls = me.oddCls,
            i, el, odd;

        me.striped = !!striped;

        if (!me.isConfiguring) {
            for (i = 0; i < dataItems.length; ++i) {
                el = dataItems[i].el;
                odd = striped ? +el.dom.getAttribute('data-recordindex') : 0;
                el.toggleCls(oddCls, odd % 2);
            }
        }
    },

    //-----------------------------------------------------------------------

    privates: {
        dataRange: null,
        infinite: false, // to disable pieces that infinite Lists don't want
        striped: false,

        _itemChangeHandlers: [
            'changeItemRecordIndex',
            'changeItemRecord',
            'changeItemIsFirst',
            'changeItemIsLast'
        ],

        acquireItem: function (cfg, itemsFocusable) {
            var me = this,
                at = null,
                el, item;

            if (typeof cfg === 'number') {
                at = cfg;
                cfg = null;
            }

            if (!cfg) {
                cfg = me.getItemConfig();
                itemsFocusable = me.getItemsFocusable();
            }

            // Pull from the itemCache first
            if (!(item = me.itemCache.pop())) {
                // Failing that, create new ones
                item = me.createDataItem(cfg);
                item = me.addDataItem(item, at);

                el = item.element;

                // The element must accept focus for navigation to occur.
                // The item component must not be focusable. It must not participate in a
                // FocusableContainer relationship with the List's container,
                // and must not react to focus events or its focus API itself.
                // It is a slave of the NavigationModel.
                if (itemsFocusable) {
                    (item.getFocusEl() || el).setTabIndex(-1);
                }

                // Set up itemSelector attribute
                el.dom.setAttribute('data-viewid', me.id);
            }
            else {
                item.removeCls(me._cachedRemoveClasses); // just in case
                me.addDataItem(item, at);
            }

            return item;
        },

        addDataItem: function (item, at) {
            var me = this;

            if (at === null) {
                at = me.findTailItem(/*rawElements=*/false);
            }

            item = (at < 0) ? me.add(item) : me.insert(at, item);

            me.dataItems.push(item); // if this changes, check List.dislodgeItem

            return item;
        },

        /**
         * This method changes the record bound to the specified item.
         * @param {Number} itemIndex The index of the item in `dataItems`. Negative
         * numbers are used to index backwards such that `-1` is the last item.
         * @param {Number} recordIndex The record's index in the store.
         * @private
         */
        changeItem: function (itemIndex, recordIndex) {
            var me = this,
                store = me.store,
                page = store.currentPage,
                datasetIndex = recordIndex + (page ? ((page - 1) * store.pageSize) : 0),
                dataItems = me.dataItems,
                realIndex = (itemIndex < 0) ? dataItems.length + itemIndex : itemIndex,
                item = dataItems[realIndex],
                storeCount = store.getCount(),
                handlers = me._itemChangeHandlers,
                options = {
                    isFirst: !recordIndex,
                    isLast: recordIndex === storeCount -1,
                    item: item,
                    itemIndex: realIndex,
                    record: me.dataRange.records[recordIndex],
                    recordIndex: recordIndex,
                    datasetIndex: datasetIndex
                },
                i, itemEl;

            // To cope with List headers and footers, we track beforeEl and afterEl
            // as the elements before which or after which to insert adjacent things.
            options.afterEl = options.beforeEl = options.itemEl = itemEl =
                item.renderElement;

            options.itemClasses = itemEl.getClassMap(/*clone=*/false);
            options.isFirstChanged = item.isFirst !== options.isFirst;
            options.isLastChanged = item.isLast !== options.isLast;

            for (i = 0; i < handlers.length; ++i) {
                me[handlers[i]](options);
            }

            itemEl.setClassMap(options.itemClasses, /*keep=*/true);

            return options;
        },

        changeItemIsFirst: function (options) {
            if (!options.isFirstChanged) {
                return;
            }

            var me = this,
                firstCls = me.firstCls,
                item = options.item,
                itemClasses = options.itemClasses,
                items = me.scrollDockedItems,
                i, len;

            if (!(item.isFirst = options.isFirst)) {
                delete itemClasses[firstCls];
            }
            else {
                itemClasses[firstCls] = 1;

                if (items && !me.infinite) {
                    // Infinite lists maintain DOM order optionally and in their
                    // own ways...
                    items = items.start.items;
                    len = items.length;

                    for (i = 0; i < len; ++i) {
                        items[i].renderElement.insertBefore(options.beforeEl);
                    }
                }
            }
        },

        changeItemIsLast: function (options) {
            if (!options.isLastChanged) {
                return;
            }

            var me = this,
                item = options.item,
                itemClasses = options.itemClasses,
                lastCls = me.lastCls,
                items = me.scrollDockedItems,
                i, len;

            if (!(item.isLast = options.isLast)) {
                delete itemClasses[lastCls];
            }
            else {
                itemClasses[lastCls] = 1;

                if (items && !me.infinite) {
                    // Infinite lists maintain DOM order optionally and in their
                    // own ways...
                    items = items.end.items;
                    len = items.length;

                    for (i = 0; i < len; ++i) {
                        items[i].renderElement.insertAfter(options.afterEl);
                    }
                }
            }
        },

        changeItemRecord: function (options) {
            this.syncItemRecord(options);
        },

        changeItemRecordIndex: function (options) {
            var item = options.item,
                recordIndex = options.recordIndex,
                itemClasses = options.itemClasses,
                oddCls = this.oddCls;

            // Row needs to know its position in the dataset WRT paged stores.
            // Currently used by Ext.grid.cell.RowNumberer
            item.$datasetIndex = options.datasetIndex;

            if (item.isDataViewItem) {
                if (item.getRecordIndex() !== recordIndex) {
                    item.setRecordIndex(recordIndex);
                }
            } else {
                item.el.dom.setAttribute('data-recordindex', recordIndex);
            }

            if (this.striped && options.recordIndex % 2) {
                itemClasses[oddCls] = 1;
            }
            else {
                delete itemClasses[oddCls];
            }
        },

        clearItemCaches: function() {
            var cache = this.itemCache;
            Ext.destroy(cache);
            cache.length = 0;
        },

        clearItems: function () {
            var me = this,
                dataItems = me.dataItems,
                len = dataItems.length,
                itemCache = me.itemCache,
                i;

            for (i = 0; i < len; ++i) {
                me.removeDataItem(dataItems[i], true);
            }

            Ext.destroy(itemCache);

            dataItems.length = itemCache.length = 0;

            me.setItemCount(0);
        },

        createDataItem: function (cfg) {
            var me = this,
                markDirty = me.getMarkDirty(),
                cls = markDirty ? me.markDirtyCls : '',
                itemCls = me.getItemCls(),
                config;

            if (itemCls) {
                if (markDirty) {
                    cls += ' ';
                }

                cls += itemCls;
            }
            
            config = {
                xtype: me.getDefaultType(),
                cls: cls,
                tpl: me.getItemTpl(),
                $dataItem: 'record'
            };

            cls = me.getItemInnerCls();
            if (cls) {
                config.innerCls = cls;
            }

            cls = me.getItemContentCls();
            if (cls) {
                config.contentCls = cls;
            }

            return Ext.apply(config, cfg || me.getItemConfig());
        },

        doClear: function() {
            this.setItemCount(0);

            this.callParent();
        },

        doRefresh: function(scrollToTop) {
            var me = this,
                storeCount = me.dataRange.records.length,
                scroller = me.getScrollable(),
                restoreFocus;

            ++me.refreshCounter;

            if (scroller && scrollToTop) {
                scroller.scrollTo(0, 0);
            }

            if (storeCount) {
                // Stashes the NavigationModel's location for restoration after refresh
                restoreFocus = me.saveFocusState();

                me.hideEmptyText();

                me.setItemCount(storeCount);

                me.syncItemRange();

                if (me.hasSelection()) {
                    me.setItemSelection(me.getSelections(), true);
                }
                restoreFocus();
            }
            else {
                me.doClear();
            }
        },

        getFastItems: function() {
            return this.getInnerItems();
        },

        getStoreChangeSyncIndex: function(index) {
            return index;
        },

        removeCachedItem: function(item, preventCache, cache, max, preventRemoval) {
            var me = this,
                ret = false;

            if (!preventCache && cache.length < max) {
                // If we are allowed to do so, then cache what we don't
                // need right now
                if (preventRemoval) {
                    me.setItemHidden(item, true);
                } else {
                    me.remove(item, /*destroy=*/false);
                }
                cache.push(item);
            } else {
                item.destroy();
                ret = true;
            }

            return ret;
        },

        removeDataItem: function (item, preventCache) {
            return this.removeCachedItem(item, preventCache, this.itemCache,
                this.getMaxItemCache());
        },

        syncItemRange: function (start, end) {
            var count = this.store.getCount(),
                i;

            if (end == null) {
                end = count;
            }

            for (i = start || 0; i < end; ++i) {
                this.changeItem(i, i);
            }
        },

        syncItemRecord: function (options, tombstoneRec) {
            var me = this,
                item = options.item,
                itemClasses = options && options.itemClasses,
                oldRecord = item.getRecord(),
                record = tombstoneRec || options.record,
                dataMap = me.getItemDataMap(),
                el = item.el,
                viewModel = item.getViewModel(),
                selectedCls = me.selectedCls;

            if (oldRecord === record) {
                if (!tombstoneRec) {
                    if (item.isRecordRefreshable) {
                        item.refresh(options);
                    }
                    else {
                        item.updateRecord(record, oldRecord);
                    }
                }
            }
            else {
                // Ask the selection model if this record is selected
                if (me.getSelectable().isRowSelected(record)) {
                    if (itemClasses) {
                        itemClasses[selectedCls] = true;
                    }
                    else {
                        el.addCls(selectedCls);
                    }
                }
                else if (itemClasses) {
                    delete itemClasses[selectedCls];
                }
                else {
                    el.removeCls(selectedCls);
                }

                item.setRecord(record);

                item.el.dom.setAttribute('data-recordid', record.internalId);
            }

            if (dataMap) {
                Ext.dataview.DataItem.executeDataMap(record, item, dataMap);
            }

            if (viewModel) {
                viewModel.setData({
                    record: options.record  // will be null for a tombstone
                });
            }
        },

        traverseItem: function (item, delta) {
            var me = this,
                items = me.innerItems,
                next = null,
                cmp = item,
                i;

            if (item) {
                if (item.isElement) {
                    cmp = Ext.getCmp(item.id);
                }

                i = items.indexOf(cmp);
                if (i > -1) {
                    next = items[i + delta] || null;
                }
            }

            return next;
        },

        //--------------------------------------------
        // Private Configs

        // itemCount

        updateItemCount: function (count) {
            var me = this,
                items = me.dataItems,
                cfg, itemsFocusable;

            if (items.length < count) {
                cfg = me.getItemConfig();
                itemsFocusable = me.getItemsFocusable();

                while (items.length < count) {
                    me.acquireItem(cfg, itemsFocusable);
                }
            }

            while (items.length > count) {
                me.removeDataItem(items.pop());
            }
        }

    } // privates
},
function (ComponentDataView) {
    var proto = ComponentDataView.prototype;

    proto._cachedRemoveClasses = [
        proto.pressedCls,
        proto.selectedCls
    ];
});
