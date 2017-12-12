/**
 * DataView makes it easy to render one or more data items, typically from a server backend
 * or any other data source. The DataView is what powers more powerful components like
 * {@link Ext.dataview.List List} and {@link Ext.grid.Grd Grid}.
 *
 * Use DataView whenever you want to repeat the same element structure for multiple records
 * of data.
 *
 * # Creating a Simple DataView
 *
 * At its simplest, a DataView is just a Store full of data and a simple template that
 * renders each item:
 *
 *      @example
 *      var team = Ext.create({
 *          xtype: 'dataview',
 *          fullscreen: true,
 *
 *          store: [
 *              { name: 'Peter',  age: 26 },
 *              { name: 'Ray',   age: 21 },
 *              { name: 'Egon', age: 24 },
 *              { name: 'Winston', age: 24 }
 *          ],
 *
 *          itemTpl: '<div>{name} is {age} years old</div>'
 *      });
 *
 * Here we just defined everything inline so it's all local with nothing being loaded from
 * a server. For each of the data items, the DataView will render HTML using the `itemTpl`
 * For details see {@link Ext.XTemplate XTemplate}.
 *
 * Because DataView actually uses an underlying {@link Ext.data.Store Store}, any changes
 * to the data are immediately reflected on the screen. For example, if we add a new record
 * to the Store it will be rendered into our DataView.
 *
 * The same happens if we modify one of the existing records in the Store:
 *
 *      @example
 *      var team = Ext.create({
 *          xtype: 'dataview',
 *          fullscreen: true,
 *
 *          store: {
 *             fields: ['name', 'age'],
 *             data: [
 *                 {name: 'Peter',  age: 26},
 *                 {name: 'Ray',   age: 21},
 *                 {name: 'Egon', age: 24},
 *                 {name: 'Winston', age: 24}
 *             ]
 *          },
 *
 *          itemTpl: '<div>{name} is {age} years old</div>'
 *      });
 *
 *      team.getStore().add({
 *          name: 'Gozer',
 *          age: 21
 *      });
 *
 *      team.getStore().getAt(0).set('age', 42);
 *
 * This last step will get the first record in the Store (Peter), change the age to 42 and
 * automatically update what's on the screen.
 *
 * # Loading data from a server
 *
 * To load data from a server, we need to give the `store` some information about the
 * server. This is done with the `proxy` config:
 *
 *      @example
 *      Ext.create({
 *          xtype: 'dataview',
 *          fullscreen: true,
 *
 *          store: {
 *              autoLoad: true,
 *              proxy: {
 *                  type: 'jsonp',
 *                  url: 'https://itunes.apple.com/search?term=Pink+Floyd&entity=album',
 *
 *                  reader: {
 *                      type: 'json',
 *                      rootProperty: 'results'
 *                  }
 *              }
 *          },
 *
 *          itemTpl: '<h2>{collectionName}</h2><p><img src="{artworkUrl100}" /></p>'
 *      });
 *
 * The Store now loads using a {@link Ext.data.proxy.Proxy Proxy}, which fetches the data
 * for us. In this case we used a JSON-P proxy so that we can load from Twitter's JSON-P
 * search API. We also specified the fields present for each tweet, and used store's
 * {@link Ext.data.Store#autoLoad autoLoad} configuration to load automatically. Finally,
 * we configured a `reader` to decode the response from Twitter, telling it to expect
 * JSON and that the tweets can be found in the 'results' part of the JSON response.
 *
 * The last piece is an update to the `itemTpl` to render the image, Twitter username and
 * message. All we need to do now is add a little CSS to style the list the way we want
 * it and we end up with a very basic Twitter viewer. Click the preview button on the
 * example above to see it in action.
 *
 * # Components As Items
 *
 * To use an `Ext.Component` to render and manage data items, see `Ext.dataview.Component`.
 * Prior to version 6.5 the `useComponents` config was used but this config has been replaced
 * by a dedicated class.
 */
Ext.define('Ext.dataview.DataView', {
    extend: 'Ext.dataview.Abstract',

    alternateClassName: 'Ext.DataView',

    xtype: 'dataview',

    isElementDataView: true,

    /**
     * @event childtouchstart
     * Fires when a child is first touched.
     * @param {Ext.dataview.DataView} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childtouchmove
     * Fires when a touch move occurs on a child.
     * @param {Ext.dataview.DataView} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childtouchend
     * Fires when a touch ends on a child.
     * @param {Ext.dataview.DataView} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childtouchcancel
     * Fires when a touch is cancelled.
     * @param {Ext.dataview.DataView} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childtap
     * Fires when a child is tapped.
     * @param {Ext.dataview.DataView} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childlongpress
     * Fires when a child is long-pressed.
     * @param {Ext.dataview.DataView} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childtaphold
     * Fires when a child is tap-held.
     * @param {Ext.dataview.DataView} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childsingletap
     * Fires when a child is single tapped.
     * @param {Ext.dataview.DataView} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childdoubletap
     * Fires when a child is double tapped.
     * @param {Ext.dataview.DataView} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childmouseenter
     * Fires when the mouse pointer enters a child.
     * @param {Ext.dataview.DataView} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    /**
     * @event childmouseleave
     * Fires when the mouse pointer leaves a child.
     * @param {Ext.dataview.DataView} this This dataview.
     * @param {Ext.dataview.Location} location The location for the event.
     *
     * @since 6.5.0
     */

    constructor: function (config) {
        if (config && config.useComponents) {
            //<debug>
            if (this.self !== Ext.dataview.DataView) {
                Ext.raise('The useComponents config has been replaced by Ext.dataview.Component');
            }
            Ext.log.warn('The useComponents config has been replaced by Ext.dataview.Component');
            //</debug>

            // For compatibility sake, we can redirect the creation to the right
            // place:
            return new Ext.dataview['Component'](config); // hide usage from Cmd
        }

        //<debug>
        if (this.useComponents) {
            Ext.raise('The useComponents config has been replaced by Ext.dataview.Component');
        }
        //</debug>

        this.callParent([ config ]);
    },

    getViewItems: function() {
        return Array.prototype.slice.call(this.getFastItems());
    },

    onStoreAdd: function (store, records, index) {
        this.callParent(arguments);

        this.renderItems(index, index + records.length);
    },

    onStoreRemove: function(store, records, index) {
        this.removeItems(index, index + records.length);
    },

    privates: {
        dirtyCls: Ext.baseCSSPrefix + 'dirty',

        changeItem: function (recordIndex) {
            var me = this,
                dataItems = me.dataItems,
                item = dataItems[recordIndex],
                record = me.dataRange.records[recordIndex],
                storeCount = me.store.getCount(),
                options = {
                    isFirst: !recordIndex,
                    isLast: recordIndex === storeCount -1,
                    item: item,
                    record: record,
                    recordIndex: recordIndex
                };

            me.syncItemRecord(options);
        },

        clearItems: function() {
            var elements = this.dataItems,
                dom;

            while (elements.length) {
                dom = elements.pop();
                Ext.fly(dom).destroy();
            }
        },

        createDataItem: function (index, record) {
            var me = this,
                store = me.store,
                data = me.gatherData(record, index),
                markDirty = me.getMarkDirty(),
                dom, itemEl;

            itemEl = Ext.Element.create(me.getItemElementConfig(index, data, store));
            dom = itemEl.dom;

            if (markDirty) {
                itemEl.addCls(me.markDirtyCls);
            }

            dom.setAttribute('data-viewid', me.id);
            dom.setAttribute('data-recordid', record.internalId);
            dom.setAttribute('data-recordindex', index);

            return itemEl;
        },

        doClear: function() {
            this.clearItems();

            this.callParent();
        },

        doRefresh: function (scrollToTop) {
            var me = this,
                records = me.dataRange.records,
                storeCount = records.length,
                itemCount = me.dataItems.length,
                scroller = me.getScrollable(),
                restoreFocus, i;

            if (scroller && scrollToTop) {
                scroller.scrollTo(0, 0);
            }

            ++me.refreshCounter;

            // No items, hide all the items from the collection.
            if (!storeCount) {
                me.doClear();
            }
            else {
                // Stashes the NavigationModel's location for restoration after refresh
                restoreFocus = me.saveFocusState();
                me.hideEmptyText();

                if (itemCount > storeCount) {
                    me.removeItems(storeCount, itemCount);
                    // We've removed extra items, but all remaining items need to
                    // be refreshed
                    itemCount = storeCount;
                }
                else if (itemCount < storeCount) {
                    me.renderItems(itemCount, storeCount);
                    // We've rendered the new items but all pre-existing items
                    // need to be refreshed
                }

                for (i = 0; i < itemCount; ++i) {
                    me.changeItem(i);
                }

                if (me.hasSelection()) {
                    me.setItemSelection(me.getSelections(), true);
                }
                restoreFocus();
            }
        },

        getFastItems: function() {
            return this.getRenderTarget().dom.childNodes;
        },

        getItemElementConfig: function (index, data, store) {
            var me = this,
                result = {
                    cls: me.baseCls + '-item ' + (me.getItemCls() || ''),
                    html: me.renderItemTpl(index, data, store)
                };

            // The element must accept focus for navigation to occur.
            // The item component must not be focusable. It must not participate in a
            // FocusableContainer relationship with the List's container,
            // and must not react to focus events or its focus API itself.
            // It is a slave of the NavigationModel.
            if (me.getItemsFocusable()) {
                result.tabIndex = -1;
            }

            return result;
        },

        removeItems: function (from, to) {
            var me = this,
                items = me.dataItems.splice(from, to - from),
                i;

            for (i = 0; i < items.length; ++i) {
                Ext.fly(items[i]).destroy();
            }
        },

        renderItems: function (from, to) {
            var me = this,
                dataItems = me.dataItems,
                records = me.dataRange.records,
                parentNode = me.getRenderTarget().dom,
                args = [ from, 0 ],
                before = me.dataItems[from] || null,
                dom, i;

            if (records.length) {
                me.hideEmptyText();
            }

            if (!before) {
                // We don't have a data item rendered beyond this range, so either
                // before should be null (to append to parentNode) or it should be
                // the last scrollDock:'end'
                before = me.findTailItem(/*rawElements=*/true);
                before = before && before.el.dom;
            }

            for (i = from; i < to; ++i) {
                args.push(dom = me.createDataItem(i, records[i]).dom);

                parentNode.insertBefore(dom, before);
            }

            dataItems.splice.apply(dataItems, args);
        },

        renderItemTpl: function (index, data, store) {
            var itemTpl = this.getItemTpl(),
                parent = store.getData().items,
                value;

            data.xcount = typeof data.xcount === 'number' ? data.xcount : store.getCount();
            data.xindex = typeof data.xindex === 'number' ? data.xindex : index;

            value = itemTpl.apply(data, parent, index+1, parent.length);

            value = (value == null) ? '' : String(value);

            return value || this.getEmptyItemText();
        },

        syncItemRecord: function (options) {
            var me = this,
                item = options.item,
                record = options.record,
                store = me.store,
                recordIndex = options ? options.recordIndex : store.indexOf(record),
                data = me.gatherData(record, recordIndex),
                dirtyCls = me.$dirty;

            item.innerHTML = me.renderItemTpl(recordIndex, data, store);
            item.setAttribute('data-recordid', record.internalId);
            item.setAttribute('data-recordindex', recordIndex);

            Ext.fly(item).toggleCls(me.dirtyCls, record.dirty);
        },

        traverseItem: function (item, delta) {
            var me = this,
                items = me.getRenderTarget().dom.childNodes,
                next = null,
                dom, i;

            if (item) {
                if (item.isElement) {
                    dom = item.dom;
                }
                else if (item.isWidget) {
                    dom = item.el.dom;
                }

                i = Array.prototype.indexOf.call(items, dom);
                if (i > -1) {
                    next = items[i + delta] || null;

                    if (next) {
                        next = Ext.getCmp(next.id) || next;
                    }
                }
            }

            return next;
        }
    }
});
