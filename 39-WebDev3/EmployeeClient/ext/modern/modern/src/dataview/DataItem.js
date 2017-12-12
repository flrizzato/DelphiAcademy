/**
 * This is the default type for items in a {@link Ext.dataview.Component component dataview}.
 * It ties together {@link Ext.data.Model records} to its contained Components.
 *
 * Consider the following example:
 *
 *      @example
 *      Ext.create({
 *          xtype: 'componentdataview',
 *
 *          store: [
 *              { name: 'Peter', age: 26 },
 *              { name: 'Ray', age: 28 },
 *              { name: 'Egon', age: 24 },
 *              { name: 'Winston', age: 29 }
 *          ],
 *
 *          itemConfig: {
 *              layout: 'hbox',
 *              padding: 10,
 *
 *              items: [{
 *                  xtype: 'component',
 *                  reference: 'textCmp'
 *              }, {
 *                  xtype: 'button',
 *                  margin: '0 0 0 5',
 *                  reference: 'checkBtn',
 *                  text: 'Check'
 *              }]
 *          },
 *
 *          itemDataMap: {
 *              textCmp: {
 *                  html: 'name'
 *              }
 *          }
 *      });
 *
 * If the mapping of records to components is more complex, you can extend this class and
 * provide a custom `updateRecord` method or use {@link Ext.app.ViewModel data binding}.
 *
 *      @example
 *      Ext.create({
 *          xtype: 'componentdataview',
 *
 *          store: [
 *              { name: 'Peter', age: 26 },
 *              { name: 'Ray', age: 28 },
 *              { name: 'Egon', age: 24 },
 *              { name: 'Winston', age: 29 }
 *          ],
 *
 *          itemConfig: {
 *              layout: 'hbox',
 *              padding: 10,
 *              viewModel: true, // enable per-item record binding
 *
 *              items: [{
 *                  xtype: 'component',
 *                  bind: 'Greetings {record.name}!'
 *              }, {
 *                  xtype: 'button',
 *                  margin: '0 0 0 5',
 *                  text: 'Check'
 *              }]
 *          }
 *      });
 */
Ext.define('Ext.dataview.DataItem', function (DataItem) { return {
    extend: 'Ext.Container',
    alternateClassName: 'Ext.dataview.component.DataItem',
    xtype: 'dataitem',

    mixins: [
        'Ext.dataview.GenericItem'
    ],

    config: {
        /**
         * @cfg {String} itemCls
         * An additional CSS class to apply to items within the DataView.
         */
        itemCls: null,

        /**
         * @cfg {Object} dataMap
         * The dataMap allows you to map {@link #record} fields to specific configurations
         * in this component.
         *
         * The `dataMap` object's keys describe the target objects to receive data from
         * the associated {@link #cfg!record record}. These keys are either `'#'` (for the
         * item itself) or a {@link Ext.Component#cfg!reference reference} to a component
         * contained in the item.
         *
         * For each target listed in `dataMap`, the value is another map describing the
         * config name (in the key) and the data field name (as the value).
         *
         * For example:
         *
         *      dataMap: {
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
         *      this.setTitle(this.getRecord().get('fullName'));
         *      this.lookup('text').setHtml(this.getRecord().get('name'));
         *
         * **Note:** Prior to version 6.5, the first level keys were names of getter
         * methods and the second level keys were names of setter methods. While this
         * form is still supported, it is deprecated and will be removed in 7.0.
         */
        dataMap: {
            cached: true,
            $value: null
        }
    },

    html: '\xA0',

    classCls: Ext.baseCSSPrefix + 'dataitem',

    inheritUi: true,

    autoSize: null,

    defaultType: 'component',

    // Since DataItem's are produce in bulk we can be sure they will create
    // duplicate reference names in the referenceHolder/controller above them
    // so we can safely assume this would be best.
    referenceHolder: true,

    template: [{
        reference: 'bodyElement',
        cls: Ext.baseCSSPrefix + 'body-el',
        uiCls: 'body-el',
        children: [{
            reference: 'innerElement',
            cls: Ext.baseCSSPrefix + 'inner-el',
            uiCls: 'inner-el'
        }]
    }],

    updateItemCls: function(newCls, oldCls) {
        this.el.replaceCls(oldCls, newCls);
    },

    /**
     * Updates this container's child items, passing through the `dataMap`.
     * @param {Ext.data.Model} record
     * @private
     */
    updateRecord: function(record) {
        if (this.destroying || this.destroyed) {
            return;
        }
        
        var me = this,
            dataMap = me.getDataMap(),
            tpl = me.getTpl(),
            data;

        if (dataMap) {
            DataItem.executeDataMap(record, me, dataMap);
        }

        me.syncDirty(record);

        if (tpl || !dataMap || me.hasListeners.updatedata) {
            data = me.parent.gatherData(record);

            if (tpl) {
                me.updateData(data);
            }

            /**
             * @event updatedata
             * Fires whenever the data of the DataItem is updated.
             * @param {Ext.dataview.DataItem} dataItem The DataItem instance.
             * @param {Object} newData The new data.
             */
            if (me.hasListeners.updatedata) {
                me.fireEvent('updatedata', me, data);
            }
        }
    },

    updateHtml: function(html, oldHtml) {
        this.callParent([this.handleEmptyText(html), oldHtml]);
    },

    privates: {
        applyDataMap: function (dataMap) {
            return DataItem.parseDataMap(dataMap);
        },

        getRenderTarget: function() {
            return this.innerElement;
        },

        statics: {
            assignDataToItem: function (record, target, mappings, legacy) {
                var configMap = Ext.Config.map,
                    cfg, dataPath, i, n, name, s, value;

                for (name in mappings) {
                    s = legacy ? name : ((cfg = configMap[name]) && cfg.names.set);
                    if (!target[s]) {
                        //<debug>
                        if (legacy) {
                            Ext.raise('No method "' + name + '" on ' + target.$className);
                        } else {
                            Ext.raise('No config "' + name + '" on ' + target.$className);
                        }
                        //</debug>
                        continue;
                    }

                    // This is an array of names to follow from the record. Like in
                    // data binding these names can be association names or field
                    // names.
                    dataPath = mappings[name];
                    value = record;
                    for (i = 0, n = dataPath.length; value && i < n; ++i) {
                        value = value.interpret(dataPath[i]);
                    }

                    target[s]((i < n) ? null : value);
                }
            },

            executeDataMap: function (record, item, dataMap) {
                var reference, legacy, target, mappings;

                for (reference in dataMap) {
                    if (!(mappings = dataMap[reference])) {
                        continue;
                    }

                    legacy = false;
                    if (!(target = (reference === '#') ? item : item.lookup(reference))) {
                        // Prior to 6.5, this first level of keys was the getter method
                        // name... so fallback but warn of the removal of this support
                        if (typeof item[reference] === 'function') {
                            target = item[reference]();
                            legacy = true;

                            //<debug>
                            if (!item.$dataMapWarning) {
                                item.$dataMapWarning = true;
                                Ext.log.warn('Using getters in dataMaps is deprecated (for ' +
                                    item.getId() + '); support will be removed in 7.0');
                            }
                            //</debug>
                        }

                        if (!target) {
                            continue;
                        }
                    }

                    DataItem.assignDataToItem(record, target, mappings, legacy);
                }
            },

            parseDataMap: function (dataMap) {
                var map = {},
                    inner, innerSrc, key1, key2;

                for (key1 in dataMap) {
                    map[key1] = inner = {};
                    innerSrc = dataMap[key1];

                    for (key2 in innerSrc) {
                        inner[key2] = innerSrc[key2].split('.');
                    }
                }

                return map;
            }
        } // statics
    } // privates
};
});
