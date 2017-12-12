/**
 * The SortableList plugin gives your list items the ability to be reordered by tapping and
 * dragging elements within the item.
 *
 * The list-sortablehandle is not added to your tpl by default, so it's important that you
 * manually include it. It's also important to recognize that list-items are not draggable
 * themselves.  You must add an element to the itemTpl for it to be dragged.
 *
 *     Ext.Viewport.add({
 *          xtype: 'list',
 *          infinite: true,
 *          plugins: {
 *              sortablelist: true
 *          },
 *          itemTpl: '<span class="myStyle ' + Ext.baseCSSPrefix + 'list-sortablehandle"></span>{text}',
 *          data: [{
 *              text: 'Item 1'
 *          }, {
 *              text: 'Item 2'
 *          }, {
 *              text: 'Item 3'
 *          }]
 *     });
 *
 * The CSS for MyStyle can be anything that creates an element to tap and drag.  For this
 * example we made a simple rectangle like so:
 *
 *      .myStyle{
 *          width:30px;
 *          height:20px;
 *          background:gray;
 *          float:left;
 *      }
 *
 * Note: You must have infinite set to 'true' when using the SortableList plugin.
 *
 */
Ext.define('Ext.dataview.plugin.SortableList', {
    extend: 'Ext.plugin.Abstract',
    alias: 'plugin.sortablelist',
    alternateClassName: 'Ext.plugin.SortableList',

    requires: [
        'Ext.drag.Source',
        'Ext.drag.proxy.Original'
    ],

    config: {
        list: null,

        source: {
            xclass: 'Ext.drag.Source',
            handle: '.' + Ext.baseCSSPrefix + 'list-sortablehandle',
            constrain: {
                vertical: true
            },
            proxy: {
                getElement: function(info) {
                    return this.getSource().list.mapToItem(info.initialEvent).el;
                }
            }
        }
    },

    init: function(list) {
        this.setList(list);
    },

    updateList: function(list) {
        var source;

        if (list) {
            source = this.getSource();
            if (source) {
                source.list = list;
                source.setElement(list.getRenderTarget());
            }
        }
    },

    applySource: function(source) {
        if (source) {
            source = Ext.create(source);
        }
        return source;
    },

    updateSource: function(source, oldSource) {
        var list = this.getList();

        Ext.destroy(oldSource);

        if (source) {
            source.on({
                scope: this,
                dragstart: 'onDragStart',
                dragmove: 'onDrag',
                dragend: 'onDragEnd'
            });

            if (list) {
                source.list = list;
                source.setElement(list.getRenderTarget());
            }
        }
    },

    onDragStart: function(source, info) {
        var list = this.getList(),
            item = list.mapToItem(info.initialEvent);

        item.addCls(Ext.baseCSSPrefix + 'item-no-ripple');

        // Clear the translate since drag uses left/top
        item.translate(0, 0);
        info.item = item;
        info.startIndex = item.getRecordIndex();
        info.listTop = list.getRenderTarget().getTop();
        info.itemHeight = item.el.measure('h');
        info.halfHeight = info.itemHeight / 2;

        list.stickItem(item, {
            floated: true
        });
    },

    onDrag: function(source, info) {
        var list = this.getList(),
            top = Math.max(0, info.cursor.current.y - info.listTop),
            idx = list.bisectPosition(top + info.halfHeight),
            o = {};

        o[idx] = info.itemHeight;
        info.index = idx;

        list.setGaps(o);
    },

    onDragEnd: function(source, info) {
        var me = this,
            list = me.getList(),
            item = info.item,
            style = info.item.el.dom.style,
            compareItem = list.mapToItem(info.index),
            top, pos;

        item.getTranslatable().on('animationend', function() {
            if (me.destroyed) {
                return;
            }

            var store = list.getStore(),
                startIndex = info.startIndex,
                index = compareItem ? compareItem.getRecordIndex() : list.getStore().getCount(),
                rec = item.getRecord();

            list.stickItem(item);
            list.setGaps(null);

            if (startIndex !== index) {
                store.insert(index, rec);
                index = store.indexOf(rec);

                // Since we've moved the item, it may have changed, grab it again
                item = list.mapToItem(rec);
                list.fireEvent('dragsort', list, item, index);
            }
            item.removeCls(Ext.baseCSSPrefix + 'item-no-ripple');
        }, me, {single: true});

        if (!compareItem) {
            pos = list.mapToItem(info.index - 1).$y1;
        } else {
            pos = compareItem.$y0;
        }

        // Dragging uses left/top, so make it translate instead
        top = item.element.getTop(true);
        style.left = style.top = '';
        item.translate(0, top);
        item.translate(null, pos, {duration: 100});
    }
});
