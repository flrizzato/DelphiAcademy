/**
 * A ListItem is a container for {@link Ext.dataview.List} with 
 * useSimpleItems: false. 
 * 
 * ListItem configures and updates the {@link Ext.data.Model records} for  
 * the sub-component items in a list. 
 *
 * *Note*: Use of ListItem increases overhead since it generates more markup than
 * using the List class with useSimpleItems: true. This overhead is more
 * noticeable in Internet Explorer. If at all possible, use
 * {@link Ext.dataview.SimpleListItem} instead via the List's
 * {@link Ext.dataview.List#useSimpleItems useSimpleItems} config.
 *
 * The following example shows how to configure and update sub-component items
 * in a list:
 *
 *      Ext.define('App.view.twitter.TweetListItem', {
 *          extend: 'Ext.dataview.ListItem',
 *          xtype : 'tweetlistitem',
 *
 *          requires: [
 *              'Ext.Img'
 *          ],
 *
 *          layout: 'vbox',
 *
 *          items: [{
 *              xtype: 'component',
 *              cls: 'username',
 *              reference: 'userName'
 *          }, {
 *              xtype: 'component',
 *              cls: 'text',
 *              reference: 'textCmp'
 *          }, {
 *              xtype : 'image',
 *              reference: 'avatarImg',
 *              docked: 'left',
 *              cls: 'avatar',
 *              width: 48,
 *              height: 48
 *          }],
 *
 *          dataMap: {
 *              // Set "html" config of component w/reference "userName"
 *              // to the "username" field from the associated record.
 *              //
 *              userName: {
 *                  html: 'username'
 *              },
 *
 *              textCmp: {
 *                  html: 'text'
 *              },
 *
 *              avatarImg: {
 *                  src: 'avatar_url'
 *              }
 *          }
 *      });
 *
 */
Ext.define('Ext.dataview.ListItem', {
    extend: 'Ext.dataview.DataItem',
    alternateClassName: 'Ext.dataview.component.ListItem',
    xtype: 'listitem',

    requires: [
        'Ext.dataview.ItemHeader'
    ],

    mixins: [
        'Ext.dataview.Disclosable', // must come before Toolable
        'Ext.mixin.Toolable',
        'Ext.dataview.Pinnable'
    ],

    classCls: [
        // ListItem is classClsRoot so that it can opt out of inheriting styles from
        // DataItem, but it still needs to inherit container and component styles
        Ext.baseCSSPrefix + 'listitem',
        Ext.baseCSSPrefix + 'container',
        Ext.baseCSSPrefix + 'component'
    ],

    classClsRoot: true,

    inheritUi: true,

    items: null,  // base class has one item by default

    updateRecord: function(record) {
        var me = this;

        if (!me.destroying && !me.destroyed) {
            me.callParent([record]);
            
            me.syncDisclosure(record);
        }
    },

    doDestroy: function() {
        this.mixins.toolable.doDestroy.call(this);
        this.callParent();
    },

    privates: {
        invokeToolHandler: function (tool, handler, scope, args, e) {
            if (this.invokeDisclosure(tool, handler, e)) {
                return false;
            }

            return tool.invokeToolHandler(tool, handler, scope, args, e);
        }
    }
});
