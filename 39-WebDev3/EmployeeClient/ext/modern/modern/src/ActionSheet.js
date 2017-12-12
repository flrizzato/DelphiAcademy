/**
 * {@link Ext.ActionSheet ActionSheets} are used to display a list of {@link Ext.Button buttons}
 * in a popup dialog.
 *
 * The key difference between ActionSheet and {@link Ext.Sheet} is that ActionSheets are
 * docked at the bottom of the screen, and the {@link #defaultType} is set to {@link Ext.Button button}.
 *
 * ## Example
 *
 *     @example preview miniphone
 *     var actionSheet = Ext.create('Ext.ActionSheet', {
 *         items: [
 *             {
 *                 text: 'Delete draft',
 *                 ui  : 'decline'
 *             },
 *             {
 *                 text: 'Save draft'
 *             },
 *             {
 *                 text: 'Cancel',
 *                 ui  : 'confirm'
 *             }
 *         ]
 *     });
 *
 *     Ext.Viewport.add(actionSheet);
 *     actionSheet.show();
 *
 * ## Edge Menus
 * Action Sheets can be used with {@link Ext.Viewport#setMenu}. They can be linked with
 * any side of the screen (top, left, bottom or right). To use this menu you will call various
 * menu related functions on the {@link Ext.Viewport Viewport} such as {@link Ext.Viewport#showMenu},
 * {@link Ext.Viewport#hideMenu}, {@link Ext.Viewport#toggleMenu}, {@link Ext.Viewport#hideOtherMenus},
 * or {@link Ext.Viewport#hideAllMenus}.
 *
 *      @example
 *      var menu = Ext.create({
 *          xtype: 'actionsheet',
 *          items: [{
 *              text: 'Settings',
 *              iconCls: 'settings'
 *          }, {
 *              text: 'New Item',
 *              iconCls: 'compose'
 *          }, {
 *              text: 'Star',
 *              iconCls: 'star'
 *          }]
 *      });
 *
 *      Ext.Viewport.add({
 *          xtype: 'panel',
 *          html: 'Main View Content'
 *      });
 *
 *      Ext.Viewport.setMenu(menu, {
 *          side: 'left',
 *          // omitting the reveal config defaults the animation to 'cover'
 *          reveal: true
 *      });
 *
 *      Ext.Viewport.showMenu('left');
 */
Ext.define('Ext.ActionSheet', {
    extend: 'Ext.Sheet',
    requires: ['Ext.Button'],
    xtype: 'actionsheet',
    classCls: Ext.baseCSSPrefix + 'actionsheet',
    centered: false,
    layout: 'vbox',
    side: 'bottom',
    defaultType: 'button'
});
