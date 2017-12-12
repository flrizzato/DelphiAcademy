/**
 * This layout is used to center contents within a container.
 *
 * Example usage:
 *
 *      // The content component is centered inside the panel
 *
 *      var p = Ext.create('Ext.Panel', {
 *          title: 'Center Layout',
 *          layout: 'center',
 *          height: 200,
 *          width: 200,
 *          items: [{
 *              title: 'Centered Content',
 *              width: '75%',
 *              height: '75%',
 *              html: 'Some content'
 *          }]
 *      });
 */
Ext.define('Ext.layout.Center', {
    extend: 'Ext.layout.Auto',
    alias: 'layout.center',
    cls: Ext.baseCSSPrefix + 'layout-center',
    itemCls: Ext.baseCSSPrefix + 'layout-center-item'
});