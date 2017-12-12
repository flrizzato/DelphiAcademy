/**
 * This is a layout for container that contain a single item that automatically expands to
 * fill the container. This class is intended to be extended or created via the layout:'fit'
 * {@link Ext.container.Container#layout} config, and should generally not need to be created
 * directly via the new keyword.
 *
 * Fit layout does not have any direct config options (other than inherited ones). To fit a
 * panel to a container using Fit layout, simply set `layout: 'fit'` on the container and
 * add a single panel to it.
 *
 *     @example
 *     var panel = Ext.create('Ext.Panel', {
 *         title: 'Fit Layout',
 *         width: 300,
 *         height: 150,
 *         layout:'fit',
 *         items: {
 *             title: 'Inner Panel',
 *             html: 'This is the inner panel content',
 *             bodyPadding: 20,
 *             border: false
 *         }
 *     });
 *
 *     Ext.Viewport.add(panel);
 */
Ext.define('Ext.layout.Fit', {
    extend: 'Ext.layout.Auto',
    alias: 'layout.fit',

    isFit: true,

    cls: Ext.baseCSSPrefix + 'layout-fit',

    itemCls: Ext.baseCSSPrefix + 'layout-fit-item'
});
