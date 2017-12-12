/**
 * This class acts as a factory for environment-specific viewport implementations.
 *
 * Please refer to the {@link Ext.Viewport} documentation about using the global instance.
 * @private
 */
Ext.define('Ext.viewport.Viewport', {
    requires: [
        'Ext.viewport.Ios',
        'Ext.viewport.Android',
        'Ext.viewport.WindowsPhone'
    ],
    singleton: true,

    setup: function (config) {
        var osName = Ext.os.name,
            viewportName,
            viewport;

        switch (osName) {
            case 'Android':
                viewportName = (Ext.browser.name === 'ChromeMobile') ? 'Default' : 'Android';
                break;

            case 'iOS':
                viewportName = 'Ios';
                break;

            case 'Windows':
                viewportName = (Ext.browser.name === 'IE') ? 'WindowsPhone' : 'Default';
                break;

            case 'WindowsPhone':
                viewportName = 'WindowsPhone';
                break;

            default:
                viewportName = 'Default';
                break;
        }

        return Ext.Viewport = viewport = Ext.create('Ext.viewport.' + viewportName, config);
    }
});

// Docs for the singleton instance created by above factory:

/**
 * @class Ext.Viewport
 * @extends Ext.viewport.Default
 * @singleton
 *
 * Ext.Viewport is an instance created when you use {@link Ext#setup}. Because {@link Ext.Viewport} extends from
 * {@link Ext.Container}, it has a {@link #layout} that defaults to {@link Ext.layout.Card}. This means you
 * can add items to it at any time, from anywhere in your code. The {@link Ext.Viewport} {@link #cfg-fullscreen}
 * configuration is `true` by default, so it will take up your whole screen.
 *
 *     @example raw
 *     Ext.application({
 *         name: 'MyApp',
 *
 *         launch: function() {
 *             Ext.Viewport.add({
 *                 xtype: 'panel',
 *                 title: 'New Panel',
 *                 html: 'My new panel!'
 *             });
 *         }
 *     });
 *
 * If you want to customize anything about this {@link Ext.Viewport} instance, you can do so by adding a property
 * called `viewport` into your {@link Ext#application} object:
 *
 *     @example raw
 *     Ext.application({
 *         name: 'MyApp',
 *
 *         viewport: {
 *             layout: 'vbox'
 *         },
 *
 *         launch: function() {
 *             Ext.Viewport.add([{
 *                 xtype: 'panel',
 *                 flex: 1,
 *                 title: 'Top Panel',
 *                 html: 'The top panel'
 *             }, {
 *                 xtype: 'panel',
 *                 flex: 1,
 *                 title: 'Bottom Panel',
 *                 html: 'The bottom panel'
 *             }]);
 *         }
 *     });
 *
 * **Note** if you use {@link Ext#onReady}, this instance of {@link Ext.Viewport} will **not** be created. Though, in most cases,
 * you should **not** use {@link Ext#onReady}.
 */
