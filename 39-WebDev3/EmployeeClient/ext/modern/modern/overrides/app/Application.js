/**
 * @class Ext.app.Application
 */

Ext.define('Ext.overrides.app.Application', {
    override: 'Ext.app.Application',
    requires: ['Ext.viewport.Viewport'],
    uses: ['Ext.tip.Manager'],

    config: {
        /**
         * @cfg {Object} viewport
         * Any configuration to be passed on to the {@link Ext.Viewport}.
         *
         * @since 6.5.0
         */
        viewport: null
    },

    // @cmd-auto-dependency {defaultType: "Ext.tip.Manager"}
    /**
     * @cfg {Boolean/Object} quickTips
     * `true` to enable quick tips to be read from the DOM and displayed
     * by the `Ext.tip.Manager`. Pass the object form as a configuration
     * for `Ext.tip.Manager`.
     *
     * @since 6.2.0
     */
    quickTips: false,

    destroy: function () {
        this.setQuickTips(false);

        this.callParent();
    },

    initMainView: function() {
        var me = this,
            viewport = me.viewport = Ext.Viewport,
            mainView;

        me.callParent();

        mainView = me.getMainView();

        // Ensure the viewport is ready by the time launch is called
        viewport.onAppLaunch();

        if (mainView) {
            viewport.add(mainView);
        }
    },

    applyQuickTips: function(quickTips) {
        quickTips = quickTips || null;

        if (quickTips) {
            if (quickTips === true) {
                quickTips = {};
            }
            quickTips = new Ext.tip['Manager'](quickTips);
        }

        return quickTips;
    },

    updateQuickTips: function(quickTips, oldQuickTips) {
        if (oldQuickTips) {
            oldQuickTips.destroy();
        }
    }
});
