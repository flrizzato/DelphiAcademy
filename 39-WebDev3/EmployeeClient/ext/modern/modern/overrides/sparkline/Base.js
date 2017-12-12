/**
 * @class Ext.sparkline.Base
 */

Ext.define('Ext.override.sparkline.Base', {
    override: 'Ext.sparkline.Base',

    statics: {
        constructTip: function() {
            return new Ext.tip['ToolTip']({
                id: 'sparklines-tooltip',
                trackMouse: true,
                showDelay: 0,
                dismissDelay: 0,
                hideDelay: 400
            });
        }
    },

    onMouseMove: function (e) {
        this.currentEvent = e;
        this.getSharedTooltip().currentTarget.attach(this.element);
        this.callParent([e]);
    },

    privates: {
        hideTip: function() {
            var tip = this.getSharedTooltip();
            // Will detach the currentTarget and hide soon...
            // unless we quickly attach to a new one.
            tip.delayHide();
        },

        showTip: function() {
            this.getSharedTooltip().handleTargetOver(this.currentEvent, this.element);
        }
    }
});