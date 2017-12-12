/**
 * @private
 */
Ext.define('Ext.layout.card.fx.Serial', {
    extend: 'Ext.layout.card.fx.Style',

    duration: 500,

    updateDuration: function(duration) {
        var halfDuration = duration / 2,
            inAnimation = this.getInAnimation(),
            outAnimation = this.getOutAnimation();

        inAnimation.setDelay(halfDuration);
        inAnimation.setDuration(halfDuration);
        outAnimation.setDuration(halfDuration);
    }
});
