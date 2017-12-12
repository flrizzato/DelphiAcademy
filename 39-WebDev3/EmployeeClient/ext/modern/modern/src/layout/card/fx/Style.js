/**
 * @private
 */
Ext.define('Ext.layout.card.fx.Style', {

    extend: 'Ext.layout.card.fx.Abstract',

    requires: [
        'Ext.fx.Animation'
    ],

    config: {
        inAnimation: {
            before: {
                visibility: null
            },
            preserveEndState: false,
            replacePrevious: true
        },

        outAnimation: {
            preserveEndState: false,
            replacePrevious: true
        }
    },

    /**
     * @property {Boolean} isAnimating
     * This value is true while the card switch animation is in progress.
     */
    isAnimating: false,

    constructor: function(config) {
        var inAnimation, outAnimation;

        this.callParent([config]);

        this.endAnimationCounter = 0;

        inAnimation = this.getInAnimation();
        outAnimation = this.getOutAnimation();

        inAnimation.on('animationend', 'incrementEnd', this);
        outAnimation.on('animationend', 'incrementEnd', this);
    },

    updateDirection: function(direction) {
        this.getInAnimation().setDirection(direction);
        this.getOutAnimation().setDirection(direction);
    },

    updateDuration: function(duration) {
        this.getInAnimation().setDuration(duration);
        this.getOutAnimation().setDuration(duration);
    },

    updateReverse: function(reverse) {
        this.getInAnimation().setReverse(reverse);
        this.getOutAnimation().setReverse(reverse);
    },

    incrementEnd: function() {
        this.endAnimationCounter++;

        if (this.endAnimationCounter > 1) {
            this.endAnimationCounter = 0;
            this.fireEvent('animationend', this);
        }
    },

    applyInAnimation: function(animation, inAnimation) {
        return Ext.factory(animation, Ext.fx.Animation, inAnimation);
    },

    applyOutAnimation: function(animation, outAnimation) {
        return Ext.factory(animation, Ext.fx.Animation, outAnimation);
    },

    updateInAnimation: function(animation) {
        animation.setScope(this);
    },

    updateOutAnimation: function(animation) {
        animation.setScope(this);
    },

    onActiveItemChange: function(cardLayout, newItem, oldItem, controller) {
        var me = this,
            inElement, outElement,
            inAnimation, outAnimation;

        if (newItem && oldItem && oldItem.isPainted()) {
            inAnimation = me.getInAnimation();
            outAnimation = me.getOutAnimation();

            inElement = newItem.renderElement;
            outElement = oldItem.renderElement;

            inAnimation.setElement(inElement);
            outAnimation.setElement(outElement);

            outAnimation.setOnEnd(function() {
                me.isAnimating = false;
                controller.resume();
            });

            inElement.dom.style.setProperty('visibility', 'hidden', 'important');
            newItem.show();
            cardLayout.renderInnerItem(newItem, true);

            me.isAnimating = true;
            Ext.Animator.run([outAnimation, inAnimation]);
            controller.pause();
        }
    },

    destroy:  function () {
        Ext.destroy(this.getInAnimation(), this.getOutAnimation());

        this.callParent();
    }
});
