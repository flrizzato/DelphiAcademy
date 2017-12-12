/**
 * @private
 */
Ext.define('Ext.layout.card.fx.Flip', {
    extend: 'Ext.layout.card.fx.Serial',

    alias: 'layout.card.fx.flip',

    config: {
        inAnimation: {
            type: 'flip',
            half: true,
            easing: 'ease-out',
            before: {
                'backface-visibility': 'hidden'
            },
            after: {
                'backface-visibility': null
            }
        },
        outAnimation: {
            type: 'flip',
            half: true,
            easing: 'ease-in',
            before: {
                'backface-visibility': 'hidden'
            },
            after: {
                'backface-visibility': null
            },
            out: true
        }
    },

    onActiveItemChange: function(cardLayout, newItem, oldItem, controller) {
        var parent = newItem.element.getParent();
        parent.addCls(Ext.baseCSSPrefix + 'layout-card-perspective');

        this.on('animationend', function() {
            parent.removeCls(Ext.baseCSSPrefix + 'layout-card-perspective');
        }, this, {single: true});

        this.callParent(arguments);
    }
});
