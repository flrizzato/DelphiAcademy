/**
 * @class Ext.util.translatable.Component
 * @private
 */

Ext.define('Ext.util.translatable.Component', {
    extend: 'Ext.util.translatable.CssTransform',

    alias: 'translatable.component', // also configures Factoryable
    
    config: {
        component: null
    },

    doTranslate: function(x, y) {
        var component = this.getComponent();

        if (component.getFloated()) {
            component.setX(x);
            component.setY(y);
        } else if (component.isPositioned()) {
            component.setLeft(x);
            component.setTop(y);
        } else {
            this.callParent([x, y]);
        }
    },

    syncPosition: function() {
        var component = this.getComponent(),
            result;

        if (component.getFloated()) {
            result = [component.getX(), component.getY()];
        } else if (component.isPositioned()) {
            result = [component.getLeft(), component.getTop()];
        } else {
            result = this.callParent();
        }
        this.x = result[0];
        this.y = result[1];
        return result;
    }
});
