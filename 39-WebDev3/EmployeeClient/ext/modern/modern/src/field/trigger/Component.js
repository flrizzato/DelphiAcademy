/**
 * A form field trigger that contains a Component
 */
Ext.define('Ext.field.trigger.Component', {
    extend: 'Ext.field.trigger.Base',
    alias: 'trigger.component',
    classCls: Ext.baseCSSPrefix + 'componenttrigger',

    config: {
        /**
         * @cfg {Ext.Component}
         */
        component: null
    },

    doDestroy: function () {
        this.setComponent(null);
        this.callParent();
    },

    applyComponent: function (config, existing) {
        return Ext.updateWidget(existing, config, this, 'createComponent');
    },

    createComponent: function (config) {
        return Ext.apply({
            $initParent: this,
            ownerCmp: this
        }, config);
    },

    updateComponent: function (component) {
        if (component) {
            delete component.$initParent;
            component.ownerCmp = this;

            this.el.appendChild(component.el);
        }
    },

    updateDisabled: function(disabled, oldDisabled) {
        this.callParent([disabled, oldDisabled]);

        this.getComponent().setDisabled(disabled);
    },

    getRefItems: function(deep) {
        var refItems = [],
            component = this.getComponent();

        if (component) {
            refItems.push(component);

            if (deep && component.getRefItems) {
                refItems.push.apply(refItems, component.getRefItems(deep));
            }
        }

        return refItems;
    }
});
