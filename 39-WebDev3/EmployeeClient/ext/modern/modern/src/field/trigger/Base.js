/**
 * Base class for form field triggers
 * @private
 */
Ext.define('Ext.field.trigger.Base', {
    extend: 'Ext.Widget',
    alias: 'trigger.base',

    mixins: [
        'Ext.mixin.Factoryable'
    ],

    factoryConfig: {
        defaultType: 'trigger',
        aliasPrefix: 'trigger.'
    },

    isTrigger: true,

    config: {
        /**
         * @cfg {Ext.field.Text}
         * The text field that created this trigger
         * @private
         */
        field: null,

        /**
         * @cfg {String} [group]
         * The name of an optional group trigger that this trigger belongs to.  If no trigger
         * Exists by that name one will automatically be created.  A group trigger is a
         * special trigger that contains other triggers.  Those triggers' elements are
         * appended to the group trigger's element in the DOM.
         *
         * The {@link #weight} of grouped triggers is relative to other triggers in the group.
         */
        group: null,

        /**
         * @cfg {'left'/'right'} [side='right']
         * The side of the text field's input to render the trigger on.
         */
        side: null,

        /**
         * @cfg {String}
         * The key used to identify this trigger in the text field's triggers config.
         * @private
         */
        name: null,

        /**
         * The triggers contained in this trigger (only applicable for trigger groups)
         * @private
         */
        triggers: null
    },

    classCls: Ext.baseCSSPrefix + 'trigger',
    groupedCls: Ext.baseCSSPrefix + 'grouped',

    inheritUi: true,

    statics: {
        /**
         * Sorts an array of triggers in place by weight
         * @param {Ext.field.Trigger[]} triggers
         * @return {Ext.field.Trigger[]}
         * @private
         * @static
         */
        sort: function (triggers) {
            Ext.Array.sort(triggers, Ext.weightSortFn);
            return triggers;
        }
    },

    doDestroy: function() {
        var triggers = this.getTriggers(),
            i, ln;

        if (triggers) {
            for (i = 0, ln = triggers.length; i < ln; i++) {
                triggers[i].destroy();
            }
        }

        this.setTriggers(null);

        this.callParent();
    },

    updateField: function (field) {
        // All Components MUST have an upward link through either parent or ownerCmp
        this.field = this.ownerCmp = field;

        this.doInheritUi();
    },


    updateGroup: function (group) {
        if (!this.isConfiguring) {
            this.getField().syncTriggers();
        }
    },

    updateSide: function () {
        if (!this.isConfiguring) {
            this.getField().syncTriggers();
        }
    },

    updateTriggers: function (triggers) {
        var me = this,
            dom = me.element.dom,
            iconElement = me.iconElement,
            i, ln;

        me.toggleCls(me.groupedCls, !!(triggers && triggers.length));

        if (triggers) {
            for (i = 0, ln = triggers.length; i < ln; i++) {
                dom.appendChild(triggers[i].element.dom);
            }
        }
    }
});