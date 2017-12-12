/**
 * Text Field {@link Ext.field.Text#triggers trigger} widget.
 */
Ext.define('Ext.field.trigger.Trigger', {
    extend: 'Ext.field.trigger.Base',
    xtype: 'trigger',
    alias: 'trigger.trigger',

    requires: [
        'Ext.util.ClickRepeater'
    ],

    config: {
        /**
         * @cfg {Function/String} handler
         * Function to run when trigger is clicked or tapped.
         * @controllable
         */
        handler: null,

        /**
         * @cfg iconCls
         * @inheritdoc Ext.Button#cfg-iconCls
         */
        iconCls: null,

        /**
         * @cfg {Boolean/Object} repeat
         * `true` to attach a {@link Ext.util.ClickRepeater tap repeater} to the trigger,
         * or a config object for a tap repeater.
         */
        repeat: null,

        /**
         * @cfg {Object} scope
         * Execution context for the {@link #handler} function.
         */
        scope: null,

        /**
         * @cfg {Boolean} focusOnTap
         * If `true`, the field will be focused upon tap of the trigger.
         *
         * To show the keyboard, tap the input field while it is focused.
         */
        focusOnTap: true
    },

    interactiveCls: Ext.baseCSSPrefix + 'interactive',

    /**
     * @property template
     * @inheritdoc
     */
    template: [{
        reference: 'iconElement',
        classList: [
            Ext.baseCSSPrefix + 'icon-el',
            Ext.baseCSSPrefix + 'font-icon'
        ]
    }],

    constructor: function(config) {
        var me = this,
            repeat;

        me.callParent([config]);

        repeat = me.getRepeat();

        if (repeat) {
            me.repeater = new Ext.util.ClickRepeater(Ext.apply({
                target: me,
                preventDefault: true,
                listeners: {
                    mousedown: 'onClickRepeaterTouchStart',
                    mouseup: 'onClickRepeaterTouchEnd',
                    click: 'onClickRepeaterClick',
                    scope: me
                }
            }, repeat));
        } else {
            me.element.on({
                click: 'onClick',
                mousedown: 'onMouseDown',
                scope: me
            });
        }
    },

    doDestroy: function() {
        Ext.destroyMembers(this, 'repeater');
        this.callParent();
    },

     onClickRepeaterClick: function(clickRepeater, e) {
        this.onClick(e);
    },

    onClick: function(e) {
        var me = this,
            handler = !me.getDisabled() && me.getHandler(),
            field = me.getField(),
            focusEl;

        if (field) {
            if (e.pointerType !== 'mouse') {
                // Do not allow the default focusing behaviour to follow on *after* the
                // hander has run and this event finishes.
                e.preventDefault();

                if (me.getFocusOnTap()) {
                    focusEl = field.getFocusTrap ? field.getFocusTrap() : field.getFocusEl();

                    if (focusEl.dom !== document.activeElement) {
                        focusEl.focus();
                    }
                }
            }
            if (handler) {
                Ext.callback(handler, me.getScope(), [field, me, e], null, field);
            }
        }
    },

    onMouseDown: function(e) {
        if (e.pointerType === 'mouse') {
            var field = this.getFocusOnTap() && this.getField();

            // Focus the field on mousedown. Touch events do it on tap.
            if (field) {
                field.focus();
            }

            e.preventDefault();
        }
    },

    onClickRepeaterTouchStart: function(clickRepeater, e) {
        this.onMouseDown(e);
    },

    onClickRepeaterTouchEnd: function(clickRepeater, e) {
        var me = this,
            field = me.field;

        Ext.callback(me.endHandler, me.scope, [field, me, e], 0, field);
    },

    updateHandler: function(handler) {
        this.toggleCls(this.interactiveCls, !!handler);
    },

    updateIconCls: function(iconCls, oldIconCls) {
        this.iconElement.replaceCls(oldIconCls, iconCls);
    }
});