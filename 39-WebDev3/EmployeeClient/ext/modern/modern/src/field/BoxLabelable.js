/**
 * @private
 * A mixin that adds boxLabel capabilities to Checkbox and Slider fields
 */
Ext.define('Ext.field.BoxLabelable', {
    extend: 'Ext.Mixin',
    mixinConfig: {
        id: 'boxLabelable',
        after: {
            initElement: 'afterInitElement'
        }
    },

    config: {
        /**
         * @cfg {String}
         * An optional label that will appear next to the input within the field body area.
         * Displays immediately before or after the input depending on {@link #boxLabelAlign}
         */
        boxLabel: null,

        /**
         * @cfg {'before'/'after'} [boxLabelAlign='after']
         * The position relative to the input where the {@link #boxLabel} should appear
         */
        boxLabelAlign: null
    },

    boxLabeledCls: Ext.baseCSSPrefix + 'box-labeled',

    getBodyTemplate: function () {
        return [{
            reference: 'boxWrapElement',
            cls: Ext.baseCSSPrefix + 'box-wrap-el',
            children: [{
                reference: 'boxElement',
                cls: Ext.baseCSSPrefix + 'box-el',
                children: this.getBoxTemplate()
            }, {
                tag: 'label',
                reference: 'boxLabelElement',
                cls: Ext.baseCSSPrefix + 'box-label-el'
            }]
        }]
    },

    getBoxTemplate: Ext.emptyFn,

    updateBoxLabel: function (boxLabel) {
        this.boxLabelElement.setHtml(boxLabel);
        this.el.toggleCls(this.boxLabeledCls, !!boxLabel);
    },

    updateBoxLabelAlign: function (boxLabelAlign, oldBoxLabelAlign) {
        var me = this,
            el = me.el;

        if (oldBoxLabelAlign) {
            el.removeCls(Ext.baseCSSPrefix + 'box-label-align-' + oldBoxLabelAlign);
        }

        if (boxLabelAlign) {
            el.addCls(Ext.baseCSSPrefix + 'box-label-align-' + boxLabelAlign);
        }
    },

    afterInitElement: function() {
        var inputElement = this.inputElement;

        if (inputElement) {
            this.boxLabelElement.dom.setAttribute('for', inputElement.id);
        }
    }

});