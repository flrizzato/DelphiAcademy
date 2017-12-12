/**
 * Form layout renders a single column of form fields, all with the same
 * {@link #labelWidth label width}.  The default behavior is to size all labels to the
 * width of the label with the longest text, but the width of the labels can also be
 * configured.
 *
 * The following example uses label-auto-widthing to size all labels to the width of the
 * largest label.
 *
 *     @example
 *     Ext.create('Ext.form.Panel', {
 *         fullscreen: true,
 *         layout: 'form',
 *         items: [
 *             {
 *                 xtype: 'textfield',
 *                 label: 'First Name'
 *             },
 *             {
 *                 xtype: 'textfield',
 *                 label: 'Last Name'
 *             },
 *             {
 *                 xtype: 'textfield',
 *                 label: 'Bank Account Number'
 *             },
 *             {
 *                 xtype: 'checkboxfield',
 *                 label: 'Approved'
 *             }
 *         ]
 *     });
 *
 * **Note** This layout can only accept form fields as children and these fields
 * cannot use `top` {@link Ext.field.Text#labelAlign}.
 */
Ext.define('Ext.layout.Form', {
    extend: 'Ext.layout.Auto',
    alias: 'layout.form',
    isFormLayout: true,
    cls: Ext.baseCSSPrefix + 'layout-form',
    itemCls: Ext.baseCSSPrefix + 'layout-form-item',

    config: {
        /**
         * @cfg {Number} itemSpacing
         * The amount of space, in pixels, around each form field. Defaults to the value
         * inherited from the theme's stylesheet as configured by
         * {@link Ext.layout.Form#$layout-form-item-spacing $layout-form-item-spacing}.
         */
        itemSpacing: null,

        /**
         * @cfg {Number/String} [labelWidth='auto']
         * The width of the labels. This can be either a number in pixels, or a valid CSS
         * "width" style, e.g. `'100px'`, or `'30%'`.  When configured, all labels will assume
         * this width, and any {@link Ext.form.Labelable#labelWidth labelWidth} specified
         * on the items will be ignored.
         *
         * The default behavior of this layout when no no labelWidth is specified is to size
         * the labels to the text-width of the label with the longest text.
         */
        labelWidth: null
    },

    hasLabelWidthCls: Ext.baseCSSPrefix + 'has-label-width',

    //<debug>
    onItemAdd: function (item, index) {
        if (item.isInner && !item.isFormField) {
            Ext.raise("Cannot add item to container.  Only Ext.field.Field instances are allowed as inner items in a form layout.");
        }

        this.callParent([item, index]);
    },
    //</debug>

    updateContainer: function(container, oldContainer) {
        var colGroup;

        colGroup = this.columnGroupElement = container.getRenderTarget().appendChild({
            cls: Ext.baseCSSPrefix + 'colgroup-el',
            cn: [{
                cls: Ext.baseCSSPrefix + 'label-column-el'
            }, {
                cls: Ext.baseCSSPrefix + 'body-column-el'
            }]
        });

        this.labelColumnElement = colGroup.first();

        this.callParent([container, oldContainer]);
    },

    updateItemSpacing: function(itemSpacing) {
        this.getContainer().getRenderTarget().setStyle('border-spacing', Ext.Element.addUnits(itemSpacing));
    },

    updateLabelWidth: function(labelWidth) {
        this.labelColumnElement.setWidth(labelWidth);

        this.getContainer().getRenderTarget().toggleCls(
            this.hasLabelWidthCls,
            labelWidth != null && labelWidth !== 'auto'
        );
    }
});