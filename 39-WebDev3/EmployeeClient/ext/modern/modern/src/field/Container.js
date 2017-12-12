/**
 * This Container Field is useful for containing multiple form fields
 * to be a single field within a form and will line up nicely with other
 * fields. This allows it to be configured with a {@link #label field label}
 * and optional {@link #errorTarget error message} around it's sub-items.
 * A common use is for grouping a set of related fields under a single label
 * in a form.
 *
 * The container's configured {@link #cfg-items} will be layed out within
 * the field body area according to the configured {@link #layout} type.
 * The default layout is `'hbox'`.
 *
 * # Example
 *
 *     @example
 *     Ext.create('Ext.form.Panel', {
 *         fullscreen: true,
 *         title: 'FieldContainer Example',
 *         bodyPadding: 10,
 *
 *         items: [{
 *             xtype: 'containerfield',
 *             label: 'Name',
 *             name: 'name',
 *             items: [{
 *                 flex: 1,
 *                 name: 'first',
 *                 placeholder: 'First'
 *             }, {
 *                 margin: '0 10',
 *                 name: 'middle',
 *                 placeholder: 'MI',
 *                 width: 50
 *             }, {
 *                 flex: 1,
 *                 name: 'last',
 *                 placeholder: 'Last'
 *             }]
 *         }]
 *     });
 */
Ext.define('Ext.field.Container', {
    extend: 'Ext.field.Field',
    xtype: [
        'containerfield',
        'fieldcontainer' //classic compat
    ],

    mixins: [
        'Ext.field.Manager',
        'Ext.mixin.ConfigProxy'
    ],

    /**
     * @cfg error
     * @hide
     */

    config: {
        /**
         * @cfg {Ext.Container} container
         * The configuration of the container used to maintain child items.
         */
        container: {
            xtype: 'container',
            autoSize: null,
            defaultType: 'textfield',
            defaults: {
                errorTarget: 'parent'
            },
            layout: {
                type: 'hbox'
            }
        }
    },

    proxyConfig: {
        container: {
            configs: [
                /**
                 * @cfg defaults
                 * @inheritdoc Ext.Container#defaults
                 */
                'defaults',

                /**
                 * @cfg defaultType
                 * @inheritdoc Ext.Container#defaultType
                 */
                'defaultType',

                /**
                 * @cfg items
                 * @inheritdoc Ext.Container#items
                 */
                'items',

                /**
                 * @cfg layout
                 * @inheritdoc Ext.Container#layout
                 */
                'layout',

                /**
                 * @cfg autoSize
                 * @inheritdoc Ext.Container#
                 */
                'autoSize'
            ],
            methods: [
                /**
                 * @method add
                 * @inheritdoc Ext.Container#add
                 */
                'add',

                /**
                 * @method insert
                 * @inheritdoc Ext.Container#insert
                 */
                'insert',

                /**
                 * @method remove
                 * @inheritdoc Ext.Container#remove
                 */
                'remove',

                /**
                 * @method removeAll
                 * @inheritdoc Ext.Container#removeAll
                 */
                'removeAll',

                /**
                 * @method getAt
                 * @inheritdoc Ext.Container#getAt
                 */
                'getAt',

                /**
                 * @method child
                 * @inheritdoc Ext.Container#child
                 */
                'child',

                /**
                 * @method down
                 * @inheritdoc Ext.Container#down
                 */
                'down',

                /**
                 * @method query
                 * @inheritdoc Ext.Container#query
                 */
                'query'
            ]
        }
    },

    bodyAlign: 'stretch',

    classCls: Ext.baseCSSPrefix + 'containerfield',

    isField: false,
    isContainer: true,
    isContainerField: true,

    errorTpl: '<tpl if="count == 1">' +
        '<tpl for="errors">{label:htmlEncode}: {error:htmlEncode}</tpl>' +
    '<tpl elseif="count">' +
        '<ul class="{listCls}">' +
            '<tpl for="errors">' +
                '<li>{label:htmlEncode}: {error:htmlEncode}</li>' +
            '</tpl>' +
        '</ul>' +
    '</tpl>',

    doDestroy: function () {
        this.setContainer(null);

        this.callParent();
    },

    applyContainer: function (container, oldContainer) {
        if (container) {
            if (!container.isInstance) {
                container = this.mergeProxiedConfigs('container', container);
                container.$initParent = this;
                container = Ext.create(container);
                delete container.$initParent;
            }

            container.ownerCmp = this;
        }

        return container;
    },

    updateContainer: function (container, oldContainer) {
        var bodyElement = this.bodyElement;

        if (oldContainer) {
            bodyElement.removeChild(oldContainer.el);

            oldContainer.destroy();
        }

        if (container) {
            bodyElement.appendChild(container.el);
        }
    },

    updateRecord: function(record) {
        this.consumeRecord(record);
    },

    onFieldErrorChange: function (field) {
        var me = this,
            errors = me.getErrors(),
            fields = me.getFields(),
            name, fieldErrors, label, messages;

        for (name in errors) {
            field = fields[name];
            fieldErrors = errors[name];

            if (fieldErrors) {
                label = field.getLabel() || field.getPlaceholder() || field.getName();
                fieldErrors = Ext.Array
                    .from(fieldErrors)
                    .map(function (error) {
                        return {
                            label: label,
                            error: error
                        };
                    });

                if (messages) {
                    messages = messages.concat(fieldErrors);
                } else {
                    messages = fieldErrors;
                }
            }
        }

        me.setError(messages || null);
    },

    /**
     * @private
     * Used by {@link Ext.ComponentQuery} to find all items
     * which can potentially be considered a child of this container.
     * @param {Boolean} deep `true` to find grandchildren of child containers.
     */
    getRefItems: function (deep) {
        var refItems = [],
            container = this.getContainer();

        if (container) {
            refItems.push(container);

            if (deep && container.getRefItems) {
                refItems.push.apply(refItems, container.getRefItems(deep));
            }
        }

        return refItems;
    },

    /**
     * @localdoc Finds the first form field that can be focused.
     */
    getFocusEl: function () {
        var items = this.getFields(false),
            length = items && items.length,
            i, item, focusEl;

        for (i = 0; i < length; i++) {
            item = items[i];
            focusEl = item.getFocusEl();

            if (focusEl) {
                return focusEl;
            }
        }

        return this.callParent();
    },

    reset: function (clearInvalid) {
        if (clearInvalid) {
            this.setError(null);
        }

        return this.mixins.fieldmanager.reset.call(this, clearInvalid);
    },

    /**
     * Marks each field invalid based on the messages passed. The object will
     * be iterated over and a child field is found based on the key matching
     * a field's name. The field will then be marked invalid with that message.
     * If the value is falsey, that field will be not be marked invalid. Example:
     *
     *     @example
     *     var form = Ext.create('Ext.form.Panel', {
     *         fullscreen: true,
     *         title: 'FieldContainer Example',
     *         bodyPadding: 10,
     *
     *         items: [{
     *             xtype: 'containerfield',
     *             label: 'Name',
     *             name: 'name',
     *             items: [{
     *                 flex: 1,
     *                 name: 'first',
     *                 placeholder: 'First',
     *                 required: true
     *             }, {
     *                 margin: '0 10',
     *                 name: 'middle',
     *                 placeholder: 'MI',
     *                 width: 50
     *             }, {
     *                 flex: 1,
     *                 name: 'last',
     *                 placeholder: 'Last',
     *                 required: true
     *             }]
     *         }]
     *     });
     *
     *     var fieldcontainer = form.child('containerfield');
     *
     *     fieldcontainer.setErrors({
     *         first: 'First name is required',
     *         last: 'Last name is required',
     *         middle: null                      //clears any errors on the field
     *     });
     *
     * @param {Object} errors The errors to set child fields with.
     * @return {Ext.field.Container} this
     */
    setErrors: function (errors) {
        var me = this,
            fields = me.getFields(),
            fieldname, field, messages;

        //<debug>
        if (!Ext.isObject(errors)) {
            Ext.raise('setErrors requires an Object parameter');
        }
        //</debug>

        me.setError(null);

        for (fieldname in errors) {
            field = fields[fieldname];

            if (field) {
                messages = errors[fieldname];

                if (messages == null || (Ext.isArray(messages) && messages.length === 0)) {
                    field.setError(null);
                } else {
                    field.setError(Ext.Array.from(messages));
                }
            }
        }

        return me;
    },

    isValid: function () {
        return this.mixins.fieldmanager.isValid.call(this);
    },

    validate: function (skiplazy) {
        return this.mixins.fieldmanager.validate.call(this, skiplazy);
    },

    getFields: function (byName, deep) {
        if (deep == null) {
            //if not passed in, default to false
            deep = false;
        }

        return this.mixins.fieldmanager.getFields.call(this, byName, deep);
    }
});
