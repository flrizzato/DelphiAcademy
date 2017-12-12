/**
 * This container is used to manage the items (such as title and tools) for `Ext.Panel`.
 *
 * @since 6.0.1
 */
Ext.define('Ext.panel.Header', {
    extend: 'Ext.Container',
    xtype: 'panelheader',

    /**
     * @property {Boolean} isPanelHeader
     * `true` in this class to identify an object as an instantiated Header, or a subclass
     * thereof.
     * @readonly
     */
    isPanelHeader: true,

    config: {
        /**
         * @cfg {String} icon
         * Path to an image to use as an icon.
         *
         * For instructions on how you can use icon fonts including those distributed in
         * the SDK see {@link #iconCls}.
         * @accessor
         */
        icon: null,

        /**
         * @cfg {'top'/'right'/'bottom'/'left'} [iconAlign='left']
         * The side of the title to render the icon.
         * @accessor
         */
        iconAlign: null,

        /**
         * @cfg {String} iconCls
         * @accessor
         * One or more space separated CSS classes to be applied to the icon element.
         * The CSS rule(s) applied should specify a background image to be used as the
         * icon.
         *
         * An example of specifying a custom icon class would be something like:
         *
         *     // specify the property in the config for the class:
         *     iconCls: 'my-home-icon'
         *
         *     // css rule specifying the background image to be used as the icon image:
         *     .my-home-icon {
         *         background-image: url(../images/my-home-icon.gif) !important;
         *     }
         *
         * In addition to specifying your own classes, you can use the font icons
         * provided in the SDK using the following syntax:
         *
         *     // using Font Awesome
         *     iconCls: 'x-fa fa-home'
         *
         *     // using Pictos
         *     iconCls: 'pictos pictos-home'
         *
         * Depending on the theme you're using, you may need include the font icon
         * packages in your application in order to use the icons included in the
         * SDK.  For more information see:
         *
         *  - [Font Awesome icons](http://fortawesome.github.io/Font-Awesome/cheatsheet/)
         *  - [Pictos icons](../guides/core_concepts/font_ext.html)
         *  - [Theming Guide](../guides/core_concepts/theming.html)
         */
        iconCls: null,

        /**
         * @cfg {'auto'/'90'/'270'/'0'}
         * The rotation of the {@link #cfg-title}.
         *
         * - `'auto'` - use the default rotation, depending on the {@link Ext.Panel#cfg-headerPosition headerPosition}.
         * - `'0'` - no rotation
         * - `'90'` - rotate 90 degrees clockwise
         * - `'270'` - rotate 270 degrees clockwise
         *
         * The default behavior of this config depends on the {@link Ext.Panel#cfg-headerPosition headerPosition}:
         *
         * - `'top'` or `'bottom'` - `'0'`
         * - `'right'` - `90`
         * - `'left'` - `270`
         */
        titleRotation: 'auto',

        /**
         * @cfg {String/Ext.panel.Title}
         * The title text or config object for the {@link Ext.panel.Title Title} component.
         * @accessor
         */
        title: null,

        /**
         * @cfg {'left'/'center'/'right'} [titleAlign='left']
         * The alignment of the title text within the available space between the
         * icon and the tools.
         * @accessor
         */
        titleAlign: null,

        layout: {
            type: 'box',
            vertical: false,
            align: 'center'
        },

        /**
         * @private
         * Used by the owning panel to inform the header of its position
         */
        position: null
    },

    autoSize: null,

    classCls: Ext.baseCSSPrefix + 'panelheader',
    verticalCls: Ext.baseCSSPrefix + 'vertical',
    horizontalCls: Ext.baseCSSPrefix + 'horizontal',
    toolEndCls: Ext.baseCSSPrefix + 'end',
    toolStartCls: Ext.baseCSSPrefix + 'start',

    rotationMap: {
        top: '0',
        right: '90',
        bottom: '0',
        left: '270'
    },

    dockCls: {
        top: Ext.baseCSSPrefix + 'docked-top',
        right: Ext.baseCSSPrefix + 'docked-right',
        bottom: Ext.baseCSSPrefix + 'docked-bottom',
        left: Ext.baseCSSPrefix + 'docked-left'
    },

    weighted: true,

    vertical: false,

    inheritUi: true,

    addTools: function (tools) {
        var items = Ext.Array.from(tools);

        if (items && items.length) {
            items = this.add(items);
        }

        return items;
    },

    applyTitle: function (newTitle, oldTitle) {
        var title = oldTitle;

        if (title) {
            if (!newTitle || typeof newTitle === 'string') {
                title.setText(newTitle || '');
            } else if (newTitle) {
                title.setConfig(newTitle);
            }
        } else {
            title = Ext.create(this.createTitle(newTitle));
        }

        return title;
    },

    createTitle: function (config) {
        var panel = this.getRefOwner();

        if (config && typeof config === 'string') {
            config = {
                text: config
            };
        }

        return Ext.merge({
            xtype: 'paneltitle',
            instanceCls: (panel && panel.titleCls) || null,
            flex: '1 1 auto'
        }, config);
    },

    onItemAdd: function(item, index) {
        var me = this,
            title = me.getTitle(),
            titleWeight = (title && title.weight) || -10,
            itemWeight = item.weight || 0;

        me.callParent([item, index]);

        if (item.isTool) {
            item.addCls((itemWeight < titleWeight) ? me.toolStartCls : me.toolEndCls);
        }
    },

    onItemRemove: function(item, index, destroying) {
        this.callParent([item, index, destroying]);

        if (item.isTool) {
            item.removeCls([this.toolStartCls, this.toolEndCls]);
        }
    },

    updateIcon: function(icon) {
        this.ensureTitle().setIcon(icon);
    },

    updateIconAlign: function(align) {
        this.ensureTitle().setIconAlign(align);
    },

    updateIconCls: function(cls) {
        this.ensureTitle().setIconCls(cls);
    },

    updateTitle: function(title, oldTitle) {
        if (oldTitle) {
            oldTitle.setConfig(title);
        } else {
            this.add(title);
        }
    },

    updateTitleAlign: function(align) {
        this.ensureTitle().setTextAlign(align);
    },

    updateTitleRotation: function(rotation) {
        var me = this,
            owner;

        if (rotation === 'auto') {
            // The panel will call to indicate the header position, so just drop out
            // now, otherwise we're calling back into the panel init sequence which
            // can cause some issues
            if (me.isConfiguring) {
                return;
            }

            owner = me.getRefOwner();
            //<debug>
            if (!owner) {
                Ext.raise('Cannot use rotation auto without an owning panel.');
            }
            //</debug>
            if (owner) {
                rotation = me.rotationMap[owner.getHeaderPosition()];
            }
        }
        me.rotateTitle(rotation);
    },

    updatePosition: function(position, oldPosition) {
        var me = this,
            layout = me.getLayout(),
            isLeft = (position === 'left'),
            isRight = (position === 'right'),
            vertical = me.vertical = (isLeft || isRight),
            verticalCls = me.verticalCls,
            horizontalCls = me.horizontalCls,
            dockCls = me.dockCls;

        layout.setVertical(vertical);
        layout.setReverse(isLeft);

        // The header is not a true docked item, but it must have the x-docked-[side]
        // css cls so that it can participate in border management
        if (oldPosition) {
            me.removeCls(dockCls[oldPosition]);
        }

        if (position) {
            me.addCls(dockCls[position]);
        }

        if (vertical) {
            me.replaceCls(horizontalCls, verticalCls);
        } else {
            me.replaceCls(verticalCls, horizontalCls);
        }

        if (me.getTitleRotation() === 'auto') {
            me.rotateTitle(me.rotationMap[position]);
        }
    },

    privates: {
        clearTools: function () {
            var items = this.getItems().items,
                c, i;

            for (i = items.length; i-- > 0; ) {
                c = items[i];

                if (c.isTool && !c.$internal) {
                    this.remove(c);
                }
            }
        },

        ensureTitle: function () {
            var me = this,
                title = me.getTitle();

            if (!title) {
                me.setTitle('');
                title = me.getTitle();
            }

            return title;
        },

        isVertical: function() {
            return this.vertical;
        },

        rotateTitle: function(rotation) {
            this.ensureTitle().setRotation(rotation);
        },

        sortByWeight: function (item1, item2) {
            return (item1.weight || 0) - (item2.weight || 0);
        }
    },

    deprecated: {
        '6.5': {
            configs: {
                /**
                 * @cfg {Number/String} glyph
                 * @removed 6.5.0 Use {@link #iconCls} instead
                 */
                glyph: null
            }
        }
    }
});
