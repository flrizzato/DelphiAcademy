/**
 * A basic title component for a Panel Header.
 *
 * @since 6.0.1
 */
Ext.define('Ext.panel.Title', {
    extend: 'Ext.Component',
    xtype: 'paneltitle',

    isPanelTitle: true,

    cachedConfig: {
        /**
         * @cfg textAlign
         * @inheritdoc Ext.panel.Header#cfg-titleAlign
         * @accessor
         */
        textAlign: 'left',

        /**
         * @cfg iconAlign
         * @inheritdoc Ext.panel.Header#cfg-iconAlign
         * @accessor
         */
        iconAlign: 'left',

        /**
         * @cfg {'90'/'270'/'0'} rotation
         * The rotation of the {@link #cfg-text}.
         *
         * - `'0'` - no rotation
         * - `'90'` - rotate 90 degrees clockwise
         * - `'270'` - rotate 270 degrees clockwise
         */
        rotation: 0,

        /**
         * @cfg {Boolean} [rotateIcon=false]
         * `true to rotate the icon in the same direction as the text when the text is rotated
         * By default the icon always remains unrotated even when the text is rotated.
         */
        rotateIcon: null
    },

    config: {
        /**
         * @cfg {String} text
         * The title's text (can contain html tags/entities)
         * @accessor
         */
        text: '',

        /**
         * @cfg icon
         * @inheritdoc Ext.panel.Header#cfg-icon
         * @accessor
         */
        icon: null,

        /**
         * @cfg iconCls
         * @inheritdoc Ext.panel.Header#cfg-iconCls
         * @accessor
         */
        iconCls: null
    },

    /**
     * @cfg weight
     * @inheritdoc
     */
    weight: -10,

    /**
     * @property inheritUi
     * @inheritdoc
     */
    inheritUi: true,

    /**
     * @property element
     * @inheritdoc
     */
    element: {
        reference: 'element',
        cls: Ext.baseCSSPrefix + 'unselectable'
    },

    /**
     * @property template
     * @inheritdoc
     */
    template: [{
        reference: 'bodyElement',
        cls: Ext.baseCSSPrefix + 'body-el',


        children: [{
            reference: 'iconElement',
            cls: Ext.baseCSSPrefix + 'icon-el ' + Ext.baseCSSPrefix + 'font-icon'
        }, {
            reference: 'textElement',
            cls: Ext.baseCSSPrefix + 'text-el'
        }]
    }],

    verticalCls: Ext.baseCSSPrefix + 'vertical',
    horizontalCls: Ext.baseCSSPrefix + 'horizontal',
    rotateIconCls: Ext.baseCSSPrefix + 'rotate-icon',
    iconAlignVerticalCls: Ext.baseCSSPrefix + 'icon-align-vertical',
    hasIconCls: Ext.baseCSSPrefix + 'has-icon',

    _textAlignClasses: {
        left: Ext.baseCSSPrefix + 'text-align-left',
        center: Ext.baseCSSPrefix + 'text-align-center',
        right: Ext.baseCSSPrefix + 'text-align-right'
    },

    _iconAlignClasses: {
        top: Ext.baseCSSPrefix + 'icon-align-top',
        right: Ext.baseCSSPrefix + 'icon-align-right',
        bottom: Ext.baseCSSPrefix + 'icon-align-bottom',
        left: Ext.baseCSSPrefix + 'icon-align-left'
    },

    _rotationClasses: {
        90: Ext.baseCSSPrefix + 'rotate-90',
        270: Ext.baseCSSPrefix + 'rotate-270'
    },

    /**
     * @property classCls
     * @inheritdoc
     */
    classCls: Ext.baseCSSPrefix + 'paneltitle',
    _titleSuffix: '-title',

    afterRender: function() {
        if (Ext.isSafari) {
            this.repaintBodyElement();
        }

        this.callParent();
    },

    updateIcon: function(icon, oldIcon) {
        var me = this,
            iconEl;

        me.syncIconVisibility();
        iconEl = me.iconElement;

        iconEl.setStyle('background-image', icon ? 'url(' + icon + ')': '');
    },

    updateIconAlign: function(align, oldAlign) {
        var me = this,
            iconAlignClasses = me._iconAlignClasses,
            el = me.el;

        if (oldAlign) {
            el.removeCls(iconAlignClasses[oldAlign]);
        }

        if (align) {
            el.addCls(iconAlignClasses[align]);
        }

        el.toggleCls(me.iconAlignVerticalCls, align === 'top' || align === 'bottom');
    },

    updateIconCls: function(cls, oldCls) {
        var iconEl = this.iconElement;

        this.syncIconVisibility();

        if (oldCls) {
            iconEl.removeCls(oldCls);
        }

        if (cls) {
            iconEl.addCls(cls);
        }
    },

    updateRotation: function(rotation, oldRotation) {
        var me = this,
            verticalCls = me.verticalCls,
            horizontalCls = me.horizontalCls,
            el = me.el;

        if (oldRotation != 0) {
            el.removeCls(me._rotationClasses[oldRotation]);
        }
        
        if (rotation == 0) {
            el.replaceCls(verticalCls, horizontalCls);
        } else {
            el.replaceCls(horizontalCls, [verticalCls, me._rotationClasses[rotation]]);
        }

        if (Ext.isSafari && this.rendered) {
            this.repaintBodyElement();
        }
    },

    updateRotateIcon: function(rotateIcon) {
        this.el.toggleCls(this.rotateIconCls, !!rotateIcon);
    },

    updateText: function(text) {
        var el = this.textElement.dom;
        el.innerHTML = text || '&#160;';
        el.setAttribute('data-title', text);
    },

    updateTextAlign: function(align, oldAlign) {
        var me = this,
            textAlignClasses = me._textAlignClasses;

        if (oldAlign) {
            me.removeCls(textAlignClasses[oldAlign]);
        }

        if (align) {
            me.addCls(textAlignClasses[align]);
        }
    },

    privates: {
        repaintBodyElement: function() {
            var bodyElement = this.bodyElement.dom,
                bodyStyle = bodyElement.style;

            // When iconAlign is 'top' or 'bottom' with vertically rotated text, Safari
            // does not initially layout the title with the correct width.  Setting the
            // width to -webkit-min-content and back to '' with a read of offsetWidth in
            // between forces a synchronous reflow and corrects the issue.  Unfortunately
            // a static width or min-width in the stylesheet does not help.
            // We use -webkit-min-content so that the next reflow after resetting the
            // min-width to '' hopefully ends up with everything the same size as before
            // thus minimizing the effect on the surrounding dom.
            bodyStyle.width = '-webkit-min-content';
            bodyElement.offsetWidth;
            bodyStyle.width = '';
        },
        
        syncIconVisibility: function() {
            this.el.toggleCls(this.hasIconCls, !!(this.getIcon() || this.getIconCls()));
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
