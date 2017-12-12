/**
 * A floated panel which animates in and out from the side of the screen when shown.
 * Used as the base class for {@link Ext.ActionSheet Action Sheets} and
 * {@link Ext.picker.Picker Pickers}
 */
Ext.define('Ext.Sheet', {
    extend: 'Ext.Panel',

    xtype: 'sheet',

    requires: [
        'Ext.viewport.Viewport',
        'Ext.Mask',
        'Ext.fx.Animation'
    ],

    /**
     * @hide
     */
    isViewportMenu: false,

    /**
     * 
     * @hide
     */
    hidden: true,

    config: {
        /**
         * @cfg {Boolean} reveal
         * Set to true to display the menu using reveal style. The Viewport will slide up,
         * down, left or right to make room for the menu to be seen.
         */
        reveal: null,

        /**
         * @cfg {Boolean} cover
         * Set to true to display the menu using cover style. The menu will be shown over
         * the Viewport from the specified side.  By default, the menu will be modal,
         * displaying a mask over the rest of the Viewport, and the user may tap on the
         * mask to dismiss the menu.
         */
        cover: null,

        /**
         * @cfg {"left"/"right"/"top"/"bottom"}
         * The side of the viewport where the menu will be positioned.
         */
        side: null,

        /**
         * @cfg {Boolean} stretchX `true` to stretch this sheet horizontally.
         */
        stretchX: null,

        /**
         * @cfg {Boolean} stretchY `true` to stretch this sheet vertically.
         */
        stretchY: null,

        /**
         * @cfg {'top'/'bottom'/'left'/'right'} enter
         * The viewport side used as the enter point when shown.
         * Applies to sliding animation effects only.
         */
        enter: 'bottom',

        /**
         * @cfg {'top'/'bottom'/'left'/'right'} exit
         * The viewport side used as the exit point when hidden.
         * Applies to sliding animation effects only.
         */
        exit: 'bottom'
    },

    translatable: {
        type: 'csstransform'
    },
    showAnimation: {
        type: 'slideIn',
        duration: 250,
        easing: 'ease-out'
    },
    hideAnimation: {
        type: 'slideOut',
        duration: 250,
        easing: 'ease-in'
    },
    modal: true,
    hideOnMaskTap: true,

    /**
     * @cfg {Boolean} centered
     * Whether or not this component is absolutely centered inside its container.
     * @accessor
     * @evented
     */
    centered: true,

    classCls: Ext.baseCSSPrefix + 'sheet',

    manageBorders: false,

    autoSize: null,
    border: true,
    bodyBorder: false,

    floated: true,

    isInputRegex: /^(input|textarea|select|a)$/i,

    destroy: function() {
        var me = this;

        me.setSide(null);

        me.callParent();
    },

    applyHideAnimation: function(config) {
        var exit = this.getExit(),
            direction = exit;

        if (exit === null) {
            return null;
        }

        if (config === true) {
            config = {
                type: 'slideOut'
            };
        }
        var anim = this.callParent([config]);

        if (anim) {
            if (exit === 'bottom') {
                direction = 'down';
            } else if (exit === 'top') {
                direction = 'up';
            }
            anim.setDirection(direction);
        }
        return anim;
    },

    applyShowAnimation: function(config) {
        var enter = this.getEnter(),
            direction = enter;

        if (enter === null) {
            return null;
        }

        if (config === true) {
            config = {
                type: 'slideIn'
            };
        }
        var anim = this.callParent([config]);

        if (anim) {
            if (enter === 'bottom') {
                direction = 'down';
            }
            if (enter === 'top') {
                direction = 'up';
            }
            anim.setBefore({
                display: null
            });
            anim.setReverse(true);
            anim.setDirection(direction);
        }
        return anim;
    },

    hide: function(animation) {
        var side = this.getSide();

        if (side) {
            Ext.Viewport.hideMenu(side, animation);
        } else {
            this.callParent([animation]);
        }
    },

    show: function(animation, options) {
        var me = this,
            VP = Ext.Viewport,

            // Allow side: null to bypass the Viewport menu show to do a default show operation
            side = options && ('side' in options) ? options.side : me.getSide();

        if (side) {
            VP.setMenu(me);
            VP.showMenu(side);
        } else {
            me.callParent([animation, options]);
        }
    },

    updateSide: function(newSide, oldSide) {
        var me = this,
            reShow = !me.isConfiguring && me.isVisible();

        me.isViewportMenu = !!newSide;

        if (oldSide) {
            Ext.Viewport.removeMenu(oldSide);
        }

        if (newSide) {
            Ext.Viewport.setMenu(me, {
                side: newSide
            });
            
            // We're flipping sides while shown.
            if (reShow) {
                me.show();
            }
        }
    },

    updateStretchX: function(newStretchX) {
        this.getLeft();
        this.getRight();

        if (newStretchX) {
            this.setLeft(0);
            this.setRight(0);
        }
    },

    updateStretchY: function(newStretchY) {
        this.getTop();
        this.getBottom();

        if (newStretchY) {
            this.setTop(0);
            this.setBottom(0);
        }
    }
});
