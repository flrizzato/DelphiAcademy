/**
 * Utility class used by Ext.slider.Slider - should never need to be used directly.
 */
Ext.define('Ext.slider.Thumb', {
    extend: 'Ext.Component',
    xtype : 'thumb',

    baseCls: Ext.baseCSSPrefix + 'thumb',

    isSliderThumb: true,

    config: {
        /**
         * @cfg {String}
         * A CSS class for styling the track fill element.  Assumes {@link #fillTrack} has
         * been set to `true`, otherwise the fill element will be invisible.
         */
        fillCls: null,

        /**
         * @cfg {Boolean/String}
         * True to add a "fill" color to the track for this thumb, or a valid CSS color
         * to use for the fill.  When a fill color is used is extends from the left
         * edge of the thumb to the previous thumb, or to the left edge of the track if
         * this thumb is the left-most thumb.
         */
        fillTrack: null,

        // private

        dragMax: null,
        dragMin: null,
        slider: null
    },

    template: [{
        reference: 'iconElement',
        classList: [
            Ext.baseCSSPrefix + 'icon-el',
            Ext.baseCSSPrefix + 'font-icon'
        ]
    }],

    /**
     * @cfg draggable
     * @inheritdoc
     */
    draggable: {
        local: true,
        constrain: {
            horizontal: true
        },
        listeners: {
            beforedragstart: 'onBeforeDragStart',
            dragstart: 'onDragStart',
            dragmove: 'onDragMove',
            dragend: 'onDragEnd',
            scope: 'this'
        }
    },

    touchAction: { panX: false },

    translatable: {
        // use cssposition instead of csstransform so that themes can use transform
        // scale to style the pressed state of the thumb (material)
        type: 'cssposition',

        listeners: {
            animationstart: 'onAnimationStart',
            animationend: 'onAnimationEnd',
            translate: 'onTranslate',
            scope: 'this'
        }
    },

    elementWidth: 0,

    pressingCls: Ext.baseCSSPrefix + 'pressing',
    fillCls: Ext.baseCSSPrefix + 'fill-el',
    sizerCls: Ext.baseCSSPrefix + 'thumb-sizer',

    constructor: function(config) {
        // Since the thumbs are absolutely positioned, the slider component cannot shrink
        // wrap to their height.  The sizer element is a 0-width element that ensures
        // the slider is at least as high as the largest thumb.
        // This has to be created early so the ui updater can access it
        this.sizerElement = Ext.Element.create({
            cls: this.sizerCls
        });

        this.callParent([config]);
    },

    initialize: function() {
        var me = this,
            fillElement;

        me.callParent();

        me.el.addClsOnClick(me.pressingCls, me.shouldAddPressingCls, me);

        fillElement = me.fillElement = Ext.Element.create({
            cls: me.fillCls
        });

        fillElement.setVisibilityMode(1); // VISIBILITY
    },

    updateFillTrack: function(fillTrack) {
        var fillElement = this.fillElement;

        if (fillTrack === false) {
            fillElement.hide();
        } else {
            fillElement.show();
            fillElement.setStyle('background-color', (typeof fillTrack === 'string') ? fillTrack : '');
        }
    },

    updateFillCls: function(fillCls, oldFillCls) {
        this.fillElement.replaceCls(oldFillCls, fillCls);
    },

    shouldAddPressingCls: function() {
        return !this.isDisabled();
    },

    initDragConstraints: function () {
        // This template method is called JIT to allow us to setup constraints
        if (this.isDisabled()) {
            return false;
        }

        this.getSlider().refreshAllThumbConstraints();
    },

    onAnimationStart: function(translatable, x, y) {
        this.getSlider().onThumbAnimationStart(this, x, y);
    },

    onAnimationEnd: function(translatable, x, y) {
        if (!this.destroyed) {
            this.getSlider().onThumbAnimationEnd(this, x, y);
        }
    },

    onBeforeDragStart: function (source, info, event) {
        if (this.isDisabled()) {
            return false;
        }

        var xy = info.proxy.current;
        this.getSlider().onThumbBeforeDragStart(this, event, xy.x, xy.y);
    },

    onDragStart: function(source, info, event) {
        var xy = info.proxy.current;
        this.getSlider().onThumbDragStart(this, event, xy.x, xy.y);
    },

    onDragMove: function (source, info, event) {
        if (this.isDisabled()) {
            return false;
        }

        var xy = info.proxy.current;
        this.getSlider().onThumbDragMove(this, event, xy.x, xy.y);
    },

    onDragEnd: function (source, info, event) {
        if (this.isDisabled()) {
            return false;
        }

        var xy = info.proxy.current;
        this.getSlider().onThumbDragEnd(this, event, xy.x, xy.y);
    },

    onTranslate: function(translatable, x, y) {
        if (this.initialized) {
            this.getSlider().syncFill(this, x);
        }
    },

    onResize: function (width) {
        var me = this,
            slider = me.ownerCmp;

        me.elementWidth = width;

        if (slider && slider.thumbs && slider.thumbs[0] === me) {
            slider.onThumbResize(me, width);
        }
    },

    getElementWidth: function() {
        return this.elementWidth;
    },

    updateUi: function(ui, oldUi) {
        var me = this,
            sizerCls = me.sizerCls,
            sizerElement = me.sizerElement;

        if (oldUi) {
            sizerElement.removeCls(oldUi, sizerCls);
        }

        if (ui) {
            sizerElement.addCls(ui, sizerCls);
        }

        me.callParent([ui, oldUi]);
    },

    updateDragMax: function (max) {
        var constraint = this.getDraggable().getConstrain(),
            range = constraint.getX();

        constraint.setX([ range && range[0], max ]);
    },

    updateDragMin: function (min) {
        var constraint = this.getDraggable().getConstrain(),
            range = constraint.getX();

        constraint.setX([ min, range && range[1] ]);
    },

    destroy: function() {
        Ext.destroyMembers(this, 'fillElement', 'sizerElement');
        this.callParent();
    }
});
