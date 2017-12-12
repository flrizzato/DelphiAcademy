/**
 * This class is a container used by the {@link Ext.dataview.plugin.ListSwiper listswiper}
 * plugin to display information and controls when an item is swiped.
 */
Ext.define('Ext.dataview.listswiper.Stepper', {
    extend: 'Ext.dataview.listswiper.Item',
    xtype: 'listswiperstepper',

    requires: [
        'Ext.fx.easing.EaseOut',
        'Ext.util.translatable.CssTransform'
    ],

    config: {
        /**
         * @cfg {String} iconCls
         * One or more space separated CSS classes to be applied to the icon element.
         * See {@link Ext.Button#iconCls} for details.
         */
        iconCls: null,

        /**
         * @cfg {String} text
         * The swipe action text.
         */
        text: null,

        /**
         * @cfg {Object} undo
         * A config object for the undo button.
         */
        undo: {
            docked: 'right',
            ui: 'listswiperstepper-trigger'
        },

        /**
         * @private
         */
        step: null,

        /**
         * @private
         */
        side: null,

        /**
         * @cfg {Boolean/Object} animation
         * `true` for the default animation (`{ duration: 500, easing: 'ease-out' }`) or
         * a standard animation config object to be used for default swipe animations.
         */
        animation: true
    },

    classCls: Ext.baseCSSPrefix + 'listswiperstepper',

    layout: {
        type: 'hbox',
        align: 'center'
    },

    scrollDock: null,

    sideCls: {
        left: Ext.baseCSSPrefix + 'side-left',
        right: Ext.baseCSSPrefix + 'side-right'
    },

    tpl: '<div class="' + Ext.baseCSSPrefix + 'listswiperstepper-text">{text}</div>',

    template: [{
        reference: 'bodyElement',
        cls: Ext.baseCSSPrefix + 'body-el',
        uiCls: 'body-el',
        children: [{
            reference: 'iconWrapElement',
            cls: Ext.baseCSSPrefix + 'icon-wrap-el',
            uiCls: 'icon-wrap-el',
            children: [{
                reference: 'iconElement',
                cls: Ext.baseCSSPrefix + 'icon-el ' + Ext.baseCSSPrefix + 'font-icon'
            }]
        }, {
            reference: 'innerElement',
            cls: Ext.baseCSSPrefix + 'inner-el',
            uiCls: 'inner-el'
        }]
    }],

    initialize: function () {
        this.callParent();
        this.bodyElement.on('tap', 'onTap', this);
    },

    onRender: function() {
        this.steps = this.buildSteps();
    },

    applyAnimation: function(animation) {
        if (animation === true) {
            animation = {
                duration: 500,
                easing: {
                    type: 'ease-out'
                }
            };
        }

        return animation;
    },

    updateTranslationTarget: function (target) {
        this.translatable = Ext.Factory.translatable({ element: target }, 'csstransform');
    },

    revert: function(animate) {
        var me = this,
            action = me.getAction();

        me.invokeAction(action, 'revert');
        me.finalize(animate);
    },

    /**
     * Dismisses the pending action by triggering the `dismiss` event.
     * See {@link Ext.dataview.plugin.ListSwiper#actionOnDismiss} for details.
     */
    dismiss: function(animate) {
        var me = this,
            action = me.getAction(),
            state = me.getState();

        if (state === 'undo') {
            me.invokeAction(action, 'commit');
        }

        me.finalize(animate);
    },

    sortFn: function(a, b) {
        return b.x - a.x;
    },

    /**
     * Builds a lookup tables with effective thresholds (in pixel) to save some calculations
     * during the multiple drag events. This tables should be invalidated every time the list
     * is horizontally resized (which should not happen during swipe interactions).
     * @private
     */
    buildSteps: function() {
        var me = this,
            item = me.ownerCmp,
            el = item.el,
            left = me.getLeftActions() || {},
            right = me.getRightActions() || {},
            width = el.getWidth(),
            steps = {r: [], l: []},
            totalThreshold = 0,
            fn = function(side, index, action) {
                var threshold = Ext.util.Format.defaultValue(action.threshold, '25%'),
                    number = parseInt(threshold, 10);

                if (isNaN(number)) {
                    return; // skip this action!
                }

                if (typeof threshold === 'string' && threshold.indexOf('%') !== -1) {
                    number = width * number / 100;
                }

                totalThreshold += number;

                steps[side].push({
                    action: action,
                    side: side === 'r' ? 'right' : 'left',
                    tx: side === 'r' ? -width : width,
                    x: totalThreshold,
                    key: action.key || index
                });
            };

        Ext.Object.each(left, fn.bind(this, 'l'));
        totalThreshold = 0;
        Ext.Object.each(right, fn.bind(this, 'r'));

        return steps;
    },

    findStep: function(dx, force) {
        var me = this,
            res = {step: null, active: true},
            steps = me.steps[dx > 0 ? 'l' : 'r'],
            ilen = steps.length,
            absDx = Math.abs(dx),
            step, i;

        for (i = ilen - 1; !res.step && i >= 0; --i) {
            step = steps[i];
            if (step.x < absDx) {
                res.step = step;
            }
        }

        if (!res.step && force && ilen > 0) {
            res.step = steps[0];
            res.active = false;
        }

        return res;
    },

    updateStep: function(step, oldStep) {
        var me = this,
            action = (step && step.action),
            oldAction = (oldStep && oldStep.action),
            actionCls = action && action.cls,
            oldActionCls = oldAction && oldAction.cls,
            actionKeyCls = step && ('swipe-action-' + step.key),
            oldActionKeyCls = oldStep && ('swipe-action-' + oldStep.key);

        if (step) {
            me.setSide(step.side);
        }

        me.replaceCls(oldActionCls, actionCls);
        me.replaceCls(oldActionKeyCls, actionKeyCls, Ext.baseCSSPrefix);

        me.syncStep();
    },

    updateSide: function(side, oldSide) {
        var me = this,
            classes = me.sideCls,
            layout = me.getLayout();

        me.replaceCls(classes[oldSide], classes[side]);
        if (layout.setPack) {
            layout.setPack(side === 'right'? 'end' : 'start');
        }
    },

    onDragStart: function(evt) {
        evt.stopPropagation();
    },

    onDragMove: function(evt) {
        var me = this,
            plugin = me.owner,
            directionLock = plugin.getDirectionLock(),
            state = me.getState(),
            step = me.getStep(),
            translatable = me.translatable,
            dx = evt.deltaX, res;

        if (state === 'undo') {
            return;
        }

        if (state === 'consumed') {
            me.setState('reaquired');
            translatable.stopAnimation();
        }

        res = me.findStep(dx, true);

        // Direction Lock causes a friction pull
        if (directionLock && (res.step && step) && (res.step.side !== step.side)) {
            me.setState('overdrag');
            res.step = null;
        } else {
            me.setState(res.step ? res.active ? 'active' : 'peek' : 'overdrag');
            me.setStep(res.step || null);
        }

        // if there is no action for the current gesture, we still want to allow the user
        // to swipe the item but with friction to let him know that no action is available.
        translatable.translateAxis('x', res.step ? dx : dx * 0.1);

        evt.stopPropagation();
    },

    onDragEnd: function(evt) {
        var me = this,
            state = me.getState(),
            step = me.getStep(),
            dx = evt.deltaX,
            res;

        if (state === 'undo' || state === 'consumed') {
            return;
        }

        evt.stopPropagation();

        res = me.findStep(dx, false);
        if (!res.step || res.step.side !== step.side) {
            me.finalize(true);
            return;
        }

        me.setStep(res.step);
        me.commit(true);
    },


    commit: function(animate) {
        var me = this,
            step = me.getStep(),
            action = step.action,
            plugin = me.owner,
            translatable = me.translatable,
            delay, precommitResult, undo;

        me.setAction(action);
        precommitResult = me.invokeAction(action, 'precommit');

        if (action.undoable) {
            me.setState('undo');
            undo = me.add(me.getUndo());
            undo.setHandler(me.onUndoTap.bind(me));
            me.setSide(undo.getDocked() === 'left'? 'right' : 'left');

            translatable.translateAxis('x', step.tx, me.getAnimation());

            delay = plugin.getCommitDelay();
            if (delay) {
                if (precommitResult && precommitResult.then) {
                    precommitResult.then(function() {
                        plugin.dismissAllTask.delay(delay);
                    });
                } else {
                    plugin.dismissAllTask.delay(delay);
                }
            }
        } else {
            if (precommitResult && precommitResult.then) {
                precommitResult.then(me.invokeAction.bind(me, action, 'commit')).then(me.finalize.bind(me, animate));
            } else {
                me.invokeAction(action, 'commit');
                me.finalize(animate);
            }
        }
    },

    finalize: function(animate) {
        var me = this,
            animation = me.getAnimation(),
            translatable = me.translatable;

        translatable.stopAnimation();
        me.setState('consumed');

        if (!animate) {
            me.doFinalize();
            return;
        }

        if (translatable.x !== 0) {
            translatable.on({
                animationend: 'doFinalize',
                single: true,
                scope: me
            });

            translatable.translateAxis('x', 0, animate && animation);
        }
    },

    doFinalize: function() {
        var me = this,
            plugin = me.owner,
            item = me.ownerCmp,
            state = me.getState(),
            translatable = me.translatable;

        // if the state is not consumed the user has picked up the swiper
        // before the close animation finished.
        if (state === 'consumed') {
            translatable.translateAxis('x', 0, false);
            if (!me.destroyed && item) {
                plugin.destroyItem(item);
            }
        }
    },

    syncStep: function() {
        var me = this,
            item = me.ownerCmp,
            record = item.getRecord(),
            step = me.getStep(),
            ui = null,
            iconCls = '',
            text = '',
            action, data;

        if (step) {
            action = step.action;

            if (action) {
                ui = action.ui;
                iconCls = action.iconCls;
                text = action.text;
                data = action.data;
            }

            data = Ext.apply({
                    text: text
                }, data,
                record ?
                    record.getData(true) :
                    {});

            this.setUi(ui);
            this.setIconCls(iconCls);
            me.setData(data);
        } else {
            this.setUi(null);
            this.setIconCls(null);
            me.setData(null);
        }
    },

    updateIconCls: function (iconCls, oldIconCls) {
        this.iconElement.replaceCls(oldIconCls, iconCls);
    },

    privates: {
        getRenderTarget: function () {
            return this.innerElement;
        },

        onTap: function (evt) {
            var me = this,
                plugin = me.owner,
                dimissOnTap = plugin.getDismissOnTap();

            evt.stopPropagation();

            if (dimissOnTap) {
                me.dismiss();
            }
        },

        onUndoTap: function(button, evt) {
            evt.stopPropagation();
            this.revert();
        }
    }
});
