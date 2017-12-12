Ext.define('Ext.dataview.listswiper.Accordion', {
    extend: 'Ext.dataview.listswiper.Item',
    xtype: 'listswiperaccordion',

    classCls: Ext.baseCSSPrefix + 'listswiperaccordion',

    cls: Ext.baseCSSPrefix + 'item-no-tap',

    config: {
        bodyOffset: null,

        actionDefaults: {
            cls: Ext.baseCSSPrefix + 'listswiperaction',
            xtype: 'button',
            iconAlign: 'top'
        },

        actionUI: 'square',

        singleActionDefaults: {},

        multiActionDefaults: {},

        undo: {
            cls: Ext.baseCSSPrefix + 'listswiperundoable',
            ui: 'undo',
            docked: 'right',
            ignoreDefaults: true
        },

        thresholds: null,

        /**
         * @cfg {Boolean} scaleDrag
         * Determines if the delta of a drag should be scaled depending on where the drag is started.
         * This causes drags that start in the middle of an item to move the items faster.
         * This means shorter drag distances when dragging from the middle or far sides
         */
        scaleDrag: true,

        /**
         * @cfg {Boolean} swipeToCommit
         * Determines if a full swipe should trigger the default action
         * If false a full swipe will result in the accordion being left in the open state
         */
        swipeToCommit: true,

        /**
         * @cfg {String} state
         * 'dragpeek','dragopen', 'dragcommit', 'open', 'undo'
         */

        /**
         * Current side that is revealed
         * @private
         */
        side: null
    },

    layout: {
        type: 'hbox',
        align: 'stretch'
    },

    template: [{
        reference: 'bodyElement',
        cls: Ext.baseCSSPrefix + 'body-el',
        uiCls: 'body-el',

        children: [{
            reference: 'leftElement',
            cls: Ext.baseCSSPrefix + 'listswiperaccordion-wrapper ' + Ext.baseCSSPrefix + 'listswiperaccordion-wrapper-left'
        }, {
            reference: 'rightElement',
            cls: Ext.baseCSSPrefix + 'listswiperaccordion-wrapper ' + Ext.baseCSSPrefix + 'listswiperaccordion-wrapper-right'
        }]
    }],

    scrollDock: null,

    constructor: function (config) {
        var me = this;

        me.left = {
            name: 'left',
            isLeft: true,
            items: []
        };

        me.right = {
            name: 'right',
            isLeft: false,
            items: []
        };

        me.callParent([config]);
    },

    initialize: function () {
        var me = this,
            target = me.getTranslationTarget();

        me.callParent();
        target.on({
            scope: me,
            tap: 'onDismissTap'
        });
    },

    destroy: function () {
        var me = this,
            target = me.getTranslationTarget();

        //<debug>
        if (me.thresholdEl) {
            me.thresholdEl.destroy();
        }
        //</debug>

        target.un({
            scope: me,
            tap: 'onDismissTap'
        });

        me.callParent();
    },

    applyLeftActions: function (items) {
        this.addActions('left', items);
    },

    applyRightActions: function (items) {
        this.addActions('right', items);
    },

    applySide: function (side) {
        this.side = side && this[side];
        return side;
    },

    getButtonBackgroundColor: function(button) {
        var action = button.$action,
            backgroundColorEl = button[action.backgroundColorEl || 'innerElement'];

        return backgroundColorEl.getStyle('backgroundColor');
    },

    addActions: function (side, items) {
        var me = this,
            i, config, action, button;

        side = me[side];
        side.el = side.isLeft ? me.leftElement : me.rightElement;
        side.multiple = items.length > 1;
        config = side.multiple ? me.getMultiActionDefaults() : me.getSingleActionDefaults();
        side.el.toggleCls(me.baseCls + '-multiple', side.multiple);
        side.el.toggleCls(me.baseCls + '-single', !side.multiple);
        for (i = 0; i < items.length; i++) {
            action = me.createActionItem(Ext.apply({}, items[i], config));
            action.$side = side;
            button = me.add(action);
            button.addUi(this.getActionUI());
            button.$action = action;
            button.$originalHandler = button.getHandler();
            button.setHandler(me.onActionTap.bind(me, action));

            side.items.push(button);
        }
    },

    createActionItem: function (config) {
        return Ext.apply({}, config, this.getActionDefaults());
    },

    getSwipeRange: function () {
        var me = this,
            side = me.side,
            plugin = me.owner,
            swipeMax = plugin.getSwipeMax();

        return me.itemWidth * (swipeMax[side.multiple ? 'multiple' : 'single'] / 100);
    },

    onActionTap: function (action, button, e) {
        var me = this,
            state = me.getState();

        if (state !== 'dragpeek') {
            e.stopPropagation();
            this.commit(e, action, button);
        }
    },

    onDismissTap: function () {
        var me = this,
            plugin = me.owner,
            dimissOnTap = plugin.getDismissOnTap();

        if (dimissOnTap) {
            me.dismiss();
        }
    },

    onRender: function () {
        var me = this,
            item = me.ownerCmp;

        me.itemWidth = item.el.measure('w');
        me.syncSides();
    },

    updateSide: function (side, oldSide) {
        var me = this,
            layout = me.getLayout();
        me.el.replaceCls(oldSide, side, me.baseCls + '-side');

        if (side === 'left') {
            layout.setPack('start');
        } else {
            layout.setPack('end');
        }
    },

    updateState: function (state, oldState) {
        var me = this,
            side = me.side,
            defaultButton = me.getDefaultButton();

        me.callParent([state, oldState]);

        if (side.multiple) {
            me.el.toggleCls(me.baseCls + '-collapsed', state === 'dragcommit');

            if (state === 'dragcommit') {
                defaultButton.el.setStyle({'flex-basis': side.maxActionWidth + 'px'});
            } else {
                defaultButton.setStyle({'flex-basis': null});
            }

            if (oldState === 'dragcommit' && me.isDragging) {
                me.el.addCls(me.baseCls + '-was-collapsed');
            } else {
                me.el.removeCls(me.baseCls + '-was-collapsed');
            }
        }
    },

    privates: {
        destroyItem: function () {
            var me = this,
                plugin = me.owner,
                item = me.ownerCmp;

            if (!me.destroyed) {
                me.animating = false;
                me.el.removeCls(me.baseCls + '-was-collapsed');
                plugin.destroyItem(item);
            }
        },
        animateItem: function (offset, config) {
            config = config || {};

            var me = this,
                side = me.side,
                target = this.getTranslationTarget(),
                duration = config.duration || 150,
                completeFn;

            return new Ext.Promise(function (resolve) {
                me.animating = true;
                me.offset = side.isLeft ? offset : -offset;
                completeFn = function () {
                    if (!me.destroyed) {
                        me.animating = false;
                        me.el.removeCls(me.baseCls + '-was-collapsed');
                    }
                    resolve();
                };

                if (target.dom) {
                    if (side.el.dom) {
                        side.el.animate({
                            preserveEndState: true,
                            duration: duration,
                            to: {
                                width: offset
                            }
                        });
                    }

                    target.animate({
                        preserveEndState: true,
                        duration: duration,
                        to: {
                            transform: {
                                translateX: me.offset
                            }
                        },

                        callback: completeFn
                    });
                } else {
                    completeFn();
                }
            });
        },

        commit: function (e, action, button) {
            var me = this,
                plugin = me.owner,
                undoable, handler,
                delay, precommitResult, undo, backgroundColor;

            action = action || me.getDefaultAction();
            button = button || me.getDefaultButton();
            undoable = action.undoable;
            handler = button.$originalHandler;

            me.setAction(action);
            me.$precommitResult = precommitResult = me.invokeAction(action, 'precommit');

            if (handler) {
                me.snapback().then(function () {
                    Ext.callback(handler, button.getScope(), [action, e], 0, button);
                }).then(function() {
                    me.destroyItem();
                });
            } else {
                if (!undoable) {
                    me.dismiss();
                } else {
                    undo = me.add(me.getUndo());
                    undo.addUi(button.getUi());

                    me.bodyElement.on({
                        scope: me,
                        tap: 'onDismissTap'
                    });

                    Ext.raf(function () {
                        me.setState('undo');
                        backgroundColor = me.getButtonBackgroundColor(button);
                        if (backgroundColor) {
                            me.el.setStyle('backgroundColor', backgroundColor);
                        }

                        undo.setHandler(me.onUndoTap.bind(me));

                        delay = plugin.getCommitDelay();
                        if (delay) {
                            if (precommitResult && precommitResult.then) {
                                precommitResult.then(function () {
                                    plugin.dismissAllTask.delay(delay);
                                });
                            } else {
                                plugin.dismissAllTask.delay(delay);
                            }
                        }
                    });
                }
            }
        },

        onUndoTap: function () {
            this.undo();
        },

        undo: function () {
            var me = this,
                action = me.getAction(),
                precommitResult = me.$precommitResult;

            me.setState('open');

            if (precommitResult && precommitResult.then) {
                precommitResult.then(function () {
                    me.$precommitResult = null;
                    me.undo();
                });
                return;
            }

            me.snapback().then(function () {
                me.invokeAction(action, 'revert');
            }).then(function() {
                me.destroyItem();
            });
        },

        //<debug>
        createThresholds: function () {
            var me = this,
                item = me.ownerCmp,
                plugin = this.owner,
                side = me.side,
                swipeRange = me.getSwipeRange(),
                commitThreshold = side.commitThreshold,
                openThreshold = side.openThreshold;

            if (plugin.showThresholds && !me.thresholdEl) {
                me.thresholdEl = item.el.append({
                    style: {
                        display: 'flex',
                        flexDirection: 'row',
                        justifyContent: 'flex-end',
                        position: 'absolute',
                        height: 'auto',
                        top: 0,
                        left: 0,
                        right: 0
                    },
                    children: [
                        {
                            style: {
                                width: (swipeRange - (commitThreshold - openThreshold) - openThreshold) + 'px',
                                height: '6px',
                                opacity: .8,
                                backgroundColor: '#EF5350'
                            }
                        },
                        {
                            style: {
                                width: (commitThreshold - openThreshold) + 'px',
                                height: '6px',
                                opacity: .8,
                                backgroundColor: '#FFEE58'
                            }
                        },
                        {
                            style: {
                                width: openThreshold + 'px',
                                height: '6px',
                                opacity: .8,
                                backgroundColor: '#66BB6A'
                            }
                        }
                    ]
                });
            }

            if (me.thresholdEl) {
                me.thresholdEl.setStyle({
                    flexDirection: !side.isLeft ? 'row' : 'row-reverse'
                });
            }
        },
        //</debug>

        dismiss: function () {
            var me = this,
                action = me.getAction(),
                precommitResult = me.$precommitResult;

            if (precommitResult && precommitResult.then) {
                precommitResult.then(function () {
                    me.$precommitResult = null;
                    me.dismiss();
                });
                return;
            }

            if (action) {
                me.snapback().then(function () {
                    me.invokeAction(action, 'commit');
                }).then(function() {
                    me.destroyItem();
                });
            } else {
                me.snapback(true);
            }
        },

        onDragStart: function (e) {
            var me = this,
                state = me.getState();
            if (me.animating || state === 'undo') {
                return;
            }

            e.claimGesture();
            me.initialOffset = me.offset || 0;
            me.startX = e.getX() - me.el.getX() - me.initialOffset;
            me.isDragging = true;
            me.syncState(e.deltaX);
        },

        onDragMove: function (e) {
            e.preventDefault();
            this.syncState(e.deltaX);
        },

        onDragEnd: function (e) {
            var me = this,
                state = me.getState();

            e.preventDefault();
            me.isDragging = false;
            if (state === 'dragcommit') {
                me.commit(e);
            } else if (state === 'dragopen') {
                me.open();
            } else {
                me.snapback(true);
            }
        },

        getDefaultButton: function(side) {
            side = side || this.side;

            var items = side.items;
            return items[side.isLeft ? 0 : items.length - 1];
        },

        getDefaultAction: function (side) {
            var button = this.getDefaultButton(side);
            return button && button.$action;
        },

        getRenderTarget: function (item) {
            var side = item && item.$side;
            if (side) {
                return side.el;
            }

            return this.callParent(arguments);
        },

        open: function () {
            return this.animateItem(this.side.naturalWidth);
        },

        snapback: function (destroy) {
            var me = this,
                anim = me.animateItem(0);

            return destroy ? anim.then(function() { me.destroyItem(); }) : anim;
        },

        syncSides: function () {
            var me = this;

            me.syncSide('left');
            me.syncSide('right');
        },

        syncSide: function (side) {
            var me = this,
                thresholds = me.getThresholds(),
                itemWidth = me.itemWidth,
                element = side === 'left' ? me.leftElement : me.rightElement,
                children = element.dom.childNodes,
                maxActionWidth = 0,
                backgroundColor, defaultButton, childWidth, i, child, naturalWidth;

            side = me[side];
            defaultButton = this.getDefaultButton(side);

            element.addCls(me.baseCls + '-measure');

            for (i = 0; i < children.length; i++) {
                child = Ext.get(children[i]);
                childWidth = child.measure('w');

                if (childWidth > maxActionWidth) {
                    maxActionWidth = childWidth;
                }
            }

            naturalWidth = side.naturalWidth = maxActionWidth * children.length;
            side.maxActionWidth = maxActionWidth;

            if (thresholds && thresholds.open) {
                side.openThreshold = (thresholds.open / 100) * itemWidth;
            } else {
                side.openThreshold = maxActionWidth;
            }

            if (thresholds && thresholds.commit) {
                side.commitThreshold = (thresholds.commit / 100) * itemWidth;
            } else {
                side.commitThreshold = Math.min(.95 * itemWidth, naturalWidth * 1.4);
            }

            element.removeCls(me.baseCls + '-measure');


            if (side.multiple) {
                backgroundColor = me.getButtonBackgroundColor(defaultButton);
                if (backgroundColor) {
                    side.el.setStyle('backgroundColor', backgroundColor);
                }
            }
        },

        syncState: function (deltaX) {
            var me = this,
                plugin = me.owner,
                itemWidth = me.itemWidth,
                swipeToCommit = me.getSwipeToCommit(),
                scaleDrag = me.getScaleDrag(),
                testOffset = me.initialOffset + deltaX,
                side = this[(testOffset < 0 ? 'right' : 'left')],
                // Determines the scale (1-3) inwhich to multiple the delta by depending on where you start the drag
                // drags started in the middle will scale faster allowing for items to be seen without extremly long drags
                scaler = scaleDrag ? Math.max(1, Math.min(3, Math.abs( (side.isLeft ? 0 : 1) - (me.startX / itemWidth)) * 3)) : 1,
                offset = me.offset = me.initialOffset + (deltaX * scaler),
                currentSide = me.side,
                directionLock = plugin.getDirectionLock(),
                positiveOffset, swipeRange, openThreshold, commitThreshold;

            if (this.left.items.length === 0 || this.right.items.length === 0) {
                directionLock = false;
            }

            // Empty Items or Direction locked will friction drag
            if (side.items.length === 0 || (currentSide && (side.name !== currentSide.name && directionLock))) {
                offset = me.offset = me.initialOffset + (deltaX * 0.1);

                // Possible first drag was on an empty side, we need to set the side
                if (side.items.length === 0) {
                    me.setSide(side.name);
                }
                me.setState('draglocked');
            } else {
                me.setSide(side.name);

                // Depends on side being set, must be done after.
                swipeRange = me.getSwipeRange();
                openThreshold = side.openThreshold;
                commitThreshold = side.commitThreshold;

                positiveOffset = (side.isLeft ? Math.abs(Math.max(0, offset)) : Math.abs(Math.min(0, offset)));

                //<debug>
                me.createThresholds();
                //</debug>

                if (positiveOffset <= openThreshold) {
                    me.setState('dragpeek');
                } else if (positiveOffset <= commitThreshold || !swipeToCommit) {
                    me.setState('dragopen');
                } else {
                    me.setState('dragcommit');
                }

                if (side.isLeft) {
                    offset = Math.min(offset, swipeRange);
                } else {
                    offset = Math.max(offset, -swipeRange);
                }
            }

            me.setBodyOffset(offset);
        },

        updateBodyOffset: function (offset) {
            var me = this,
                side = me.side,
                target = me.getTranslationTarget();

            target.setStyle('transform', 'translateX(' + offset + 'px)');
            side.el.setWidth(Math.abs(offset));
        }
    }
});