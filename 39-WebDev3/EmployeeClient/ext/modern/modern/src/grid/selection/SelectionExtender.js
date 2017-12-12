Ext.define('Ext.grid.selection.SelectionExtender', {
    requires: [
        'Ext.grid.Location',
        'Ext.util.TaskManager'
    ],

    maskBox: {},

    constructor: function(config) {
        var me = this,
            view = config.view,
            handleListeners = {
                dragstart: 'onDragStart',
                dragend: 'onDragEnd',
                scope: me
            };

        Ext.apply(me, config);

        me.el = view.outerCt;

        me.handle = view.bodyElement.createChild({
            cls: Ext.baseCSSPrefix + 'selmodel-extender-drag-handle'
        }).hide();

        // Start a drag on longpress if touch is supported.
        if (Ext.supports.Touch) {
            handleListeners.longpress = 'onHandleLongpress';
        }
        me.handle.on(handleListeners);

        me.mask = view.outerCt.createChild({
            cls: Ext.baseCSSPrefix + 'selmodel-extender-mask'
        }).hide();

        me.scrollListener = view.getScrollable().on({
            scroll: me.onViewScroll,
            scope: me,
            destroyable: true
        });
        me.viewListener = view.on({
            columnresize: 'alignHandle',
            columnhide: 'alignHandle',
            columnshow: 'alignHandle',
            columnmove: 'alignHandle',
            scope: me,
            destroyable: true
        });

        if (config && config.axes) {
            me.setAxes(config.axes);
        }
    },

    setAxes: function(axes) {
        var me = this;

        me.axes = axes;

        me.extendX = !!(axes & 1);
        me.extendY = !!(axes & 2);
    },

    setHandle: function(firstPos, endPos) {
        var me = this;

        me.firstPos = firstPos;
        me.endPos = endPos;

        if (firstPos && endPos && endPos.getCell(true)) {
            me.curPos = endPos;

            // Align centre of handle with bottom-right corner of last cell if possible.
            me.alignHandle();
        } else {
            me.disable();
        }
    },

    alignHandle: function() {
        var me = this,
            lastCell = me.endPos;

        // Cell corresponding to the position might not be rendered.
        // This will be called upon scroll
        if (me.firstPos && lastCell && me.view.isRecordRendered(lastCell.recordIndex)) {
            // Clone to refresh location in case it's moved in or out of
            // the rendered block.
            lastCell = lastCell.clone({
                record: lastCell.record,
                column: lastCell.column
            }).getCell();
            if (lastCell && lastCell.isVisible()) {
                me.enable();
            } else {
                me.disable();
            }
            me.handle.alignTo(lastCell, 'c-br');
        } else {
            me.disable();
        }
    },

    enable: function() {
        this.handle.show();
    },

    disable: function() {
        this.handle.hide();
        this.mask.hide();
    },

    onHandleLongpress: function(e) {
        e.startDrag();
    },

    onDragStart: function(e) {
        // For touch gestures, only initiate drags on longpress
        if (e.pointerType !== 'touch' || e.longpress) {
            e.claimGesture();
            this.handle.on('drag', this.onDrag, this);
        }
    },

    onDrag: function(e) {
        // The target of a Touch object remains unchanged from the touchstart target
        // even if the touch point moves outside of the original target.
        // We determine view Location from the "over" target, so polyfill using
        // the touch coordinates and document.elementFromPoint.
        if (e.changedTouches) {
            var touch = e.changedTouches[0],
                realTarget;

            // If the target does not contain the touch point, we have to correct it.
            if (touch && !Ext.fly(touch.target).getRegion().contains(touch.point)) {
                realTarget = Ext.event.Event.resolveTextNode(Ext.Element.fromPagePoint(touch.pageX, touch.pageY, true));

                // Points can sometimes go negative and return no target.
                if (realTarget) {
                    e.target = realTarget;
                }
            }
        }

        var me = this,
            target = e.target,
            view = me.view,
            scrollClientRegion = view.getScrollable().getElement().getClientRegion(),
            overCell = new Ext.grid.Location(view, target),
            scrollTask = me.scrollTask || (me.scrollTask = Ext.util.TaskManager.newTask({
                run: me.doAutoScroll,
                scope: me,
                interval: 10
            })),
            thresh = 25 * (window.devicePixelRatio || 1),
            scrollDelta = 3 * (window.devicePixelRatio || 1),
            scrollBy = me.scrollBy || (me.scrollBy = []);

        e.claimGesture();
        me.lastXY = [e.pageX, e.pageY];

        // Dragged outside the view; stop scrolling.
        if (!me.el.contains(target)) {
            scrollBy[0] = scrollBy[1] = 0;
            return scrollTask.stop();
        }

        // Near bottom of view
        if (me.lastXY[1] > scrollClientRegion.bottom - thresh) {
            if (me.extendY) {
                scrollBy[1] = scrollDelta;
                scrollTask.start();
            }
        }
        
        // Near top of view
        else if (me.lastXY[1] < scrollClientRegion.top + thresh) {
            if (me.extendY) {
                scrollBy[1] = -scrollDelta;
                scrollTask.start();
            }
        }

        // Near right edge of view
        else if (me.lastXY[0] > scrollClientRegion.right - thresh) {
            if (me.extendX) {
                scrollBy[0] = scrollDelta;
                scrollTask.start();
            }
        }
        
        // Near left edge of view
        else if (me.lastXY[0] < scrollClientRegion.left + thresh) {
            if (me.extendX) {
                scrollBy[0] = -scrollDelta;
                scrollTask.start();
            }
        }
        
        // Not near an edge, cancel autoscrolling
        else {
            scrollBy[0] = scrollBy[1] = 0;
            scrollTask.stop();
        }

        if (overCell && overCell.getCell() && !overCell.equals(me.lastOverCell)) {
            me.lastOverCell = overCell;
            me.syncMaskOnCell(overCell);
        }
    },

    doAutoScroll: function() {
        var me = this,
            view = me.view,
            scroller = view.getScrollable(),
            scrollOverCell;

        // Bump the view in whatever direction was decided in the onDrag method.
        scroller.scrollBy.apply(scroller, me.scrollBy);

        // Mouseover does not fire on autoscroll so see where the mouse is over on each scroll
        scrollOverCell = document.elementFromPoint.apply(document, me.lastXY);
        if (scrollOverCell) {
            scrollOverCell = new Ext.grid.Location(me.view, scrollOverCell);
            if (scrollOverCell && scrollOverCell.getCell() && !scrollOverCell.equals(me.lastOverCell)) {
                me.lastOverCell = scrollOverCell;
                me.syncMaskOnCell(scrollOverCell);
            }
        }
    },

    onDragEnd: function(e) {
        var me = this;

        // DRag listener is only added on successful drag start
        me.handle.un('drag', me.onDrag, me);

        if (me.scrollTask) {
            me.scrollTask.stop();
        }
        if (me.extensionDescriptor) {
            me.disable();
            me.view.getSelectable().extendSelection(me.extensionDescriptor);
        }
    },
    
    onViewScroll: function() {
        var me = this;

        // If being dragged or we have been applied to a selection block
        if ((me.active && me.lastOverCell) || me.firstPos) {
            me.endPos = me.endPos.clone({record: me.endPos.recordIndex});
            // Align centre of handle with bottom-right corner of last cell if possible.
            me.alignHandle();
        }
    },

    stopEvent: function(e) {
        e.stopEvent();
    },

    syncMaskOnCell: function(overCell) {
        var me = this,
            view = me.view,
            renderInfo = view.renderInfo,
            maskBox = me.maskBox,
            startRecordIndex = me.firstPos.recordIndex,
            endRecordIndex = me.endPos.recordIndex,
            extensionStart = me.firstPos.clone({record: startRecordIndex}),
            extensionEnd = me.endPos.clone({record: endRecordIndex}),
            selRegion, firstCell, endCell, curPos;

        // Constrain cell positions to be within rendered range.
        firstCell = me.firstPos.clone({
            record: Ext.Number.constrain(Math.min(startRecordIndex, endRecordIndex), renderInfo.indexTop, renderInfo.indexBottom - 1),
            column: me.firstPos.column
        });
        endCell = me.endPos.clone({
            record: Ext.Number.constrain(Math.max(firstCell.recordIndex, endRecordIndex), renderInfo.indexTop, renderInfo.indexBottom - 1)
        });

        me.selectionRegion = selRegion = firstCell.getCell().getRegion().union(endCell.getCell().getRegion());

        me.curPos = curPos = overCell;

        overCell = overCell.getCell('el');

        // Reset border to default, which is the overall border setting from SASS
        // We disable the border which is contiguous to the selection.
        me.mask.dom.style.borderTopWidth = me.mask.dom.style.borderRightWidth = me.mask.dom.style.borderBottomWidth = me.mask.dom.style.borderLeftWidth = '';

        // Dragged above the selection
        if (curPos.recordIndex < me.firstPos.recordIndex && me.extendY) {
            me.extensionDescriptor = {
                type: 'rows',
                start: extensionStart.clone({record: curPos.recordIndex}),
                end: extensionEnd.clone({record: me.firstPos.recordIndex - 1}),
                rows: curPos.recordIndex - me.firstPos.recordIndex,
                mousePosition: me.lastXY
            };
            me.mask.dom.style.borderBottomWidth = '0';
            maskBox.x = selRegion.x;
            maskBox.y = overCell.getY();
            maskBox.width = selRegion.right - selRegion.left;
            maskBox.height = selRegion.top - overCell.getY();
        }

        // Dragged below selection
        else if (curPos.recordIndex > me.endPos.recordIndex && me.extendY) {
            me.extensionDescriptor = {
                type: 'rows',
                start: extensionStart.clone({record: me.endPos.recordIndex + 1}),
                end: extensionEnd.clone({record: curPos.recordIndex}),
                rows: curPos.recordIndex - me.endPos.recordIndex,
                mousePosition: me.lastXY
            };
            me.mask.dom.style.borderTopWidth = '0';
            maskBox.x = selRegion.x;
            maskBox.y = selRegion.bottom;
            maskBox.width = selRegion.right - selRegion.left;
            maskBox.height = overCell.getRegion().bottom - selRegion.bottom;
        }

        // row position is within selected row range
        else {

            // Dragged to left of selection
            if (curPos.columnIndex < me.firstPos.columnIndex && me.extendX) {
                me.extensionDescriptor = {
                    type: 'columns',
                    start: extensionStart.clone({column: curPos.columnIndex}),
                    end: extensionEnd.clone({column: me.firstPos.columnIndex - 1}),
                    columns: curPos.columnIndex - me.firstPos.columnIndex,
                    mousePosition: me.lastXY
                };
                me.mask.dom.style.borderRightWidth = '0';
                maskBox.x = overCell.getX();
                maskBox.y = selRegion.top;
                maskBox.width = selRegion.left - overCell.getX();
                maskBox.height = selRegion.bottom - selRegion.top;
            }

            // Dragged to right of selection
            else if (curPos.columnIndex > me.endPos.columnIndex && me.extendX) {
                me.extensionDescriptor = {
                    type: 'columns',
                    start: extensionStart.clone({column: me.endPos.columnIndex + 1}),
                    end: extensionEnd.clone({column: curPos.columnIndex}),
                    columns: curPos.columnIndex - me.endPos.columnIndex,
                    mousePosition: me.lastXY
                };
                me.mask.dom.style.borderLeftWidth = '0';
                maskBox.x = selRegion.right;
                maskBox.y = selRegion.top;
                maskBox.width = overCell.getRegion().right - selRegion.right;
                maskBox.height = selRegion.bottom - selRegion.top;
            } else {
                me.extensionDescriptor = null;
            }
        }

        if (view.hasListeners.selectionextenderdrag) {
            view.fireEvent('selectionextenderdrag', view, view.getSelectable().getSelection(), me.extensionDescriptor);
        }
        if (me.extensionDescriptor) {
            me.mask.show();
            me.mask.setBox(maskBox);
        } else {
            me.mask.hide();
        }
    },

    destroy: function() {
        this.destroyMembers('viewListener', 'scrollListener', 'mask', 'handle');
    }
});
