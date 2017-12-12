Ext.testHelper = {
    defaultTarget: document.createElement('div'),

    createTouchList: function(touches) {
        var touchList = [],
            i, ln, touch;

        for (i = 0, ln = touches.length; i < ln; i++) {
            touch = touches[i];

            touchList.push(this.createTouch(touch));
        }

        return touchList;
    },

    createTouch: function(touch) {
        return Ext.merge({
            target: this.defaultTarget,
            timeStamp: Ext.Date.now(),
            time: Ext.Date.now(),
            pageX: 0,
            pageY: 0,
            identifier: 0,
            point: new Ext.util.Point(touch.pageX || 0, touch.pageY || 0)
        }, touch || {});
    },

    createTouchEvent: function(event) {
        var touchEvent = Ext.merge({
            type: 'touchstart',
            target: this.defaultTarget,
            timeStamp: Ext.Date.now(),
            time: Ext.Date.now(),
            touches: [],
            changedTouches: [],
            targetTouches: [],
            pageX: 0,
            pageY: 0
        }, event || {});

        touchEvent.touches = this.createTouchList(touchEvent.touches);
        touchEvent.changedTouches = this.createTouchList(touchEvent.changedTouches);
        touchEvent.targetTouches = this.createTouchList(touchEvent.targetTouches);

        return touchEvent;
    },

    createTouchEvents: function(events) {
        var ret = [],
            i, ln, event;

        for (i = 0, ln = events.length; i < ln; i++) {
            event = events[i];

            ret.push(this.createTouchEvent(event));
        }

        return ret;
    },

    recognize: function(recognizer, events) {
        var currentTouchesCount = 0,
            i, ln, e;

        events = this.createTouchEvents(events);

        mainLoop: for (i = 0, ln = events.length; i < ln; i++) {
            e = events[i];

            switch (e.type) {
                case 'touchstart':
                    var changedTouchesCount = e.changedTouches.length,
                        isStarted = currentTouchesCount > 0;

                    currentTouchesCount += changedTouchesCount;

                    if (!isStarted) {
                        if (recognizer.onStart(e) === false) {
                            break mainLoop;
                        }
                    }

                    if (recognizer.onTouchStart(e) === false) {
                        break mainLoop;
                    }

                    break;

                case 'touchmove':
                    if (recognizer.onTouchMove(e) === false) {
                        break mainLoop;
                    }
                    break;

                case 'touchend':
                    changedTouchesCount = e.changedTouches.length;

                    currentTouchesCount -= changedTouchesCount;

                    if (recognizer.onTouchEnd(e) === false) {
                        break mainLoop;
                    }

                    if (this.currentTouchesCount === 0) {
                        if (recognizer.onEnd(e) === false) {
                            break mainLoop;
                        }
                    }
                    break;
            }
        }

        return events;
    },

    pointerEvents: Ext.supports.PointerEvents ? {
        start: 'pointerdown',
        move: 'pointermove',
        end: 'pointerup',
        cancel: 'pointercancel',
        over: 'pointerover',
        out: 'pointerout',
        enter: 'pointerenter',
        // No decent way to feature detect this, pointerleave relatedTarget is
        // incorrect on IE11, so force it to use mouseleave here.
        // See: https://connect.microsoft.com/IE/feedback/details/851111/ev-relatedtarget-in-pointerleave-indicates-departure-element-not-destination-element
        leave: jasmine.browser.isIE11 ? 'mouseleave' : 'pointerleave'
    } : Ext.supports.MSPointerEvents ? {
        start: 'MSPointerDown',
        move: 'MSPointerMove',
        end: 'MSPointerUp',
        cancel: 'MSPointerCancel',
        over: 'MSPointerOver',
        out: 'MSPointerOut',
        // IE10 does not have pointer events for enter/leave
        enter: 'mouseenter',
        leave: 'mouseleave'
    } : {},

    touchEvents: {
        start: 'touchstart',
        move: 'touchmove',
        end: 'touchend',
        cancel: 'touchcancel'
    },

    mouseEvents: {
        start: 'mousedown',
        move: 'mousemove',
        end: 'mouseup',
        over: 'mouseover',
        out: 'mouseout',
        enter: 'mouseenter',
        leave: 'mouseleave',
        cancel: 'mouseup'
    },

    fireEvent: function(type, target, cfg) {
        var me = this,
            scroll = Ext.getDoc().getScroll(),
            eventType, centre;

        if (!cfg) {
            cfg = {};
        }

        cfg.id = cfg.id || 1;

        if (cfg.x == null || cfg.y == null) {
            // Default to the middle of the target
            centre = Ext.fly(target, '_testFireEvent').getAnchorXY('c');

            if (cfg.x == null) {
                cfg.x = centre[0];
            }

            if (cfg.y == null) {
                cfg.y = centre[1];
            }
        }

        target = Ext.getDom(target);
        eventType = me.pointerEvents[type];

        // If the type required can be handled through the pointer events system, use that
        if (eventType) {
            return jasmine.firePointerEvent(
                target,
                eventType,
                cfg.id,
                cfg.x - scroll.left,
                cfg.y - scroll.top,
                cfg.button || 0,
                cfg.shiftKey,
                cfg.ctrlKey,
                cfg.altKey,
                cfg.relatedTarget,
                cfg.pointerType || 'mouse'
            );
        }
        // Now decide whether to use touch or mouse events
        else {
            // Use touch if the platform supports the requested interaction type.
            // If it's "over" or "out", we'll fall through to mouse events.
            // This is important for touch enabled platforms which have a mouse but
            // do not support W3C pointer events.
            // If they insist on specifying touch events, override the fallback to mouse
            // on desktop browsers that support touch events

            eventType = me.touchEvents[type];
            if (jasmine.supportsTouch &&
                    (cfg.pointerType === 'touch' || (eventType && !Ext.os.is.Desktop))) {
                // If a translated mousemove happens with no prior mousedown
                // we have to ignore it - that's no gesture on touch.
                if (eventType === 'touchmove' && !Ext.Object.getKeys(me.activeTouches).length) {
                    return;
                }

                return me.fireSingleTouch(me.touchEvents[type], target, cfg);
            }
            // Use mouse events
            else {
                return jasmine.doFireMouseEvent(
                    target,
                    me.mouseEvents[type],
                    cfg.x - scroll.left,
                    cfg.y - scroll.top,
                    cfg.button || 0,
                    cfg.shiftKey,
                    cfg.ctrlKey,
                    cfg.altKey,
                    cfg.relatedTarget
                );
            }
        }
    },

    fireSingleTouch: function(type, target, cfg) {
        var activeTouches = this.activeTouches || (this.activeTouches = {}),
            scroll = Ext.getDoc().getScroll(),
            touch, touches, id;

        touch = activeTouches[cfg.id] = {
            identifier: cfg.id,
            pageX: cfg.x,
            pageY: cfg.y,
            clientX: cfg.x - scroll.left,
            clientY: cfg.y - scroll.top,
            screenX: cfg.x - scroll.left,
            screenY: cfg.y - scroll.top,
            shiftKey: cfg.shiftKey,
            ctrlKey: cfg.ctrlKey,
            altKey: cfg.altKey
        };

        touches = [];

        if (type === 'touchend' || type === 'touchcancel') {
            delete activeTouches[cfg.id];
        }

        for (id in activeTouches) {
            touches.push(activeTouches[id]);
        }

        jasmine.fireTouchEvent(
            target,
            type,
            touches,
            [touch]
        );

    },
    
    inputTypeSelectionSupported: /text|password|search|tel|url/i,

    getCaretPos: function(el) {
        var dom = Ext.getDom(el),
            type = dom.type,
            doSwitch = !Ext.testHelper.inputTypeSelectionSupported.test(type),
            result;

        if (doSwitch) {
            dom.type = 'text';
        }

        result = Ext.fly(dom).getCaretPos();

        if (doSwitch) {
            dom.type = type;
        }

        return result;
    },

    setCaretPos: function(el, newPos) {
        var dom = Ext.getDom(el),
            type = dom.type,
            doSwitch = !Ext.testHelper.inputTypeSelectionSupported.test(type);

        if (doSwitch) {
            dom.type = 'text';
        }

        Ext.fly(dom).setCaretPos(newPos);

        if (doSwitch) {
            dom.type = type;
        }
    },

    select: function (el, start, end, direction) {
        var dom = Ext.getDom(el),
            type = dom.type,
            doSwitch = !Ext.testHelper.inputTypeSelectionSupported.test(type);

        if (doSwitch) {
            dom.type = 'text';
        }

        Ext.fly(dom).selectText(start, end, direction);

        if (doSwitch) {
            dom.type = type;
        }
    },

    tap: function(target, cfg) {
        var scroll, ret;

        cfg = cfg || {};

        this.fireEvent('start', target, cfg);
        this.fireEvent('end', target, cfg);

        scroll = Ext.getDoc().getScroll();

        ret = jasmine.doFireMouseEvent(
            Ext.getDom(target),
            'click',
            (cfg.x || 0) - scroll.left,
            (cfg.y || 0) - scroll.top,
            cfg.button ? cfg.button : 0,
            cfg.shiftKey,
            cfg.ctrlKey,
            cfg.altKey
        );

        return ret;
    },

    touchStart: function(target, cfg) {
        this.fireEvent('start', target, Ext.apply({
            pointerType: 'touch'
        }, cfg));
    },

    touchMove: function(target, cfg) {
        this.fireEvent('move', target, Ext.apply({
            pointerType: 'touch'
        }, cfg));
    },

    touchEnd: function(target, cfg) {
        this.fireEvent('end', target, Ext.apply({
            pointerType: 'touch'
        }, cfg));
    },

    touchCancel: function(target, cfg) {
        this.fireEvent('cancel', target, Ext.apply({
            pointerType: 'touch'
        }, cfg));
    },
    
    showHeaderMenu: function(column) {
        var menu = column.getRootHeaderCt().getMenu();

        // Hide menu if it's shown for another column
        runs(function() {
            if (menu.isVisible() && menu.getRefOwner() !== column) {
                menu.hide();
            }
        });
        focusAndWait(column);
        runs(function() {
            jasmine.fireKeyEvent(column.el, 'keydown', Ext.event.Event.DOWN);
            waitsForFocus(menu.child(':focusable'), 'Column #' + column.id +
                          ' [text="' + column.text + '"] to focus');
        });
    },

    parseTransform: function(el) {
        el = Ext.getDom(el);

        var match = /translate(?:3d)?\s*\(([^\)]*)\)/.exec(el.style.transform),
            ret;

        if (match) {
            ret = match[1].split(/\s*,\s*/);
        } else {
            ret = [0, 0, 0];
        }
        return ret;
    },

    doTyping: function(el, text) {
        el = Ext.get(el);
        if (text == null) {
            text = '';
        }

        var dom = Ext.getDom(el),
            curValue = dom.value,
            cursorPos = this.getCaretPos(el),
            charCount = text.length,
            newValue = curValue.substr(0, cursorPos) + text + curValue.substr(cursorPos),
            inputEvent;

        if (document.defaultView.InputEvent) {
            inputEvent = new InputEvent('input');
            inputEvent.bubbles = true;
            inputEvent.cancelable = true;
        } else {
            inputEvent = document.createEvent('Event');
            inputEvent.initEvent('input', true, true);
        }

        dom.value = newValue;
        this.setCaretPos(el, cursorPos + charCount);
        dom.dispatchEvent(inputEvent);
    },

    // jazzman automatically invokes this method after each spec
    reset: function() {
        var activeTouches = this.activeTouches,
            id;

        for (id in activeTouches) {
            jasmine.fireTouchEvent(
                document,
                'touchcancel',
                [activeTouches[id]],
                [activeTouches[id]]
            );
        }
        
        this.activeTouches = null;

        // End any mousedown counters. Don't fire mouseup if there was no mousedown though!
        if (jasmine.doFireMouseEvent.needMouseup) {
            // We don't need stack for this one, and collecting it is expensive!
            id = jasmine.CAPTURE_CALL_STACK;
            jasmine.CAPTURE_CALL_STACK = false;

            jasmine.doFireMouseEvent(document.body, 'mouseup');

            jasmine.CAPTURE_CALL_STACK = id;
        }
    },

    hash: {
        init: function () {
            this.$win = Ext.util.History.win;

            Ext.util.History.win = Ext.testHelper.hash.create();
        },

        reset: function () {
            Ext.util.History.win = this.$win;
            this.$win = null;

            delete Ext.util.History.hashBang;

            Ext.util.History.currentToken =
                Ext.util.History.hash =
                window.location.hash = '';
        },

        create: function () {
            var HashMock = {
                hash: '',
                historyStack: [],
                currentIdx: -1,
                history: {
                    go: function (direction) {
                        var newIdx = HashMock.currentIdx + direction;

                        if (newIdx < 0) {
                            //cannot go to -1
                            newIdx = 0;
                        }

                        HashMock.currentIdx = newIdx;

                        HashMock.hash = HashMock.historyStack[newIdx] || '';
                    }
                },
                location: {
                    replace: function (uri) {
                        var stack = HashMock.historyStack,
                            idx = HashMock.currentIdx;

                        if (idx < 0) {
                            //should not replace -1
                            idx = HashMock.currentIdx = 0;
                        }

                        stack[idx] = uri;

                        HashMock.hash = '#' + uri.split('#')[1];
                    }
                },
                reset: function () {
                    this.hash = '';
                    this.historyStack.length = 0;
                    this.currentIdx = -1;
                }
            };

            if (!Ext.isIE8) {
                Object.defineProperty(HashMock.location, 'hash', {
                    enumerable : true,
                    get        : function () {
                        return HashMock.hash;
                    },
                    set        : function (frag) {
                        if (frag.substr(0, 1) !== '#') {
                            frag = '#' + frag;
                        }

                        if (HashMock.hash !== frag) {
                            var stack = HashMock.historyStack,
                                num = stack.length;

                            if (HashMock.currentIdx === -1) {
                                stack.push('');
                                HashMock.currentIdx = num;

                                num++;
                            }

                            if (num - 1 > HashMock.currentIdx) {
                                stack.length = num - 1;
                            }

                            stack.push(HashMock.hash = frag);

                            HashMock.currentIdx = num;
                        }
                    }
                });
            }

            return HashMock;
        }
    }
};
