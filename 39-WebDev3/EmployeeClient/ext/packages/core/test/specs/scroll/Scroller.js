/* global Ext, expect, Infinity, jasmine, spyOn */

topSuite("Ext.scroll.Scroller", function() {
    var ctWidth = 100,
        ctHeight = 100,
        scroller, el, child;

    function makeScroller(config) {
        scroller = new Ext.scroll.Scroller(Ext.apply({
            element: el
        }, config));
    }

    function appendEl(w, h, styles) {
        styles = Ext.apply({}, styles);
        if (typeof w === 'number') {
            styles.width = w + 'px';
        }

        if (typeof h === 'number') {
            styles.height = h + 'px';
        }

        el.appendChild({
            style: styles
        }, true);
    }

    function makeOverflow (cfg) {
        for (var i = 1; i <= 100; ++i) {
            el.appendChild({
                id: Ext.id(),
                html: 'Line' + i,
                cls: 'line',
                style: 'height: 20px'
            }, true);
        }

        makeScroller(cfg);
    }

    function makeInline (cfg) {
        el.setStyle('white-space', 'nowrap');

        for (var i = 1; i <= 100; ++i) {
            el.appendChild({
                id: Ext.id(),
                html: 'Line' + i,
                cls: 'line',
                style: 'height: 20px;width: 50px;display: inline-block;'
            }, true);
        }

        makeScroller(cfg);
    }

    function makeAbsoluteOverflow (left, top) {
        el.dom.style.position = 'relative';

        child = el.createChild({
            style: {
                width: '100px',
                height: '100px',
                html: 'Foo',
                position: 'absolute',
                left: (left || 0) + 'px',
                top: (top || 0) + 'px'
            }
        });

        makeScroller();

        return child;
    }

    function makeNoOverflow (cfg) {
        el.appendChild({
            style: 'height:100px;width:100px;'
        }, true);

        makeScroller(cfg);
    }

    function getChild(i, asEl) {
        var node = el.dom.childNodes[i];

        if (asEl) {
            node = Ext.fly(node);
        }

        return node;
    }

    beforeEach(function() {
        el = Ext.getBody().createChild({
            style: {
                height: ctHeight + 'px',
                width: ctWidth + 'px'
            }
        });
    });

    afterEach(function() {
        scroller = el = child = Ext.destroy(child, scroller, el);
    });

    function calculateMaxScrollPosition(contentWidth, contentHeight) {
        var sz = Ext.getScrollbarSize();
        return {
            x: contentWidth - ctWidth + sz.width,
            y: contentHeight - ctHeight + sz.height
        };
    }

    describe("configuring the element", function() {
        var element;

        afterEach(function() {
            if (element && !element.destroyed) {
                Ext.fly(element).destroy();
            }
        });

        it("should accept an HTMLElement", function() {
            element = document.createElement('div');
            document.body.appendChild(element, true);

            scroller = new Ext.scroll.Scroller({
                element: element
            });

            expect(scroller.getElement().isElement).toBe(true);
            expect(scroller.getElement().dom).toBe(element);
        });

        it("should accept an Element ID", function() {
            element = document.createElement('div');
            element.id = 'theEl';
            document.body.appendChild(element, true);

            scroller = new Ext.scroll.Scroller({
                element: 'theEl'
            });

            expect(scroller.getElement().isElement).toBe(true);
            expect(scroller.getElement().dom).toBe(element);
        });

        it("should accept an Ext.dom.Element", function() {
            element = Ext.getBody().createChild();

            scroller = new Ext.scroll.Scroller({
                element: element
            });

            expect(scroller.getElement()).toBe(element);
        });

        it("should throw an error if element with given id not found", function() {
            expect(function() {
                scroller = new Ext.scroll.Scroller({
                    element: 'foobarelement'
                });
            }).toThrow("Cannot create Ext.scroll.Scroller instance. Element with id 'foobarelement' not found.");
        });
    });

    describe("x", function() {
        it("should set overflow-x:auto on the element by default", function() {
            makeScroller();

            expect(el.dom.style.overflowX).toBe('auto');
        });

        it("should set overflow-x:auto on the element when x is true", function() {
            makeScroller({
                x: true
            });

            expect(el.dom.style.overflowX).toBe('auto');
        });

        it("should set overflow-x:auto on the element when x is 'auto'", function() {
            makeScroller({
                x: 'auto'
            });

            expect(el.dom.style.overflowX).toBe('auto');
        });

        it("should set overflow-x:scroll on the element when x is 'scroll'", function() {
            makeScroller({
                x: 'scroll'
            });

            expect(el.dom.style.overflowX).toBe('scroll');
        });

        it("should set overflow-x:hidden the element when x is false", function() {
            makeScroller({
                x: false
            });

            expect(el.dom.style.overflowX).toBe('hidden');
        });
    });

    describe("y", function() {
        it("should set overflow-y:auto on the element by default", function() {
            makeScroller();

            expect(el.dom.style.overflowY).toBe('auto');
        });

        it("should set overflow-y:auto on the element when y is true", function() {
            makeScroller({
                y: true
            });

            expect(el.dom.style.overflowY).toBe('auto');
        });

        it("should set overflow-y:auto on the element when y is 'auto'", function() {
            makeScroller({
                y: 'auto'
            });

            expect(el.dom.style.overflowY).toBe('auto');
        });

        it("should set overflow-y:scroll on the element when y is 'scroll'", function() {
            makeScroller({
                y: 'scroll'
            });

            expect(el.dom.style.overflowY).toBe('scroll');
        });

        it("should set overflow-y:hidden on the element when y is false", function() {
            makeScroller({
                y: false
            });

            expect(el.dom.style.overflowY).toBe('hidden');
        });
    });

    describe("direction", function() {
        it("should set overflow-x:auto and overflow-y:auto on the element when direction is 'auto'", function() {
            makeScroller({
                direction: 'auto'
            });

            expect(el.dom.style.overflowX).toBe('auto');
            expect(el.dom.style.overflowY).toBe('auto');
        });

        it("should set overflow-x:scroll and overflow-y:scroll on the element when direction is 'both'", function() {
            makeScroller({
                direction: 'both'
            });

            expect(el.dom.style.overflowX).toBe('scroll');
            expect(el.dom.style.overflowY).toBe('scroll');
        });

        it("should set overflow-y:auto on the element when direction is 'vertical'", function() {
            makeScroller({
                direction: 'vertical'
            });

            expect(el.dom.style.overflowY).toBe('auto');
            expect(el.dom.style.overflowX).toBe('hidden');
        });

        it("should set overflow-x:auto on the element when direction is 'horizontal'", function() {
            makeScroller({
                direction: 'horizontal'
            });

            expect(el.dom.style.overflowX).toBe('auto');
            expect(el.dom.style.overflowY).toBe('hidden');
        });
    });

    describe("getSize", function() {
        beforeEach(function() {
            el.appendChild({
                style: 'height:200px;width:300px;'
            }, true);
        });

        it("should return the content size with x:auto and y:auto", function() {
            makeScroller();
            expect(scroller.getSize()).toEqual({
                x: 300,
                y: 200
            });
        });

        it("should return the content size with x:scroll and y:scroll", function() {
            makeScroller({
                x: 'scroll',
                y: 'scroll'
            });
            expect(scroller.getSize()).toEqual({
                x: 300,
                y: 200
            });
        });

        it("should return the content size with x:false and y:false", function() {
            makeScroller({
                x: false,
                y: false
            });

            expect(scroller.getSize()).toEqual({
                x: 300,
                y: 200
            });
        });

        it("should return the content size with x:false and y:auto", function() {
            makeScroller({
                x: false,
                y: true
            });

            expect(scroller.getSize()).toEqual({
                x: 300,
                y: 200
            });
        });

        it("should return the content size with x:auto and y:false", function() {
            makeScroller({
                x: true,
                y: false
            });

            expect(scroller.getSize()).toEqual({
                x: 300,
                y: 200
            });
        });

        it("should return the content size with x:false and y:scroll", function() {
            makeScroller({
                x: false,
                y: 'scroll'
            });

            expect(scroller.getSize()).toEqual({
                x: 300,
                y: 200
            });
        });

        it("should return the content size with x:scroll and y:false", function() {
            makeScroller({
                x: 'scroll',
                y: false
            });

            expect(scroller.getSize()).toEqual({
                x: 300,
                y: 200
            });
        });
    });

    describe("setSize", function() {
        it("should set the size", function() {
            makeScroller();
            scroller.setSize({
                x: 300,
                y: 200
            });

            expect(scroller.getSize()).toEqual({
                x: 300,
                y: 200
            });
        });

        it("should unset the size", function() {
            makeScroller();
            scroller.setSize({
                x: 300,
                y: 200
            });

            scroller.setSize(null);

            expect(scroller.getSize()).toEqual({
                x: 100,
                y: 100
            });
        });

        it("should set the size on both axes to a single number", function() {
            makeScroller();
            scroller.setSize(200);

            expect(scroller.getSize()).toEqual({
                x: 200,
                y: 200
            });
        });

        it("should set the x size", function() {
            makeScroller();
            scroller.setSize({
                x: 200
            });

            expect(scroller.getSize()).toEqual({
                x: 200,
                y: 100 - Ext.getScrollbarSize().height
            });
        });

        it("should set the y size", function() {
            makeScroller();
            scroller.setSize({
                y: 200
            });

            expect(scroller.getSize()).toEqual({
                x: 100 - Ext.getScrollbarSize().width,
                y: 200
            });
        });

        it("should not change the existing y size when setting the x size", function() {
            makeScroller();

            scroller.setSize({
                y: 300
            });

            scroller.setSize({
                x: 200
            });

            expect(scroller.getSize()).toEqual({
                x: 200,
                y: 300
            });
        });

        it("should not change the existing x size when setting the y size", function() {
            makeScroller();

            scroller.setSize({
                x: 300
            });

            scroller.setSize({
                y: 200
            });

            expect(scroller.getSize()).toEqual({
                x: 300,
                y: 200
            });
        });

        it("should reset the x size using 0", function() {
            makeScroller();

            scroller.setSize(300);

            scroller.setSize({
                x: 0
            });

            expect(scroller.getSize()).toEqual({
                x: 100 - Ext.getScrollbarSize().width,
                y: 300
            });
        });

        it("should reset the y size using 0", function() {
            makeScroller();

            scroller.setSize(300);

            scroller.setSize({
                y: 0
            });

            expect(scroller.getSize()).toEqual({
                x: 300,
                y: 100 - Ext.getScrollbarSize().height
            });
        });

        it("should reset the x size using null", function() {
            makeScroller();

            scroller.setSize(300);

            scroller.setSize({
                x: null
            });

            expect(scroller.getSize()).toEqual({
                x: 100 - Ext.getScrollbarSize().width,
                y: 300
            });
        });

        it("should reset the y size using null", function() {
            makeScroller();

            scroller.setSize(300);

            scroller.setSize({
                y: null
            });

            expect(scroller.getSize()).toEqual({
                x: 300,
                y: 100 - Ext.getScrollbarSize().height
            });
        });
    });

    describe("getClientSize", function() {
        beforeEach(function() {
            el.destroy();

            el = Ext.getBody().createChild({
                style: {
                    height: '200px',
                    width: '200px',
                    borderColor: 'red',
                    borderStyle: 'solid',
                    borderWidth: '10px 20px',
                    padding: '30px 40px'
                }
            });
        });

        function makeNumbers() {
            var content = [],
                i;

            for (i = 0; i < 100; ++i) {
                content.push(i);
            }

            return content;
        }

        it("should return the clientWidth of the element", function() {
            el.setHtml(makeNumbers().join('<br />'));

            makeScroller();

            var size = scroller.getClientSize();
            expect(size.x).toBe(200 - (20 * 2) - Ext.getScrollbarSize().width);
            expect(size.y).toBe(200 - (10 * 2));
        });

        it("should return the clientHeight of the element", function() {
            el.setHtml(makeNumbers().join(''));

            makeScroller();

            var size = scroller.getClientSize();
            expect(size.x).toBe(200 - (20 * 2));
            expect(size.y).toBe(200 - (10 * 2) - Ext.getScrollbarSize().height);
        });

        it("should read by the clientWidth and clientHeight of the element", function() {
            var content = makeNumbers();
            content[0] = makeNumbers().join('');
            el.setHtml(content.join('<br />'));

            makeScroller();

            var size = scroller.getClientSize();
            expect(size.x).toBe(200 - (20 * 2) - Ext.getScrollbarSize().width);
            expect(size.y).toBe(200 - (10 * 2) - Ext.getScrollbarSize().height);
        });
    });

    describe("getPosition", function() {
        beforeEach(function() {
            el.appendChild({
                style: 'height:200px;width:300px;'
            }, true);
        });

        it("should return the current position", function() {
            makeScroller();

            expect(scroller.getPosition()).toEqual({
                x: 0,
                y: 0
            });

            scroller.scrollTo(20, 40);

            expect(scroller.getPosition()).toEqual({
                x: 20,
                y: 40
            });
        });
    });

    describe('getEnsureVisibleXY', function () {
        describe('el', function () {
            function createElSpec (asObj) {
                describe(asObj ? 'as object' : 'as node', function () {
                    function parseEl (el) {
                        if (asObj) {
                            el = {
                                element: el
                            };
                        }

                        return el;
                    }

                    it('should accept el as first argument', function () {
                        makeOverflow();

                        var el = getChild(10),
                            position = scroller.getEnsureVisibleXY(parseEl(el));

                        expect(position.x).toBe(0);
                        expect(position.y).toBe(120);
                    });

                    it('should accept el as an Element as first argument', function () {
                        makeOverflow();

                        var el = getChild(10, true),
                            position = scroller.getEnsureVisibleXY(parseEl(el));

                        expect(position.x).toBe(0);
                        expect(position.y).toBe(120);
                    });

                    it('should accept el as a String as first argument', function () {
                        makeOverflow();

                        var el = getChild(10),
                            position = scroller.getEnsureVisibleXY(parseEl(el.id));

                        expect(position.x).toBe(0);
                        expect(position.y).toBe(120);
                    });
                });
            }

            createElSpec();
            createElSpec(true);
        });

        describe('options', function () {
            describe('align', function () {
                function makeSpecs (align, x, y, asString) {
                    function buildAlign (axis) {
                        if (asString) {
                            return align;
                        } else {
                            var cfg = {};

                            cfg[axis] = align;

                            return cfg;
                        }
                    }

                    describe(align + (asString ? ' as string' : ''), function () {
                        it('should align by x', function () {
                            makeInline();

                            child = getChild(40);

                            var position = scroller.getEnsureVisibleXY(child, {
                                align : buildAlign('x'),
                                y     : false
                            });

                            expect(position.x).toBe(x);
                            expect(position.y).toBe(0);
                        });

                        it('should align by y', function () {
                            makeOverflow();

                            child = getChild(50);

                            var position = scroller.getEnsureVisibleXY(child, {
                                align : buildAlign('y'),
                                x     : false
                            });

                            expect(position.x).toBe(0);
                            expect(position.y).toBe(y);
                        });
                    });
                }

                makeSpecs('center', 1950, 950);
                makeSpecs('center', 1950, 950, true);
                makeSpecs('end',    1950, 920);
                makeSpecs('end',    1950, 920, true);
                makeSpecs('start',  2000, 1000);
                makeSpecs('start',  2000, 1000, true);
            });

            function makeXYSpec (axis, x, y) {
                describe(axis, function () {
                    beforeEach(function () {
                        if (axis === 'x') {
                            makeInline();
                        } else {
                            makeOverflow();
                        }

                        child = getChild(70);
                    });

                    function buildOptions (value) {
                        var options = {};

                        options[axis] = value;

                        return options;
                    }

                    it('should allow scrolling', function () {
                        var position = scroller.getEnsureVisibleXY(child);

                        expect(position.x).toBe(x);
                        expect(position.y).toBe(y);
                    });

                    it('should allow scrolling passing true', function () {
                        var position = scroller.getEnsureVisibleXY(child, buildOptions(true));

                        expect(position.x).toBe(x);
                        expect(position.y).toBe(y);
                    });

                    it('should disallow scrolling', function () {
                        var position = scroller.getEnsureVisibleXY(child, buildOptions(false));

                        expect(position.x).toBe(0);
                        expect(position.y).toBe(0);
                    });
                });
            }

            makeXYSpec('x', 3450, 0);
            makeXYSpec('y', 0,    1320);
        });
    });

    xdescribe("scrolling methods", function() {
        describe("scrollTo", function() {
            var contentWidth = 300,
                contentHeight = 200;

            it("should scroll on the x axis", function() {
                makeOverflow();

                scroller.scrollTo(50, 0);

                expect(scroller.getPosition()).toEqual({
                    x: 50,
                    y: 0
                });
            });

            it("should scroll on the x axis when the x axis is disabled", function() {
                makeOverflow({
                    x: false
                });

                scroller.scrollTo(50, 0);

                expect(scroller.getPosition()).toEqual({
                    x: 50,
                    y: 0
                });
            });

            it("should not scroll on the x axis if the content does not overflow horizontally", function() {
                makeNoOverflow();

                scroller.scrollTo(50, 0);

                expect(scroller.getPosition()).toEqual({
                    x: 0,
                    y: 0
                });
            });

            it("should constrain to the max x position", function() {
                makeOverflow();

                scroller.scrollTo(250, 0);

                expect(scroller.getPosition()).toEqual({
                    x: 200 + Ext.getScrollbarSize().width,
                    y: 0
                });
            });

            it("should scroll on the y axis", function() {
                makeOverflow();

                scroller.scrollTo(0, 50);

                expect(scroller.getPosition()).toEqual({
                    x: 0,
                    y: 50
                });
            });

            it("should scroll on the y axis when the y axis is disabled", function() {
                makeOverflow({
                    y: false
                });

                scroller.scrollTo(0, 50);

                expect(scroller.getPosition()).toEqual({
                    x: 0,
                    y: 50
                });
            });

            it("should not scroll on the y axis if the content does not overflow vertically", function() {
                makeNoOverflow();

                scroller.scrollTo(0, 50);

                expect(scroller.getPosition()).toEqual({
                    x: 0,
                    y: 0
                });
            });

            it("should constrain to the max y position", function() {
                makeOverflow();

                scroller.scrollTo(0, 250);

                expect(scroller.getPosition()).toEqual({
                    x: 0,
                    y: 100 + Ext.getScrollbarSize().height
                });
            });

            it("should scroll on both axes", function() {
                makeOverflow();

                scroller.scrollTo(50, 60);

                expect(scroller.getPosition()).toEqual({
                    x: 50,
                    y: 60
                });
            });

            it("should constrain to max x and y", function() {
                makeOverflow();

                scroller.scrollTo(300, 300);

                expect(scroller.getPosition()).toEqual({
                    x: 200 + Ext.getScrollbarSize().width,
                    y: 100 + Ext.getScrollbarSize().height
                });
            });

            it("should scroll to max x using Infinity", function() {
                makeOverflow();

                scroller.scrollTo(Infinity, 0);

                expect(scroller.getPosition()).toEqual({
                    x: 200 + Ext.getScrollbarSize().height,
                    y: 0
                });
            });

            it("should scroll to max y using Infinity", function() {
                makeOverflow();

                scroller.scrollTo(0, Infinity);

                expect(scroller.getPosition()).toEqual({
                    x: 0,
                    y: 100 + Ext.getScrollbarSize().width
                });
            });

            it("should scroll to max x and y using Infinity", function() {
                makeOverflow();

                scroller.scrollTo(Infinity, Infinity);

                expect(scroller.getPosition()).toEqual({
                    x: 200 + Ext.getScrollbarSize().height,
                    y: 100 + Ext.getScrollbarSize().width
                });
            });

            it("should ignore x if null is passed", function() {
                makeOverflow();

                scroller.scrollTo(10, 10);

                scroller.scrollTo(null, 20);

                expect(scroller.getPosition()).toEqual({
                    x: 10,
                    y: 20
                });
            });

            it("should ignore y if null is passed", function() {
                makeOverflow();

                scroller.scrollTo(10, 10);

                scroller.scrollTo(20, null);

                expect(scroller.getPosition()).toEqual({
                    x: 20,
                    y: 10
                });
            });

            it("should ignore x and y if both null", function() {
                makeOverflow();

                scroller.scrollTo(10, 10);

                scroller.scrollTo(null, null);

                expect(scroller.getPosition()).toEqual({
                    x: 10,
                    y: 10
                });
            });

            it("should scroll to negative offset from max x", function() {
                makeOverflow();

                scroller.scrollTo(-20, 0);

                expect(scroller.getPosition()).toEqual({
                    x: 180 + Ext.getScrollbarSize().height,
                    y: 0
                });
            });

            it("should scroll to negative offset from max y", function() {
                makeOverflow();

                scroller.scrollTo(0, -20);

                expect(scroller.getPosition()).toEqual({
                    x: 0,
                    y: 80 + Ext.getScrollbarSize().width
                });
            });

            it("should scroll to negative offset from max x and y", function() {
                makeOverflow();

                scroller.scrollTo(-20, -20);

                expect(scroller.getPosition()).toEqual({
                    x: 180 + Ext.getScrollbarSize().height,
                    y: 80 + Ext.getScrollbarSize().width
                });
            });

            describe("promise return value", function() {
                var spy;

                beforeEach(function() {
                    spy = jasmine.createSpy();
                    makeOverflow();
                });

                afterEach(function() {
                    spy = null;
                });

                function scrollAndWait(x, y, animate, expectedX, expectedY) {
                    var promise = scroller.scrollTo(x, y, animate);
                    promise.then(spy);
                    waitsForSpy(spy);
                    runs(function() {
                        var xVal = x === null ? 0 : x,
                            yVal = y === null ? 0 : y;

                        expectedX = expectedX === undefined ? xVal : expectedX;
                        expectedY = expectedY === undefined ? yVal : expectedY;

                        expect(spy.callCount).toBe(1);
                        expect(scroller.getPosition()).toEqual({
                            x: expectedX,
                            y: expectedY
                        });
                    });
                }

                Ext.Array.forEach([false, true], function(animate) {
                    describe(animate ? "with animation" : "without animation", function() {
                        it("should resolve if the position doesn't change", function() {
                            scrollAndWait(0, 0, animate);
                        });

                        it("should resolve if only the x value changes", function() {
                            scrollAndWait(50, null, animate);
                        });

                        it("should resolve if only the y value changes", function() {
                            scrollAndWait(null, 50, animate);
                        });

                        it("should resolve when both values change", function() {
                            scrollAndWait(50, 50, animate);
                        });

                        it("should have the correct value if past scroll boundaries", function() {
                            var max = calculateMaxScrollPosition(contentWidth, contentHeight);
                            scrollAndWait(3000, 3000, animate, max.x, max.y);
                        });

                        it("should reject if there is no element available", function() {
                            scroller.setElement(null);
                            var promise = scroller.scrollTo(50, 50, animate).then(null, spy);
                            waitsForSpy(spy);
                            runs(function() {
                                expect(spy.callCount).toBe(1);
                            });
                        });

                        if (animate) {
                            it("should reject if destroyed during animation", function() {
                                var promise = scroller.scrollTo(50, 50, animate).then(null, spy);
                                waits(1);
                                runs(function() {
                                    scroller.destroy();
                                });
                                waitsForSpy(spy);
                                runs(function() {
                                    expect(spy.callCount).toBe(1);
                                });
                            });
                        }
                    });
                });
            });

            it("should fire scroll events if calling getPosition after scrolling", function() {
                makeOverflow();
                var spy = jasmine.createSpy();
                scroller.on('scroll', spy);
                scroller.scrollTo(20, 40);
                scroller.getPosition();
                waitsForSpy(spy);
                runs(function() {
                    var args = spy.mostRecentCall.args;
                    expect(spy.callCount).toBe(1);
                    expect(args[0]).toBe(scroller);
                    expect(args[1]).toBe(20);
                    expect(args[2]).toBe(40);
                });
            });
        });

        describe("scrollBy", function() {
            var contentWidth = 300,
                contentHeight = 200;

            beforeEach(function() {
                el.appendChild({
                    style: {
                        height: contentHeight + 'px',
                        width: contentWidth + 'px'
                    }
                }, true);
            });

            it("should set the scroll position", function() {
                makeScroller();

                scroller.scrollBy(20, 10);

                expect(scroller.getPosition()).toEqual({
                    x: 20,
                    y: 10
                });

                scroller.scrollBy(-10, -5);

                expect(scroller.getPosition()).toEqual({
                    x: 10,
                    y: 5
                });
            });

            it("should ignore x if null is passed", function() {
                makeScroller();

                scroller.scrollTo(10, 10);

                scroller.scrollBy(null, 10);

                expect(scroller.getPosition()).toEqual({
                    x: 10,
                    y: 20
                });
            });

            it("should ignore y if null is passed", function() {
                makeScroller();

                scroller.scrollTo(10, 10);

                scroller.scrollBy(10, null);

                expect(scroller.getPosition()).toEqual({
                    x: 20,
                    y: 10
                });
            });

            it("should ignore x and y if both null", function() {
                makeScroller();

                scroller.scrollTo(10, 10);

                scroller.scrollBy(null, null);

                expect(scroller.getPosition()).toEqual({
                    x: 10,
                    y: 10
                });
            });

            it("should constrain to the max x position", function() {
                makeScroller();

                scroller.scrollBy(250, 0);

                expect(scroller.getPosition()).toEqual({
                    x: 200 + Ext.getScrollbarSize().height,
                    y: 0
                });
            });

            it("should constrain to the min x position", function() {
                makeScroller();

                scroller.scrollBy(-10, 0);

                expect(scroller.getPosition()).toEqual({
                    x: 0,
                    y: 0
                });
            });

            it("should constrain to the max y position", function() {
                makeScroller();

                scroller.scrollBy(0, 250);

                expect(scroller.getPosition()).toEqual({
                    x: 0,
                    y: 100 + Ext.getScrollbarSize().width
                });
            });

            it("should constrain to the min y position", function() {
                makeScroller();

                scroller.scrollBy(0, -10);

                expect(scroller.getPosition()).toEqual({
                    x: 0,
                    y: 0
                });
            });

            it("should constrain to max x and y", function() {
                makeScroller();

                scroller.scrollBy(300, 300);

                expect(scroller.getPosition()).toEqual({
                    x: 200 + Ext.getScrollbarSize().height,
                    y: 100 + Ext.getScrollbarSize().width
                });
            });

            it("should constrain to min x and y", function() {
                makeScroller();

                scroller.scrollBy(-10, -10);

                expect(scroller.getPosition()).toEqual({
                    x: 0,
                    y: 0
                });
            });

            describe("promise return value", function() {
                var spy;

                beforeEach(function() {
                    spy = jasmine.createSpy();
                    makeScroller();
                });

                afterEach(function() {
                    spy = null;
                });

                function scrollAndWait(x, y, animate, expectedX, expectedY) {
                    var promise = scroller.scrollBy(x, y, animate);
                    promise.then(spy);
                    waitsForSpy(spy);
                    runs(function() {
                        var xVal = x === null ? 0 : x,
                            yVal = y === null ? 0 : y;

                        expectedX = expectedX === undefined ? xVal : expectedX;
                        expectedY = expectedY === undefined ? yVal : expectedY;

                        expect(spy.callCount).toBe(1);
                        expect(scroller.getPosition()).toEqual({
                            x: expectedX,
                            y: expectedY
                        });
                    });
                }

                Ext.Array.forEach([false, true], function(animate) {
                    describe(animate ? "with animation" : "without animation", function() {
                        it("should resolve if the position doesn't change", function() {
                            scrollAndWait(0, 0, animate);
                        });

                        it("should resolve if only the x value changes", function() {
                            scrollAndWait(50, null, animate);
                        });

                        it("should resolve if only the y value changes", function() {
                            scrollAndWait(null, 50, animate);
                        });

                        it("should resolve when both values change", function() {
                            scrollAndWait(50, 50, animate);
                        });

                        it("should have the correct value if past scroll boundaries", function() {
                            var max = calculateMaxScrollPosition(contentWidth, contentHeight);
                            scrollAndWait(3000, 3000, animate, max.x, max.y);
                        });

                        it("should reject if there is no element available", function() {
                            scroller.setElement(null);
                            var promise = scroller.scrollBy(50, 50, animate).then(null, spy);
                            waitsForSpy(spy);
                            runs(function() {
                                expect(spy.callCount).toBe(1);
                            });
                        });

                        if (animate) {
                            it("should reject if destroyed during animation", function() {
                                var promise = scroller.scrollBy(50, 50, animate).then(null, spy);
                                waits(1);
                                runs(function() {
                                    scroller.destroy();
                                });
                                waitsForSpy(spy);
                                runs(function() {
                                    expect(spy.callCount).toBe(1);
                                });
                            });
                        }
                    });
                });
            });

            it("should fire scroll events if calling getPosition after scrolling", function() {
                makeScroller();
                var spy = jasmine.createSpy();
                scroller.on('scroll', spy);
                scroller.scrollBy(20, 40);
                scroller.getPosition();
                waitsForSpy(spy);
                runs(function() {
                    var args = spy.mostRecentCall.args;
                    expect(spy.callCount).toBe(1);
                    expect(args[0]).toBe(scroller);
                    expect(args[1]).toBe(20);
                    expect(args[2]).toBe(40);
                });
            });
        });

        describe("ensureVisible", function() {
            // TODO: add tests for ensureVisible

            describe("promise return value", function() {
                var spy;

                beforeEach(function() {
                    spy = jasmine.createSpy();
                });

                afterEach(function() {
                    spy = null;
                });

                function scrollAndWait(el, hscroll, animate, expectedX, expectedY) {
                    if (typeof el === 'number') {
                        el = getChild(el);
                    }
                    var promise = scroller.ensureVisible(el, {
                        animation: animate,
                        x: hscroll
                    });
                    promise.then(spy);
                    waitsForSpy(spy);
                    runs(function() {
                        expect(spy.callCount).toBe(1);
                        expect(scroller.isInView(el)).toEqual({
                            x: true,
                            y: true
                        });
                    });
                }

                Ext.Array.forEach([false, true], function(animate) {
                    describe(animate ? "with animation" : "without animation", function() {
                        it("should resolve if the position doesn't change", function() {
                            makeOverflow();
                            scrollAndWait(0, null, animate, 0, 0);
                        });

                        it("should resolve if only the x value changes", function() {
                            makeAbsoluteOverflow(200, 0);
                            scrollAndWait(0, null, animate, 200, 0);
                        });

                        it("should resolve if only the y value changes", function() {
                            makeOverflow();
                            scrollAndWait(50, null, animate, 0, 920);
                        });

                        it("should resolve when both values change", function() {
                            makeAbsoluteOverflow(200, 200);
                            scrollAndWait(0, null, animate, 200, 200);
                        });

                        if (animate) {
                            it("should reject if destroyed during animation", function() {
                                makeOverflow();
                                var promise = scroller.ensureVisible(getChild(20), {
                                    animation: animate,
                                    x: null
                                }).then(null, spy);
                                waits(1);
                                runs(function() {
                                    scroller.destroy();
                                });
                                waitsForSpy(spy);
                                runs(function() {
                                    expect(spy.callCount).toBe(1);
                                });
                            });
                        }
                    });
                });
            });
        });
    });

    describe("getMaxPosition and getMaxUserPosition", function() {
        beforeEach(function() {
            el.appendChild({
                style: 'height:200px;width:300px;'
            }, true);
        });

        describe("with x:true and y:true", function() {
            beforeEach(function() {
                makeScroller();
            });

            it("should return the maxPosition", function() {
                expect(scroller.getMaxPosition()).toEqual({
                    x: 200 + Ext.getScrollbarSize().height,
                    y: 100 + Ext.getScrollbarSize().width
                });
            });

            it("should return the maxUserPosition", function() {
                expect(scroller.getMaxUserPosition()).toEqual({
                    x: 200 + Ext.getScrollbarSize().height,
                    y: 100 + Ext.getScrollbarSize().width
                });
            });
        });

        describe("with x:true and y:false", function() {
            beforeEach(function() {
                makeScroller({
                    x: true,
                    y: false
                });
            });

            it("should return the maxPosition", function() {
                expect(scroller.getMaxPosition()).toEqual({
                    x: 200,
                    y: 100 + Ext.getScrollbarSize().width
                });
            });

            it("should return the maxUserPosition", function() {
                expect(scroller.getMaxUserPosition()).toEqual({
                    x: 200,
                    y: 0
                });
            });
        });

        describe("with x:false and y:true", function() {
            beforeEach(function() {
                makeScroller({
                    x: false,
                    y: true
                });
            });

            it("should return the maxPosition", function() {
                expect(scroller.getMaxPosition()).toEqual({
                    x: 200 + Ext.getScrollbarSize().height,
                    y: 100
                });
            });

            it("should return the maxUserPosition", function() {
                expect(scroller.getMaxUserPosition()).toEqual({
                    x: 0,
                    y: 100
                });
            });
        });

        describe("with x:false and y:false", function() {
            beforeEach(function() {
                makeScroller({
                    x: false,
                    y: false
                });
            });

            it("should return the maxPosition", function() {
                expect(scroller.getMaxPosition()).toEqual({
                    x: 200,
                    y: 100
                });
            });

            it("should return the maxUserPosition", function() {
                expect(scroller.getMaxUserPosition()).toEqual({
                    x: 0,
                    y: 0
                });
            });
        });
    });

    describe("events", function() {
        var w = 200,
            h = 300,
            spy, oldBuffer;

        function makeEventScroll(x, y) {
            makeScroller({
                x: x,
                y: y 
            });
            appendEl(w, h);
        }

        beforeEach(function() {
            spy = jasmine.createSpy();

            var P = Ext.scroll.Scroller.prototype;

            // The default runner modifies these
            oldBuffer = P.scrollEndBuffer;
            P.scrollEndBuffer = 100;
        });

        afterEach(function() {
            Ext.scroll.Scroller.prototype.scrollEndBuffer = oldBuffer;
            spy = oldBuffer = null;
        });

        describe("scrollstart", function() {
            function makeSuite(x, y, negative) {
                var offset = negative ? -1 : 1,
                    offsetX = x ? 20 : 0,
                    offsetY = y ? 20 : 0,
                    scrollByX = x ? offsetX * offset : null,
                    scrollByY = y ? offsetY * offset : null,
                    endSpy;

                beforeEach(function() {
                    var called = false;
                    endSpy = jasmine.createSpy();

                    makeEventScroll(x, y);
                    if (negative) {
                        scroller.on('scrollend', function() {
                            called = true;
                        }, null, {single: true});
                        scroller.scrollTo(x ? w : null, y ? 300 : null, false);
                    } else {
                        called = true;
                    }

                    waitsFor(function() {
                        return called;
                    });

                    runs(function() {
                        scroller.on({
                            scrollstart: spy,
                            scrollend: endSpy
                        });
                    });
                });

                afterEach(function() {
                    endSpy = null;
                });

                function getValue(xVal, yVal) {
                    var xy = {
                        x: 0,
                        y: 0
                    };

                    if (x) {
                        xy.x = xVal;
                        if (negative) {
                            xy.x = w - ctWidth - xy.x;
                            if (y) {
                                xy.x += Ext.getScrollbarSize().width;
                            }
                        }
                    }

                    if (y) {
                        xy.y = yVal;
                        if (negative) {
                            xy.y = h - ctHeight - xy.y;
                            if (x) {
                                xy.y += Ext.getScrollbarSize().height;
                            }
                        }

                    }   
                    return xy;
                }

                it("should fire the event", function() {
                    scroller.scrollBy(scrollByX, scrollByY);
                    waitsForSpy(spy);
                    runs(function() {
                        expect(spy.callCount).toBe(1);
                        var args = spy.mostRecentCall.args,
                            value = getValue(offsetX, offsetY);

                        expect(args[0]).toBe(scroller);
                        expect(args[1]).toBe(value.x);
                        expect(args[2]).toBe(value.y);
                    });
                    waitsForSpy(endSpy);
                });

                it("should only fire at the start", function() {
                    var scrollSpy = jasmine.createSpy(),
                        wait = function() {
                            return scrollSpy.callCount > 0;
                        },
                        scrollIt = function() {
                            scrollSpy.reset();
                            scroller.scrollBy(scrollByX, scrollByY);
                        };

                    scroller.on('scroll', scrollSpy);

                    runs(scrollIt);
                    waitsFor(wait);
                    
                    runs(scrollIt);
                    waitsFor(wait);

                    runs(scrollIt);
                    waitsFor(wait);

                    waitsForSpy(endSpy);
                    runs(function() {
                        expect(spy.callCount).toBe(1);
                        var args = spy.mostRecentCall.args,
                            value = getValue(offsetX, offsetY);

                        expect(args[0]).toBe(scroller);
                        expect(args[1]).toBe(value.x);
                        expect(args[2]).toBe(value.y);
                    });
                });

                it("should fire for each independent scroll", function() {
                    scroller.scrollBy(scrollByX, scrollByY);
                    waitsForSpy(endSpy);
                    runs(function() {
                        expect(spy.callCount).toBe(1);
                        var args = spy.mostRecentCall.args,
                            value = getValue(offsetX, offsetY);

                        expect(args[0]).toBe(scroller);
                        expect(args[1]).toBe(value.x);
                        expect(args[2]).toBe(value.y);
                    });

                    runs(function() {
                        endSpy.reset();
                        scroller.scrollBy(scrollByX, scrollByY);
                    });

                    waitsForSpy(endSpy);
                    runs(function() {
                        expect(spy.callCount).toBe(2);
                        var args = spy.mostRecentCall.args,
                            value = getValue(offsetX * 2, offsetY * 2);

                        expect(args[0]).toBe(scroller);
                        expect(args[1]).toBe(value.x);
                        expect(args[2]).toBe(value.y);
                    });
                });

                it("should not fire if a scroll doesn't occur", function() {
                    scroller.scrollBy(-scrollByX * 1000, -scrollByY * 1000);
                    waits(200);
                    runs(function() {
                        expect(spy).not.toHaveBeenCalled();
                    });
                });
            }

            describe("x only", function() {
                describe("positive", function() {
                    makeSuite(true, false, false);
                });

                describe("negative", function() {
                    makeSuite(true, false, true);
                });
            });

            describe("y only", function() {
                describe("positive", function() {
                    makeSuite(false, true, false);
                });

                describe("negative", function() {
                    makeSuite(false, true, true);
                });
            });

            describe("both", function() {
                describe("positive", function() {
                    makeSuite(true, true, false);
                });

                describe("negative", function() {
                    makeSuite(true, true, true);
                });
            });
        });

        describe("scroll", function() {
            function makeSuite(x, y, negative) {
                var offset = negative ? -1 : 1,
                    offsetX = x ? 20 : 0,
                    offsetY = y ? 20 : 0,
                    scrollByX = x ? offsetX * offset : null,
                    scrollByY = y ? offsetY * offset : null,
                    endSpy;

                beforeEach(function() {
                    var called = false;
                    endSpy = jasmine.createSpy();

                    makeEventScroll(x, y);
                    if (negative) {
                        scroller.on('scrollend', function() {
                            called = true;
                        }, null, {single: true});
                        scroller.scrollTo(x ? w : null, y ? 300 : null, false);
                    } else {
                        called = true;
                    }

                    waitsFor(function() {
                        return called;
                    });

                    runs(function() {
                        scroller.on({
                            scroll: spy,
                            scrollend: endSpy
                        });
                    });
                });

                afterEach(function() {
                    endSpy = null;
                });

                function getValue(xVal, yVal) {
                    var xy = {
                        x: 0,
                        y: 0
                    };

                    if (x) {
                        xy.x = xVal;
                        if (negative) {
                            xy.x = w - ctWidth - xy.x;
                            if (y) {
                                xy.x += Ext.getScrollbarSize().width;
                            }
                        }
                    }

                    if (y) {
                        xy.y = yVal;
                        if (negative) {
                            xy.y = h - ctHeight - xy.y;
                            if (x) {
                                xy.y += Ext.getScrollbarSize().height;
                            }
                        }

                    }   
                    return xy;
                }

                it("should only fire at the start", function() {
                    var wait = function() {
                            return spy.callCount > 0;
                        },
                        scrollIt = function() {
                            spy.reset();
                            scroller.scrollBy(scrollByX, scrollByY);
                        },
                        expectArgs = function(x, y, dx, dy) {
                            expect(spy.callCount).toBe(1);
                            var args = spy.mostRecentCall.args,
                                value = getValue(x, y);

                            expect(args[0]).toBe(scroller);
                            expect(args[1]).toBe(value.x);
                            expect(args[2]).toBe(value.y);
                            expect(args[3]).toBe(x ? dx : 0);
                            expect(args[4]).toBe(y ? dy : 0);
                        };

                    runs(scrollIt);
                    waitsFor(wait);
                    runs(function() {
                        expectArgs(offsetX, offsetY, scrollByX, scrollByY);
                    });
                    
                    runs(scrollIt);
                    waitsFor(wait);
                    runs(function() {
                        expectArgs(offsetX * 2, offsetY * 2, scrollByX, scrollByY);
                    });

                    runs(scrollIt);
                    waitsFor(wait);
                    runs(function() {
                        expectArgs(offsetX * 3, offsetY * 3, scrollByX, scrollByY);
                    });

                    waitsForSpy(endSpy);
                });

                it("should fire as deltas go positive to negative", function() {
                    var count = 0,
                        offset,
                        wait = function() {
                            return spy.callCount > 0;
                        },
                        scrollIt = function() {
                            offset = count % 2 === 0 ? 1 : -1;

                            ++count;
                            spy.reset();
                            scroller.scrollBy(x ? scrollByX * offset : null, y ? scrollByY * offset : null);
                        },
                        expectPos = function() {
                            expect(spy.callCount).toBe(1);
                            var args = spy.mostRecentCall.args,
                                value;

                            expect(args[0]).toBe(scroller);

                            if (offset === 1) {
                                value = getValue(offsetX, offsetY);
                                expect(args[1]).toBe(value.x);
                                expect(args[2]).toBe(value.y);
                                expect(args[3]).toBe(x ? scrollByX : 0);
                                expect(args[4]).toBe(y ? scrollByY : 0);
                            } else {
                                expect(args[0]).toBe(scroller);
                                expect(args[1]).toBe(start.x);
                                expect(args[2]).toBe(start.y);
                                expect(args[3]).toBe(x ? -scrollByX : 0);
                                expect(args[4]).toBe(y ? -scrollByY : 0);
                            }
                        };

                    var start = scroller.getPosition();

                    runs(scrollIt);
                    waitsFor(wait);
                    runs(expectPos);
                    
                    runs(scrollIt);
                    waitsFor(wait);
                    runs(expectPos);

                    runs(scrollIt);
                    waitsFor(wait);
                    runs(expectPos);

                    runs(scrollIt);
                    waitsFor(wait);
                    runs(expectPos);

                    waitsForSpy(endSpy);
                });
            }

            describe("x only", function() {
                describe("positive", function() {
                    makeSuite(true, false, false);
                });

                describe("negative", function() {
                    makeSuite(true, false, true);
                });
            });

            describe("y only", function() {
                describe("positive", function() {
                    makeSuite(false, true, false);
                });

                describe("negative", function() {
                    makeSuite(false, true, true);
                });
            });

            describe("both", function() {
                describe("positive", function() {
                    makeSuite(true, true, false);
                });

                describe("negative", function() {
                    makeSuite(true, true, true);
                });
            });
        });

        describe("scrollend", function() {
            function makeSuite(x, y, negative) {
                var offset = negative ? -1 : 1,
                    offsetX = x ? 20 : 0,
                    offsetY = y ? 20 : 0,
                    scrollByX = x ? offsetX * offset : null,
                    scrollByY = y ? offsetY * offset : null,
                    endSpy;

                beforeEach(function() {
                    var called = false;

                    makeEventScroll(x, y);
                    if (negative) {
                        scroller.on('scrollend', function() {
                            called = true;
                        }, null, {single: true});
                        scroller.scrollTo(x ? w : null, y ? 300 : null, false);
                    } else {
                        called = true;
                    }

                    waitsFor(function() {
                        return called;
                    });

                    runs(function() {
                        scroller.on('scrollend', spy);
                    });
                });

                function getValue(xVal, yVal) {
                    var xy = {
                        x: 0,
                        y: 0
                    };

                    if (x) {
                        xy.x = xVal;
                        if (negative) {
                            xy.x = w - ctWidth - xy.x;
                            if (y) {
                                xy.x += Ext.getScrollbarSize().width;
                            }
                        }
                    }

                    if (y) {
                        xy.y = yVal;
                        if (negative) {
                            xy.y = h - ctHeight - xy.y;
                            if (x) {
                                xy.y += Ext.getScrollbarSize().height;
                            }
                        }

                    }   
                    return xy;
                }

                it("should fire the event", function() {
                    scroller.scrollBy(scrollByX, scrollByY);
                    waitsForSpy(spy);
                    runs(function() {
                        expect(spy.callCount).toBe(1);
                        var args = spy.mostRecentCall.args,
                            value = getValue(offsetX, offsetY);

                        expect(args[0]).toBe(scroller);
                        expect(args[1]).toBe(value.x);
                        expect(args[2]).toBe(value.y);
                        expect(args[3]).toBe(x ? scrollByX : 0);
                        expect(args[4]).toBe(y ? scrollByY : 0);
                    });
                });

                it("should only fire at the end", function() {
                    var scrollSpy = jasmine.createSpy(),
                        wait = function() {
                            return scrollSpy.callCount > 0;
                        },
                        scrollIt = function() {
                            scrollSpy.reset();
                            expect(spy.callCount).toBe(0);
                            scroller.scrollBy(scrollByX, scrollByY);
                        };

                    scroller.on('scroll', scrollSpy);

                    runs(scrollIt);
                    waitsFor(wait);
                    
                    runs(scrollIt);
                    waitsFor(wait);

                    runs(scrollIt);
                    waitsFor(wait);

                    waitsForSpy(spy);
                    runs(function() {
                        expect(spy.callCount).toBe(1);
                        var args = spy.mostRecentCall.args,
                            value = getValue(offsetX * 3, offsetY * 3);

                        expect(args[0]).toBe(scroller);
                        expect(args[1]).toBe(value.x);
                        expect(args[2]).toBe(value.y);
                        expect(args[3]).toBe(x ? scrollByX * 3 : 0);
                        expect(args[4]).toBe(y ? scrollByY * 3 : 0);
                    });
                });

                it("should fire for each independent scroll", function() {
                    scroller.scrollBy(scrollByX, scrollByY);

                    waitsForSpy(spy);
                    runs(function() {
                        expect(spy.callCount).toBe(1);
                        var args = spy.mostRecentCall.args,
                            value = getValue(offsetX, offsetY);

                        expect(args[0]).toBe(scroller);
                        expect(args[1]).toBe(value.x);
                        expect(args[2]).toBe(value.y);
                        expect(args[3]).toBe(x ? scrollByX : 0);
                        expect(args[4]).toBe(y ? scrollByY : 0);
                    });

                    runs(function() {
                        spy.reset();
                        scroller.scrollBy(scrollByX, scrollByY);
                    });

                    waitsForSpy(spy);
                    runs(function() {
                        expect(spy.callCount).toBe(1);
                        var args = spy.mostRecentCall.args,
                            value = getValue(offsetX * 2, offsetY * 2);

                        expect(args[0]).toBe(scroller);
                        expect(args[1]).toBe(value.x);
                        expect(args[2]).toBe(value.y);
                        expect(args[3]).toBe(x ? scrollByX : 0);
                        expect(args[4]).toBe(y ? scrollByY : 0);
                    });
                });

                it("should not fire if a scroll doesn't occur", function() {
                    scroller.scrollBy(-scrollByX * 1000, -scrollByY * 1000);
                    waits(200);
                    runs(function() {
                        expect(spy).not.toHaveBeenCalled();
                    });
                });

                it("should fire even if the delta is 0", function() {
                    var count = 0,
                        scrollSpy = jasmine.createSpy(),
                        wait = function() {
                            return scrollSpy.callCount > 0;
                        },
                        scrollIt = function() {
                            var offset = count % 2 === 0 ? 1 : -1;

                            ++count;
                            scrollSpy.reset();
                            expect(spy.callCount).toBe(0);
                            scroller.scrollBy(x ? scrollByX * offset : null, y ? scrollByY * offset : null);
                        };

                    var start = scroller.getPosition();

                    scroller.on('scroll', scrollSpy);

                    runs(scrollIt);
                    waitsFor(wait);
                    
                    runs(scrollIt);
                    waitsFor(wait);

                    runs(scrollIt);
                    waitsFor(wait);

                    runs(scrollIt);
                    waitsFor(wait);

                    waitsForSpy(spy);
                    runs(function() {
                        expect(spy.callCount).toBe(1);
                        var args = spy.mostRecentCall.args;

                        expect(args[0]).toBe(scroller);
                        expect(args[1]).toBe(start.x);
                        expect(args[2]).toBe(start.y);
                        expect(args[3]).toBe(0);
                        expect(args[4]).toBe(0);
                    });
                });
            }

            describe("x only", function() {
                describe("positive", function() {
                    makeSuite(true, false, false);
                });

                describe("negative", function() {
                    makeSuite(true, false, true);
                });
            });

            describe("y only", function() {
                describe("positive", function() {
                    makeSuite(false, true, false);
                });

                describe("negative", function() {
                    makeSuite(false, true, true);
                });
            });

            describe("both", function() {
                describe("positive", function() {
                    makeSuite(true, true, false);
                });

                describe("negative", function() {
                    makeSuite(true, true, true);
                });
            });
        });
    });

    describe("partnership", function() {
        var scrollSpy, scrollSpy2, scrollSpy3, el2, el3, scroller2, scroller3;

        function makeScroller2() {
            el2 = Ext.getBody().createChild({
                style: 'height:100px;width:100px;',
                cn: [{
                    style: 'height:200px;width:300px;'
                }]
            });

            scroller2 = new Ext.scroll.Scroller({
                element: el2
            });

            scroller2.on('scroll', scrollSpy2);
        }

        function makeScroller3() {
            el3 = Ext.getBody().createChild({
                style: 'height:100px;width:100px;',
                cn: [{
                    style: 'height:200px;width:300px;'
                }]
            });

            scroller3 = new Ext.scroll.Scroller({
                element: el3
            });

            scroller3.on('scroll', scrollSpy3);
        }

        beforeEach(function() {
            scrollSpy = jasmine.createSpy();
            scrollSpy2 = jasmine.createSpy();
            scrollSpy3 = jasmine.createSpy();

            el.appendChild({
                style: 'height:200px;width:300px;'
            }, true);

            makeScroller();

            scroller.on('scroll', scrollSpy);
        });

        afterEach(function() {
            if (scroller2) {
                scroller2.destroy();
                scroller2 = null;
            }
            if (scroller3) {
                scroller3.destroy();
                scroller3 = null;
            }
            if (el2) {
                el2.destroy();
                el2 = null;
            }
            if (el3) {
                el3.destroy();
                el3 = null;
            }
        });

        describe("single partner", function() {
            beforeEach(function() {
                makeScroller2();
            });

            describe("both axes enabled", function() {
                beforeEach(function() {
                    scroller.addPartner(scroller2);
                });

                it("should sync the partner's scroll position when the scroller is scrolled", function() {
                    // The partner should take action upon scroll start and end
                    spyOn(scroller2, 'fireScrollStart').andCallThrough();
                    spyOn(scroller2, 'fireScrollEnd').andCallThrough();

                    scroller.scrollTo(10, 20);

                    waitsFor(function() {
                        return scrollSpy.wasCalled
                                // The passive side should also have fired its start and end scroll events
                            &&     scroller2.fireScrollStart.callCount === 1
                            &&     scroller2.fireScrollEnd.callCount === 1;
                    }, 'scroller2 to have started scrolling, scrolled, and ended scrolling');

                    runs(function() {
                        expect(scroller2.getPosition()).toEqual({
                            x: 10,
                            y: 20
                        });
                    });
                });

                it("should sync the scroller's scroll position when the partner is scrolled", function() {
                    // The scroller should take action upon scroll start and end
                    spyOn(scroller, 'fireScrollStart').andCallThrough();
                    spyOn(scroller, 'fireScrollEnd').andCallThrough();

                    scroller2.scrollTo(10, 20);

                    waitsFor(function() {
                        return scrollSpy2.wasCalled
                                // The passive side should also have fired its start and end scroll events
                            &&     scroller.fireScrollStart.callCount === 1
                            &&     scroller.fireScrollEnd.callCount === 1;
                    }, 'scroller to have started scrolling, scrolled, and ended scrolling');

                    runs(function() {
                        expect(scroller.getPosition()).toEqual({
                            x: 10,
                            y: 20
                        });
                    });
                });
            });

            describe("x-axis only", function() {
                beforeEach(function() {
                    scroller.addPartner(scroller2, 'x');
                });

                it("should sync the partner's scroll position when the scroller is scrolled", function() {
                    scroller.scrollTo(10, 20);

                    waitsFor(function() {
                        return scrollSpy.wasCalled;
                    });

                    runs(function() {
                        expect(scroller2.getPosition()).toEqual({
                            x: 10,
                            y: 0
                        });
                    });
                });

                it("should sync the scroller's scroll position when the partner is scrolled", function() {
                    scroller2.scrollTo(10, 20);

                    waitsFor(function() {
                        return scrollSpy2.wasCalled;
                    });

                    runs(function() {
                        expect(scroller.getPosition()).toEqual({
                            x: 10,
                            y: 0
                        });
                    });
                });
            });

            describe("y-axis only", function() {
                beforeEach(function() {
                    scroller.addPartner(scroller2, 'y');
                });

                it("should sync the partner's scroll position when the scroller is scrolled", function() {
                    scroller.scrollTo(10, 20);

                    waitsFor(function() {
                        return scrollSpy.wasCalled;
                    });

                    runs(function() {
                        expect(scroller2.getPosition()).toEqual({
                            x: 0,
                            y: 20
                        });
                    });
                });

                it("should sync the scroller's scroll position when the partner is scrolled", function() {
                    scroller2.scrollTo(10, 20);

                    waitsFor(function() {
                        return scrollSpy2.wasCalled;
                    });

                    runs(function() {
                        expect(scroller.getPosition()).toEqual({
                            x: 0,
                            y: 20
                        });
                    });
                });
            });

            it("should remove the partner", function() {
                scroller.addPartner(scroller2);
                scroller.removePartner(scroller2);

                scroller.scrollTo(10, 20);

                waitsFor(function() {
                    return scrollSpy.wasCalled;
                });

                runs(function() {
                    expect(scroller2.getPosition()).toEqual({
                        x: 0,
                        y: 0
                    });
                    scroller2.scrollTo(40, 30);
                });

                waitsFor(function() {
                    return scrollSpy2.wasCalled;
                });

                runs(function() {
                    expect(scroller.getPosition()).toEqual({
                        x: 10,
                        y: 20
                    });
                });
            });
        });

        describe("multiple partners", function() {
            beforeEach(function() {
                makeScroller2();
                makeScroller3();

                scroller.addPartner(scroller2);
                scroller.addPartner(scroller3);
            });

            it("should sync multiple partners when the scroller is scrolled", function() {
                scroller.scrollTo(10, 15);

                waitsFor(function() {
                    return scrollSpy.wasCalled;
                });

                runs(function() {
                    expect(scroller2.getPosition()).toEqual({
                        x: 10,
                        y: 15
                    });

                    expect(scroller3.getPosition()).toEqual({
                        x: 10,
                        y: 15
                    });
                });
            });

            it("should sync multiple partners when the first partner scroller is scrolled", function() {
                scroller2.scrollTo(20, 30);

                waitsFor(function() {
                    return scrollSpy2.wasCalled;
                });

                runs(function() {
                    expect(scroller.getPosition()).toEqual({
                        x: 20,
                        y: 30
                    });

                    expect(scroller3.getPosition()).toEqual({
                        x: 20,
                        y: 30
                    });
                });
            });

            it("should sync multiple partners when the second partner scroller is scrolled", function() {
                scroller3.scrollTo(30, 60);

                waitsFor(function() {
                    return scrollSpy3.wasCalled;
                });

                runs(function() {
                    expect(scroller.getPosition()).toEqual({
                        x: 30,
                        y: 60
                    });
                    expect(scroller2.getPosition()).toEqual({
                        x: 30,
                        y: 60
                    });
                });
            });

            it("should remove a partner", function() {
                scroller.removePartner(scroller2);

                scroller2.scrollTo(15, 20);

                waitsFor(function() {
                    return scrollSpy2.wasCalled;
                });

                runs(function() {
                    expect(scroller.getPosition()).toEqual({
                        x: 0,
                        y: 0
                    });

                    // scroller3 still attached
                    scroller3.scrollTo(30, 45);
                });

                waitsFor(function() {
                    return(scrollSpy3.wasCalled);
                });

                runs(function() {
                    expect(scroller.getPosition()).toEqual({
                        x: 30,
                        y: 45
                    });
                });
            });
        });

        describe("cleanup", function() {
            it("should cleanup partners on destroy", function() {
                makeScroller2();
                scroller.addPartner(scroller2);

                scroller2.destroy();

                scroller.scrollBy(50, 50);
                waitsFor(function() {
                    return scrollSpy.wasCalled;
                });
                runs(function() {
                    expect(scroller.getPosition()).toEqual({
                        x: 50,
                        y: 50
                    });
                });
            });
        });
    });
});
