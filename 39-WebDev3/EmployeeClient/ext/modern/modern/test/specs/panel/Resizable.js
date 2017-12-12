topSuite("Ext.panel.Resizable", [
    'Ext.Panel',
    'Ext.layout.HBox',
    'Ext.layout.VBox',
    'Ext.panel.Resizer'
], function() {
    var baseSize = 300,
        touchId = 0,
        helper = Ext.testHelper,
        activeEdge, cursorTrack,
        resizable, panel, allEdges, splitEdges, startBox;

    var edgeInfo = {
        north: {
            horz: false,
            vert: {
                offsetKey: 'top',
                largerOffset: -1,
                smallerOffset: 1
            }
        },
        northeast: {
            horz: {
                offsetKey: 'right',
                largerOffset: 1,
                smallerOffset: -1
            },
            vert: {
                offsetKey: 'top',
                largerOffset: -1,
                smallerOffset: 1
            }
        },
        east: {
            horz: {
                offsetKey: 'right',
                largerOffset: 1,
                smallerOffset: -1
            },
            vert: false
        },
        southeast: {
            horz: {
                offsetKey: 'right',
                largerOffset: 1,
                smallerOffset: -1
            },
            vert: {
                offsetKey: 'bottom',
                largerOffset: 1,
                smallerOffset: -1
            }
        },
        south: {
            horz: false,
            vert: {
                offsetKey: 'bottom',
                largerOffset: 1,
                smallerOffset: -1
            }
        },
        southwest: {
            horz: {
                offsetKey: 'left',
                largerOffset: -1,
                smallerOffset: 1
            },
            vert: {
                offsetKey: 'bottom',
                largerOffset: 1,
                smallerOffset: -1
            }
        },
        west: {
            horz: {
                offsetKey: 'left',
                largerOffset: -1,
                smallerOffset: 1
            },
            vert: false
        },
        northwest: {
            horz: {
                offsetKey: 'left',
                largerOffset: -1,
                smallerOffset: 1
            },
            vert: {
                offsetKey: 'top',
                largerOffset: -1,
                smallerOffset: 1
            }
        }
    };

    function start(cfg, target) {
        cursorTrack = [cfg.x || 0, cfg.y || 0];
        helper.touchStart(target, cfg);
    }

    function move(cfg, target) {
        cursorTrack = [cfg.x || 0, cfg.y || 0];
        helper.touchMove(target, cfg);
    }

    function end(cfg, target) {
        cursorTrack = [cfg.x || 0, cfg.y || 0];
        helper.touchEnd(target, cfg);
    }

    function cancel(cfg, target) {
        cursorTrack = [cfg.x || 0, cfg.y || 0];
        helper.touchCancel(target, cfg);
    }

    function startDrag(edge) {
        runs(function() {
            startBox = panel.element.getRegion();
            activeEdge = edge;
            edge = resizable.getEdge(edge);
            var xy = getCenter(edge);

            start({
                id: touchId,
                x: xy[0],
                y: xy[1]
            }, edge);
        });
        waitsForAnimation();
    }

    function moveBy(x, y) {
        if (Ext.isArray(x)) {
            y = x[1];
            x = x[0];
        }

        runs(function() {
            move({
                id: touchId,
                x: cursorTrack[0] + (x || 0),
                y: cursorTrack[1] + (y || 0)
            }, resizable.getEdge(activeEdge));
        });
        waitsForAnimation();
    }

    function endDrag(x, y) {
        runs(function() {
            x = x || cursorTrack[0];
            y = y || cursorTrack[1];

            end({
                id: touchId,
                x: x,
                y: y
            }, resizable.getEdge(activeEdge));
        });
        waitsForAnimation();
        runs(function() {
            ++touchId;
            activeEdge = null;
        });
    }

    function dragCancel(x, y) {
        runs(function() {
            x = x || cursorTrack[0];
            y = y || cursorTrack[1];

            cancel({
                id: touchId,
                x: x,
                y: y
            }, resizable ? resizable.getEdge(activeEdge) : Ext.getBody());
        });
        waitsForAnimation();
        runs(function() {
            ++touchId;
            activeEdge = null;
        });
    }

    function negateMove(move) {
        var x = move[0],
            y = move[1];

        return [x ? -x : null, y ? -y : null];
    }

    function negateBox(box) {
        var ret = {},
            key, v;

        for (key in box) {
            v = box[key];
            ret[key] = v ? -v : 0;
        }
        return ret;
    }

    function getCenter(el) {
        var xy = el.getXY();
        return [xy[0] + (el.getWidth() / 2), xy[1] + (el.getHeight() / 2)];
    }

    function adjustBox(box, offsets) {
        return box.adjust(offsets.top, offsets.right, offsets.bottom, offsets.left);
    }

    function compareBoxes(box, newBox) {
        expect(box.top || 0).toBe(newBox.top || 0);
        expect(box.right || 0).toBe(newBox.right || 0);
        expect(box.bottom || 0).toBe(newBox.bottom || 0);
        expect(box.left || 0).toBe(newBox.left || 0);
    }

    function expectBox(newBox) {
        var box = panel.element.getRegion();
        compareBoxes(box, newBox);
    }

    function expectBoxOffset(offsets) {
        var box = startBox.copy();
        expectBox(adjustBox(box, offsets));
    }

    function expectSize(w, h) {
        var el = panel.element;
        expect(el.getWidth()).toBe(w);
        expect(el.getHeight()).toBe(h);
    }

    function runsExpectBoxOffset(offsets) {
        runs(function() {
            expectBoxOffset(offsets);
        });
    }

    function runsExpectSize(w, h) {
        runs(function() {
            expectSize(w, h);
        });
    }

    function createPanel(resizerCfg, panelCfg) {
        panel = new Ext.Panel(Ext.apply({
            width: baseSize,
            height: baseSize,
            floated: true,
            resizable: resizerCfg || null
        }, panelCfg));
        panel.show();
        panel.center();
        resizable = panel.getResizable();
    }

    beforeEach(function() {
        cursorTrack = null;
        ++touchId;
        allEdges = ['east', 'north', 'northeast', 'northwest', 'south', 'southeast', 'southwest', 'west'];
        splitEdges = ['east', 'north', 'south', 'west'];
    });

    afterEach(function() {
        startBox = resizable = activeEdge = cursorTrack = panel = Ext.destroy(panel);
    });

    describe("panel level configuring", function() {
        var customCls = 'spec.MyCustomResizer';
        beforeEach(function() {
            Ext.define(customCls, {
                extend: 'Ext.panel.Resizer'
            });
        });

        afterEach(function() {
            Ext.undefine(customCls);
        });

        describe("resizable", function() {
            describe("at construction", function() {
                it("should default to null", function() {
                    createPanel();
                    expect(panel.getResizable()).toBeNull();
                });

                it("should create a Ext.panel.Resizer instance when specified as true", function() {
                    createPanel(true);
                    expect(panel.getResizable().$className).toBe('Ext.panel.Resizer');
                });

                it("should default the type to Ext.panel.Resizer when passed an object", function() {
                    createPanel({});
                    expect(panel.getResizable().$className).toBe('Ext.panel.Resizer');
                });

                it("should accept a custom xclass", function() {
                    createPanel({
                        xclass: customCls
                    });
                    expect(panel.getResizable().$className).toBe(customCls);
                });
            });

            describe("after construction", function() {
                describe("no resizer -> resizer", function() {
                    it("should configure the new resizer", function() {
                        createPanel();
                        panel.setResizable(true);
                        expect(panel.getResizable().$className).toBe('Ext.panel.Resizer');
                    });
                });

                describe("resizer -> no resizer", function() {
                    it("should destroy the old resizer", function() {
                        createPanel(true);
                        var old = panel.getResizable();
                        panel.setResizable(null);
                        expect(panel.getResizable()).toBeNull();
                        expect(old.destroy).toBe(Ext.emptyFn);
                    });
                });

                describe("resizer -> different resizer", function() {
                    it("should destroy the old resizer and configure a new one", function() {
                        createPanel(true);
                        var old = panel.getResizable(),
                            r;

                        panel.setResizable({});
                        r = panel.getResizable();
                        expect(r.$className).toBe('Ext.panel.Resizer');
                        expect(r).not.toBe(old);
                        expect(old.destroy).toBe(Ext.emptyFn);
                    });
                });
            });
        });

        describe("ui", function() {
            function expectUi(ui) {
                allEdges.forEach(function(edge) {
                    edge = resizable.getEdge(edge);
                    expect(edge).toHaveCls('x-panelresizer-' + ui);
                });
            }

            function expectNotUi(ui) {
                allEdges.forEach(function(edge) {
                    edge = resizable.getEdge(edge);
                    expect(edge).not.toHaveCls('x-panelresizer-' + ui);
                });
            }


            describe("at construction", function() {
                describe("with no resizable", function() {
                    it("should not cause an exception", function() {
                        createPanel(null, {
                            ui: 'foo'
                        });
                        expect(panel.element).toHaveCls('x-panel-foo');
                    });
                });

                describe("with a resizable", function() {
                    it("should copy over a single ui", function() {
                        createPanel({
                            edges: 'all'
                        }, {
                            ui: 'foo'
                        });
                        expectUi('foo');
                    });

                    it("should copy multiples uis", function() {
                        createPanel({
                            edges: 'all'
                        }, {
                            ui: 'foo bar'
                        });
                        expectUi('foo');
                        expectUi('bar');
                    });
                });
            });

            describe("after construction", function() {
                describe("with no resizable", function() {
                    describe("no ui -> ui", function() {
                        it("should not cause an exception", function() {
                            createPanel(null);
                            panel.setUi('foo');
                            expect(panel.element).toHaveCls('x-panel-foo');
                        });
                    });

                    describe("ui -> no ui", function() {
                        it("should not cause an exception", function() {
                            createPanel(null, {
                                ui: 'foo'
                            });
                            panel.setUi('');
                            expect(panel.element).not.toHaveCls('x-panel-foo');
                        });
                    });

                    describe("changing ui", function() {
                        it("should not cause an exception", function() {
                            createPanel(null, {
                                ui: 'foo'
                            });
                            panel.setUi('bar');
                            expect(panel.element).not.toHaveCls('x-panel-foo');
                            expect(panel.element).toHaveCls('x-panel-bar');
                        });
                    });

                    describe("creating resizable", function() {
                        it("should use a single ui", function() {
                            createPanel(null, {
                                ui: 'foo'
                            });
                            panel.setResizable({
                                edges: allEdges
                            });
                            resizable = panel.getResizable();
                            expectUi('foo');
                        });

                        it("should use multiple uis", function() {
                            createPanel(null, {
                                ui: 'foo bar'
                            });
                            panel.setResizable({
                                edges: allEdges
                            });
                            resizable = panel.getResizable();
                            expectUi('foo');
                            expectUi('bar');
                        });
                    });
                });

                describe("with a resizable", function() {
                    describe("no ui -> ui", function() {
                        it("should be able to add a single ui", function() {
                            createPanel({
                                edges: allEdges
                            });
                            panel.setUi('foo');
                            expectUi('foo');
                        });

                        it("should be able to add mulitple uis", function() {
                            createPanel({
                                edges: allEdges
                            });
                            panel.setUi('foo bar');
                            expectUi('foo');
                            expectUi('bar');
                        });
                    });

                    describe("ui -> no ui", function() {
                        it("should clear a single ui", function() {
                            createPanel({
                                edges: allEdges
                            }, {
                                ui: 'foo'
                            });
                            panel.setUi('');
                            expectNotUi('foo');
                        });

                        it("should clear multiple uis", function() {
                            createPanel({
                                edges: allEdges
                            }, {
                                ui: 'foo bar'
                            });
                            panel.setUi('');
                            expectNotUi('foo');
                            expectNotUi('bar');
                        });
                    });

                    describe("changing ui", function() {
                        describe("with single ui", function() {
                            beforeEach(function() {
                                createPanel({
                                    edges: allEdges
                                }, {
                                    ui: 'foo'
                                });
                            });

                            it("should replace with a single ui", function() {
                                panel.setUi('bar');
                                expectUi('bar');
                                expectNotUi('foo');
                            });

                            it("should replace with multiple uis", function() {
                                panel.setUi('bar baz');
                                expectUi('bar');
                                expectUi('baz');
                                expectNotUi('foo');
                            });
                        });

                        describe("with multiple uis", function() {
                            beforeEach(function() {
                                createPanel({
                                    edges: allEdges
                                }, {
                                    ui: 'foo bar'
                                });
                            });

                            it("should replace with a single ui", function() {
                                panel.setUi('baz');
                                expectUi('baz');
                                expectNotUi('foo');
                                expectNotUi('bar');
                            });

                            it("should replace with multiple uis", function() {
                                panel.setUi('baz qux');
                                expectUi('baz');
                                expectUi('qux');
                                expectNotUi('foo');
                                expectNotUi('bar');
                            });
                        });
                    });
                });
            });
        });
    });

    describe("destruction", function() {
        it("should remove handles if the panel is not destroying", function() {
            createPanel({
                edges: 'all'
            });

            // 8 edges
            expect(panel.element.query('.' + Ext.panel.Resizer.prototype.baseCls).length).toBe(8);

            panel.setResizable(null);

            expect(panel.element.query('.' + Ext.panel.Resizer.prototype.baseCls).length).toBe(0);
        });

        it("should clear the proxy element if destroyed while dragging", function() {
            createPanel({
                edges: 'all',
                dynamic: false
            });
            startDrag('east');
            moveBy(20, 0);
            runs(function() {
                var proxy = panel.getResizable().getProxy(),
                    dom = proxy.dom;

                expect(proxy.isElement).toBe(true);
                expect(proxy.parent()).toBe(Ext.getBody());
                panel.setResizable(null);
                resizable = null;
                expect(dom.parentNode).toBeNull();
            });
            dragCancel();
        });
    });

    describe("resizable configuration", function() {
        describe("edges", function() {
            describe("config types", function() {
                describe("as a string", function() {
                    it("should accept a single side", function() {
                        createPanel({
                            edges: 'east'
                        });
                        expect(resizable.getEdges()).toEqual(['east']);
                    });

                    it("should accept multiple sides delimited by a space", function() {
                        createPanel({
                            edges: 'east west northwest'
                        });
                        expect(resizable.getEdges()).toEqual(['east', 'northwest', 'west']);
                    });

                    it("should handle leading and trailing spaces", function() {
                        createPanel({
                            edges: '    east        west  northwest    '
                        });
                        expect(resizable.getEdges()).toEqual(['east', 'northwest', 'west']);
                    });

                    it("should accept multiple sides delimited by a comma", function() {
                        createPanel({
                            edges: 'east,west,northwest'
                        });
                        expect(resizable.getEdges()).toEqual(['east', 'northwest', 'west']);
                    });

                    it("should support a mixture of strings and commas", function() {
                        createPanel({
                            edges: 'east, west, northwest'
                        });
                        expect(resizable.getEdges()).toEqual(['east', 'northwest', 'west']);
                    });

                    it("should support 'all'", function() {
                        createPanel({
                            edges: 'all'
                        });
                        expect(resizable.getEdges()).toEqual(allEdges);
                    });
                });

                describe("shortcuts", function() {
                    it("should transform shortcut strings", function() {
                        createPanel({
                            edges: 'n,ne,e,se,s,sw,w,nw'
                        });
                        expect(resizable.getEdges()).toEqual(allEdges);
                    });

                    it("should transform shortcut arrays", function() {
                        createPanel({
                            edges: ['n', 'ne', 'e', 'se', 's', 'sw', 'w', 'nw']
                        });
                        expect(resizable.getEdges()).toEqual(allEdges);
                    });

                    it("should accept a mixture of shortcuts + full names", function() {
                        createPanel({
                            edges: ['north', 'ne', 'e', 'southeast', 's', 'sw', 'west', 'nw']
                        });
                        expect(resizable.getEdges()).toEqual(allEdges);
                    });
                });
            });

            describe("edge elements", function() {
                describe("positioning", function() {
                    function expectBox(edge, box) {
                        edge = resizable.getEdge(edge);
                        expect(edge.getRegion().equals(box)).toBe(true);
                    }

                    it("should position the edges correctly", function() {
                        createPanel({
                            edges: 'all'
                        });

                        var size = resizable.getEdge('northwest').getSize(),
                            box = panel.element.getBox();

                        expectBox('north', {
                            top: box.top,
                            right: box.right,
                            bottom: box.top + size.height,
                            left: box.left
                        });

                        expectBox('northeast', {
                            top: box.top,
                            right: box.right,
                            bottom: box.top + size.height,
                            left: box.right - size.width
                        });

                        expectBox('east', {
                            top: box.top,
                            right: box.right,
                            bottom: box.bottom,
                            left: box.right - size.width
                        });

                        expectBox('southeast', {
                            top: box.bottom - size.height,
                            right: box.right,
                            bottom: box.bottom,
                            left: box.right - size.width
                        });

                        expectBox('south', {
                            top: box.bottom - size.height,
                            right: box.right,
                            bottom: box.bottom,
                            left: box.left
                        });

                        expectBox('southwest', {
                            top: box.bottom - size.height,
                            right: box.left + size.width,
                            bottom: box.bottom,
                            left: box.left
                        });

                        expectBox('west', {
                            top: box.top,
                            right: box.left + size.width,
                            bottom: box.bottom,
                            left: box.left
                        });

                        expectBox('northwest', {
                            top: box.top,
                            right: box.left + size.width,
                            bottom: box.top + size.height,
                            left: box.left
                        });
                    });
                });

                describe("at construction", function() {
                    it("should create edges as children of the panel element", function() {
                        createPanel({
                            edges: 'all'
                        });

                        allEdges.forEach(function(edge) {
                            expect(resizable.getEdge(edge).parent()).toBe(panel.element);
                        });
                    });

                    it("should create only the specified edges", function() {
                        createPanel({
                            edges: ['west', 'southeast']
                        });
                        // B2 edges
                        expect(panel.element.query('.x-panelresizer').length).toBe(2);
                        allEdges.forEach(function(name) {
                            var edge = resizable.getEdge(name);
                            if (name === 'west' || name === 'southeast') {
                                expect(edge.isElement).toBe(true);
                            } else {
                                expect(edge).toBeNull();
                            }
                        });
                    });

                    describe("DOM position", function() {
                        it("should place corners after side edges", function() {
                            createPanel({
                                edges: ['ne', 'se', 'sw', 'nw', 'n', 'e', 's', 'w']
                            });

                            var edges = panel.element.query('.x-panelresizer').map(function(el) {
                                return el.getAttribute('data-edge');
                            });
                            expect(edges).toEqual([
                                'east',
                                'north',
                                'south',
                                'west',
                                'northeast',
                                'northwest',
                                'southeast',
                                'southwest'
                            ]);
                        });
                    });
                });

                describe("dynamic", function() {
                    it("should be able to set different edges", function() {
                        createPanel({
                            edges: ['west', 'southeast']
                        });
                        resizable.setEdges(['east', 'northwest', 'south']);
                        // 3 edges
                        expect(panel.element.query('.' + Ext.panel.Resizer.prototype.baseCls).length).toBe(3);

                        allEdges.forEach(function(name) {
                            var edge = resizable.getEdge(name);
                            if (name === 'east' || name === 'northwest' || name === 'south') {
                                expect(edge.isElement).toBe(true);
                            } else {
                                expect(edge).toBeNull();
                            }
                        });
                    });

                    it("should be draggable after reconfiguring edges", function() {
                        createPanel({
                            edges: ['west']
                        });
                        resizable.setEdges(['east']);
                        startDrag('east');
                        moveBy(50, 0);
                        endDrag();
                        runsExpectSize(baseSize + 50, baseSize);
                    });

                    describe("with split: false", function() {
                        it("should not add the x-splitter class to the edges", function() {
                            createPanel({
                                edges: ['west'],
                                split: false
                            });
                            resizable.setEdges('all');
                            allEdges.forEach(function(edge) {
                                var el = resizable.getEdge(edge);
                                expect(el).not.toHaveCls(Ext.baseCSSPrefix + 'splitter');
                            });
                        });
                    });

                    describe("with split: true", function() {
                        it("should add the x-splitter class to the edges", function() {
                            createPanel({
                                edges: ['west'],
                                split: true
                            });
                            resizable.setEdges('all');
                            allEdges.forEach(function(edge) {
                                var el = resizable.getEdge(edge);
                                expect(el).toHaveCls(Ext.baseCSSPrefix + 'splitter');
                            });
                        });
                    });
                });
            });
        });

        describe("split", function() {
            var measured;

            function createSplitPanel(split) {
                createPanel({
                    edges: split ? splitEdges : allEdges,
                    split: split
                });
            }

            beforeEach(function() {
                if (!measured) {
                    createSplitPanel(true);
                    measured = resizable.getEdge('west').getWidth();
                    resizable = panel = Ext.destroy(panel);
                }
            });

            describe("at construction", function() {
                describe("with split: false", function() {
                    it("should not have x-splitter on edges", function() {
                        createSplitPanel(false);
                        allEdges.forEach(function(edge) {
                            var el = resizable.getEdge(edge);
                            expect(el).not.toHaveCls(Ext.baseCSSPrefix + 'splitter');
                        });
                    });
                }); 

                describe("with split: true", function() {
                    describe("edges", function() {
                        it("should not have x-splitter on edges", function() {
                            createSplitPanel(true);
                            splitEdges.forEach(function(edge) {
                                var el = resizable.getEdge(edge);
                                expect(el).toHaveCls(Ext.baseCSSPrefix + 'splitter');
                            });
                        });
                    });

                    describe("panel spacing", function() {
                        function makeSpaceSuite(edge) {
                            var edgeItem = edgeInfo[edge],
                                dir = edgeItem.horz || edgeItem.vert;

                            describe("edge: " + edge, function() {
                                it("should add space for the edge", function() {
                                    createPanel({
                                        split: true,
                                        edges: edge
                                    });
                                    var el = panel.element;

                                    expect(el).toHaveCls('x-split-' + edge);
                                    expect(el.getPadding(dir.offsetKey[0])).toBe(measured);
                                });
                            });
                        }

                        makeSpaceSuite('north');
                        makeSpaceSuite('east');
                        makeSpaceSuite('south');
                        makeSpaceSuite('west');
                    });
                });
            });

            describe("dynamic", function() {
                describe("from false -> true", function() {
                    it("should add the x-splitter class to the edges", function() {
                        createSplitPanel(false);
                        resizable.setSplit(true);
                        resizable.setEdges(splitEdges);
                        splitEdges.forEach(function(edge) {
                            var el = resizable.getEdge(edge);
                            expect(el).toHaveCls(Ext.baseCSSPrefix + 'splitter');
                        });
                    });
                });

                describe("from true -> false", function() {
                    it("should remove the x-splitter class to the edges", function() {
                        createSplitPanel(true);
                        resizable.setSplit(false);
                        resizable.setEdges(allEdges);
                        allEdges.forEach(function(edge) {
                            var el = resizable.getEdge(edge);
                            expect(el).not.toHaveCls(Ext.baseCSSPrefix + 'splitter');
                        });
                    });
                });

                describe("changing split edges", function() {
                    function makeSplitSuite(start, others) {
                        var base = 'x-split-',
                            startItem = edgeInfo[start],
                            startDir = startItem.horz || startItem.vert;

                        describe("from " + start, function() {
                            others.forEach(function(other) {
                                var otherItem = edgeInfo[other],
                                    otherDir = otherItem.horz || otherItem.vert;

                                describe("to " + other, function() {
                                    it("should change the side", function() {
                                        createPanel({
                                            split: true,
                                            edges: start
                                        });

                                        var el = panel.element;

                                        resizable.setEdges(other);
                                        expect(el).toHaveCls(base + other);
                                        expect(el.getPadding(otherDir.offsetKey[0])).toBe(measured);
                                        expect(el).not.toHaveCls(base + start);
                                        expect(el.getPadding(startDir.offsetKey[0])).toBe(0);
                                    });
                                });
                            });
                        });
                    }

                    makeSplitSuite('north', ['east', 'south', 'west']);
                    makeSplitSuite('east', ['north', 'south', 'west']);
                    makeSplitSuite('south', ['north', 'east', 'west']);
                    makeSplitSuite('west', ['north', 'east', 'south']);
                });
            });
        });
    });

    describe("dragging", function() {
        describe("with split: false", function() {
            function makeDynamicSuite(dynamic) {
                describe("with dynamic: " + dynamic, function() {
                    function makeFloatPositionSuite(floated) {
                        describe(floated ? "as floated" : "as positioned", function() {
                            function expectProxyBox(newBox) {
                                var box = resizable.getProxy().getRegion();
                                compareBoxes(box, newBox);
                            }

                            function expectProxyBoxOffset(offsets) {
                                var box = startBox.copy();
                                expectProxyBox(adjustBox(box, offsets));
                            }

                            function runsExpectProxyBoxOffset(offsets) {
                                runs(function() {
                                    expectProxyBoxOffset(offsets);
                                });
                            }

                            var runsBoxOffsetFn = dynamic ? runsExpectBoxOffset : runsExpectProxyBoxOffset,
                                createSuitePanel, ct;

                            if (floated) {
                                createSuitePanel = function(resizerCfg, panelCfg) {
                                    resizerCfg = Ext.apply({
                                        dynamic: dynamic,
                                        split: false
                                    }, resizerCfg);
                                    panel = new Ext.Panel(Ext.apply({
                                        width: baseSize,
                                        height: baseSize,
                                        floated: true,
                                        resizable: resizerCfg
                                    }, panelCfg));
                                    panel.show();
                                    panel.center();
                                    resizable = panel.getResizable();
                                };
                            } else {
                                createSuitePanel = function(resizerCfg, panelCfg) {
                                    resizerCfg = Ext.apply({
                                        dynamic: dynamic,
                                        split: false
                                    }, resizerCfg);

                                    ct = new Ext.Container({
                                        renderTo: Ext.getBody(),
                                        height: 800,
                                        width: 800,
                                        items: [Ext.apply({
                                            xtype: 'panel',
                                            left: 0,
                                            top: 0,
                                            width: baseSize,
                                            height: baseSize,
                                            resizable: resizerCfg
                                        }, panelCfg)]
                                    });

                                    panel = ct.items.first();
                                    panel.center();
                                    resizable = panel.getResizable();
                                };
                            }

                            afterEach(function() {
                                ct = Ext.destroy(ct);
                            });

                            function dragAndMove(dragEdge, moves, expectedOffsets) {
                                if (!Ext.isArray(moves[0])) {
                                    moves = [moves];
                                }

                                if (!Ext.isArray(expectedOffsets)) {
                                    expectedOffsets = [expectedOffsets];
                                }

                                var len = moves.length,
                                    cumulativeOffset = new Ext.util.Region(0, 0, 0, 0),
                                    i;

                                startDrag(dragEdge);
                                for (i = 0; i < len; ++i) {
                                    adjustBox(cumulativeOffset, expectedOffsets[i]);
                                    moveBy(moves[i]);
                                    runsBoxOffsetFn(cumulativeOffset.copy());
                                }
                                endDrag();
                                // Complete, so always check the element box
                                runsExpectBoxOffset(cumulativeOffset);
                            }

                            function simpleDragMove(edge, move, offsets) {
                                createSuitePanel({
                                    edges: edge
                                });
                                dragAndMove(edge, move, offsets);
                            }

                            describe("basic functionality", function() {
                                describe("basic drag in a single direction", function() {
                                    function makeSuite(edge, move, offset) {
                                        describe("edge: " + edge, function() {
                                            it("should be able to resize larger", function() {
                                                simpleDragMove(edge, move, offset);
                                            });

                                            it("should be able to resize smaller", function() {
                                                simpleDragMove(edge, negateMove(move), negateBox(offset));
                                            });
                                        });
                                    }

                                    makeSuite('north', [null, -50], { top: -50 });
                                    makeSuite('northeast', [30, -50], { top: -50, right: 30 });
                                    makeSuite('east', [50, null], { right: 50 });
                                    makeSuite('southeast', [50, 40], { bottom: 40, right: 50 });
                                    makeSuite('south', [null, 40], { bottom: 40 });
                                    makeSuite('southwest', [-30, 40], { bottom: 40, left: -30 });
                                    makeSuite('west', [-70, null], { left: -70 });
                                    makeSuite('northwest', [-30, -20], { top: -20, left: -30 });
                                });

                                describe("ignoring movement in other directions", function() {
                                    function makeSuite(otherAxis, edge, move, offset) {
                                        describe("edge: " + edge, function() {
                                            it("should ignore movements in the " + otherAxis + " when resizing larger", function() {
                                                simpleDragMove(edge, move, offset);
                                            });

                                            it("should ignore movements in the " + otherAxis + " when resizing smaller", function() {
                                                simpleDragMove(edge, negateMove(move), negateBox(offset));
                                            });
                                        });
                                    }

                                    makeSuite('x axis', 'north', [30, -50], { top: -50 });
                                    makeSuite('y axis', 'east', [30, 50], { right: 30 });
                                    makeSuite('x axis', 'south', [-30, 50], { bottom: 50 });
                                    makeSuite('y axis', 'west', [-30, -20], { left: -30 });
                                });

                                describe("multiple moves", function() {
                                    function makeSuite(edge, moves, offsets) {
                                        describe("edge: " + edge, function() {
                                            it("should make multiple moves when resizing larger", function() {
                                                simpleDragMove(edge, moves, offsets);
                                            });

                                            it("should make multiple moves when resizing smaller", function() {
                                                simpleDragMove(edge, moves.map(negateMove), offsets.map(negateBox));
                                            });
                                        });
                                    }

                                    makeSuite('north', [[null, -30], [null, -60]], [{ top: -30 }, { top: -60 }]);
                                    makeSuite('northeast', [[50, -30], [100, -60]], [{ top: -30, right: 50 }, { top: -60, right: 100 }]);
                                    makeSuite('east', [[60, null], [120, null]], [{ right: 60 }, { right: 120 }]);
                                    makeSuite('southeast', [[40, 30], [80, 60]], [{ bottom: 30, right: 40 }, { bottom: 60, right: 80 }]);
                                    makeSuite('south', [[null, 70], [null, 140]], [{ bottom: 70 }, { bottom: 140 }]);
                                    makeSuite('southwest', [[-20, 30], [-40, 60]], [{ bottom: 30, left: -20 }, { bottom: 60, left: -40 }]);
                                    makeSuite('west', [[-50, null], [-100, null]], [{ left: -50 }, { left: -100 }]);
                                    makeSuite('northwest', [[-25, -75], [-50, -150]], [{ top: -75, left: -25 }, { top: -150, left: -50 }]);
                                });
                            });

                            describe("snapping", function() {
                                var snapBase = 20;

                                var singles = [
                                    { move: 8, offset: 0 }, // 8
                                    { move: 1, offset: 0 }, // 9
                                    { move: 1, offset: snapBase }, // 10
                                    { move: 10, offset: 0 }, // 20
                                    { move: 10, offset: snapBase }, // 30
                                    { move: 20, offset: snapBase }, // 50
                                    { move: 30, offset: snapBase }, // 80
                                    { move: 5, offset: 0 } // 85
                                ];

                                var doubles = [
                                    { move: [15, 8], offset: { vert: 0, horz: snapBase } }, // 15, 8
                                    { move: [4, 1], offset: { vert: 0, horz: 0 } }, // 19, 9
                                    { move: [2, 1], offset: { vert: snapBase, horz: 0 } }, // 21, 10
                                    { move: [8, 9], offset: { vert: 0, horz: 0 } }, // 29, 19
                                    { move: [2, 2], offset: { vert: 0, horz: snapBase } }, // 31, 21
                                    { move: [25, 35], offset: { vert: snapBase * 2, horz: snapBase } }, // 56, 56
                                    { move: [2, 3], offset: { vert: 0, horz: 0 } }, // 58, 59
                                    { move: [4, 2], offset: { vert: 0, horz: 0 } } // 62, 61
                                ];

                                function makeSuite(snap, edge, movesAndOffsets) {
                                    var expandedSnap = !Ext.isArray(snap) ? [snap, snap] : snap,
                                        moves = [],
                                        offsets = [],
                                        key = 'not snap',
                                        edgeItem = edgeInfo[edge],
                                        snapHorz = edgeItem.horz && expandedSnap[0] !== null,
                                        snapVert = edgeItem.vert && expandedSnap[1] !== null;

                                    if (snapHorz && snapVert) {
                                        key = 'snap in both dimensions';
                                    } else if (snapVert) {
                                        key = 'snap vertically';
                                    } else if (snapHorz) {
                                        key = 'snap horizontally';
                                    }

                                    movesAndOffsets.forEach(function(item) {
                                        moves.push(item.move);
                                        offsets.push(item.offset);
                                    });

                                    describe("edge: " + edge, function() {
                                        it("should " + key + " when resizing larger", function() {
                                            createSuitePanel({
                                                snap: snap,
                                                edges: edge
                                            });
                                            dragAndMove(edge, moves, offsets);
                                        });

                                        it("should " + key + " when resizing smaller", function() {
                                            createSuitePanel({
                                                snap: snap,
                                                edges: edge
                                            });
                                            dragAndMove(edge, moves.map(negateMove), offsets.map(negateBox));
                                        });
                                    });
                                }

                                function generateSingles(options, arr) {
                                    var edge;

                                    if (typeof options === 'string') {
                                        edge = edgeInfo[options];
                                        options = {};
                                    } else {
                                        edge = edgeInfo[options.edge];
                                    }

                                    var horz = edge.horz,
                                        moveOffset = horz ? edge.horz.largerOffset : edge.vert.largerOffset,
                                        offsetKey = horz ? edge.horz.offsetKey : edge.vert.offsetKey,
                                        noSnap = options.noSnap;

                                    return (arr || singles).map(function(o) {
                                        var move = o.move,
                                            offset = {},
                                            v;

                                        move *= moveOffset;
                                        v = move;
                                        if (!noSnap) {
                                            v = o.offset * moveOffset;
                                        }
                                        offset[offsetKey] = v;

                                        return {
                                            move: [horz ? move : null, horz ? null : move],
                                            offset: offset
                                        };
                                    });
                                }

                                function generateDoubles(options, arr) {
                                    var edge;

                                    if (typeof options === 'string') {
                                        edge = edgeInfo[options];
                                        options = {};
                                    } else {
                                        edge = edgeInfo[options.edge];
                                    }

                                    var horzMoveOffset = edge.horz.largerOffset,
                                        vertMoveOffset = edge.vert.largerOffset,
                                        noSnapHorz = options.noSnapHorz,
                                        noSnapVert = options.noSnapVert;

                                    return (arr || doubles).map(function(o) {
                                        var offset = {},
                                            move = o.move,
                                            moveHorz = move[0] * horzMoveOffset,
                                            moveVert = move[1] * vertMoveOffset,
                                            horz = moveHorz,
                                            vert = moveVert;

                                        if (!noSnapHorz) {
                                            horz = o.offset.horz * horzMoveOffset;
                                        }

                                        if (!noSnapVert) {
                                            vert = o.offset.vert * vertMoveOffset;
                                        }

                                        offset[edge.horz.offsetKey] = horz;
                                        offset[edge.vert.offsetKey] = vert;

                                        return {
                                            move: [moveHorz, moveVert],
                                            offset: offset
                                        };
                                    });
                                }

                                describe("single number", function() {
                                    makeSuite(snapBase, 'north', generateSingles('north'));

                                    makeSuite(snapBase, 'northeast', generateDoubles('northeast'));

                                    makeSuite(snapBase, 'east', generateSingles('east'));

                                    makeSuite(snapBase, 'southeast', generateDoubles('southeast'));

                                    makeSuite(snapBase, 'south', generateSingles('south'));

                                    makeSuite(snapBase, 'southwest', generateDoubles('southwest'));

                                    makeSuite(snapBase, 'west', generateSingles('west'));

                                    makeSuite(snapBase, 'northwest', generateDoubles('northwest'));
                                });

                                describe("x with a value, y null", function() {
                                    var snap = [snapBase, null];

                                    makeSuite(snap, 'north', generateSingles({
                                        edge: 'north',
                                        noSnap: true
                                    }));

                                    makeSuite(snap, 'northeast', generateDoubles({
                                        edge: 'northeast',
                                        noSnapVert: true
                                    }));

                                    makeSuite(snap, 'east', generateSingles({
                                        edge: 'east',
                                        noSnap: false
                                    }));

                                    makeSuite(snap, 'southeast', generateDoubles({
                                        edge: 'southeast',
                                        noSnapVert: true
                                    }));

                                    makeSuite(snap, 'south', generateSingles({
                                        edge: 'south',
                                        noSnap: true
                                    }));

                                    makeSuite(snap, 'southwest', generateDoubles({
                                        edge: 'southwest',
                                        noSnapVert: true
                                    }));

                                    makeSuite(snap, 'west', generateSingles({
                                        edge: 'west',
                                        noSnap: false
                                    }));

                                    makeSuite(snap, 'northwest', generateDoubles({
                                        edge: 'northwest',
                                        noSnapVert: true
                                    }));
                                });

                                describe("y with a value, x null", function() {
                                    var snap = [null, snapBase];

                                    makeSuite(snap, 'north', generateSingles({
                                        edge: 'north',
                                        noSnap: false
                                    }));

                                    makeSuite(snap, 'northeast', generateDoubles({
                                        edge: 'northeast',
                                        noSnapHorz: true
                                    }));

                                    makeSuite(snap, 'east', generateSingles({
                                        edge: 'east',
                                        noSnap: true
                                    }));

                                    makeSuite(snap, 'southeast', generateDoubles({
                                        edge: 'southeast',
                                        noSnapHorz: true
                                    }));

                                    makeSuite(snap, 'south', generateSingles({
                                        edge: 'south',
                                        noSnap: false
                                    }));

                                    makeSuite(snap, 'southwest', generateDoubles({
                                        edge: 'southwest',
                                        noSnapHorz: true
                                    }));

                                    makeSuite(snap, 'west', generateSingles({
                                        edge: 'west',
                                        noSnap: true
                                    }));

                                    makeSuite(snap, 'northwest', generateDoubles({
                                        edge: 'northwest',
                                        noSnapHorz: true
                                    }));
                                });

                                describe("separate values for x & y", function() {
                                    var xSnap = 30,
                                        ySnap = 20,
                                        snap = [xSnap, ySnap];

                                    makeSuite(snap, 'north', [
                                        { move: [null, -30], offset: { top: -ySnap * 2 } }, // -30
                                        { move: [null, -10], offset: { top: 0 } }, // -40
                                        { move: [null, -10], offset: { top: -ySnap } }, // -50
                                        { move: [null, -20], offset: { top: -ySnap } }  // -70
                                    ]);

                                    makeSuite(snap, 'northeast', [
                                        { move: [10, -30], offset: { top: -ySnap * 2, right: 0 } }, // 10, -30
                                        { move: [5, -10], offset: { top: 0, right: xSnap } }, // 15, -40
                                        { move: [30, -10], offset: { top: -ySnap, right: xSnap } }, // 45, -50
                                        { move: [5, -20], offset: { top: -ySnap, right: 0 } }  // 50, -70
                                    ]);

                                    makeSuite(snap, 'east', [
                                        { move: [10, null], offset: { right: 0 } }, // 10
                                        { move: [5, null], offset: { right: xSnap } }, // 15
                                        { move: [30, null], offset: { right: xSnap } }, // 45
                                        { move: [5, null], offset: { right: 0 } } // 50
                                    ]);

                                    makeSuite(snap, 'southeast', [
                                        { move: [10, 30], offset: { bottom: ySnap * 2, right: 0 } }, // 10, 30
                                        { move: [5, 10], offset: { bottom: 0, right: xSnap } }, // 15, 40
                                        { move: [30, 10], offset: { bottom: ySnap, right: xSnap } }, // 45, 50
                                        { move: [5, 20], offset: { bottom: ySnap, right: 0 } } // 50, 70
                                    ]);

                                    makeSuite(snap, 'south', [
                                        { move: [null, 30], offset: { bottom: ySnap * 2 } }, // 30
                                        { move: [null, 10], offset: { bottom: 0 } }, // 40
                                        { move: [null, 10], offset: { bottom: ySnap } }, // 50
                                        { move: [null, 20], offset: { bottom: ySnap } }  // 70
                                    ]);

                                    makeSuite(snap, 'southwest', [
                                        { move: [-10, 30], offset: { bottom: ySnap * 2, left: 0 } }, // -10, 30
                                        { move: [-5, 10], offset: { bottom: 0, left: -xSnap } }, // -15, 40
                                        { move: [-30, 10], offset: { bottom: ySnap, left: -xSnap } }, // -45, 50
                                        { move: [-5, 20], offset: { bottom: ySnap, left: 0 } }  // -50, 70
                                    ]);

                                    makeSuite(snap, 'west', [
                                        { move: [-10, null], offset: { left: 0 } }, // -10, 30
                                        { move: [-5, null], offset: { left: -xSnap } }, // -15, 40
                                        { move: [-30, null], offset: { left: -xSnap } }, // -45, 50
                                        { move: [-5, null], offset: { left: 0 } }  // -50, 70
                                    ]);

                                    makeSuite(snap, 'northwest', [
                                        { move: [-10, -30], offset: { top: -ySnap * 2, left: 0 } }, // -10, -30
                                        { move: [-5, -10], offset: { top: 0, left: -xSnap } }, // -15, -40
                                        { move: [-30, -10], offset: { top: -ySnap, left: -xSnap } }, // -45, -50
                                        { move: [-5, -20], offset: { top: -ySnap, left: 0 } }  // -50, -70
                                    ]);
                                });

                                describe("dynamically setting the value", function() {
                                    describe("no snap -> snap", function() {
                                        it("should respect the snap value", function() {
                                            createSuitePanel({
                                                edges: 'southeast'
                                            });
                                            startDrag('southeast');
                                            moveBy(10, 10);
                                            endDrag();
                                            runsExpectBoxOffset({
                                                right: 10,
                                                bottom: 10
                                            });
                                            runs(function() {
                                                resizable.setSnap(30);
                                            });
                                            startDrag('southeast');
                                            moveBy(40, 40);
                                            endDrag();
                                            runsExpectBoxOffset({
                                                right: 50,
                                                bottom: 50
                                            });
                                        });
                                    });

                                    describe("snap -> no snap", function() {
                                        it("should respect the cleared snap value", function() {
                                            createSuitePanel({
                                                edges: 'southeast',
                                                snap: 30
                                            });
                                            startDrag('southeast');
                                            moveBy(20, 20);
                                            endDrag();
                                            runsExpectBoxOffset({
                                                right: 30,
                                                bottom: 30
                                            });
                                            runs(function() {
                                                resizable.setSnap(null);
                                            });
                                            startDrag('southeast');
                                            moveBy(10, 10);
                                            endDrag();
                                            runsExpectBoxOffset({
                                                right: 10,
                                                bottom: 10
                                            });
                                        });
                                    });

                                    describe("change snap value", function() {
                                        it("should respect the new snap value", function() {
                                            createSuitePanel({
                                                edges: 'southeast',
                                                snap: 30
                                            });
                                            startDrag('southeast');
                                            moveBy(20, 20);
                                            endDrag();
                                            runsExpectBoxOffset({
                                                right: 30,
                                                bottom: 30
                                            });
                                            runs(function() {
                                                resizable.setSnap(50);
                                            });
                                            startDrag('southeast');
                                            moveBy(45, 45);
                                            endDrag();
                                            runsExpectBoxOffset({
                                                right: 70,
                                                bottom: 70
                                            });
                                        });
                                    });
                                });

                                // This is not supposed to be extensive, just that constraints work with snapping
                                describe("with constraints", function() {
                                    var snap = 50,
                                        minSize = 120,
                                        maxSize = 420;

                                    function makeSuite(edge, move) {
                                        var edgeItem = edgeInfo[edge];

                                        describe("edge: " + edge, function() {
                                            it("should constrain to the max size", function() {
                                                var offset = {},
                                                    horz = edgeItem.horz,
                                                    vert = edgeItem.vert;

                                                if (horz) {
                                                    offset[horz.offsetKey] = horz.largerOffset * (maxSize - baseSize);
                                                }

                                                if (vert) {
                                                    offset[vert.offsetKey] = vert.largerOffset * (maxSize - baseSize);
                                                }

                                                createSuitePanel({
                                                    edges: edge,
                                                    maxSize: maxSize,
                                                    snap: snap
                                                });
                                                startDrag(edge);
                                                moveBy(move);
                                                runsBoxOffsetFn(offset);
                                                endDrag();
                                                runsExpectBoxOffset(offset);
                                            });

                                            it("should constrain to the min size", function() {
                                                move = negateMove(move);

                                                var offset = {},
                                                    horz = edgeItem.horz,
                                                    vert = edgeItem.vert;

                                                if (horz) {
                                                    offset[horz.offsetKey] = horz.smallerOffset * (baseSize - minSize);
                                                }

                                                if (vert) {
                                                    offset[vert.offsetKey] = vert.smallerOffset * (baseSize - minSize);
                                                }

                                                createSuitePanel({
                                                    edges: edge,
                                                    minSize: minSize,
                                                    snap: snap
                                                });
                                                startDrag(edge);
                                                moveBy(move);
                                                runsBoxOffsetFn(offset);
                                                endDrag();
                                                runsExpectBoxOffset(offset);
                                            });
                                        });
                                    }

                                    makeSuite('north', [null, -200]);
                                    makeSuite('northeast', [200, -200]);
                                    makeSuite('east', [200, null]);
                                    makeSuite('southeast', [200, 200]);
                                    makeSuite('south', [null, 200]);
                                    makeSuite('southwest', [-200, 200]);
                                    makeSuite('west', [-200, null]);
                                    makeSuite('northwest', [-200, -200]);
                                });
                            });

                            describe("constraints", function() {
                                describe("basic constraints", function() {
                                    var min = 200,
                                        max = 400,
                                        minOffset = baseSize - min,
                                        maxOffset = max - baseSize;

                                    function runConstrainSuite(onComponent, prop, constrainVal, edge, move, offsets) {
                                        var panelCfg = {},
                                            resizerCfg = { edges: edge };

                                        if (onComponent) {
                                            panelCfg[prop] = constrainVal;
                                        } else {
                                            resizerCfg[prop] = constrainVal;
                                        }

                                        createSuitePanel(resizerCfg, panelCfg);

                                        startDrag(edge);
                                        moveBy(move);
                                        runsBoxOffsetFn(offsets);
                                        endDrag();
                                        runsExpectBoxOffset(offsets);
                                    }

                                    describe("on the component", function() {
                                        function makeComponentConstrainSuite(prop, name, constrainVal, edge, move, offsets) {
                                            var isHorz = prop === 'minWidth' || prop === 'maxWidth',
                                                constrain = edgeInfo[edge][isHorz ? 'horz' : 'vert'],
                                                qualifier = constrain ? '' : ' not';

                                            describe("edge: " + edge, function() {
                                                it("should" + qualifier + " constrain the " + name, function() {
                                                    runConstrainSuite(true, prop, constrainVal, edge, move, offsets);
                                                });
                                            });
                                        }

                                        describe("minWidth", function() {
                                            function makeSuite(edge, move, offsets) {
                                                makeComponentConstrainSuite('minWidth', 'width', min, edge, move, offsets);
                                            }

                                            makeSuite('north', [null, 150], { top: 150 });
                                            makeSuite('northeast', [-150, 150], { top: 150, right: -minOffset });
                                            makeSuite('east', [-150, null], { right: -minOffset });
                                            makeSuite('southeast', [-150, -150], { bottom: -150, right: -minOffset });
                                            makeSuite('south', [null, -150], { bottom: -150 });
                                            makeSuite('southwest', [150, -150], { bottom: -150, left: minOffset });
                                            makeSuite('west', [150, null], { left: minOffset });
                                            makeSuite('northwest', [150, 150], { top: 150, left: minOffset });
                                        });

                                        describe("minHeight", function() {
                                            function makeSuite(edge, move, offsets) {
                                                makeComponentConstrainSuite('minHeight', 'height', min, edge, move, offsets);
                                            }

                                            makeSuite('north', [null, 150], { top: minOffset });
                                            makeSuite('northeast', [-150, 150], { top: minOffset, right: -150 });
                                            makeSuite('east', [-150, null], { right: -150 });
                                            makeSuite('southeast', [-150, -150], { bottom: -minOffset, right: -150 });
                                            makeSuite('south', [null, -150], { bottom: -minOffset });
                                            makeSuite('southwest', [150, -150], { bottom: -minOffset, left: 150 });
                                            makeSuite('west', [150, null], { left: 150 });
                                            makeSuite('northwest', [150, 150], { top: minOffset, left: 150 });
                                        });

                                        describe("maxWidth", function() {
                                            function makeSuite(edge, move, offsets) {
                                                makeComponentConstrainSuite('maxWidth', 'width', max, edge, move, offsets);
                                            }

                                            makeSuite('north', [null, -150], { top: -150 });
                                            makeSuite('northeast', [150, -150], { top: -150, right: maxOffset });
                                            makeSuite('east', [150, null], { right: maxOffset });
                                            makeSuite('southeast', [150, 150], { bottom: 150, right: maxOffset });
                                            makeSuite('south', [null, 150], { bottom: 150 });
                                            makeSuite('southwest', [-150, 150], { bottom: 150, left: -maxOffset });
                                            makeSuite('west', [-150, null], { left: -maxOffset });
                                            makeSuite('northwest', [-150, -150], { top: -150, left: -maxOffset });
                                        });

                                        describe("maxHeight", function() {
                                            function makeSuite(edge, move, offsets) {
                                                makeComponentConstrainSuite('maxHeight', 'height', max, edge, move, offsets);
                                            }

                                            makeSuite('north', [null, -150], { top: -maxOffset });
                                            makeSuite('northeast', [150, -150], { top: -maxOffset, right: 150 });
                                            makeSuite('east', [150, null], { right: 150 });
                                            makeSuite('southeast', [150, 150], { bottom: maxOffset, right: 150 });
                                            makeSuite('south', [null, 150], { bottom: maxOffset });
                                            makeSuite('southwest', [-150, 150], { bottom: maxOffset, left: -150 });
                                            makeSuite('west', [-150, null], { left: -150 });
                                            makeSuite('northwest', [-150, -150], { top: -maxOffset, left: -150 });
                                        });
                                    });

                                    describe("on the resizer", function() {
                                        function makeResizerConstrainSuite(prop, constrainVal, edge, move, offsets) {
                                            var expanded = typeof constrainVal === 'number' ? [constrainVal, constrainVal] : constrainVal,
                                                constrainW = expanded[0] !== null,
                                                constrainH = expanded[1] !== null,
                                                msg = [],
                                                append;

                                            if (edgeInfo[edge].horz) {
                                                if (constrainW) {
                                                    msg.push('constrain the width');
                                                } else {
                                                    msg.push('not constrain the width');
                                                }
                                            }

                                            if (edgeInfo[edge].horz) {
                                                append = msg.length ? ' & ' : '';
                                                if (constrainH) {
                                                    msg.push(append + 'constrain the height');
                                                } else {
                                                    msg.push(append + 'not constrain the height');
                                                }
                                            }

                                            describe("edge: " + edge, function() {
                                                it("should " + msg.join(''), function() {
                                                    runConstrainSuite(false, prop, constrainVal, edge, move, offsets);
                                                });
                                            });
                                        }

                                        describe("minSize", function() {
                                            describe("as a number", function() {
                                                function makeSuite(edge, move, offsets) {
                                                    makeResizerConstrainSuite('minSize', min, edge, move, offsets);
                                                }

                                                makeSuite('north', [null, 150], { top: minOffset });
                                                makeSuite('northeast', [-150, 150], { top: minOffset, right: -minOffset });
                                                makeSuite('east', [-150, null], { right: -minOffset });
                                                makeSuite('southeast', [-150, -150], { bottom: -minOffset, right: -minOffset });
                                                makeSuite('south', [null, -150], { bottom: -minOffset });
                                                makeSuite('southwest', [150, -150], { bottom: -minOffset, left: minOffset });
                                                makeSuite('west', [150, null], { left: minOffset });
                                                makeSuite('northwest', [150, 150], { top: minOffset, left: minOffset });
                                            });

                                            describe("with a width only value", function() {
                                                function makeSuite(edge, move, offsets) {
                                                    makeResizerConstrainSuite('minSize', [min, null], edge, move, offsets);
                                                }

                                                makeSuite('north', [null, 150], { top: 150 });
                                                makeSuite('northeast', [-150, 150], { top: 150, right: -minOffset });
                                                makeSuite('east', [-150, null], { right: -minOffset });
                                                makeSuite('southeast', [-150, -150], { bottom: -150, right: -minOffset });
                                                makeSuite('south', [null, -150], { bottom: -150 });
                                                makeSuite('southwest', [150, -150], { bottom: -150, left: minOffset });
                                                makeSuite('west', [150, null], { left: minOffset });
                                                makeSuite('northwest', [150, 150], { top: 150, left: minOffset });
                                            });

                                            describe("height only value", function() {
                                                function makeSuite(edge, move, offsets) {
                                                    makeResizerConstrainSuite('minSize', [null, min], edge, move, offsets);
                                                }

                                                makeSuite('north', [null, 150], { top: minOffset });
                                                makeSuite('northeast', [-150, 150], { top: minOffset, right: -150 });
                                                makeSuite('east', [-150, null], { right: -150 });
                                                makeSuite('southeast', [-150, -150], { bottom: -minOffset, right: -150 });
                                                makeSuite('south', [null, -150], { bottom: -minOffset });
                                                makeSuite('southwest', [150, -150], { bottom: -minOffset, left: 150 });
                                                makeSuite('west', [150, null], { left: 150 });
                                                makeSuite('northwest', [150, 150], { top: minOffset, left: 150 });
                                            });

                                            describe("width and height", function() {
                                                var minW = min + 5,
                                                    minH = min - 5,
                                                    minWOffset = baseSize - minW,
                                                    minHOffset = baseSize - minH;

                                                function makeSuite(edge, move, offsets) {
                                                    makeResizerConstrainSuite('minSize', [minW, minH], edge, move, offsets);
                                                }

                                                makeSuite('north', [null, 150], { top: minHOffset });
                                                makeSuite('northeast', [-150, 150], { top: minHOffset, right: -minWOffset });
                                                makeSuite('east', [-150, null], { right: -minWOffset });
                                                makeSuite('southeast', [-150, -150], { bottom: -minHOffset, right: -minWOffset });
                                                makeSuite('south', [null, -150], { bottom: -minHOffset });
                                                makeSuite('southwest', [150, -150], { bottom: -minHOffset, left: minWOffset });
                                                makeSuite('west', [150, null], { left: minWOffset });
                                                makeSuite('northwest', [150, 150], { top: minHOffset, left: minWOffset });
                                            });
                                        });

                                        describe("maxSize", function() {
                                            describe("as a number", function() {
                                                function makeSuite(edge, move, offsets) {
                                                    makeResizerConstrainSuite('maxSize', max, edge, move, offsets);
                                                }

                                                makeSuite('north', [null, -150], { top: -maxOffset });
                                                makeSuite('northeast', [150, -150], { top: -maxOffset, right: maxOffset });
                                                makeSuite('east', [150, null], { right: maxOffset });
                                                makeSuite('southeast', [150, 150], { bottom: maxOffset, right: maxOffset });
                                                makeSuite('south', [null, 150], { bottom: maxOffset });
                                                makeSuite('southwest', [-150, 150], { bottom: maxOffset, left: -maxOffset });
                                                makeSuite('west', [-150, null], { left: -maxOffset });
                                                makeSuite('northwest', [-150, -150], { top: -maxOffset, left: -maxOffset });
                                            });

                                            describe("with a width only value", function() {
                                                function makeSuite(edge, move, offsets) {
                                                    makeResizerConstrainSuite('maxSize', [max, null], edge, move, offsets);
                                                }

                                                makeSuite('north', [null, -150], { top: -150 });
                                                makeSuite('northeast', [150, -150], { top: -150, right: maxOffset });
                                                makeSuite('east', [150, null], { right: maxOffset });
                                                makeSuite('southeast', [150, 150], { bottom: 150, right: maxOffset });
                                                makeSuite('south', [null, 150], { bottom: 150 });
                                                makeSuite('southwest', [-150, 150], { bottom: 150, left: -maxOffset });
                                                makeSuite('west', [-150, null], { left: -maxOffset });
                                                makeSuite('northwest', [-150, -150], { top: -150, left: -maxOffset });
                                            });

                                            describe("height only value", function() {
                                                function makeSuite(edge, move, offsets) {
                                                    makeResizerConstrainSuite('maxSize', [null, max], edge, move, offsets);
                                                }

                                                makeSuite('north', [null, -150], { top: -maxOffset });
                                                makeSuite('northeast', [150, -150], { top: -maxOffset, right: 150 });
                                                makeSuite('east', [150, null], { right: 150 });
                                                makeSuite('southeast', [150, 150], { bottom: maxOffset, right: 150 });
                                                makeSuite('south', [null, 150], { bottom: maxOffset });
                                                makeSuite('southwest', [-150, 150], { bottom: maxOffset, left: -150 });
                                                makeSuite('west', [-150, null], { left: -150 });
                                                makeSuite('northwest', [-150, -150], { top: -maxOffset, left: -150 });
                                            });

                                            describe("width and height", function() {
                                                var maxW = max + 5,
                                                    maxH = max - 5,
                                                    maxWOffset = maxW - baseSize,
                                                    maxHOffset = maxH - baseSize;

                                                function makeSuite(edge, move, offsets) {
                                                    makeResizerConstrainSuite('maxSize', [maxW, maxH], edge, move, offsets);
                                                }

                                                makeSuite('north', [null, -150], { top: -maxHOffset });
                                                makeSuite('northeast', [150, -150], { top: -maxHOffset, right: maxWOffset });
                                                makeSuite('east', [150, null], { right: maxWOffset });
                                                makeSuite('southeast', [150, 150], { bottom: maxHOffset, right: maxWOffset });
                                                makeSuite('south', [null, 150], { bottom: maxHOffset });
                                                makeSuite('southwest', [-150, 150], { bottom: maxHOffset, left: -maxWOffset });
                                                makeSuite('west', [-150, null], { left: -maxWOffset });
                                                makeSuite('northwest', [-150, -150], { top: -maxHOffset, left: -maxWOffset });
                                            });
                                        });
                                    });
                                });

                                describe("component vs resizable", function() {
                                    describe("minWidth", function() {
                                        function runSuite(resizerVal, componentVal, move, offset) {
                                            createSuitePanel({
                                                minSize: resizerVal,
                                                edges: 'east'
                                            }, {
                                                minWidth: componentVal
                                            });
                                            dragAndMove('east', move, offset);
                                        }

                                        it("should favour the component value when the component value is larger", function() {
                                            runSuite([100, null], 200, [-250, null], { right: -100 });
                                        });

                                        it("should favour the component value when the component value is smaller", function() {
                                            runSuite([200, null], 100, [-250, null], { right: -200 });
                                        });
                                    });

                                    describe("minHeight", function() {
                                        function runSuite(resizerVal, componentVal, move, offset) {
                                            createSuitePanel({
                                                minSize: resizerVal,
                                                edges: 'south'
                                            }, {
                                                minHeight: componentVal
                                            });
                                            dragAndMove('south', move, offset);
                                        }

                                        it("should favour the component value when the component value is larger", function() {
                                            runSuite([null, 100], 200, [null, -250], { bottom: -100 });
                                        });

                                        it("should favour the component value when the component value is smaller", function() {
                                            runSuite([null, 200], 100, [null, -250], { bottom: -200 });
                                        });
                                    });

                                    describe("maxWidth", function() {
                                        function runSuite(resizerVal, componentVal, move, offset) {
                                            createSuitePanel({
                                                maxSize: resizerVal,
                                                edges: 'east'
                                            }, {
                                                maxWidth: componentVal
                                            });
                                            dragAndMove('east', move, offset);
                                        }

                                        it("should favour the component value when the component value is larger", function() {
                                            runSuite([400, null], 500, [300, null], { right: 200 });
                                        });

                                        it("should favour the component value when the component value is smaller", function() {
                                            runSuite([500, null], 400, [300, null], { right: 100 });
                                        });
                                    });

                                    describe("maxHeight", function() {
                                        function runSuite(resizerVal, componentVal, move, offset) {
                                            createSuitePanel({
                                                maxSize: resizerVal,
                                                edges: 'south'
                                            }, {
                                                maxHeight: componentVal
                                            });
                                            dragAndMove('south', move, offset);
                                        }

                                        it("should favour the component value when the component value is larger", function() {
                                            runSuite([null, 400], 500, [null, 300], { bottom: 200 });
                                        });

                                        it("should favour the component value when the component value is smaller", function() {
                                            runSuite([null, 500], 400, [null, 300], { bottom: 100 });
                                        });
                                    });
                                });

                                describe("relative constraints on the component", function() {
                                    // TODO
                                });

                                describe("constrainToParent", function() {
                                    // TODO
                                });
                            });

                            describe("preserveRatio", function() {
                            });
                        });
                    }
                    makeFloatPositionSuite(false);
                    makeFloatPositionSuite(true);
                });
            }

            makeDynamicSuite(false);
            makeDynamicSuite(true);
        });

        describe("with split: true", function() {
            function makeDynamicSuite(dynamic) {
                function expectProxyOffset(offset) {
                    var box = resizable.getProxy().getRegion(),
                        key;

                    for (key in offset) {
                        expect(box[key]).toBe(startBox[key] + offset[key]);
                    }
                }

                function runsExpectProxyOffset(offset) {
                    var allowed = {
                        top: 1,
                        right: 1,
                        bottom: 1,
                        left: 1
                    };

                    runs(function() {
                        var o = {},
                            key;

                        for (key in offset) {
                            if (allowed.hasOwnProperty('key')) {
                                o[key] = offset[key];
                            }
                        }
                        expectProxyOffset(o);
                    });
                }

                var ctBaseSize = 600,
                    runsBoxOffsetFn = dynamic ? runsExpectBoxOffset : runsExpectProxyOffset,
                    ct;

                function createSuitePanel(horz, atStart, resizerCfg, panelCfg) {
                    resizerCfg = Ext.apply({
                        dynamic: dynamic,
                        split: true
                    }, resizerCfg);

                    var id = Ext.id(),
                        other = {
                            flex: 1
                        }, 
                        p = Ext.apply({
                            id: id,
                            resizable: resizerCfg
                        }, panelCfg);

                    p[horz ? 'width' : 'height'] = baseSize;

                    ct = new Ext.container.Container({
                        renderTo: Ext.getBody(),
                        width: ctBaseSize,
                        height: ctBaseSize,
                        defaultType: 'panel',
                        layout: {
                            type: 'box',
                            vertical: !horz,
                            align: 'stretch'
                        },
                        items: atStart ? [p, other] : [other, p]
                    });

                    panel = Ext.getCmp(id);
                    resizable = panel.getResizable();
                }

                function createDockedSuitePanel(dock, resizerCfg, panelCfg) {
                    panelCfg = Ext.apply({
                        docked: dock
                    }, panelCfg);
                    createSuitePanel(dock === 'left' || dock === 'right', true, resizerCfg, panelCfg);
                }

                function moveForEdge(edge, move) {
                    var horz = edgeInfo[edge].horz;

                    return [horz ? move : null, horz ? null : move];
                }

                function dragAndMove(dragEdge, moves, expectedOffsets) {
                    if (!Ext.isArray(moves[0])) {
                        moves = [moves];
                    }

                    if (!Ext.isArray(expectedOffsets)) {
                        expectedOffsets = [expectedOffsets];
                    }

                    var len = moves.length,
                        cumulativeOffset = new Ext.util.Region(0, 0, 0, 0),
                        i, offsets;

                    startDrag(dragEdge);
                    for (i = 0; i < len; ++i) {
                        offsets = expectedOffsets[i];
                        adjustBox(cumulativeOffset, offsets);

                        moveBy(moves[i]);
                        runsBoxOffsetFn(cumulativeOffset.copy());
                    }
                    endDrag();
                    // Complete, so always check the element box
                    runsExpectBoxOffset(cumulativeOffset);
                }

                function simpleDragMove(edge, move, offsets, resizerCfg, panelCfg) {
                    createSuitePanel(edgeInfo[edge].horz, edge === 'east' || edge === 'south', Ext.apply({
                        edges: edge
                    }, resizerCfg), panelCfg);
                    dragAndMove(edge, move, offsets);
                }

                var edgeToDock = {
                    north: 'bottom',
                    east: 'left',
                    south: 'top',
                    west: 'right'
                };

                function dockDragMove(edge, move, offsets, resizerCfg, panelCfg) {
                    createDockedSuitePanel(edgeToDock[edge], Ext.apply({
                        edges: edge
                    }, resizerCfg), panelCfg);
                    dragAndMove(edge, move, offsets);
                }

                afterEach(function() {
                    ct = Ext.destroy(ct);
                });

                describe("with dynamic: " + dynamic, function() {
                    describe("basic functionality", function() {
                        function makeDockedNonDocked(prefix, edge, moves, offsets) {
                            if (!Ext.isArray(moves[0])) {
                                moves = [moves];
                            }

                            if (!Ext.isArray(offsets)) {
                                offsets = [offsets];
                            }

                            describe("edge: " + edge, function() {
                                describe("not docked", function() {
                                    it(prefix + " larger", function() {
                                        simpleDragMove(edge, moves, offsets);
                                    });

                                    it(prefix + " smaller", function() {
                                        simpleDragMove(edge, moves.map(negateMove), offsets.map(negateBox));
                                    });
                                });

                                describe("docked", function() {
                                    it(prefix + " larger", function() {
                                        dockDragMove(edge, moves, offsets);
                                    });

                                    it(prefix + " smaller", function() {
                                        dockDragMove(edge, moves.map(negateMove), offsets.map(negateBox));
                                    });
                                });
                            });
                        }

                        describe("basic drag in a single direction", function() {
                            function makeSuite(edge, move, offset) {
                                move = moveForEdge(edge, move);
                                makeDockedNonDocked("should be able to resize", edge, move, offset);
                            }

                            makeSuite('north', -50, { top: -50 });
                            makeSuite('east', 50, { right: 50 });
                            makeSuite('south', 40, { bottom: 40 } );
                            makeSuite('west', -70, { left: -70 });
                        });

                        describe("ignoring movement in other directions", function() {
                            function makeSuite(otherAxis, edge, move, offset) {
                                makeDockedNonDocked("should ignore movements in the " + otherAxis + " when resizing", edge, move, offset);
                            }

                            makeSuite('x axis', 'north', [30, -50], { top: -50 });
                            makeSuite('y axis', 'east', [30, 50], { right: 30 });
                            makeSuite('x axis', 'south', [-30, 50], { bottom: 50 });
                            makeSuite('y axis', 'west', [-30, -20], { left: -30 });
                        });

                        describe("multiple moves", function() {
                            function makeSuite(edge, moves, offsets) {
                                makeDockedNonDocked("should make multiple moves when resizing", edge, moves, offsets);
                            }

                            makeSuite('north', [[null, -30], [null, -60]], [{ top: -30 }, { top: -60 }]);
                            makeSuite('east', [[60, null], [120, null]], [{ right: 60 }, { right: 120 }]);
                            makeSuite('south', [[null, 70], [null, 140]], [{ bottom: 70 }, { bottom: 140 }]);
                            makeSuite('west', [[-50, null], [-100, null]], [{ left: -50 }, { left: -100 }]);
                        });
                    });

                    describe("snapping", function() {
                        var snapBase = 20;

                        function makeSuite(snap, edge, moves, offsets) {
                            var expandedSnap = !Ext.isArray(snap) ? [snap, snap] : snap,
                                key = 'not snap',
                                edgeItem = edgeInfo[edge],
                                horz = edgeItem.horz,
                                vert = edgeItem.vert,
                                snapHorz = horz && expandedSnap[0] !== null,
                                snapVert = vert && expandedSnap[1] !== null;

                            if (snapVert) {
                                key = 'vertically';
                            } else if (snapHorz) {
                                key = 'horizontally';
                            }

                            moves = moves.map(function(move) {
                                return moveForEdge(edge, move);
                            });

                            offsets = offsets.map(function(offset) {
                                var o = {};
                                o[(horz || vert).offsetKey] = offset;
                                return o;
                            });

                            describe("edge: " + edge, function() {
                                describe("not docked", function() {
                                    it("should snap " + key + " when resizing larger", function() {
                                        simpleDragMove(edge, moves, offsets, {
                                            snap: snap
                                        });
                                    });

                                    it("should snap " + key + " when resizing smaller", function() {
                                        simpleDragMove(edge, moves.map(negateMove), offsets.map(negateBox), {
                                            snap: snap
                                        });
                                    });
                                });

                                describe("docked", function() {
                                    it("should snap " + key + " when resizing larger", function() {
                                        dockDragMove(edge, moves, offsets, {
                                            snap: snap
                                        });
                                    });

                                    it("should snap " + key + " when resizing smaller", function() {
                                        dockDragMove(edge, moves.map(negateMove), offsets.map(negateBox), {
                                            snap: snap
                                        });
                                    });
                                });
                            });
                        }

                        describe("single number", function() {
                            makeSuite(snapBase, 'north',
                                [-8, -1, -1, -10, -10, -20, -30, -5],
                                [0, 0, -snapBase, 0, -snapBase, -snapBase, -snapBase, 0]
                            );

                            makeSuite(snapBase, 'east',
                                [8, 1, 1, 10, 10, 20, 30, 5],
                                [0, 0, snapBase, 0, snapBase, snapBase, snapBase, 0]
                            );

                            makeSuite(snapBase, 'south',
                                [8, 1, 1, 10, 10, 20, 30, 5],
                                [0, 0, snapBase, 0, snapBase, snapBase, snapBase, 0]
                            );

                            makeSuite(snapBase, 'west',
                                [-8, -1, -1, -10, -10, -20, -30, -5],
                                [0, 0, -snapBase, 0, -snapBase, -snapBase, -snapBase, 0]
                            );
                        });

                        describe("x with a value, y null", function() {
                            var snap = [snapBase, null];

                            makeSuite(snap, 'north',
                                [-8, -1, -1, -10, -10, -20, -30, -5],
                                [-8, -1, -1, -10, -10, -20, -30, -5]
                            );

                            makeSuite(snap, 'east',
                                [8, 1, 1, 10, 10, 20, 30, 5],
                                [0, 0, snapBase, 0, snapBase, snapBase, snapBase, 0]
                            );

                            makeSuite(snap, 'south',
                                [8, 1, 1, 10, 10, 20, 30, 5],
                                [8, 1, 1, 10, 10, 20, 30, 5]
                            );

                            makeSuite(snap, 'west',
                                [-8, -1, -1, -10, -10, -20, -30, -5],
                                [0, 0, -snapBase, 0, -snapBase, -snapBase, -snapBase, 0]
                            );
                        });

                        describe("y with a value, x null", function() {
                            var snap = [null, snapBase];

                            makeSuite(snap, 'north',
                                [-8, -1, -1, -10, -10, -20, -30, -5],
                                [0, 0, -snapBase, 0, -snapBase, -snapBase, -snapBase, 0]
                            );

                            makeSuite(snap, 'east',
                                [8, 1, 1, 10, 10, 20, 30, 5],
                                [8, 1, 1, 10, 10, 20, 30, 5]
                            );

                            makeSuite(snap, 'south',
                                [8, 1, 1, 10, 10, 20, 30, 5],
                                [0, 0, snapBase, 0, snapBase, snapBase, snapBase, 0]
                            );

                            makeSuite(snap, 'west',
                                [-8, -1, -1, -10, -10, -20, -30, -5],
                                [-8, -1, -1, -10, -10, -20, -30, -5]
                            );
                        });

                        describe("separate values for x & y", function() {
                            var xSnap = 30,
                                ySnap = 20,
                                snap = [xSnap, ySnap];

                            makeSuite(snap, 'north',
                                [-30, -10, -10, -20],
                                [-ySnap * 2, 0, -ySnap, -ySnap]
                            );

                            makeSuite(snap, 'east',
                                [10, 5, 30, 5],
                                [0, xSnap, xSnap, 0]
                            );

                            makeSuite(snap, 'south',
                                [30, 10, 10, 20],
                                [ySnap * 2, 0, ySnap, ySnap]
                            );

                            makeSuite(snap, 'west',
                                [-10, -5, -30, -5],
                                [0, -xSnap, -xSnap, 0]
                            );
                        });

                        describe("dynamically setting the value", function() {
                            describe("no snap -> snap", function() {
                                function runSuite() {
                                    startDrag('east');
                                    moveBy(10, null);
                                    endDrag();
                                    runsExpectBoxOffset({
                                        right: 10
                                    });
                                    runs(function() {
                                        resizable.setSnap(30);
                                    });
                                    startDrag('east');
                                    moveBy(40, null);
                                    endDrag();
                                    runsExpectBoxOffset({
                                        right: 50
                                    });
                                }

                                describe("not docked", function() {
                                    it("should respect the snap value", function() {
                                        createSuitePanel(true, true, {
                                            edges: 'east'
                                        });
                                        runSuite();
                                    });
                                });

                                describe("docked", function() {
                                    it("should respect the snap value", function() {
                                        createDockedSuitePanel('left', {
                                            edges: 'east'
                                        });
                                        runSuite();
                                    });
                                });
                            });

                            describe("snap -> no snap", function() {
                                function runSuite() {
                                    startDrag('east');
                                    moveBy(20, null);
                                    endDrag();
                                    runsExpectBoxOffset({
                                        right: 30
                                    });
                                    runs(function() {
                                        resizable.setSnap(null);
                                    });
                                    startDrag('east');
                                    moveBy(10, null);
                                    endDrag();
                                    runsExpectBoxOffset({
                                        right: 10
                                    });
                                }

                                describe("not docked", function() {
                                    it("should respect the cleared snap value", function() {
                                        createSuitePanel(true, true, {
                                            edges: 'east',
                                            snap: 30
                                        });
                                        runSuite();
                                    });
                                });

                                describe("docked", function() {
                                    it("should respect the snap value", function() {
                                        createDockedSuitePanel('left', {
                                            edges: 'east',
                                            snap: 30
                                        });
                                        runSuite();
                                    });
                                });
                            });

                            describe("change snap value", function() {
                                function runSuite() {
                                    startDrag('east');
                                    moveBy(20, null);
                                    endDrag();
                                    runsExpectBoxOffset({
                                        right: 30
                                    });
                                    runs(function() {
                                        resizable.setSnap(50);
                                    });
                                    startDrag('east');
                                    moveBy(45, null);
                                    endDrag();
                                    runsExpectBoxOffset({
                                        right: 70
                                    });
                                }

                                describe("not docked", function() {
                                    it("should respect the new snap value", function() {
                                        createSuitePanel(true, true, {
                                            edges: 'east',
                                            snap: 30
                                        });
                                        runSuite();
                                    });
                                });

                                describe("docked", function() {
                                    it("should respect the snap value", function() {
                                        createDockedSuitePanel('left', {
                                            edges: 'east',
                                            snap: 30
                                        });
                                        runSuite();
                                    });
                                });
                            });
                        });

                        // This is not supposed to be extensive, just that constraints work with snapping
                        describe("with constraints", function() {
                            var snap = 50,
                                minSize = 120,
                                maxSize = 420;

                            function makeSuite(edge, move, maxOffset, minOffset) {
                                describe("edge: " + edge, function() {
                                    describe("not docked", function() {
                                        it("should constrain to the max size", function() {
                                            simpleDragMove(edge, move, maxOffset, {
                                                maxSize: maxSize,
                                                snap: snap
                                            });
                                        });

                                        it("should constrain to the min size", function() {
                                            simpleDragMove(edge, negateMove(move), minOffset, {
                                                minSize: minSize,
                                                snap: snap
                                            });
                                        });
                                    });

                                    describe("docked", function() {
                                        it("should constrain to the max size", function() {
                                            dockDragMove(edge, move, maxOffset, {
                                                maxSize: maxSize,
                                                snap: snap
                                            });
                                        });

                                        it("should constrain to the min size", function() {
                                            dockDragMove(edge, negateMove(move), minOffset, {
                                                minSize: minSize,
                                                snap: snap
                                            });
                                        });
                                    });
                                });
                            }

                            makeSuite('north', [null, -200], { top: -(maxSize - baseSize) }, { top: baseSize - minSize } );
                            makeSuite('east', [200, null], { right: maxSize - baseSize }, { right: -(baseSize - minSize) });
                            makeSuite('south', [null, 200], { bottom: maxSize - baseSize }, { bottom: -(baseSize - minSize) });
                            makeSuite('west', [-200, null], { left: -(maxSize - baseSize) }, { left: baseSize - minSize });
                        });
                    });

                    describe("constraints", function() {
                        describe("basic constraints", function() {
                            var min = 200,
                                max = 400,
                                minOffset = baseSize - min,
                                maxOffset = max - baseSize;

                            function runConstrainSuite(createFn, onComponent, prop, constrainVal, edge, move, offsets) {
                                var panelCfg = {},
                                    resizerCfg = { edges: edge };

                                if (onComponent) {
                                    panelCfg[prop] = constrainVal;
                                } else {
                                    resizerCfg[prop] = constrainVal;
                                }
                                createFn(edge, move, offsets, resizerCfg, panelCfg);
                            }

                            describe("on the component", function() {
                                function makeComponentConstrainSuite(prop, name, constrainVal, edge, move, offsets) {
                                    var isHorz = prop === 'minWidth' || prop === 'maxWidth',
                                        constrain = edgeInfo[edge][isHorz ? 'horz' : 'vert'],
                                        qualifier = constrain ? '' : ' not';

                                    describe("edge: " + edge, function() {
                                        describe("not docked", function() {
                                            it("should" + qualifier + " constrain the " + name, function() {
                                                runConstrainSuite(simpleDragMove, true, prop, constrainVal, edge, move, offsets);
                                            });
                                        });

                                        describe("docked", function() {
                                            it("should" + qualifier + " constrain the " + name, function() {
                                                runConstrainSuite(dockDragMove, true, prop, constrainVal, edge, move, offsets);
                                            });
                                        });
                                    });
                                }

                                describe("minWidth", function() {
                                    function makeSuite(edge, move, offsets) {
                                        makeComponentConstrainSuite('minWidth', 'width', min, edge, move, offsets);
                                    }

                                    makeSuite('north', [null, 150], { top: 150 });
                                    makeSuite('east', [-150, null], { right: -minOffset });
                                    makeSuite('south', [null, -150], { bottom: -150 });
                                    makeSuite('west', [150, null], { left: minOffset });
                                });

                                describe("minHeight", function() {
                                    function makeSuite(edge, move, offsets) {
                                        makeComponentConstrainSuite('minHeight', 'height', min, edge, move, offsets);
                                    }

                                    makeSuite('north', [null, 150], { top: minOffset });
                                    makeSuite('east', [-150, null], { right: -150 });
                                    makeSuite('south', [null, -150], { bottom: -minOffset });
                                    makeSuite('west', [150, null], { left: 150 });
                                });

                                describe("maxWidth", function() {
                                    function makeSuite(edge, move, offsets) {
                                        makeComponentConstrainSuite('maxWidth', 'width', max, edge, move, offsets);
                                    }

                                    makeSuite('north', [null, -150], { top: -150 });
                                    makeSuite('east', [150, null], { right: maxOffset });
                                    makeSuite('south', [null, 150], { bottom: 150 });
                                    makeSuite('west', [-150, null], { left: -maxOffset });
                                });

                                describe("maxHeight", function() {
                                    function makeSuite(edge, move, offsets) {
                                        makeComponentConstrainSuite('maxHeight', 'height', max, edge, move, offsets);
                                    }

                                    makeSuite('north', [null, -150], { top: -maxOffset });
                                    makeSuite('east', [150, null], { right: 150 });
                                    makeSuite('south', [null, 150], { bottom: maxOffset });
                                    makeSuite('west', [-150, null], { left: -150 });
                                });
                            });

                            describe("on the resizer", function() {
                                function makeResizerConstrainSuite(prop, constrainVal, edge, move, offsets) {
                                    var expanded = typeof constrainVal === 'number' ? [constrainVal, constrainVal] : constrainVal,
                                        constrainW = expanded[0] !== null,
                                        constrainH = expanded[1] !== null,
                                        msg = [],
                                        append;

                                    if (edgeInfo[edge].horz) {
                                        if (constrainW) {
                                            msg.push('constrain the width');
                                        } else {
                                            msg.push('not constrain the width');
                                        }
                                    }

                                    if (edgeInfo[edge].horz) {
                                        append = msg.length ? ' & ' : '';
                                        if (constrainH) {
                                            msg.push(append + 'constrain the height');
                                        } else {
                                            msg.push(append + 'not constrain the height');
                                        }
                                    }

                                    describe("edge: " + edge, function() {
                                        describe("not docked", function() {
                                            it("should " + msg.join(''), function() {
                                                runConstrainSuite(simpleDragMove, false, prop, constrainVal, edge, move, offsets);
                                            });
                                        });

                                        describe("docked", function() {
                                            it("should " + msg.join(''), function() {
                                                runConstrainSuite(dockDragMove, false, prop, constrainVal, edge, move, offsets);
                                            });
                                        });
                                    });
                                }

                                describe("minSize", function() {
                                    describe("as a number", function() {
                                        function makeSuite(edge, move, offsets) {
                                            makeResizerConstrainSuite('minSize', min, edge, move, offsets);
                                        }

                                        makeSuite('north', [null, 150], { top: minOffset });
                                        makeSuite('east', [-150, null], { right: -minOffset });
                                        makeSuite('south', [null, -150], { bottom: -minOffset });
                                        makeSuite('west', [150, null], { left: minOffset });
                                    });

                                    describe("with a width only value", function() {
                                        function makeSuite(edge, move, offsets) {
                                            makeResizerConstrainSuite('minSize', [min, null], edge, move, offsets);
                                        }

                                        makeSuite('north', [null, 150], { top: 150 });
                                        makeSuite('east', [-150, null], { right: -minOffset });
                                        makeSuite('south', [null, -150], { bottom: -150 });
                                        makeSuite('west', [150, null], { left: minOffset });
                                    });

                                    describe("height only value", function() {
                                        function makeSuite(edge, move, offsets) {
                                            makeResizerConstrainSuite('minSize', [null, min], edge, move, offsets);
                                        }

                                        makeSuite('north', [null, 150], { top: minOffset });
                                        makeSuite('east', [-150, null], { right: -150 });
                                        makeSuite('south', [null, -150], { bottom: -minOffset });
                                        makeSuite('west', [150, null], { left: 150 });
                                    });

                                    describe("width and height", function() {
                                        var minW = min + 5,
                                            minH = min - 5,
                                            minWOffset = baseSize - minW,
                                            minHOffset = baseSize - minH;

                                        function makeSuite(edge, move, offsets) {
                                            makeResizerConstrainSuite('minSize', [minW, minH], edge, move, offsets);
                                        }

                                        makeSuite('north', [null, 150], { top: minHOffset });
                                        makeSuite('east', [-150, null], { right: -minWOffset });
                                        makeSuite('south', [null, -150], { bottom: -minHOffset });
                                        makeSuite('west', [150, null], { left: minWOffset });
                                    });
                                });

                                describe("maxSize", function() {
                                    describe("as a number", function() {
                                        function makeSuite(edge, move, offsets) {
                                            makeResizerConstrainSuite('maxSize', max, edge, move, offsets);
                                        }

                                        makeSuite('north', [null, -150], { top: -maxOffset });
                                        makeSuite('east', [150, null], { right: maxOffset });
                                        makeSuite('south', [null, 150], { bottom: maxOffset });
                                        makeSuite('west', [-150, null], { left: -maxOffset });
                                    });

                                    describe("with a width only value", function() {
                                        function makeSuite(edge, move, offsets) {
                                            makeResizerConstrainSuite('maxSize', [max, null], edge, move, offsets);
                                        }

                                        makeSuite('north', [null, -150], { top: -150 });
                                        makeSuite('east', [150, null], { right: maxOffset });
                                        makeSuite('south', [null, 150], { bottom: 150 });
                                        makeSuite('west', [-150, null], { left: -maxOffset });
                                    });

                                    describe("height only value", function() {
                                        function makeSuite(edge, move, offsets) {
                                            makeResizerConstrainSuite('maxSize', [null, max], edge, move, offsets);
                                        }

                                        makeSuite('north', [null, -150], { top: -maxOffset });
                                        makeSuite('east', [150, null], { right: 150 });
                                        makeSuite('south', [null, 150], { bottom: maxOffset });
                                        makeSuite('west', [-150, null], { left: -150 });
                                    });

                                    describe("width and height", function() {
                                        var maxW = max + 5,
                                            maxH = max - 5,
                                            maxWOffset = maxW - baseSize,
                                            maxHOffset = maxH - baseSize;

                                        function makeSuite(edge, move, offsets) {
                                            makeResizerConstrainSuite('maxSize', [maxW, maxH], edge, move, offsets);
                                        }

                                        makeSuite('north', [null, -150], { top: -maxHOffset });
                                        makeSuite('east', [150, null], { right: maxWOffset });
                                        makeSuite('south', [null, 150], { bottom: maxHOffset });
                                        makeSuite('west', [-150, null], { left: -maxWOffset });
                                    });
                                });
                            });
                        });

                        describe("component vs resizable", function() {
                            describe("minWidth", function() {
                                function runSuite(createFn, resizerVal, componentVal, move, offset) {
                                    createFn('east', move, offset, {
                                        minSize: resizerVal,
                                        edges: 'east'
                                    }, {
                                        minWidth: componentVal
                                    });
                                }

                                function makeMinSuite(createFn) {
                                    it("should favour the component value when the component value is larger", function() {
                                        runSuite(createFn, [100, null], 200, [-250, null], { right: -100 });
                                    });

                                    it("should favour the component value when the component value is smaller", function() {
                                        runSuite(createFn, [200, null], 100, [-250, null], { right: -200 });
                                    });
                                }

                                describe("not docked", function() {
                                    makeMinSuite(simpleDragMove);
                                });

                                describe("docked", function() {
                                    makeMinSuite(dockDragMove);
                                });
                            });

                            describe("minHeight", function() {
                                function runSuite(createFn, resizerVal, componentVal, move, offset) {
                                    createFn('south', move, offset, {
                                        minSize: resizerVal,
                                        edges: 'south'
                                    }, {
                                        minHeight: componentVal
                                    });
                                }

                                function makeMinSuite(createFn) {
                                    it("should favour the component value when the component value is larger", function() {
                                        runSuite(createFn, [null, 100], 200, [null, -250], { bottom: -100 });
                                    });

                                    it("should favour the component value when the component value is smaller", function() {
                                        runSuite(createFn, [null, 200], 100, [null, -250], { bottom: -200 });
                                    });
                                }

                                describe("not docked", function() {
                                    makeMinSuite(simpleDragMove);
                                });

                                describe("docked", function() {
                                    makeMinSuite(dockDragMove);
                                });
                            });

                            describe("maxWidth", function() {
                                function runSuite(createFn, resizerVal, componentVal, move, offset) {
                                    createFn('east', move, offset, {
                                        maxSize: resizerVal,
                                        edges: 'east'
                                    }, {
                                        maxWidth: componentVal
                                    });
                                }

                                function makeMaxSuite(createFn) {
                                    it("should favour the component value when the component value is larger", function() {
                                        runSuite(createFn, [400, null], 500, [300, null], { right: 200 });
                                    });

                                    it("should favour the component value when the component value is smaller", function() {
                                        runSuite(createFn, [500, null], 400, [300, null], { right: 100 });
                                    });
                                }

                                describe("not docked", function() {
                                    makeMaxSuite(simpleDragMove);
                                });

                                describe("docked", function() {
                                    makeMaxSuite(dockDragMove);
                                });
                            });

                            describe("maxHeight", function() {
                                function runSuite(createFn, resizerVal, componentVal, move, offset) {
                                    createFn('south', move, offset, {
                                        maxSize: resizerVal,
                                        edges: 'south'
                                    }, {
                                        maxHeight: componentVal
                                    });
                                }

                                function makeMaxSuite(createFn) {
                                    it("should favour the component value when the component value is larger", function() {
                                        runSuite(createFn, [null, 400], 500, [null, 300], { bottom: 200 });
                                    });

                                    it("should favour the component value when the component value is smaller", function() {
                                        runSuite(createFn, [null, 500], 400, [null, 300], { bottom: 100 });
                                    });
                                }

                                describe("not docked", function() {
                                    makeMaxSuite(simpleDragMove);
                                });

                                describe("docked", function() {
                                    makeMaxSuite(dockDragMove);
                                });
                            });
                        });

                        describe("relative constraints on the component", function() {
                            // TODO
                        });

                        describe("constrainToParent", function() {
                            // TODO
                        });
                    });

                    if (!dynamic) {
                        describe("splitter positioning", function() {
                            function makeSuite(edge) {
                                var horz = edgeInfo[edge].horz,
                                    other = horz ? 'height' : 'width';

                                function runSuite(edge) {
                                    startDrag(edge);
                                    moveBy(moveForEdge(edge, 50));
                                    runs(function() {
                                        var proxyBox = resizable.getProxy().getRegion();
                                        if (horz) {
                                            expect(proxyBox.top).toBe(startBox.top);
                                            expect(proxyBox.bottom).toBe(startBox.bottom);
                                        } else {
                                            expect(proxyBox.left).toBe(startBox.left);
                                            expect(proxyBox.right).toBe(startBox.right);
                                        }
                                    });
                                    endDrag();
                                }

                                describe("edge: " + edge, function() {
                                    describe("not docked", function() {
                                        it("should set the " + other + " of the splitter to equal the box", function() {
                                            createSuitePanel(!!horz, edge === 'east' || edge === 'south', {
                                                edges: edge
                                            });
                                            panel.getParent().setMargin(100);
                                            runSuite(edge);
                                        });
                                    });

                                    describe("docked", function() {
                                        it("should set the " + other + " of the splitter to equal the box", function() {
                                            createDockedSuitePanel(edgeToDock[edge], {
                                                edges: edge
                                            });
                                            panel.getParent().setMargin(100);
                                            runSuite(edge);
                                        });
                                    });
                                });
                            }

                            makeSuite('north');
                            makeSuite('east');
                            makeSuite('south');
                            makeSuite('west');
                        });
                    }
                });
            }

            makeDynamicSuite(false);
            makeDynamicSuite(true);
        });

        describe("in box layout", function() {
            var ct;

            afterEach(function() {
                ct = Ext.destroy(ct);
            });

            describe("horizontal box", function() {
                it("should not clear the flex when resizing vertically", function() {
                    ct = new Ext.container.Container({
                        renderTo: document.body,
                        width: 400,
                        height: 400,
                        layout: 'hbox',
                        defaultType: 'panel',
                        items: [{
                            resizable: {
                                edges: 'south'
                            },
                            flex: 2
                        }, {
                            flex: 1
                        }]
                    });

                    panel = ct.items.first();
                    resizable = panel.getResizable();
                    expect(panel.element.getHeight()).toBe(400);

                    startDrag('south');
                    moveBy(0, -50);
                    endDrag();
                    runs(function() {
                        expect(panel.getHeight()).toBe(350);
                        expect(panel.getFlex()).toBe(2);
                    });
                });
            });

            describe("vertical box", function() {
                it("should not clear the flex when resizing horizontally", function() {
                    ct = new Ext.container.Container({
                        renderTo: document.body,
                        width: 400,
                        height: 400,
                        layout: 'vbox',
                        defaultType: 'panel',
                        items: [{
                            resizable: {
                                edges: 'east'
                            },
                            flex: 2
                        }, {
                            flex: 1
                        }]
                    });

                    panel = ct.items.first();
                    resizable = panel.getResizable();
                    expect(panel.element.getWidth()).toBe(400);

                    startDrag('east');
                    moveBy(-50, 0);
                    endDrag();
                    runs(function() {
                        expect(panel.getWidth()).toBe(350);
                        expect(panel.getFlex()).toBe(2);
                    });
                });
            });
        });
    });
});