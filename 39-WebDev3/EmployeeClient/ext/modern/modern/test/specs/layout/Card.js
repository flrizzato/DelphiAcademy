describe("Ext.layout.Card", function() {
    var ct,
        layout,
        animation;

    afterEach(function() {
        ct = Ext.destroy(ct);
    });

    function createContainer(cfg) {
        cfg = cfg || {};
        cfg = Ext.apply({
            xtype: 'container',
            layout: {
                type: 'card',
                animation: false
            },
            renderTo: Ext.getBody(),
            width: 200,
            height: 200,
            items: [{
                width: 100,
                height: 100
            }]

        }, cfg);

        ct = Ext.create(cfg);
        layout = ct.getLayout();
        animation = layout.getAnimation();
    }

    function setAnimation(newAnimation) {
        layout.setAnimation(newAnimation);
        animation = layout.getAnimation();
    }

    describe("deferRender", function() {
        var fields;

        function makeDeferredForm(deferred) {
            createContainer({
                xtype: 'formpanel',
                title: 'test',
                viewModel: {
                    data: {
                        field1: 'abc',
                        field2: 'def',
                        field3: 'ghi'
                    }
                },
                layout: {
                    type: 'card',
                    deferRender: deferred
                },
                items: [
                    {
                        items: [
                            {
                                xtype: 'textfield',
                                label: 'card1',
                                name: 'card1',
                                required: true,
                                bind: '{field1}'
                            }
                        ]
                    },
                    {
                        items: [
                            {
                                xtype: 'textfield',
                                required: true,
                                label: 'card2',
                                name: 'card2',
                                bind: '{field2}'
                            }
                        ]

                    },
                    {
                        items: [
                            {
                                xtype: 'textfield',
                                required: true,
                                label: 'card3',
                                name: 'card3',
                                bind: '{field3}'
                            }
                        ]

                    }
                ]
            });
            fields = ct.getFields();
        }

        it("should defer binding if deferRender is true", function() {
            makeDeferredForm(true);

            waitsFor(function() {
                return ct.items.items[0].innerItems[0].inputElement.dom.value !== '';
            });

            runs(function() {
                ct.validate();
                expect(ct.isValid()).toBe(false);    // it really is valid but the bindings aren't done; deferRender
                expect(fields.card1.isValid()).toBe(true);
                expect(fields.card2.isValid()).toBe(false);
                expect(fields.card3.isValid()).toBe(false);
            });
        });

        it("should  bind immediately if deferRender is false", function() {
            makeDeferredForm(false);

            waitsFor(function() {
                return ct.items.items[0].innerItems[0].inputElement.dom.value !== '';
            });

            runs(function() {
                ct.validate();
                expect(ct.isValid()).toBe(true);    // it really is valid, the bindings are already done
                expect(fields.card1.isValid()).toBe(true);
                expect(fields.card2.isValid()).toBe(true);
                expect(fields.card3.isValid()).toBe(true);
            });
        });
    });

    describe("Animations", function() {

        it("shouldn't create animation if animation config is falsy", function() {
            createContainer();
            expect(animation).toBe(null);
        });

        it("should default animation direction to null", function() {
            createContainer({
                layout: {
                    type: 'card',
                    animation: 'slide'
                }
            });
            expect(animation).not.toBe(null);
            expect(animation.getDirection()).toBe(null);
            expect(layout.autoDirection).toBe('horizontal');
        });

        it("should honor configured direction", function() {
            createContainer({
                layout: {
                    type: 'card',
                    animation: {
                        type: 'slide',
                        direction: 'left'
                    }
                }
            });
            expect(animation).not.toBe(null);
            expect(animation.getDirection()).toBe('left');
            expect(layout.autoDirection).toBe(null);
        });

        it("should honor configured direction horizontal", function() {
            createContainer({
                layout: {
                    type: 'card',
                    animation: {
                        type: 'slide',
                        direction: 'horizontal'
                    }
                }
            });
            expect(animation).not.toBe(null);
            expect(animation.getDirection()).toBe(null);
            expect(layout.autoDirection).toBe('horizontal');
        });

        it("should honor configured direction vertical", function() {
            createContainer({
                layout: {
                    type: 'card',
                    animation: {
                        type: 'slide',
                        direction: 'vertical'
                    }
                }
            });
            expect(animation).not.toBe(null);
            expect(animation.getDirection()).toBe(null);
            expect(layout.autoDirection).toBe('vertical');
        });

        it("should automatically choose correct direction when animating card change", function() {
            createContainer({
                layout: {
                    type: 'card',
                    animation: 'slide'
                },
                items: [{
                    html: 'card 1'
                }, {
                    html: 'card 2'
                }]
            });
            expect(animation.isAnimating).toBe(false);
            ct.setActiveItem(1);
            expect(animation.isAnimating).toBe(true);
            expect(animation.getDirection()).toBe('left');
            waitsFor(function() {
                return !animation.isAnimating;
            });
            runs(function() {
                expect(animation.getDirection()).toBe('left');
                ct.setActiveItem(0);
                expect(animation.getDirection()).toBe('right');
            });
            waitsFor(function() {
                return !animation.isAnimating;
            });
            runs(function() {
                expect(animation.getDirection()).toBe('right');
            });
        });

        it("should automatically choose correct direction when animating card change, horizontal", function() {
            createContainer({
                layout: {
                    type: 'card',
                    animation: {
                        type: 'slide',
                        direction: 'horizontal'
                    }
                },
                items: [{
                    html: 'card 1'
                }, {
                    html: 'card 2'
                }]
            });
            expect(animation.isAnimating).toBe(false);
            ct.setActiveItem(1);
            expect(animation.isAnimating).toBe(true);
            expect(animation.getDirection()).toBe('left');
            waitsFor(function() {
                return !animation.isAnimating;
            });
            runs(function() {
                expect(animation.getDirection()).toBe('left');
                ct.setActiveItem(0);
                expect(animation.getDirection()).toBe('right');
            });
            waitsFor(function() {
                return !animation.isAnimating;
            });
            runs(function() {
                expect(animation.getDirection()).toBe('right');
            });
        });

        it("should automatically choose correct direction when animating card change, vertical", function() {
            createContainer({
                layout: {
                    type: 'card',
                    animation: {
                        type: 'slide',
                        direction: 'vertical'
                    }
                },
                items: [{
                    html: 'card 1'
                }, {
                    html: 'card 2'
                }]
            });
            expect(animation.isAnimating).toBe(false);
            ct.setActiveItem(1);
            expect(animation.isAnimating).toBe(true);
            expect(animation.getDirection()).toBe('down');
            waitsFor(function() {
                return !animation.isAnimating;
            });
            runs(function() {
                expect(animation.getDirection()).toBe('down');
                ct.setActiveItem(0);
                expect(animation.getDirection()).toBe('up');
            });
            waitsFor(function() {
                return !animation.isAnimating;
            });
            runs(function() {
                expect(animation.getDirection()).toBe('up');
            });
        });

        it("should not automatically choose direction when setAnimation(null) called", function() {
            createContainer({
                layout: {
                    type: 'card',
                    animation: 'slide'
                },
                items: [{
                    html: 'card 1'
                }, {
                    html: 'card 2'
                }]
            });
            expect(animation.isAnimating).toBe(false);
            setAnimation(null);
            ct.setActiveItem(1);
            expect(layout.getAnimation()).toBe(null);
            expect(ct.getActiveItem().getHtml()).toBe('card 2');
            ct.setActiveItem(0);
            expect(layout.getAnimation()).toBe(null);
            expect(ct.getActiveItem().getHtml()).toBe('card 1');
        });

        it("should automatically choose direction when setAnimation() called", function() {
            createContainer({
                layout: {
                    type: 'card'
                },
                items: [{
                    html: 'card 1'
                }, {
                    html: 'card 2'
                }]
            });
            expect(animation).toBe(null);
            setAnimation({
                type: 'slide'
            });
            expect(animation).not.toBe(null);
            expect(animation.getDirection()).toBe(null);
            expect(animation.isAnimating).toBe(false);
            ct.setActiveItem(1);
            expect(animation.isAnimating).toBe(true);
            expect(animation.getDirection()).toBe('left');
            waitsFor(function() {
                return !animation.isAnimating;
            });
            runs(function() {
                expect(animation.getDirection()).toBe('left');
                ct.setActiveItem(0);
                expect(animation.getDirection()).toBe('right');
            });
            waitsFor(function() {
                return !animation.isAnimating;
            });
            runs(function() {
                expect(animation.getDirection()).toBe('right');
            });
        });

        it("should honor direction when setAnimation() called", function() {
            createContainer({
                layout: {
                    type: 'card'
                },
                items: [{
                    html: 'card 1'
                }, {
                    html: 'card 2'
                }]
            });
            expect(animation).toBe(null);
            setAnimation({
                type: 'slide',
                direction: 'left'
            });
            expect(animation).not.toBe(null);
            expect(animation.getDirection()).toBe('left');
            expect(animation.isAnimating).toBe(false);
            ct.setActiveItem(1);
            expect(animation.isAnimating).toBe(true);
            expect(animation.getDirection()).toBe('left');
            waitsFor(function() {
                return !animation.isAnimating;
            });
            runs(function() {
                expect(animation.getDirection()).toBe('left');
                ct.setActiveItem(0);
                expect(animation.getDirection()).toBe('left');
            });
            waitsFor(function() {
                return !animation.isAnimating;
            });
            runs(function() {
                expect(animation.getDirection()).toBe('left');
            });
        });
    });

    describe('indicator', function () {
        it('should not create an indicator', function () {
            ct = Ext.create({
                xtype: 'container',
                layout: 'card'
            });

            var layout = ct.getLayout();

            expect(layout.getConfig('indicator', null, true)).toBe(null);
        });

        it('should create an indicator', function () {
            ct = Ext.create({
                xtype: 'container',
                layout: 'card'
            });

            var layout = ct.getLayout();

            expect(layout.getIndicator()).not.toBe(null);
        });

        it('should create number of dots', function () {
            ct = Ext.create({
                xtype: 'container',
                layout: 'card',
                items: [
                    {},
                    {},
                    {},
                    {}
                ]
            });

            var layout = ct.getLayout(),
                indicator = layout.getIndicator(),
                dots = indicator.indicators;

            expect(dots.length).toBe(4);
        });

        it('should add a dot when an item is added', function () {
            ct = Ext.create({
                xtype: 'container',
                layout: 'card',
                items: [
                    {},
                    {},
                    {},
                    {}
                ]
            });

            var layout = ct.getLayout(),
                indicator = layout.getIndicator(),
                dots = indicator.indicators;

            expect(dots.length).toBe(4);

            ct.add({});

            expect(dots.length).toBe(5);
        });

        it('should remove a dot when an item is removed', function () {
            ct = Ext.create({
                xtype: 'container',
                layout: 'card',
                items: [
                    {},
                    {},
                    {},
                    {}
                ]
            });

            var layout = ct.getLayout(),
                indicator = layout.getIndicator(),
                dots = indicator.indicators;

            expect(dots.length).toBe(4);

            ct.remove(ct.getAt(0));

            expect(dots.length).toBe(3);
        });

        it('should set the first dot as active', function () {
            ct = Ext.create({
                xtype: 'container',
                layout: 'card',
                items: [
                    {},
                    {},
                    {},
                    {}
                ]
            });

            var layout = ct.getLayout(),
                indicator = layout.getIndicator(),
                indicators = indicator.indicators;

            expect(indicators[0].hasCls('x-indicator-active')).toBe(true);
            expect(indicator.getActiveIndex()).toBe(0);
        });

        it('should set proper dot as active when active item changes', function () {
            ct = Ext.create({
                xtype: 'container',
                layout: 'card',
                items: [
                    {},
                    {},
                    {},
                    {}
                ]
            });

            var layout = ct.getLayout(),
                indicator = layout.getIndicator(),
                indicators = indicator.indicators;

            expect(indicators[0].hasCls('x-indicator-active')).toBe(true);
            expect(indicator.getActiveIndex()).toBe(0);

            ct.setActiveItem(3);

            expect(indicators[0].hasCls('x-indicator-active')).toBe(false);
            expect(indicators[3].hasCls('x-indicator-active')).toBe(true);
            expect(indicator.getActiveIndex()).toBe(3);
        });

        it('should set first dot active when current active item is removed', function () {
            ct = Ext.create({
                xtype: 'container',
                layout: 'card',
                items: [
                    {},
                    {},
                    {},
                    {}
                ]
            });

            var layout = ct.getLayout(),
                indicator = layout.getIndicator(),
                indicators = indicator.indicators;

            ct.setActiveItem(3);
            ct.remove(ct.getActiveItem());

            expect(indicators[0].hasCls('x-indicator-active')).toBe(true);
            expect(ct.getActiveItem()).toBe(ct.getAt(0));
            expect(indicator.getActiveIndex()).toBe(0);
        });
    });

    describe('next', function () {
        it('should set the next item active', function () {
            ct = Ext.create({
                xtype: 'container',
                layout: 'card',
                items: [
                    {},
                    {}
                ]
            });

            var layout = ct.getLayout();

            expect(ct.getActiveItem()).toBe(ct.getAt(0));

            layout.next();

            expect(ct.getActiveItem()).toBe(ct.getAt(1));
        });

        it('should not try to go next if on last item', function () {
            ct = Ext.create({
                xtype: 'container',
                layout: 'card',
                items: [
                    {},
                    {}
                ]
            });

            ct.setActiveItem(1);

            var layout = ct.getLayout();

            expect(ct.getActiveItem()).toBe(ct.getAt(1));

            layout.next();

            expect(ct.getActiveItem()).toBe(ct.getAt(1));
        });

        it('should keep the indicator in sync', function () {
            ct = Ext.create({
                xtype: 'container',
                layout: 'card',
                items: [
                    {},
                    {},
                    {}
                ]
            });

            var layout = ct.getLayout(),
                indicator = layout.getIndicator(),
                indicators = indicator.indicators;

            expect(ct.getActiveItem()).toBe(ct.getAt(0));
            expect(indicator.getActiveIndex()).toBe(0);
            expect(indicators[0].hasCls('x-indicator-active')).toBe(true);
            expect(indicators[1].hasCls('x-indicator-active')).toBe(false);
            expect(indicators[2].hasCls('x-indicator-active')).toBe(false);

            layout.next();

            expect(ct.getActiveItem()).toBe(ct.getAt(1));
            expect(indicator.getActiveIndex()).toBe(1);
            expect(indicators[0].hasCls('x-indicator-active')).toBe(false);
            expect(indicators[1].hasCls('x-indicator-active')).toBe(true);
            expect(indicators[2].hasCls('x-indicator-active')).toBe(false);

            layout.next();

            expect(ct.getActiveItem()).toBe(ct.getAt(2));
            expect(indicator.getActiveIndex()).toBe(2);
            expect(indicators[0].hasCls('x-indicator-active')).toBe(false);
            expect(indicators[1].hasCls('x-indicator-active')).toBe(false);
            expect(indicators[2].hasCls('x-indicator-active')).toBe(true);

            //shouldn't go next, on last item
            layout.next();

            expect(ct.getActiveItem()).toBe(ct.getAt(2));
            expect(indicator.getActiveIndex()).toBe(2);
            expect(indicators[0].hasCls('x-indicator-active')).toBe(false);
            expect(indicators[1].hasCls('x-indicator-active')).toBe(false);
            expect(indicators[2].hasCls('x-indicator-active')).toBe(true);
        });
    });

    describe('previous', function () {
        it('should set the previous item active', function () {
            ct = Ext.create({
                xtype: 'container',
                layout: 'card',
                items: [
                    {},
                    {}
                ]
            });

            ct.setActiveItem(1);

            var layout = ct.getLayout();

            expect(ct.getActiveItem()).toBe(ct.getAt(1));

            layout.previous();

            expect(ct.getActiveItem()).toBe(ct.getAt(0));
        });

        it('should not try to go previous if on first item', function () {
            ct = Ext.create({
                xtype: 'container',
                layout: 'card',
                items: [
                    {},
                    {}
                ]
            });

            var layout = ct.getLayout();

            expect(ct.getActiveItem()).toBe(ct.getAt(0));

            layout.previous();

            expect(ct.getActiveItem()).toBe(ct.getAt(0));
        });

        it('should keep the indicator in sync', function () {
            ct = Ext.create({
                xtype: 'container',
                layout: 'card',
                items: [
                    {},
                    {},
                    {}
                ]
            });

            ct.setActiveItem(2);

            var layout = ct.getLayout(),
                indicator = layout.getIndicator(),
                indicators = indicator.indicators;

            expect(ct.getActiveItem()).toBe(ct.getAt(2));
            expect(indicator.getActiveIndex()).toBe(2);
            expect(indicators[0].hasCls('x-indicator-active')).toBe(false);
            expect(indicators[1].hasCls('x-indicator-active')).toBe(false);
            expect(indicators[2].hasCls('x-indicator-active')).toBe(true);

            layout.previous();

            expect(ct.getActiveItem()).toBe(ct.getAt(1));
            expect(indicator.getActiveIndex()).toBe(1);
            expect(indicators[0].hasCls('x-indicator-active')).toBe(false);
            expect(indicators[1].hasCls('x-indicator-active')).toBe(true);
            expect(indicators[2].hasCls('x-indicator-active')).toBe(false);

            layout.previous();

            expect(ct.getActiveItem()).toBe(ct.getAt(0));
            expect(indicator.getActiveIndex()).toBe(0);
            expect(indicators[0].hasCls('x-indicator-active')).toBe(true);
            expect(indicators[1].hasCls('x-indicator-active')).toBe(false);
            expect(indicators[2].hasCls('x-indicator-active')).toBe(false);

            //shouldn't go next, on first item
            layout.previous();

            expect(ct.getActiveItem()).toBe(ct.getAt(0));
            expect(indicator.getActiveIndex()).toBe(0);
            expect(indicators[0].hasCls('x-indicator-active')).toBe(true);
            expect(indicators[1].hasCls('x-indicator-active')).toBe(false);
            expect(indicators[2].hasCls('x-indicator-active')).toBe(false);
        });
    });
});
