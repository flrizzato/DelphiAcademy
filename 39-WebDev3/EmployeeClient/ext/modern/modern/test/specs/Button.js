topSuite("Ext.Button", [
        'Ext.menu.Menu',
        'Ext.ActionSheet' ], function() {
    var button;
    
    function createButton(config) {
        config = Ext.apply({
        }, config);
        
        button = new Ext.Button(config);
    }
    
    function makeButton(config) {
        config = Ext.apply({
            renderTo: document.body
        }, config);
        
        createButton(config);
    }

    afterEach(function() {
        button = Ext.destroy(button);
    });

    describe("pressed", function() {
        function createRenderButton(config) {
            Ext.apply(config, {
                renderTo: Ext.getBody(),
                text: 'Foo'
            });
            createButton(config);
        }

        describe("configuration", function() {
            describe("pressed state", function() {
                it("should not be pressed by default", function() {
                    createRenderButton();
                    expect(button.isPressed()).toBe(false);
                });

                it("should not be pressed with pressed: false", function() {
                    createRenderButton({
                        pressed: false
                    });
                    expect(button.isPressed()).toBe(false);
                });

                it("should be pressed with pressed: true", function() {
                    createRenderButton({
                        pressed: true
                    });
                    expect(button.isPressed()).toBe(true);
                });
            });

            describe("pressedCls", function() {
                it("should not have the pressedCls by default", function() {
                    createRenderButton();
                    expect(button.element).not.toHaveCls(button.pressedCls);
                });

                it("should not have the pressedCls with pressed: false", function() {
                    createRenderButton({
                        pressed: false
                    });
                    expect(button.element).not.toHaveCls(button.pressedCls);
                });

                it("should have the pressedCls with pressed: true", function() {
                    createRenderButton({
                        pressed: true
                    });
                    expect(button.element).toHaveCls(button.pressedCls);
                });

                it("should accept a custom pressedCls", function() {
                    createRenderButton({
                        pressed: true,
                        pressedCls: 'foo'
                    });
                    expect(button.element).toHaveCls('foo');
                });
            });

            describe("events", function() {
                it("should not fire events with pressed: false", function() {
                    var spy = jasmine.createSpy();
                    createRenderButton({
                        pressed: false,
                        listeners: {
                            beforepressedchange: spy,
                            pressedchange: spy
                        }
                    });
                    expect(spy).not.toHaveBeenCalled();
                });

                it("should not fire events with pressed: true", function() {
                    var spy = jasmine.createSpy();
                    createRenderButton({
                        pressed: true,
                        listeners: {
                            beforepressedchange: spy,
                            pressedchange: spy
                        }
                    });
                    expect(spy).not.toHaveBeenCalled();
                });
            }); 
        });

        describe("dynamic", function() {
            describe("setPressed", function() {
                describe("when pressed: false", function() {
                    describe("setPressed(false)", function() {
                        it("should leave the pressed state as false", function() {
                            createRenderButton({
                                pressed: false
                            });
                            button.setPressed(false);
                            expect(button.isPressed()).toBe(false);
                        });

                        it("should not add the pressedCls", function() {
                            createRenderButton({
                                pressed: false
                            });
                            button.setPressed(false);
                            expect(button.element).not.toHaveCls(button.pressedCls);
                        });

                        describe("events", function() {
                            it("should not fire events", function() {
                                var spy = jasmine.createSpy();
                                createRenderButton({
                                    pressed: false,
                                    listeners: {
                                        beforepressedchange: spy,
                                        pressedchange: spy
                                    }
                                });
                                button.setPressed(false);
                                expect(spy).not.toHaveBeenCalled();
                            });
                        });
                    });

                    describe("setPressed(true)", function() {
                        it("should set the pressed state to true", function() {
                            createRenderButton({
                                pressed: false
                            });
                            button.setPressed(true);
                            expect(button.isPressed()).toBe(true);
                        });

                        it("should add the pressedCls", function() {
                            createRenderButton({
                                pressed: false
                            });
                            button.setPressed(true);
                            expect(button.element).toHaveCls(button.pressedCls);
                        });

                        describe("events", function() {
                            it("should fire the beforepressedchange and pressedchange events, in that order", function() {
                                var order = [],
                                    beforeSpy = jasmine.createSpy().andCallFake(function() {
                                        order.push('beforechange');
                                    }),
                                    spy = jasmine.createSpy().andCallFake(function() {
                                        order.push('change');
                                    });

                                createRenderButton({
                                    pressed: false,
                                    listeners: {
                                        beforepressedchange: beforeSpy,
                                        pressedchange: spy
                                    }
                                });
                                button.setPressed(true);

                                expect(beforeSpy.callCount).toBe(1);
                                expect(beforeSpy.mostRecentCall.args[0]).toBe(button);
                                expect(beforeSpy.mostRecentCall.args[1]).toBe(true);
                                expect(beforeSpy.mostRecentCall.args[2]).toBe(false);

                                expect(spy.callCount).toBe(1);
                                expect(spy.mostRecentCall.args[0]).toBe(button);
                                expect(spy.mostRecentCall.args[1]).toBe(true);
                                expect(spy.mostRecentCall.args[2]).toBe(false);

                                expect(order).toEqual(['beforechange', 'change']);
                            });

                            it("should not set the pressed state if beforepressedchange returns false", function() {
                                var beforeSpy = jasmine.createSpy().andReturn(false),
                                    spy = jasmine.createSpy();

                                createRenderButton({
                                    pressed: false,
                                    listeners: {
                                        beforepressedchange: beforeSpy,
                                        pressedchange: spy
                                    }
                                });
                                button.setPressed(true);

                                expect(beforeSpy.callCount).toBe(1);
                                expect(spy).not.toHaveBeenCalled();
                                expect(button.isPressed()).toBe(false);
                            });
                        });
                    });
                });

                describe("when pressed: true", function() {
                    describe("setPressed(true)", function() {
                        it("should leave the pressed state as true", function() {
                            createRenderButton({
                                pressed: true
                            });
                            button.setPressed(true);
                            expect(button.isPressed()).toBe(true);
                        });

                        it("should not remove the pressedCls", function() {
                            createRenderButton({
                                pressed: true
                            });
                            button.setPressed(true);
                            expect(button.element).toHaveCls(button.pressedCls);
                        });

                        describe("events", function() {
                            it("should not fire events", function() {
                                var spy = jasmine.createSpy();
                                createRenderButton({
                                    pressed: true,
                                    listeners: {
                                        beforepressedchange: spy,
                                        pressedchange: spy
                                    }
                                });
                                button.setPressed(true);
                                expect(spy).not.toHaveBeenCalled();
                            });
                        });
                    });

                    describe("setPressed(false)", function() {
                        it("should set the pressed state to false", function() {
                            createRenderButton({
                                pressed: true
                            });
                            button.setPressed(false);
                            expect(button.isPressed()).toBe(false);
                        });

                        it("should remove the pressedCls", function() {
                            createRenderButton({
                                pressed: true
                            });
                            button.setPressed(false);
                            expect(button.element).not.toHaveCls(button.pressedCls);
                        });

                        describe("events", function() {
                            it("should fire the beforepressedchange and pressedchange events, in that order", function() {
                                var order = [],
                                    beforeSpy = jasmine.createSpy().andCallFake(function() {
                                        order.push('beforechange');
                                    }),
                                    spy = jasmine.createSpy().andCallFake(function() {
                                        order.push('change');
                                    });

                                createRenderButton({
                                    pressed: true,
                                    listeners: {
                                        beforepressedchange: beforeSpy,
                                        pressedchange: spy
                                    }
                                });
                                button.setPressed(false);

                                expect(beforeSpy.callCount).toBe(1);
                                expect(beforeSpy.mostRecentCall.args[0]).toBe(button);
                                expect(beforeSpy.mostRecentCall.args[1]).toBe(false);
                                expect(beforeSpy.mostRecentCall.args[2]).toBe(true);

                                expect(spy.callCount).toBe(1);
                                expect(spy.mostRecentCall.args[0]).toBe(button);
                                expect(spy.mostRecentCall.args[1]).toBe(false);
                                expect(spy.mostRecentCall.args[2]).toBe(true);

                                expect(order).toEqual(['beforechange', 'change']);
                            });

                            it("should not set the pressed state if beforepressedchange returns false", function() {
                                var beforeSpy = jasmine.createSpy().andReturn(false),
                                    spy = jasmine.createSpy();

                                createRenderButton({
                                    pressed: true,
                                    listeners: {
                                        beforepressedchange: beforeSpy,
                                        pressedchange: spy
                                    }
                                });
                                button.setPressed(false);

                                expect(beforeSpy.callCount).toBe(1);
                                expect(spy).not.toHaveBeenCalled();
                                expect(button.isPressed()).toBe(true);
                            });
                        });
                    });
                });
            });

            describe("toggle", function() {
                describe("when not pressed", function() {
                    it("should set the pressed state to true", function() {
                        createRenderButton({
                            pressed: false
                        });
                        button.toggle();
                        expect(button.isPressed()).toBe(true);
                    });

                    it("should add the pressedCls", function() {
                        createRenderButton({
                            pressed: false
                        });
                        button.toggle();
                        expect(button.element).toHaveCls(button.pressedCls);
                    });

                    describe('toggleHandler', function () {
                        it('should not execute toggleHandler on instantiation', function () {
                            var spy = jasmine.createSpy();

                            createRenderButton({
                                pressed: true,
                                toggleHandler: spy
                            });

                            expect(spy).not.toHaveBeenCalled();
                        });

                        it('should execute toggleHandler on toggle when pressed', function () {
                            var spy = jasmine.createSpy();

                            createRenderButton({
                                pressed: true,
                                toggleHandler: spy
                            });

                            expect(spy).not.toHaveBeenCalled();

                            button.toggle();

                            expect(spy).toHaveBeenCalled();
                        });

                        it('should execute toggleHandler on toggle when not pressed', function () {
                            var spy = jasmine.createSpy();

                            createRenderButton({
                                toggleHandler: spy
                            });

                            button.toggle();

                            expect(spy).toHaveBeenCalled();
                        });
                    });

                    describe("events", function() {
                        it("should fire the beforepressedchange and pressedchange events, in that order", function() {
                            var order = [],
                                beforeSpy = jasmine.createSpy().andCallFake(function() {
                                    order.push('beforechange');
                                }),
                                spy = jasmine.createSpy().andCallFake(function() {
                                    order.push('change');
                                });

                            createRenderButton({
                                pressed: false,
                                listeners: {
                                    beforepressedchange: beforeSpy,
                                    pressedchange: spy
                                }
                            });
                            button.toggle();

                            expect(beforeSpy.callCount).toBe(1);
                            expect(beforeSpy.mostRecentCall.args[0]).toBe(button);
                            expect(beforeSpy.mostRecentCall.args[1]).toBe(true);
                            expect(beforeSpy.mostRecentCall.args[2]).toBe(false);

                            expect(spy.callCount).toBe(1);
                            expect(spy.mostRecentCall.args[0]).toBe(button);
                            expect(spy.mostRecentCall.args[1]).toBe(true);
                            expect(spy.mostRecentCall.args[2]).toBe(false);

                            expect(order).toEqual(['beforechange', 'change']);
                        });

                        it("should not set the pressed state if beforepressedchange returns false", function() {
                            var beforeSpy = jasmine.createSpy().andReturn(false),
                                spy = jasmine.createSpy();

                            createRenderButton({
                                pressed: false,
                                listeners: {
                                    beforepressedchange: beforeSpy,
                                    pressedchange: spy
                                }
                            });
                            button.toggle();

                            expect(beforeSpy.callCount).toBe(1);
                            expect(spy).not.toHaveBeenCalled();
                            expect(button.isPressed()).toBe(false);
                        });
                    });
                });

                describe("when pressed", function() {
                    it("should set the pressed state to false", function() {
                        createRenderButton({
                            pressed: true
                        });
                        button.toggle();
                        expect(button.isPressed()).toBe(false);
                    });

                    it("should remove the pressedCls", function() {
                        createRenderButton({
                            pressed: true
                        });
                        button.toggle();
                        expect(button.element).not.toHaveCls(button.pressedCls);
                    });

                    describe("events", function() {
                        it("should fire the beforepressedchange and pressedchange events, in that order", function() {
                            var order = [],
                                beforeSpy = jasmine.createSpy().andCallFake(function() {
                                    order.push('beforechange');
                                }),
                                spy = jasmine.createSpy().andCallFake(function() {
                                    order.push('change');
                                });

                            createRenderButton({
                                pressed: true,
                                listeners: {
                                    beforepressedchange: beforeSpy,
                                    pressedchange: spy
                                }
                            });
                            button.toggle();

                            expect(beforeSpy.callCount).toBe(1);
                            expect(beforeSpy.mostRecentCall.args[0]).toBe(button);
                            expect(beforeSpy.mostRecentCall.args[1]).toBe(false);
                            expect(beforeSpy.mostRecentCall.args[2]).toBe(true);

                            expect(spy.callCount).toBe(1);
                            expect(spy.mostRecentCall.args[0]).toBe(button);
                            expect(spy.mostRecentCall.args[1]).toBe(false);
                            expect(spy.mostRecentCall.args[2]).toBe(true);

                            expect(order).toEqual(['beforechange', 'change']);
                        });

                        it("should not set the pressed state if beforepressedchange returns false", function() {
                            var beforeSpy = jasmine.createSpy().andReturn(false),
                                spy = jasmine.createSpy();

                            createRenderButton({
                                pressed: true,
                                listeners: {
                                    beforepressedchange: beforeSpy,
                                    pressedchange: spy
                                }
                            });
                            button.toggle();

                            expect(beforeSpy.callCount).toBe(1);
                            expect(spy).not.toHaveBeenCalled();
                            expect(button.isPressed()).toBe(true);
                        });
                    });
                });
            });

            describe("user interaction", function() {
                function clickIt() {
                    // Ideally this would fire using events, however for now it's
                    // difficult to simulate across devices
                    button.onTap();
                }

                describe("with enableToggle: false", function() {
                    it("should not set the pressed state on tap", function() {
                        createRenderButton({
                            enableToggle: false
                        });
                        clickIt();
                        expect(button.getPressed()).toBe(false);
                    });

                    describe("events", function() {
                        it("should not fire events", function() {
                            var spy = jasmine.createSpy();

                            createRenderButton({
                                enableToggle: false,
                                listeners: {
                                    beforepressedchange: spy,
                                    pressedchange: spy
                                }
                            });
                            clickIt();
                            expect(spy).not.toHaveBeenCalled();
                        });
                    });
                });

                describe("with enableToggle: true", function() {
                    function createToggleRenderButton(config) {
                        Ext.apply(config, {
                            enableToggle: true
                        });
                        createRenderButton(config);
                    }

                    describe("when not pressed", function() {
                        describe("with allowDepress: false", function() {
                            it("should set the pressed state to true", function() {
                                createToggleRenderButton({
                                    pressed: false,
                                    allowDepress: false
                                });
                                clickIt();
                                expect(button.isPressed()).toBe(true);
                            });

                            it("should add the pressedCls", function() {
                                createToggleRenderButton({
                                    pressed: false,
                                    allowDepress: false
                                });
                                clickIt();
                                expect(button.element).toHaveCls(button.pressedCls);
                            });

                            describe("events", function() {
                                it("should fire the beforepressedchange and pressedchange events, in that order", function() {
                                    var order = [],
                                        beforeSpy = jasmine.createSpy().andCallFake(function() {
                                            order.push('beforechange');
                                        }),
                                        spy = jasmine.createSpy().andCallFake(function() {
                                            order.push('change');
                                        });

                                    createToggleRenderButton({
                                        pressed: false,
                                        allowDepress: false,
                                        listeners: {
                                            beforepressedchange: beforeSpy,
                                            pressedchange: spy
                                        }
                                    });
                                    clickIt();

                                    expect(beforeSpy.callCount).toBe(1);
                                    expect(beforeSpy.mostRecentCall.args[0]).toBe(button);
                                    expect(beforeSpy.mostRecentCall.args[1]).toBe(true);
                                    expect(beforeSpy.mostRecentCall.args[2]).toBe(false);

                                    expect(spy.callCount).toBe(1);
                                    expect(spy.mostRecentCall.args[0]).toBe(button);
                                    expect(spy.mostRecentCall.args[1]).toBe(true);
                                    expect(spy.mostRecentCall.args[2]).toBe(false);

                                    expect(order).toEqual(['beforechange', 'change']);
                                });

                                it("should not set the pressed state if beforepressedchange returns false", function() {
                                    var beforeSpy = jasmine.createSpy().andReturn(false),
                                        spy = jasmine.createSpy();

                                    createToggleRenderButton({
                                        pressed: false,
                                        allowDepress: false,
                                        listeners: {
                                            beforepressedchange: beforeSpy,
                                            pressedchange: spy
                                        }
                                    });
                                    clickIt();

                                    expect(beforeSpy.callCount).toBe(1);
                                    expect(spy).not.toHaveBeenCalled();
                                    expect(button.isPressed()).toBe(false);
                                });
                            });
                        });

                        describe("with allowDepress: true", function() {
                            it("should set the pressed state to true", function() {
                                createToggleRenderButton({
                                    pressed: false,
                                    allowDepress: true
                                });
                                clickIt();
                                expect(button.isPressed()).toBe(true);
                            });

                            it("should add the pressedCls", function() {
                                createToggleRenderButton({
                                    pressed: false,
                                    allowDepress: true
                                });
                                clickIt();
                                expect(button.element).toHaveCls(button.pressedCls);
                            });

                            describe("events", function() {
                                it("should fire the beforepressedchange and pressedchange events, in that order", function() {
                                    var order = [],
                                        beforeSpy = jasmine.createSpy().andCallFake(function() {
                                            order.push('beforechange');
                                        }),
                                        spy = jasmine.createSpy().andCallFake(function() {
                                            order.push('change');
                                        });

                                    createToggleRenderButton({
                                        pressed: false,
                                        allowDepress: true,
                                        listeners: {
                                            beforepressedchange: beforeSpy,
                                            pressedchange: spy
                                        }
                                    });
                                    clickIt();

                                    expect(beforeSpy.callCount).toBe(1);
                                    expect(beforeSpy.mostRecentCall.args[0]).toBe(button);
                                    expect(beforeSpy.mostRecentCall.args[1]).toBe(true);
                                    expect(beforeSpy.mostRecentCall.args[2]).toBe(false);

                                    expect(spy.callCount).toBe(1);
                                    expect(spy.mostRecentCall.args[0]).toBe(button);
                                    expect(spy.mostRecentCall.args[1]).toBe(true);
                                    expect(spy.mostRecentCall.args[2]).toBe(false);

                                    expect(order).toEqual(['beforechange', 'change']);
                                });

                                it("should not set the pressed state if beforepressedchange returns false", function() {
                                    var beforeSpy = jasmine.createSpy().andReturn(false),
                                        spy = jasmine.createSpy();

                                    createToggleRenderButton({
                                        pressed: false,
                                        allowDepress: true,
                                        listeners: {
                                            beforepressedchange: beforeSpy,
                                            pressedchange: spy
                                        }
                                    });
                                    clickIt();

                                    expect(beforeSpy.callCount).toBe(1);
                                    expect(spy).not.toHaveBeenCalled();
                                    expect(button.isPressed()).toBe(false);
                                });
                            });
                        });
                    });

                    describe("when pressed", function() {
                        describe("with allowDepress: false", function() {
                            it("should not alter the pressed state", function() {
                                createToggleRenderButton({
                                    pressed: true,
                                    allowDepress: false
                                });
                                clickIt();
                                expect(button.isPressed()).toBe(true);
                            });

                            it("should leave the pressedCls", function() {
                                createToggleRenderButton({
                                    pressed: true,
                                    allowDepress: false
                                });
                                clickIt();
                                expect(button.element).toHaveCls(button.pressedCls);
                            });

                            describe("events", function() {
                                it("should not fire events", function() {
                                    var spy = jasmine.createSpy();

                                    createToggleRenderButton({
                                        pressed: true,
                                        allowDepress: false,
                                        listeners: {
                                            beforepressedchange: spy,
                                            pressedchange: spy
                                        }
                                    });
                                    clickIt();

                                    expect(spy).not.toHaveBeenCalled();
                                });
                            });
                        });

                        describe("with allowDepress: true", function() {
                            it("should set the pressed state to false", function() {
                                createToggleRenderButton({
                                    pressed: true,
                                    allowDepress: true
                                });
                                clickIt();
                                expect(button.isPressed()).toBe(false);
                            });

                            it("should remove the pressedCls", function() {
                                createToggleRenderButton({
                                    pressed: true,
                                    allowDepress: true
                                });
                                clickIt();
                                expect(button.element).not.toHaveCls(button.pressedCls);
                            });

                            describe("events", function() {
                                it("should fire the beforepressedchange and pressedchange events, in that order", function() {
                                    var order = [],
                                        beforeSpy = jasmine.createSpy().andCallFake(function() {
                                            order.push('beforechange');
                                        }),
                                        spy = jasmine.createSpy().andCallFake(function() {
                                            order.push('change');
                                        });

                                    createToggleRenderButton({
                                        pressed: true,
                                        allowDepress: true,
                                        listeners: {
                                            beforepressedchange: beforeSpy,
                                            pressedchange: spy
                                        }
                                    });
                                    clickIt();

                                    expect(beforeSpy.callCount).toBe(1);
                                    expect(beforeSpy.mostRecentCall.args[0]).toBe(button);
                                    expect(beforeSpy.mostRecentCall.args[1]).toBe(false);
                                    expect(beforeSpy.mostRecentCall.args[2]).toBe(true);

                                    expect(spy.callCount).toBe(1);
                                    expect(spy.mostRecentCall.args[0]).toBe(button);
                                    expect(spy.mostRecentCall.args[1]).toBe(false);
                                    expect(spy.mostRecentCall.args[2]).toBe(true);

                                    expect(order).toEqual(['beforechange', 'change']);
                                });

                                it("should not set the pressed state if beforepressedchange returns false", function() {
                                    var beforeSpy = jasmine.createSpy().andReturn(false),
                                        spy = jasmine.createSpy();

                                    createToggleRenderButton({
                                        pressed: true,
                                        allowDepress: true,
                                        listeners: {
                                            beforepressedchange: beforeSpy,
                                            pressedchange: spy
                                        }
                                    });
                                    clickIt();

                                    expect(beforeSpy.callCount).toBe(1);
                                    expect(spy).not.toHaveBeenCalled();
                                    expect(button.isPressed()).toBe(true);
                                });
                            });
                        });
                    });
                });
            });
        });
    });
    
    describe("configurations", function() {
        describe("autoHandler", function() {
            describe("configuration", function() {

                it("should set the autoHandler configuration", function() {
                    createButton({autoEvent: 'test'});

                    expect(button.getAutoEvent()).not.toBeNull();
                });

                it("should set a handler", function() {
                    createButton({autoEvent: 'test'});

                    expect(button.getHandler()).not.toBeNull();
                });

                it("should set a scope", function() {
                    createButton({autoEvent: 'test'});

                    expect(button.getScope()).not.toBeNull();
                });

                describe("transforming", function() {
                    it("should transform a string into an object", function() {
                        createButton({autoEvent: 'test'});

                        var ae = button.getAutoEvent();

                        expect(ae).not.toBeNull();
                        expect(typeof ae).toEqual("object");
                    });

                    it("should set the name of the object", function() {
                        createButton({autoEvent: 'test'});

                        var ae = button.getAutoEvent();

                        expect(ae.name).toEqual('test');
                    });

                    it("should set the scope of the object", function() {
                        createButton({autoEvent: 'test'});

                        var ae = button.getAutoEvent();

                        expect(ae.scope).not.toBeNull();
                    });
                });
            });

            describe("method", function() {
                it("should set the autoHandler configuration", function() {
                    createButton();
                    button.setAutoEvent('test');

                    expect(button.getAutoEvent()).not.toBeNull();
                });

                it("should set a handler", function() {
                    createButton();
                    button.setAutoEvent('test');

                    expect(button.getHandler()).not.toBeNull();
                });

                it("should set a scope", function() {
                    createButton();
                    button.setAutoEvent('test');

                    expect(button.getScope()).not.toBeNull();
                });

                describe("transforming", function() {
                    it("should transform a string into an object", function() {
                        createButton();
                        button.setAutoEvent('test');

                        var ae = button.getAutoEvent();

                        expect(ae).not.toBeNull();
                        expect(typeof ae).toEqual("object");
                    });

                    it("should set the name of the object", function() {
                        createButton();
                        button.setAutoEvent('test');

                        var ae = button.getAutoEvent();

                        expect(ae.name).toEqual('test');
                    });

                    it("should set the scope of the object", function() {
                        createButton();
                        button.setAutoEvent('test');

                        var ae = button.getAutoEvent();

                        expect(ae.scope).not.toBeNull();
                    });
                });
            });
        });

        describe("badgeText", function() {
            describe("configuration", function() {
                beforeEach(function() {
                    createButton({badgeText: 'test'});
                });

                it("should set the badgeText", function() {
                    expect(button.getBadgeText()).toEqual('test');
                });

                describe("after render", function() {
                    beforeEach(function() {
                        button.render(Ext.getBody());
                    });
                    it("should create a badgeEl", function() {
                        expect(button.badgeElement).not.toBeNull();
                    });
                    it("should have the badgeText value in the badgeEl", function() {
                        expect(button.badgeElement.dom.innerHTML).toEqual('test');
                    });
                });
            });

            describe("methods", function() {
                beforeEach(function() {
                    createButton();
                });

                describe("after render", function() {
                    beforeEach(function() {
                        button.render(Ext.getBody());

                        button.setBadgeText('test');
                    });

                    it("should set the badgeText", function() {
                         expect(button.getBadgeText()).toEqual('test');
                    });

                    it("should create a badgeEl", function() {
                        expect(button.badgeElement).not.toBeNull();
                    });

                    describe("when removing badgeText", function() {
                        beforeEach(function() {
                            button.setBadgeText(null);
                        });

                        it("should remove the badgeText configuration", function() {

                            expect(button.getBadgeText()).toBeNull();
                        });
                    });

                    it("should have the badgeText value in the badgeEl", function() {
                        expect(button.badgeElement.dom.innerHTML).toEqual('test');
                    });
                });
            });
        });

        describe("text", function() {
            describe("configuration", function() {
                beforeEach(function() {
                    createButton({
                        text: 'test'
                    });
                });

                it("should set the text", function() {
                    expect(button.getText()).toEqual('test');
                });

                describe("after render", function() {
                    beforeEach(function() {
                        button.render(Ext.getBody());
                    });

                    it("should create a textEl", function() {
                        expect(button.textElement).not.toBeNull();
                    });
                });
            });

            describe("configuration using html as text", function() {
                beforeEach(function() {
                    createButton({
                        html: '<u>test</u>'
                    });
                });

                it("should set the html", function() {
                    expect(button.textElement.dom.innerHTML.toLowerCase()).toEqual('<u>test</u>');
                });
            });

            describe("methods", function() {
                beforeEach(function() {
                    createButton();
                });

                it("should set the text", function() {
                    button.setText('test');
                    expect(button.getText()).toEqual('test');
                });

                describe("after render", function() {

                    it("should create a textEl", function() {
                        expect(button.textElement).not.toBeNull();
                    });

                    describe("when removing text", function() {
                        beforeEach(function() {
                            button.setText(null);
                        });

                        it("should remove the text configuration", function() {
                            expect(button.getText()).toBeNull();
                        });
                    });
                });
            });
        });

        describe("icon", function() {
            describe("configuration", function() {
                beforeEach(function() {
                    createButton({
                        icon: 'resources/images/test.gif'
                    });
                });

                it("should set the icon", function() {
                    expect(button.getIcon()).toEqual('resources/images/test.gif');
                });

                describe("after render", function() {
                    beforeEach(function() {
                        button.render(Ext.getBody());
                    });

                    it("should create a iconEl", function() {
                        expect(button.iconElement).not.toBeNull();
                    });
                });
            });

            describe("methods", function() {
                beforeEach(function() {
                    createButton();
                });
                
                it("should set the icon", function() {
                    button.setIcon('resources/images/another.gif');
                    expect(button.getIcon()).toEqual('resources/images/another.gif');
                });

                describe("after render", function() {
                    beforeEach(function() {
                        button.render(Ext.getBody());
                    });

                    it("should create a iconEl", function() {
                        expect(button.iconElement).not.toBeNull();
                    });

                    describe("when remove the icon", function() {
                        beforeEach(function() {
                            button.setIcon(null);
                        });

                        it("should remove the icon configuration", function() {
                            expect(button.getIcon()).toBeNull();
                        });
                    });

                    it("should have the new background-image on the iconEl", function() {
                        button.setIcon('resources/images/another.gif');

                        expect(button.iconElement.getStyle('background-image')).toMatch('another');
                    });

                    it("should remove any old cls on the iconEl", function() {
                        button.setIcon('resources/images/another.gif');

                        expect(button.iconElement.getStyle('background-image')).toMatch('another');

                        button.setIcon('resources/images/new.gif');

                        expect(button.iconElement.getStyle('background-image')).not.toMatch('another');
                        expect(button.iconElement.getStyle('background-image')).toMatch('new');
                    });
                });
            });
        });

        describe("iconCls", function() {
            describe("configuration", function() {
                beforeEach(function() {
                    createButton({
                        iconCls: 'test'
                    });
                });

                it("should set the iconCls", function() {
                    expect(button.getIconCls()).toEqual('test');
                });

                describe("after render", function() {
                    beforeEach(function() {
                        button.render(Ext.getBody());
                    });

                    it("should insert the iconEl", function() {
                        expect(button.iconElement.parentNode).not.toBeNull();
                    });
                });
            });

            describe("methods", function() {
                beforeEach(function() {
                    createButton();
                });

                it("should set the iconCls", function() {
                    button.setIconCls('test');
                    expect(button.getIconCls()).toEqual('test');
                });

                  it("should create an iconEl", function() {
                    expect(button.iconElement).not.toBeNull();
                });

                describe("after render", function() {
                    beforeEach(function() {
                        button.render(Ext.getBody());
                        button.setIconCls('test');
                    });

                    describe("when removing iconCls", function() {
                        beforeEach(function() {
                            button.setIconCls(null);
                        });

                        it("should remove the iconCls configuration", function() {
                            expect(button.getIconCls()).toBeNull();
                        });

                        it("should remove the iconCls", function() {
                            expect(button.element.hasCls('test')).toBeFalsy();
                        });
                    });

                    it("should have the new cls on the iconEl", function() {
                        button.setIconCls('another');

                        expect(button.iconElement.hasCls('another')).toBeTruthy();
                    });

                    it("should remove any old cls on the iconEl", function() {
                        button.setIconCls('another');

                        expect(button.iconElement.hasCls('another')).toBeTruthy();

                        button.setIconCls('new');

                        expect(button.iconElement.hasCls('another')).toBeFalsy();
                        expect(button.iconElement.hasCls('new')).toBeTruthy();
                    });
                });
            });
        });

        describe("iconAlign", function() {
            var value = 'right',
                cls   = Ext.baseCSSPrefix + 'icon-align-' + value;

            describe("with icon", function() {
                describe("configuration", function() {
                    beforeEach(function() {
                        createButton({
                            iconAlign: value,
                            icon: 'resources/images/test.gif',
                            text: 'test'
                        });
                    });

                    it("should set the iconAlign", function() {
                        expect(button.getIconAlign()).toEqual(value);
                    });

                    describe("after render", function() {
                        beforeEach(function() {
                            button.render(Ext.getBody());
                        });

                        it("should add the iconAlign class", function() {
                            expect(button.element.hasCls(cls)).toBeTruthy();
                        });
                    });
                });

                describe("methods", function() {
                    beforeEach(function() {
                        createButton({
                            icon: 'resources/images/test.gif',
                            text: 'test'
                        });
                    });

                    it("should set the iconAlign", function() {
                        button.setIconAlign(value);
                        expect(button.getIconAlign()).toEqual(value);
                    });

                    describe("after render", function() {
                        beforeEach(function() {
                            button.render(Ext.getBody());

                            button.setIconAlign(value);
                        });

                        it("should add the iconAlign cls", function() {
                            expect(button.element.hasCls(cls)).toBeTruthy();
                        });

                        describe("when removing iconAlign", function() {
                            beforeEach(function() {
                                button.setIconAlign(null);
                            });

                            it("should remove the iconAlign configuration", function() {
                                expect(button.getIconAlign()).toBeNull();
                            });

                            it("should remove the iconAlign cls", function() {
                                expect(button.element.hasCls(cls)).not.toBeTruthy();
                            });
                        });
                    });
                });
            });

            describe("without icon", function() {
                describe("configuration", function() {
                    beforeEach(function() {
                        createButton({iconAlign: 'right'});
                    });

                    describe("after render", function() {
                        beforeEach(function() {
                            button.render(Ext.getBody());
                        });

                        it("should add the iconAlign cls", function() {
                            expect(button.element).toHaveCls(cls);
                        });
                    });
                });

                describe("methods", function() {
                    beforeEach(function() {
                        createButton();
                    });

                    describe("after render", function() {
                        beforeEach(function() {
                            button.render(Ext.getBody());
                            button.setIconAlign(value);
                        });

                        it("should add the iconAlign cls", function() {
                            expect(button.element).toHaveCls(cls);
                        });

                        describe("when adding icon", function() {
                            beforeEach(function() {
                                button.setText('another');
                                button.setIcon('another');
                            });

                            it("should add the iconAlign configuration", function() {
                                expect(button.getIconAlign()).toEqual(value);
                            });

                            it("should add the iconAlign cls", function() {
                                expect(button.element.hasCls(cls)).toBeTruthy();
                            });
                        });
                    });
                });
            });
        });
        
        describe("arrowAlign", function() {
            describe("without menu", function() {
                it("should have arrow-align-right cls", function() {
                    makeButton();
                    
                    expect(button.element).toHaveCls('x-arrow-align-right');
                    expect(button.element).not.toHaveCls('x-arrow-align-bottom');
                });
                
                it("should have arrow-align-bottom cls", function() {
                    makeButton({
                        arrowAlign: 'bottom'
                    });
                    
                    expect(button.element).not.toHaveCls('x-arrow-align-right');
                    expect(button.element).toHaveCls('x-arrow-align-bottom');
                });
                
                it("should allow setting arrowAlign: 'right'", function() {
                    makeButton({
                        arrowAlign: 'down'
                    });
                    
                    button.setArrowAlign('right');
                    
                    expect(button.element).toHaveCls('x-arrow-align-right');
                    expect(button.element).not.toHaveCls('x-arrow-align-bottom');
                });
                
                it("should allow setting arrowAlign: 'bottom'", function() {
                    makeButton();
                    
                    button.setArrowAlign('bottom');
                    
                    expect(button.element).toHaveCls('x-arrow-align-bottom');
                    expect(button.element).not.toHaveCls('x-arrow-align-right');
                });
            });
            
            describe("with menu", function() {
                it("should have arrow-align-right cls", function() {
                    makeButton({
                        menu: {}
                    });
                    
                    expect(button.element).toHaveCls('x-arrow-align-right');
                    expect(button.element).not.toHaveCls('x-arrow-align-bottom');
                });
                
                it("should have arrow-align-bottom cls", function() {
                    makeButton({
                        arrowAlign: 'bottom',
                        menu: {}
                    });
                    
                    expect(button.element).toHaveCls('x-arrow-align-bottom');
                    expect(button.element).not.toHaveCls('x-arrow-align-right');
                });
                
                it("should allow setting arrowAlign: 'right'", function() {
                    makeButton({
                        arrowAlign: 'down',
                        menu: {}
                    });
                    
                    button.setArrowAlign('right');
                    
                    expect(button.element).toHaveCls('x-arrow-align-right');
                    expect(button.element).not.toHaveCls('x-arrow-align-bottom');
                });
                
                it("should allow setting arrowAlign: 'bottom'", function() {
                    makeButton({
                        menu: {}
                    });
                    
                    button.setArrowAlign('bottom');
                    
                    expect(button.element).toHaveCls('x-arrow-align-bottom');
                    expect(button.element).not.toHaveCls('x-arrow-align-right');
                });
            });
        });
        
        describe("menu", function() {
            beforeEach(function() {
                Ext.Viewport = new Ext.viewport.Default();
            });

            afterEach(function() {
                //we need to destroy the menu before Ext.Viewport is
                button.setMenu(null);

                Ext.Viewport.destroy();
                Ext.Viewport = null;
            });

            it("should accept menu config object", function() {
                makeButton({
                    menu: {
                        id: 'bar',
                        items: [{ text: 'foo' }]
                    }
                });

                expect(button.getMenu().id).toBe('bar');
                expect(button.arrowElement.isVisible()).toBe(true);
            });

            it("should accept menu config object and create the specified xtype", function() {
                makeButton({
                    menu: {
                        id: 'bar',
                        xtype: 'actionsheet',
                        items: [{ text: 'foo' }]
                    }
                });

                expect(button.getMenu().$className).toBe('Ext.ActionSheet');
            });

            it("should accept array of items", function() {
                makeButton({
                    menu: [{
                        text: 'foo'
                    }, {
                        text: 'bar'
                    }]
                });
                
                expect(button.getMenu().getItems().getCount()).toBe(2);
            });

            it('should honour arrow: false', function() {
                makeButton({
                    arrow: false,
                    menu: [{
                        text: 'foo'
                    }, {
                        text: 'bar'
                    }],
                    renderTo: document.body
                });

                expect(button.arrowElement.isVisible()).toBe(false);
            });

            it("should show menu on button click", function () {
                makeButton({
                    renderTo: document.body,
                    menu: [{
                        text: 'foo'
                    }, {
                        text: 'bar'
                    }]
                });

                button.onTap();

                expect(button.getMenu().getHidden()).toBe(false);
            });

            describe("action sheet", function () {
                it("should create an action sheet", function () {
                    makeButton({
                        menu: {
                            xtype: 'actionsheet',
                            items: [{ text: 'foo' }]
                        }
                    });

                    expect(button.getMenu().$className).toBe('Ext.ActionSheet');
                });

                it("should show the action sheet on button click", function () {
                    makeButton({
                        menu: {
                            xtype: 'actionsheet',
                            side: 'right',
                            items: [{ text: 'foo' }]
                        }
                    });

                    button.onTap();

                    var menu = button.getMenu();

                    expect(menu.getDisplayed()).toBe(true);

                    waitsFor(function () {
                        return !menu.isAnimating;
                    });

                    runs(function () {
                        expect(menu.getDisplayed()).toBe(true);
                        menu.hide();
                    });

                    waitsFor(function () {
                        return !menu.isAnimating;
                    });
                });
            });

            describe("stretchMenu", function() {
                function getMenuWidth() {
                    button.showMenu();
                    return button.getMenu().element.getWidth();
                }

                describe("configuration", function() {
                    describe("stretchMenu: false", function() {
                        it("should not stretch", function() {
                            makeButton({
                                width: 800,
                                stretchMenu: false,
                                menu: {
                                    items: [{
                                        text: 'Small'
                                    }]
                                }
                            });
                            expect(getMenuWidth()).toBeLessThan(100);
                        });

                        it("should respect a passed minWidth", function() {
                            makeButton({
                                width: 800,
                                stretchMenu: false,
                                menu: {
                                    minWidth: 200,
                                    items: [{
                                        text: 'Small'
                                    }]
                                }
                            });
                            expect(getMenuWidth()).toBe(200);
                        });
                    });

                    describe("stretchMenu: true", function() {
                        it("should stretch", function() {
                            makeButton({
                                width: 800,
                                stretchMenu: true,
                                menu: {
                                    items: [{
                                        text: 'Small'
                                    }]
                                }
                            });
                            expect(getMenuWidth()).toBe(800);
                        });

                        it("should respect a minWidth", function() {
                            makeButton({
                                width: 800,
                                stretchMenu: true,
                                menu: {
                                    minWidth: 400,
                                    items: [{
                                        text: 'Small'
                                    }]
                                }
                            });
                            expect(getMenuWidth()).toBe(400);
                        });
                    });
                });

                describe("dynamic", function() {
                    describe("from true -> false", function() {
                        it("should not stretch the menu", function() {
                            makeButton({
                                width: 800,
                                stretchMenu: true,
                                menu: {
                                    items: [{
                                        text: 'Small'
                                    }]
                                }
                            });
                            button.showMenu();
                            button.getMenu().hide();
                            button.setStretchMenu(false);
                            expect(getMenuWidth()).toBeLessThan(100);
                        });

                        it("should respect a minWidth", function() {
                            makeButton({
                                width: 800,
                                stretchMenu: true,
                                menu: {
                                    minWidth: 400,
                                    items: [{
                                        text: 'Small'
                                    }]
                                }
                            });
                            button.showMenu();
                            button.getMenu().hide();
                            button.setStretchMenu(false);
                            window.foo = 1;
                            expect(getMenuWidth()).toBe(400);
                            delete window.foo;
                        });
                    });

                    describe("from false -> true", function() {
                        it("should not stretch the menu", function() {
                            makeButton({
                                width: 800,
                                stretchMenu: false,
                                menu: {
                                    items: [{
                                        text: 'Small'
                                    }]
                                }
                            });
                            button.showMenu();
                            button.getMenu().hide();
                            button.setStretchMenu(true);
                            expect(getMenuWidth()).toBe(800);
                        });

                        it("should respect a minWidth", function() {
                            makeButton({
                                width: 800,
                                stretchMenu: false,
                                menu: {
                                    minWidth: 400,
                                    items: [{
                                        text: 'Small'
                                    }]
                                }
                            });
                            button.showMenu();
                            button.getMenu().hide();
                            button.setStretchMenu(true);
                            expect(getMenuWidth()).toBe(400);
                        });
                    });
                });
            });

            describe('press button', function () {
                it('should press button on menu show', function () {
                    makeButton({
                        menu: [{
                            text: 'Small'
                        }]
                    });

                    button.showMenu();

                    expect(button.getPressed()).toBeTruthy();
                });

                it('should unpress button on menu hide', function () {
                    makeButton({
                        menu: [{
                            text: 'Small'
                        }]
                    });

                    button.showMenu();

                    expect(button.getPressed()).toBeTruthy();

                    button.getMenu().hide();

                    expect(button.getPressed()).toBeFalsy();
                });

                it('should unpress button when hiding menu', function () {
                    makeButton({
                        menu: [{
                            text: 'Small'
                        }]
                    });

                    var menu = button.getMenu();

                    jasmine.fireMouseEvent(button.el, 'click');

                    waitsFor(function () {
                        return menu.isVisible();
                    });

                    runs(function () {
                        expect(button.getPressed()).toBeTruthy();

                        jasmine.fireMouseEvent(button.el, 'click');
                    });

                    waitsFor(function () {
                        return !menu.isVisible();
                    });

                    runs(function () {
                        expect(button.getPressed()).toBeFalsy();
                    });
                });

                it('should not unpress a pressed button on menu hide', function () {
                    makeButton({
                        pressed: true,
                        menu: [{
                            text: 'Small'
                        }]
                    });

                    expect(button.getPressed()).toBeTruthy();

                    button.showMenu();

                    expect(button.getPressed()).toBeTruthy();

                    button.getMenu().hide();

                    expect(button.getPressed()).toBeTruthy();
                });

                it('should not press button for viewport menu', function () {
                    makeButton({
                        menu: {
                            xtype: 'actionsheet',
                            items: [{
                                text: 'Delete draft',
                                ui: 'decline'
                            }, {
                                text: 'Save draft'
                            }, {
                                text: 'Cancel',
                                ui: 'confirm'
                            }]
                        }
                    });

                    button.showMenu();

                    expect(button.getPressed()).toBeFalsy();

                    button.getMenu().hide();

                    expect(button.getPressed()).toBeFalsy();
                });
            });
        });
    });

    describe("#onTap", function() {
        beforeEach(function() {
            createButton();
        });

        it("should return false if disabled", function() {
            button.disabled = true;

            expect(button.onTap()).toBeFalsy();
        });

        it("should call fireAction", function() {
            spyOn(button, 'fireAction');

            button.onTap();

            expect(button.fireAction).toHaveBeenCalled();
        });
    });

    describe("#doTap", function() {
        describe("no menu", function() {
            beforeEach(function() {
                createButton();
            });
    
            describe("no handler", function() {
                it("should return false", function() {
                    expect(button.doTap(button)).toBeFalsy();
                });
            });
    
            describe("with handler", function() {
                describe("string", function() {
                    it("should call the function", function() {
                        button.testFoo = function() {};
                        spyOn(button, 'testFoo');
    
                        button.setHandler('testFoo');
    
                        button.doTap(button);
    
                        expect(button.testFoo).toHaveBeenCalled();
                    });
                });
    
                describe("reference", function() {
                    it("should call the function", function() {
                        button.testFoo = function() {};
                        spyOn(button, 'testFoo');
    
                        button.setHandler(button.testFoo);
    
                        button.doTap(button);
    
                        expect(button.testFoo).toHaveBeenCalled();
                    });
                });
            });
        });
        
        describe("with menu", function() {
            var handlerSpy;
            
            beforeEach(function() {
                handlerSpy = jasmine.createSpy('button handler');
                
                makeButton({
                    menu: {},
                    handler: handlerSpy
                });
            });
            
            it("should show the menu when it's hidden", function() {
                button.doTap(button);
                
                expect(button.getMenu().isVisible()).toBe(true);
            });
            
            it("should hide the menu when it's visible", function() {
                button.showMenu();
                button.doTap(button);
                
                expect(button.getMenu().isVisible()).toBe(false);
            });

            it('should show and hide a menu', function () {
                var menu = button.getMenu();

                jasmine.fireMouseEvent(button.el, 'click');

                waitsFor(function () {
                    return menu.isVisible();
                });

                runs(function () {
                    jasmine.fireMouseEvent(button.el, 'click');
                });

                waitsFor(function () {
                    return !menu.isVisible();
                });
            });

            it("should not fire the handler", function() {
                button.doTap(button);
                
                expect(handlerSpy).not.toHaveBeenCalled();
            });
        });
    });

    describe("el", function() {
        beforeEach(function() {
            createButton();
            button.render(Ext.getBody());
        });
        
        describe("click", function() {
            it("should call onClick", function() {
                spyOn(button, 'onClick');
                
                jasmine.fireMouseEvent(button.el, 'click');
                
                expect(button.onClick).toHaveBeenCalled();
            });
        });

        (jasmine.supportsTouch ? describe : xdescribe)("touchstart + touchend sequence", function() {
            it("should call onRelease", function() {
                spyOn(button, "onRelease").andCallThrough();

                Ext.testHelper.fireEvent('start', button.el);
                expect(button.onRelease).not.toHaveBeenCalled();
                expect(button).toHaveCls(button.pressingCls);

                // Touch end anywhere in the document should release pressed state
                Ext.testHelper.fireEvent('end', document.body);
                expect(button.onRelease).toHaveBeenCalled();
                expect(button).not.toHaveCls(button.pressingCls);
            });
        });
        (jasmine.supportsTouch ? xdescribe : describe)("mousedown + mouseup sequence", function() {
            it("should call onRelease", function() {
                spyOn(button, "onRelease").andCallThrough();

                jasmine.fireMouseEvent(button.el, 'mousedown');
                expect(button.onRelease).not.toHaveBeenCalled();

                expect(button).toHaveCls(button.pressingCls);

                // Mouse up anywhere in the document should release pressed state
                jasmine.fireMouseEvent(document.body, 'mouseup');
                expect(button.onRelease).toHaveBeenCalled();
                expect(button).not.toHaveCls(button.pressingCls);
            });
        });
    });

    describe("focusing", function() {
        beforeEach(function() {
            createButton({
                text: 'blergo',
                renderTo: Ext.getBody()
            });
            
            focusAndWait(button);
        });
        
        it("should be able to focus", function() {
            expectFocused(button);
        });
        
        it("should apply focusCls", function() {
            expect(button.getFocusClsEl().hasCls(button.focusCls)).toBe(true);
        });
        
        it("should remove focusCls on blur", function() {
            button.blur();
            
            expect(button.getFocusClsEl().hasCls(button.focusCls)).toBe(false);
        });
    });
    
    describe("keyboard interaction", function() {
        describe("with menu", function() {
            var fooItem;
            
            beforeEach(function() {
                makeButton({
                    menu: [{
                        text: 'foo'
                    }, {
                        text: 'bar'
                    }]
                });
                
                fooItem = button.getMenu().getItems().getAt(0);
            });
            
            afterEach(function() {
                fooItem = null;
            });
            
            it("should react to Space key", function() {
                pressKey(button, 'space');
                expectFocused(fooItem);
            });
            
            it("should react to Enter key", function() {
                pressKey(button, 'enter');
                expectFocused(fooItem);
            });
            
            it("should react to Down arrow key", function() {
                pressKey(button, 'down');
                expectFocused(fooItem);
            });
            
            it("should react to Esc key when focus is in the menu", function() {
                pressKey(button, 'space');
                
                waitForFocus(fooItem);

                runs(function() {
                    pressKey(fooItem, 'esc');
                });
                
                waitForFocus(button);
                
                runs(function() {
                    expect(button.getMenu().isVisible()).toBe(false);
                });
            });
            
            it("should react to Esc key when focus is on the button", function() {
                button.showMenu();
                
                pressKey(button, 'esc');
                
                runs(function() {
                    expect(button.getMenu().isVisible()).toBe(false);
                });
            });
        });
    });
    
    describe("enable/disable", function() {
        describe("default enabled", function() {
            beforeEach(function() {
                makeButton({
                    text: 'blergo'
                });
            });
            
            it("should not have disabled flag on the buttonElement", function() {
                expect(button.buttonElement.dom.disabled).toBeFalsy();
            });
            
            it("should not have disabled style on the element", function() {
                expect(button.element).not.toHaveCls(button.disabledCls);
            });
        });
        
        describe("default disabled", function() {
            beforeEach(function() {
                makeButton({
                    text: 'gonzo',
                    disabled: true
                });
            });
            
            it("should have disabled flag on the buttonElement", function() {
                expect(button.buttonElement.dom.disabled).toBeTruthy();
            });
            
            it("should have disabled style on the element", function() {
                expect(button.element).toHaveCls(button.disabledCls);
            });
        });
        
        describe("disabling", function() {
            beforeEach(function() {
                makeButton({
                    text: 'throbbe'
                });
                
                button.disable();
            });
            
            it("should set disabled flag on the buttonElement", function() {
                expect(button.buttonElement.dom.disabled).toBeTruthy();
            });
            
            it("should add disabled style to the element", function() {
                expect(button.element).toHaveCls(button.disabledCls);
            });
            
            describe("re-enabling", function() {
                beforeEach(function() {
                    button.enable();
                });
                
                it("should reset disabled flag on the buttonElement", function() {
                    expect(button.el.dom.disabled).toBeFalsy();
                });
                
                it("should reset disabled style on the element", function() {
                    expect(button.element).not.toHaveCls(button.disabledCls);
                });
            });
        });
    });

    describe('type', function () {
        it('should set default type onto the buttonElement', function () {
            makeButton();

            expect(button.buttonElement.dom.type).toBe('button');
        });

        it('should set configured type', function () {
            makeButton({
                buttonType: 'submit'
            });

            expect(button.buttonElement.dom.type).toBe('submit');
        });

        it('should set type using setType', function () {
            makeButton();

            expect(button.buttonElement.dom.type).toBe('button');

            button.setButtonType('submit');

            expect(button.buttonElement.dom.type).toBe('submit');
        });
    });
});
