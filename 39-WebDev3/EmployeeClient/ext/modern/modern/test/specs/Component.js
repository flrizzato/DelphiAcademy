/* global Ext, expect */

topSuite("Ext.Component",
    ['Ext.Container', 'Ext.app.ViewModel', 'Ext.layout.HBox',
     'Ext.layout.VBox', 'Ext.Mask', 'Ext.MessageBox'],
function() {
    var component;

    function makeComponent(config) {
        component = new Ext.Component(config);
        return component;
    }

    afterEach(function() {
        component = Ext.destroy(component);
    });


    describe("userSelectable", function() {
        var userSelect,
            userSelectAuto = 'text';

        beforeAll(function() {
            var el = document.createElement('div'),
                style = el.style;

            Ext.each([
                'user-select', '-moz-user-select', '-ms-user-select', '-webkit-user-select'
            ], function (name) {
                if (style[name] !== undefined) {
                    userSelect = name;
                    return false;
                }
            });
            if (userSelect === '-moz-user-select') {
                userSelectAuto = 'auto';
            }
            else if (userSelect === '-ms-user-select') {
                userSelectAuto = 'text';
            }
        });

        it("should default userSelectable off", function() {
            makeComponent({
                renderTo: Ext.getBody()
            });
            expect(component.el.getStyle(userSelect)).toBe('none');
        });
        it("should allow userSelectable configured as a boolean on main element", function() {
            makeComponent({
                renderTo: Ext.getBody(),
                userSelectable: true
            });

            expect(component.el.getStyle(userSelect)).toBe(userSelectAuto);
        });
        it("should allow userSelectable configured as an object with element prop", function() {
            makeComponent({
                renderTo: Ext.getBody(),
                userSelectable: {
                    element: true
                }
            });
            expect(component.el.getStyle(userSelect)).toBe(userSelectAuto);
        });
        it("should allow userSelectable configured as an object with reference element prop", function() {
            component = new Ext.Container({
                referenceHolder: true,
                renderTo: Ext.getBody(),
                userSelectable: {
                    bodyElement: true
                }
            });

            expect(component.bodyElement.getStyle(userSelect)).toBe(userSelectAuto);
            expect(component.element.getStyle(userSelect)).toBe('none');

        });
        it("should pass userSelectable to child component via inheritance", function() {
            component = new Ext.Container({
                userSelectable: true,
                renderTo: Ext.getBody(),
                items: [
                    {
                        html: 'foo'
                    }
                ]
            });

            expect(component.el.getStyle(userSelect)).toBe(userSelectAuto);
        });
        it("should allow child component to override userSelectable of parent", function() {
            component = new Ext.Container({
                userSelectable: true,
                renderTo: Ext.getBody(),
                items: [
                    {
                        userSelectable: false,
                        html: 'foo'
                    }
                ]
            });

            expect(component.element.getStyle(userSelect)).toBe(userSelectAuto);
            expect(component.items.items[0].element.getStyle(userSelect)).toBe('none');

        });
    });

    describe("animation", function() {
        var oldOnError = window.onerror;

        afterEach(function() {
            window.onerror = oldOnError;
        });

        // This spec fails around 50% of the time locally in Chrome, going
        // to disable it until it can be made more stable
        xit("should allow show twice in succession while animating", function() {
            var onErrorSpy = jasmine.createSpy();
            window.onerror = onErrorSpy.andCallFake(function() {
                if (oldOnError) {
                    oldOnError();
                }
            });

            Ext.Msg.confirm('Title', 'question', Ext.emptyFn);
            waitsFor(function() {
                return !Ext.Msg.getHidden();
            }, 'MessageBox to be shown');
            runs(function() {
                Ext.Msg.hide();
                Ext.Msg.confirm('Title2', 'question2', Ext.emptyFn);
            });
            waitsFor(function() {
                return !Ext.Msg.getHidden();
            }, 'MessageBox2 to be shown');
            // this bit to cleanup modal mask - don't want to exit test with it showing
            runs(function() {
                Ext.Msg.hide();
                expect(onErrorSpy).not.toHaveBeenCalled();
                Ext.Msg.hideModalMask();
            });
        });

        it("should be visible during hide animation until hidden", function() {
            var spy = jasmine.createSpy();

            makeComponent({
                renderTo: Ext.getBody(),
                hideAnimation: {
                    type: 'fadeOut',
                    duration: 300
                },
                listeners: {
                    hide: spy
                }
            });
            component.hide();
            waits(100);
            runs(function() {
                expect(spy).not.toHaveBeenCalled();
                expect(component.isVisible()).toBe(true);
            });
            waitsFor(function() {
                return spy.callCount > 0;
            });
            runs(function() {
                expect(spy.callCount).toBe(1);
                expect(component.isVisible()).toBe(false);
            });
        });

        it("should be visible during show animation as soon as it's visible", function() {
            var spy = jasmine.createSpy();

            makeComponent({
                renderTo: Ext.getBody(),
                hidden: true,
                showAnimation: {
                    type: 'fadeIn',
                    duration: 300
                },
                listeners: {
                    show: spy
                }
            });
            component.show();
            waits(100);
            runs(function() {
                expect(spy).not.toHaveBeenCalled();
                expect(component.isVisible()).toBe(true);
            });
            waitsFor(function() {
                return spy.callCount > 0;
            });
            runs(function() {
                expect(spy.callCount).toBe(1);
                expect(component.isVisible()).toBe(true);
            });
        });
    });

    describe('configuration', function() {
        it('should not fire show/hide events during configuration', function() {
            var beforeShowCalled = false,
                showCalled = false,
                beforeHideCalled = false,
                hideCalled = false,
                InstrumentedComponent = Ext.define(null, {
                    extend: 'Ext.Component',

                    fireEvent: function(eventName) {
                        if (eventName === 'beforeshow') {
                            beforeShowCalled = true;
                        }
                        if (eventName === 'show') {
                            showCalled = true;
                        }
                        if (eventName === 'beforehide') {
                            beforeHideCalled = true;
                        }
                        if (eventName === 'hide') {
                            hideCalled = true;
                        }
                        this.callParent(arguments);
                    }
                });

            component = new InstrumentedComponent();
            Ext.destroy(component);
            // "hide" is fired during destroy
            hideCalled = false;
            component = new InstrumentedComponent({hidden: true});

            expect(beforeShowCalled).toBe(false);
            expect(showCalled).toBe(false);
            expect(beforeHideCalled).toBe(false);
            expect(hideCalled).toBe(false);
        });
    });

    describe("bind", function() {
        describe("defaultBindProperty", function() {
            it("should bind with a string", function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    viewModel: {
                        data: {
                            theHtml: 'foo'
                        }
                    },
                    bind: '{theHtml}'
                });

                // The component's defaultBindProperty is bound
                expect(component.isBound(component.defaultBindProperty)).toBe(true);

                // No arg version should check defaultBindProperty
                expect(component.isBound()).toBe(true);

                component.getViewModel().notify();
                expect(component.getInnerHtmlElement().dom.innerHTML).toBe('foo');
            });

            it("should throw an exception if we have no default bind", function() {
                expect(function() {
                    makeComponent({
                        id: 'this-should-throw',
                        defaultBindProperty: '',
                        viewModel: {
                            data: {
                                theHtml: 'foo'
                            }
                        },
                        bind: '{theHtml}'
                    });

                    // Any arbitrary name will return false as not being bound
                    expect(component.isBound('foo')).toBe(false);

                    // No defaultBindProperty - should return false
                    expect(component.isBound()).toBe(false);

                    component.getBind();
                }).toThrow();
                // The exception prevented the assoignment to the component var, but the
                // component MUST be cleaned up in the afterEach to prevent ViewModel/Scheduler
                // timer leaks, so we must collect the component into the var now.
                component = Ext.getCmp('this-should-throw');
            });
        });
    });

    describe("'cls' methods", function() {
        var spy;
        var spacesRe = /\s+/;

        function getClsList (el) {
            if (el.isWidget) {
                el = el.el;
            }

            var list = el.dom.className.split(spacesRe);

            Ext.Array.remove(list, 'x-root');
            Ext.Array.remove(list, 'x-component');

            return list;
        }

        function getClsMap (el) {
            var classes = getClsList(el);
            var map = {};

            while (classes.length) {
                map[classes.pop()] = 1;
            }

            return map;
        }

        function expectCls (el, cls) {
            if (el.isWidget) {
                el = el.el;
            }

            var classes = typeof cls === 'string' ? cls.split(' ') : cls;
            var map = getClsMap(el);

            while (classes.length) {
                var c = classes.pop();

                if (c) {
                    if (!map[c]) {
                        Ext.raise('Expected element to have class "' + c +
                            '" but it had these "' + el.dom.className + '"');
                    }

                    delete map[c];
                }
            }

            classes = Ext.Object.getKeys(map);
            if (classes.length) {
                Ext.raise('Expected cls to have only "' + cls + '" but found "' +
                    classes.join(' ') + '"');
            }
        }

        describe("configuration", function() {
            it("should start empty", function() {
                makeComponent();

                expectCls(component, '');
            });

            it("should convert a string into an array", function() {
                makeComponent({
                    cls: 'one'
                });

                expect(component.getCls()).toEqual(['one']);
            });

            it("should accept an array", function() {
                makeComponent({
                    cls: ['one', 'two']
                });

                expect(component.getCls()).toEqual(['one', 'two']);
            });
        });

        describe("addCls", function() {
            beforeEach(function() {
                makeComponent();
            });

            describe("no prefix/suffix", function() {
                it("should convert the cls to an array and add it to the component", function() {
                    component.addCls('one');
                    expectCls(component, 'one');
                    expect(getClsMap(component)).toEqual({ one: 1 });

                    component.addCls('two');
                    expectCls(component, 'one two');
                    expect(getClsMap(component)).toEqual({ one: 1, two: 1 });
                });

                it("should add each of the cls to the component", function() {
                    component.addCls(['one', 'two']);
                    expectCls(component, 'one two');

                    component.addCls(['two', 'three']);
                    expectCls(component, 'one two three');
                    expect(getClsMap(component)).toEqual({ one: 1, two: 1, three: 1 });
                });

                it("should allow for adding both strings and arrays", function() {
                    component.addCls('one');
                    expectCls(component, 'one');

                    component.addCls(['two', 'three']);
                    expectCls(component, 'one two three');
                });

                it("should allow for adding both strings and arrays (reverse)", function() {
                    component.addCls(['two', 'three']);
                    expectCls(component, 'two three');

                    component.addCls('one');
                    expectCls(component, 'one two three');
                });
            });

            describe("prefix", function() {
                it("should convert the cls to an array and add it to the component", function() {
                    component.addCls('one', 'x-');
                    expectCls(component, 'x-one');

                    component.addCls('two', 'x-');
                    expectCls(component, 'x-one x-two');
                });

                it("should trim spaces and add it to the component", function() {
                    component.addCls('   one   ', 'x-');
                    expectCls(component, 'x-one');

                    component.addCls('two', 'x-');
                    expectCls(component, 'x-one x-two');
                });

                it("should add each of the cls to the component", function() {
                    component.addCls(['one', 'two'], 'x-');
                    expectCls(component, 'x-one x-two');

                    component.addCls(['two', 'three'], 'x-');
                    expectCls(component, 'x-one x-two x-three');
                });

                it("should allow for adding both strings and arrays", function() {
                    component.addCls('one', 'x-');
                    expectCls(component, 'x-one');

                    component.addCls(['two', 'three'], 'x-');
                    expectCls(component, 'x-one x-two x-three');
                });

                it("should allow for adding both strings and arrays (reverse)", function() {
                    component.addCls(['two', 'three'], 'x-');
                    expectCls(component, 'x-two x-three');

                    component.addCls('one', 'x-');
                    expectCls(component, 'x-one x-two x-three');
                });
            });

            describe("suffix", function() {
                it("should convert the cls to an array and add it to the component", function() {
                    component.addCls('one', null, '-y');
                    expectCls(component, 'one-y');

                    component.addCls('two', null, '-y');
                    expectCls(component, 'one-y two-y');
                });

                it("should add each of the cls to the component", function() {
                    component.addCls(['one', 'two'], null, '-y');
                    expectCls(component, 'one-y two-y');

                    component.addCls(['two', 'three'], null, '-y');
                    expectCls(component, 'one-y two-y three-y');
                });

                it("should allow for adding both strings and arrays", function() {
                    component.addCls('one', null, '-y');
                    expectCls(component, 'one-y');

                    component.addCls(['two', 'three'], null, '-y');
                    expectCls(component, 'one-y two-y three-y');
                });

                it("should allow for adding both strings and arrays (reverse)", function() {
                    component.addCls(['two', 'three'], null, '-y');
                    expectCls(component, 'two-y three-y');

                    component.addCls('one', null, '-y');
                    expectCls(component, 'one-y two-y three-y');
                });
            });

            describe("prefix + suffix", function() {
                it("should convert the cls to an array and add it to the component", function() {
                    component.addCls('one', 'x-', '-y');
                    expectCls(component, 'x-one-y');

                    component.addCls('two', 'x-', '-y');
                    expectCls(component, 'x-one-y x-two-y');
                });

                it("should add each of the cls to the component", function() {
                    component.addCls(['one', 'two'], 'x-', '-y');
                    expectCls(component, 'x-one-y x-two-y');

                    component.addCls(['two', 'three'], 'x-', '-y');

                    expectCls(component, 'x-one-y x-two-y x-three-y');
                });

                it("should allow for adding both strings and arrays", function() {
                    component.addCls('one', 'x-', '-y');
                    expectCls(component, 'x-one-y');

                    component.addCls(['two', 'three'], 'x-', '-y');
                    expectCls(component, 'x-one-y x-two-y x-three-y');
                });

                it("should allow for adding both strings and arrays (reverse)", function() {
                    component.addCls(['two', 'three'], 'x-', '-y');
                    expectCls(component, 'x-two-y x-three-y');

                    component.addCls('one', 'x-', '-y');
                    expectCls(component, 'x-one-y x-two-y x-three-y');
                });
            });
        });

        describe("removeCls", function() {
            describe("no prefix/suffix", function() {
                describe("removing nothing", function() {
                    beforeEach(function() {
                        makeComponent();
                    });

                    it("should do nothing", function() {
                        var s = component.el.dom.className;

                        component.removeCls('one');

                        expect(component.el.dom.className).toEqual(s);
                    });
                });

                describe("removing single cls", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: 'one'
                        });
                    });

                    it("should remove the cls (string)", function() {
                        expect(component.getCls()).toEqual(['one']);

                        component.removeCls('one');

                        expect(getClsList(component)).toEqual([]);
                    });

                    it("should remove the cls (array)", function() {
                        expect(component.getCls()).toEqual(['one']);

                        component.removeCls(['one']);

                        expect(getClsList(component)).toEqual([]);
                    });
                });

                describe("removing mulitple cls", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: ['one', 'two']
                        });
                    });

                    it("should remove the cls (string)", function() {
                        expect(component.getCls()).toEqual(['one', 'two']);

                        component.removeCls('two');

                        expectCls(component, 'one');
                    });

                    it("should remove the cls (array)", function() {
                        expect(component.getCls()).toEqual(['one', 'two']);

                        component.removeCls(['one']);

                        expectCls(component, 'two');
                    });

                    it("should remove the cls (array, multiple)", function() {
                        expect(component.getCls()).toEqual(['one', 'two']);

                        component.removeCls(['one', 'two']);

                        expect(getClsList(component)).toEqual([]);
                    });
                });
            });

            describe("prefix", function() {
                describe("removing nothing", function() {
                    beforeEach(function() {
                        makeComponent();
                    });

                    it("should do nothing", function() {
                        var s = component.el.dom.className;

                        component.removeCls('one', 'x-');

                        expect(component.el.dom.className).toEqual(s);
                    });
                });

                describe("removing single cls", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: 'x-one'
                        });
                    });

                    it("should remove the cls (string)", function() {
                        expect(component.getCls()).toEqual(['x-one']);

                        component.removeCls('one', 'x-');

                        expect(getClsList(component)).toEqual([]);
                    });

                    it("should remove the cls (array)", function() {
                        expect(component.getCls()).toEqual(['x-one']);

                        component.removeCls(['one'], 'x-');

                        expect(getClsList(component)).toEqual([]);
                    });
                });

                describe("removing mulitple cls", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: ['x-one', 'x-two']
                        });
                    });

                    it("should remove the cls (string)", function() {
                        expect(component.getCls()).toEqual(['x-one', 'x-two']);

                        component.removeCls('two', 'x-');

                        expectCls(component, 'x-one');
                    });

                    it("should remove the cls (array)", function() {
                        expect(component.getCls()).toEqual(['x-one', 'x-two']);

                        component.removeCls(['one'], 'x-');

                        expectCls(component, 'x-two');
                    });

                    it("should remove the cls (array, multiple)", function() {
                        expect(component.getCls()).toEqual(['x-one', 'x-two']);

                        component.removeCls(['one', 'two'], 'x-');

                        expect(getClsList(component)).toEqual([]);
                    });
                });
            });

            describe("suffix", function() {
                describe("removing nothing", function() {
                    beforeEach(function() {
                        makeComponent();
                    });

                    it("should do nothing", function() {
                        var s = component.el.dom.className;

                        component.removeCls('one', null, '-y');

                        expect(component.el.dom.className).toEqual(s);
                    });
                });

                describe("removing single cls", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: 'one-y'
                        });
                    });

                    it("should remove the cls (string)", function() {
                        expect(component.getCls()).toEqual(['one-y']);

                        component.removeCls('one', null, '-y');

                        expect(getClsList(component)).toEqual([]);
                    });

                    it("should remove the cls (array)", function() {
                        expect(component.getCls()).toEqual(['one-y']);

                        component.removeCls(['one'], null, '-y');

                        expect(getClsList(component)).toEqual([]);
                    });
                });

                describe("removing mulitple cls", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: ['one-y', 'two-y']
                        });
                    });

                    it("should remove the cls (string)", function() {
                        expect(component.getCls()).toEqual(['one-y', 'two-y']);

                        component.removeCls('two', null, '-y');

                        expectCls(component, 'one-y');
                    });

                    it("should remove the cls (array)", function() {
                        expect(component.getCls()).toEqual(['one-y', 'two-y']);

                        component.removeCls(['one'], null, '-y');

                        expectCls(component, 'two-y');
                    });

                    it("should remove the cls (array, multiple)", function() {
                        expect(component.getCls()).toEqual(['one-y', 'two-y']);

                        component.removeCls(['one', 'two'], null, '-y');

                        expect(getClsList(component)).toEqual([]);
                    });
                });
            });

            describe("prefix + suffix", function() {
                describe("removing nothing", function() {
                    beforeEach(function() {
                        makeComponent();
                    });

                    it("should do nothing", function() {
                        var s = component.el.dom.className;

                        component.removeCls('one', 'x-', '-y');

                        expect(component.el.dom.className).toEqual(s);
                    });
                });

                describe("removing single cls", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: 'x-one-y'
                        });
                    });

                    it("should remove the cls (string)", function() {
                        expect(component.getCls()).toEqual(['x-one-y']);

                        component.removeCls('one', 'x-', '-y');

                        expect(getClsList(component)).toEqual([]);
                    });

                    it("should remove the cls (array)", function() {
                        expect(component.getCls()).toEqual(['x-one-y']);

                        component.removeCls(['one'], 'x-', '-y');

                        expect(getClsList(component)).toEqual([]);
                    });
                });

                describe("removing mulitple cls", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: ['x-one-y', 'x-two-y']
                        });
                    });

                    it("should remove the cls (string)", function() {
                        expect(component.getCls()).toEqual(['x-one-y', 'x-two-y']);

                        component.removeCls('two', 'x-', '-y');

                        expectCls(component, 'x-one-y');
                    });

                    it("should remove the cls (array)", function() {
                        expect(component.getCls()).toEqual(['x-one-y', 'x-two-y']);

                        component.removeCls(['one'], 'x-', '-y');

                        expectCls(component, 'x-two-y');
                    });

                    it("should remove the cls (array, multiple)", function() {
                        expect(component.getCls()).toEqual(['x-one-y', 'x-two-y']);

                        component.removeCls(['one', 'two'], 'x-', '-y');

                        expect(getClsList(component)).toEqual([]);
                    });
                });
            });
        });

        describe("setCls", function() {
            describe("with no existing cls", function() {
                beforeEach(function() {
                    makeComponent();
                    spy = spyOn(component, "updateCls");
                });

                it("should set the cls (string)", function() {
                    expect(component.getCls()).toEqual(null);

                    component.setCls('one');

                    expect(spy).toHaveBeenCalledWith(['one'], null);
                    expect(component.getCls()).toEqual(['one']);
                });

                it("should set the cls (array)", function() {
                    expect(component.getCls()).toEqual(null);

                    component.setCls(['one', 'two']);

                    expect(spy).toHaveBeenCalledWith(['one', 'two'], null);
                    expect(component.getCls()).toEqual(['one', 'two']);
                });
            });

            describe("with existing cls (string)", function() {
                beforeEach(function() {
                    makeComponent({
                        cls: 'one'
                    });
                    spy = spyOn(component, "updateCls");
                });

                it("should set the cls (string)", function() {
                    expect(component.getCls()).toEqual(['one']);

                    component.setCls('two');

                    expect(spy).toHaveBeenCalledWith(['two'], ['one']);
                    expect(component.getCls()).toEqual(['two']);
                });

                it("should set the cls (array)", function() {
                    expect(component.getCls()).toEqual(['one']);

                    component.setCls(['two', 'three']);

                    expect(spy).toHaveBeenCalledWith(['two', 'three'], ['one']);
                    expect(component.getCls()).toEqual(['two', 'three']);
                });
            });

            describe("with existing cls (array)", function() {
                beforeEach(function() {
                    makeComponent({
                        cls: ['one', 'two']
                    });
                    spy = spyOn(component, "updateCls");
                });

                it("should set the cls (string)", function() {
                    expect(component.getCls()).toEqual(['one', 'two']);

                    component.setCls('three');

                    expect(spy).toHaveBeenCalledWith(['three'], ['one', 'two']);
                    expect(component.getCls()).toEqual(['three']);
                });

                it("should set the cls (array)", function() {
                    expect(component.getCls()).toEqual(['one', 'two']);

                    component.setCls(['four', 'three']);

                    expect(spy).toHaveBeenCalledWith(['four', 'three'], ['one', 'two']);
                    expect(component.getCls()).toEqual(['four', 'three']);
                });
            });
        });

        describe("replaceCls", function() {
            describe("no prefix/suffix", function() {
                describe("with no existing cls", function() {
                    beforeEach(function() {
                        makeComponent();
                    });

                    it("should set the cls (string)", function() {
                        expect(component.getCls()).toEqual(null);

                        component.replaceCls('two', 'one');

                        expectCls(component, 'one');
                    });

                    it("should set the cls (array)", function() {
                        expect(component.getCls()).toEqual(null);

                        component.replaceCls(['one', 'two'], ['three', 'four']);

                        expectCls(component, 'three four');
                    });
                });

                describe("with existing cls (string)", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: 'one'
                        });
                    });

                    it("should replace the cls (string)", function() {
                        expect(component.getCls()).toEqual(['one']);

                        component.replaceCls('one', 'two');

                        expectCls(component, 'two');
                    });

                    it("should replace the cls (array)", function() {
                        expect(component.getCls()).toEqual(['one']);

                        component.replaceCls(['one'], ['two']);

                        expectCls(component, 'two');
                    });

                    it("should replace the cls (array, multiple)", function() {
                        expect(component.getCls()).toEqual(['one']);

                        component.replaceCls(['one'], ['two', 'three']);

                        expectCls(component, 'two three');
                    });
                });

                describe("with existing cls (array)", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: ['one', 'two']
                        });
                    });

                    it("should replace the cls (string)", function() {
                        expect(component.getCls()).toEqual(['one', 'two']);

                        component.replaceCls('one', 'three');

                        expectCls(component, 'two three');
                    });

                    it("should replace the cls (array)", function() {
                        expect(component.getCls()).toEqual(['one', 'two']);

                        component.replaceCls(['one', 'two'], ['four', 'three']);

                        expectCls(component, 'three four');
                    });
                });
            });

            describe("prefix", function() {
                describe("with no existing cls", function() {
                    beforeEach(function() {
                        makeComponent();
                    });

                    it("should set the cls (string)", function() {
                        expect(component.getCls()).toEqual(null);

                        component.replaceCls('two', 'one', 'x-');

                        expectCls(component, 'x-one');
                    });

                    it("should set the cls (array)", function() {
                        expect(component.getCls()).toEqual(null);

                        component.replaceCls(['one', 'two'], ['three', 'four'], 'x-');

                        expectCls(component, 'x-three x-four');
                    });
                });

                describe("with existing cls (string)", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: 'x-one'
                        });
                    });

                    it("should replace the cls (string)", function() {
                        expect(component.getCls()).toEqual(['x-one']);

                        component.replaceCls('one', 'two', 'x-');

                        expectCls(component, 'x-two');
                    });

                    it("should replace the cls (array)", function() {
                        expect(component.getCls()).toEqual(['x-one']);

                        component.replaceCls(['one'], ['two'], 'x-');

                        expectCls(component, 'x-two');
                    });

                    it("should replace the cls (array, multiple)", function() {
                        expect(component.getCls()).toEqual(['x-one']);

                        component.replaceCls(['one'], ['two', 'three'], 'x-');

                        expectCls(component, 'x-two x-three');
                    });
                });

                describe("with existing cls (array)", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: ['x-one', 'x-two']
                        });
                    });

                    it("should replace the cls (string)", function() {
                        expect(component.getCls()).toEqual(['x-one', 'x-two']);

                        component.replaceCls('one', 'three', 'x-');

                        expectCls(component, 'x-two x-three');
                    });

                    it("should replace the cls (array)", function() {
                        expect(component.getCls()).toEqual(['x-one', 'x-two']);

                        component.replaceCls(['one', 'two'], ['four', 'three'], 'x-');

                        expectCls(component, 'x-four x-three');
                    });
                });
            });

            describe("suffix", function() {
                describe("with no existing cls", function() {
                    beforeEach(function() {
                        makeComponent();
                    });

                    it("should set the cls (string)", function() {
                        expect(component.getCls()).toEqual(null);

                        component.replaceCls('two', 'one', null, '-y');

                        expectCls(component, 'one-y');
                    });

                    it("should set the cls (array)", function() {
                        expect(component.getCls()).toEqual(null);

                        component.replaceCls(['one', 'two'], ['three', 'four'], null, '-y');

                        expectCls(component, 'three-y four-y');
                    });
                });

                describe("with existing cls (string)", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: 'one-y'
                        });
                    });

                    it("should replace the cls (string)", function() {
                        expect(component.getCls()).toEqual(['one-y']);

                        component.replaceCls('one', 'two', null, '-y');

                        expectCls(component, 'two-y');
                    });

                    it("should replace the cls (array)", function() {
                        expect(component.getCls()).toEqual(['one-y']);

                        component.replaceCls(['one'], ['two'], null, '-y');

                        expectCls(component, 'two-y');
                    });

                    it("should replace the cls (array, multiple)", function() {
                        expect(component.getCls()).toEqual(['one-y']);

                        component.replaceCls(['one'], ['two', 'three'], null, '-y');

                        expectCls(component, 'two-y three-y');
                    });
                });

                describe("with existing cls (array)", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: ['one-y', 'two-y']
                        });
                    });

                    it("should replace the cls (string)", function() {
                        expect(component.getCls()).toEqual(['one-y', 'two-y']);

                        component.replaceCls('one', 'three', null, '-y');

                        expectCls(component, 'two-y three-y');
                    });

                    it("should replace the cls (array)", function() {
                        expect(component.getCls()).toEqual(['one-y', 'two-y']);

                        component.replaceCls(['one', 'two'], ['four', 'three'], null, '-y');

                        expectCls(component, 'four-y three-y');
                    });
                });
            });

            describe("prefix+suffix", function() {
                describe("with no existing cls", function() {
                    beforeEach(function() {
                        makeComponent();
                    });

                    it("should set the cls (string)", function() {
                        expect(component.getCls()).toEqual(null);

                        component.replaceCls('two', 'one', 'x-', '-y');

                        expectCls(component, 'x-one-y');
                    });

                    it("should set the cls (array)", function() {
                        expect(component.getCls()).toEqual(null);

                        component.replaceCls(['one', 'two'], ['three', 'four'], 'x-', '-y');

                        expectCls(component, 'x-three-y x-four-y');
                    });
                });

                describe("with existing cls (string)", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: 'x-one-y'
                        });
                    });

                    it("should replace the cls (string)", function() {
                        expect(component.getCls()).toEqual(['x-one-y']);

                        component.replaceCls('one', 'two', 'x-', '-y');

                        expectCls(component, 'x-two-y');
                    });

                    it("should replace the cls (array)", function() {
                        expect(component.getCls()).toEqual(['x-one-y']);

                        component.replaceCls(['one'], ['two'], 'x-', '-y');

                        expectCls(component, 'x-two-y');
                    });

                    it("should replace the cls (array, multiple)", function() {
                        expect(component.getCls()).toEqual(['x-one-y']);

                        component.replaceCls(['one'], ['two', 'three'], 'x-', '-y');

                        expectCls(component, 'x-two-y x-three-y');
                    });
                });

                describe("with existing cls (array)", function() {
                    beforeEach(function() {
                        makeComponent({
                            cls: ['x-one-y', 'x-two-y']
                        });
                    });

                    it("should replace the cls (string)", function() {
                        expect(component.getCls()).toEqual(['x-one-y', 'x-two-y']);

                        component.replaceCls('one', 'three', 'x-', '-y');

                        expectCls(component, 'x-two-y x-three-y');
                    });

                    it("should replace the cls (array)", function() {
                        expect(component.getCls()).toEqual(['x-one-y', 'x-two-y']);
                        component.replaceCls(['one', 'two'], ['four', 'three'], 'x-', '-y');

                        expectCls(component, 'x-three-y x-four-y');
                    });
                });
            });
        });

        describe("toggleCls", function() {
            describe("add cls", function() {
                it("add to component's element", function() {
                    makeComponent();

                    component.toggleCls('one');

                    expect(component.element).toHaveCls('one');
                });

                it("force add cls to component", function() {
                    makeComponent({
                        cls : 'one'
                    });

                    //normally since the component already has the cls it would remove
                    //but since we are passing `true`, it will force it to add
                    component.toggleCls('one', true);

                    expect(component.element).toHaveCls('one');
                });
            });

            describe("remove cls", function() {
                it("remove from component's element", function() {
                    makeComponent({
                        cls : 'one'
                    });

                    component.toggleCls('one');

                    expect(component.element).not.toHaveCls('one');
                });
            });
        });
    });

    describe("visibility", function() {
        describe("isHidden", function() {
            describe("deep=undefined", function() {
                describe("as a root", function() {
                    it("should return true if the component is hidden", function() {
                        makeComponent({
                            hidden: true
                        });
                        expect(component.isHidden()).toBe(true);
                    });

                    it("should return false if the component is not hidden", function() {
                        makeComponent();
                        expect(component.isHidden()).toBe(false);
                    });
                });

                describe("in a container", function() {
                    it("should return true if the component is hidden but the container is not", function() {
                        var ct = new Ext.Container({
                            items: {
                                xtype: 'component',
                                hidden: true
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isHidden()).toBe(true);
                        ct.destroy();
                    });

                    it("should return true if the component is hidden and the container is hidden", function() {
                        var ct = new Ext.Container({
                            hidden: true,
                            items: {
                                xtype: 'component',
                                hidden: true
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isHidden()).toBe(true);
                        ct.destroy();
                    });

                    it("should return false if the component is not hidden and the container is not", function() {
                        var ct = new Ext.Container({
                            items: {
                                xtype: 'component'
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isHidden()).toBe(false);
                        ct.destroy();
                    });

                    it("should return false if the component is not hidden and the container is hidden", function() {
                        var ct = new Ext.Container({
                            hidden: true,
                            items: {
                                xtype: 'component'
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isHidden()).toBe(false);
                        ct.destroy();
                    });

                    it("should return false if the component is not hidden and a high level container is hidden", function() {
                        var ct = new Ext.Container({
                            hidden: true,
                            items: {
                                xtype: 'container',
                                items: {
                                    xtype: 'container',
                                    items: {
                                        xtype: 'component',
                                        itemId: 'c'
                                    }
                                }
                            }
                        });
                        component = ct.down('#c');
                        expect(component.isHidden()).toBe(false);
                        ct.destroy();
                    });
                });
            });

            describe("deep=false", function() {
                describe("as a root", function() {
                    it("should return true if the component is hidden", function() {
                        makeComponent({
                            hidden: true
                        });
                        expect(component.isHidden(false)).toBe(true);
                    });

                    it("should return false if the component is not hidden", function() {
                        makeComponent();
                        expect(component.isHidden(false)).toBe(false);
                    });
                });

                describe("in a container", function() {
                    it("should return true if the component is hidden but the container is not", function() {
                        var ct = new Ext.Container({
                            items: {
                                xtype: 'component',
                                hidden: true
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isHidden(false)).toBe(true);
                        ct.destroy();
                    });

                    it("should return true if the component is hidden and the container is hidden", function() {
                        var ct = new Ext.Container({
                            hidden: true,
                            items: {
                                xtype: 'component',
                                hidden: true
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isHidden(false)).toBe(true);
                        ct.destroy();
                    });

                    it("should return false if the component is not hidden and the container is not", function() {
                        var ct = new Ext.Container({
                            items: {
                                xtype: 'component'
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isHidden(false)).toBe(false);
                        ct.destroy();
                    });

                    it("should return false if the component is not hidden and the container is hidden", function() {
                        var ct = new Ext.Container({
                            hidden: true,
                            items: {
                                xtype: 'component'
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isHidden(false)).toBe(false);
                        ct.destroy();
                    });

                    it("should return false if the component is not hidden and a high level container is hidden", function() {
                        var ct = new Ext.Container({
                            hidden: true,
                            items: {
                                xtype: 'container',
                                items: {
                                    xtype: 'container',
                                    items: {
                                        xtype: 'component',
                                        itemId: 'c'
                                    }
                                }
                            }
                        });
                        component = ct.down('#c');
                        expect(component.isHidden(false)).toBe(false);
                        ct.destroy();
                    });
                });
            });

            describe("deep=true", function() {
                describe("as a root", function() {
                    it("should return true if the component is hidden", function() {
                        makeComponent({
                            hidden: true
                        });
                        expect(component.isHidden(true)).toBe(true);
                    });

                    it("should return false if the component is not hidden", function() {
                        makeComponent();
                        expect(component.isHidden(true)).toBe(false);
                    });
                });

                describe("in a container", function() {
                    it("should return true if the component is hidden but the container is not", function() {
                        var ct = new Ext.Container({
                            items: {
                                xtype: 'component',
                                hidden: true
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isHidden(true)).toBe(true);
                        ct.destroy();
                    });

                    it("should return true if the component is hidden and the container is hidden", function() {
                        var ct = new Ext.Container({
                            hidden: true,
                            items: {
                                xtype: 'component',
                                hidden: true
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isHidden(true)).toBe(true);
                        ct.destroy();
                    });

                    it("should return false if the component is not hidden and the container is not", function() {
                        var ct = new Ext.Container({
                            items: {
                                xtype: 'component'
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isHidden(true)).toBe(false);
                        ct.destroy();
                    });

                    it("should return true if the component is not hidden and the container is hidden", function() {
                        var ct = new Ext.Container({
                            hidden: true,
                            items: {
                                xtype: 'component'
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isHidden(true)).toBe(true);
                        ct.destroy();
                    });

                    it("should return true if the component is not hidden and a high level container is hidden", function() {
                        var ct = new Ext.Container({
                            hidden: true,
                            items: {
                                xtype: 'container',
                                items: {
                                    xtype: 'container',
                                    items: {
                                        xtype: 'component',
                                        itemId: 'c'
                                    }
                                }
                            }
                        });
                        component = ct.down('#c');
                        expect(component.isHidden(true)).toBe(true);
                        ct.destroy();
                    });
                });
            });
        });

        describe("isVisible", function() {
            describe("deep=undefined", function() {
                describe("as a root", function() {
                    it("should return false if the component is hidden", function() {
                        makeComponent({
                            hidden: true
                        });
                        expect(component.isVisible()).toBe(false);
                    });

                    it("should return true if the component is not hidden", function() {
                        makeComponent({
                            renderTo: document.body
                        });
                        expect(component.isVisible()).toBe(true);
                    });
                });

                describe("in a container", function() {
                    it("should return false if the component is hidden but the container is not", function() {
                        var ct = new Ext.Container({
                            items: {
                                xtype: 'component',
                                hidden: true
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isVisible()).toBe(false);
                        ct.destroy();
                    });

                    it("should return false if the component is hidden and the container is hidden", function() {
                        var ct = new Ext.Container({
                            hidden: true,
                            items: {
                                xtype: 'component',
                                hidden: true
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isVisible()).toBe(false);
                        ct.destroy();
                    });

                    it('should return false if the component is not rendered', function() {
                        var ct = new Ext.Container({
                            items: {
                                xtype: 'component'
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isVisible()).toBe(false);
                        ct.destroy();
                    });

                    it("should return true if the component is not hidden and the container is not", function() {
                        var ct = new Ext.Container({
                            renderTo: document.body,
                            items: {
                                xtype: 'component'
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isVisible()).toBe(true);
                        ct.destroy();
                    });

                    it("should return true if the component is not hidden and the container is hidden", function() {
                        var ct = new Ext.Container({
                            renderTo: document.body,
                            hidden: true,
                            items: {
                                xtype: 'component'
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isVisible()).toBe(true);
                        ct.destroy();
                    });

                    it("should return true if the component is not hidden and a high level container is hidden", function() {
                        var ct = new Ext.Container({
                            renderTo: document.body,
                            hidden: true,
                            items: {
                                xtype: 'container',
                                items: {
                                    xtype: 'container',
                                    items: {
                                        xtype: 'component',
                                        itemId: 'c'
                                    }
                                }
                            }
                        });
                        component = ct.down('#c');
                        expect(component.isVisible()).toBe(true);
                        ct.destroy();
                    });
                });
            });

            describe("deep=false", function() {
                describe("as a root", function() {
                    it("should return false if the component is hidden", function() {
                        makeComponent({
                            hidden: true
                        });
                        expect(component.isVisible(false)).toBe(false);
                    });

                    it("should return true if the component is not hidden", function() {
                        makeComponent({
                            renderTo: document.body
                        });
                        expect(component.isVisible(false)).toBe(true);
                    });
                });

                describe("in a container", function() {
                    it("should return false if the component is hidden but the container is not", function() {
                        var ct = new Ext.Container({
                            items: {
                                xtype: 'component',
                                hidden: true
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isVisible(false)).toBe(false);
                        ct.destroy();
                    });

                    it("should return false if the component is hidden and the container is hidden", function() {
                        var ct = new Ext.Container({
                            hidden: true,
                            items: {
                                xtype: 'component',
                                hidden: true
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isVisible(false)).toBe(false);
                        ct.destroy();
                    });

                    it("should return true if the component is not hidden and the container is not", function() {
                        var ct = new Ext.Container({
                            renderTo: document.body,
                            items: {
                                xtype: 'component'
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isVisible(false)).toBe(true);
                        ct.destroy();
                    });

                    it("should return true if the component is not hidden and the container is hidden", function() {
                        var ct = new Ext.Container({
                            renderTo: document.body,
                            hidden: true,
                            items: {
                                xtype: 'component'
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isVisible(false)).toBe(true);
                        ct.destroy();
                    });

                    it("should return true if the component is not hidden and a high level container is hidden", function() {
                        var ct = new Ext.Container({
                            renderTo: document.body,
                            hidden: true,
                            items: {
                                xtype: 'container',
                                items: {
                                    xtype: 'container',
                                    items: {
                                        xtype: 'component',
                                        itemId: 'c'
                                    }
                                }
                            }
                        });
                        component = ct.down('#c');
                        expect(component.isVisible(false)).toBe(true);
                        ct.destroy();
                    });
                });
            });

            describe("deep=true", function() {
                describe("as a root", function() {
                    it("should return false if the component is hidden", function() {
                        makeComponent({
                            hidden: true
                        });
                        expect(component.isVisible(true)).toBe(false);
                    });

                    it("should return true if the component is not hidden", function() {
                        makeComponent({
                            renderTo: document.body
                        });
                        expect(component.isVisible(true)).toBe(true);
                    });
                });

                describe("in a container", function() {
                    it("should return false if the component is hidden but the container is not", function() {
                        var ct = new Ext.Container({
                            renderTo: document.body,
                            items: {
                                xtype: 'component',
                                hidden: true
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isVisible(true)).toBe(false);
                        ct.destroy();
                    });

                    it("should return false if the component is hidden and the container is hidden", function() {
                        var ct = new Ext.Container({
                            hidden: true,
                            items: {
                                xtype: 'component',
                                hidden: true
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isVisible(true)).toBe(false);
                        ct.destroy();
                    });

                    it("should return true if the component is not hidden and the container is not", function() {
                        var ct = new Ext.Container({
                            renderTo: document.body,
                            items: {
                                xtype: 'component'
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isVisible(true)).toBe(true);
                        ct.destroy();
                    });

                    it("should return false if the component is not hidden and the container is hidden", function() {
                        var ct = new Ext.Container({
                            hidden: true,
                            items: {
                                xtype: 'component'
                            }
                        });
                        component = ct.getItems().first();
                        expect(component.isVisible(true)).toBe(false);
                        ct.destroy();
                    });

                    it("should return false if the component is not hidden and a high level container is hidden", function() {
                        var ct = new Ext.Container({
                            hidden: true,
                            items: {
                                xtype: 'container',
                                items: {
                                    xtype: 'container',
                                    items: {
                                        xtype: 'component',
                                        itemId: 'c'
                                    }
                                }
                            }
                        });
                        component = ct.down('#c');
                        expect(component.isVisible(true)).toBe(false);
                        ct.destroy();
                    });
                });
            });
        });
    });

    describe('modal positioned', function() {
        var ct;
        afterEach(function() {
            Ext.destroy(ct);
        });

        it("should set the mask's zIndex one less that its own zIndex", function() {
            makeComponent({
                centered: true,
                modal: true
            });
            ct = new Ext.Container({
                items: component,
                renderTo: document.body
            });
            component.show();

            // Mask must be below component
            expect(Number(component.getModal().el.dom.style.zIndex)).toBe(Number(component.el.dom.style.zIndex) - 1);
        });
    });

    describe('tpl/data call', function() {
        function makeTplComponent(cfg) {
            makeComponent(Ext.apply({
                renderTo: Ext.getBody()
            }, cfg));
        }

        describe("at construction", function() {
            it("should be able to configure just a tpl", function() {
                makeTplComponent({
                    tpl: '{foo}'
                });
                expect(component.getInnerHtmlElement()).toHaveHtml('');
            });

            it("should be able to configure just data", function() {
                makeTplComponent({
                    data: {
                        foo: 1
                    }
                });
                expect(component.getInnerHtmlElement()).toHaveHtml('');
            });

            it("should be able to configure tpl and data", function() {
                makeTplComponent({
                    tpl: '{foo}',
                    data: {
                        foo: 1
                    }
                });
                expect(component.getInnerHtmlElement()).toHaveHtml('1');
            });

            it("should accept an array tpl", function() {
                makeTplComponent({
                    tpl: ['{foo}', '{bar}'],
                    data: {
                        foo: 1,
                        bar: 2
                    }
                });
                expect(component.getInnerHtmlElement()).toHaveHtml('12');
            });
        });

        describe("dynamic", function() {
            describe("tpl", function() {
                describe("setting a tpl", function() {
                    describe("with no data", function() {
                        it("should be empty", function() {
                            makeTplComponent();
                            component.setTpl('{foo}');
                            expect(component.getInnerHtmlElement()).toHaveHtml('');
                        });
                    });

                    describe("with data", function() {
                        it("should render the data with the new tpl", function() {
                            makeTplComponent({
                                data: {
                                    foo: 1
                                }
                            });
                            component.setTpl('{foo}');
                            expect(component.getInnerHtmlElement()).toHaveHtml('1');
                        });
                    });
                });

                describe("clearing a tpl", function() {
                    describe("with no data", function() {
                        it("should be empty", function() {
                            makeTplComponent({
                                tpl: '{foo}'
                            });
                            component.setTpl(null);
                            expect(component.getInnerHtmlElement()).toHaveHtml('');
                        });
                    });

                    describe("with data", function() {
                        it("should be empty", function() {
                            makeTplComponent({
                                tpl: '{foo}',
                                data: {
                                    foo: 1
                                }
                            });
                            component.setTpl(null);
                            expect(component.getInnerHtmlElement()).toHaveHtml('');
                        });
                    });
                });

                describe("changing a tpl", function() {
                    describe("with no data", function() {
                        it("should be empty", function() {
                            makeTplComponent({
                                tpl: '{foo}'
                            });
                            component.setTpl('{bar}');
                            expect(component.getInnerHtmlElement()).toHaveHtml('');
                        });
                    });

                    describe("with data", function() {
                        it("should render the data with the new tpl", function() {
                            makeTplComponent({
                                tpl: '{foo}',
                                data: {
                                    foo: 1,
                                    bar: 2
                                }
                            });
                            component.setTpl('{bar}');
                            expect(component.getInnerHtmlElement()).toHaveHtml('2');
                        });
                    });
                });
            });

            describe("data", function() {
                describe("setting data", function() {
                    describe("with no tpl", function() {
                        it("should be empty", function() {
                            makeTplComponent();
                            component.setData({
                                foo: 1
                            });
                            expect(component.getInnerHtmlElement()).toHaveHtml('');
                        });
                    });

                    describe("with a tpl", function() {
                        it("should render the data", function() {
                            makeTplComponent({
                                tpl: '{foo}'
                            });
                            component.setData({
                                foo: 1
                            });
                            expect(component.getInnerHtmlElement()).toHaveHtml('1');
                        });
                    });
                });

                describe("clearing data", function() {
                    describe("with no tpl", function() {
                        it("should be empty", function() {
                            makeTplComponent({
                                data: {
                                    foo: 1
                                }
                            });
                            component.setData(null);
                            expect(component.getInnerHtmlElement()).toHaveHtml('');
                        });
                    });

                    describe("with a tpl", function() {
                        it("should be empty", function() {
                            makeTplComponent({
                                tpl: '{foo}',
                                data: {
                                    foo: 1
                                }
                            });
                            component.setData(null);
                            expect(component.getInnerHtmlElement()).toHaveHtml('');
                        });
                    });
                });

                describe("changing data", function() {
                    describe("with no tpl", function() {
                        it("should be empty", function() {
                            makeTplComponent({
                                data: {
                                    foo: 1
                                }
                            });
                            component.setData({
                                foo: 2
                            });
                            expect(component.getInnerHtmlElement()).toHaveHtml('');
                        });
                    });

                    describe("with a tpl", function() {
                        it("should be empty", function() {
                            makeTplComponent({
                                tpl: '{foo}',
                                data: {
                                    foo: 1
                                }
                            });
                            component.setData({
                                foo: 2
                            });
                            expect(component.getInnerHtmlElement()).toHaveHtml('2');
                        });
                    });
                });
            });

            describe("tplWriteMode", function() {
                it("should respect the tplWriteMode", function() {
                    makeComponent({
                        tpl: '{foo}',
                        data: {
                            foo: 1
                        }
                    });
                    expect(component.getInnerHtmlElement()).toHaveHtml('1');
                    component.setData({
                        foo: 2
                    });
                    expect(component.getInnerHtmlElement()).toHaveHtml('2');
                    component.setTplWriteMode('append');
                    component.setData({
                        foo: 3
                    });
                    expect(component.getInnerHtmlElement()).toHaveHtml('23');
                    component.setTpl('a{foo}');
                    expect(component.getInnerHtmlElement()).toHaveHtml('23a3');
                    component.setTplWriteMode('insertFirst');
                    component.setData({
                        foo: 4
                    });
                    expect(component.getInnerHtmlElement()).toHaveHtml('a423a3');
                });
            });
        });
    });

    describe('responding to resizing', function() {
        var container, onResizeSpy, resizeEventSpy;

        beforeEach(function() {
            onResizeSpy = jasmine.createSpy();
            resizeEventSpy = jasmine.createSpy();
        });

        afterEach(function() {
            container = Ext.destroy(container);
        });

        function getInfo(flag) {
            return {
                flag: flag,  // w = 0x01, h = 0x02
                width: component.element.measure('w'),
                height: component.element.measure('h'),
                contentWidth: component.el.dom.offsetWidth,
                contentHeight: component.el.dom.offsetHeight
            };
        }

        function waitsForCalls(n) {
            waitsFor(function() {
                return onResizeSpy.callCount === n &&
                       resizeEventSpy.callCount === n;
            });
        }

        function expectSizeCalls(flag, w, h, oldW, oldH) {
            var info = getInfo(flag);

            oldW = oldW || null;
            oldH = oldH || null;

            expect(onResizeSpy.mostRecentCall.args).toEqual(
                [ w, h, oldW, oldH, info ]);

            expect(resizeEventSpy.mostRecentCall.args.slice(0, 6)).toEqual(
                [ component, w, h, oldW, oldH, info ]);

            // onResize is called first - its element resize listener is at priority 1000
            expect(onResizeSpy.callSequence).toBeLessThan(resizeEventSpy.callSequence);
        }

        function makeSizeComponent(cfg) {
            makeComponent(Ext.apply(cfg, {
                renderTo: Ext.getBody(),
                listeners: {
                    resize: resizeEventSpy
                }
            }));
            component.onResize = onResizeSpy;

            waitsForCalls(1);

            runs(function() {
                expectSizeCalls(3, component.el.getWidth(), component.el.getHeight());
            });

            return component;
        }

        function makeSizeContainer(ctCfg, cfg) {
            ctCfg.items = ctCfg.items || [];

            ctCfg.items.push(Ext.apply({
                xtype: 'component',
                listeners: {
                    resize: resizeEventSpy
                }
            }, cfg));

            container = new Ext.Container(Ext.apply({
                renderTo: Ext.getBody()
            }, ctCfg));
            component = container.items.last();
            component.onResize = onResizeSpy;

            waitsForCalls(1);

            runs(function() {
                expectSizeCalls(3, component.el.getWidth(), component.el.getHeight());
            });
        }

        describe('shrinkWrap', function() {
            it('should respond to content size changes', function() {
                makeSizeComponent({
                    style: 'position: absolute;',
                    html: '<div class="foo" style="height: 100px; width: 100px;">Foo</div><div style="height: 70px; width: 50px;">Bar</div>'
                });

                runs(function() {
                    component.el.down('.foo').destroy();
                });
                waitsForCalls(2);
                runs(function() {
                    expectSizeCalls(3, 50, 70, 100, 170);
                });
            });
        });

        describe('Auto sizing', function() {
            it('should respond to changes in relative sizing values', function() {
                makeSizeContainer({
                    layout: 'hbox',
                    height: 100,
                    width: 100
                }, {
                    width: '50%',
                    height: 100
                });

                runs(function() {
                    component.setWidth('70%');
                });

                waitsForCalls(2);
                runs(function() {
                    expectSizeCalls(1, 70, 100, 50, 100);

                    // Now widen the container
                    container.setWidth(200);
                    // Force a repaint
                    component.el.dom.offsetWidth;
                });

                waitsForCalls(3);
                runs(function() {
                    expectSizeCalls(1, 140, 100, 70, 100);
                });
            });
        });

        describe('Layout sizing', function() {
            it('should respond to layout-induced changes', function() {
                makeSizeContainer({
                    layout: 'hbox',
                    height: 100,
                    width: 100,
                    items: [{
                        flex: 1
                    }]
                }, {
                    flex: 1,
                    height: 100
                });

                runs(function() {
                    component.setFlex(3);
                });

                waitsForCalls(2);
                runs(function() {
                    expectSizeCalls(1, 75, 100, 50, 100);

                    container.setWidth(200);
                });

                waitsForCalls(3);

                // Wait for the layout and the async event to run on the tail end of a browser layout
                runs(function() {
                    expectSizeCalls(1, 150, 100, 75, 100);
                });
            });
        });

        describe('Constraints', function() {
            function makeSuite(cfgName) {
                var vertical = Ext.String.endsWith(cfgName, 'Height'),
                    isMin = Ext.String.startsWith(cfgName, 'min'),
                    setter = Ext.Config.get(cfgName).names.set,
                    flag = vertical ? 2 : 1,
                    size = isMin ? 60 : 40;

                function makeCt(doSet) {
                    var o = {
                        flex: 1
                    };

                    if (doSet) {
                        o[cfgName] = size;
                    }

                    makeSizeContainer({
                        layout: {
                            type: vertical ? 'vbox' : 'hbox',
                            align: 'stretch'
                        },
                        height: 100,
                        width: 100,
                        items: [{
                            flex: 1
                        }]
                    }, o);
                }

                describe(cfgName, function() {
                    it("should react to setting " + cfgName, function() {
                        makeCt(false);
                        runs(function() {
                            component[setter](size);
                        });

                        waitsForCalls(2);

                        runs(function() {
                            var w = vertical ? 100 : size,
                                h = !vertical ? 100 : size,
                                oldW = vertical ? 100 : 50,
                                oldH = !vertical ? 100 : 50;

                            expectSizeCalls(flag, w, h, oldW, oldH);
                        });
                    });

                    it("should react to clearing " + cfgName, function() {
                        makeCt(true);
                        runs(function() {
                            component[setter](null);
                        });

                        waitsForCalls(2);

                        runs(function() {
                            var w = vertical ? 100 : 50,
                                h = !vertical ? 100 : 50,
                                oldW = vertical ? 100 : size,
                                oldH = !vertical ? 100 : size;

                            expectSizeCalls(flag, w, h, oldW, oldH);
                        });
                    });
                });
            }

            makeSuite('maxHeight');
            makeSuite('maxWidth');
            makeSuite('minHeight');
            makeSuite('minWidth');
        });
    });

    describe('destroy', function () {
        it("should fire the 'destroy' event", function () {
            var cmp = makeComponent({}),
                isFired;

            cmp.on('destroy', function () {
                isFired = true;
            });
            cmp.destroy();

            expect(isFired).toBe(true);
        });

        it("should destroy the animations when destroying the component", function() {
            var cmp = makeComponent({
                    showAnimation: {
                        type: 'slideIn',
                        duration: 5,
                        easing: 'ease-out'
                    },
    
                    hideAnimation: {
                        type: 'slideOut',
                        duration: 5,
                        easing: 'ease-in'
                    },
                    modal: true,
                    floated: true,
                    html: 'Test'
                }),
                showAnim, hideAnim;
            
            showAnim = cmp.getShowAnimation();
            hideAnim = cmp.getHideAnimation();

            cmp.show();

            waitsFor(function () {
                return !cmp.activeAnimation;
            });

            runs(function () {
                cmp.hide();
            });

            waitsFor(function () {
                return !cmp.activeAnimation;
            });

            runs(function () {
                var showAnimSpy = spyOn(showAnim, 'destroy').andCallThrough();
                var hideAnimSpy = spyOn(hideAnim, 'destroy').andCallThrough();

                cmp.destroy();

                expect(showAnimSpy).toHaveBeenCalled();
                expect(hideAnimSpy).toHaveBeenCalled();
            });
        });
    });

    describe("rootCls", function() {
        it("should add the rootCls to the component if it has no parent container", function() {
            var cmp = new Ext.Component();

            expect(cmp.el).toHaveCls('x-root');

            cmp.destroy();
        });

        it("should remove the rootCls from the component when it is added to a container", function() {
            var cmp = new Ext.Component(),
                ct = new Ext.Container();

            ct.add(cmp);
            expect(cmp.el).not.toHaveCls('x-root');
            expect(ct.el).toHaveCls('x-root');

            ct.destroy();
        });

        it("should add the rootCls when the component is removed from a container", function() {
            var ct = Ext.create({
                xtype: 'container',
                items: [{
                    xtype: 'component',
                    id: 'cmp'
                }]
            });

            var cmp = Ext.getCmp('cmp');

            ct.remove(cmp, false);

            expect(cmp).toHaveCls('x-root');

            ct.destroy();
            cmp.destroy();
        });

        it("should add the rootCls to only the top-level component in a hierarchy", function() {
            var ct = Ext.create({
                xtype: 'container',
                items: [{
                    xtype: 'container',
                    id: 'ct2',
                    items: [{
                        xtype: 'component',
                        id: 'cmp'
                    }]
                }]
            });

            expect(ct).toHaveCls('x-root');
            expect(Ext.getCmp('ct2')).not.toHaveCls('x-root');
            expect(Ext.getCmp('cmp')).not.toHaveCls('x-root');

            ct.destroy();
        });
    });

    describe('destroyable element listeners', function() {
        it('should remove a destroyable element listener when destroyed', function() {
            component = new Ext.Component({
                style: 'height:100px;width:200px',
                renderTo: document.body
            });

            var called = false,
                myListeners = component.on({
                    element: 'element',
                    destroyable: true,
                    tap: function() {
                        called = true;
                        Ext.destroy(myListeners);
                    }
                });

            Ext.testHelper.tap(component.element);
            expect(called).toBe(true);

            // The listener should have been destroyed and removed.
            called = false;
            Ext.testHelper.tap(component.element);
            expect(called).toBe(false);
        });
        it('should remove a destroyable element listener when destroyed - multi arg form', function() {
            component = new Ext.Component({
                style: 'height:100px;width:200px',
                renderTo: document.body
            });

            var called = false,
                myListeners = component.on('tap', function() {
                    called = true;
                    Ext.destroy(myListeners);
                }, null, {
                    element: 'element',
                    destroyable: true
                });

            Ext.testHelper.tap(component.element);
            expect(called).toBe(true);

            // The listener should have been destroyed and removed.
            called = false;
            Ext.testHelper.tap(component.element);
            expect(called).toBe(false);
        });
    });

    describe("whenVisible", function() {
        var Cls = Ext.define(null, {
            extend: 'Ext.Component',
            fn1: Ext.emptyFn,
            fn2: Ext.emptyFn
        }), ct;

        function makeCls(hidden, preventRender) {
            component = new Cls({
                renderTo: preventRender ? null : Ext.getBody(),
                hidden: hidden
            });
        }

        afterEach(function() {
            ct = Ext.destroy(ct);
        });

        describe("when component is visible", function() {
            it("should run the passed function", function() {
                makeCls();
                spyOn(component, 'fn1');
                component.whenVisible('fn1');
                expect(component.fn1.callCount).toBe(1);
            });

            it("should not pass args by default", function() {
                makeCls();
                spyOn(component, 'fn1');
                component.whenVisible('fn1');
                expect(component.fn1).toHaveBeenCalledWith();
            });

            it("should use the passed args", function() {
                makeCls();
                spyOn(component, 'fn1');
                component.whenVisible('fn1', ['a', 'b']);
                expect(component.fn1).toHaveBeenCalledWith('a', 'b');
            });

            describe("with a show pending", function() {
                it("should trigger any existing calls", function() {
                    var spy = jasmine.createSpy();

                    makeCls(true);

                    component.setShowAnimation({});
                    component.on('show', spy);

                    spyOn(component, 'fn1');
                    spyOn(component, 'fn2');

                    component.whenVisible('fn1');
                    component.show();
                    component.whenVisible('fn2');
                    expect(component.fn1.callCount).toBe(1);
                    expect(component.fn2.callCount).toBe(1);

                    waitsFor(function() {
                        // Wait for the animation to complete
                        return spy.callCount === 1;
                    });
                    runs(function() {
                        expect(component.fn1.callCount).toBe(1);
                        expect(component.fn2.callCount).toBe(1);
                    });
                });
            });

            describe("clearing", function() {
                it("should not cause an exception", function() {
                    makeCls();
                    component.clearWhenVisible('fn1');
                    expect(component.isVisible()).toBe(true);
                });

                it("should not prevent future functions from running", function() {
                    makeCls();
                    spyOn(component, 'fn1');
                    component.clearWhenVisible('fn1');
                    expect(component.fn1).not.toHaveBeenCalled();
                    component.whenVisible('fn1');
                    expect(component.fn1.callCount).toBe(1);
                });
            });
        });

        describe("when component is not visible", function() {
            describe("not nested", function() {
                it("should be able to call multiple methods", function() {
                    makeCls(true);
                    spyOn(component, 'fn1');
                    spyOn(component, 'fn2');

                    component.whenVisible('fn1');
                    component.whenVisible('fn2');

                    component.show();
                    expect(component.fn1.callCount).toBe(1);
                    expect(component.fn2.callCount).toBe(1);
                });

                it("should replace existing calls", function() {
                    makeCls(true);
                    spyOn(component, 'fn1');

                    component.whenVisible('fn1', ['a']);
                    component.whenVisible('fn1', ['b']);

                    component.show();
                    expect(component.fn1.callCount).toBe(1);
                    expect(component.fn1).toHaveBeenCalledWith('b');
                });

                it("should be able to clear a call", function() {
                    makeCls(true);
                    spyOn(component, 'fn1');
                    spyOn(component, 'fn2');

                    component.whenVisible('fn1');
                    component.whenVisible('fn2');

                    component.clearWhenVisible('fn1');

                    component.show();
                    expect(component.fn1).not.toHaveBeenCalled();
                    expect(component.fn2.callCount).toBe(1);
                });

                it("should not be called when a component in another hierarchy is shown", function() {
                    var other = new Ext.Component({
                        renderTo: Ext.getBody(),
                        hidden: true
                    });
                    makeCls(true);
                    spyOn(component, 'fn1');

                    component.whenVisible('fn1');
                    other.show();
                    expect(component.fn1).not.toHaveBeenCalled();

                    other.destroy();
                });

                it("should only fire once", function() {
                    makeCls(true);
                    spyOn(component, 'fn1');

                    component.whenVisible('fn1');
                    component.show();

                    expect(component.fn1.callCount).toBe(1);
                    component.fn1.reset();
                    component.hide();
                    component.show();
                    expect(component.fn1).not.toHaveBeenCalled();
                });
            });

            describe("nested", function() {
                describe("showing container", function() {
                    describe("when component is hidden", function() {
                        it("should not fire", function() {
                            makeCls(true, true);
                            ct = new Ext.Container({
                                renderTo: Ext.getBody(),
                                hidden: true,
                                items: [component]
                            });
                            spyOn(component, 'fn1');

                            component.whenVisible('fn1');
                            ct.show();

                            expect(component.fn1).not.toHaveBeenCalled();
                        });
                    });

                    describe("when component is visible", function() {
                        it("should fire", function() {
                            makeCls(false, true);
                            ct = new Ext.Container({
                                renderTo: Ext.getBody(),
                                hidden: true,
                                items: [component]
                            });
                            spyOn(component, 'fn1');

                            component.whenVisible('fn1');
                            ct.show();

                            expect(component.fn1.callCount).toBe(1);
                        });
                    });
                });

                describe("showing component", function() {
                    describe("when container is hidden", function() {
                        it("should not fire", function() {
                            makeCls(true, true);
                            ct = new Ext.Container({
                                renderTo: Ext.getBody(),
                                hidden: true,
                                items: [component]
                            });
                            spyOn(component, 'fn1');

                            component.whenVisible('fn1');
                            component.show();

                            expect(component.fn1).not.toHaveBeenCalled();
                        });
                    });

                    describe("when container is visible", function() {
                        it("should fire", function() {
                            makeCls(true, true);
                            ct = new Ext.Container({
                                renderTo: Ext.getBody(),
                                items: [component]
                            });
                            spyOn(component, 'fn1');

                            component.whenVisible('fn1');
                            component.show();

                            expect(component.fn1.callCount).toBe(1);
                        });
                    });
                });
            });

            describe("when a component in another hierarchy is shown", function() {
                it("should not fire", function() {
                    makeCls(false, true);
                    ct = new Ext.Container({
                        renderTo: Ext.getBody(),
                        hidden: true,
                        items: [component]
                    });
                    spyOn(component, 'fn1');

                    component.whenVisible('fn1');

                    var other = new Ext.Component({
                        renderTo: Ext.getBody(),
                        hidden: true
                    });

                    other.show();
                    expect(component.fn1).not.toHaveBeenCalled();
                    other.destroy();
                    ct.show();
                    expect(component.fn1.callCount).toBe(1);
                });
            });
        });
    });
});
