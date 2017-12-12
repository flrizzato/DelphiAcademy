/* global Ext, jasmine, expect, xit */

topSuite("Ext.menu.Item", ['Ext.menu.Menu', 'Ext.app.ViewModel', 'Ext.app.ViewController'], function() {
    var itNotTouch = jasmine.supportsTouch ? xit : it,
        menu, item;

    function makeMenu(itemCfg, menuCfg) {
        if (!Ext.isArray(itemCfg)) {
            itemCfg = [itemCfg];
        }

        menu = Ext.widget(Ext.apply({
            mouseLeaveDelay: 0,
            xtype: 'menu',
            items: itemCfg
        }, menuCfg));
        menu.showAt(0, 0);

        item = menu.items.getAt(0);
    }

    afterEach(function() {
        Ext.destroy(menu, item);
        menu = item = null;
    });

    function clickItem(theItem, doClick) {
        theItem = theItem || item;
        jasmine.fireMouseEvent(theItem.ariaEl.dom, 'click');
    }

    describe('on click', function () {
        describe("click event/handler", function() {
            var spy;

            beforeEach(function() {
                spy = jasmine.createSpy();
            });

            afterEach(function() {
                spy = null;
            });

            describe("handler", function() {
                it("should fire the handler", function() {
                    makeMenu({
                        text: 'Foo',
                        handler: spy
                    });
                    clickItem();
                    expect(spy.callCount).toBe(1);
                });

                it("should not fire when disabled", function() {
                    makeMenu({
                        text: 'Foo',
                        handler: spy,
                        disabled: true
                    });
                    clickItem();
                    expect(spy).not.toHaveBeenCalled();
                });

                it("should pass the item and the event object", function() {
                    makeMenu({
                        text: 'Foo',
                        handler: spy
                    });
                    clickItem();

                    var args = spy.mostRecentCall.args;
                    expect(args[0]).toBe(item);
                    expect(args[1] instanceof Ext.event.Event).toBe(true);
                });

                it("should default the scope to the item", function() {
                    makeMenu({
                        text: 'Foo',
                        handler: spy
                    });
                    clickItem();
                    expect(spy.mostRecentCall.object).toBe(item);
                });

                it("should used the passed scope", function() {
                    var o = {};
                    makeMenu({
                        text: 'Foo',
                        scope: o,
                        handler: spy
                    });
                    clickItem();
                    expect(spy.mostRecentCall.object).toBe(o);
                });

                it("should be able to route the handler to a view controller", function() {
                    var ctrl = new Ext.app.ViewController();
                    ctrl.onFoo = spy;
                    makeMenu({
                        text: 'Foo',
                        handler: 'onFoo'
                    }, {
                        controller: ctrl
                    });
                    clickItem();
                    expect(spy.callCount).toBe(1);
                });

                it("should have the menu hidden when the handler fires with hideOnClick: true", function() {
                    var visible;
                    makeMenu({
                        text: 'Foo',
                        handler: spy.andCallFake(function() {
                            visible = menu.isVisible();
                        })
                    });
                    clickItem();
                    expect(visible).toBe(false);
                });

                it("should fire the handler after the click event", function() {
                    var order = [];
                    makeMenu({
                        text: 'Foo',
                        listeners: {
                            click: function() {
                                order.push('click');
                            }
                        },
                        handler: function() {
                            order.push('handler');
                        }
                    });
                    clickItem();
                    expect(order).toEqual(['click', 'handler']);
                });

                it("should not call the handler if the click event returns false", function() {
                    makeMenu({
                        text: 'Foo',
                        listeners: {
                            click: function() {
                                return false;
                            }
                        },
                        handler: spy
                    });
                    clickItem();
                    expect(spy).not.toHaveBeenCalled();
                });

                it("it should not fire the callback if the menu is destroyed in the click event", function() {
                    makeMenu({
                        text: 'Foo',
                        listeners: {
                            click: function() {
                                menu.destroy();
                            }
                        },
                        handler: spy
                    });
                    clickItem();
                    expect(spy).not.toHaveBeenCalled();
                });
            });

            describe("click event", function() {
                it("should fire the click event", function() {
                    makeMenu({
                        text: 'Foo',
                        listeners: {
                            click: spy
                        }
                    });
                    clickItem();
                    expect(spy.callCount).toBe(1);
                });

                it("should not fire when disabled", function() {
                    makeMenu({
                        text: 'Foo',
                        listeners: {
                            click: spy
                        },
                        disabled: true
                    });
                    clickItem();
                    expect(spy).not.toHaveBeenCalled();
                });

                it("should pass the item and the event object", function() {
                    makeMenu({
                        text: 'Foo',
                        listeners: {
                            click: spy
                        }
                    });
                    clickItem();

                    var args = spy.mostRecentCall.args;
                    expect(args[0]).toBe(item);
                    expect(args[1] instanceof Ext.event.Event).toBe(true);
                });

                it("should have the menu hidden when the handler fires with hideOnClick: true", function() {
                    var visible;
                    makeMenu({
                        text: 'Foo',
                        listeners: {
                            click: spy.andCallFake(function() {
                                visible = menu.isVisible();
                            })
                        }
                    });
                    clickItem();
                    expect(visible).toBe(false);
                });
            });
        });

        describe("hideOnClick", function() {
            it("should hide the menu with hideOnClick: true", function() {
                makeMenu({
                    text: 'Foo',
                    hideOnClick: true
                });
                clickItem();
                expect(menu.isVisible()).toBe(false);
            });

            it("should not hide the menu with hideOnClick: false", function() {
                makeMenu({
                    text: 'Foo',
                    hideOnClick: false
                });
                clickItem();
                expect(menu.isVisible()).toBe(true);
            });

            describe("hierarchy", function() {
                function expand(item) {
                    item.activated = true;
                    item.expandMenu(null, 0);

                    return item.getMenu();
                }
                it("should hide a parent menu", function() {
                    makeMenu({
                        text: 'Foo',
                        menu: {
                            items: [{
                                text: 'Bar'
                            }]
                        }
                    });

                    var item = menu.items.first(),
                        subMenu = item.getMenu();

                    expand(item);
                    clickItem(subMenu.items.first());
                    expect(subMenu.isVisible()).toBe(false);
                    expect(menu.isVisible()).toBe(false);
                });

                it("should hide all parent menus", function() {
                    makeMenu({
                        text: 'Foo',
                        menu: {
                            items: [{
                                text: 'Bar',
                                menu: {
                                    items: [{
                                        text: 'Baz',
                                        menu: {
                                            items: [{
                                                text: 'Qux'
                                            }]
                                        }
                                    }]
                                }
                            }]
                        }
                    });

                    var sub1 = expand(menu.items.first()),
                        sub2 = expand(sub1.items.first()),
                        sub3 = expand(sub2.items.first());

                    clickItem(sub3.items.first());

                    expect(sub3.isVisible()).toBe(false);
                    expect(sub2.isVisible()).toBe(false);
                    expect(sub1.isVisible()).toBe(false);
                    expect(menu.isVisible()).toBe(false);
                });
            });
        });

        describe('href property', function () {
            // Note that the specs were failing in FF 24 without the waitsFor().
            // Note that it's necessary to set the activeItem and focusedItem to test the API!
            var menuItem;

            afterEach(function () {
                menuItem = null;
                location.hash = '';
            });

            it('should follow the target', function () {
                makeMenu([{
                    text: 'menu item one',
                    href: '#ledzep'
                }, {
                    text: 'menu item two'
                }]);
                item.focus();
                
                waitsForFocus(item);
                
                runs(function() {
                    clickItem(item, true);
                });

                waitsFor(function () {
                    return location.hash === '#ledzep';
                }, 'timed out waiting for hash to change', 1000);

                runs(function () {
                    expect(location.hash).toBe('#ledzep');
                });
            });

            it("should be able to assign the href target", function() {
                makeMenu([{
                    text: 'Foo',
                    href: '#someAnchor',
                    target: '_blank'
                }]);
                expect(item.bodyElement.dom.target).toBe('_blank');
            });
            
            // TODO This test does not work properly due to events being translated
            // TODO: Reinstate this for touch platforms when https://sencha.jira.com/browse/EXT-4 is fixed.
            // We cannot now preventDefault on native click events on touch because of click event synthesis.
            xit('should not follow the target link if the click listener stops the event', function () {
                var hashValue = Ext.isIE ? '#' : '';

                makeMenu([{
                    text: 'menu item one',
                    href: '#motley',
                    listeners: {
                        click: function (cmp, e) {
                            e.preventDefault();
                        }
                    }
                }, {
                    text: 'menu item two'
                }]);

                menu.activeItem = menu.focusedItem = item;
                clickItem(item, Ext.isIE9m);

                waitsFor(function () {
                    return location.hash === hashValue;
                }, 'timed out waiting for hash to change', 1000);

                runs(function () {
                    expect(location.hash).toBe(hashValue);
                });
            });
        });
    });

    describe('disabled', function () {
        describe('when item has an href config', function () {
            it('should stop the event', function () {
                makeMenu({
                    disabled: true,
                    href: '#menu'
                });
                clickItem();
                expect(location.hash).not.toBe('menu');
            });
        });

        itNotTouch("should gain focus and activate on mouseover", function() {
            makeMenu([{
                text: 'Foo',
                disabled: true
            }]);
            var item = menu.items.getAt(0);
            jasmine.fireMouseEvent(item.bodyElement, 'mouseover');
            waitsFor(function() {
                return item.containsFocus === true;
            }, "Never focused");
            runs(function() {
                expect(item.containsFocus).toBe(true);
            });
        });

        describe("submenu", function() {
            itNotTouch("should not show a submenu on mouseover", function() {
                makeMenu([{
                    text: 'Foo',
                    disabled: true,
                    menuExpandDelay: 0,
                    menu: {
                        items: [{
                            text: 'Sub1'
                        }]
                    }
                }]);

                var item = menu.items.getAt(0),
                    sub = item.getMenu();

                jasmine.fireMouseEvent(item.bodyElement, 'mouseover');
                waitsFor(function() {
                    return item.containsFocus === true;
                }, "Never focused");
                runs(function() {
                    expect(sub.rendered).toBe(false);
                });
            });
        });
    });

    describe('when destroying', function () {
        var m;

        beforeEach(function () {
            makeMenu([{
                clearPrototypeOnDestroy: false,
                text: 'The Office, UK',
                menu: {
                    itemId: 'office-submenu'
                }
            }]);
            m = menu.down('#office-submenu');
            item.destroy();
        });

        it('should destroy its menu', function () {
            expect(m.destroyed).toBe(true);
        });

        it('should cleanup its menu reference', function () {
            expect(item.getMenu()).toBe(null);
        });
    });

    describe("binding", function() {
        it("should be able to bind properties higher up in the hierarchy", function() {
            var vm = new Ext.app.ViewModel({
                data: {
                    title: 'someTitle'
                }
            });
            makeMenu({
                text: 'Foo',
                menu: {
                    bind: {
                        title: '{title}'
                    }
                }
            }, {
                viewModel: vm
            });
            var subMenu = item.getMenu();
            // Render to force the VM to fire
            subMenu.show();
            vm.notify();
            expect(subMenu.getTitle()).toBe('someTitle');
        });
    });
    
    describe("ARIA", function() {
        describe("simple", function() {
            beforeEach(function() {
                makeMenu({
                    text: 'foo'
                });
                
                menu.show();
            });
            
            it("should have proper ariaEl", function() {
                expect(item.ariaEl.dom.tagName).toBe('A');
            });

            // TODO: Alex. Implement ARIA roles
            xit("should have menuitem role", function() {
                expect(item).toHaveAttr('role', 'menuitem');
            });
            
            it("should not have aria-haspopup", function() {
                expect(item).not.toHaveAttr('aria-haspopup');
            });
            
            it("should not have aria-owns", function() {
                expect(item).not.toHaveAttr('aria-owns');
            });
        });
        
        describe("plain", function() {
            beforeEach(function() {
                makeMenu({
                    text: 'plain',
                    plain: true
                });
                
                menu.show();
            });
            
            it("should have proper ariaEl", function() {
                expect(item.ariaEl.dom.tagName).toBe('A');
            });

            // TODO: Alex. Implement ARIA roles
            xit("should have menuitem role", function() {
                expect(item).toHaveAttr('role', 'menuitem');
            });

            it("should not have aria-haspopup", function() {
                expect(item).not.toHaveAttr('aria-haspopup');
            });
            
            it("should not have aria-owns", function() {
                expect(item).not.toHaveAttr('aria-owns');
            });
        });
        
        describe("with submenu", function() {
            function makeSuite(menuCreator) {
                describe("at config time", function() {
                    beforeEach(function() {
                        makeMenu({
                            text: 'Root Level',
                            menu: menuCreator()
                        });
                        menu.show();
                    });

                    it("should have aria-haspopup", function() {
                        expect(item).toHaveAttr('aria-haspopup', 'true');
                    });

                    it("should have aria-owns", function() {
                        expect(item).toHaveAttr('aria-owns', item.getMenu().id);
                    });

                    it("should have the arrow class", function() {
                        expect(item).toHaveCls(Ext.baseCSSPrefix + 'has-arrow');
                    });

                    it("should show the submenu", function() {
                        item.expandMenu();
                        expect(item.getMenu().isVisible()).toBe(true);
                    });
                });

                describe("via setMenu", function() {
                    beforeEach(function() {
                        makeMenu({
                            text: 'Root Level'
                        });
                    });

                    describe("before rendering", function() {
                        beforeEach(function() {
                            item.setMenu(menuCreator());
                            menu.show();
                        });

                        it("should have aria-haspopup", function() {
                            expect(item).toHaveAttr('aria-haspopup', 'true');
                        });

                        it("should have aria-owns", function() {
                            expect(item).toHaveAttr('aria-owns', item.getMenu().id);
                        });

                        it("should have the arrow class", function() {
                            expect(item).toHaveCls(Ext.baseCSSPrefix + 'has-arrow');
                        });

                        it("should show the submenu", function() {
                            item.expandMenu();
                            expect(item.getMenu().isVisible()).toBe(true);
                        });
                    });

                    describe("after rendering", function() {
                        beforeEach(function() {
                            menu.show();

                            item.setMenu(menuCreator());
                        });

                        it("should have aria-haspopup", function() {
                            expect(item).toHaveAttr('aria-haspopup', 'true');
                        });

                        it("should have aria-owns", function() {
                            expect(item).toHaveAttr('aria-owns', item.getMenu().id);
                        });

                        it("should have the arrow class", function() {
                            expect(item).toHaveCls(Ext.baseCSSPrefix + 'has-arrow');
                        });

                        it("should show the submenu", function() {
                            item.expandMenu();
                            expect(item.getMenu().isVisible()).toBe(true);
                        });
                    });
                });

                describe("removing via setMenu", function() {
                    var child;

                    beforeEach(function() {
                        makeMenu({
                            text: 'Root Level',
                            menu: menuCreator()
                        });
                        child = item.getMenu();
                    });

                    afterEach(function() {
                        child = null;
                    });

                    describe("before rendering", function() {
                        it("should not have aria-haspopup", function() {
                            item.setMenu(null);
                            expect(item).not.toHaveAttr('aria-haspopup');
                        });

                        it("should have no aria-owns", function() {
                            item.setMenu(null);
                            expect(item).not.toHaveAttr('aria-owns');
                        });

                        it("should not have the arrow cls", function() {
                            item.setMenu(null);
                            expect(item).not.toHaveCls(Ext.baseCSSPrefix + 'has-arrow');
                        });

                        it("should destroy the menu", function() {
                            item.setMenu(null);
                            expect(child.destroyed).toBe(true);
                        });

                        it("should not destroy with destroyMenu: false", function() {
                            item.destroyMenu = false;
                            item.setMenu(null);
                            expect(child.destroyed).toBe(false);
                            child.destroy();
                        });
                    });

                    describe("after rendering", function() {
                        beforeEach(function() {
                            menu.show();
                        });

                        it("should not have aria-haspopup", function() {
                            item.setMenu(null);
                            expect(item).not.toHaveAttr('aria-haspopup');
                        });

                        it("should have no aria-owns", function() {
                            item.setMenu(null);
                            expect(item).not.toHaveAttr('aria-owns');
                        });

                        it("should not have the arrow cls", function() {
                            item.setMenu(null);
                            expect(item).not.toHaveCls(Ext.baseCSSPrefix + 'has-arrow');
                        });

                        it("should destroy the menu", function() {
                            item.setMenu(null);
                            expect(child.destroyed).toBe(true);
                        });

                        it("should not destroy with destroyMenu: false", function() {
                            item.destroyMenu = false;
                            item.setMenu(null);
                            expect(child.destroyed).toBe(false);
                            child.destroy();
                        });
                    });
                });
            }

            describe("with a menu config", function() {
                makeSuite(function() {
                    return {
                        items: [{
                            text: 'Sub Menu Item'
                        }]
                    };
                });
            });

            describe("with a menu instance", function() {
                makeSuite(function() {
                    return new Ext.menu.Menu({
                        items: [{
                            text: 'Sub Menu Item'
                        }]
                    });
                });
            });
        });
    });
});
