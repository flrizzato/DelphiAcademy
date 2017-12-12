/*global expect, jasmine, Ext, spyOn, xdescribe, describe, it */

topSuite('Ext.ActionSheet', ['Ext.layout.VBox'], function() {
    var menus = [];

    jasmine.usesViewport();

    function getMenuCfg() {
        return [
            {
                text: 'Item 1'
            }, {
                text: 'Item 2'
            }, {
                text: 'Item 3'
            }
        ];
    }

    function makeMenu(cfg) {
        var menu = new Ext.ActionSheet(Ext.applyIf({
            items: getMenuCfg(),
            width: 400
        }, cfg));
        menus.push(menu);
        return menu;
    }

    function makeMenuSide(side, reveal) {
        reveal = Boolean(reveal);
        return makeMenu({
            side: side,
            reveal: reveal,
            cover: !reveal
        });
    }

    function destroyMenu(menu) {
        if (menu) {
            menu.destroy();
        }
        menu = null;
    }

    function getMenu(side) {
        return Ext.Viewport.getMenus()[side];
    }

    afterEach(function() {
        Ext.destroy(menus);
        menus.length = 0;
    });

    describe("Construction/Destruction, change side", function() {
        it("should destroy", function() {
            var menu = makeMenu();
            menu.destroy();
        });
        it("should destroy side", function() {
            var menu = makeMenuSide('left');
            menu.destroy();
        });
        it("should destroy floatWrap", function() {
            var menu = makeMenuSide('left', false);
            menu.show();
            expect(menu.floatWrap).not.toBeFalsy();
            menu.destroy();
            expect(menu.floatWrap).toBeFalsy();
        });
        it("should removeMenu", function() {
            Ext.Viewport.removeMenu('left');
            Ext.Viewport.removeMenu('right');
            Ext.Viewport.removeMenu('top');
            Ext.Viewport.removeMenu('bottom');
        });
        it("should change side", function() {
            var menu = makeMenuSide('left');
            expect(getMenu('left')).toBe(menu);
            menu.setSide('right');
            expect(getMenu('left')).toBeFalsy();
            expect(getMenu('right')).toBe(menu);
        });
    });

    describe("No Ext.Viewport API", function() {
        it("should silently execute setDisplayed", function() {
            var menu = makeMenuSide('left');
            menu.setDisplayed(true);

            waitsFor(function () {
                return !menu.isTranslating;
            });

            runs(function () {
                expect(menu.el.isVisible(true)).toBe(true);
                menu.setDisplayed(false);
            });

            waitsFor(function () {
                return !menu.isTranslating;
            });
        });

        it("should obey side config", function() {
            var left = makeMenuSide('left'),
                right = makeMenuSide('right'),
                top = makeMenuSide('top'),
                bottom = makeMenuSide('bottom');

            expect(getMenu('left')).toBe(left);
            expect(getMenu('right')).toBe(right);
            expect(getMenu('top')).toBe(top);
            expect(getMenu('bottom')).toBe(bottom);
            expect(left.isHidden()).toBe(true);
            expect(right.isHidden()).toBe(true);
            expect(top.isHidden()).toBe(true);
            expect(bottom.isHidden()).toBe(true);
        });
        it("should show if displayed set", function() {
            var left = makeMenuSide('left'),
                right = makeMenuSide('right'),
                top = makeMenuSide('top'),
                bottom = makeMenuSide('bottom');

            expect(getMenu('left')).toBe(left);
            expect(getMenu('right')).toBe(right);
            expect(getMenu('top')).toBe(top);
            expect(getMenu('bottom')).toBe(bottom);
            expect(left.isHidden()).toBe(true);
            expect(right.isHidden()).toBe(true);
            expect(top.isHidden()).toBe(true);
            expect(bottom.isHidden()).toBe(true);
            left.setDisplayed(true);
            waitsFor(function() {
                return !left.isTranslating;
            });
            runs(function() {
                expect(left.isHidden()).toBe(false);
            });
        });
        it("should hide if displayed cleared", function() {
            var left = makeMenuSide('left'),
                right = makeMenuSide('right'),
                top = makeMenuSide('top'),
                bottom = makeMenuSide('bottom');

            expect(getMenu('left')).toBe(left);
            expect(getMenu('right')).toBe(right);
            expect(getMenu('top')).toBe(top);
            expect(getMenu('bottom')).toBe(bottom);
            expect(left.isHidden()).toBe(true);
            expect(right.isHidden()).toBe(true);
            expect(top.isHidden()).toBe(true);
            expect(bottom.isHidden()).toBe(true);
            left.setDisplayed(true);
            waitsFor(function() {
                return !left.isTranslating;
            });
            runs(function() {
                left.setDisplayed(false);
            });
            waitsFor(function() {
                return left.isHidden();
            });
        });
    });

    describe("Ext.Viewport API", function() {
        it("should setMenu", function() {
            var menu = makeMenu();
            Ext.Viewport.setMenu(menu, {
                side: 'left',
                reveal: true
            });
            expect(getMenu('left')).toBe(menu);
        });
        it("should allow hideMenu with no menus", function() {
            Ext.Viewport.hideMenu('left');
            Ext.Viewport.hideMenu('right');
            Ext.Viewport.hideMenu('top');
            Ext.Viewport.hideMenu('bottom');
        });
        it("should showMenu left", function() {
            var menu = makeMenu();
            Ext.Viewport.setMenu(menu, {
                side: 'left',
                reveal: true
            });
            expect(menu.isHidden()).toBe(true);
            Ext.Viewport.showMenu('left');
            expect(menu.isHidden()).toBe(false);
        });
        it("should hideMenu left", function() {
            var menu = makeMenu();
            Ext.Viewport.setMenu(menu, {
                side: 'left',
                reveal: false
            });
            expect(menu.isHidden()).toBe(true);
            Ext.Viewport.showMenu('left');
            waitsFor(function() {
                return menu.isTranslating === false;
            });
            runs(function() {
                expect(menu.isHidden()).toBe(false);
                Ext.Viewport.hideMenu('left', false);
            });
            waitsFor(function() {
                return menu.isHidden();
            });
        });
        it("should showMenu right", function() {
            var menu = makeMenu();
            Ext.Viewport.setMenu(menu, {
                side: 'right',
                reveal: true
            });
            expect(menu.isHidden()).toBe(true);
            Ext.Viewport.showMenu('right');
            expect(menu.isHidden()).toBe(false);
        });
        it("should hideMenu right", function() {
            var menu = makeMenu();
            Ext.Viewport.setMenu(menu, {
                side: 'right',
                reveal: false
            });
            expect(menu.isHidden()).toBe(true);
            Ext.Viewport.showMenu('right');
            waitsFor(function() {
                return !menu.isTranslating;
            });
            runs(function() {
                expect(menu.isHidden()).toBe(false);
                Ext.Viewport.hideMenu('right', false);
            });
            waitsFor(function() {
                return menu.isHidden();
            });
        });
        it("should showMenu top", function() {
            var menu = makeMenu();
            Ext.Viewport.setMenu(menu, {
                side: 'top',
                reveal: true
            });
            expect(menu.isHidden()).toBe(true);
            Ext.Viewport.showMenu('top');
            expect(menu.isHidden()).toBe(false);
        });
        it("should hideMenu top", function() {
            var menu = makeMenu();
            Ext.Viewport.setMenu(menu, {
                side: 'top',
                reveal: false
            });
            expect(menu.isHidden()).toBe(true);
            Ext.Viewport.showMenu('top');
            waitsFor(function() {
                return !menu.isTranslating;
            });
            runs(function() {
                expect(menu.isHidden()).toBe(false);
                Ext.Viewport.hideMenu('top', false);
            });
            waitsFor(function() {
                return menu.isHidden();
            });
        });
        it("should showMenu bottom", function() {
            var menu = makeMenu();
            Ext.Viewport.setMenu(menu, {
                side: 'bottom',
                reveal: true
            });
            expect(menu.isHidden()).toBe(true);
            Ext.Viewport.showMenu('bottom');
            expect(menu.isHidden()).toBe(false);
        });
        it("should hideMenu bottom", function() {
            var menu = makeMenu();
            Ext.Viewport.setMenu(menu, {
                side: 'bottom',
                reveal: false
            });
            expect(menu.isHidden()).toBe(true);
            Ext.Viewport.showMenu('bottom');
            waitsFor(function() {
                return !menu.isTranslating;
            });
            runs(function() {
                expect(menu.isHidden()).toBe(false);
                Ext.Viewport.hideMenu('bottom', false);
            });
            waitsFor(function() {
                return menu.isHidden();
            });
        });
        it("should hideAllMenus left", function() {
            var menu = makeMenu();
            Ext.Viewport.setMenu(menu, {
                side: 'left',
                reveal: false
            });
            expect(menu.isHidden()).toBe(true);
            Ext.Viewport.showMenu('left');
            waitsFor(function() {
                return !menu.isTranslating;
            });
            runs(function() {
                expect(menu.isHidden()).toBe(false);
                Ext.Viewport.hideAllMenus('left', false);
            });
            waitsFor(function() {
                return menu.isHidden();
            });
        });
        it("should allow multiple menus", function() {
            function make(side) {
                var m = makeMenu();
                if (side === 'left' || side === 'right') {
                    m.setWidth(400);
                } else {
                    m.setHeight(400);
                }

                Ext.Viewport.setMenu(m, {
                    side: side,
                    reveal: false
                });
                return m;
            }
            var left = make('left'),
                right = make('right'),
                top = make('top'),
                bottom = make('bottom');

            expect(getMenu('left')).toBe(left);
            expect(getMenu('right')).toBe(right);
            expect(getMenu('top')).toBe(top);
            expect(getMenu('bottom')).toBe(bottom);
        });
        it("should hideOther menus when a menu is shown", function() {
            function make(side) {
                var m = makeMenu();
                if (side === 'left' || side === 'right') {
                    m.setWidth(400);
                } else {
                    m.setHeight(400);
                }

                Ext.Viewport.setMenu(m, {
                    side: side,
                    reveal: false
                });

                return m;
            }
            var left = make('left'),
                right = make('right'),
                top = make('top'),
                bottom = make('bottom');

            Ext.Viewport.showMenu('left');
            Ext.Viewport.showMenu('right');

            waitsFor(function() {
                return !left.isTranslating && !right.isTranslating && !top.isTranslating && !bottom.isTranslating;
            });

            runs(function() {
                expect(left.isHidden()).toBe(true);
                expect(right.isHidden()).toBe(false);
                expect(top.isHidden()).toBe(true);
                expect(bottom.isHidden()).toBe(true);
            });

            waitsFor(function()  {
                return !right.isHidden() && left.isHidden() && top.isHidden() && bottom.isHidden();
            });

            runs(function () {
                right.hide();
            });

            waitsFor(function() {
                return !left.isTranslating && !right.isTranslating && !top.isTranslating && !bottom.isTranslating;
            });
        });
    });

    describe('changing from floated to inline and vice versa', function() {
        it('should be able to move between inline and floated', function() {
            var menu = makeMenu({
                    side: 'left',
                    reveal: true,
                    cover: false
                }),
                VP = Ext.Viewport;

            VP.showMenu('left');

            waitsFor(function() {
                return !menu.isTranslating;
            });

            runs(function() {
                // It's reveal, so it must be in the document body
                expect(menu.el.dom.parentNode).toBe(document.body);
                expect(menu).toHaveCls(menu.floatingCls);
                expect(menu).not.toHaveCls(menu.floatedCls);

                // Hide the menu immediately
                VP.hideMenu('left', false);

                VP.setMenu(menu, {
                    side: 'left',
                    reveal: false,
                    cover: true
                });
                VP.showMenu('left');
            });

            waitsFor(function() {
                return !menu.isTranslating;
            });

            runs(function() {
                // Now it's cover, so it's a floated component in the floatRoot
                expect(menu.el.dom.parentNode).toBe(menu.floatWrap.dom);
                expect(menu.floatWrap.dom.parentNode).toBe(Ext.floatRoot.dom);
                expect(menu).not.toHaveCls(menu.floatingCls);
                expect(menu).toHaveCls(menu.floatedCls);

                // Hide the menu immediately
                VP.hideMenu('left', false);

                VP.setMenu(menu, {
                    side: 'left',
                    reveal: true,
                    cover: false
                });
                VP.showMenu('left');
            });


            waitsFor(function() {
                return !menu.isTranslating;
            });

            runs(function() {
                // It's reveal, so it must be in the document body
                expect(menu.el.dom.parentNode).toBe(document.body);
                expect(menu).toHaveCls(menu.floatingCls);
                expect(menu).not.toHaveCls(menu.floatedCls);

                // Hide the menu immediately
                VP.hideMenu('left', false);

                VP.setMenu(menu, {
                    side: 'left',
                    reveal: false,
                    cover: true
                });
                VP.showMenu('left');
            });
            waitsFor(function() {
                return !menu.isTranslating;
            });

            runs(function() {
                // Now it's cover, so it's a floated component in the floatRoot
                expect(menu.el.dom.parentNode).toBe(menu.floatWrap.dom);
                expect(menu.floatWrap.dom.parentNode).toBe(Ext.floatRoot.dom);
                expect(menu).not.toHaveCls(menu.floatingCls);
                expect(menu).toHaveCls(menu.floatedCls);
            });
        });
    });

    describe('ComponentQuery', function () {
        function makeSuite(reveal) {
            describe(reveal ? 'reveal' : 'cover', function () {
                it('should return one instance', function () {
                    var menu = makeMenu({
                            cover: !reveal,
                            //will force it to add to Ext.Viewport
                            displayed: true,
                            reveal: reveal
                        }),
                        results = Ext.ComponentQuery.query('actionsheet');

                    expect(results.length).toBe(1);
                    expect(results[0]).toBe(menu);
                });

                it('should return one instance with nested selector', function () {
                    var menu = makeMenu({
                            cover: !reveal,
                            //will force it to add to Ext.Viewport
                            displayed: true,
                            reveal: reveal
                        }),
                        results = Ext.ComponentQuery.query('viewport actionsheet');

                    expect(results.length).toBe(1);
                    expect(results[0]).toBe(menu);
                });

                it('should return one instance with child selector', function () {
                    var menu = makeMenu({
                            cover: !reveal,
                            //will force it to add to Ext.Viewport
                            displayed: true,
                            reveal: reveal
                        }),
                        results = Ext.ComponentQuery.query('viewport > actionsheet');

                    expect(results.length).toBe(1);
                    expect(results[0]).toBe(menu);
                });
            });
        }

        makeSuite();
        makeSuite(true);
    });
});
