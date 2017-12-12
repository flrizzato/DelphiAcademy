/* global Ext, expect */

topSuite("Ext.Sheet", [
    'Ext.layout.VBox'
], function() {
    var menus = [],
        sheet;

    jasmine.usesViewport();  // setup in beforeAll, teardown in afterAll

    function createSheet(config) {
        sheet = Ext.create('Ext.Sheet', config || {});
        return sheet;
    }

    function destroyMenu(menu) {
        if (menu) {
            menu.destroy();
        }
        menu = null;
    }

    afterEach(function() {
        if (menus.length) {
            while (menus.length) {
                destroyMenu(menus.pop());
            }
        }
        else if (sheet) {
            sheet.destroy();
        }

        sheet = null;
    });

    function getMenuCfg() {
        return {
            defaultType: 'button',
            layout: {
                type: 'vbox',
                align: 'stretch'
            },
            items: [
                {
                    text: 'Item 1'
                }, {
                    text: 'Item 2'
                }, {
                    text: 'Item 3'
                }
            ]
        };
    }

    function makeMenu(cfg) {
        var menu = createSheet(Ext.applyIf(getMenuCfg(), cfg));
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

    function getMenu(side) {
        return Ext.Viewport.getMenus()[side];
    }

    describe("Construction/Destruction, change side", function() {
        it("should destroy", function() {
            var menu = makeMenu();
            menu.destroy();
        });

        it("should destroy floatWrap", function() {
            var menu = makeMenuSide('left', false);
            menu.show();
            expect(menu.floatWrap).not.toBeFalsy();
            menu.destroy();
            expect(menu.floatWrap).toBeFalsy();
        });

        it("should destroy side", function() {
            var menu = makeMenuSide('left');
            menu.destroy();
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
                return !left.isTranslating && left.isHidden() === true;
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
                return !menu.isTranslating;
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
                Ext.Viewport.hideOtherMenus('left');
            });
            waitsFor(function()  {
                return left.isHidden() && right.isHidden() && top.isHidden() && bottom.isHidden();
            });
        });
    });

    describe('deprecated', function() {
        describe("configurations", function() {
            describe("stretchX", function() {
                it("should set the floatable property", function() {
                    createSheet({stretchX: true});

                    expect(sheet.getLeft()).toEqual(0);
                    expect(sheet.getRight()).toEqual(0);
                });
            });

            describe("stretchY", function() {

                it("should set the floatable property", function() {
                    createSheet({stretchY: true});

                    expect(sheet.getTop()).toEqual(0);
                    expect(sheet.getBottom()).toEqual(0);
                });

            });
        });
    });

    describe("configurations", function() {
        describe('positioning', function() {
            it('should not center positioned Sheets', function() {
                createSheet({
                    centered: true,
                    right: 0,
                    stretchY: true,
                    width: 400
                });
                spyOn(sheet, 'translate');
                sheet.show();

                // Wait for animation to finish
                waitsFor(function() {
                    return !sheet.activeAnimation;
                });
                runs(function() {
                    // translate must be all 0px
                    expect(sheet.translate.mostRecentCall.args.slice(0,2)).toEqual([0, 0]);
                });
            });
        });
    });
});
