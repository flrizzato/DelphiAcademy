topSuite("Ext.menu.RadioItem", ['Ext.menu.Menu', 'Ext.app.ViewModel'], function() {
    var nameHolder, menu, radios;

    function makeMenu(menuCfg) {
        nameHolder = Ext.widget({
            xtype: 'panel',
            nameHolder: true,
            items: Ext.apply({
                xtype: 'menu',
                hidden: false,
                items: [
                    makeItem({
                        text: 'Desktop',
                        checked: true,
                        value: 'desktop'
                    }, menuCfg),
                    makeItem({
                        text: 'Tablet',
                        checked: false,
                        value: 'tablet'
                    }, menuCfg),
                    makeItem({
                        text: 'Phone',
                        checked: false,
                        value: 'phone'
                    }, menuCfg)
                ]
            }, menuCfg)
        });

        menu = nameHolder.down('menu');

        radios = {
            desktop: menu.items.items[0],
            tablet: menu.items.items[1],
            phone: menu.items.items[2]
        };
    }

    function makeItem (cfg, menuCfg) {
        var defaults = menuCfg && menuCfg.defaults;

        if (!defaults || !defaults.group) {
            cfg.group = 'ui-choice';
        }

        return cfg;
    }

    afterEach(function() {
        nameHolder = menu = radios = Ext.destroy(nameHolder);
    });

    function clickIt(item, event) {
        jasmine.fireMouseEvent(item.checkboxElement, event || 'click');
    }

    describe("initial config", function() {
        describe("normal", function () {
            beforeEach(function () {
                makeMenu();
            });

            it("should have the only one checked", function () {
                expect(radios.desktop.getChecked()).toBe(true);
                expect(radios.tablet.getChecked()).toBeFalsy();
                expect(radios.phone.getChecked()).toBeFalsy();
            });

            describe("rendered", function () {
                it("should have itemEl as ariaEl", function () {
                    expect(radios.desktop.ariaEl).toBe(radios.desktop.checkboxElement);
                });

                // TODO: Alex. Implement ARIA roles
                xit("should have menuitemcheckbox role", function () {
                    expect(radios.desktop).toHaveAttr('role', 'menuitemcheckbox');
                });

                it("should not have aria-label", function () {
                    expect(radios.desktop).not.toHaveAttr('aria-label');
                });

                describe("aria-checked", function () {
                    it("should be only one checked", function () {
                        expect(radios.desktop.ariaEl).toHaveAttr('aria-checked', 'true');
                        expect(radios.tablet.ariaEl).not.toHaveAttr('aria-checked', 'true');
                        expect(radios.phone.ariaEl).not.toHaveAttr('aria-checked', 'true');
                    });
                });
            });
        });
    });

    describe('menu defaults', function () {
        it('should create radio item with group in defaults', function () {
            makeMenu({
                defaults: {
                    group: 'foo'
                }
            });

            expect(radios.desktop instanceof Ext.menu.RadioItem).toBe(true);
            expect(radios.phone instanceof Ext.menu.RadioItem).toBe(true);
            expect(radios.tablet instanceof Ext.menu.RadioItem).toBe(true);

            expect(radios.desktop.getGroup()).toBe('foo');
            expect(radios.phone.getGroup()).toBe('foo');
            expect(radios.tablet.getGroup()).toBe('foo');
        });

        it('should create radio item with name in defaults', function () {
            makeMenu({
                defaults: {
                    name: 'foo'
                }
            });

            expect(radios.desktop instanceof Ext.menu.RadioItem).toBe(true);
            expect(radios.phone instanceof Ext.menu.RadioItem).toBe(true);
            expect(radios.tablet instanceof Ext.menu.RadioItem).toBe(true);
        });

        it('should create radio item with xtype in defaults', function () {
            makeMenu({
                defaults: {
                    xtype: 'menuradioitem'
                }
            });

            expect(radios.desktop instanceof Ext.menu.RadioItem).toBe(true);
            expect(radios.phone instanceof Ext.menu.RadioItem).toBe(true);
            expect(radios.tablet instanceof Ext.menu.RadioItem).toBe(true);
        });
    });

    describe("setChecked", function() {
        it("should set the checked state on the component", function() {
            makeMenu();

            radios.tablet.setChecked(true);

            expect(radios.desktop.getChecked()).toBe(false);
            expect(radios.tablet.getChecked()).toBe(true);
            expect(radios.phone.getChecked()).toBe(false);

            expect(radios.desktop.ariaEl).toHaveAttr('aria-checked', 'false');
            expect(radios.tablet.ariaEl).toHaveAttr('aria-checked', 'true');
            expect(radios.phone.ariaEl).toHaveAttr('aria-checked', 'false');

            radios.tablet.setChecked(false);

            // Unchecking radio items is always permitted programatically
            expect(radios.desktop.getChecked()).toBe(false);
            expect(radios.tablet.getChecked()).toBe(false);
            expect(radios.phone.getChecked()).toBe(false);

            expect(radios.desktop.ariaEl).toHaveAttr('aria-checked', 'false');
            expect(radios.tablet.ariaEl).toHaveAttr('aria-checked', 'false');
            expect(radios.phone.ariaEl).toHaveAttr('aria-checked', 'false');
        });

        it("should not throw an error when the group config is after the name config", function() {
            menu = nameHolder = Ext.widget({
                xtype: 'menu',
                listeners: {
                    beforeshow: function(menu) {
                        menu.add([{
                            xtype: 'menuradioitem',
                            group: 'uiChoice',
                            text: 'Desktop',
                            name: 'desktop'
                        }, {
                            checked: true,      // When processing this,
                                                // It must pull through the group into the name.
                                                // This is a pathological use case, but KS
                                                // configures its theme menu this way.
                            xtype: 'menuradioitem',
                            text: 'Tablet',
                            name: 'tablet',
                            group: 'uiChoice'
                        }, {
                            xtype: 'menuradioitem',
                            group: 'uiChoice',
                            text: 'Phone',
                            checked: false,
                            name: 'phone'
                        }]);
                    }
                }
            });

            expect(function() {
                menu.showAt(0, 0);
            }).not.toThrow();
        });
    });

    describe("pointer interaction", function() {
        it("should set the checked state on the component", function() {
            makeMenu();

            clickIt(radios.tablet);

            expect(radios.desktop.getChecked()).toBe(false);
            expect(radios.tablet.getChecked()).toBe(true);
            expect(radios.phone.getChecked()).toBe(false);

            expect(radios.desktop.ariaEl).toHaveAttr('aria-checked', 'false');
            expect(radios.tablet.ariaEl).toHaveAttr('aria-checked', 'true');
            expect(radios.phone.ariaEl).toHaveAttr('aria-checked', 'false');

            clickIt(radios.tablet);

            // Unchecking radio items is invalid. No change must take place
            expect(radios.desktop.getChecked()).toBe(false);
            expect(radios.tablet.getChecked()).toBe(true);
            expect(radios.phone.getChecked()).toBe(false);

            expect(radios.desktop.ariaEl).toHaveAttr('aria-checked', 'false');
            expect(radios.tablet.ariaEl).toHaveAttr('aria-checked', 'true');
            expect(radios.phone.ariaEl).toHaveAttr('aria-checked', 'false');
        });
    });

    describe('Binding the groupValue', function() {
        var panel,
            viewModel,
            button,
            menu,
            mobileItem,
            desktopItem;

        afterEach(function() {
            Ext.destroy(panel);
        });

        it('should publish the groups on check', function() {
            panel = Ext.create('Ext.Panel', {
                viewModel: {
                    data: {
                        platformMenu: {
                            uiType: 'Mobile'
                        }
                    }
                },
                height: 400,
                width: 600,
                renderTo: document.body,
                items: [{
                    xtype: 'button',
                    id: 'the-button',
                    bind: '{platformMenu.uiType}',
                    menu: {
                        id: 'the-menu',
                        bind: {
                            groups: '{platformMenu}'
                        },
                        items: [{
                            id: 'mobile-item',
                            group: 'uiType',
                            text: 'Móvil',
                            value: 'Mobile'
                        }, {
                            id: 'desktop-item',
                            group: 'uiType',
                            text: 'Escritorio',
                            value: 'Desktop'
                        }]
                    }
                }]
            });
            button = Ext.getCmp('the-button');

            viewModel = panel.getViewModel();
            viewModel.notify();

            // The uiType should have been flushed to the Button
            expect(button.getText()).toBe('Mobile');

            // Tap the button to show the menu
            Ext.testHelper.tap(button.ariaEl);
            viewModel.notify();

            menu = Ext.getCmp('the-menu');
            mobileItem = Ext.getCmp('mobile-item');
            desktopItem = Ext.getCmp('desktop-item');

            // Menu should be shown
            expect(menu.isVisible()).toBe(true);

            // The uiType should have been flushed down into the RadioItems
            expect(mobileItem.getChecked()).toBe(true);
            expect(desktopItem.getChecked()).toBe(false);

            // Toggle to "Desktop"
            Ext.testHelper.tap(desktopItem.ariaEl);
            viewModel.notify();

            // The uiType should have been flushed to the RadioItems and Button
            expect(mobileItem.getChecked()).toBe(false);
            expect(desktopItem.getChecked()).toBe(true);

            expect(button.getText()).toBe('Desktop');
        });

        it("should publish the groups on check using text when there's no value", function() {
            panel = Ext.create('Ext.Panel', {
                viewModel: {
                    data: {
                        platformMenu: {
                            uiType: 'Mobile'
                        }
                    }
                },
                height: 400,
                width: 600,
                renderTo: document.body,
                items: [{
                    xtype: 'button',
                    id: 'the-button',
                    bind: '{platformMenu.uiType}',
                    menu: {
                        id: 'the-menu',
                        bind: {
                            groups: '{platformMenu}'
                        },
                        items: [{
                            id: 'mobile-item',
                            group: 'uiType',
                            text: 'Mobile'
                        }, {
                            id: 'desktop-item',
                            group: 'uiType',
                            text: 'Desktop'
                        }]
                    }
                }]
            });
            button = Ext.getCmp('the-button');

            viewModel = panel.getViewModel();
            viewModel.notify();

            // The uiType should have been flushed to the Button
            expect(button.getText()).toBe('Mobile');

            // Tap the button to show the menu
            Ext.testHelper.tap(button.ariaEl);
            viewModel.notify();

            menu = Ext.getCmp('the-menu');
            mobileItem = Ext.getCmp('mobile-item');
            desktopItem = Ext.getCmp('desktop-item');

            // Menu should be shown
            expect(menu.isVisible()).toBe(true);

            // The uiType should have been flushed down into the RadioItems
            expect(mobileItem.getChecked()).toBe(true);
            expect(desktopItem.getChecked()).toBe(false);

            // Toggle to "Desktop"
            Ext.testHelper.tap(desktopItem.ariaEl);
            viewModel.notify();

            // The uiType should have been flushed to the RadioItems and Button
            expect(mobileItem.getChecked()).toBe(false);
            expect(desktopItem.getChecked()).toBe(true);

            expect(button.getText()).toBe('Desktop');
        });
    });

    describe('The groupchange event', function() {
        var menu,
            mobileItem,
            desktopItem,
            groupChangeSpy;

        afterEach(function() {
            Ext.destroy(menu);
        });

        it('should fire groupchange on check', function() {
            groupChangeSpy = jasmine.createSpy();

            menu = Ext.create('Ext.menu.Menu', {
                height: 400,
                width: 600,
                id: 'the-menu',
                listeners: {
                    groupchange: groupChangeSpy
                },
                items: [{
                    id: 'mobile-item',
                    group: 'uiType',
                    text: 'Móvil',
                    value: 'Mobile',
                    checked: true
                }, {
                    id: 'desktop-item',
                    group: 'uiType',
                    text: 'Escritorio',
                    value: 'Desktop'
                }]
            });
            menu.showAt(0, 0);

            mobileItem = Ext.getCmp('mobile-item');
            desktopItem = Ext.getCmp('desktop-item');

            // "Mobile" was checked.
            expect(mobileItem.getChecked()).toBe(true);
            expect(desktopItem.getChecked()).toBe(false);

            // Toggle to "Desktop"
            Ext.testHelper.tap(desktopItem.ariaEl);

            // The uiType should have been flushed to the RadioItems and Button
            expect(mobileItem.getChecked()).toBe(false);
            expect(desktopItem.getChecked()).toBe(true);

            // We checked Desktop where Mobile had been checked.
            expect(groupChangeSpy.mostRecentCall.args.slice(0, 4)).toEqual([menu, 'uiType', 'Desktop', 'Mobile']);
        });

        it('should fire groupchange on uncheck', function() {
            groupChangeSpy = jasmine.createSpy();

            menu = Ext.create('Ext.menu.Menu', {
                height: 400,
                width: 600,
                id: 'the-menu',
                listeners: {
                    groupchange: groupChangeSpy
                },
                items: [{
                    id: 'mobile-item',
                    group: 'uiType',
                    text: 'Móvil',
                    value: 'Mobile',
                    checked: true,
                    allowUncheck: true
                }, {
                    id: 'desktop-item',
                    group: 'uiType',
                    text: 'Escritorio',
                    value: 'Desktop',
                    allowUncheck: true
                }]
            });
            menu.showAt(0, 0);

            mobileItem = Ext.getCmp('mobile-item');
            desktopItem = Ext.getCmp('desktop-item');

            // "Mobile" was checked.
            expect(mobileItem.getChecked()).toBe(true);
            expect(desktopItem.getChecked()).toBe(false);

            // Toggle to none checked
            Ext.testHelper.tap(mobileItem.ariaEl);

            // The uiType should have been flushed to the RadioItems and Button
            expect(mobileItem.getChecked()).toBe(false);
            expect(desktopItem.getChecked()).toBe(false);

            // We unchecked Mobile
            expect(groupChangeSpy.mostRecentCall.args.slice(0, 4)).toEqual([menu, 'uiType', null, 'Mobile']);
        });
    });
});
