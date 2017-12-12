topSuite('Ext.field.trigger.Menu', ['Ext.field.Text', 'Ext.viewport.Default', 'Ext.ActionSheet', 'Ext.Mask', 'Ext.ActionSheet'], function () {
    var field, trigger;

    function createField (menuTriggerCfg, cfg) {
        cfg = cfg || {};

        if (menuTriggerCfg) {
            if (!cfg.triggers) {
                cfg.triggers = {}
            }

            if (!menuTriggerCfg.type) {
                menuTriggerCfg.type = 'menu';
            }

            cfg.triggers.menu = menuTriggerCfg;
        }

        field = Ext.factory(cfg, Ext.field.Text);

        var triggers = field.getTriggers();

        trigger = triggers.menu;

        return field;
    }

    afterEach(function () {
        field = trigger = Ext.destroy(field);
    });

    describe('create menu', function () {
        it('should be lazily created', function () {
            createField({
                menu: [{
                    text: 'Foo'
                }]
            });

            expect(trigger.getConfig('menu', null, true)).toBe(null);

            var menu = trigger.getMenu();

            expect(menu).not.toBe(null);
        });
    });

    describe('menu', function() {
        beforeEach(function() {
            Ext.Viewport = new Ext.viewport.Default();
        });

        afterEach(function() {
            //we need to destroy the menu before Ext.Viewport is
            trigger.setMenu(null);

            Ext.Viewport = Ext.destroy(Ext.Viewport);
        });

        it('should accept menu config object', function() {
            createField({
                menu: {
                    id: 'bar',
                    items: [{ text: 'foo' }]
                }
            });

            expect(trigger.getMenu().id).toBe('bar');
        });

        it('should accept menu config object and create the specified xtype', function() {
            createField({
                menu: {
                    xtype: 'actionsheet',
                    id: 'bar',
                    items: [{ text: 'foo' }]
                }
            });

            expect(trigger.getMenu().$className).toBe('Ext.ActionSheet');
        });

        it('should accept array of items', function() {
            createField({
                menu: [{
                    text: 'foo'
                }, {
                    text: 'bar'
                }]
            });

            expect(trigger.getMenu().getItems().getCount()).toBe(2);
        });

        it('should show menu on trigger click', function () {
            createField({
                menu: [{
                    text: 'foo'
                }, {
                    text: 'bar'
                }]
            });

            trigger.onClick();

            expect(trigger.getMenu().getHidden()).toBe(false);
        });

        describe('action sheet', function () {
            it('should create an action sheet', function () {
                createField({
                    menu: {
                        xtype: 'actionsheet',
                        items: [{ xtype: 'button', text: 'foo' }]
                    }
                });

                expect(trigger.getMenu().$className).toBe('Ext.ActionSheet');
            });

            it('should show the action sheet on button click', function () {
                createField({
                    menu: {
                        xtype: 'actionsheet',
                        side: 'right',
                        items: [{ text: 'foo' }]
                    }
                });

                trigger.onClick();

                var menu = trigger.getMenu();

                expect(menu.getDisplayed()).toBe(true);

                waitsFor(function () {
                    return !menu.isAnimating;
                });

                runs(function () {
                    menu.setDisplayed(false);
                });

                waitsFor(function () {
                    return !menu.isAnimating;
                });
            });
        });
    });
});
