xtopSuite("Ext.dataview.listswiper.Item", [
    'Ext.layout.HBox'
], function() {
    var helper = Ext.testHelper,
        views = {};

    function acquireView(config) {
        var view = Ext.create(Ext.merge({
            xtype: 'listswiperitem',
            renderTo: Ext.getBody()
        }, config || {}));

        views[view.getId()] = view;
        view.refresh();
        return view;
    }

    afterEach(function() {
        Ext.Object.getValues(views).forEach(function(view) {
            var id = view.getId();
            delete views[id];
            view.destroy();
        });
    });

    describe('sanity checks', function() {
        it('should not be initially active or pending', function() {
            var item = acquireView();

            expect(item.getActive()).toBeFalsy();
            expect(item.getPending()).toBeFalsy();
            expect(item.el).not.toHaveCls('x-active');
            expect(item.el).not.toHaveCls('x-pending');
        });

        it('should correctly apply cls when active and pending', function() {
            var item = acquireView({
                active: true,
                pending: true
            });

            expect(item.getActive()).toBeTruthy();
            expect(item.getPending()).toBeTruthy();
            expect(item.el).toHaveCls('x-active');
            expect(item.el).toHaveCls('x-pending');
        });
    });

    describe("component events", function() {
        it('should fire the commit event when pending and calling commit()', function() {
            var spy = jasmine.createSpy(),
                item = acquireView({
                    listeners: {
                        commit: spy
                    }
                });

            item.commit();

            expect(spy).not.toHaveBeenCalled();

            item.setPending(true);
            item.commit();

            expect(spy.callCount).toBe(1);
            expect(spy.calls[0].args[0]).toBe(item);
        });

        it('should fire the cancel event when pending and calling cancel()', function() {
            var spy = jasmine.createSpy(),
                item = acquireView({
                    listeners: {
                        cancel: spy
                    }
                });

            item.cancel();

            expect(spy).not.toHaveBeenCalled();

            item.setPending(true);
            item.cancel();

            expect(spy.callCount).toBe(1);
            expect(spy.calls[0].args[0]).toBe(item);
        });

        it('should fire the dismiss event when pending and calling dismiss()', function() {
            var spy = jasmine.createSpy(),
                item = acquireView({
                    listeners: {
                        dismiss: spy
                    }
                });

            item.dismiss();

            expect(spy).not.toHaveBeenCalled();

            item.setPending(true);
            item.dismiss();

            expect(spy.callCount).toBe(1);
            expect(spy.calls[0].args[0]).toBe(item);
        });
    });

    describe('dismiss configuration', function() {
        it('should dismiss pending action on tap if dismissOnTap is true', function() {
            var spy = jasmine.createSpy(),
                item = acquireView({
                    dismissOnTap: true,
                    pending: true,
                    listeners: {
                        dismiss: spy
                    }
                });

            expect(spy).not.toHaveBeenCalled();

            helper.tap(item.el);

            // [BUG] helper.tap() triggers mouseup and click events, causing the item
            // to be dismissed twice, so can't expect exactly one event fired.
            expect(spy).toHaveBeenCalled();
            expect(spy.calls[0].args[0]).toBe(item);
        });

        it('should NOT dismiss pending action on tap if dismissOnTap is false', function() {
            var spy = jasmine.createSpy(),
                item = acquireView({
                    dismissOnTap: false,
                    pending: true,
                    listeners: {
                        dismiss: spy
                    }
                });

            expect(spy).not.toHaveBeenCalled();

            helper.tap(item.el);

            expect(spy).not.toHaveBeenCalled();
        });
    });

    describe('undo customization and interactions', function() {
        it('should accept a custom undo component', function() {
            Ext.define('CustomListSwiperItemUndo', {
                extend: 'Ext.Button',
                xtype: 'customlistswiperitemundo'
            });

            var item = acquireView({
                    pending: true,
                    undo: {
                        xtype: 'customlistswiperitemundo',
                        iconCls: 'x-foo',
                        text: 'Foo'
                    }
                });

            expect(Ext.getClassName(item.getUndo())).toBe('CustomListSwiperItemUndo');
            expect(item.getUndo().getIconCls()).toBe('x-foo');
            expect(item.getUndo().getText()).toBe('Foo');

            Ext.undefine('CustomListSwiperItemUndo');
        });

        it('should cancel pending action when undo is tapped', function() {
            var spies = {
                    cancel: jasmine.createSpy('cancel'),
                    others: jasmine.createSpy('others')
                },
                item = acquireView({
                    pending: true,
                    listeners: {
                        dismiss: spies.others,
                        cancel: spies.cancel,
                        commit: spies.others
                    }
                });

            // there is tap interactions on the listswiperitem (container) and the undo button,
            // so spying on other events to make sure that only the "cancel" event is fired.
            expect(spies.cancel).not.toHaveBeenCalled();
            expect(spies.others).not.toHaveBeenCalled();

            helper.tap(item.getUndo().el);

            expect(spies.cancel).toHaveBeenCalled();
            expect(spies.cancel.calls[0].args[0]).toBe(item);
            expect(spies.others).not.toHaveBeenCalled();
        });
    });
});
