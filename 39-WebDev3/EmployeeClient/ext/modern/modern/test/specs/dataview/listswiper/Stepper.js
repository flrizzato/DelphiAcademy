xtopSuite("Ext.dataview.listswiper.Stepper", [
    'Ext.dataview.listswiper.ListSwiper',
    'Ext.dataview.List', 'Ext.data.ArrayStore', 'Ext.layout.Fit'
], function() {
    var helper = Ext.testHelper,
        views = {};

    function acquireView (config) {
        var view = Ext.create(Ext.merge({
            xtype: 'list',
            renderTo: Ext.getBody(),
            itemTpl: '{value}',
            height: 256,
            width: 256,
            plugins: {
                type: 'listswiper',
                widget: {
                    xtype: 'listswiperstepper'
                }
            },
            store: {
                data: Array.apply(null, Array(128)).map(function(value, index) {
                    return {value: index}
                })
            }
        }, config || {}));

        views[view.getId()] = view;
        view.refresh();
        return view;
    }

    function swipe (item, direction, distance, maintain) {
        var el = item.el || item.element,
            dx = (distance || 0) * (direction === 'left' ? -1 : 1),
            x = direction === 'left' ? el.getWidth() : 0,
            y = item.el.getHeight() / 2;

        helper.touchStart(el, {x: x, y: y});
        helper.touchMove(el, {x: x + dx, y: y});
        if (!maintain) {
            helper.touchEnd(el, {x: x + dx, y: y});
        }
    }

    function swipeEnd (item) {
        var swiperItem = item.$swiperWidget;
        helper.touchEnd(item.el || item.element);

        // Force the swiper item to end without animation
        swiperItem.finalize(false);
    }

    afterEach(function() {
        Ext.Object.getValues(views).forEach(function(view) {
            var id = view.getId();
            delete views[id];
            view.destroy();
        });
    });

    describe("interaction states", function() {
        it('should activate the correct action depending on the direction', function() {
            var list = acquireView({
                    plugins: {
                        left: [{
                            text: 'Foo',
                            key: 'foo'
                        }],
                        right: [{
                            text: 'Bar',
                            key: 'bar'
                        }]
                    }
                }),
                item = list.getItemAt(0), swiperItem;

            [{dir: 'left', dist: 32, state: 'peek'},
                {dir: 'left', dist: 128, state:'active'},
                {dir: 'right', dist: 32, state: 'peek'},
                {dir: 'right', dist: 128, state: 'active'}
            ].forEach(function(expected) {
                var dir = expected.dir,
                    side = dir === 'left' ? 'right' : 'left',
                    key = dir === 'left' ? 'bar' : 'foo', state, step;

                swipe(item, dir, expected.dist, true);
                swiperItem = item.$swiperWidget;

                expect(swiperItem).not.toBeNull();

                state = swiperItem.getState();
                step = swiperItem.getStep();
                expect(state).toBe(expected.state);
                expect(step.key).toBe(key);
                expect(swiperItem.el).toHaveCls('x-side-' + side);
                expect(swiperItem.el).not.toHaveCls('x-side-' + (side === 'right' ? 'left' : 'right'));

                swipeEnd(item);
            });
        });

        it('should activate the correct action depending on the threshold', function() {
            var list = acquireView({
                    plugins: {

                        left: [
                            {
                                threshold: '64px',
                                key: 'foo'
                            },
                            {
                                threshold: 128,
                                key: 'bar'
                            },
                            {
                                threshold: '75%',
                                key: 'bla'
                            }
                        ]
                    }
                }),
                item = list.getItemAt(0);


            [{dist: 16, key: 'foo', state: 'peek'},
                {dist: 96, key: 'foo', state: 'active'},
                {dist: 160, key: 'bar', state: 'active'},
                {dist: 224, key: 'bla', state: 'active'}
            ].forEach(function(expected) {
                var dir = 'left',
                    side = dir === 'left' ? 'right' : 'left',
                    swiperItem, state, step;

                swipe(item, dir, expected.dist, true);

                swiperItem = item.$swiperWidget;

                expect(swiperItem).not.toBeNull();

                state = swiperItem.getState();
                step = swiperItem.getStep();

                expect(state).toBe(expected.state);
                expect(step.key).toBe(expected.key);
                expect(item.el).toHaveCls('x-side-' + side);
                expect(item.el).not.toHaveCls('x-side-' + (side === 'left' ? 'right' : 'left'));

                ['foo', 'bar', 'bla'].forEach(function(k) {
                    if (expected.key === k) {
                        expect(item.el).toHaveCls('x-swipe-action-' + k);
                    } else {
                        expect(item.el).not.toHaveCls('x-swipe-action-' + k);
                    }
                });

                swipeEnd(item);
            });
        });

        it('should enter the pending state when an undoable action is swiped', function() {
            var list = acquireView({
                    plugins: {
                        left: [
                            {
                                key: 'foo',
                                undoable: true
                            }
                        ]
                    }
                }),
                plugin = list.getPlugin(),
                item = list.getItemAt(0),
                context = plugin.getContext(item);

            expect(context).toBeNull();

            swipe(item, 'left', 16);

            context = plugin.getContext(item);
            expect(context).not.toBeNull();
            expect(context.active).toBeFalsy();
            expect(context.pending).toBeFalsy();
            expect(context.step.key).toBe('foo');

            swipe(item, 'left', 128);

            context = plugin.getContext(item);
            expect(context).not.toBeNull();
            expect(context.active).toBeTruthy();
            expect(context.pending).toBeTruthy();
            expect(context.step.key).toBe('foo');
        });
    });

    describe("action events", function() {
        it('should activate the correct action depending on the direction', function() {
            var spy = jasmine.createSpy(),
                list = acquireView({
                    plugins: {
                        left: [
                            {
                                threshold: 128,
                                key: 'foo'
                            },
                            {
                                threshold: 128,
                                key: 'bar'
                            }
                        ]
                    },
                    listeners: {
                        itemaction: spy
                    }
                }),
                store = list.getStore();

            swipe(list.getItemAt(0), 'left', 64);
            swipe(list.getItemAt(0), 'left', 192);
            swipe(list.getItemAt(3), 'left', 192);
            swipe(list.getItemAt(5), 'right', 64);
            swipe(list.getItemAt(5), 'right', 192);
            swipe(list.getItemAt(6), 'right', 192);

            expect(spy.callCount).toBe(4);

            [{index: 0, key: 'foo'},
                {index: 3, key: 'foo'},
                {index: 5, key: 'bar'},
                {index: 6, key: 'bar'}
            ].forEach(function(expected, i) {
                expect(spy.calls[i].args[0]).toBe(list);
                expect(spy.calls[i].args[1]).toBe(expected.index);
                expect(spy.calls[i].args[2]).toBe(store.getAt(expected.index));
                expect(spy.calls[i].args[3]).toBe(expected.key);
            });
        });

        it('should activate the correct action depending on the threshold', function() {
            var spy = jasmine.createSpy(),
                list = acquireView({
                    plugins: {
                        left: [
                            {
                                key: 'foo',
                                threshold: '64px'
                            },
                            {
                                key: 'bar',
                                threshold: 128
                            },
                            {
                                key: 'bla',
                                direction: 'left',
                                threshold: '75%'
                            }
                        ]
                    },
                    listeners: {
                        itemaction: spy
                    }
                }),
                store = list.getStore(),
                item = list.getItemAt(3);

            swipe(item, 'left', 32);
            swipe(item, 'left', 96);
            swipe(item, 'left', 160);
            swipe(item, 'left', 224);

            expect(spy.callCount).toBe(3);

            [{key: 'foo'},
                {key: 'bar'},
                {key: 'bla'}
            ].forEach(function(expected, i) {
                expect(spy.calls[i].args[0]).toBe(list);
                expect(spy.calls[i].args[1]).toBe(3);
                expect(spy.calls[i].args[2]).toBe(store.getAt(3));
                expect(spy.calls[i].args[3]).toBe(expected.key);
            });
        });

        it('should NOT trigger action events when an undoable event is swiped', function() {
            var spy = jasmine.createSpy(),
                list = acquireView({
                    plugins: {
                        left: [
                            {
                                key: 'foo',
                                threshold: 64
                            },
                            {
                                key: 'bar',
                                threshold: 128,
                                undoable: true
                            },
                            {
                                key: 'bla',
                                threshold: 128,
                                undoable: true
                            }
                        ]
                    },
                    listeners: {
                        itemaction: spy
                    }
                }),
                store = list.getStore(),
                item = list.getItemAt(3);

            swipe(item, 'left', 32);
            swipe(item, 'left', 96);
            swipe(item, 'left', 160);
            swipe(item, 'right', 160);

            expect(spy.callCount).toBe(1);

            expect(spy.calls[0].args[0]).toBe(list);
            expect(spy.calls[0].args[1]).toBe(3);
            expect(spy.calls[0].args[2]).toBe(store.getAt(3));
            expect(spy.calls[0].args[3]).toBe('foo');
        });
    });

    describe('dismiss configuration', function() {
        it('should execute pending actions on dismiss', function() {
            var spy = jasmine.createSpy(),
                list = acquireView({
                    plugins: {
                        left: [
                            {
                                key: 'foo',
                                undoable: true
                            }
                        ],
                        right: [
                            {
                                key: 'bar',
                                undoable: true
                            }
                        ]
                    },
                    listeners: {
                        itemaction: spy
                    }
                }),
                plugin = list.getPlugin(),
                store = list.getStore();

            swipe(list.getItemAt(1), 'left', 192);
            swipe(list.getItemAt(3), 'right', 192);
            swipe(list.getItemAt(4), 'left', 192);

            expect(spy.callCount).toBe(0);

            plugin.dismissAll();

            expect(spy.callCount).toBe(3);

            [{index: 1, key: 'foo'},
                {index: 3, key: 'bar'},
                {index: 4, key: 'foo'}
            ].forEach(function(expected, i) {
                expect(spy.calls[i].args[0]).toBe(list);
                expect(spy.calls[i].args[1]).toBe(expected.index);
                expect(spy.calls[i].args[2]).toBe(store.getAt(expected.index));
                expect(spy.calls[i].args[3]).toBe(expected.key);
            });
        });

        it('should dismiss pending actions on scroll if dismissOnScroll is true', function(done) {
            var spy = jasmine.createSpy(),
                list = acquireView({
                    plugins: {
                        dismissOnScroll: true,
                        left: [
                            {
                                key: 'foo',
                                undoable: true
                            }
                        ],
                        right: [
                            {
                                key: 'bar',
                                undoable: true
                            }
                        ]
                    },
                    listeners: {
                        itemaction: spy
                    }
                }),
                scrollable = list.getScrollable();

            swipe(list.getItemAt(1), 'left', 192);
            swipe(list.getItemAt(3), 'right', 192);
            swipe(list.getItemAt(4), 'left', 192);

            expect(spy.callCount).toBe(0);

            scrollable.on('scrollend', function() {
                expect(spy.callCount).toBe(3);
                done();
            });

            scrollable.scrollBy(0, 128);
        });

        it('should NOT dismiss pending actions on scroll if dismissOnScroll is false', function(done) {
            var spy = jasmine.createSpy(),
                list = acquireView({
                    plugins: {
                        dismissOnScroll: false,
                        left: [
                            {
                                key: 'foo',
                                undoable: true
                            }
                        ],
                        right: [
                            {
                                key: 'bar',
                                undoable: true
                            }
                        ]
                    },
                    listeners: {
                        itemaction: spy
                    }
                }),
                scrollable = list.getScrollable();

            swipe(list.getItemAt(1), 'left', 192);
            swipe(list.getItemAt(3), 'right', 192);
            swipe(list.getItemAt(4), 'left', 192);

            expect(spy.callCount).toBe(0);

            scrollable.on('scrollend', function() {
                expect(spy.callCount).toBe(0);
                done();
            });

            scrollable.scrollBy(0, 128);
        });

        it('should dismiss pending actions after dismissDelay milliseconds if greater than 0', function() {
            var spy = jasmine.createSpy(),
                list = acquireView({
                    plugins: {
                        dismissDelay: 250,
                        left: [
                            {
                                key: 'foo',
                                undoable: true
                            }
                        ],
                        right: [
                            {
                                key: 'bar',
                                undoable: true
                            }
                        ]
                    },
                    listeners: {
                        itemaction: spy
                    }
                });

            swipe(list.getItemAt(1), 'left', 192);
            swipe(list.getItemAt(3), 'right', 192);
            swipe(list.getItemAt(4), 'left', 192);

            expect(spy.callCount).toBe(0);
            
            waits(200);
            
            runs(function() {
                expect(spy.callCount).toBe(0);
            });
            
            waits(100);
            
            runs(function() {
                expect(spy.callCount).toBe(3);
            });
        });

        it('should NOT dismiss pending actions if dismissDelay is 0', function() {
            var spy = jasmine.createSpy(),
                list = acquireView({
                    plugins: {
                        dismissDelay: 0,
                        left: [
                            {
                                key: 'foo',
                                undoable: true
                            }
                        ],
                        right: [
                            {
                                key: 'bar',
                                undoable: true
                            }
                        ]
                    },
                    listeners: {
                        itemaction: spy
                    }
                });

            swipe(list.getItemAt(1), 'left', 192);
            swipe(list.getItemAt(3), 'right', 192);
            swipe(list.getItemAt(4), 'left', 192);
            
            waits(500)
            
            runs(function() {
                expect(spy.callCount).toBe(0);
            });
        });
    });

    describe("widget customization and interactions", function() {
        it('should handle global widget configuration', function() {
            Ext.define('CustomListSwiperItem', {
                extend: 'Ext.dataview.listswiper.Item',
                xtype: 'customlistswiperitem'
            });

            var list = acquireView({
                    plugins: {
                        dismissDelay: 0,
                        left: [
                            {
                                key: 'foo',
                                direction: 'left'
                            }
                        ],
                        widget: {
                            xtype: 'customlistswiperitem',
                            iconCls: 'x-foo',
                            text: 'FooBar'
                        }
                    }
                }),
                item = list.getItemAt(1),
                context;

            swipe(item, 'left', 192, true);

            context = list.getPlugin().getContext(item);
            expect(context).not.toBeNull();
            expect(Ext.getClassName(context.widget)).toBe('CustomListSwiperItem');
            expect(context.widget.getIconCls()).toBe('x-foo');
            expect(context.widget.getText()).toBe('FooBar');

            swipeEnd(item);

            Ext.undefine('CustomListSwiperItem');
        });

        it('should handle per action widget configuration', function() {
            var list = acquireView({
                    plugins: {
                        dismissDelay: 0,
                        left: [
                            {
                                key: 'foo',
                                iconCls: 'x-foo',
                                text: 'Foo'
                            }
                        ],
                        right: [
                            {
                                key: 'bar',
                                iconCls: 'x-bar',
                                text: 'Bar'
                            }
                        ]
                    }
                }),
                plugin = list.getPlugin(),
                item = list.getItemAt(1),
                context;

            swipe(item, 'left', 192, true);

            context = plugin.getContext(item);
            expect(context).not.toBeNull();
            expect(context.widget.getIconCls()).toBe('x-foo');
            expect(context.widget.getText()).toBe('Foo');

            swipe(item, 'right', 192, true);

            context = plugin.getContext(item);
            expect(context).not.toBeNull();
            expect(context.widget.getIconCls()).toBe('x-bar');
            expect(context.widget.getText()).toBe('Bar');

            swipeEnd(item);
        });

        it('should commit the pending action', function() {
            var spy = jasmine.createSpy(),
                list = acquireView({
                    plugins: {
                        left: [
                            {
                                key: 'foo',
                                undoable: true
                            }
                        ]
                    },
                    listeners: {
                        itemaction: spy
                    }
                }),
                plugin = list.getPlugin(),
                item = list.getItemAt(3),
                context = plugin.getContext(item);

            swipe(item, 'left', 192);

            context = plugin.getContext(item);
            expect(context).not.toBeNull();
            expect(context.consumed).toBeFalsy();
            expect(context.pending).toBeTruthy();
            expect(spy.callCount).toBe(0);

            context.widget.commit();

            expect(context.consumed).toBeTruthy();
            expect(context.pending).toBeFalsy();
            expect(spy.callCount).toBe(1);
            expect(spy.calls[0].args[1]).toBe(3);
            expect(spy.calls[0].args[3]).toBe('foo');
        });

        it('should cancel the pending action', function() {
            var spy = jasmine.createSpy(),
                list = acquireView({
                    plugins: {
                        left: [
                            {
                                key: 'foo',
                                undoable: true
                            }
                        ]
                    },
                    listeners: {
                        itemaction: spy
                    }
                }),
                plugin = list.getPlugin(),
                item = list.getItemAt(3),
                context = plugin.getContext(item);

            swipe(item, 'left', 192);

            context = plugin.getContext(item);
            expect(context).not.toBeNull();
            expect(context.consumed).toBeFalsy();
            expect(context.pending).toBeTruthy();

            context.widget.cancel();

            expect(context.consumed).toBeTruthy();
            expect(context.pending).toBeFalsy();
            expect(spy.callCount).toBe(0);
        });

        it('should dismiss the pending action', function() {
            var spy = jasmine.createSpy(),
                list = acquireView({
                    plugins: {
                        left: [
                            {
                                key: 'foo',
                                undoable: true
                            }
                        ]

                    },
                    listeners: {
                        itemaction: spy
                    }
                }),
                plugin = list.getPlugin(),
                item = list.getItemAt(3),
                context = plugin.getContext(item);

            spyOn(plugin, 'dismiss').andCallThrough();

            swipe(item, 'left', 192);

            context = plugin.getContext(item);
            expect(context).not.toBeNull();
            expect(context.consumed).toBeFalsy();
            expect(context.pending).toBeTruthy();
            expect(plugin.dismiss.callCount).toBe(0);
            expect(spy.callCount).toBe(0);

            context.widget.dismiss();

            expect(plugin.dismiss.callCount).toBe(1);
            expect(spy.callCount).toBe(1);
            expect(spy.calls[0].args[1]).toBe(3);
            expect(spy.calls[0].args[3]).toBe('foo');
        });
    });
});
