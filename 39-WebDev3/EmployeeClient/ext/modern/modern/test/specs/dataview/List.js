topSuite("Ext.dataview.List", [
    'Ext.dataview.ListItem'
], function() {
    var defaultSize = 400,
        list, store, selModel, paintSpy;

    afterEach(function() {
        paintSpy = store = list = Ext.destroy(list);
    });

    function makeData (count, start) {
        var data = [],
            i, id, h;

        start = start || 0;

        count = count || 100;

        for (i = 1; i <= count; ++i) {
            id = i + start;
            h = i % 3 === 0 ? 50 : null;
            data.push({
                id: id,
                name: 'Item' + id,
                height: h,
                heightStyle: h ? h + 'px' : 'auto'
            });
        }

        return data;
    }

    function makeList(config, storeData) {
        list = Ext.create(Ext.merge({
            xtype: 'list',
            renderTo: Ext.getBody(),
            itemTpl: '{name}',
            height: defaultSize,
            width: defaultSize,
            store: {
                type: 'store',
                data: storeData || makeData()
            }
        }, config));
        store = list.getStore();
        selModel = list.getSelectable();
    }

    function getItemYPos(item) {
        var y;

        if (list.itemTranslationMethod === 'cssposition') {
            y = item.element.getLocalY();
        } else {
            y = parseFloat(Ext.testHelper.parseTransform(item.element)[1]);
        }
        return y;
    }

    function scrollAndWait(pos) {
        var scrolled;

        list.getScrollable().scrollTo(null, pos).then(function() {
            scrolled = true;
        });
        waitsFor(function() {
            return scrolled;
        });
    }

    describe("dataMap", function() {
        describe("at the root level", function() {
            beforeEach(function() {
                makeList({
                    itemDataMap: {
                        '#': {
                            cls: 'cls',
                            disabled: 'disabled'
                        }
                    }
                }, [{
                    cls: 'a',
                    disabled: false
                }, {
                    cls: 'b',
                    disabled: true
                }]);
            });

            it("should assign properties to the item itself", function() {
                var item = list.mapToItem(0);
                expect(item.element).toHaveCls('a');
                expect(item.getDisabled()).toBe(false);

                item = list.mapToItem(1);
                expect(item.element).toHaveCls('b');
                expect(item.getDisabled()).toBe(true);
            });

            it("should react to updates", function() {
                list.getStore().getAt(0).set({
                    cls: 'c',
                    disabled: true
                });

                var item = list.mapToItem(0);
                expect(item.element).not.toHaveCls('a');
                expect(item.element).toHaveCls('c');
                expect(item.getDisabled()).toBe(true);
            });
        });

        describe("children", function() {
            beforeEach(function() {
                makeList({
                    itemConfig: {
                        xtype: 'listitem',
                        items: [{
                            xtype: 'component',
                            reference: 'first'
                        }, {
                            xtype: 'component',
                            reference: 'last'
                        }]
                    },
                    itemDataMap: {
                        '#': {
                            cls: 'cls',
                            disabled: 'disabled'
                        }
                    }
                }, [{
                    cls: 'a',
                    disabled: false
                }, {
                    cls: 'b',
                    disabled: true
                }]);
            });

            it("should assign properties to child references", function() {
                it("should assign properties to children", function() {
                    var item = list.mapToItem(0);
                    expect(item.lookup('first').element).toHaveCls('a');
                    expect(item.lookup('last').getDisabled()).toBe(false);

                    item = list.mapToItem(1);
                    expect(item.lookup('first').element).toHaveCls('b');
                    expect(item.lookup('last').getDisabled()).toBe(true);
                });

                it("should react to updates", function() {
                    list.getStore().getAt(0).set({
                        cls: 'c',
                        disabled: true
                    });

                    var item = list.mapToItem(0);
                    expect(item.lookup('first').element).not.toHaveCls('a');
                    expect(item.lookup('first').element).toHaveCls('c');
                    expect(item.lookup('last').getDisabled()).toBe(true);
                });
            });
        });
    });

    describe('Rendered list with loaded store', function() {
        it('should immediately render records', function() {
            makeList({}, [
                    { name: 'foo' },
                    { name: 'bar' }
                ]);

            // Should be two simplelistitems in the List
            var items = list.getItems().items;

            expect(items.length).toBe(2);

            var rec = items[0].getRecord();
            expect(rec.data.name).toBe('foo');

            rec = items[1].getRecord();
            expect(rec.data.name).toBe('bar');
        });
    });

    describe("infinite lists", function() {
        function makeSuiteList (config, data) {
            if (Ext.isArray(config)) {
                data = config;
                config = null;
            }

            config = config || {};
            config.infinite = true;

            makeList(config, data);
        }

        it("should limit itemCount to number of records", function() {
            makeSuiteList([
                { name: 'foo' },
                { name: 'bar' }
            ]);

            var bodyHeight = list.bodyElement.getHeight();

            expect(list.getItemCount()).toBe(2);

            // The bodyElement gets sized to stretch the scroll region
            var scroller = list.getScrollable();
            var size = scroller.getSize();

            expect(bodyHeight).toBe(size.y);
        });

        it('should adjust rendered range due to scroll', function () {
            makeSuiteList();

            var scroller = list.getScrollable();

            scroller.scrollBy(0, list.rowHeight * 20);

            waitsFor(function () {
                // scrolling down 20 rows should trigger a 10 row shift
                return list.getTopRenderedIndex() >= 10;
            });
        });

        it('should auto height using maxHeight', function () {
            makeSuiteList({
                height: null,
                maxHeight: 400
            });

            waitsFor(function () {
                return list.dataItems.length > 0;
            });

            runs(function () {
                expect(list.el.getHeight()).toBe(400);
            });
        });

        it("should redraw correctly with multiple refreshes where the top index doesn't change", function() {
            function expectIds(ids) {
                ids.forEach(function(id, index) {
                    var rec = store.getById(id),
                        item = list.mapToItem(rec);

                    expect(item.getRecord().id).toBe(id);
                    expect(list.mapToViewIndex(item)).toBe(index);
                });
            }

            makeSuiteList(null, makeData(5));
            expectIds([1, 2, 3, 4, 5]);
            store.loadData(makeData(5, 5));
            expectIds([6, 7, 8, 9, 10]);
            store.loadData(makeData(5, 10));
            expectIds([11, 12, 13, 14, 15]);
        });
    });

    describe('Grouped List', function() {
        it('should correctly handle headers when store is cleared', function() {
            makeList({
                grouped: true,
                store: {
                    grouper: {
                        property: 'group'
                    },
                    data: [
                        { group: 'A', name: 'bar' },
                        { group: 'F', name: 'foo' },
                        { group: 'F', name: 'foobar' }
                    ]
                }
            });

            var store = list.getStore();
            var groupInfo = list.groupingInfo;

            expect(store.getCount()).toBe(3);

            expect(groupInfo.headers.indices).toEqual([0, 1]);

            expect(groupInfo.footers.indices).toEqual([0, 2]);

            expect(store.removeAll.bind(store)).not.toThrow();
        });
    });

    describe('scrollToTopOnRefresh', function() {
        it('should not scroll to top when config is false', function() {
            makeList({scrollToTopOnRefresh: false});

            var scroller = list.getScrollable();

            scroller.scrollTo(null, 150);
            var pos = scroller.getPosition();
            expect(pos.y).toBe(150);

            list.refresh();
            pos = scroller.getPosition();
            expect(pos.y).toBe(150);
        });

        it('should not scroll to top when adding a record', function() {
            makeList({scrollToTopOnRefresh: true});

            var scroller = list.getScrollable();

            scroller.scrollTo(null, 150);
            expect(scroller.getPosition().y).toBe(150);

            list.getStore().add({name: 'New item'});
            expect(scroller.getPosition().y).toBe(150);
        });

        it('should not scroll to top when removing a record', function() {
            makeList({scrollToTopOnRefresh: true});

            var scroller = list.getScrollable();

            scroller.scrollTo(null, 150);
            expect(scroller.getPosition().y).toBe(150);

            list.getStore().removeAt(42);
            expect(scroller.getPosition().y).toBe(150);
        });

        it('should not scroll to top when updating a record', function() {
            makeList({scrollToTopOnRefresh: true});

            var scroller = list.getScrollable();

            scroller.scrollTo(null, 150);
            expect(scroller.getPosition().y).toBe(150);

            list.getStore().getAt(42).set('name', 'foobar');
            expect(scroller.getPosition().y).toBe(150);
        });

        it('should scroll to top when the store is refreshed', function() {
            makeList({scrollToTopOnRefresh: true});

            var scroller = list.getScrollable();

            scroller.scrollTo(null, 150);
            expect(scroller.getPosition().y).toBe(150);

            list.getStore().sort('id', 'DESC');
            expect(scroller.getPosition().y).toBe(0);
        });

        it('should scroll to top when calling the refresh() method', function() {
            makeList({scrollToTopOnRefresh: true});

            var scroller = list.getScrollable();

            scroller.scrollTo(null, 150);
            expect(scroller.getPosition().y).toBe(150);

            list.refresh();
            expect(scroller.getPosition().y).toBe(0);
        });

        it('should not scroll to top if the refresh event is prevented', function() {
            makeList({scrollToTopOnRefresh: true});

            var scroller = list.getScrollable();

            scroller.scrollTo(null, 150);
            expect(scroller.getPosition().y).toBe(150);

            list.on('beforerefresh', function() { return false; });
            list.refresh();
            expect(scroller.getPosition().y).toBe(150);
        });
    });

    describe("store changes while hidden", function() {
        var sizeCls = 'x-size-monitors',
            paintCls = 'x-paint-monitor';

        describe("with infinite: false", function() {
            function expectContent() {
                var count = store.getCount(),
                    i, items, item, rec;

                items = Ext.Array.from(list.getRenderTarget().dom.childNodes);
                items = items.filter(function(el) {
                    var cls = el.classList;
                    return !(cls.contains(sizeCls) || cls.contains(paintCls));
                });

                expect(items.length).toBe(count);

                for (i = 0; i < count; ++i) {
                    rec = store.getAt(i);
                    item = list.mapToItem(rec);

                    expect(item.element.dom).toBe(items[i]);
                    expect(item.element.down('.x-innerhtml')).hasHTML(rec.get('name'));
                }
            }

            describe("add", function() {
                it("should render the new record", function() {
                    makeList({
                        infinite: false
                    }, makeData(4));
                    list.hide();
                    store.add({ id: 5, name: 'Item5' });
                    list.show();
                    expectContent();
                });
            });

            describe("remove", function() {
                it("should clear the removed record", function() {
                    makeList({
                        infinite: false
                    }, makeData(4));
                    list.hide();
                    store.removeAt(0);
                    list.show();
                    expectContent();
                });
            });

            describe("update", function() {
                it("should change the item", function() {
                    makeList({
                        infinite: false
                    }, makeData(4));
                    list.hide();
                    store.getAt(0).set('name', 'Foo');
                    list.show();
                    expectContent();
                });
            });

            describe("refresh", function() {
                describe("with scrollToTopOnRefresh: false", function() {
                    it("should retain scroll position", function() {
                        makeList({
                            infinite: false,
                            scrollToTopOnRefresh: false
                        }, makeData(100));

                        var scrollable = list.getScrollable(),
                            pos;

                        // Scroll to end
                        scrollAndWait(10000);
                        runs(function() {
                            pos = scrollable.getPosition().y;
                            list.hide();
                            store.loadData(makeData(100, 100));
                            list.show();
                            expect(scrollable.getPosition().y).toBe(pos);
                            expectContent();
                        });
                    });
                });

                describe("with scrollToTopOnRefresh: true", function() {
                    it("should scroll to the top", function() {
                        makeList({
                            infinite: false,
                            scrollToTopOnRefresh: true
                        }, makeData(100));

                        var scrollable = list.getScrollable();

                        // Scroll to end
                        scrollAndWait(10000);
                        runs(function() {
                            list.hide();
                            store.loadData(makeData(100, 100));
                            list.show();
                            expect(scrollable.getPosition().y).toBe(0);
                            expectContent();
                        });
                    });
                });
            });
        });

        describe("with infinite: true", function() {
            function expectContent() {
                var count = store.getCount(),
                    pos = 0,
                    i, items, item, rec, el, y, content;

                items = Ext.Array.from(list.getRenderTarget().dom.childNodes);
                items = items.filter(function(el) {
                    var cls = el.classList;
                    return !(cls.contains(sizeCls) || cls.contains(paintCls));
                });

                expect(items.length).toBe(count);

                items.sort(function(a, b) {
                    a = Ext.getCmp(a.id).$position;
                    b = Ext.getCmp(b.id).$position;
                    return a - b;
                });

                for (i = 0; i < count; ++i) {
                    rec = store.getAt(i);
                    item = list.mapToItem(rec);
                    el = item.element;

                    expect(el.dom).toBe(items[i]);
                    content = el.down('.content') || el.down('.x-innerhtml');
                    expect(content).hasHTML(rec.get('name'));
                    y = getItemYPos(item);
                    expect(y).toBe(pos);

                    pos += el.measure('h') + el.getMargin('b');
                }
            }

            function checkFilled() {
                var scrollTop = list.getScrollable().getPosition().y,
                    scrollBottom = scrollTop + defaultSize,
                    spaceLeft = scrollBottom - scrollTop,
                    items;

                items = Ext.Array.from(list.getRenderTarget().dom.childNodes).map(function(node) {
                    return Ext.getCmp(node.id);
                }).filter(function(c) {
                    var top = c.$position,
                        bottom = top + c.$height;

                    return !c.$hidden && bottom > scrollTop && top < scrollBottom;
                });

                items.sort(function(a, b) {
                    return a.$position- b.$position;
                });

                items.forEach(function(c) {
                    var h = c.$height,
                        top = c.$position,
                        bottom = top + h;

                    if (top < scrollTop) {
                        spaceLeft -= bottom - scrollTop;
                    } else if (bottom > scrollBottom) {
                        spaceLeft -= bottom - scrollBottom;
                    } else {
                        spaceLeft -= h;
                    }
                });

                expect(spaceLeft).toBe(0);
            }

            describe("with variableHeights: false", function() {
                function makeSuiteList(cfg, storeData) {
                    makeList(Ext.apply({
                        infinite: true,
                        variableHeights: false
                    }, cfg), storeData);
                }

                describe("add", function() {
                    it("should render the new record", function() {
                        makeSuiteList(null, makeData(4));
                        list.hide();
                        store.insert(0, { id: 5, name: 'Item5' });
                        list.show();
                    expectContent();
                    });
                });

                describe("remove", function() {
                    it("should clear the removed record", function() {
                        makeSuiteList(null, makeData(4));
                        list.hide();
                        store.removeAt(0);
                        list.show();
                        expectContent();
                    });
                });

                describe("update", function() {
                    it("should change the item", function() {
                        makeSuiteList(null, makeData(4));
                        list.hide();
                        store.getAt(0).set('name', 'Foo');
                        list.show();
                        expectContent();
                    });
                });

                describe("refresh", function() {
                    describe("with scrollToTopOnRefresh: false", function() {
                        it("should retain scroll position", function() {
                            makeSuiteList({
                                scrollToTopOnRefresh: false
                            }, makeData(100));

                            var scrollable = list.getScrollable(),
                                pos;

                            // Scroll to end
                            scrollAndWait(10000);
                            runs(function() {
                                pos = scrollable.getPosition().y;
                                list.hide();
                                store.loadData(makeData(100, 100));
                                list.show();
                                expect(scrollable.getPosition().y).toBe(pos);

                                // Wait for it to resume the scroll position and update the rendered block
                                // to remove the first record's item
                                waitsFor(function() {
                                    return list.mapToItem(store.first()) == null;
                                });

                                runs(function() {
                                    checkFilled();
                                });
                            });
                        });
                    });

                    describe("with scrollToTopOnRefresh: true", function() {
                        it("should scroll to the top", function() {
                            makeSuiteList({
                                scrollToTopOnRefresh: true
                            }, makeData(100));

                            var scrollable = list.getScrollable();

                            // Scroll to end
                            scrollAndWait(10000);
                            runs(function() {
                                list.hide();
                                store.loadData(makeData(100, 100));
                                list.show();
                                expect(scrollable.getPosition().y).toBe(0);
                                expect(list.mapToItem(store.last())).toBeNull();
                                checkFilled();
                            });
                        });
                    });
                });
            });

            describe("with variableHeights: true", function() {
                function makeSuiteList(cfg, storeData) {
                    makeList(Ext.apply({
                        infinite: true,
                        variableHeights: true,
                        itemTpl: '<div class="content" style="{heightStyle}">{name}</div>'
                    }, cfg), storeData);
                }

                describe("add", function() {
                    it("should render the new record", function() {
                        makeSuiteList(null, makeData(4));
                        list.hide();
                        store.insert(0, { id: 5, name: 'Item5', height: 100, heightStyle: '100px' });
                        list.show();
                        expectContent();
                    });
                });

                describe("remove", function() {
                    it("should clear the removed record", function() {
                        makeSuiteList(null, makeData(4));
                        list.hide();
                        store.removeAt(0);
                        list.show();
                        expectContent();
                    });
                });

                describe("update", function() {
                    it("should change the item", function() {
                        makeSuiteList(null, makeData(4));
                        list.hide();
                        store.getAt(0).set({
                            name: 'Foo',
                            height: 60,
                            heightStyle: '60px'
                        });
                        list.show();
                        expectContent();
                    });
                });

                describe("refresh", function() {
                    describe("with scrollToTopOnRefresh: false", function() {
                        it("should retain scroll position", function() {
                            makeSuiteList({
                                scrollToTopOnRefresh: false
                            }, makeData(100));

                            var scrollable = list.getScrollable(),
                                pos;

                            // Scroll to end
                            scrollAndWait(10000);
                            runs(function() {
                                pos = scrollable.getPosition().y;
                                list.hide();
                                store.loadData(makeData(100, 100));
                                list.show();
                                expect(scrollable.getPosition().y).toBe(pos);
                                expect(list.mapToItem(store.first())).toBeNull();
                                checkFilled();
                            });
                        });
                    });

                    describe("with scrollToTopOnRefresh: true", function() {
                        it("should scroll to the top", function() {
                            makeSuiteList({
                                scrollToTopOnRefresh: true
                            }, makeData(100));

                            var scrollable = list.getScrollable();

                            // Scroll to end
                            scrollAndWait(10000);
                            runs(function() {
                                list.hide();
                                store.loadData(makeData(100, 100));
                                list.show();
                                expect(scrollable.getPosition().y).toBe(0);
                                expect(list.mapToItem(store.last())).toBeNull();
                                checkFilled();
                            });
                        });
                    });
                });
            });
        });
    });

    describe('select event', function () {
        describe('single mode', function () {
            it('should have single select record', function () {
                makeList();

                var record = store.getAt(0),
                    spy = spyOn({
                        testFn: Ext.emptyFn
                    }, 'testFn');

                list.on('select', spy);

                list.select(record);

                expect(spy).toHaveBeenCalledWith(list, record);
            });
        });

        describe('multi mode', function () {
            beforeEach(function() {
                makeList({
                    selectable: {
                        mode: 'multi'
                    }
                });
            });
            
            it('should have single select record', function () {
                var record = store.getAt(0),
                    spy = spyOn({
                        testFn: Ext.emptyFn
                    }, 'testFn');

                list.on('select', spy);

                list.select(record);

                expect(spy).toHaveBeenCalledWith(list, [ record ]);
            });

            it('should have multiple selected records', function () {
                var records = store.getRange(0, 2),
                    spy = spyOn({
                        testFn: Ext.emptyFn
                    }, 'testFn');

                list.on('select', spy);

                list.select(records);

                expect(spy).toHaveBeenCalledWith(list, records);
            });

            it("should select records with tap", function() {
                var selectSpy = jasmine.createSpy();

                selModel.on('selectionchange', selectSpy);

                Ext.testHelper.tap(list.getItemAt(0).el);

                waitsForSpy(selectSpy);

                runs(function() {
                    selectSpy.reset();
                    Ext.testHelper.tap(list.getItemAt(3).el);
                });

                waitsForSpy(selectSpy);

                runs(function() {
                    var selection = list.getSelected();

                    expect(selection.getAt(0)).toBe(store.getAt(0));
                    expect(selection.getAt(1)).toBe(store.getAt(3));
                });
            });
        });

        describe('simple mode', function () {
            it('should have single select record', function () {
                makeList({
                    selectable: {
                        mode: 'simple'
                    }
                });

                var record = store.getAt(0),
                    spy = spyOn({
                        testFn: Ext.emptyFn
                    }, 'testFn');

                list.on('select', spy);

                list.select(record);

                expect(spy).toHaveBeenCalledWith(list, [ record ]);
            });

            it('should have multiple selected records', function () {
                makeList({
                    selectable: {
                        mode: 'simple'
                    }
                });

                var records = store.getRange(0, 2),
                    spy = spyOn({
                        testFn: Ext.emptyFn
                    }, 'testFn');

                list.on('select', spy);

                list.select(records);

                expect(spy).toHaveBeenCalledWith(list, records);
            });
        });
    });
});
