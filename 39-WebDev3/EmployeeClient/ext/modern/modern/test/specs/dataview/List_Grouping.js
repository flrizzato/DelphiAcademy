topSuite("Ext.dataview.List_Grouping", [
    false,
    'Ext.dataview.List',
    'Ext.data.Store'
], function() {
    var defaultTotal = 100,
        defaultNumGroups = 4,
        defaultSize = 600,
        list, store, navModel;

    var M = Ext.define(null, {
        extend: 'Ext.data.Model',
        fields: ['name', 'group']
    });

    function createData(options) {
        if (typeof options === 'number') {
            options = {};
        } else if (!options) {
            options = {};
        }

        options.numGroups = options.numGroups || defaultNumGroups;
        options.total = options.total || defaultTotal;
        options.base = options.base || 0;

        var data = [],
            i = 1,
            group = options.groupBase || 0,
            next = Math.floor(options.total / options.numGroups),
            id;

        for (i = 0; i < options.total; ++i) {
            if (i % next === 0) {
                ++group;
            }

            id = options.base + i + 1;

            data.push({
                id: id,
                name: 'Item' + Ext.String.leftPad(id, 3, '0'),
                group: 'g' + group
            });
        }

        return data;
    }

    function createDataForGroup(group, count, base) {
        var data = [],
            i, id;

        base = base || 0;

        for (i = 0; i < count; ++i) {
            id = base + i + 1;
            data.push({
                id: id,
                name: 'Item' + Ext.String.leftPad(id, 3, '0'),
                group: group
            });
        }

        return data;
    }

    function createStore(data, cfg) {
        cfg = cfg || {};

        if (!cfg.data && data !== false && !Array.isArray(data)) {
            data = createData(data);
        }

        store = new Ext.data.Store(Ext.apply({
            asynchronousLoad: false,
            groupField: 'group',
            model: M,
            data: data
        }, cfg));
    }

    function createList(cfg, storeCfg) {
        cfg = cfg || {};

        if (!cfg.store && storeCfg !== false && !store) {
            if (Array.isArray(storeCfg)) {
                createStore(storeCfg);
            } else {
                createStore(null, storeCfg);
            }
        }

        list = new Ext.dataview.List(Ext.apply({
            renderTo: Ext.getBody(),
            width: defaultSize,
            height: defaultSize,
            itemTpl: '{name}',
            store: store,
            grouped: true
        }, cfg));
        navModel = list.getNavigationModel();
    }

    function getDataItem(index) {
        var item = index;
        if (!item.isComponent)  {
            item = list.itemFromRecord(index);
        }
        return item;
    }

    function expectContent(item, value) {
        var el = getDataItem(item).el.down('.x-innerhtml');
        expect(el).hasHTML(value);
    }

    function expectHeader(item, value) {
        item = getDataItem(item);
        var el = item.$header.el.down('.x-innerhtml');
        expect(el).hasHTML(value + ' - Header');
    }

    function expectFooter(item, value) {
        item = getDataItem(item);
        var el = item.$footer.el.down('.x-innerhtml');
        expect(el).hasHTML(value + ' - Footer');
    }

    function expectNoHeader(item) {
        item = getDataItem(item);
        expect(item.$header).toBeNull();
    }

    function expectNoFooter(item) {
        item = getDataItem(item);
        expect(item.$footer).toBeNull();
    }

    beforeEach(function() {
        MockAjaxManager.addMethods();
    });

    afterEach(function() {
        store = list = Ext.destroy(list, store);
        MockAjaxManager.removeMethods();
    });

    describe("infinite: false", function() {
        function createSuite(withHeaders, withFooters) {
            function createSuiteList(cfg, storeCfg) {
                createList(Ext.apply({
                    infinite: false,
                    groupHeader: withHeaders ? {
                        xtype: 'itemheader',
                        tpl: '{html} - Header'
                    } : null,
                    groupFooter: withFooters ? {
                        xtype: 'itemheader',
                        tpl: '{html} - Footer'
                    } : null
                }, cfg), storeCfg);
            }

            function expectListContent(headers, footers) {
                headers = withHeaders ? headers : null;
                footers = withFooters ? footers : null;

                var count = store.getCount(),
                    extras = (headers ? headers.length : 0) + (footers ? footers.length : 0),
                    i, rec;

                if (list.innerCt.child('.x-size-monitors')) {
                    extras++;
                }

                if (list.innerCt.child('.x-paint-monitor')) {
                    extras++;
                }

                expect(list.getRenderTarget().dom.childNodes.length).toBe(count + extras);

                for (i = 0; i < count; ++i) {
                    rec = store.getAt(i);

                    if (withHeaders && headers && headers.indexOf(i) > -1) {
                        expectHeader(i, rec.get('group'));
                    } else {
                        expectNoHeader(i);
                    }

                    expectContent(i, rec.get('name'));

                    if (withFooters && footers && footers.indexOf(i) > -1) {
                        expectFooter(i, rec.get('group'));
                    } else {
                        expectNoFooter(i);
                    }
                }
            }

            describe("basic rendering", function() {
                describe("grouped config", function() {
                    it("should render the group headers", function() {
                        createSuiteList({
                            grouped: true
                        });

                        expectListContent([0, 25, 50, 75], [24, 49, 74, 99]);
                    });

                    it("should not render groups if grouped: false is set", function() {
                        createSuiteList({
                            grouped: false
                        });

                        expectListContent();
                    });

                    it("should unrender groups if grouped is set to false", function() {
                        createSuiteList();
                        list.setGrouped(false);

                        expectListContent();
                    });

                    it("should render groups if grouped is set to true", function() {
                        createSuiteList({
                            grouped: false
                        });
                        list.setGrouped(true);
                        
                        expectListContent([0, 25, 50, 75], [24, 49, 74, 99]);
                    });

                    it("should not render groups if grouped is set to true but the store is not grouped", function() {
                        createSuiteList({
                            grouped: false
                        }, {
                            groupField: ''
                        });
                        list.setGrouped(true);
                        
                        expectListContent();
                    });
                });

                describe("store grouper", function() {
                    it("should not render groups if the store is not grouped", function() {
                        createSuiteList({}, {
                            groupField: ''
                        });
                        
                        expectListContent();
                    });

                    it("should not render groups if the store grouper is removed", function() {
                        createSuiteList();
                        store.setGrouper(null);
                        
                        expectListContent();
                    });

                    it("should render groups if the store grouper is set", function() {
                        createSuiteList({}, {
                            groupField: ''
                        });
                        store.setGroupField('group');

                        expectListContent([0, 25, 50, 75], [24, 49, 74, 99]);
                    });
                });
            });

            describe("dynamic store changes", function() {
                describe("adding", function() {
                    describe("to an existing group", function() {
                        beforeEach(function() {
                            createSuiteList();
                        });

                        describe("the first group", function() {
                            it("should insert at the start of the group", function() {
                                store.insert(0, {
                                    id: -1,
                                    name: 'NewItem',
                                    group: 'g1'
                                });

                                expect(store.getAt(0).id).toBe(-1);
                                expectListContent([0, 26, 51, 76], [25, 50, 75, 100]);
                            });

                            it("should insert in the middle of the group", function() {
                                store.insert(12, {
                                    id: -1,
                                    name: 'NewItem',
                                    group: 'g1'
                                });

                                expect(store.getAt(12).id).toBe(-1);
                                expectListContent([0, 26, 51, 76], [25, 50, 75, 100]);
                            });

                            it("should append to the end of the group", function() {
                                store.insert(25, {
                                    id: -1,
                                    name: 'NewItem',
                                    group: 'g1'
                                });

                                expect(store.getAt(25).id).toBe(-1);
                                expectListContent([0, 26, 51, 76], [25, 50, 75, 100]);
                            });
                        });

                        describe("a middle group", function() {
                            it("should insert at the start of the group", function() {
                                store.insert(25, {
                                    id: -1,
                                    name: 'NewItem',
                                    group: 'g2'
                                });

                                expect(store.getAt(25).id).toBe(-1);
                                expectListContent([0, 25, 51, 76], [24, 50, 75, 100]);
                            });

                            it("should insert in the middle of the group", function() {
                                store.insert(38, {
                                    id: -1,
                                    name: 'NewItem',
                                    group: 'g2'
                                });

                                expect(store.getAt(38).id).toBe(-1);
                                expectListContent([0, 25, 51, 76], [24, 50, 75, 100]);
                            });

                            it("should append to the end of the group", function() {
                                store.insert(50, {
                                    id: -1,
                                    name: 'NewItem',
                                    group: 'g2'
                                });

                                expect(store.getAt(50).id).toBe(-1);
                                expectListContent([0, 25, 51, 76], [24, 50, 75, 100]);
                            });
                        });

                        describe("the last group", function() {
                            it("should insert at the start of the group", function() {
                                store.insert(75, {
                                    id: -1,
                                    name: 'NewItem',
                                    group: 'g4'
                                });

                                expect(store.getAt(75).id).toBe(-1);
                                expectListContent([0, 25, 50, 75], [24, 49, 74, 100]);
                            });

                            it("should insert in the middle of the group", function() {
                                store.insert(87, {
                                    id: -1,
                                    name: 'NewItem',
                                    group: 'g4'
                                });

                                expect(store.getAt(87).id).toBe(-1);
                                expectListContent([0, 25, 50, 75], [24, 49, 74, 100]);
                            });

                            it("should append to the end of the group", function() {
                                store.insert(100, {
                                    id: -1,
                                    name: 'NewItem',
                                    group: 'g4'
                                });

                                expect(store.getAt(100).id).toBe(-1);
                                expectListContent([0, 25, 50, 75], [24, 49, 74, 100]);
                            });
                        });
                    });

                    describe("a new group", function() {
                        beforeEach(function() {
                            createSuiteList();
                        });

                        it("should be able to insert before the first group", function() {
                            store.add({
                                id: -1,
                                name: 'NewItem',
                                group: 'g0'
                            });

                            expect(store.getAt(0).id).toBe(-1);
                            expectListContent([0, 1, 26, 51, 76], [0, 25, 50, 75, 100]);
                        });

                        it("should be able to add in the middle", function() {
                            store.add({
                                id: -1,
                                name: 'NewItem',
                                group: 'g2_5'
                            });

                            expect(store.getAt(50).id).toBe(-1);
                            expectListContent([0, 25, 50, 51, 76], [24, 49, 50, 75, 100]);
                        });

                        it("should be able to add after the last group", function() {
                            store.add({
                                id: -1,
                                name: 'NewItem',
                                group: 'g6'
                            });

                            expect(store.getAt(100).id).toBe(-1);
                            expectListContent([0, 25, 50, 75, 100], [24, 49, 74, 99, 100]);
                        });
                    });

                    describe("with an empty store", function() {
                        beforeEach(function() {
                            createSuiteList(null, []);
                        });

                        it("should add the new group", function() {
                            store.add({
                                id: -1,
                                name: 'NewItem',
                                group: 'g6'
                            });
                            expectListContent([0], [0]);
                        });
                    });
                });

                describe("removing", function() {
                    describe("removing doesn't cause group removal", function() {
                        beforeEach(function() {
                            createSuiteList();
                        });

                        describe("the first group", function() {
                            it("should be able to remove the first item", function() {
                                store.removeAt(0);

                                expectListContent([0, 24, 49, 74], [23, 48, 73, 98]);
                            });

                            it("should be able to remove a middle item", function() {
                                store.removeAt(12);

                                expectListContent([0, 24, 49, 74], [23, 48, 73, 98]);
                            });

                            it("should be able to remove the last item", function() {
                                store.removeAt(24);

                                expectListContent([0, 24, 49, 74], [23, 48, 73, 98]);
                            });
                        });

                        describe("a middle group", function() {
                            it("should be able to remove the first item", function() {
                                store.removeAt(25);

                                expectListContent([0, 25, 49, 74], [24, 48, 73, 98]);
                            });

                            it("should be able to remove a middle item", function() {
                                store.removeAt(38);

                                expectListContent([0, 25, 49, 74], [24, 48, 73, 98]);
                            });

                            it("should be able to remove the last item", function() {
                                store.removeAt(49);

                                expectListContent([0, 25, 49, 74], [24, 48, 73, 98]);
                            });
                        });

                        describe("the last group", function() {
                            it("should be able to remove the first item", function() {
                                store.removeAt(75);

                                expectListContent([0, 25, 50, 75], [24, 49, 74, 98]);
                            });

                            it("should be able to remove a middle item", function() {
                                store.removeAt(87);

                                expectListContent([0, 25, 50, 75], [24, 49, 74, 98]);
                            });

                            it("should be able to remove the last item", function() {
                                store.removeAt(99);

                                expectListContent([0, 25, 50, 75], [24, 49, 74, 98]);
                            });
                        });
                    });

                    describe("removing causes group removal", function() {
                        it("should be able to remove the first group", function() {
                            var data = createDataForGroup('g1', 1)
                                       .concat(createDataForGroup('g2', 25, 1))
                                       .concat(createDataForGroup('g3', 25, 26))
                                       .concat(createDataForGroup('g4', 25, 51));

                            createSuiteList(null, data);

                            store.removeAt(0);
                            expectListContent([0, 25, 50], [24, 49, 74]);
                        });

                        it("should be able to remove a middle group", function() {
                            var data = createDataForGroup('g1', 25, 0)
                                       .concat(createDataForGroup('g2', 25, 25))
                                       .concat(createDataForGroup('g3', 1, 50))
                                       .concat(createDataForGroup('g4', 25, 51));

                            createSuiteList(null, data);

                            store.removeAt(50);
                            expectListContent([0, 25, 50], [24, 49, 74]);
                        });

                        it("should be able to remove the last group", function() {
                            var data = createDataForGroup('g1', 25, 0)
                                       .concat(createDataForGroup('g2', 25, 25))
                                       .concat(createDataForGroup('g3', 25, 50))
                                       .concat(createDataForGroup('g4', 1, 75));

                            createSuiteList(null, data);

                            store.removeAt(75);
                            expectListContent([0, 25, 50], [24, 49, 74]);
                        });
                    });

                    describe("removing the final group", function() {
                        it("should remove the final group", function() {
                            createSuiteList(null, [{
                                id: 1,
                                name: 'Item001',
                                group: 'g1'
                            }]);

                            store.removeAt(0);
                            expect(getDataItem(0)).toBeNull();
                            expectListContent();
                        });
                    });
                });

                describe("updating", function() {
                    describe("updating within the same group", function() {
                        beforeEach(function() {
                            createSuiteList();
                            store.getSorters().add({
                                property: 'name',
                                direction: 'asc'
                            });
                        });

                        describe("first group", function() {
                            it("should be able to move to be the first item", function() {
                                store.getAt(24).set('name', 'Item000');
                                expectListContent([0, 25, 50, 75], [24, 49, 74, 99]);
                            });

                            it("should be able to move to be a middle item", function() {
                                store.getAt(0).set('name', 'Item012_5');
                                expectListContent([0, 25, 50, 75], [24, 49, 74, 99]);
                            });

                            it("should be able to move to be the last item", function() {
                                store.getAt(0).set('name', 'Item025_5');
                                expectListContent([0, 25, 50, 75], [24, 49, 74, 99]);
                            });
                        });

                        describe("a middle group", function() {
                            it("should be able to move to be the first item", function() {
                                store.getAt(49).set('name', 'Item025_5');
                                expectListContent([0, 25, 50, 75], [24, 49, 74, 99]);
                            });

                            it("should be able to move to be a middle item", function() {
                                store.getAt(25).set('name', 'Item037_5');
                                expectListContent([0, 25, 50, 75], [24, 49, 74, 99]);
                            });

                            it("should be able to move to be the last item", function() {
                                store.getAt(25).set('name', 'Item050_5');
                                expectListContent([0, 25, 50, 75], [24, 49, 74, 99]);
                            });
                        });

                        describe("last group", function() {
                            it("should be able to move to be the first item", function() {
                                store.getAt(99).set('name', 'Item074_5');
                                expectListContent([0, 25, 50, 75], [24, 49, 74, 99]);
                            });

                            it("should be able to move to be a middle item", function() {
                                store.getAt(75).set('name', 'Item087_5');
                                expectListContent([0, 25, 50, 75], [24, 49, 74, 99]);
                            });

                            it("should be able to move to be the last item", function() {
                                store.getAt(75).set('name', 'Item100_5');
                                expectListContent([0, 25, 50, 75], [24, 49, 74, 99]);
                            });
                        });
                    });

                    describe("updating to a different group", function() {
                        beforeEach(function() {
                            createSuiteList();
                            store.getSorters().add({
                                property: 'name',
                                direction: 'asc'
                            });
                        });

                        describe("first group", function() {
                            it("should be able to move to be the first item", function() {
                                store.getAt(99).set({
                                    group: 'g1',
                                    name: 'Item000'
                                });
                                expectListContent([0, 26, 51, 76], [25, 50, 75, 99]);
                            });

                            it("should be able to move to be a middle item", function() {
                                store.getAt(99).set({
                                    group: 'g1',
                                    name: 'Item012_5'
                                });
                                expectListContent([0, 26, 51, 76], [25, 50, 75, 99]);
                            });

                            it("should be able to move to be the last item", function() {
                                store.getAt(99).set({
                                    group: 'g1',
                                    name: 'Item025_5'
                                });
                                expectListContent([0, 26, 51, 76], [25, 50, 75, 99]);
                            });

                            describe("where the index doesn't change", function() {
                                it("should update when it becomes the last item", function() {
                                    store.getAt(25).set('group', 'g1');
                                    expectListContent([0, 26, 50, 75], [25, 49, 74, 99]);
                                });
                            });
                        });

                        describe("a middle group", function() {
                            it("should be able to move to be the first item", function() {
                                store.getAt(0).set({
                                    group: 'g2',
                                    name: 'Item025_5'
                                });
                                expectListContent([0, 24, 50, 75], [23, 49, 74, 99]);
                            });

                            it("should be able to move to be a middle item", function() {
                                store.getAt(0).set({
                                    group: 'g2',
                                    name: 'Item037_5'
                                });
                                expectListContent([0, 24, 50, 75], [23, 49, 74, 99]);
                            });

                            it("should be able to move to be the last item", function() {
                                store.getAt(0).set({
                                    group: 'g2',
                                    name: 'Item050_5'
                                });
                                expectListContent([0, 24, 50, 75], [23, 49, 74, 99]);
                            });

                            describe("where the index doesn't change", function() {
                                it("should update when it becomes the last item", function() {
                                    store.getAt(50).set('group', 'g2');
                                    expectListContent([0, 25, 51, 75], [24, 50, 74, 99]);
                                });

                                it("should update when it becomes the first item", function() {
                                    store.getAt(24).set('group', 'g2');
                                    expectListContent([0, 24, 50, 75], [23, 49, 74, 99]);
                                });
                            });
                        });
                    });

                    describe("updating that causes group addition", function() {
                        beforeEach(function() {
                            createSuiteList();
                        });

                        it("should be able to add a group at the start", function() {
                            store.getAt(37).set('group', 'g0');
                            expectListContent([0, 1, 26, 50, 75], [0, 25, 49, 74, 99]);
                        });

                        it("should be able to add a group in the middle", function() {
                            store.getAt(0).set('group', 'g2_5');
                            expectListContent([0, 24, 49, 50, 75], [23, 48, 49, 74, 99]);
                        });

                        it("should be able to add a group at the end", function() {
                            store.getAt(12).set('group', 'g5');
                            expectListContent([0, 24, 49, 74, 99], [23, 48, 73, 98, 99]);
                        });
                    });

                    describe("updating that causes group removal", function() {
                        it("should be able to remove a group at the start", function() {
                            var data = createDataForGroup('g1', 1)
                                       .concat(createDataForGroup('g2', 25, 1))
                                       .concat(createDataForGroup('g3', 25, 26))
                                       .concat(createDataForGroup('g4', 25, 51));

                            createSuiteList(null, data);

                            store.getAt(0).set('group', 'g2');
                            expectListContent([0, 26, 51], [25, 50, 75]);
                        });

                        it("should be able to remove a group in the middle", function() {
                            var data = createDataForGroup('g1', 25, 0)
                                       .concat(createDataForGroup('g2', 25, 25))
                                       .concat(createDataForGroup('g3', 1, 50))
                                       .concat(createDataForGroup('g4', 25, 51));

                            createSuiteList(null, data);

                            store.getAt(50).set('group', 'g1');
                            expectListContent([0, 26, 51], [25, 50, 75]);
                        });

                        it("should be able to remove a group at the end", function() {
                            var data = createDataForGroup('g1', 25, 0)
                                       .concat(createDataForGroup('g2', 25, 25))
                                       .concat(createDataForGroup('g3', 25, 50))
                                       .concat(createDataForGroup('g4', 1, 75));

                            createSuiteList(null, data);

                            store.getAt(75).set('group', 'g1');
                            expectListContent([0, 26, 51], [25, 50, 75]);
                        });
                    });
                });

                describe("sorting", function() {
                    it("should be able to change group direction", function() {
                        createSuiteList();

                        store.setGrouper({
                            property: 'group',
                            direction: 'desc'
                        });
                        expectListContent([0, 25, 50, 75], [24, 49, 74, 99]);
                    });

                    it("should react to sorting changes", function() {
                        createSuiteList();

                        store.getSorters().add({
                            property: 'id',
                            direction: 'DESC'
                        });
                        expectListContent([0, 25, 50, 75], [24, 49, 74, 99]);
                    });
                });

                describe("filtering", function() {
                    beforeEach(function() {
                        createSuiteList();

                        var groups = store.getGroups();
                        store.getFilters().add({
                            filterFn: function(rec) {
                                var group = groups.getItemGroup(rec);
                                return group.indexOf(rec) >= 10;
                            }
                        });
                    });

                    it("should react to a filter being added", function() {
                        expectListContent([0, 15, 30, 45], [14, 29, 44, 59]);
                    });

                    it("should react to a filter being cleared", function() {
                        store.getFilters().removeAll();
                        expectListContent([0, 25, 50, 75], [24, 49, 74, 99]);
                    });
                });

                describe("loading", function() {
                    beforeEach(function() {
                        createSuiteList();
                    });

                    it("should load entirely new groups", function() {
                        store.loadData(createData({
                            groupBase: 10,
                            total: 30,
                            numGroups: 3
                        }));

                        expectListContent([0, 10, 20], [9, 19, 29]);
                    });

                    it("should load partially new groups", function() {
                        store.loadData(
                            createData({
                                total: 30,
                                numGroups: 2
                            }).concat(createData({
                                base: 30,
                                groupBase: 7,
                                total: 40,
                                numGroups: 2
                            }))
                        );

                        expectListContent([0, 15, 30, 50], [14, 29, 49, 69]);
                    });

                    it("should load the same groups", function() {
                        store.loadData(createData({
                            total: 80,
                            numGroups: 4
                        }));
                        expectListContent([0, 20, 40, 60], [19, 39, 59, 79]);
                    });
                });
            });

            describe("header/footer caching", function() {
                it("should ensure all cached headers/footers are destroyed", function() {
                    var count = Ext.ComponentManager.getCount();

                    createSuiteList(null, createData({
                        total: 100,
                        numGroups: 100
                    }));

                    store.removeAt(1, 99);
                    list.destroy();

                    expect(Ext.ComponentManager.getCount()).toBe(count);
                });
            });
        }

        describe("with headers only", function() {
            createSuite(true, false);
        });

        describe("with footers only", function() {
            createSuite(false, true);
        });

        describe("with headers and footers", function() {
            createSuite(true, true);
        });
    });

    describe("infinite: true", function() {
        var measured;

        function waitsScroll(y) {
            var scroller = list.getScrollable(),
                goal = [];

            runs(function() {
                goal[0] = scroller.getPosition().y + y;

                scroller.scrollBy(null, y, false);
            });

            waitsFor(function () {
                return list.getVisibleTop() === goal[0];
            });
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

        beforeAll(function() {
            createList({
                groupHeader: {
                    xtype: 'itemheader',
                    tpl: '{html}'
                },
                groupFooter: {
                    xtype: 'itemheader',
                    tpl: '{html}'
                }
            }, [{
                id: 1,
                name: 'Item001',
                group: 'g1'
            }, {
                id: 2,
                name: 'Item002',
                group: 'g1'
            }, {
                id: 3,
                name: 'Item003',
                group: 'g1'
            }]);

            var item = getDataItem(1),
                h = item.element.measure('h');

            measured = {
                item: h,
                header: getDataItem(0).$header.element.measure('h'),
                footer: getDataItem(2).$footer.element.measure('h'),
                toFill: Math.ceil(defaultSize / h)
            };

            store = list = Ext.destroy(list, store);
        });

        function makeSuite(withHeaders, withFooters) {
            function createSuiteList(cfg, storeCfg) {
                createList(Ext.apply({
                    infinite: true,
                    pinHeaders: false,
                    pinFooters: false,
                    groupHeader: withHeaders ? {
                        xtype: 'itemheader',
                        tpl: '{html} - Header'
                    } : null,
                    groupFooter: withFooters ? {
                        xtype: 'itemheader',
                        tpl: '{html} - Footer'
                    } : null
                }, cfg), storeCfg);
            }

            function expectListContent(options, headers, footers) {
                headers = withHeaders ? headers : null;
                footers = withFooters ? footers : null;

                var topRendered = options.start || 0,
                    bottomRendered = options.end,
                    currentTop = options.top || 0,
                    count = bottomRendered - topRendered + 1,
                    visibleDom, i, rec, extras, item;

                visibleDom = Ext.Array.from(list.getRenderTarget().dom.childNodes).filter(function(node) {
                    return !Ext.getCmp(node.id).$hidden;
                });

                extras = (headers ? headers.length : 0) + (footers ? footers.length : 0);
                expect(visibleDom.length).toBe(count + extras);

                for (i = topRendered; i <= bottomRendered; ++i) {
                    rec = store.getAt(i);
                    item = getDataItem(i);

                    if (withHeaders && headers && headers.indexOf(i) > -1) {
                        expectHeader(item, rec.get('group'));
                        expect(item.$header.$position).toBe(currentTop);
                        currentTop += measured.header;
                    } else {
                        expectNoHeader(i);
                    }

                    expectContent(item, rec.get('name'));
                    expect(item.$position).toBe(currentTop);
                    currentTop += measured.item;

                    if (withFooters && footers && footers.indexOf(i) > -1) {
                        expectFooter(item, rec.get('group'));
                        expect(item.$footer.$position).toBe(currentTop);
                        currentTop += measured.footer;
                    } else {
                        expectNoFooter(i);
                    }
                }
            }

            function runsExpectListContent(options, headers, footers) {
                runs(function() {
                    expectListContent(options, headers, footers);
                });
            }

            describe("basic rendering", function() {
                describe("grouped config", function() {
                    it("should render the group headers", function() {
                        createSuiteList({
                            grouped: true
                        });
                        checkFilled();
                    });

                    describe("not rendering groups", function() {
                        describe("grouped: false", function() {
                            it("should not render groups initially", function() {
                                createSuiteList({
                                    grouped: false
                                });
                                checkFilled();
                            });

                            it("should not render groups while scrolling", function() {
                                createSuiteList({
                                    grouped: false
                                });

                                waitsScroll(200);
                                runs(function() {
                                    checkFilled();
                                });
                                waitsScroll(200);
                                runs(function() {
                                    checkFilled();
                                });
                                waitsScroll(400);
                                runs(function() {
                                    checkFilled();
                                });
                            });
                        });

                        describe("setting to grouped: false", function() {
                            it("should unrender groups", function() {
                                createSuiteList();
                                list.setGrouped(false);

                                runs(function() {
                                    checkFilled();
                                });
                            });

                            it("should unrender groups if the list was scrolled", function() {
                                createSuiteList();
                                waitsScroll(400);
                                runs(function() {
                                    list.setGrouped(false);
                                });
                                runs(function() {
                                    checkFilled();
                                });
                                waitsScroll(200);
                                runs(function() {
                                    checkFilled();
                                });
                            });
                        });

                        describe("store not grouped", function() {
                            it("should not render groups if grouped is set to true but the store is not grouped", function() {
                                createSuiteList({
                                    grouped: false
                                }, {
                                    groupField: ''
                                });
                                list.setGrouped(true);
                                
                                checkFilled();
                                waitsScroll(500);
                                runs(function() {
                                    checkFilled();
                                });
                            });
                        });
                    });

                    describe("rendering groups", function() {
                        describe("setting grouped: true", function() {
                            beforeEach(function() {
                                createSuiteList({
                                    grouped: false
                                });
                            });

                            it("should render groups", function() {
                                list.setGrouped(true);
                                checkFilled();
                            });

                            it("should render groups if the list is scrolled", function() {
                                waitsScroll(500);
                                runs(function() {
                                    list.setGrouped(true);
                                });
                                runs(function() {
                                    checkFilled();
                                });
                                waitsScroll(-500);
                                runs(function() {
                                    checkFilled();
                                });
                            });
                        });
                    });                    
                });

                describe("store grouper", function() {
                    describe("not rendering groups", function() {
                        describe("no grouper", function() {
                            it("should not render groups", function() {
                                createSuiteList({}, {
                                    groupField: ''
                                });
                        
                                checkFilled();
                            });

                            it("should not render groups while scrolling", function() {
                                createSuiteList({}, {
                                    groupField: ''
                                });
                                waitsScroll(200);
                                runs(function() {
                                    checkFilled();
                                });
                                waitsScroll(200);
                                runs(function() {
                                    checkFilled();
                                });
                                waitsScroll(200);
                                runs(function() {
                                    checkFilled();
                                });
                            });
                        });

                        describe("grouper cleared", function() {
                            it("should not render groups", function() {
                                createSuiteList();
                                store.setGrouper(null);
                                
                                checkFilled();
                            });

                            it("should not render if the list is scrolled", function() {
                                createSuiteList();

                                waitsScroll(500);
                                runs(function() {
                                    store.setGrouper(null);
                                });
                                runs(function() {
                                    checkFilled();
                                });
                                waitsScroll(-500);
                                runs(function() {
                                    checkFilled();
                                });
                            });
                        });
                    });

                    describe("rendering groups", function() {
                        describe("grouper set", function() {
                            it("should render groups", function() {
                                createSuiteList({}, {
                                    groupField: ''
                                });
                                store.setGroupField('group');

                                checkFilled();
                            });

                            it("should render groups if the list is scrolled", function() {
                                createSuiteList({}, {
                                    groupField: ''
                                });

                                waitsScroll(500);
                                runs(function() {
                                    // This triggers a refresh
                                    store.setGroupField('group');
                                });
                                runs(function() {
                                    checkFilled();
                                });
                            });
                        });
                    });
                });

                describe("scrolling", function() {
                    it("should update correctly as the list scrolls", function() {
                        createSuiteList();

                        waitsScroll(100);
                        runs(function() {
                            checkFilled();
                        });
                        waitsScroll(200);
                        runs(function() {
                            checkFilled();
                        });
                        waitsScroll(200);
                        runs(function() {
                            checkFilled();
                        });
                        waitsScroll(100);
                        runs(function() {
                            checkFilled();
                        });
                        waitsScroll(200);
                        runs(function() {
                            checkFilled();
                        });
                        waitsScroll(300);
                        runs(function() {
                            checkFilled();
                        });
                        waitsScroll(250);
                        runs(function() {
                            checkFilled();
                        });
                        waitsScroll(100);
                        runs(function() {
                            checkFilled();
                        });
                        waitsScroll(400);
                        runs(function() {
                            checkFilled();
                        });
                    });
                });

                describe("portions with no extremities", function() {
                    it("should be able to render sections with no headers/footers", function() {
                        createSuiteList(null, createDataForGroup('g1', 1000));
                        waitsScroll(2000);
                        runs(function() {
                            checkFilled();
                        });
                    });
                });
            });

            describe("dynamic store changes", function() {
                describe("adding", function() {
                    describe("to an existing group", function() {

                    });
                });
            });

            describe("header/footer caching", function() {
                it("should ensure all cached headers/footers are destroyed", function() {
                    var count = Ext.ComponentManager.getCount();

                    createSuiteList(null, createData({
                        total: 100,
                        numGroups: 100
                    }));

                    store.removeAt(1, 99);
                    list.destroy();

                    expect(Ext.ComponentManager.getCount()).toBe(count);
                });
            });
        }

        describe("with headers only", function() {
            makeSuite(true, false);
        });

        describe("with footers only", function() {
            makeSuite(false, true);
        });

        describe("with headers and footers", function() {
            makeSuite(true, true);
        });

        describe("pinning", function() {

        });
    });

    describe('ensureVisible', function () {
        it('should scroll the requested item into view taking pinned header and footer into account', function () {
            createList({
                infinite: true,
                groupHeader: {
                    xtype: 'itemheader',
                    tpl: '{html} - Header'
                },
                groupFooter:  {
                    xtype: 'itemheader',
                    tpl: '{html} - Footer'
                },
                pinHeaders: true,
                pinFooters: true
            },  createData({
                total: 100,
                numGroups: 100
            }));
            var scroller = list.getScrollable(),
                firstItem = list.mapToItem(list.getStore().first());

            // Scroll to about halfway down
            navModel.setLocation(list.mapToItem(list.getStore().getAt(20)));
            waits(100);

            runs(function() {
                expect(scroller.getPosition().y).toBeGreaterThan(500);

                // Request to go to top.
                navModel.setLocation(firstItem);
                expect(scroller.getPosition().y).toBe(0);

                // Request to go to end
                navModel.setLocation(list.getStore().getCount() - 1);
                expect(scroller.getPosition().y).toBe(scroller.getMaxUserPosition().y);
            });
        });
    });

    describe('navigation at top with header', function () {
        it('should scroll the requested item into view taking pinned header and footer into account', function () {
            createList({
                infinite: true,
                groupHeader: {
                    xtype: 'itemheader',
                    tpl: '{html} - Header'
                },
                groupFooter:  {
                    xtype: 'itemheader',
                    tpl: '{html} - Footer'
                },
                pinHeaders: true,
                pinFooters: true
            },  createData({
                total: 100,
                numGroups: 100
            }));

            navModel.setLocation(0);

            waitsFor(function() {
                return list.containsFocus;
            });

            runs(function() {
                var location = navModel.getLocation();

                jasmine.fireKeyEvent(location.sourceElement, 'keydown', Ext.event.Event.UP);

                // Should not have moved. Should return immediately and not enter an infinite loop.
                expect(navModel.getLocation().equals(location)).toBe(true);
            });
        });
    });
});