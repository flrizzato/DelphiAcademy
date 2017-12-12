topSuite("Ext.dataview.IndexBar", [
    false,
    'Ext.dataview.List',
    'Ext.data.Store'
], function() {
    var defaultSize = 600,
        defaultGroups = (function () {
            var groups = [],
                i = 'a'.charCodeAt(0),
                finish = 'z'.charCodeAt(0);

            for (; i <= finish; ++i) {
                groups.push(String.fromCharCode(i));
            }

            return groups;
        })(),
        indexBar, list, scroller, store;

    var M = Ext.define(null, {
        extend: 'Ext.data.Model',
        fields: ['name', 'group']
    });

    function expectScrollPosition (y, x) {
        waitsFor(function () {
            var position = scroller.getPosition();

            return (y == null || position.y === y) &&
                   (x == null || position.x === x);
        }, 'Never scrolled to correct position');

        runs(function () {
            var position = scroller.getPosition();

            if (y != null) {
                expect(position.y).toBe(y);
            }

            if (x != null) {
                expect(position.x).toBe(x);
            }
        });
    }

    function createData (options) {
        options = options || {};

        var groups = options.groups || defaultGroups,
            base = options.base || 0,
            data = [];

        Ext.Array.forEach(groups, function (group) {
            var total = options.total || 10,
                i, id;

            for (i = 0; i < total; ++i) {
                id = base + i + 1;

                data.push({
                    id: group + '-' + id,
                    name: 'Item' + Ext.String.leftPad(id, 3, '0'),
                    group: group
                });
            }
        });

        return data;
    }

    function createStore (data) {
        var cfg = Array.isArray(data) ? {} : data;

        if (data !== false && !Array.isArray(data)) {
            data = createData(cfg);
        }

        store = new Ext.data.Store(Ext.apply({
            groupField: 'group',
            model: M,
            data: data
        }, cfg));
    }

    function createList(cfg, storeCfg) {
        cfg = cfg || {};

        if (!cfg.store && storeCfg !== false && !store) {
            createStore(storeCfg);
        }

        list = new Ext.dataview.List(Ext.apply({
            renderTo: Ext.getBody(),
            width: defaultSize,
            height: defaultSize,
            itemTpl: '{name}',
            store: store,
            indexBar: true,
            grouped: true
        }, cfg));

        indexBar = list.getIndexBar();
        scroller = list.getScrollable();
    }

    function getItem (index) {
        if (typeof index === 'string') {
            index = defaultGroups.indexOf(index);
        }

        if (index < 0) {
            index = 0;
        }

        return indexBar.el.down('.x-indexbar-item:nth-child(' + (index + 1) + ')');
    }

    afterEach(function() {
        indexBar = list = scroller = store = Ext.destroy(list, store);
    });

    describe('click', function () {
        describe('infinite: false', function () {
            it('should scroll to item', function () {
                var item;

                createList();

                item = getItem('o');

                jasmine.fireMouseEvent(item, 'click');

                expectScrollPosition(3850);
            });

            it('should scroll to item without animation', function () {
                var item;

                createList({
                    indexBar: {
                        animation: false
                    }
                });

                item = getItem('o');

                jasmine.fireMouseEvent(item, 'click');

                expectScrollPosition(3850);
            });

            it('should scroll to closest group', function () {
                var groups = defaultGroups.slice(),
                    item;

                groups.splice(7, 1); //remove 'h'

                createList(null, {
                    groups: groups
                });

                item = getItem('h');

                jasmine.fireMouseEvent(item, 'click');

                //should show 'i' group

                expectScrollPosition(1925);
            });

            it('should scroll to closest group without animation', function () {
                var groups = defaultGroups.slice(),
                    item;

                groups.splice(7, 1); //remove 'h'

                createList({
                    indexBar: {
                        animation: false
                    }
                }, {
                    groups: groups
                });

                item = getItem('h');

                jasmine.fireMouseEvent(item, 'click');

                //should show 'i' group

                expectScrollPosition(1925);
            });
        });

        describe('infinite: true', function () {
            it('should scroll to item', function () {
                var item;

                createList({
                    infinite: true
                });

                item = getItem('s');

                jasmine.fireMouseEvent(item, 'click');

                expectScrollPosition(4586);
            });

            it('should scroll to item with pinHeaders: false', function () {
                var item;

                createList({
                    infinite: true,
                    pinHeaders: false
                });

                item = getItem('j');

                jasmine.fireMouseEvent(item, 'click');

                expectScrollPosition(2293);
            });

            it('should scroll to closest group', function () {
                var groups = defaultGroups.slice(),
                    item;

                groups.splice(12, 1); //remove 'm'

                createList(
                    {
                        infinite: true
                    },
                    {
                        groups: groups
                    }
                );

                item = getItem('m');

                jasmine.fireMouseEvent(item, 'click');

                //should show 'n' group

                expectScrollPosition(3060);
            });
        });
    });
});
