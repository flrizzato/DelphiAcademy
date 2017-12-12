describe('Ext.dataview.NestedList', function () {
    var nestedlist, store;

    function createNestedList (cfg, nodes) {
        if (Ext.isArray(cfg)) {
            nodes = cfg;
            cfg = null;
        } else if (!nodes) {
            nodes = [{
                id: '/ford',
                text: 'Ford',
                children: [{
                    id: '/ford/mustang',
                    text: 'Mustang',
                    children: [{
                        id: '/ford/mustang/gt',
                        text: 'GT',
                        leaf: true
                    }]
                }]
            }];
        }

        nestedlist = Ext.create(Ext.apply({
            xtype: 'nestedlist',
            layout: {
                animation: null
            },
            store: {
                root: {
                    children: nodes
                }
            }
        }, cfg));

        store = nestedlist.getStore();
    }

    function getTitle () {
        if (nestedlist) {
            return nestedlist.getToolbar().getTitle();
        }
    }

    afterEach(function () {
        nestedlist = store = Ext.destroy(nestedlist);
    });

    describe('back event', function () {
        it('should be preventable', function () {
            createNestedList(
                {
                    detailCard: {
                        html: 'Ford Mustang GT'
                    },
                    listeners: {
                        back: function () {
                            return false;
                        }
                    }
                }
            );

            var gt_node = store.getNodeById('/ford/mustang/gt'),
                spy = spyOn(nestedlist, 'doBack');

            nestedlist.goToLeaf(gt_node);

            expect(nestedlist.getLastNode()).toBe(gt_node);
            expect(getTitle()).toBe('GT');

            nestedlist.onBackTap();

            expect(nestedlist.getLastNode()).toBe(gt_node);
            expect(getTitle()).toBe('GT');

            // back listener should have prevented doBack from being called
            expect(spy).not.toHaveBeenCalled();
        });

        it('should go to node within back event', function () {
            createNestedList(
                {
                    detailCard: {
                        html: 'Ford Mustang GT'
                    },
                    listeners: {
                        back: function () {
                            var mustang_node = store.getNodeById('/ford/mustang');

                            nestedlist.goToNode(mustang_node);

                            return false;
                        }
                    }
                }
            );

            var mustang_node = store.getNodeById('/ford/mustang'),
                gt_node = store.getNodeById('/ford/mustang/gt'),
                spy = spyOn(nestedlist, 'doBack');

            nestedlist.goToLeaf(gt_node);

            expect(nestedlist.getLastNode()).toBe(gt_node);
            expect(getTitle()).toBe('GT');

            nestedlist.onBackTap();

            expect(nestedlist.getLastNode()).toBe(mustang_node);
            expect(getTitle()).toBe('Mustang');

            // back listener should have prevented doBack from being called
            expect(spy).not.toHaveBeenCalled();
        });
    });
});
