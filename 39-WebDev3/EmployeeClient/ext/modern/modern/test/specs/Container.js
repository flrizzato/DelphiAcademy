topSuite("Ext.Container",
    ['Ext.form.Panel', 'Ext.viewport.Default', 'Ext.layout.VBox', 'Ext.app.ViewController', 'Ext.app.ViewModel', 'Ext.Toolbar'],
function() {
    var ct, items, vm;

    afterEach(function() {
        ct = Ext.destroy(ct);
    });

    function makeContainer(cfg) {
        if (Ext.isArray(cfg)) {
            cfg = {
                items: cfg
            };
        }
        ct = new Ext.container.Container(cfg);
        items = ct.getItems().items;
        vm = ct.getViewModel();
        return ct;
    }

    describe("bindings", function() {
        it("should track activeItemIndex change", function() {
            makeContainer({
                viewModel: { },
                renderTo: Ext.getBody(),
                bind: {
                    activeItemIndex: '{index}'
                },
                twoWayBindable: 'activeItemIndex',
                items: [{
                    text: {
                        bind: 'foo {index}'
                    }
                }, {
                    text: {
                        bind: 'bar {index}'
                    }
                }, {
                    text: {
                        bind: 'bletch {index}'
                    }
                }]
            });
            ct.setActiveItemIndex(1);
            vm.notify();
            expect(ct.getActiveItemIndex()).toBe(1);
            expect(ct.getActiveItem()).toBe(ct.innerItems[1]);
        });

        it("should track activeItem change", function() {
            makeContainer({
                viewModel: { },
                renderTo: Ext.getBody(),
                bind: {
                    activeItemIndex: '{index}'
                },
                twoWayBindable: 'activeItemIndex',
                items: [{
                    text: {
                        bind: 'foo {index}'
                    }
                }, {
                    text: {
                        bind: 'bar {index}'
                    }
                }, {
                    text: {
                        bind: 'bletch {index}'
                    }
                }]
            });
            ct.setActiveItem(ct.innerItems[1]);
            expect(ct.getActiveItemIndex()).toBe(1);
            expect(ct.getActiveItem()).toBe(ct.innerItems[1]);
        });

        it("should publish activeItemIndex change", function() {
            makeContainer({
                viewModel: { },
                renderTo: Ext.getBody(),
                bind: {
                    activeItemIndex: '{index}'
                },
                twoWayBindable: 'activeItemIndex',
                items: [{
                    text: {
                        bind: 'foo {index}'
                    }
                }, {
                    text: {
                        bind: 'bar {index}'
                    }
                }, {
                    text: {
                        bind: 'bletch {index}'
                    }
                }]
            });
            ct.setActiveItemIndex(1);
            vm.notify();
            expect(vm.get('index')).toBe(1);
        });

        it("should publish activeItemIndex change activeItem", function() {
            makeContainer({
                viewModel: { },
                renderTo: Ext.getBody(),
                bind: {
                    activeItemIndex: '{index}'
                },
                twoWayBindable: 'activeItemIndex',
                items: [{
                    itemId: 'foo',
                    weight: 3
                }, {
                    itemId: 'bar',
                    weight: 2
                }, {
                    itemId: 'bletch',
                    weight: 1
                }]
            });
            ct.setActiveItem(ct.innerItems[1]);
            vm.notify();
            expect(vm.get('index')).toBe(1);
        });

        it("should change activeItem via ViewModel binding", function() {
            makeContainer({
                viewModel: { },
                renderTo: Ext.getBody(),
                bind: {
                    activeItemIndex: '{index}'
                },
                twoWayBindable: 'activeItemIndex',
                items: [{
                    itemId: 'foo',
                    weight: 3
                }, {
                    itemId: 'bar',
                    weight: 2
                }, {
                    itemId: 'bletch',
                    weight: 1
                }]
            });
            vm.set('index', 1);
            vm.notify();
            expect(vm.get('index')).toBe(1);
            expect(ct.getActiveItemIndex()).toBe(1);
            expect(ct.getActiveItem()).toBe(ct.innerItems[1]);
            vm.set('index', 2);
            vm.notify();
            expect(vm.get('index')).toBe(2);
            expect(ct.getActiveItemIndex()).toBe(2);
            expect(ct.getActiveItem()).toBe(ct.innerItems[2]);
        });
    });

    describe('configured items', function() {
        describe('weighted items', function() {
            function expectOrder(order) {
                var childNodes = ct.getRenderTarget().dom.childNodes;
                order.forEach(function(id, idx) {
                    var item = items[idx];
                    expect(item.getItemId()).toBe(id);
                    expect(childNodes[idx]).toBe(item.element.dom);
                });
            }

            describe("configuration time", function() {
                it("should ignore weights if not weighted", function () {
                    makeContainer({
                        items: [{
                            itemId: 'a',
                            weight: 3
                        }, {
                            itemId: 'b',
                            weight: 2
                        }, {
                            itemId: 'c',
                            weight: 1
                        }]
                    });
                    expectOrder(['a', 'b', 'c']);
                });

                describe("as an object", function() {
                    it("should create the items with itemId and respect the weight", function () {
                        makeContainer({
                            weighted: true,
                            items: {
                                a: { weight: 3 },
                                b: { weight: 2 },
                                c: { weight: 1 }
                            }
                        });
                        expect(items[0].getItemId()).toBe('c');
                        expect(items[1].getItemId()).toBe('b');
                        expect(items[2].getItemId()).toBe('a');
                        expectOrder(['c', 'b', 'a']);
                    });

                    it("should treat no weight as 0", function() {
                        makeContainer({
                            weighted: true,
                            items: {
                                a: { weight: 1 },
                                b: { weight: -1 },
                                c: {}
                            }
                        });
                        expectOrder(['b', 'c', 'a']);
                    });

                    it("should treat equal weights as being in order", function() {
                        makeContainer({
                            weighted: true,
                            items: {
                                a: { weight: 3 },
                                b: { weight: 1 },
                                c: { weight: 1 }
                            }
                        });
                        expectOrder(['b', 'c', 'a']);
                    });

                    it("should create items in order with no weights", function() {
                        makeContainer({
                            weighted: true,
                            items: {
                                a: {},
                                b: {},
                                c: {}
                            }
                        });
                        expect(items[0].getItemId()).toBe('a');
                        expect(items[1].getItemId()).toBe('b');
                        expect(items[2].getItemId()).toBe('c');
                        expectOrder(['a', 'b', 'c']);
                    });
                });

                describe("as an array", function() {
                    it("should order items by weight", function() {
                        makeContainer({
                            weighted: true,
                            items: [
                                { itemId: 'a', weight: 3 },
                                { itemId: 'b', weight: 2 },
                                { itemId: 'c', weight: 1 },
                            ]
                        });
                        expectOrder(['c', 'b', 'a']);
                    });

                    it("should treat no weight as 0", function() {
                        makeContainer({
                            weighted: true,
                            items: [
                                { itemId: 'a', weight: 1 },
                                { itemId: 'b', weight: -1 },
                                { itemId: 'c' },
                            ]
                        });
                        expectOrder(['b', 'c', 'a']);
                    });

                    it("should treat equal weights as being in order", function() {
                        makeContainer({
                            weighted: true,
                            items: [
                                { itemId: 'a', weight: 3 },
                                { itemId: 'b', weight: 1 },
                                { itemId: 'c', weight: 1 },
                            ]
                        });
                        expectOrder(['b', 'c', 'a']);
                    });

                    it("should create items in order with no weights", function() {
                        makeContainer({
                            weighted: true,
                            items: [
                                { itemId: 'a' },
                                { itemId: 'b' },
                                { itemId: 'c' },
                            ]
                        });
                        expectOrder(['a', 'b', 'c']);
                    });
                });
            });

            describe("after configuration", function() {
                beforeEach(function() {
                    makeContainer({
                        weighted: true,
                        items: {
                            a: { weight: -1 },
                            b: { weight: 3 },
                            c: { weight: 5 }
                        }
                    });
                });

                describe("adding a single object", function() {
                    it("should be able to add at the start", function() {
                        ct.add({
                            itemId: 'd',
                            weight: -3
                        });
                        expectOrder(['d', 'a', 'b', 'c']);
                    });

                    it("should be able to add in the middle", function() {
                        ct.add({
                            itemId: 'd',
                            weight: 1
                        });
                        expectOrder(['a', 'd', 'b', 'c']);
                    });

                    it("should be able to add at the end", function() {
                        ct.add({
                            itemId: 'd',
                            weight: 7
                        });
                        expectOrder(['a', 'b', 'c', 'd']);
                    });

                    it("should treat no weight as 0", function() {
                        ct.add({
                            itemId: 'd'
                        });
                        expectOrder(['a', 'd', 'b', 'c']);
                    });
                });

                describe("adding an array of items", function() {
                    it("should be able to add at the start", function() {
                        ct.add([{
                            itemId: 'd',
                            weight: -3
                        }]);
                        expectOrder(['d', 'a', 'b', 'c']);
                    });

                    it("should be able to add in the middle", function() {
                        ct.add([{
                            itemId: 'd',
                            weight: 1
                        }]);
                        expectOrder(['a', 'd', 'b', 'c']);
                    });

                    it("should be able to add at the end", function() {
                        ct.add([{
                            itemId: 'd',
                            weight: 7
                        }]);
                        expectOrder(['a', 'b', 'c', 'd']);
                    });

                    it("should treat no weight as 0", function() {
                        ct.add([{
                            itemId: 'd'
                        }]);
                        expectOrder(['a', 'd', 'b', 'c']);
                    });

                    it("should be able to add items of different weights", function() {
                        ct.add([
                            { itemId: 'd', weight: 7 },
                            { itemId: 'e', weight: -3 },
                            { itemId: 'f', weight: 2 },
                        ]);
                        expectOrder(['e', 'a', 'f', 'b', 'c', 'd']);
                    });
                });

                describe("changing weight", function() {
                    it("should be able to move to the start", function() {
                        ct.down('#b').setWeight(-2);
                        expectOrder(['b', 'a', 'c']);
                    });

                    it("should be able to move to the middle", function() {
                        ct.down('#a').setWeight(4);
                        expectOrder(['b', 'a', 'c']);
                    });

                    it("should be able to move to the end", function() {
                        ct.down('#a').setWeight(100);
                        expectOrder(['b', 'c', 'a']);
                    });
                });
            });
        });
    });

    describe('insert', function () {
        beforeEach(function () {
            makeContainer([
                { html: 'one inner' },
                { html: 'one docked', docked: 'top' },
                { html: 'two inner' },
                { html: 'two docked', docked: 'top' },
                { html: 'three inner' },
                { html: 'three docked', docked: 'top' },
                { html: 'four inner' },
                { html: 'four docked', docked: 'top' },
                { html: 'five inner' },
                { html: 'five docked', docked: 'top' }
            ]);
        });

        it('should insert inner component at specific index', function () {
            var c = ct.insert(2, {
                html: 'inserted inner'
            });

            expect(ct.items.getAt(2)).toBe(c);
            expect(ct.getInnerItems()[1]).toBe(c);
        });

        it('should insert docked component at specific index', function () {
            var c = ct.insert(6, {
                html: 'inserted docked',
                docked: 'top'
            });

            expect(ct.items.getAt(6)).toBe(c);
            expect(ct.getInnerItems().indexOf(c)).toBe(-1);
            expect(ct.getDockedItems()[3]).toBe(c);
        });

        it('should move inner component to new index', function () {
            var c = ct.insert(2, {
                html: 'inserted inner'
            });

            expect(ct.items.getAt(2)).toBe(c);
            expect(ct.getInnerItems()[1]).toBe(c);

            ct.insert(5, c);

            expect(ct.items.getAt(5)).toBe(c);
            expect(ct.getInnerItems()[3]).toBe(c);
        });

        it('should move docked component to new index', function () {
            var c = ct.insert(2, {
                html: 'inserted docked',
                docked: 'top'
            });

            expect(ct.items.getAt(2)).toBe(c);
            expect(c.isDocked()).toBe(true);
            expect(ct.getInnerItems().indexOf(c)).toBe(-1);
            expect(ct.getDockedItems()[1]).toBe(c);

            ct.insert(5, c);

            expect(ct.items.getAt(5)).toBe(c);
            expect(c.isDocked()).toBe(true);
            expect(ct.getInnerItems().indexOf(c)).toBe(-1);
            expect(ct.getDockedItems()[2]).toBe(c);
        });
    });

    describe("add", function() {
        beforeEach(function() {
            makeContainer();
        });
        
        it("should return the item when adding single item", function() {
            var c = ct.add({
                xtype: 'component'
            });
            
            expect(ct.items.getAt(0)).toBe(c);
        });
        
        it("should return the array of added items when passed an array", function() {
            var cs = ct.add([{ xtype: 'component' }]);
            
            expect(Ext.isArray(cs)).toBe(true);
            expect(cs.length).toBe(1);
            expect(ct.items.getAt(0)).toBe(cs[0]);
        });
        
        it("should return the array of added items when adding more than one", function() {
            var cs = ct.add([
                { xtype: 'component' },
                { xtype: 'component' }
            ]);
            
            expect(Ext.isArray(cs)).toBe(true);
            expect(cs.length).toBe(2);
            expect(ct.items.getAt(0)).toBe(cs[0]);
            expect(ct.items.getAt(1)).toBe(cs[1]);
        });

        describe('weighted items', function() {
            it('should not respect item weight upon add when container not configured weighted: true', function() {
                var c3 = ct.add({
                        xtype: 'component',
                        weight: 3
                    }),
                    c2 = ct.add({
                        xtype: 'component',
                        weight: 2
                    }),
                    c1 = ct.add({
                        xtype: 'component',
                        weight: 1
                    }),
                    c1_5 = new Ext.Component({
                        weight: 1.5
                    });

                expect(items[0]).toBe(c3);
                expect(items[1]).toBe(c2);
                expect(items[2]).toBe(c1);

                ct.add(c1_5);

                // Should have inserted at the right place
                expect(items[0]).toBe(c3);
                expect(items[1]).toBe(c2);
                expect(items[2]).toBe(c1);
                expect(items[3]).toBe(c1_5);
            });
            it('should respect item weight upon add when container configured weighted: true', function() {
                ct.weighted = true;
                var c3 = ct.add({
                        xtype: 'component',
                        weight: 3
                    }),
                    c2 = ct.add({
                        xtype: 'component',
                        weight: 2
                    }),
                    c1 = ct.add({
                        xtype: 'component',
                        weight: 1
                    }),
                    c1_5 = new Ext.Component({
                        weight: 1.5
                    });

                expect(items[0]).toBe(c1);
                expect(items[1]).toBe(c2);
                expect(items[2]).toBe(c3);

                ct.add(c1_5);

                // Should have inserted at the right place
                expect(items[0]).toBe(c1);
                expect(items[1]).toBe(c1_5);
                expect(items[2]).toBe(c2);
                expect(items[3]).toBe(c3);
            });
        });
    });
    
    describe("remove", function() {
        var c0, c1;
        
        beforeEach(function() {
            makeContainer({
                items: [{
                    // itemIds are reversed to trip the tests
                    // if something goes wrong
                    xtype: 'component',
                    itemId: '1'
                }, {
                    xtype: 'component',
                    itemId: '0'
                }]
            });
            
            c0 = ct.items.getAt(0);
            c1 = ct.items.getAt(1);
        });
        
        afterEach(function() {
            Ext.destroy(c0, c1);
            c0 = c1 = null;
        });
        
        describe("by instance", function() {
            it("should remove an item", function() {
                ct.remove(c0);
                
                expect(ct.items.getCount()).toBe(1);
            });
            
            it("should return the removed item", function() {
                var ret = ct.remove(c0);
                
                expect(ret).toBe(c0);
            });
            
            it("should destroy the item when asked to", function() {
                var ret = ct.remove(c0, true);
                
                expect(ret.destroyed).toBe(true);
            });
            
            it("should not remove the remaining item", function() {
                var ret = ct.remove(c0);
                
                expect(ct.items.getAt(0)).toBe(c1);
            });
        });
        
        describe("by index", function() {
            it("should remove an item", function() {
                ct.remove(0);
                
                expect(ct.items.getCount()).toBe(1);
            });
            
            it("should return the removed item", function() {
                var ret = ct.remove(0);
                
                expect(ret).toBe(c0);
            });
            
            it("should destroy the item when asked to", function() {
                var ret = ct.remove(0, true);
                
                expect(ret.destroyed).toBe(true);
            });
            
            it("should not remove the remaining item", function() {
                ct.remove(0);
                
                expect(ct.items.getAt(0)).toBe(c1);
            });
        });
        
        describe("by itemId", function() {
            it("should remove an item", function() {
                ct.remove('0');
                
                expect(ct.items.getCount()).toBe(1);
            });
            
            it("should return the removed item", function() {
                var ret = ct.remove('0');
                
                expect(ret).toBe(c1);
            });
            
            it("should destroy the item when asked to", function() {
                var ret = ct.remove('0');
                
                expect(ret.destroyed).toBe(true);
            });
            
            it("should not remove the remaining item", function() {
                ct.remove('0');
                
                expect(ct.items.getAt(0)).toBe(c0);
            });
        });

        describe("autoDestroy: true", function () {
            var destroySpy;

            beforeEach(function () {
                destroySpy = spyOn(Ext.Component.prototype, 'destroy').andCallThrough();
            });

            it("should destroy the item", function () {
                ct.remove(c0);

                expect(destroySpy.callCount).toBe(1);
                expect(destroySpy.mostRecentCall.scope).toBe(c0);
            });

            it("should not destroy the item if destroy is false", function () {
                ct.remove(c0, false);

                expect(destroySpy.callCount).toBe(0);
            });
        });

        describe("autoDestroy: false", function () {
            var destroySpy;

            beforeEach(function () {
                destroySpy = spyOn(Ext.Component.prototype, 'destroy').andCallThrough();
                ct.setAutoDestroy(false);
            });

            it("should not destroy the item", function () {
                ct.remove(c0);

                expect(destroySpy.callCount).toBe(0);
            });

            it("should destroy the item if destroy is true", function () {
                ct.remove(c0, true);

                expect(destroySpy.callCount).toBe(1);
                expect(destroySpy.mostRecentCall.scope).toBe(c0);
            });
        });
    });
    
    describe("removeAll", function() {
        var c0, c1;
        
        beforeEach(function() {
            makeContainer({
                items: [{
                    xtype: 'component'
                }, {
                    xtype: 'component'
                }]
            });
            
            c0 = ct.items.getAt(0);
            c1 = ct.items.getAt(1);
        });
        
        afterEach(function() {
            Ext.destroy(c0, c1);
            c0 = c1 = null;
        });
        
        it("should remove all items", function() {
            ct.removeAll();
            
            expect(ct.items.getCount()).toBe(0);
        });
        
        it("should return the removed items", function() {
            var ret = ct.removeAll();
            
            expect(Ext.isArray(ret)).toBe(true);
            expect(ret.length).toBe(2);
            expect(ret[0]).toBe(c0);
            expect(ret[1]).toBe(c1);
        });
        
        it("should destroy the items when asked", function() {
            var ret = ct.removeAll(true);
            
            expect(ret[0].destroyed).toBe(true);
            expect(ret[1].destroyed).toBe(true);
        });
        
        // TODO removeAll(true, true)
        xit("should remove everything", function() {
        });
    });
    
    describe("removeAt", function() {
        var c0, c1;
        
        beforeEach(function() {
            makeContainer({
                items: [{
                    xtype: 'component'
                }, {
                    xtype: 'component'
                }]
            });
            
            c0 = ct.items.getAt(0);
            c1 = ct.items.getAt(1);
        });
        
        afterEach(function() {
            Ext.destroy(c0, c1);
            c0 = c1 = null;
        });
        
        it("should remove the item at index", function() {
            ct.removeAt(0);
            
            expect(ct.items.getCount()).toBe(1);
        });
        
        it("should return the removed item", function() {
            var ret = ct.removeAt(0);
            
            expect(ret).toBe(c0);
        });
        
        it("should not remove other items", function() {
            ct.removeAt(0);
            
            expect(ct.items.getAt(0)).toBe(c1);
        });

        describe("autoDestroy: true", function () {
            var destroySpy;

            beforeEach(function () {
                destroySpy = spyOn(Ext.Component.prototype, 'destroy').andCallThrough();
            });

            it("should destroy the item", function () {
                ct.removeAt(0);

                expect(destroySpy.callCount).toBe(1);
                expect(destroySpy.mostRecentCall.scope).toBe(c0);
            });

            it("should not destroy the item if destroy is false", function () {
                ct.removeAt(0, false);

                expect(destroySpy.callCount).toBe(0);
            });
        });

        describe("autoDestroy: false", function () {
            var destroySpy;

            beforeEach(function () {
                destroySpy = spyOn(Ext.Component.prototype, 'destroy').andCallThrough();
                ct.setAutoDestroy(false);
            });

            it("should not destroy the item", function () {
                ct.removeAt(0);

                expect(destroySpy.callCount).toBe(0);
            });

            it("should destroy the item if destroy is true", function () {
                ct.removeAt(0, true);

                expect(destroySpy.callCount).toBe(1);
                expect(destroySpy.mostRecentCall.scope).toBe(c0);
            });
        });
    });
    
    // TODO Not sure what an inner item is and how to add it? - AT
    xdescribe("removeInnerAt", function() {
    });

    describe("names", function() {
        it("should not be a name holder by default", function() {
            makeContainer({
                items: {
                    xtype: 'component',
                    nameable: true,
                    name: 'a'
                }
            });
            expect(ct.getNamedItems()).toBeNull();
            expect(ct.lookupName('a')).toBeNull();
        });
        it("should support basic lookupName", function() {
            makeContainer({
                nameHolder: true,
                items: {
                    xtype: 'component',
                    itemId: 'compA',
                    nameable: true,
                    name: 'a'
                }
            });
            expect(ct.getNamedItems()).not.toBeNull();
            expect(ct.lookupName('a')).not.toBeNull();
            expect(ct.lookupName('a')).toBe(ct.down('#compA'));
            expect(ct.lookupName('foo')).toBeNull();
        });
                
        it("should support deep name lookups", function() {
            makeContainer({
                nameHolder: true,
                items: {
                    xtype: 'container',
                    items: {
                        xtype: 'container',
                        items: {
                            xtype: 'container',
                            items: {
                                xtype: 'container',
                                items: {
                                    xtype: 'component',
                                    itemId: 'compA',
                                    nameable: true,
                                    name: 'a'
                                }
                            }
                        }
                    }
                }
            });
            expect(ct.lookupName('a')).not.toBeNull();
            expect(ct.lookupName('a')).toBe(ct.down('#compA'));
        });

        it("should support name lookups at different depths", function() {
            makeContainer({
                nameHolder: true,
                items: [
                    {
                        xtype: 'component',
                        itemId: 'compB',
                        nameable: true,
                        name: 'b'
                    },
                    {
                        xtype: 'container',
                        items: {
                            xtype: 'container',
                            items: {
                                xtype: 'container',
                                items: {
                                    xtype: 'container',
                                    items: {
                                        xtype: 'component',
                                        itemId: 'compA',
                                        nameable: true,
                                        name: 'a'
                                    }
                                }
                            }
                        }
                    }
                ]
            });
            expect(ct.lookupName('a')).not.toBeNull();
            expect(ct.lookupName('a')).toBe(ct.down('#compA'));
            expect(ct.lookupName('b')).not.toBeNull();
            expect(ct.lookupName('b')).toBe(ct.down('#compB'));
        });

        it("should support shareableName and return an array of fields with the same name", function() {
            var names;
            makeContainer({
                items: {
                    xtype: 'formpanel',
                    items: [
                        {
                            xtype: 'component',
                            itemId: 'compA',
                            name: 'a',
                            shareableName: true,
                            nameable: true
                        },
                        {
                            xtype: 'component',
                            itemId: 'compB',
                            name: 'a',
                            shareableName: true,
                            nameable: true
                        }
                    ]
                }
            });
            names = ct.down('formpanel').lookupName('a');
            expect(names[0]._itemId).toBe('compA');
            expect(names[0].name).toBe('a');
            expect(names[1]._itemId).toBe('compB');
            expect(names[1].name).toBe('a');
        });
    });

    describe("references", function() {
        describe("static", function() {
            it("should not be a reference holder by default", function() {
                makeContainer({
                    items: {
                        xtype: 'component',
                        reference: 'a'
                    }
                });    
                expect(ct.lookupReference('foo')).toBeNull();
            });
            
            it("should support a direct child", function() {
                makeContainer({
                    referenceHolder: true,
                    items: {
                        xtype: 'component',
                        itemId: 'compA',
                        reference: 'a'
                    }
                });
                expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
            });
            
            it("should support a deep child", function() {
                makeContainer({
                    referenceHolder: true,
                    items: {
                        xtype: 'container',
                        items: {
                            xtype: 'container',
                            items: {
                                xtype: 'container',
                                items: {
                                    xtype: 'component',
                                    itemId: 'compA',
                                    reference: 'a'
                                }
                            }
                        }
                    }
                });
                expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
            });
            
            it("should support children at multiple depths", function() {
                makeContainer({
                    referenceHolder: true,
                    items: [{
                        xtype: 'component',
                        itemId: 'compA',
                        reference: 'a'
                    }, {
                        xtype: 'container',
                        items: {
                            xtype: 'component',
                            itemId: 'compB',
                            reference: 'b'
                        }
                    }]
                }); 
                expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                expect(ct.lookupReference('b')).toBe(ct.down('#compB'));
            });
            
            it("should support multiple children at the same depth", function() {
                makeContainer({
                    referenceHolder: true,
                    items: [{
                        xtype: 'component',
                        itemId: 'compA',
                        reference: 'a'
                    }, {
                        xtype: 'component',
                        itemId: 'compB',
                        reference: 'b'
                    }]
                });
                expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                expect(ct.lookupReference('b')).toBe(ct.down('#compB'));
            });
            
            it("should support multiple children down the some tree", function() {
                    makeContainer({
                    referenceHolder: true,
                    items: [{
                        xtype: 'container',
                        itemId: 'compA',
                        reference: 'a',
                        items: {
                            xtype: 'container',
                            itemId: 'compB',
                            reference: 'b',
                            items: {
                                xtype: 'component',
                                itemId: 'compC',
                                reference: 'c'
                            }
                        }
                    }]
                });
                expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                expect(ct.lookupReference('b')).toBe(ct.down('#compB'));
                expect(ct.lookupReference('c')).toBe(ct.down('#compC'));
            });
            
            it("should support a reference holder not being at the root", function() {
                makeContainer({
                    items: {
                        xtype: 'container',
                        items: {
                            xtype: 'container',
                            itemId: 'ref',
                            referenceHolder: true,
                            items: {
                                xtype: 'container',
                                items: {
                                    xtype: 'component',
                                    itemId: 'compA',
                                    reference: 'a'
                                }
                            }
                        }
                    }
                });  
                var ref = ct.down('#ref');
                expect(ref.lookupReference('a')).toBe(ref.down('#compA'));
            });
            
            it("should support multiple ref holders in a tree", function() {
                makeContainer({
                    referenceHolder: true,
                    items: {
                        xtype: 'container',
                        itemId: 'compA',
                        reference: 'a',
                        items: {
                            xtype: 'container',
                            referenceHolder: true,
                            itemId: 'ref',
                            items: {
                                xtype: 'container',
                                items: {
                                    xtype: 'component',
                                    itemId: 'compB',
                                    reference: 'b'
                                }
                            }
                        }
                    }
                });
                var ref = ct.down('#ref');
                expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                expect(ref.lookupReference('b')).toBe(ref.down('#compB'));
            });
            
            it("should hide inner references from outer holders", function() {
                makeContainer({
                    referenceHolder: true,
                    items: {
                        xtype: 'container',
                        itemId: 'compA',
                        reference: 'a',
                        items: {
                            xtype: 'container',
                            referenceHolder: true,
                            itemId: 'ref',
                            items: {
                                xtype: 'container',
                                items: {
                                    xtype: 'component',
                                    itemId: 'compB',
                                    reference: 'b'
                                }
                            }
                        }
                    }
                });
                expect(ct.lookupReference('b')).toBeNull();
            });
            
            it("should allow a reference holder to have a reference", function() {
                makeContainer({
                    referenceHolder: true,
                    items: {
                        referenceHolder: true,
                        xtype: 'container',
                        itemId: 'compA',
                        reference: 'a',
                        items: {
                            xtype: 'container',
                            itemId: 'compB',
                            reference: 'b'
                        }
                    }
                });
                
                var inner = ct.down('#compA');
                
                expect(inner.lookupReference('b')).toBe(inner.down('#compB'));
                expect(ct.lookupReference('a')).toBe(inner);
            });
            
            describe("docking", function() {
                it("should get a reference to a direct child", function() {
                    makeContainer({
                        referenceHolder: true,
                        items: {
                            docked: 'top',
                            xtype: 'component',
                            itemId: 'compA',
                            reference: 'a'
                        }
                    });
                    expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                });
                
                it("should get a reference to an indirect child", function() {
                    makeContainer({
                        referenceHolder: true,
                        items: {
                            xtype: 'container',
                            docked: 'top',
                            items: {
                                xtype: 'component',
                                itemId: 'compA',
                                reference: 'a'
                            }
                        }
                    });
                    expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                });
            });
            
            describe("chained references", function() {
                it("should gain a reference to a deep child", function() {
                    makeContainer({
                        referenceHolder: true,
                        items: [{
                            xtype: 'container',
                            reference: 'parent>',
                            items: {
                                xtype: 'component',
                                itemId: 'compA',
                                reference: 'a'
                            }
                        }]
                    });
                    expect(ct.lookupReference('parent.a')).toBe(ct.down('#compA'));
                });
                
                it("should strip the > from the parent reference", function() {
                    makeContainer({
                        referenceHolder: true,
                        items: [{
                            xtype: 'container',
                            reference: 'a>',
                            itemId: 'compA',
                            items: {
                                xtype: 'component',
                                reference: 'b'
                            }
                        }]
                    });
                    expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                });
                
                it("should allow the parent to be reference even if there's no children", function() {
                    makeContainer({
                        referenceHolder: true,
                        items: [{
                            xtype: 'container',
                            reference: 'a>',
                            itemId: 'compA'
                        }]
                    });
                    expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                });
                
                it("should not setup a deep reference if the there's an intervening referenceHolder", function() {
                    makeContainer({
                        referenceHolder: true,
                        items: [{
                            xtype: 'container',
                            referenceHolder: true,
                            reference: 'a>',
                            items: {
                                xtype: 'component',
                                reference: 'b'
                            }
                        }]
                    });
                    expect(ct.lookupReference('b')).toBeNull();
                });
                
                it("should allow for a multiple depth reference", function() {
                    makeContainer({
                        referenceHolder: true,
                        items: [{
                            xtype: 'container',
                            reference: 'a>',
                            items: {
                                xtype: 'container',
                                reference: 'b>',
                                items: {
                                    xtype: 'container',
                                    reference: 'c>',
                                    items: {
                                        xtype: 'container',
                                        reference: 'd>',
                                        items: {
                                            xtype: 'component',
                                            reference: 'e',
                                            itemId: 'compE'
                                        }
                                    }
                                }
                            }
                        }]
                    });
                    expect(ct.lookupReference('a.b.c.d.e')).toBe(ct.down('#compE'));
                });
                
                it("should isolate references by parent", function() {
                    makeContainer({
                        referenceHolder: true,
                        items: [{
                            xtype: 'container',
                            reference: 'parent1>',
                            items: {
                                xtype: 'component',
                                reference: 'child',
                                itemId: 'compA'
                            }
                        }, {
                            xtype: 'container',
                            reference: 'parent2>',
                            items: {
                                xtype: 'component',
                                reference: 'child',
                                itemId: 'compB'
                            }
                        }]
                    });
                    expect(ct.lookupReference('parent1.child')).toBe(ct.down('#compA'));
                    expect(ct.lookupReference('parent2.child')).toBe(ct.down('#compB'));
                });
                
                it("should allow the reference holder to begin at any depth", function() {
                    makeContainer({
                        items: [{
                            xtype: 'container',
                            reference: 'a>',
                            items: {
                                xtype: 'container',
                                reference: 'b>',
                                items: {
                                    xtype: 'container',
                                    referenceHolder: true,
                                    reference: 'c>',
                                    itemId: 'compC',
                                    items: {
                                        xtype: 'container',
                                        reference: 'd>',
                                        items: {
                                            xtype: 'component',
                                            reference: 'e',
                                            itemId: 'compE'
                                        }
                                    }
                                }
                            }
                        }]
                    });
                    var inner = ct.down('#compC');
                    expect(inner.lookupReference('d.e')).toBe(ct.down('#compE'));
                });
                
                it("should allow multiple references in the tree", function() {
                    makeContainer({
                        referenceHolder: true,
                        items: [{
                            xtype: 'container',
                            reference: 'a>',
                            itemId: 'compA',
                            items: {
                                xtype: 'container',
                                reference: 'b>',
                                itemId: 'compB',
                                items: {
                                    xtype: 'container',
                                    referenceHolder: true,
                                    reference: 'c>',
                                    itemId: 'compC',
                                    items: {
                                        xtype: 'container',
                                        reference: 'd>',
                                        itemId: 'compD',
                                        items: {
                                            xtype: 'component',
                                            reference: 'e',
                                            itemId: 'compE'
                                        }
                                    }
                                }
                            }
                        }]
                    });
                    expect(ct.lookupReference('a.b')).toBe(ct.down('#compB'));
                    expect(ct.lookupReference('a.b.c')).toBe(ct.down('#compC'));
                    expect(ct.lookupReference('a.b.c.d')).toBeNull();
                    expect(ct.lookupReference('a.b.c.d.e')).toBeNull();
                }); 
                
                describe("docking", function() {
                    it("should get a reference to an indirect child", function() {
                        makeContainer({
                            referenceHolder: true,
                            items: {
                                xtype: 'container',
                                docked: 'top',
                                reference: 'a>',
                                items: {
                                    xtype: 'component',
                                    itemId: 'compB',
                                    reference: 'b'
                                }
                            }
                        });
                        expect(ct.lookupReference('a.b')).toBe(ct.down('#compB'));
                    });
                });
            });
        });
        
        describe("dynamic", function() {
            describe("adding", function() {
                it("should gain a reference to a direct child", function() {
                    makeContainer({
                        referenceHolder: true
                    });
                    ct.add({
                        xtype: 'component',
                        itemId: 'compA',
                        reference: 'a'
                    });
                    expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                }); 
                
                it("should gain a reference to an indirect child", function() {
                    makeContainer({
                        referenceHolder: true
                    });
                    ct.add({
                        xtype: 'container',
                        items: {
                            xtype: 'component',
                            itemId: 'compA',
                            reference: 'a'
                        }
                    });
                    expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                });
                
                it("should gain a reference to a component inside an already constructed container", function() {
                    var local = new Ext.container.Container({
                        items: {
                            xtype: 'component',
                            itemId: 'compA',
                            reference: 'a'
                        }
                    });    
                    
                    makeContainer({
                        referenceHolder: true,
                        items: local
                    });
                    expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                });
                
                it("should gain a reference to a component added to containers child", function() {
                    makeContainer({
                        referenceHolder: true,
                        items: {
                            xtype: 'container'
                        }
                    });  
                    ct.items.first().add({
                        xtype: 'component',
                        itemId: 'compA',
                        reference: 'a'
                    });  
                    expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                });
                
                describe("chained references", function() {
                    it("should gain a reference to an indirect child", function() {
                        makeContainer({
                            referenceHolder: true
                        });
                        ct.add({
                            xtype: 'container',
                            reference: 'parent>',
                            items: {
                                xtype: 'component',
                                itemId: 'compA',
                                reference: 'a'
                            }
                        });
                        expect(ct.lookupReference('parent.a')).toBe(ct.down('#compA'));
                    });

                    it("should gain a reference to a component inside an already constructed container", function() {
                        var local = new Ext.container.Container({
                            reference: 'parent>',
                            items: {
                                xtype: 'component',
                                itemId: 'compA',
                                reference: 'a'
                            }
                        });    

                        makeContainer({
                            referenceHolder: true,
                            items: local
                        });
                        expect(ct.lookupReference('parent.a')).toBe(ct.down('#compA'));
                    });

                    it("should gain a reference to a component added to containers child", function() {
                        makeContainer({
                            referenceHolder: true,
                            items: {
                                xtype: 'container',
                                reference: 'parent>'
                            }
                        });  
                        ct.items.first().add({
                            xtype: 'component',
                            itemId: 'compA',
                            reference: 'a'
                        });  
                        expect(ct.lookupReference('parent.a')).toBe(ct.down('#compA'));
                    });
                    
                    describe("docking", function() {
                        it("should gain a reference to an indirect child", function() {
                            makeContainer({
                                referenceHolder: true
                            });
                            ct.add({
                                xtype: 'container',
                                docked: 'top',
                                reference: 'parent>',
                                items: {
                                    xtype: 'component',
                                    itemId: 'compA',
                                    reference: 'a'
                                }
                            });
                            expect(ct.lookupReference('parent.a')).toBe(ct.down('#compA'));
                        });

                        it("should gain a reference to a component inside an already constructed container", function() {
                            var local = new Ext.container.Container({
                                docked: 'top',
                                reference: 'parent>',
                                items: {
                                    xtype: 'component',
                                    itemId: 'compA',
                                    reference: 'a'
                                }
                            });    

                            makeContainer({
                                referenceHolder: true,
                                items: local
                            });
                            expect(ct.lookupReference('parent.a')).toBe(ct.down('#compA'));
                        });

                        it("should gain a reference to a component added to containers child", function() {
                            makeContainer({
                                referenceHolder: true,
                                items: {
                                    docked: 'top',
                                    xtype: 'container',
                                    itemId: 'docked',
                                    reference: 'parent>'
                                }
                            });  
                            ct.down('#docked').add({
                                xtype: 'component',
                                itemId: 'compA',
                                reference: 'a'
                            });  
                            expect(ct.lookupReference('parent.a')).toBe(ct.down('#compA'));
                        });
                    });
                });
                
                describe("docking", function() {
                    it("should gain a reference to a direct child", function() {
                        makeContainer({
                            referenceHolder: true
                        });
                        ct.add({
                            xtype: 'component',
                            docked: 'top',
                            itemId: 'compA',
                            reference: 'a'
                        });
                        expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                    }); 
                    
                    it("should gain a reference to an indirect child", function() {
                        makeContainer({
                            referenceHolder: true
                        });
                        ct.add({
                            xtype: 'container',
                            docked: 'top',
                            items: {
                                xtype: 'component',
                                itemId: 'compA',
                                reference: 'a'
                            }
                        });
                        expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                    });
                    
                    it("should gain a reference to a component inside an already constructed container", function() {
                        var local = new Ext.container.Container({
                            docked: 'top',
                            items: {
                                xtype: 'component',
                                itemId: 'compA',
                                reference: 'a'
                            }
                        });    
                        
                        makeContainer({
                            referenceHolder: true,
                            items: local
                        });
                        expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                    });
                    
                    it("should gain a reference to a component added to containers child", function() {
                        makeContainer({
                            referenceHolder: true,
                            items: {
                                xtype: 'container',
                                docked: 'top',
                                itemId: 'docked'
                            }
                        });  
                        ct.down('#docked').add({
                            xtype: 'component',
                            itemId: 'compA',
                            reference: 'a'
                        });  
                        expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
                    });
                });   
            });
            
            describe("removing", function() {
                it("should not have a reference when removing a direct child", function() {
                    makeContainer({
                        referenceHolder: true,
                        items: {
                            xtype: 'component',
                            reference: 'a'
                        }
                    });    
                    var c = ct.lookupReference('a');
                    c.destroy();
                    expect(ct.lookupReference('a')).toBeNull();
                });
                
                it("should not have a reference when removing an indirect child", function() {
                    makeContainer({
                        referenceHolder: true,
                        items: {
                            xtype: 'container',
                            items: {
                                xtype: 'component',
                                reference: 'a'
                            }
                        }
                    });    
                    var c = ct.lookupReference('a');
                    c.destroy();
                    expect(ct.lookupReference('a')).toBeNull();
                });
                
                it("should not have a reference when removing+destroying a container that has references", function() {
                    makeContainer({
                        referenceHolder: true,
                        items: {
                            xtype: 'container',
                            items: {
                                xtype: 'component',
                                reference: 'a'
                            }
                        }
                    });    
                    var c = ct.lookupReference('a');
                    var removed = ct.remove(0);
                    expect(ct.lookupReference('a')).toBeNull();
                    removed.destroy();
                });
                
                it("should not have a reference when only removing a container that has references", function() {
                    makeContainer({
                        id: 'a',
                        referenceHolder: true,
                        items: {
                            id: 'b',
                            xtype: 'container',
                            items: {
                                id: 'c',
                                xtype: 'component',
                                reference: 'a'
                            }
                        }
                    });
                    
                    var c = ct.lookupReference('a');
                    var removed = ct.remove(0, false);
                    expect(ct.lookupReference('a')).toBeNull();
                    removed.destroy();
                });
                
                describe("chained references", function() {
                    it("should not have a reference when removing an indirect child", function() {
                        makeContainer({
                            referenceHolder: true,
                            items: {
                                xtype: 'container',
                                reference: 'parent>',
                                items: {
                                    xtype: 'component',
                                    reference: 'a'
                                }
                            }
                        });    
                        var c = ct.lookupReference('parent.a');
                        c.destroy();
                        expect(ct.lookupReference('parent.a')).toBeNull();
                    });

                    it("should not have a reference when removing+destroying a container that has references", function() {
                        makeContainer({
                            referenceHolder: true,
                            items: {
                                xtype: 'container',
                                reference: 'parent>',
                                items: {
                                    xtype: 'component',
                                    reference: 'a'
                                }
                            }
                        });    
                        var c = ct.lookupReference('parent.a');
                        var removed = ct.remove(0);
                        expect(ct.lookupReference('parent.a')).toBeNull();
                        removed.destroy();
                    });

                    it("should not have a reference when only removing a container that has references", function() {
                        makeContainer({
                            referenceHolder: true,
                            items: {
                                xtype: 'container',
                                reference: 'parent>',
                                items: {
                                    xtype: 'component',
                                    reference: 'a'
                                }
                            }
                        });    
                        var c = ct.lookupReference('parent.a');
                        var removed = ct.remove(0, false);
                        expect(ct.lookupReference('parent.a')).toBeNull();
                        removed.destroy();
                    });
                    
                    describe("docking", function() {
                        it("should not have a reference when removing an indirect child", function() {
                            makeContainer({
                                referenceHolder: true,
                                items: {
                                    xtype: 'container',
                                    docked: 'top',
                                    reference: 'parent>',
                                    items: {
                                        xtype: 'component',
                                        reference: 'a'
                                    }
                                }
                            });    
                            var c = ct.lookupReference('parent.a');
                            c.destroy();
                            expect(ct.lookupReference('parent.a')).toBeNull();
                        });

                        it("should not have a reference when removing+destroying a container that has references", function() {
                            makeContainer({
                                referenceHolder: true,
                                items: {
                                    xtype: 'container',
                                    docked: 'top',
                                    itemId: 'docked',
                                    reference: 'parent>',
                                    items: {
                                        xtype: 'component',
                                        reference: 'a'
                                    }
                                }
                            });    
                            var dock = ct.down('#docked');

                            ct.remove(dock);
                            expect(ct.lookupReference('parent.a')).toBeNull();
                        });

                        it("should not have a reference when only removing a container that has references", function() {
                            makeContainer({
                                referenceHolder: true,
                                items: {
                                    xtype: 'container',
                                    docked: 'top',
                                    itemId: 'docked',
                                    reference: 'parent>',
                                    items: {
                                        xtype: 'component',
                                        reference: 'a'
                                    }
                                }
                            });    
                            var dock = ct.down('#docked');

                            var removed = ct.remove(dock, false);
                            expect(ct.lookupReference('parent.a')).toBeNull();
                            removed.destroy();
                        }); 
                    });
                });
                
                describe("docking", function() {
                    it("should not have a reference when removing a direct child", function() {
                        makeContainer({
                            referenceHolder: true,
                            items: {
                                xtype: 'component',
                                docked: 'top',
                                reference: 'a'
                            }
                        });    
                        var c = ct.lookupReference('a');
                        c.destroy();
                        expect(ct.lookupReference('a')).toBeNull();
                    });
                    
                    it("should not have a reference when removing an indirect child", function() {
                        makeContainer({
                            referenceHolder: true,
                            items: {
                                xtype: 'container',
                                docked: 'top',
                                items: {
                                    xtype: 'component',
                                    reference: 'a'
                                }
                            }
                        });    
                        var c = ct.lookupReference('a');
                        c.destroy();
                        expect(ct.lookupReference('a')).toBeNull();
                    });
                    
                    it("should not have a reference when removing+destroying a container that has references", function() {
                        makeContainer({
                            referenceHolder: true,
                            items: {
                                xtype: 'container',
                                docked: 'top',
                                itemId: 'docked',
                                items: {
                                    xtype: 'component',
                                    reference: 'a'
                                }
                            }
                        });    
                        var dock = ct.down('#docked');
                            
                        ct.remove(dock);
                        expect(ct.lookupReference('a')).toBeNull();
                    });
                    
                    xit("should not have a reference when only removing a container that has references", function() {
                        makeContainer({
                            referenceHolder: true,
                            dockedItems: {
                                xtype: 'container',
                                docked: 'top',
                                itemId: 'docked',
                                items: {
                                    xtype: 'component',
                                    reference: 'a'
                                }
                            }
                        });
                        
                        var dock = ct.down('#docked');
                            
                        var removed = ct.remove(dock, false);
                        expect(ct.lookupReference('a')).toBeNull();
                        removed.destroy();
                    });    
                });
            });
        });

        describe("setup", function() {
            it("should not create references on the rootInheritedState if not requested", function() {
                var vp = new Ext.viewport.Default({
                    referenceHolder: true
                });

                var temp = new Ext.container.Container({
                    items: {
                        xtype: 'component',
                        reference: 'a'
                    }
                });

                var c = temp.items.first();


                ct = new Ext.container.Container({
                    referenceHolder: true,
                    items: temp
                });

                expect(vp.lookupReference('a')).toBeNull();
                expect(ct.lookupReference('a')).toBe(c);

                vp.destroy();
            });

            describe("with a controller", function() {
                it("should mark the container as a referenceHolder", function() {
                    makeContainer({
                        controller: 'controller',
                        items: [{
                            reference: 'child'
                        }]
                    });
                    expect(ct.referenceHolder).toBe(true);
                    var c = ct.lookup('child');
                    expect(ct.items.first()).toBe(c);

                    ct.remove(c);

                    expect(ct.lookup('child')).toBeNull();

                    c = ct.add({
                        reference: 'child'
                    });
                    expect(ct.lookup('child')).toBe(c);
                });
            });
        });
    });

    describe("view controllers", function() {
        var Controller;
        beforeEach(function() {
            Controller = Ext.define('spec.TestController', {
                extend: 'Ext.app.ViewController',
                alias: 'controller.test'
            });
        });
        
        afterEach(function() {
            Controller = null;
            Ext.undefine('spec.TestController');
            Ext.Factory.controller.instance.clearCache();
        });
        
        it("should use a defined controller as a referenceHolder", function() {
            makeContainer({
                controller: 'test',
                items: {
                    xtype: 'component',
                    itemId: 'compA',
                    reference: 'a'
                }
            });       
            expect(ct.lookupReference('a')).toBe(ct.down('#compA'));
        });   
    });
    
    describe("defaultListenerScope", function() {
        describe("static", function() {
            it("should fire on a direct parent", function() {
                makeContainer({
                    defaultListenerScope: true,
                    items: {
                        xtype: 'container',
                        itemId: 'compA',
                        listeners: {
                            custom: 'callFn'
                        }
                    }
                });
                ct.callFn = jasmine.createSpy();
                ct.down('#compA').fireEvent('custom');
                expect(ct.callFn).toHaveBeenCalled();    
            });

            it("should fire on an indirect parent", function() {
                makeContainer({
                    defaultListenerScope: true,
                    items: {
                        xtype: 'container',
                        items: {
                            xtype: 'container',
                            itemId: 'compA',
                            listeners: {
                                custom: 'callFn'
                            }
                        }
                    }
                });
                ct.callFn = jasmine.createSpy();
                ct.down('#compA').fireEvent('custom');
                expect(ct.callFn).toHaveBeenCalled(); 
            });

            it("should fire children in the same tree", function() {
                makeContainer({
                    defaultListenerScope: true,
                    items: {
                        xtype: 'container',
                        itemId: 'compA',
                        listeners: {
                            custom: 'callFn'
                        },
                        items: {
                            xtype: 'container',
                            itemId: 'compB',
                            listeners: {
                                custom: 'callFn'
                            }
                        }
                    }
                });
                ct.callFn = jasmine.createSpy();
                ct.down('#compA').fireEvent('custom');
                ct.down('#compB').fireEvent('custom');
                expect(ct.callFn.callCount).toBe(2); 
            });

            it("should fire when the ref holder isn't at the root", function() {
                makeContainer({
                    items: {
                        defaultListenerScope: true,
                        xtype: 'container',
                        itemId: 'compA',
                        items: {
                            xtype: 'container',
                            itemId: 'compB',
                            listeners: {
                                custom: 'callFn'
                            }
                        }
                    }
                });
                var c = ct.down('#compA'); 
                c.callFn = jasmine.createSpy();
                ct.down('#compB').fireEvent('custom');
                expect(c.callFn).toHaveBeenCalled(); 
            });

            it("should only fire the event at the closest defaultListenerScope holder", function() {
                makeContainer({
                    defaultListenerScope: true,
                    items: {
                        defaultListenerScope: true,
                        xtype: 'container',
                        itemId: 'compA',
                        items: {
                            xtype: 'container',
                            itemId: 'compB',
                            listeners: {
                                custom: 'callFn'
                            }
                        }
                    }
                });
                var c = ct.down('#compA');
                ct.callFn = jasmine.createSpy();
                c.callFn = jasmine.createSpy();

                ct.down('#compB').fireEvent('custom');
                expect(c.callFn).toHaveBeenCalled();
                expect(ct.callFn).not.toHaveBeenCalled(); 
            });
        });
        
        describe("dynamic", function() {
            it("should fire on a direct parent", function() {
                makeContainer({
                    defaultListenerScope: true
                });

                var c = ct.add({
                    xtype: 'component',
                    listeners: {
                        custom: 'callFn'
                    }
                });

                ct.callFn = jasmine.createSpy();
                c.fireEvent('custom');
                expect(ct.callFn).toHaveBeenCalled();
            });

            it("should fire on an indirect parent", function() {
                makeContainer({
                    defaultListenerScope: true,
                    items: {
                        xtype: 'container'
                    }
                });

                var c = ct.items.first().add({
                    xtype: 'component',
                    listeners: {
                        custom: 'callFn'
                    }
                });

                ct.callFn = jasmine.createSpy();
                c.fireEvent('custom');
                expect(ct.callFn).toHaveBeenCalled();
            });

            it("should resolve a new method in a new hierarchy", function() {
                makeContainer({
                    defaultListenerScope: true,
                    items: {
                        xtype: 'component',
                        itemId: 'compA',
                        listeners: {
                            custom: 'callFn'
                        }
                    }
                });

                var other = new Ext.container.Container({
                    defaultListenerScope: true
                });

                var c = ct.down('#compA');

                ct.callFn = jasmine.createSpy();
                other.callFn = jasmine.createSpy();

                c.fireEvent('custom');
                expect(ct.callFn).toHaveBeenCalled();

                other.add(c);
                ct.callFn.reset();
                c.fireEvent('custom');

                expect(ct.callFn).not.toHaveBeenCalled();
                expect(other.callFn).toHaveBeenCalled();

                other.destroy();
            });

            it("should resolve a new method in the same hierarchy", function() {
                makeContainer({
                    defaultListenerScope: true,
                    items: {
                        defaultListenerScope: true,
                        xtype: 'container',
                        itemId: 'compA',
                        items: {
                            xtype: 'component',
                            itemId: 'compB',
                            listeners: {
                                custom: 'callFn'
                            }
                        }
                    }
                });

                var inner = ct.down('#compA'),
                    c = ct.down('#compB');

                ct.callFn = jasmine.createSpy();
                inner.callFn = jasmine.createSpy();

                c.fireEvent('custom');
                expect(inner.callFn).toHaveBeenCalled();
                expect(ct.callFn).not.toHaveBeenCalled();

                ct.add(c);
                inner.callFn.reset();

                c.fireEvent('custom');
                expect(ct.callFn).toHaveBeenCalled();
                expect(inner.callFn).not.toHaveBeenCalled();
            });
        });
    });

    // the intent here is not to test ComponentQuery, just that the API calls the appropriate methods
    describe("ComponentQuery", function() {
        beforeEach(function(){
            ct = new Ext.Container({
                items: [{
                    foo: 1,
                    id: 'top1',
                    items: [{
                        foo: 3,
                        id: 'child1'
                    }, {
                        bar: 2,
                        itemId: 'child2',
                        items: [{
                            foo: 5
                        }]
                    }]
                }, {
                    foo: 2,
                    itemId: 'top2',
                    items: [{
                        foo: 7,
                        itemId: 'child3'
                    }, {
                        bar: 4
                    }]
                }, {
                    bar: 3
                }, {
                    foo: 8
                }]
            });
        });

        describe('getRefItems', function() {
            it("should not return the inner items array of the container's collection", function() {
                var result = ct.getRefItems();

                // Must not return the private items property of the items Collection
                expect(result).not.toBe(ct.getItems.items);
            });
        });

        describe("query", function(){
            it("should return all items if the selector is empty", function(){
                var arr = [];
                function buildItems(root) {
                    root.items.each(function(item){
                        arr.push(item);
                        buildItems(item);
                    });
                }
                buildItems(ct);
                expect(ct.query()).toEqual(arr);
            });

            it("should return an empty array for no matches", function(){
                var arr = ct.query('list');
                expect(arr).toEqual([]);
            });

            it("should return a filled array with matches", function(){
                var arr = ct.query('#child1');
                expect(arr).toEqual([Ext.getCmp('child1')]);
                arr = ct.query('[foo=1] #child1');
                expect(arr).toEqual([Ext.getCmp('child1')]);
            });

        });

        describe('child', function () {
            it('should return the first item if the selector is empty', function () {
                var c = ct.items.first();
                expect(ct.child()).toBe(c);
            });

            describe('selector is a string', function () {
                it('should return null if no match is found', function () {
                    expect(ct.child('#foo')).toBeNull();
                });

                it('should only return direct children', function () {
                    expect(ct.child('#child3')).toBeNull();
                });

                it('should return matching direct children', function () {
                    var c = ct.items.last();
                    expect(ct.child('component[foo="8"]')).toEqual(c);
                });

                it('should return null if component is not a direct child', function () {
                    var child1 = ct.query('#child1')[0],
                        child2 = ct.query('#child2')[0];

                    expect(ct.child('#child2')).toBeNull();
                    expect(child1.child('#child2')).toBeNull();
                    expect(child2.child('#top1')).toBeNull();
                });
            });

            describe('selector is a component', function () {
                it('should return null if no match is found', function () {
                    var cmp = Ext.create('Ext.Component', {
                        renderTo: document.body
                    });

                    expect(ct.child(cmp)).toBeNull();

                    cmp.destroy();
                    cmp = null;
                });

                it('should only return direct children', function () {
                    var child0 = ct.query('#child1')[0],
                        child1 = ct.query('#child2')[0],
                        child2 = ct.query('#child3')[0];

                    expect(ct.child(child0)).toBeNull();
                    expect(ct.child(child1)).toBeNull();
                    expect(ct.child(child2)).toBeNull();
                });

                it('should return matching direct children', function () {
                    var level0 = ct.query('#top1')[0],
                        c = ct.items.last();

                    expect(ct.child(level0)).toBe(level0);
                    expect(ct.child(c)).toBe(c);
                });

                it('should return null if component is not a direct child', function () {
                    var top1 = ct.query('#top1')[0],
                        child1 = ct.query('#child1')[0],
                        child2 = ct.query('#child2')[0];

                    expect(ct.child(child2)).toBeNull();
                    expect(child1.child(child2)).toBeNull();
                    expect(child2.child(top1)).toBeNull();
                });
            });

            describe('component ids with non alpha-numeric chars', function () {
                var ct,
                    makeCt = function (id1, id2) {
                        return new Ext.container.Container({
                            items: [{
                                id: id1,
                                items: [{
                                    id: id2
                                }, {
                                    itemId: 'child2'
                                }]
                            }]
                        });
                    },
                    name1, name2, child1, child2;

                afterEach(function () {
                    Ext.destroy(ct);
                    ct = child1 = child2 = name1 = name2 = null;
                });

                it('should allow non alpha-numeric chars', function () {
                    name1 = 'a-1_23_456-';
                    name2 = 'b-------222222222______';
                    ct = makeCt(name1, name2);

                    child1 = ct.query('#' + name1)[0];
                    child2 = child1.query('#' + name2)[0];

                    expect(ct.child(child1)).toBe(child1);
                    expect(child1.child(child2)).toBe(child2);

                });
            });
        });

        describe('down', function () {
            it('should return the first item if the selector is empty', function () {
                var c = ct.items.first();
                expect(ct.down()).toBe(c);
            });

            describe('selector is a string', function () {
                it('should return null if no match is found', function () {
                    expect(ct.down('#foo')).toBeNull();
                });

                it('should return null if component is not a descendant', function () {
                    var child1 = ct.query('#child1')[0],
                        child2 = ct.query('#child2')[0];

                    expect(child1.down('#child2')).toBeNull();
                    expect(child2.down('#top1')).toBeNull();
                });

                it('should return children at any level', function () {
                    var c = ct.items.getAt(1).items.first();
                    expect(ct.down('#child3')).toEqual(c);
                });

                it('should return the first match', function () {
                    var c = ct.items.first();
                    expect(ct.down('component[foo]')).toEqual(c);
                });
            });

            describe('selector is a component', function () {
                it('should return null if no match is found', function () {
                    var cmp = Ext.create('Ext.Component', {
                        renderTo: document.body
                    });

                    expect(ct.down(cmp)).toBeNull();

                    cmp.destroy();
                    cmp = null;
                });

                it('should return null if component is not a descendant', function () {
                    var top1 = ct.query('#top1')[0],
                        child1 = ct.query('#child1')[0],
                        child2 = ct.query('#child2')[0];

                    expect(child1.down(child2)).toBeNull();
                    expect(child2.down(top1)).toBeNull();
                });

                it('should return children at any level', function () {
                    var top1 = ct.query('#top1')[0],
                        child1 = ct.query('#child1')[0],
                        child2 = ct.query('#child2')[0];

                    expect(ct.down(top1)).toEqual(top1);
                    expect(ct.down(child1)).toEqual(child1);
                    expect(ct.down(child2)).toEqual(child2);
                });
            });

            describe('component ids with non alpha-numeric chars', function () {
                var ct,
                    makeCt = function (id1, id2) {
                        return new Ext.container.Container({
                            items: [{
                                id: 'foo',
                                items: [{
                                    id: id1
                                }, {
                                    items: [{
                                        itemId: id2
                                    }]
                                }]
                            }]
                        });
                    },
                    name1, name2, descendant1, descendant2;

                afterEach(function () {
                    Ext.destroy(ct);
                    ct = descendant1 = descendant2 = name1 = name2 = null;
                });

                it('should allow non alpha-numeric chars', function () {
                    name1 = 'a-1_23_456-';
                    name2 = 'b-------222222222______';
                    ct = makeCt(name1, name2);

                    descendant1 = ct.query('#' + name1)[0];
                    descendant2 = ct.query('#' + name2)[0];

                    expect(ct.down(descendant1)).toBe(descendant1);
                    expect(ct.down(descendant2)).toBe(descendant2);

                });
            });
        });
    });

    describe("queryBy", function(){
        it("should return no items if the container is empty", function(){
            makeContainer();
            expect(ct.queryBy(function(){})).toEqual([]);
        });

        it("should default the scope to the current component", function(){
            var scopes = [],
                c1 = new Ext.Component(),
                c2 = new Ext.Component(),
                c3 = new Ext.Component();

            makeContainer({
                items: [c1, c2, c3]
            });

            ct.queryBy(function(c){
                scopes.push(c);
            });
            expect(scopes).toEqual([c1, c2, c3]);
        });

        it("should use the specified scope", function(){
            var o = {},
                c1 = new Ext.Component(),
                scope;

            makeContainer({
                items: c1
            });
            ct.queryBy(function(){
                scope = this;
            }, o);

            expect(scope).toBe(o);
        });

        it("should only exclude items if the return value is false", function(){
            var c1 = new Ext.Component(),
                c2 = new Ext.Component(),
                c3 = new Ext.Component();

            makeContainer({
                items: [c1, c2, c3]
            });
            expect(ct.queryBy(function(c){

            })).toEqual([c1, c2, c3]);
        });

        it("should exclude items if the return value is false", function(){
            var c1 = new Ext.Component(),
                c2 = new Ext.Component(),
                c3 = new Ext.Component();

            makeContainer({
                items: [c1, c2, c3]
            });
            expect(ct.queryBy(function(c){
                return c !== c2;
            })).toEqual([c1, c3]);
        });

        it("should retrieve items in nested containers", function(){
            var c1 = new Ext.Component(),
                c2 = new Ext.Container({
                    items: c1
                }),
                c3 = new Ext.Container({
                    items: c2
                });

            makeContainer({
                items: c3
            });
            expect(ct.queryBy(function(c){
                return c === c1;
            })).toEqual([c1]);
        });
    });

    describe('defaults', function () {
        function createTests (name, withInstances) {
            describe(name, function () {
                it('should apply defaults with object', function () {
                    var item1 = {
                            html: 'foo'
                        },
                        item2 = {
                            html: 'bar'
                        };

                    if (withInstances) {
                        item1 = new Ext.Component(item1);
                        item2 = new Ext.Component(item2);
                    }

                    makeContainer({
                        defaults: {
                            test: 'default'
                        },
                        items: [item1, item2]
                    });

                    if (!withInstances) {
                        item1 = ct.getAt(0);
                        item2 = ct.getAt(1);
                    }

                    expect(item1.test).toBe('default');
                    expect(item2.test).toBe('default');

                    //make sure we cleaned this up
                    expect(item1.$defaultsApplied).toBe(undefined);
                    expect(item2.$defaultsApplied).toBe(undefined);
                });

                it('should not overwrite existing config with object', function () {
                    var item1 = {
                            html: 'foo'
                        },
                        item2 = {
                            html: 'bar',
                            test: 'foo'
                        };

                    if (withInstances) {
                        item1 = new Ext.Component(item1);
                        item2 = new Ext.Component(item2);
                    }

                    makeContainer({
                        defaults: {
                            test: 'default'
                        },
                        items: [item1, item2]
                    });

                    if (!withInstances) {
                        item1 = ct.getAt(0);
                        item2 = ct.getAt(1);
                    }

                    expect(item1.test).toBe('default');
                    expect(item2.test).toBe('foo');

                    //make sure we cleaned this up
                    expect(item1.$defaultsApplied).toBe(undefined);
                    expect(item2.$defaultsApplied).toBe(undefined);
                });

                it('should apply defaults with a function', function () {
                    var item1 = {
                            html: 'foo'
                        },
                        item2 = {
                            html: 'bar'
                        };

                    if (withInstances) {
                        item1 = new Ext.Component(item1);
                        item2 = new Ext.Component(item2);
                    }

                    makeContainer({
                        defaults: function (item) {
                            return {
                                test: 'default'
                            };
                        },
                        items: [item1, item2]
                    });

                    if (!withInstances) {
                        item1 = ct.getAt(0);
                        item2 = ct.getAt(1);
                    }

                    expect(item1.test).toBe('default');
                    expect(item2.test).toBe('default');

                    //make sure we cleaned this up
                    expect(item1.$defaultsApplied).toBe(undefined);
                    expect(item2.$defaultsApplied).toBe(undefined);
                });

                it('should not overwrite existing config with function', function () {
                    var item1 = {
                            html: 'foo'
                        },
                        item2 = {
                            html: 'bar',
                            test: 'foo'
                        };

                    if (withInstances) {
                        item1 = new Ext.Component(item1);
                        item2 = new Ext.Component(item2);
                    }

                    makeContainer({
                        defaults: function (item) {
                            return {
                                test: 'default'
                            };
                        },
                        items: [item1, item2]
                    });

                    if (!withInstances) {
                        item1 = ct.getAt(0);
                        item2 = ct.getAt(1);
                    }

                    expect(item1.test).toBe('default');
                    expect(item2.test).toBe('foo');

                    //make sure we cleaned this up
                    expect(item1.$defaultsApplied).toBe(undefined);
                    expect(item2.$defaultsApplied).toBe(undefined);
                });
            });
        }

        createTests('item as config');
        createTests('item as instance', true);
    });
    
    describe("autoSize", function() {
        describe("rendered to page", function() {
            var renderEl;

            function makeCt(cfg) {
                return makeContainer(Ext.apply({
                    items: [{
                        xtype: 'component',
                        style: 'width:200px;height:100px;'
                    }]
                }, cfg));
            }
            
            beforeEach(function () {
                renderEl = Ext.getBody().append({
                    style: 'width: 400px; height: 300px;'
                });
            });

            afterEach(function () {
                renderEl = Ext.destroy(renderEl);
            });

            it("should default to autoSize: true", function() {
                makeCt({
                    renderTo: renderEl
                });

                expect(ct.getAutoSize()).toBe(null);
                expect(ct).toHaveLayout({
                    el: { xywh: '0 0 400 100'},
                    bodyElement: { xywh: '0 0 400 100' }
                });
                expect(ct.bodySizerElement == null).toBe(true);
            });

            it("should wrap the bodyElement when autoSize: false", function() {
                makeCt({
                    renderTo: renderEl,
                    autoSize: false
                });

                expect(ct.bodySizerElement instanceof Ext.Element).toBe(true);
                expect(ct.bodySizerElement.first()).toBe(ct.bodyElement);

                // Container is zero height because of the absolutely positioned wrapper
                expect(ct).toHaveLayout({
                    el: {xywh: '0 0 400 0'},
                    bodySizerElement: {xywh: '0 0 400 0'},
                    bodyElement: {xywh: '0 0 400 0'}
                });
            });

            it("should size the wrapper and position the bodyElement", function() {
                makeCt({
                    renderTo: renderEl,
                    autoSize: false,
                    width: 300,
                    height: 200
                });

                expect(ct.bodySizerElement instanceof Ext.Element).toBe(true);
                expect(ct.bodySizerElement.first()).toBe(ct.bodyElement);

                expect(ct).toHaveLayout({
                    el: {xywh: '0 0 300 200'},
                    bodySizerElement: {xywh: '0 0 300 200'},
                    bodyElement: {xywh: '0 0 300 200'}
                });

                expect(ct.bodyElement.getStyle('position')).toBe('absolute');
                expect(parseInt(ct.bodyElement.getStyle('top'), 10)).toBe(0);
                expect(parseInt(ct.bodyElement.getStyle('right'), 10)).toBe(0);
                expect(parseInt(ct.bodyElement.getStyle('bottom'), 10)).toBe(0);
                expect(parseInt(ct.bodyElement.getStyle('left'), 10)).toBe(0);
            });
        });

        describe("floated", function() {
            function makeCt(cfg) {
                return makeContainer(Ext.apply({
                    items: [{
                        xtype: 'component',
                        style: 'width:200px;height:100px;'
                    }]
                }, cfg));
            }

            it("should default to autoSize: true", function() {
                makeCt({
                    floated: true
                });

                ct.show();

                expect(ct.getAutoSize()).toBe(true);
                expect(ct).toHaveLayout({
                    el: {xywh: '0 0 200 100'},
                    bodyElement: {xywh: '0 0 200 100'}
                });
                expect(ct.bodySizerElement == null).toBe(true);
            });

            it("should force autoSize to true if the floated container is not sized on either axis", function() {
                makeCt({
                    floated: true,
                    autoSize: false
                });

                ct.show();

                expect(ct.getAutoSize()).toBe(true);
                expect(ct).toHaveLayout({
                    el: {xywh: '0 0 200 100'},
                    bodyElement: {xywh: '0 0 200 100'}
                });
                expect(ct.bodySizerElement == null).toBe(true);
            });

            it("should force autoSize to true if the floated container is not widthed", function () {
                makeCt({
                    floated: true,
                    autoSize: false,
                    height: 150
                });

                ct.show();

                expect(ct.getAutoSize()).toBe(true);
                expect(ct).toHaveLayout({
                    el: {xywh: '0 0 200 150'},
                    bodyElement: {xywh: '0 0 200 150'}
                });
                expect(ct.bodySizerElement == null).toBe(true);
            });

            it("should force autoSize to true if the floated container is not heighted", function () {
                makeCt({
                    floated: true,
                    autoSize: false,
                    width: 300
                });

                ct.show();

                expect(ct.getAutoSize()).toBe(true);
                expect(ct).toHaveLayout({
                    el: {xywh: '0 0 300 100'},
                    bodyElement: {xywh: '0 0 300 100'}
                });
                expect(ct.bodySizerElement == null).toBe(true);
            });

            it("should respect the user's autoSize config if the floated container is sized on both axes", function() {
                makeCt({
                    floated: true,
                    autoSize: false,
                    width: 300,
                    height: 200
                });

                ct.show();

                expect(ct.getAutoSize()).toBe(false);
                expect(ct).toHaveLayout({
                    el: {xywh: '0 0 300 200'},
                    bodySizerElement: {xywh: '0 0 300 200'},
                    bodyElement: {xywh: '0 0 300 200'}
                });
            });
        });

        describe("positioned", function() {
            var parentCt;
            
            function makeCt(cfg) {
                parentCt = new Ext.Container({
                    renderTo: Ext.getBody(),
                    width: 400,
                    height: 300,
                    items: Ext.apply({
                        items: [{
                            xtype: 'component',
                            style: 'width:200px;height:100px;'
                        }]
                    }, cfg)
                });

                ct = parentCt.items.getAt(0);
            }
            
            afterEach(function() {
                parentCt = Ext.destroy(parentCt);
            });

            it("should default to autoSize: true", function () {
                makeCt({
                    left: 50,
                    top: 30
                });

                expect(ct.getAutoSize()).toBe(true);
                expect(parentCt).toHaveLayout({
                    items: {
                        0: {
                            el: {xywh: '50 30 200 100'},
                            bodyElement: {xywh: '50 30 200 100'}
                        }
                    }
                });
                expect(ct.bodySizerElement == null).toBe(true);
            });

            it("should force autoSize to true if the positioned container is not sized on either axis", function() {
                makeCt({
                    left: 50,
                    top: 30,
                    autoSize: false
                });

                expect(ct.getAutoSize()).toBe(true);
                expect(parentCt).toHaveLayout({
                    items: {
                        0: {
                            el: {xywh: '50 30 200 100'},
                            bodyElement: {xywh: '50 30 200 100'}
                        }
                    }
                });
                expect(ct.bodySizerElement == null).toBe(true);
            });

            it("should force autoSize to true if the positioned container is not widthed", function () {
                makeCt({
                    left: 50,
                    top: 30,
                    autoSize: false,
                    height: 150
                });

                expect(ct.getAutoSize()).toBe(true);
                expect(parentCt).toHaveLayout({
                    items: {
                        0: {
                            el: {xywh: '50 30 200 150'},
                            bodyElement: {xywh: '50 30 200 150'}
                        }
                    }
                });
                expect(ct.bodySizerElement == null).toBe(true);
            });
            
            it("should force autoSize to true if the positioned container is not heighted", function () {
                makeCt({
                    left: 50,
                    top: 30,
                    autoSize: false,
                    width: 300
                });

                expect(ct.getAutoSize()).toBe(true);
                expect(parentCt).toHaveLayout({
                    items: {
                        0: {
                            el: {xywh: '50 30 300 100'},
                            bodyElement: {xywh: '50 30 300 100'}
                        }
                    }
                });
                expect(ct.bodySizerElement == null).toBe(true);
            });

            it("should respect the user's autoSize config if the positioned container is sized on both axes", function() {
                makeCt({
                    left: 50,
                    top: 30,
                    autoSize: false,
                    width: 300,
                    height: 200
                });

                ct.show();

                expect(ct.getAutoSize()).toBe(false);
                expect(parentCt).toHaveLayout({
                    items: {
                        0: {
                            el: {xywh: '50 30 300 200'},
                            bodySizerElement: {xywh: '50 30 300 200'},
                            bodyElement: {xywh: '50 30 300 200'}
                        }
                    }
                });
            });
        });

        describe("centered", function() {
            var parentCt;

            function makeCt(cfg) {
                parentCt = new Ext.Container({
                    renderTo: Ext.getBody(),
                    width: 400,
                    height: 300,
                    items: Ext.apply({
                        centered: true,
                        items: [{
                            xtype: 'component',
                            style: 'width:200px;height:100px;'
                        }]
                    }, cfg)
                });

                ct = parentCt.items.getAt(0);
            }

            afterEach(function () {
                parentCt = Ext.destroy(parentCt);
            });

            it("should default to autoSize: true", function () {
                makeCt();

                expect(ct.getAutoSize()).toBe(true);
                expect(parentCt).toHaveLayout({
                    items: {
                        0: {
                            el: {xywh: '100 100 200 100'},
                            bodyElement: {xywh: '100 100 200 100'}
                        }
                    }
                });
                expect(ct.bodySizerElement == null).toBe(true);
            });

            it("should force autoSize to true if the centered container is not sized on either axis", function () {
                makeCt({
                    autoSize: false
                });

                expect(ct.getAutoSize()).toBe(true);
                expect(parentCt).toHaveLayout({
                    items: {
                        0: {
                            el: {xywh: '100 100 200 100'},
                            bodyElement: {xywh: '100 100 200 100'}
                        }
                    }
                });
                expect(ct.bodySizerElement == null).toBe(true);
            });

            it("should force autoSize to true if the centered container is not widthed", function () {
                makeCt({
                    autoSize: false,
                    height: 150
                });

                expect(ct.getAutoSize()).toBe(true);
                expect(parentCt).toHaveLayout({
                    items: {
                        0: {
                            el: {xywh: '100 75 200 150'},
                            bodyElement: {xywh: '100 75 200 150'}
                        }
                    }
                });
                expect(ct.bodySizerElement == null).toBe(true);
            });

            it("should force autoSize to true if the centered container is not heighted", function () {
                makeCt({
                    autoSize: false,
                    width: 300
                });

                expect(ct.getAutoSize()).toBe(true);
                expect(parentCt).toHaveLayout({
                    items: {
                        0: {
                            el: {xywh: '50 100 300 100'},
                            bodyElement: {xywh: '50 100 300 100'}
                        }
                    }
                });
                expect(ct.bodySizerElement == null).toBe(true);
            });

            it("should respect the user's autoSize config if the centered container is sized on both axes", function () {
                makeCt({
                    autoSize: false,
                    width: 300,
                    height: 200
                });

                ct.show();

                expect(ct.getAutoSize()).toBe(false);
                expect(parentCt).toHaveLayout({
                    items: {
                        0: {
                            el: {xywh: '50 50 300 200'},
                            bodySizerElement: {xywh: '50 50 300 200'},
                            bodyElement: {xywh: '50 50 300 200'}
                        }
                    }
                });
            });
        });
    });
});
