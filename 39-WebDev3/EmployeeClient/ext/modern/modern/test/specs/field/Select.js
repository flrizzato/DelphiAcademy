topSuite("Ext.field.Select", ['Ext.data.ArrayStore', 'Ext.viewport.Default', 'Ext.app.ViewModel'], function() {
    var field, 
        viewport,
        store,
        picker,
        pickerStore,
        selModel,
        valueCollection,
        createField = function(config) {
            var s;

            if (field) {
                field.destroy();
            }

            field = Ext.create('Ext.field.Select', Ext.apply({
                // Tests use the floated version
                picker: 'floated'
            }, config));

            s = field.getStore();
            if (s) {
                store = s;
            }
            valueCollection = field.getValueCollection();

            Ext.override(field, {
                getPicker: function () {
                    picker = this.callParent(arguments);

                    // It's a picker which contains a "slot" which is a List.
                    if (picker.isPicker) {
                        selModel = picker.innerItems[0].getSelectable();
                    }
                    // It's a floated BoundList
                    else {
                        selModel = picker.getSelectable();
                    }

                    return picker;
                },

                updateStore: function() {
                    this.callParent(arguments);
                    pickerStore = this._pickerStore;
                }
            });
        },
        synchronousLoad = true,
        proxyStoreLoad = Ext.data.ProxyStore.prototype.load,
        loadStore;

    function getByVal(val) {
        var index = store.findExact('value', val);
        return store.getAt(index);
    }

    beforeEach(function() {
        // Override so that we can control asynchronous loading
        loadStore = Ext.data.ProxyStore.prototype.load = function() {
            proxyStoreLoad.apply(this, arguments);
            if (synchronousLoad) {
                this.flushLoad.apply(this, arguments);
            }
            return this;
        };
    });

    afterEach(function() {
        var toDestroy = [ Ext.Viewport ];

        // Undo the overrides.
        Ext.data.ProxyStore.prototype.load = proxyStoreLoad;

        if (field) {
            toDestroy.push(field);
        }
        if (store) {
            toDestroy.push(store);
        }
        if (viewport && viewport !== Ext.Viewport) {
            toDestroy.push(viewport);
        }
        viewport = Ext.Viewport = Ext.destroy(toDestroy);
    });

    describe("configurations", function() {
        describe('clearable', function () {
            it('should respond properly to the clear trigger', function () {
                createField({
                    clearable: true,
                    options: [
                        { text: 'One', value: 1 },
                        { text: 'Two', value: 2 },
                        { text: 'Three', value: 3 }
                    ]
                });
                field.render(Ext.getBody());

                var trigger = field.getTriggers().clear;

                field.setValue(2);

                var v = field.getInputValue();
                var vc = field.getValueCollection();

                expect(v).toBe('Two');
                expect(trigger.isVisible()).toBe(true);
                expect(vc.length).toBe(1);

                field.onClearIconTap();

                v = field.getValue();

                expect(v).toBe(null);
                expect(trigger.isVisible()).toBe(false);

                expect(vc.length).toBe(0);
            });
        });

        describe('Checking required field value', function() {
            it('should be valid when it has a value selected', function() {
                createField({
                    required: false,
                    renderTo: document.body,
                    options: [
                        { text: 'One', value: 1 },
                        { text: 'Two', value: 2 },
                        { text: 'Three', value: 3 }
                    ]
                });
                expect(field.isValid()).toBe(true);

                field.setRequired(true);

                expect(field.isValid()).toBe(false);

                field.setValue(1);

                expect(field.isValid()).toBe(true);
            });
        });

        describe("options", function() {
            beforeEach(function() {
                createField({
                    autoSelect: 'initial',
                    options: [
                        {text: 'One', value: 1},
                        {text: 'Two', value: 2},
                        {text: 'Three', value: 3}
                    ]
                });
            });

            it("should create a store with 3 items", function() {
                expect(field.getStore().getCount()).toEqual(3);
            });

            it("should set the value configuration to the first item", function() {
                expect(field.getSelection()).toEqual(field.getStore().getAt(0));
            });

            describe("with value", function() {
                beforeEach(function() {
                    createField({
                        value: 3,
                        options: [
                            {text: 'One', value: 1},
                            {text: 'Two', value: 2},
                            {text: 'Three', value: 3}
                        ]
                    });
                });

                it("should create a store with 3 items", function() {
                    expect(field.getStore().getCount()).toEqual(3);
                });

                it("should set the value configuration to the third item", function() {
                    expect(field.getSelection()).toEqual(field.getStore().getAt(2));
                    expect(field.getSelection().get('value')).toEqual(3);
                    expect(field.getSelection().get('text')).toEqual('Three');
                    expect(field.getValue()).toEqual(3);
                });
            });
        });

        describe("store", function() {
            beforeEach(function() {
                createField({
                    store: {
                        fields: ['text', 'value'],
                        data: [
                            {text: 'One', value: 1},
                            {text: 'Two', value: 2},
                            {text: 'Three', value: 3}
                        ]
                    }
                });
            });

            it("should create a store with 3 items", function() {
                expect(field.getStore().getCount()).toEqual(3);
            });

            describe("with value", function() {
                beforeEach(function() {
                    createField({
                        value: 3,
                        store: {
                            fields: ['text', 'value'],
                            data: [
                                {text: 'One', value: 1},
                                {text: 'Two', value: 2},
                                {text: 'Three', value: 3}
                            ]
                        }
                    });
                });

                it("should create a store with 3 items", function() {
                    expect(field.getStore().getCount()).toEqual(3);
                });

                it("should set the value configuration to the third item", function() {
                    expect(field.getSelection()).toEqual(field.getStore().getAt(2));
                    expect(field.getSelection().get('value')).toEqual(3);
                    expect(field.getSelection().get('text')).toEqual('Three');
                    expect(field.getValue()).toEqual(3);
                });
            });
        });

        describe("value", function() {
            describe("0", function() {
                beforeEach(function() {
                    createField({
                        value: 0,
                        options: [
                            {text: 'One', value: 0},
                            {text: 'Two', value: 1},
                            {text: 'Three', value: 2}
                        ]
                    });
                });

                it("should set the value after adding options", function() {
                    expect(field.getValue()).toEqual(0);
                });
            });

            describe("1", function() {
                beforeEach(function() {
                    createField({
                        value: 1,
                        options: [
                            {text: 'One', value: 0},
                            {text: 'Two', value: 1},
                            {text: 'Three', value: 2}
                        ]
                    });
                });

                it("should set the value after adding options", function() {
                    expect(field.getValue()).toEqual(1);
                });
            });

            describe("false", function() {
                beforeEach(function() {
                    createField({
                        value: false,
                        options: [
                            {text: 'One', value: false},
                            {text: 'Two', value: 1},
                            {text: 'Three', value: 2}
                        ]
                    });
                });

                it("should set the value after adding options", function() {
                    expect(field.getValue()).toEqual(false);
                });
            });

            describe("default value", function() {
                describe("none", function() {
                    beforeEach(function() {
                        createField({
                            autoSelect: 'initial'
                        });
                    });

                    it("should set the value after adding options", function() {
                        expect(field.getValue()).toEqual(null);

                        field.setStore({
                            fields: ['text', 'value'],
                            data: [
                                {text: 'One', value: 1},
                                {text: 'Two', value: 2},
                                {text: 'Three', value: 3}
                            ]
                        });

                        //autoSelect
                        expect(field.getValue()).toEqual(1);
                    });
                });

                describe("value", function() {
                    beforeEach(function() {
                        createField({
                            value: 3
                        });
                    });

                    it("should set the value after adding options", function() {
                        expect(field.getValue()).toEqual(3);

                        field.setStore({
                            fields: ['text', 'value'],
                            data: [
                                {text: 'One', value: 1},
                                {text: 'Two', value: 2},
                                {text: 'Three', value: 3}
                            ]
                        });

                        expect(field.getValue()).toEqual(3);
                        expect(field.getSelection().data.text).toEqual('Three');
                    });
                });

                describe("setValue", function() {
                    describe("with value and store", function() {
                        beforeEach(function() {
                            createField({
                                value: 3,
                                clearable: true,
                                store: {
                                    fields: ['text', 'value'],
                                    data: [
                                        {text: 'One', value: 1},
                                        {text: 'Two', value: 2},
                                        {text: 'Three', value: 3}
                                    ]
                                }
                            });
                        });

                        it("should set to null", function() {
                            expect(field.getValue()).toEqual(3);
                            field.setValue(null);
                            expect(field.getValue()).toEqual(null);
                        });
                    });

                    describe("with no value", function() {
                        beforeEach(function() {
                            createField({
                                autoSelect: 'initial'
                            });
                        });

                        it("should set to null", function() {
                            expect(field.getValue()).toEqual(null);
                            field.setStore({
                                fields: ['text', 'value'],
                                data: [
                                    {text: 'One', value: 1},
                                    {text: 'Two', value: 2},
                                    {text: 'Three', value: 3}
                                ]
                            });
                            expect(field.getValue()).toEqual(1);
                            field.setValue(null);
                            expect(field.getValue()).toEqual(null);
                        });
                    });

                    describe("with value and late store", function(){
                        beforeEach(function() {
                            createField({
                                clearable: true
                            });
                        });

                        it("should preserve value until store is set", function() {
                            expect(field.getValue()).toEqual(null);
                            field.setValue(2);
                            expect(field.getValue()).toEqual(2);
                            field.setStore({
                                fields: ['text', 'value'],
                                data: [
                                    {text: 'One', value: 1},
                                    {text: 'Two', value: 2},
                                    {text: 'Three', value: 3}
                                ]
                            });
                            expect(field.getValue()).toEqual(2);
                            field.setValue(null);
                            expect(field.getValue()).toEqual(null);
                        });
                    });

                    describe("with 0 value and late store", function(){
                        beforeEach(function() {
                            createField({
                                clearable: true
                            });
                        });

                        it("should preserve value until store is set", function() {
                            expect(field.getValue()).toEqual(null);
                            field.setValue(0);
                            expect(field.getValue()).toEqual(0);
                            field.setStore({
                                fields: ['text', 'value'],
                                data: [
                                    {text: 'One', value: 1},
                                    {text: 'Two', value: 0},
                                    {text: 'Three', value: 3}
                                ]
                            });
                            expect(field.getValue()).toEqual(0);
                            field.setValue(null);
                            expect(field.getValue()).toEqual(null);
                        });
                    });

                    describe("with false value and late store", function(){
                        beforeEach(function() {
                            createField({
                                clearable: true
                            });
                        });

                        it("should preserve value until store is set", function() {
                            expect(field.getValue()).toEqual(null);
                            field.setValue(false);
                            expect(field.getValue()).toEqual(false);
                            field.setStore({
                                fields: ['text', 'value'],
                                data: [
                                    {text: 'One', value: 1},
                                    {text: 'Two', value: false},
                                    {text: 'Three', value: 3}
                                ]
                            });
                            expect(field.getValue()).toEqual(false);
                            field.setValue(null);
                            expect(field.getValue()).toEqual(null);
                        });
                    });
                });
            });
        });

        describe("autoSelect", function() {
            describe("when on", function() {
                beforeEach(function() {
                    createField({
                        autoSelect: true,
                        store: {
                            fields: ['text', 'value'],
                            data: [
                                {text: 'One', value: 1},
                                {text: 'Two', value: 2},
                                {text: 'Three', value: 3}
                            ]
                        }
                    });
                });

                it("should set the value configuration to the first item", function() {
                    expect(field.getSelection()).toEqual(field.getStore().getAt(0));
                });
            });

            describe("when off", function() {
                beforeEach(function() {
                    createField({
                        autoSelect: false,
                        store: {
                            fields: ['text', 'value'],
                            data: [
                                {text: 'One', value: 1},
                                {text: 'Two', value: 2},
                                {text: 'Three', value: 3}
                            ]
                        }
                    });
                });

                it("should set the value to null", function() {
                    expect(field.getSelection()).toEqual(null);
                });
            });
        });
    });

    describe("TOUCH-2431", function() {
        it("should use store configuration", function() {
            Ext.define('Ext.MySelect', {
                extend: 'Ext.field.Select',

                config: {
                    store: {
                        fields: ['name', 'value'],
                        data: [
                            {
                                name: 'one',
                                value: 1
                            },
                            {
                                name: 'two',
                                value: 2
                            }
                        ]
                    }
                }
            });

            var select = Ext.create('Ext.MySelect');
            expect(select.getStore().getCount()).toEqual(2);
            
            select.destroy();
        });
    });

    describe("events", function() {
        describe("change", function() {
            describe("without options", function() {
                beforeEach(function() {
                    createField({
                        autoSelect: 'initial'
                    });
                });

                it("should only fire change once when adding options", function() {
                    var spy = jasmine.createSpy();

                    field.on('change', spy);

                    field.setOptions([
                        {text: 'One', value: 1},
                        {text: 'Two', value: 2},
                        {text: 'Three', value: 3}
                    ]);

                    expect(spy.callCount).toBe(1);
                });
            });

            describe("with options", function() {
                beforeEach(function() {
                    createField({
                        autoSelect: 'initial',
                        options: [
                            {text: 'One', value: 1},
                            {text: 'Two', value: 2},
                            {text: 'Three', value: 3}
                        ]
                    });
                });

                it("should fire when you change the value", function() {
                    var spy = jasmine.createSpy();

                    field.on('change', spy);

                    field.setValue(2);

                    expect(spy.callCount).toBe(1);
                });

                it("should not fire when you dont change the value", function() {
                    var spy = jasmine.createSpy();

                    field.on('change', spy);

                    field.setValue(1);

                    expect(spy).not.toHaveBeenCalled();
                });
            });
        });
    });

    describe("methods", function() {
        describe("reset", function() {
            describe("when autoSelect is on", function() {
                beforeEach(function() {
                    createField({
                        autoSelect: true,
                        store: {
                            fields: ['text', 'value'],
                            data: [
                                { text: 'One',   value: 1 },
                                { text: 'Two',   value: 2 },
                                { text: 'Three', value: 3 }
                            ]
                        }
                    });

                    field.setValue(3);
                });

                it("should set the value configuration to the first item", function() {
                    field.reset();

                    expect(field.getSelection()).toBe(field.getStore().getAt(0));
                });
            });

            describe("when autoSelect is off", function() {
                beforeEach(function() {
                    createField({
                        autoSelect : false,
                        store      : {
                            fields  : ['text', 'value'],
                            data    : [
                                { text : 'One',   value : 1 },
                                { text : 'Two',   value : 2 },
                                { text : 'Three', value : 3 }
                            ]
                        }
                    });

                    field.setValue(3);
                });

                it("should set the value to null", function() {
                    field.reset();

                    expect(field.getSelection()).toBe(null);
                });
            });
        });

        describe("showPicker", function() {
            beforeEach(function() {
                viewport = Ext.Viewport = new Ext.viewport.Default({
                    layout: 'auto'
                });

                createField({
                    picker: 'floated',
                    store: {
                        fields: ['text', 'value'],
                        data: (function() {
                            var data = [], i;
                            for(i=0; i<100; i++) {
                                data.push({text: i, value: i});
                            }
                            return data;
                        })()
                    }
                });

                viewport.add(field);
            });

            it("should scroll to initial value", function() {
                var scrollComplete = false,
                    resizeSpy = spyOn(field, 'realignFloatedPicker').andCallThrough(),
                    item, scroller, scrollHeight, scrollMin, scrollMax,
                    offset, picker, list;

                field.setValue(45);
                field.showPicker();
                
                picker = field.getPicker();
                list = picker;
                scroller = list.getScrollable();

                scroller.on('scrollend', function() {
                    scrollComplete = true;
                }); 

                waitsFor(function() {
                    return scrollComplete;
                }, 'slot to scroll selection into view', 800);
                
                waitsForSpy(resizeSpy);

                runs(function() {
                    item = list.getItemAt(45);
                    scrollHeight = picker.element.getHeight();
                    scrollMin = scroller.getPosition().y;                    
                    scrollMax = scrollMin+scrollHeight;
                    offset = item.renderElement.dom.offsetTop;
                });
            });

            it("should scroll to selected value", function() {
                var scrollEndSpy,
                    scrollSpy = spyOn(field, 'setPickerLocation').andCallThrough(),
                    item, scroller, scrollHeight, scrollMin, scrollMax,
                    offset, picker, list;

                field.showPicker();
                picker = field.getPicker();
                picker.refresh();
                list = picker;
                scroller = list.getScrollable();
                scrollEndSpy = spyOnEvent(scroller, 'scrollend');

                field.setValue(78);

                waitsForSpy(scrollSpy, 'slot to scroll selection into view', 800);
                runs(function() {
                    item = list.getItemAt(78);
                    scrollHeight = picker.element.getHeight();
                    scrollMin = scroller.getPosition().y;                    
                    scrollMax = scrollMin+scrollHeight;
                    offset = item.renderElement.dom.offsetTop;

                    expect(scrollSpy).toHaveBeenCalled();
                    // should be between the current scroll position and max height of scrollable area
                    expect(offset >= scrollMin && offset <= scrollMax).toBeTruthy();
                });
            });
        });

        describe('should edge picker', function () {
            beforeEach(function() {
                viewport = Ext.Viewport = new Ext.viewport.Default();

                createField({
                    picker: 'edge',
                    store: {
                        fields: ['text', 'value'],
                        data: (function() {
                            var data = [], i;
                            for(i=0; i<100; i++) {
                                data.push({text: i, value: i});
                            }
                            return data;
                        })()
                    }
                });
            });

            it('should open edge picker', function () {
                field.showPicker();

                var picker = field.getPicker();

                expect(picker.rendered).toBeTruthy();
                expect(field.pickerType).toBe('edge');
            });
        });
    });

    describe('With valueField: null', function() {
        beforeEach(function () {
            store = Ext.create('Ext.data.Store');
            for (var i = 0; i < 10; ++i) {
                store.add({ title: 'item-' + i });
            }

            viewport = Ext.Viewport = new Ext.viewport.Default();
        });

        it('should be able to have a value set via the selection', function() {
            var container = Ext.Viewport.add({
                xtype: 'container',

                items: [{
                    // NO store + initial selection -> exception
                    itemId: 'selectfield-1',
                    xtype: 'selectfield',
                    displayField: 'title',
                    valueField: null,
                    selection: store.first()
                }]
            }),
            s1 = container.down('#selectfield-1');

            // Value comes from selection, so first two have values
            // the valueField defaults to the displayField so that a field value
            // is always published.
            expect(s1.getValue()).toBe('item-0');

            // The selection of the first two are set
            // The last one will only arrive when the bound data
            // arrives later.
            expect(s1.getSelection()).toBe(store.first());

            expect(s1.inputElement.dom.value).toBe('item-0');
        });

        it('should be able to have a value set via the selection w/store', function() {
            var container = Ext.Viewport.add({
                xtype: 'container',

                items: [{
                    // store + initial selection -> fail
                    // selected item should be first one
                    itemId: 'selectfield-2',
                    xtype: 'selectfield',
                    displayField: 'title',
                    valueField: null,
                    selection: store.getAt(1),
                    store: store
                }]
            }),
            s2 = container.down('#selectfield-2');

            // Value comes from selection, so first two have values
            // the valueField defaults to the displayField so that a field value
            // is always published.
            expect(s2.getValue()).toBe('item-1');

            // The selection of the first two are set
            // The last one will only arrive when the bound data
            // arrives later.
            expect(s2.getSelection()).toBe(store.getAt(1));

            expect(s2.inputElement.dom.value).toBe('item-1');
        });

        it('should be able to have a value set though binding the selection', function() {
            var container = Ext.Viewport.add({
                xtype: 'container',
                viewModel: {
                    data: {
                        the_store: null,
                        the_selection: null
                    }
                },

                items: [{
                    // Bindings doesn't work
                    itemId: 'selectfield-3',
                    xtype: 'selectfield',
                    displayField: 'title',
                    valueField: null,
                    bind: {
                        selection: '{the_selection}',
                        store: '{the_store}'
                    }
                }]
            }),
            s3 = container.down('#selectfield-3');

            // Value comes from selection, so first two have values
            // the valueField defaults to the displayField so that a field value
            // is always published.
            expect(s3.getValue()).toBe(null);

            // The selection of the first two are set
            // The last one will only arrive when the bound data
            // arrives later.
            expect(s3.getSelection()).toBe(null);

            // Set the store which will propagate into the select fields
            // and the selection which will propagate into s3
            container.getViewModel().set({
                the_selection: store.getAt(2),
                the_store: store
            });

            // The arrival of the store on the next bind tick should
            // set the values of the fields and the selection of s3.
            waitsFor(function() {
                return s3.inputElement.dom.value === 'item-2'
                    && s3.getSelection() === store.getAt(2);
            }, 'store arrival to trigger field value reconciliation', 500);
        });
    });
    
    describe("pointer interaction", function() {
        var picker, item;
        
        beforeEach(function() {
            createField({
                renderTo: Ext.getBody(),
                options: [
                    { text: 'One', value: 1 },
                    { text: 'Two', value: 2 },
                    { text: 'Three', value: 3 }
                ]
            });
        });
        
        afterEach(function() {
            picker = item = null;
        });
        
        it("should select value when item is clicked", function() {
            field.expand();
            
            picker = field.getPicker();
            picker.refresh();
            item = picker.getViewItems()[1];
            
            jasmine.fireMouseEvent(item.el, 'click');
            
            expect(field.getValue()).toBe(2);
        });
    });

    describe("binding", function() {
        var CBTestModel = Ext.define(null, {
                extend: 'Ext.data.Model',
                fields: [
                    {type: 'string', name: 'text'},
                    {type: 'string', name: 'value'}
                ]
            }),
            preventStore, viewModel, spy;

        beforeEach(function() {
            spy = jasmine.createSpy();
            viewModel = new Ext.app.ViewModel();

            Ext.define('spec.MyStore',{
                extend : 'Ext.data.Store',
                alias : 'store.foo',
                proxy: {
                    type: 'memory'
                },
                model: CBTestModel,
                data: [
                    {id: 1, text: 'text 1', value: 'value 1'},
                    {id: 2, text: 'text 2', value: 'value 2'},
                    {id: 3, text: 'text 3', value: 'value 3'},
                    {id: 4, text: 'text 31', value: 'value 31'},
                    {id: 5, text: 'text 32', value: 'value 32'},
                    {id: 6, text: 'text 33', value: 'value 33'},
                    {id: 7, text: 'text 34', value: 'value 34'},
                    {id: 8, text: 'Foo', value: 'foo1'},
                    {id: 9, text: 'Foo', value: 'foo2'}
                ]
            });
            store = new spec.MyStore();
        });

        afterEach(function() {
            Ext.undefine('spec.MyStore');
            spy = viewModel = null;
        });

        function makeViewModelSelectField(cfg) {
            var config = Ext.apply({
                width: 200,
                name: 'test',
                store: preventStore ? null : store,
                picker: 'floated',
                displayField: 'text',
                valueField: 'value',
                viewModel: viewModel,
                renderTo: Ext.getBody()
            }, cfg);
            field = new Ext.field.Select(config);
        }

        describe("view model selection", function() {
            function selectNotify(rec) {
                field.expand();
                field.getPicker().select(rec);
                viewModel.notify();
                field.collapse();
            }

            describe("reference", function() {
                describe("no initial value", function() {
                    beforeEach(function() {
                        viewModel.bind('{userList.selection}', spy);
                        makeViewModelSelectField({
                            autoSelect: false,
                            reference: 'userList'
                        });
                        viewModel.notify();
                    });

                    it("should publish null by default", function() {
                        var args = spy.mostRecentCall.args;
                        expect(args[0]).toBeNull();
                        expect(args[1]).toBeUndefined();
                    });

                    it("should publish the value when selected", function() {
                        var rec = getByVal('value 1');
                        selectNotify(rec);
                        var args = spy.mostRecentCall.args;
                        expect(args[0]).toBe(rec);
                        expect(args[1]).toBeNull();
                        expect(field.getValue()).toBe('value 1');
                    });

                    it("should publish when the selection is changed", function() {
                        var rec1 = getByVal('value 1'),
                            rec2 = getByVal('value 2');

                        selectNotify(rec1);
                        spy.reset();
                        selectNotify(rec2);
                        var args = spy.mostRecentCall.args;
                        expect(args[0]).toBe(rec2);
                        expect(args[1]).toBe(rec1);
                        expect(field.getValue()).toBe('value 2');
                    });

                    it("should publish the record when setting the value", function() {
                        field.setValue('value 1');
                        viewModel.notify();
                        var args = spy.mostRecentCall.args;
                        expect(args[0]).toBe(getByVal('value 1'));
                        expect(args[1]).toBeNull();
                    });

                    it("should publish the record when the value is changed", function() {
                        field.setValue('value 1');
                        viewModel.notify();
                        spy.reset();
                        field.setValue('value 2');
                        viewModel.notify();
                        var args = spy.mostRecentCall.args;
                        expect(args[0]).toBe(getByVal('value 2'));
                        expect(args[1]).toBe(getByVal('value 1'));
                    });

                    it("should publish the record when the value is cleared", function() {
                        field.setValue('value 1');
                        viewModel.notify();
                        spy.reset();
                        field.setValue(null);
                        viewModel.notify();
                        var args = spy.mostRecentCall.args;
                        expect(args[0]).toBeNull();
                        expect(args[1]).toBe(getByVal('value 1'));
                    });
                });

                describe("with initial value", function() {
                    beforeEach(function() {
                        viewModel.bind('{userList.selection}', spy);
                        makeViewModelSelectField({
                            reference: 'userList',
                            value: 'value 2'
                        });
                        viewModel.notify();
                    });

                    it("should publish the record", function() {
                        var args = spy.mostRecentCall.args;
                        expect(args[0]).toBe(getByVal('value 2'));
                        expect(args[1]).toBeUndefined();
                    });
                });
            });

            describe("two way binding", function() {
                describe("no initial value", function() {
                    beforeEach(function() {
                        viewModel.bind('{foo}', spy);
                        makeViewModelSelectField({
                            bind: {
                                selection: '{foo}'
                            }
                        });
                        viewModel.notify();
                    });

                    describe("changing the selection", function() {
                        it("should trigger the binding when adding a selection", function() {
                            var rec = getByVal('value 1');

                            selectNotify(rec);

                            var args = spy.mostRecentCall.args;

                            expect(args[0]).toBe(rec);
                            expect(args[1]).toBeUndefined();
                        });

                        it("should trigger the binding when changing the selection", function() {
                            var rec1 = getByVal('value 1'),
                                rec2 = getByVal('value 2');

                            selectNotify(rec1);
                            spy.reset();
                            selectNotify(rec2);
                            var args = spy.mostRecentCall.args;
                            expect(args[0]).toBe(rec2);
                            expect(args[1]).toBe(rec1);
                        });

                        it("should trigger the binding when setting the value", function() {
                            field.setValue('value 1');
                            viewModel.notify();
                            var args = spy.mostRecentCall.args;
                            expect(args[0]).toBe(getByVal('value 1'));
                            expect(args[1]).toBeUndefined();
                        });

                        it("should trigger the binding when the value is changed", function() {
                            field.setValue('value 1');
                            viewModel.notify();
                            spy.reset();
                            field.setValue('value 2');
                            viewModel.notify();
                            var args = spy.mostRecentCall.args;
                            expect(args[0]).toBe(getByVal('value 2'));
                            expect(args[1]).toBe(getByVal('value 1'));
                        });

                        it("should trigger the binding when the value is cleared", function() {
                            field.setValue('value 1');
                            viewModel.notify();
                            spy.reset();
                            field.setValue(null);
                            viewModel.notify();
                            var args = spy.mostRecentCall.args;
                            expect(args[0]).toBeNull();
                            expect(args[1]).toBe(getByVal('value 1'));
                        });
                    });

                    describe("changing the view model value", function() {
                        it("should set the value when setting the record", function() {
                            var rec = getByVal('value 1');
                            viewModel.set('foo', rec);
                            viewModel.notify();
                            expect(field.getValue()).toBe('value 1');
                        });

                        it("should set the value when updating the record", function() {
                            viewModel.set('foo', getByVal('value 1'));
                            viewModel.notify();
                            viewModel.set('foo', getByVal('value 2'));
                            viewModel.notify();
                            expect(field.getValue()).toBe('value 2');
                        });

                        it("should deselect when clearing the value", function() {
                            viewModel.set('foo', getByVal('value 1'));
                            viewModel.notify();
                            viewModel.set('foo', null);
                            viewModel.notify();
                            expect(field.getValue()).toBeNull();
                        });
                    });
                });

                // Not sure if we want to support this, leave this out for now
                xdescribe("with initial value", function() {
                    it("should trigger the binding with an initial value in the select field", function() {
                        viewModel.bind('{foo}', spy);
                        makeViewModelSelectField({
                            value: 'value 2',
                            bind: {
                                selection: '{foo}'
                            }
                        });
                        viewModel.notify();
                        var args = spy.mostRecentCall.args;
                        expect(args[0]).toBe(getByVal('value 2'));
                        expect(args[1]).toBeUndefined();
                    });
                });

                describe("reloading the store", function() {
                    beforeEach(function() {
                        MockAjaxManager.addMethods();
                        viewModel.bind('{foo}', spy);
                        makeViewModelSelectField({
                            bind: {
                                selection: '{foo}'
                            }
                        });
                        viewModel.notify();

                        selectNotify(getByVal('value 1'));
                        spy.reset();

                        store.setProxy({
                            type: 'ajax',
                            url: 'fake'
                        });
                        store.load();
                    });

                    afterEach(function() {
                        MockAjaxManager.removeMethods();
                    });

                    describe("when the selected record is in the result set", function() {
                        it("should trigger the selection binding", function() {
                            field.expand();

                            Ext.Ajax.mockComplete({
                                status: 200,
                                responseText: Ext.encode([
                                    {id: 1, text: 'text 1', value: 'value 1'},
                                    {id: 2, text: 'text 2', value: 'value 2'}
                                ])
                            });

                            viewModel.notify();
                            field.expand();

                            // After the new record with the same ID arrives the selection must be
                            // synched to contain the new record by that ID.
                            expect(field.getPicker().getSelectable().getSelections()[0]).toBe(store.byValue.get('value 1'));
                            expect(spy.callCount).toBe(1);
                            expect(spy.mostRecentCall.args[0]).toBe(store.getAt(0));
                        });
                    });

                    describe("when the selected record is not in the result set", function() {
                        it("should trigger the selection binding", function() {
                            Ext.Ajax.mockComplete({
                                status: 200,
                                responseText: '[]'
                            });

                            viewModel.notify();
                            expect(spy.callCount).toBe(1);
                            expect(spy.mostRecentCall.args[0]).toBeNull();
                        });
                    });
                });
            });
        });
    });

    // TODO: multiselect
    xdescribe('multiSelect: true', function() {
        var createMultiSelectField = function(config) {
            config = Ext.apply({
                renderTo: document.body,
                multiSelect: true,
                autoSelect: false,
                options: [
                    'ExtJS',
                    'Javascript',
                    'CSS',
                    'Git',
                    'Java',
                    'PHP',
                    'COBOL',
                    'Node.js',
                    'JSON',
                    'HTML5',
                    'RIA',
                    'OOP',
                    'Scrum',
                    'REST',
                    'MVC'
                ]
            }, config);
            createField(config);
        };

        describe('General tests', function() {
            // TODO: Update this when https://sencha.jira.com/browse/EXT-486 is fixed
            // and we have a Chip dataview
            it('should update the value and the selected item UI upon record mutation', function() {
                createMultiSelectField();
                field.setValue('ExtJS');

                expect(field.getValue()).toEqual(['ExtJS']);

                // This wull not be right when https://sencha.jira.com/browse/EXT-486 is fixed
                expect(field.getInputValue()).toEqual('ExtJS');

                store.getAt(0).set({
                    value: 'Sencha',
                    text: 'Our Framework'
                });

                expect(field.getValue()).toEqual(['Sencha']);

                // This wull not be right when https://sencha.jira.com/browse/EXT-486 is fixed
                expect(field.getInputValue()).toEqual('Our Framework');
            });
        });

        describe('Adding to the valueCollection on BoundList item click', function() {
            it('should set the value to an array', function() {
                createMultiSelectField();
                field.expand();

                // Tapping on the tool should select.
                // The default List behaviour of ignoring tool taps
                // should be defeated by passive: true Tools
                Ext.testHelper.tap(picker.itemFromRecord(0).getTools()[0].el);

                expect(field.getValue()).toEqual([store.getAt(0).get(field.getValueField())]);
                expect(field.getSelection()).toEqual([store.getAt(0)]);

                // Tap the second item
                Ext.testHelper.tap(picker.itemFromRecord(1).el);

                // Now field has two values in the array
                expect(field.getValue()).toEqual([
                    store.getAt(0).get(field.getValueField()),
                    store.getAt(1).get(field.getValueField())
                ]);
                expect(field.getSelection()).toEqual([
                    store.getAt(0),
                    store.getAt(1)
                ]);

                // Should deselect item 0
                Ext.testHelper.tap(picker.itemFromRecord(0).getTools()[0].el);

                expect(field.getValue()).toEqual([store.getAt(1).get(field.getValueField())]);
                expect(field.getSelection()).toEqual([store.getAt(1)]);
            });
        });

        describe('Adding to the valueCollection on BoundList ENTER key', function() {
            it('should set the value to an array', function() {
                createMultiSelectField();
                focusAndWait(field);

                runs(function() {
                    jasmine.fireKeyEvent(field.inputElement, 'keydown', Ext.event.Event.DOWN);

                    // Select item 0
                    jasmine.fireKeyEvent(field.inputElement, 'keydown', Ext.event.Event.ENTER);

                    expect(field.getValue()).toEqual([store.getAt(0).get(field.getValueField())]);
                    expect(field.getSelection()).toEqual([store.getAt(0)]);

                    // Select the second item
                    jasmine.fireKeyEvent(field.inputElement, 'keydown', Ext.event.Event.DOWN);
                    jasmine.fireKeyEvent(field.inputElement, 'keydown', Ext.event.Event.ENTER);

                    // Now field has two values in the array
                    expect(field.getValue()).toEqual([
                        store.getAt(0).get(field.getValueField()),
                        store.getAt(1).get(field.getValueField())
                    ]);
                    expect(field.getSelection()).toEqual([
                        store.getAt(0),
                        store.getAt(1)
                    ]);

                    // Should deselect item 0
                    jasmine.fireKeyEvent(field.inputElement, 'keydown', Ext.event.Event.UP);
                    jasmine.fireKeyEvent(field.inputElement, 'keydown', Ext.event.Event.ENTER);

                    expect(field.getValue()).toEqual([store.getAt(1).get(field.getValueField())]);
                    expect(field.getSelection()).toEqual([store.getAt(1)]);
                });
            });
        });

        describe('filterPickList: true', function() {
            it("should remove selected records from the picker's store", function() {
                createMultiSelectField({
                    filterPickList: true
                });
                focusAndWait(field);

                runs(function() {
                    jasmine.fireKeyEvent(field.inputElement, 'keydown', Ext.event.Event.DOWN);
                    var pickerCount = picker.getStore().getCount();

                    // Select item 0
                    jasmine.fireKeyEvent(field.inputElement, 'keydown', Ext.event.Event.ENTER);

                    // And that should have disappeared from the picker.
                    expect(picker.getStore().getCount()).toBe(pickerCount - 1);

                    // Value collection should not be in the picker store
                    expect(picker.getStore().getData().containsAll(valueCollection.getRange())).toBe(false);

                    expect(field.getValue()).toEqual([store.getAt(0).get(field.getValueField())]);
                    expect(field.getSelection()).toEqual([store.getAt(0)]);

                    // Select the second item
                    jasmine.fireKeyEvent(field.inputElement, 'keydown', Ext.event.Event.ENTER);

                    // Now field has two values in the array
                    expect(field.getValue()).toEqual([
                        store.getAt(0).get(field.getValueField()),
                        store.getAt(1).get(field.getValueField())
                    ]);
                    expect(field.getSelection()).toEqual([
                        store.getAt(0),
                        store.getAt(1)
                    ]);

                    // And two should have gone from the picker
                    expect(picker.getStore().getCount()).toBe(pickerCount - 2);

                    // Value collection should not be in the picker store
                    expect(picker.getStore().getData().containsAll(valueCollection.getRange())).toBe(false);
                });
            });
        });

        describe("two way binding", function() {
            var selectionSpy, valueSpy, viewModel, container;

            beforeEach(function() {
                selectionSpy = jasmine.createSpy();
                valueSpy = jasmine.createSpy();
                viewModel = new Ext.app.ViewModel({
                    data: {
                        selection: undefined,
                        value: undefined,
                        skillsStore: undefined
                    }
                });
            });

            afterEach(function() {
                selectionSpy = valueSpy = viewModel = container = Ext.destroy(container);
            });

            describe('With options', function() {
                function makeViewModelMultiSelectField(cfg) {
                    var config = Ext.apply({
                        width: 200,
                        name: 'test',
                        options: [
                            'ExtJS',
                            'Javascript',
                            'CSS',
                            'Git',
                            'Java',
                            'PHP',
                            'COBOL',
                            'Node.js',
                            'JSON',
                            'HTML5',
                            'RIA',
                            'OOP',
                            'Scrum',
                            'REST',
                            'MVC'
                        ],
                        multiSelect: true,
                        autoSelect: false,
                        picker: 'floated',
                        viewModel: viewModel,
                        bind: {
                            selection: '{selection}',
                            value: '{value}'
                        },
                        renderTo: Ext.getBody()
                    }, cfg);
                    createField(config);
                }

                function selectNotify(rec, keepExisting) {
                    field.expand();
                    field.getPicker().select(rec, keepExisting);
                    viewModel.notify();
                    field.collapse();
                }

                describe("no initial value", function () {
                    beforeEach(function () {
                        viewModel.bind('{selection}', selectionSpy);
                        viewModel.bind('{value}', valueSpy);
                        makeViewModelMultiSelectField();
                        viewModel.notify();
                    });

                    describe("changing the selection", function () {
                        it("should trigger the binding when adding a selection", function () {
                            var rec = getByVal('ExtJS');

                            selectNotify(rec);

                            var args = selectionSpy.mostRecentCall.args;

                            expect(args[0]).toEqual([rec]);
                            expect(args[1]).toBeUndefined();
                        });

                        it("should trigger the binding when changing the selection", function () {
                            var rec1 = getByVal('ExtJS'),
                                rec2 = getByVal('COBOL');

                            selectNotify(rec1);
                            selectionSpy.reset();
                            selectNotify(rec2, true);
                            var args = selectionSpy.mostRecentCall.args;
                            expect(args[0]).toEqual([rec1, rec2]);
                            expect(args[1]).toEqual([rec1]);
                        });

                        it("should trigger the binding when setting the value", function () {
                            field.setValue('ExtJS');
                            viewModel.notify();
                            var args = selectionSpy.mostRecentCall.args;
                            expect(args[0]).toEqual([getByVal('ExtJS')]);
                            expect(args[1]).toBeUndefined();
                        });

                        it("should trigger the binding when the value is changed", function () {
                            var rec1 = getByVal('ExtJS'),
                                rec2 = getByVal('COBOL');

                            field.setValue('ExtJS');
                            viewModel.notify();
                            selectionSpy.reset();
                            field.setValue(['ExtJS', 'COBOL']);
                            viewModel.notify();
                            var args = selectionSpy.mostRecentCall.args;
                            expect(args[0]).toEqual([rec1, rec2]);
                            expect(args[1]).toEqual([rec1]);
                        });

                        it("should trigger the binding when the value is cleared", function () {
                            field.setValue('ExtJS');
                            viewModel.notify();
                            selectionSpy.reset();
                            field.setValue(null);
                            viewModel.notify();
                            var args = selectionSpy.mostRecentCall.args;
                            expect(args[0]).toBeNull();
                            expect(args[1]).toEqual([getByVal('ExtJS')]);
                        });
                    });

                    describe("setting the ViewModel selection property", function () {
                        it("should set the value when setting the record", function () {
                            var rec = getByVal('ExtJS');
                            viewModel.set('selection', rec);
                            viewModel.notify();
                            expect(field.getValue()).toEqual(['ExtJS']);
                            expect(viewModel.get('value')).toEqual(['ExtJS']);
                        });

                        it("should set the value when updating the record", function () {
                            viewModel.set('selection', [getByVal('ExtJS'), getByVal('COBOL')]);
                            viewModel.notify();
                            viewModel.set('selection', [getByVal('Java'), getByVal('PHP')]);
                            viewModel.notify();
                            expect(field.getValue()).toEqual(['Java', 'PHP']);
                            expect(viewModel.get('value')).toEqual(['Java', 'PHP']);
                        });

                        it("should deselect when clearing the value", function () {
                            viewModel.set('selection', getByVal('ExtJS'));
                            viewModel.notify();
                            viewModel.set('selection', null);
                            viewModel.notify();
                            expect(field.getValue()).toBeNull();
                            expect(viewModel.get('value')).toBeNull();
                        });
                    });

                    describe("setting the ViewModel value property", function () {
                        it("should set the value when setting the record", function () {
                            var rec = getByVal('ExtJS');
                            viewModel.set('value', 'ExtJS');
                            viewModel.notify();
                            expect(field.getValue()).toEqual(['ExtJS']);
                            expect(viewModel.get('selection')).toEqual([rec]);
                        });

                        it("should set the value when updating the record", function () {
                            viewModel.set('value', ['ExtJS', 'COBOL']);
                            viewModel.notify();
                            viewModel.set('value', ['Java', 'PHP']);
                            viewModel.notify();
                            expect(field.getValue()).toEqual(['Java', 'PHP']);
                            expect(viewModel.get('selection')).toEqual([getByVal('Java'), getByVal('PHP')]);
                        });

                        it("should deselect when clearing the value", function () {
                            viewModel.set('value', 'ExtJS');
                            viewModel.notify();
                            viewModel.set('value', null);
                            viewModel.notify();
                            expect(field.getValue()).toBeNull();
                            expect(viewModel.get('selection')).toEqual(null);
                        });
                    });
                });

                // Not sure if we want to support this, leave this out for now
                xdescribe("with initial value", function () {
                    it("should trigger the binding with an initial value in the select field", function () {
                        viewModel.bind('{selection}', selectionSpy);
                        makeViewModelMultiSelectField({
                            value: 'COBOL'
                        });
                        viewModel.notify();
                        var args = selectionSpy.mostRecentCall.args;
                        expect(args[0]).toEqual([getByVal('COBOL')]);
                        expect(args[1]).toBeUndefined();
                    });
                });

                describe("reloading the store", function () {
                    beforeEach(function () {
                        MockAjaxManager.addMethods();
                        viewModel.bind('{selection}', selectionSpy);
                        makeViewModelMultiSelectField();
                        viewModel.notify();

                        selectNotify(getByVal('ExtJS'));
                        selectionSpy.reset();

                        store.setProxy({
                            type: 'ajax',
                            url: 'fake'
                        });
                        store.load();
                    });

                    afterEach(function () {
                        MockAjaxManager.removeMethods();
                    });

                    describe("when the selected record is in the result set", function () {
                        it("should trigger the selection binding", function () {
                            field.expand();

                            Ext.Ajax.mockComplete({
                                status: 200,
                                responseText: Ext.encode([
                                    {id: 'ExtJS', text: 'ExtJS', value: 'ExtJS'},
                                    {id: 'COBOL', text: 'COBOL', value: 'COBOL'}
                                ])
                            });

                            viewModel.notify();
                            field.expand();

                            // After the new record with the same ID arrives the selection must be
                            // synched to contain the new record by that ID.
                            expect(field.getPicker().getSelectable().getSelections()[0]).toBe(store.byValue.get('ExtJS'));
                            expect(selectionSpy.callCount).toBe(1);
                            expect(selectionSpy.mostRecentCall.args[0]).toEqual([store.getAt(0)]);
                            expect(viewModel.get('selection')).toEqual([store.byValue.get('ExtJS')]);
                        });
                    });

                    describe("when the selected record is not in the result set", function () {
                        it("should trigger the selection binding", function () {
                            Ext.Ajax.mockComplete({
                                status: 200,
                                responseText: '[]'
                            });

                            viewModel.notify();
                            expect(selectionSpy.callCount).toBe(1);
                            expect(selectionSpy.mostRecentCall.args[0]).toBeNull();
                        });
                    });
                });
            });

            describe('With bound store', function() {
                function makeViewModelMultiSelectField(cfg) {
                    var config = Ext.apply({
                        width: 200,
                        name: 'test',
                        multiSelect: true,
                        autoSelect: false,
                        picker: 'floated',
                        viewModel: viewModel,
                        options: [
                            'ExtJS',
                            'Javascript',
                            'CSS',
                            'Git',
                            'Java',
                            'PHP',
                            'COBOL',
                            'Node.js',
                            'JSON',
                            'HTML5',
                            'RIA',
                            'OOP',
                            'Scrum',
                            'REST',
                            'MVC'
                        ],
                        bind: {
                            selection: '{selection}',
                            value: '{value}',
                            store: '{skillsStore}'
                        },
                        renderTo: Ext.getBody()
                    }, cfg);
                    createField(config);

                    // Steal the Store that was auto-created from the options and
                    // put it into the ViewModel so that it arrives in the next tick.
                    // Remove the store from the field.
                    store = field.getStore();
                    store.setAutoDestroy(false);
                    field.setStore(null);
                    viewModel.set('skillsStore', store);

                    // Just making sure our initial conditions are what we are testing
                    expect(field.getStore()).toBe(null);
                }

                function selectNotify(rec, keepExisting) {
                    field.expand();
                    field.getPicker().select(rec, keepExisting);
                    viewModel.notify();
                    field.collapse();
                }

                describe("no initial value", function () {
                    beforeEach(function () {
                        viewModel.bind('{selection}', selectionSpy);
                        viewModel.bind('{value}', valueSpy);
                        makeViewModelMultiSelectField();
                        viewModel.notify();
                        viewModel.set('skillsStore', store);
                    });

                    describe("Setting the value before the store arrives", function () {
                        it("should trigger the binding when the store arrives and the value finds a match", function () {
                            field.setValue('ExtJS');

                            // No store yet, so can't have a selection
                            expect(selectionSpy.callCount).toBe(0);

                            viewModel.notify();

                            var args = selectionSpy.mostRecentCall.args;

                            expect(args[0]).toEqual([getByVal('ExtJS')]);
                            expect(args[1]).toBeUndefined();
                        });
                    });

                    describe("Setting the selection before the store arrives", function () {
                        it("should trigger the binding when the store arrives and the value finds a match", function () {
                            // Selection is a temporary, value holding record
                            field.setSelection(new Ext.data.Model({
                                id: 'ExtJS',
                                text: 'ExtJS',
                                value: 'ExtJS'
                            }));

                            viewModel.notify();

                            var args = selectionSpy.mostRecentCall.args;

                            // Selection should now be the real, matching record
                            expect(args[0]).toEqual([getByVal('ExtJS')]);
                            expect(args[1]).toBeUndefined();

                            args = valueSpy.mostRecentCall.args;

                            expect(args[0]).toEqual(['ExtJS']);
                            expect(args[1]).toBeUndefined();
                        });
                    });

                    describe("setting the ViewModel selection property", function () {
                        it("should set the value when setting the record", function () {
                            var rec = getByVal('ExtJS');
                            viewModel.set('selection', rec);
                            viewModel.notify();
                            expect(field.getValue()).toEqual(['ExtJS']);
                            expect(viewModel.get('value')).toEqual(['ExtJS']);
                        });

                        it("should set the value when updating the record", function () {
                            viewModel.set('selection', [getByVal('ExtJS'), getByVal('COBOL')]);
                            viewModel.notify();
                            viewModel.set('selection', [getByVal('Java'), getByVal('PHP')]);
                            viewModel.notify();
                            expect(field.getValue()).toEqual(['Java', 'PHP']);
                            expect(viewModel.get('value')).toEqual(['Java', 'PHP']);
                        });

                        it("should deselect when clearing the value", function () {
                            viewModel.set('selection', getByVal('ExtJS'));
                            viewModel.notify();
                            viewModel.set('selection', null);
                            viewModel.notify();
                            expect(field.getValue()).toBeNull();
                            expect(viewModel.get('value')).toBeNull();
                        });
                    });

                    describe("setting the ViewModel value property", function () {
                        it("should set the value when setting the record", function () {
                            var rec = getByVal('ExtJS');
                            viewModel.set('value', 'ExtJS');
                            viewModel.notify();
                            expect(field.getValue()).toEqual(['ExtJS']);
                            expect(viewModel.get('selection')).toEqual([rec]);
                        });

                        it("should set the value when updating the record", function () {
                            viewModel.set('value', ['ExtJS', 'COBOL']);
                            viewModel.notify();
                            viewModel.set('value', ['Java', 'PHP']);
                            viewModel.notify();
                            expect(field.getValue()).toEqual(['Java', 'PHP']);
                            expect(viewModel.get('selection')).toEqual([getByVal('Java'), getByVal('PHP')]);
                        });

                        it("should deselect when clearing the value", function () {
                            viewModel.set('value', 'ExtJS');
                            viewModel.notify();
                            viewModel.set('value', null);
                            viewModel.notify();
                            expect(field.getValue()).toBeNull();
                            expect(viewModel.get('selection')).toEqual(null);
                        });
                    });
                });

                // Not sure if we want to support this, leave this out for now
                xdescribe("with initial value", function () {
                    it("should trigger the binding with an initial value in the select field", function () {
                        viewModel.bind('{selection}', selectionSpy);
                        makeViewModelMultiSelectField({
                            value: 'COBOL'
                        });
                        viewModel.notify();
                        var args = selectionSpy.mostRecentCall.args;
                        expect(args[0]).toEqual([getByVal('COBOL')]);
                        expect(args[1]).toBeUndefined();
                    });
                });

                describe("reloading the store", function () {
                    beforeEach(function () {
                        MockAjaxManager.addMethods();
                        viewModel.bind('{selection}', selectionSpy);
                        makeViewModelMultiSelectField();
                        viewModel.notify();

                        selectNotify(getByVal('ExtJS'));
                        selectionSpy.reset();

                        store.setProxy({
                            type: 'ajax',
                            url: 'fake'
                        });
                        store.load();
                    });

                    afterEach(function () {
                        MockAjaxManager.removeMethods();
                    });

                    describe("when the selected record is in the result set", function () {
                        it("should trigger the selection binding", function () {
                            field.expand();

                            Ext.Ajax.mockComplete({
                                status: 200,
                                responseText: Ext.encode([
                                    {id: 'ExtJS', text: 'ExtJS', value: 'ExtJS'},
                                    {id: 'COBOL', text: 'COBOL', value: 'COBOL'}
                                ])
                            });

                            viewModel.notify();
                            field.expand();

                            // After the new record with the same ID arrives the selection must be
                            // synched to contain the new record by that ID.
                            expect(field.getPicker().getSelectable().getSelections()[0]).toBe(store.byValue.get('ExtJS'));
                            expect(selectionSpy.callCount).toBe(1);
                            expect(selectionSpy.mostRecentCall.args[0]).toEqual([store.getAt(0)]);
                            expect(viewModel.get('selection')).toEqual([store.byValue.get('ExtJS')]);
                        });
                    });

                    describe("when the selected record is not in the result set", function () {
                        it("should trigger the selection binding", function () {
                            Ext.Ajax.mockComplete({
                                status: 200,
                                responseText: '[]'
                            });

                            viewModel.notify();
                            expect(selectionSpy.callCount).toBe(1);
                            expect(selectionSpy.mostRecentCall.args[0]).toBeNull();
                        });
                    });
                });
            });
        });
    });

    describe('chained select fields', function() {
        var CountriesStore,
            StatesStore,
            panel,
            vm,
            countries,
            states;

        beforeEach(function() {
            CountriesStore = Ext.define('Ext.test.ChainedSelectTestCountries', {
                alias: 'store.chainedSelectTestCountries',
                extend: 'Ext.data.Store',
                fields: [
                    'name'
                ],
                data: [
                    { name: 'USA' },
                    { name: 'Canada' }
                ]
            });
            StatesStore = Ext.define('Ext.test.ChainedSelectTestStates', {
                alias: 'store.chainedSelectTestStates',
                extend: 'Ext.data.Store',
                fields: [
                    'abbr', 'country', 'state', 'description'
                ],
                data: [
                    { abbr: 'AL', country: 'USA', state: 'Alabama', 		description: 'The Heart of Dixie' },
                    { abbr: 'AK', country: 'USA', state: 'Alaska', 			description: 'The Land of the Midnight Sun' },
                    { abbr: 'AZ', country: 'USA', state: 'Arizona', 		description: 'The Grand Canyon State' },
                    { abbr: 'AR', country: 'USA', state: 'Arkansas', 		description: 'The Natural State' },
                    { abbr: 'CA', country: 'USA', state: 'California', 		description: 'The Golden State' },
                    { abbr: 'CO', country: 'USA', state: 'Colorado', 		description: 'The Mountain State' },
                    { abbr: 'CT', country: 'USA', state: 'Connecticut', 	description: 'The Constitution State' },
                    { abbr: 'DE', country: 'USA', state: 'Delaware', 		description: 'The First State' },
                    { abbr: 'DC', country: 'USA', state: 'District of Columbia', description: "The Nation's Capital" },
                    { abbr: 'FL', country: 'USA', state: 'Florida', 		description: 'The Sunshine State' },
                    { abbr: 'GA', country: 'USA', state: 'Georgia', 		description: 'The Peach State' },
                    { abbr: 'HI', country: 'USA', state: 'Hawaii', 			description: 'The Aloha State' },
                    { abbr: 'ID', country: 'USA', state: 'Idaho', 			description: 'Famous Potatoes' },
                    { abbr: 'IL', country: 'USA', state: 'Illinois', 		description: 'The Prairie State' },
                    { abbr: 'IN', country: 'USA', state: 'Indiana', 		description: 'The Hospitality State' },
                    { abbr: 'IA', country: 'USA', state: 'Iowa', 			description: 'The Corn State' },
                    { abbr: 'KS', country: 'USA', state: 'Kansas', 			description: 'The Sunflower State' },
                    { abbr: 'KY', country: 'USA', state: 'Kentucky', 		description: 'The Bluegrass State' },
                    { abbr: 'LA', country: 'USA', state: 'Louisiana', 		description: 'The Bayou State' },
                    { abbr: 'ME', country: 'USA', state: 'Maine', 			description: 'The Pine Tree State' },
                    { abbr: 'MD', country: 'USA', state: 'Maryland', 		description: 'Chesapeake State' },
                    { abbr: 'MA', country: 'USA', state: 'Massachusetts', 	description: 'The Spirit of America' },
                    { abbr: 'MI', country: 'USA', state: 'Michigan', 		description: 'Great Lakes State' },
                    { abbr: 'MN', country: 'USA', state: 'Minnesota', 		description: 'North Star State' },
                    { abbr: 'MS', country: 'USA', state: 'Mississippi', 	description: 'Magnolia State' },
                    { abbr: 'MO', country: 'USA', state: 'Missouri', 		description: 'Show Me State' },
                    { abbr: 'MT', country: 'USA', state: 'Montana', 		description: 'Big Sky Country' },
                    { abbr: 'NE', country: 'USA', state: 'Nebraska', 		description: 'Beef State' },
                    { abbr: 'NV', country: 'USA', state: 'Nevada', 			description: 'Silver State' },
                    { abbr: 'NH', country: 'USA', state: 'New Hampshire', 	description: 'Granite State' },
                    { abbr: 'NJ', country: 'USA', state: 'New Jersey', 		description: 'Garden State' },
                    { abbr: 'NM', country: 'USA', state: 'New Mexico', 		description: 'Land of Enchantment' },
                    { abbr: 'NY', country: 'USA', state: 'New York', 		description: 'Empire State' },
                    { abbr: 'NC', country: 'USA', state: 'North Carolina', 	description: 'First in Freedom' },
                    { abbr: 'ND', country: 'USA', state: 'North Dakota', 	description: 'Peace Garden State' },
                    { abbr: 'OH', country: 'USA', state: 'Ohio', 			description: 'The Heart of it All' },
                    { abbr: 'OK', country: 'USA', state: 'Oklahoma', 		description: 'Oklahoma is OK' },
                    { abbr: 'OR', country: 'USA', state: 'Oregon', 			description: 'Pacific Wonderland' },
                    { abbr: 'PA', country: 'USA', state: 'Pennsylvania', 	description: 'Keystone State' },
                    { abbr: 'RI', country: 'USA', state: 'Rhode Island', 	description: 'Ocean State' },
                    { abbr: 'SC', country: 'USA', state: 'South Carolina', 	description: 'Nothing Could be Finer' },
                    { abbr: 'SD', country: 'USA', state: 'South Dakota', 	description: 'Great Faces, Great Places' },
                    { abbr: 'TN', country: 'USA', state: 'Tennessee', 		description: 'Volunteer State' },
                    { abbr: 'TX', country: 'USA', state: 'Texas', 			description: 'Lone Star State' },
                    { abbr: 'UT', country: 'USA', state: 'Utah', 			description: 'Salt Lake State' },
                    { abbr: 'VT', country: 'USA', state: 'Vermont', 		description: 'Green Mountain State' },
                    { abbr: 'VA', country: 'USA', state: 'Virginia', 		description: 'Mother of States' },
                    { abbr: 'WA', country: 'USA', state: 'Washington', 		description: 'Green Tree State' },
                    { abbr: 'WV', country: 'USA', state: 'West Virginia', 	description: 'Mountain State' },
                    { abbr: 'WI', country: 'USA', state: 'Wisconsin', 		description: "America's Dairyland" },
                    { abbr: 'WY', country: 'USA', state: 'Wyoming', 		description: 'Like No Place on Earth' },

                    { abbr: 'ON', country: 'Canada', state: 'Ontario' },
                    { abbr: 'QC', country: 'Canada', state: 'Quebec' },
                    { abbr: 'NS', country: 'Canada', state: 'Nova Scotia' },
                    { abbr: 'NB', country: 'Canada', state: 'New Brunswick' },
                    { abbr: 'MB', country: 'Canada', state: 'Manitoba' },
                    { abbr: 'BC', country: 'Canada', state: 'British Columbia' },
                    { abbr: 'PE', country: 'Canada', state: 'Prince Edward Island' },
                    { abbr: 'SK', country: 'Canada', state: 'Saskatchewan' },
                    { abbr: 'AB', country: 'Canada', state: 'Alberta' },
                    { abbr: 'NL', country: 'Canada', state: 'Newfoundland and Labrador' }
                ]
            });
            panel = new Ext.Panel({
                renderTo: document.body,
                title: 'Country and state',
                width: 400,
                height: 200,
                viewModel: {
                    stores: {
                        countries: {
                            type: 'chainedSelectTestCountries',
                            autoLoad: true
                        },
                        states: {
                            type: 'chainedSelectTestStates',
                            autoLoad: true,
                            filters: [{
                                property: 'country',
                                value: '{countryField.selection.name}'
                            }],
                            sorters: [{
                                property: 'state'
                            }]
                        }
                    }
                },
                items: [{
                    xtype: 'selectfield',
                    label: 'Country',
                    placeholder: 'Choose a country',
                    reference: 'countryField',
                    valueField: 'name',
                    displayField: 'name',
                    bind: {
                        store: '{countries}'
                    },
                    value: 'USA'
                }, {
                    xtype: 'selectfield',
                    label: 'States',
                    reference: 'statesField',
                    valueField: 'abbr',
                    displayField: 'state',
                    bind: {
                        store: '{states}',
                        placeholder: '{countryField.value === "USA" ? "Chose a state" : countryField.value === "Canada" ? "Chose a province" : ""}'
                    },
                    value: 'AL'
                }]
            });
            vm = panel.getViewModel();
            countries = panel.child('[reference=countryField]');
            states = panel.child('[reference=statesField]');

            // Flush ViewModel data
            vm.getScheduler().onTick();

        });
        afterEach(function() {
            Ext.destroy(panel);
            Ext.undefine('Ext.test.ChainedSelectTestCountries');
            Ext.undefine('Ext.test.ChainedSelectTestStates');
        });

        // Note that this expectation will only work when the dependent combobox
        // has had its value picked from the open picker.
        it('should clear the dependent field when its selected record is filtered out', function() {
            // Select Colorado
            states.expand();
            states.getPicker().getSelectable().select(5);
            expect(states.getValue()).toBe('CO');

            // This will refresh the picker's selmodel and evict the state.
            countries.setValue('Canada');

            // When binding ticks, the states should be cleared because its
            // selected record is no longer in its store.
            waitsFor(function() {
                return states.getValue() == null;
            });
        });
    });
});
