topSuite("Ext.field.ComboBox",
    ['Ext.app.ViewModel', 'Ext.form.Panel', 'Ext.Dialog',
     'Ext.data.ArrayStore', 'Ext.layout.Fit'],
function() {
    
    var component,
        store,
        picker,
        pickerStore,
        selModel,
        valueCollection,
        CBTestModel,
        itNotIE = Ext.isIE ? xit : it,
        itNotIE9m = Ext.isIE9m ? xit : it,
        synchronousLoad = true,
        storeLoad = Ext.data.ProxyStore.prototype.load,
        storeFlushLoad = Ext.data.ProxyStore.prototype.flushLoad,
        loadStore = function() {
            storeLoad.apply(this, arguments);
            if (synchronousLoad) {
                this.flushLoad.apply(this, arguments);
            }
            return this;
        };

    // There's no simple way to simulate user typing, so going
    // to reach in too far here to call this method. Not ideal, but
    // the infrastructure to get typing simulation is fairly large
    function doTyping(value, isBackspace, keepPickerVisible) {
        // Focus the field so that trigger taps are processed immediately
        if (document.activeElement !== component.inputElement.dom) {
            component.inputElement.focus();
        }

        component.inputElement.dom.value = value;
        component.onInput({
            type: 'input',
            target: component.inputElement.dom
        });

        var filterTask = component.doFilterTask;

        if (filterTask) {
            filterTask.flush();
            // // Query not executed on empty
            // component.doFilterTask.cancel();
            // component.doRawFilter();
            //
            // if (!keepPickerVisible) {
            //     component.getPicker().hide();
            // }
        }
    }

    function getRawValue () {
        return component.inputElement.dom.value;
    }

    function setRawValue (v) {
        component.inputElement.dom.value = v;
    }

    function makeComponent(config, preventStore) {
        config = Ext.apply({
            width: 200,
            name: 'test',
            store: preventStore ? null : store,
            picker: 'floated'
        }, config);

        // If we are using the test's default, locally loaded store, then
        // we must be using queryMode: 'local' unless otherwise configured
        if (!('queryMode' in config) && config.store && config.store.$isDefaultTestStore) {
            config.queryMode = 'local';
        }

        component = new Ext.form.field.ComboBox(config);

        valueCollection = component.getValueCollection();

        Ext.override(component, {
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
    }

    function getTextSelectionIndices (field) {
        var indices = [];
        if (document.selection) {
            var range = document.selection.createRange(),
                stored = range.duplicate(),
                start, len;

            stored.expand('textedit');
            stored.setEndPoint('EndToEnd', range);

            len = range.text.length;
            start = stored.text.length - len;

            indices.push(start);
            indices.push(start + len);
        }
        else {
            indices.push(field.selectionStart);
            indices.push(field.selectionEnd);
        }

        return indices;
    }

    beforeEach(function() {
        // Override so that we can control asynchronous loading
        Ext.data.ProxyStore.prototype.load = loadStore;

        CBTestModel = Ext.define(null, {
            extend: 'Ext.data.Model',
            fields: [
                {type: 'string', name: 'text'},
                {type: 'string', name: 'value'}
            ]
        });

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
        store.$isDefaultTestStore = true;
    });

    afterEach(function() {
        // Undo the overrides.
        Ext.data.ProxyStore.prototype.load = storeLoad;

        if (store) {
            store.destroy();
        }
        if (component) {
            component.destroy();
        }
        Ext.undefine('spec.MyStore');
        component = store = null;
    });

    function findRecord(value, theStore) {
        var found;
        theStore = theStore || store;
        theStore.each(function(rec) {
            if (rec.get(component.getValueField()) === value || rec.get(component.getDisplayField()) === value) {
                found = rec;
                return false;
            }
        });

        return found;
    }

    function clickListItem(value, theStore) {
        var picker;
        component.expand();
        picker = component.getPicker();
        picker.refresh();
        jasmine.fireMouseEvent(picker.itemFromRecord(findRecord(value, theStore)).el, 'click');
    }

    function clickExpandTrigger(cmp) {
        cmp = cmp || component;

        var triggers = cmp.getTriggers();

        if (triggers && triggers.expand) {
            jasmine.fireMouseEvent(triggers.expand.el, 'click');
        }
    }

    it("should encode the input value in the template", function(){
        makeComponent({
            renderTo: Ext.getBody(),
            value: 'test "  <br/> test'
        });

        expect(component.inputElement.dom.value).toBe('test "  <br/> test');
    });

    // https://sencha.jira.com/browse/EXTJS-25893
    // BoundList Locations must always promoted to use the encapsulating element el.
    it("should select value when child element of first item is clicked", function() {
        var picker,
            item;

        makeComponent({
            displayField: 'text',
            valueField: 'value',
            queryMode: 'local',
            renderTo: Ext.getBody()
        });
        clickExpandTrigger();

        waitsFor(function() {
            return (picker = component.getPicker()).isVisible();
        }, 'picker to show for the first time', 500);

        runs(function() {
            item = picker.getViewItems()[0].el;

            // The initial location must be sourced on the list item's encapsulating element
            expect(Ext.getDom(picker.getNavigationModel().getLocation().sourceElement)).toBe(item.dom);

            jasmine.fireMouseEvent(item.down('.x-innerhtml'), 'click');
            expect(component.getValue()).toBe('value 1');
        });

        waitsFor(function() {
            return !picker.isVisible();
        }, 'picker to hide', 500);

        // Now try selecting the same item again.
        // It should still collapse the picker
        runs(function() {
            clickExpandTrigger();
        });

        waitsFor(function() {
            return (picker = component.getPicker()).isVisible();
        }, 'picker to show for the second time', 500);

        runs(function() {
            item = picker.getViewItems()[0].el;

            jasmine.fireMouseEvent(item.down('.x-innerhtml'), 'click');
        });

        waitsFor(function() {
            return !picker.isVisible();
        });
    });

    describe("forceSelecton false", function() {
        it("should allow arbitrary typing", function() {
            makeComponent({
                displayField: 'text',
                valueField: 'value',
                queryMode: 'local',
                value: 'foo2',
                renderTo: Ext.getBody(),
                forceSelection: false
            });
            doTyping("abc");
            component.completeEdit();
            expect(component.getValue()).toBe("abc");
        });

        it("should retain invalid entry text and null value on blur", function() {
            makeComponent({
                displayField: 'text',
                valueField: 'value',
                queryMode: 'local',
                renderTo: Ext.getBody(),
                forceSelection: false
            });
            doTyping("abc");

            component.completeEdit();

            expect(component.getInputValue()).toBe('abc');
            expect(component.getValue()).toBe('abc');
        });

        it('should respond properly to the clear trigger', function () {
            makeComponent({
                clearable: true,
                displayField: 'text',
                valueField: 'value',
                queryMode: 'local',
                value: 'foo2',
                renderTo: Ext.getBody(),
                forceSelection: false
            });
            doTyping("abc");

            component.completeEdit();

            expect(component.getValue()).toBe("abc");

            var trigger = component.getTriggers().clear;

            var v = component.getInputValue();

            expect(v).toBe('abc');
            expect(trigger.isVisible()).toBe(true);

            component.onClearIconTap();

            v = component.getValue();

            expect(v).toBe('');
            expect(trigger.isVisible()).toBe(false);

            v = component.getInputValue();
            expect(v).toBe('');

            v = getRawValue();
            expect(v).toBe('');
        });

        it("should fire change on keystrokes", function() {
            makeComponent({
                displayField: 'text',
                valueField: 'value',
                queryMode: 'local',
                value: 'foo2',
                renderTo: Ext.getBody(),
                forceSelection: false
            });

            var count = 0;

            component.on('change', function() {
                count++;
            });

            doTyping("a");
            doTyping("bc");

            expect(count).toBe(2);
        });

        it("should fire select events on typing", function() {
            makeComponent({
                displayField: 'text',
                valueField: 'value',
                queryMode: 'local',
                value: 'foo2',
                renderTo: Ext.getBody(),
                forceSelection: false
            });
            var count = 0;
            component.on('select', function() {
                count++;
            });
            doTyping("a");

            // An "isEntered" record has been selected representing "a"
            expect(count).toBe(1);

            doTyping("ab");

            // An "isEntered" record has been selected representing "ab"
            expect(count).toBe(2);
        });

        it("should fire action event on ENTER", function() {
            makeComponent({
                displayField: 'text',
                valueField: 'value',
                queryMode: 'local',
                value: 'foo2',
                renderTo: Ext.getBody(),
                forceSelection: false
            });
            var selectCount = 0;
            var value;

            component.on('action', function() {
                selectCount++;
                value = component.getValue();
            });

            doTyping("a");

            expect(selectCount).toBe(0);

            var target = component.inputElement.dom,
                key = 13;

            jasmine.fireKeyEvent(target, 'keydown', key);
            jasmine.fireKeyEvent(target, 'keyup', key);
            jasmine.fireKeyEvent(target, 'keypress', key);

            expect(selectCount).toBe(1);
            expect(value).toBe('a');
        });
    });

    describe("forceSelection true", function() {
        it("should have null value if invalid entry", function() {
            makeComponent({
                displayField: 'text',
                valueField: 'value',
                queryMode: 'local',
                renderTo: Ext.getBody(),
                forceSelection: true
            });
            doTyping("abc");
            expect(component.getValue()).toBeNull();
        });

        it("should clear text when invalid and null on blur", function() {
            makeComponent({
                displayField: 'text',
                valueField: 'value',
                queryMode: 'local',
                renderTo: Ext.getBody(),
                forceSelection: true
            });
            doTyping("abc");

            expect(component.getInputValue()).toBe('abc');
            expect(component.getValue()).toBeNull();

            component.completeEdit();

            expect(component.getInputValue()).toBe('');
            expect(component.getValue()).toBeNull();
        });

        it("should have store value if valid entry", function() {
            makeComponent({
                renderTo: Ext.getBody(),
                valueField: 'value',
                displayField: 'text',
                queryMode: 'local'
            });

            doTyping('text 3');

            var filters = component._pickerStore.getFilters();

            expect(filters.getCount()).toBe(1);

            var filter = filters.getAt(0);
            expect(filter.getProperty()).toBe('text');
            expect(filter.getValue()).toBe('text 3');
        });

        it('should respond properly to the clear trigger', function () {
            makeComponent({
                clearable: true,
                displayField: 'text',
                valueField: 'value',
                queryMode: 'local',
                renderTo: Ext.getBody(),
                forceSelection: true
            });

            jasmine.fireMouseEvent(component.getTriggers().expand.el, 'click');
            clickListItem('value 3');
            expect(component.getValue()).toBe('value 3');

            var trigger = component.getTriggers().clear;
            var v = component.getInputValue();
            var vc = component.getValueCollection();

            expect(v).toBe('text 3');
            expect(trigger.isVisible()).toBe(true);
            expect(vc.length).toBe(1);

            component.onClearIconTap();

            v = component.getValue();

            expect(v).toBe(null);
            expect(trigger.isVisible()).toBe(false);
            expect(vc.length).toBe(0);

            v = component.getInputValue();
            expect(v).toBe('');

            v = getRawValue();
            expect(v).toBe('');
        });

        it('should clear incomplete values on the clear trigger', function () {
            makeComponent({
                clearable: true,
                displayField: 'text',
                valueField: 'value',
                queryMode: 'local',
                renderTo: Ext.getBody(),
                forceSelection: true
            });

            doTyping("abc");
            var trigger, vc, v = component.getValue();

            expect(v).toBe(null);

            trigger = component.getTriggers().clear;
            v = component.getInputValue();
            vc = component.getValueCollection();

            expect(v).toBe('abc');
            expect(trigger.isVisible()).toBe(true);
            expect(vc.length).toBe(0);

            component.onClearIconTap();

            v = component.getValue();

            expect(v).toBe(null);
            expect(trigger.isVisible()).toBe(false);
            expect(vc.length).toBe(0);

            v = component.getInputValue();
            expect(v).toBe('');

            v = getRawValue();
            expect(v).toBe('');
        });

        it("should fire select if item in picker is selected", function() {
            makeComponent({
                renderTo: Ext.getBody(),
                valueField: 'value',
                displayField: 'text',
                queryMode: 'local'
            });
            var selectCount = 0;

            component.on('select', function() {
                selectCount++;
            });
            jasmine.fireMouseEvent(component.getTriggers().expand.el, 'click');
            clickListItem('value 1');
            expect(selectCount).toBe(1);
        });
    });

    describe("store shortcuts", function() {
        describe('with 1-dimensional array', function() {
            it("should set the value & raw value correctly", function() {
                component = new Ext.form.field.ComboBox({
                    options: ['Item 1', 'Item 2', 'Item 3']
                });
                component.setValue('Item 1');
                expect(component.getValue()).toBe('Item 1');
                expect(getRawValue()).toBe('Item 1');
            });

            it("should not overwrite a configured displayTpl", function() {
                component = new Ext.form.field.ComboBox({
                    options: ['Item 1', 'Item 2', 'Item 3'],
                    displayTpl: 'Value is {text}'
                });
                component.setValue('Item 1');
                expect(getRawValue()).toBe('Value is Item 1');
            });
        });

        describe('with 2-dimensional array', function() {
            it("should set the value & raw value correctly", function() {
                component = new Ext.form.field.ComboBox({
                    options: [[1, 'Item 1'], [2, 'Item 2'], [3, 'Item 3']]
                });
                component.setValue(2);
                expect(component.getValue()).toBe(2);
                expect(getRawValue()).toBe('Item 2');
            });

            it("should not overwrite a configured displayTpl", function() {
                component = new Ext.form.field.ComboBox({
                    options: [[1, 'Item 1'], [2, 'Item 2'], [3, 'Item 3']],
                    displayTpl: 'Value is {text}'
                });
                component.setValue(1);
                expect(getRawValue()).toBe('Value is Item 1');
            });
        });
    });

    describe("value initialization", function() {
        describe("without a value", function() {
            it("should have value === null when autoSelect is false", function() {
                makeComponent({
                    autoSelect: false,
                    valueField: 'value'
                });
                expect(component.getValue()).toBeNull();
            });

            it("should have first store value if no value configured and autoSelect is true", function() {
                makeComponent({
                    autoSelect: true,
                    valueField: 'value'
                });
                expect(component.getValue()).toBe('value 1');
            });
        });
        describe('with a value', function() {
            it("should select the corresponding list item on expand", function() {
                makeComponent({
                    displayField: 'text',
                    valueField: 'value',
                    queryMode: 'local',
                    value: 'foo2',
                    renderTo: Ext.getBody()
                });
                jasmine.focusAndWait(component);
                runs(function() {
                    component.expand();

                    expect(component.getPicker().getSelectable().getSelections()[0]).toBe(store.byValue.get('foo2'));
                });
            });
        });
    });

    describe("onExpand", function() {
        var getInnerTpl = function() {
            return 'foo';
        };

        beforeEach(function() {
            makeComponent({
                renderTo: Ext.getBody(),
                displayField: 'value',
                picker: 'floated',
                floatedPicker: {
                    width: 234,
                    matchFieldWidth: false,
                    maxHeight: 345,
                    loadingText: 'gazingazang',
                    emptyText: 'buffoopaloo',
                    getInnerTpl: getInnerTpl,
                    type: 'floated'
                },
                matchFieldWidth: false,
                value: 'value 2'
            });
            component.expand();
        });

        it("should create a Ext.dataview.List as the picker", function() {
            expect(component.getPicker()).toBeDefined();
            expect(component.getPicker() instanceof Ext.dataview.List).toBe(true);
        });
        it("should pass a ChainedStore to the BoundList when queryMode: 'local'", function() {
            var pickerStore = component.getPicker().getStore();
            expect(pickerStore.isChainedStore).toBe(true);
            expect(pickerStore.getSource()).toBe(component.getStore());
        });
        xit("should pass the configured displayField to the BoundList", function() {
            expect(component.getPicker().displayField).toEqual(component.displayField);
        });
        it("should pass the configured picker.width to the BoundList", function() {
            expect(component.getPicker().getWidth()).toEqual(234);
        });
        it("should pass the configured picker.maxHeight to the BoundList", function() {
            expect(component.getPicker().getMaxHeight()).toEqual(345);
        });
        xit("should pass the configured picker.loadingText to the BoundList", function() {
            expect(component.getPicker().loadingText).toEqual('gazingazang');
        });
        it("should pass the configured picker.emptyText to the BoundList", function() {
            expect(component.getPicker().getEmptyText()).toEqual('buffoopaloo');
        });
        xit("should pass a configured picker.getInnerTpl method to the BoundList config", function() {
            expect(component.getPicker().getInnerTpl).toBe(getInnerTpl);
        });
        it("should set the BoundList's selection to match the current value", function() {
            expect(component.getPicker().getSelections().length).toEqual(1);
        });
        xit("should initialize a BoundListKeyNav on the BoundList", function() {
            expect(component.keyMap).toBeDefined();
            expect(component.getPicker().getNavigationModel() instanceof Ext.view.BoundListKeyNav).toBe(true);
        });
        it("should enable the BoundListKeyNav", function() {
            waitsFor(function() {
                return component.getPicker().getNavigationModel().getDisabled() !== true;
            });
        });

        it("should set aria-activedescendant", function() {
            var node = component.getPicker().getNavigationModel().location.item;

            expect(component.ariaEl).toHaveAttr('aria-activedescendant', node.id);
        });
    });


    describe("onCollapse", function() {
        it("should disable the BoundListKeyNav", function() {
            runs(function() {
                makeComponent({
                    renderTo: Ext.getBody()
                });
                component.expand();
            });
            waitsFor(function() {
                return component.getPicker().getNavigationModel().getDisabled() !== true;
            });
            runs(function() {
                component.collapse();
                expect(component.getPicker().getNavigationModel().getDisabled()).toBe(true);
            });
        });
    });


    describe("setting value", function() {
        describe("value config", function() {
            it("should accept a single string", function() {
                makeComponent({
                    value: 'value 2',
                    valueField: 'value'
                });
                expect(component.getValue()).toEqual('value 2');
            });
            xit("should accept an array of string values", function() {
                makeComponent({
                    multiSelect: true,
                    value: ['value 3', 'not in store'],
                    valueField: 'value'
                });
                expect(component.getValue()).toEqual(['value 3', 'not in store']);
            });
            it("should accept a single Ext.data.Model", function() {
                makeComponent({
                    value: store.getAt(0),
                    valueField: 'value'
                });
                expect(component.getValue()).toEqual('value 1');
            });
            //\\TODO: multiselect
            xit("should accept an array of Ext.data.Model objects", function() {
                makeComponent({
                    multiSelect: true,
                    value: [store.getAt(0), store.getAt(2)],
                    valueField: 'value'
                });
                expect(component.getValue()).toEqual(['value 1', 'value 3']);
            });
            //\\TODO: multiselect
            xit("should display the values separated by the configured delimiter", function() {
                makeComponent({
                    multiSelect: true,
                    value: ['value 1', 'value 2'],
                    valueField: 'value',
                    renderTo: Ext.getBody(),
                    delimiter: '|'
                });
                expect(component.inputElement.dom.value).toEqual('text 1|text 2');
            });
        });

        describe("setValue method", function() {
            it("should accept a single string", function() {
                makeComponent({
                    valueField: 'value'
                });
                component.setValue('value 2');
                expect(component.getValue()).toBe('value 2');
            });
            //\\TODO: multiselect
            xit("should accept an array of string values", function() {
                makeComponent({
                    multiSelect: true,
                    valueField: 'value'
                });
                component.setValue(['value 3', 'not in store']);
                expect(component.getValue()).toEqual(['value 3', 'not in store']);
            });
            it("should accept a single Ext.data.Model", function() {
                makeComponent({
                    valueField: 'value'
                });
                component.setValue(store.getAt(0));
                expect(component.getValue()).toEqual('value 1');
            });
            //\\TODO: multiselect
            xit("should accept an array of Ext.data.Model objects", function() {
                makeComponent({
                    multiSelect: true,
                    valueField: 'value'
                });
                component.setValue([store.getAt(0), store.getAt(2)]);
                expect(component.getValue()).toEqual(['value 1', 'value 3']);
            });

            //\\ TODO: Define behaviour for multi values passed to single value combo
            xit("should only display the first value if not multiSelect", function() {
                makeComponent({
                    valueField: 'value',
                    renderTo: Ext.getBody(),
                    delimiter: '|'
                });
                component.setValue(['value 1', 'value 2']);
                expect(component.inputElement.dom.value).toEqual('text 1');
            });
            //\\TODO: multiselect
            xit("should display the values separated by the configured delimiter if multiSelect", function() {
                makeComponent({
                    valueField: 'value',
                    multiSelect: true,
                    renderTo: Ext.getBody(),
                    delimiter: '|'
                });
                component.setValue(['value 1', 'value 2']);
                expect(component.inputElement.dom.value).toEqual('text 1|text 2');
            });
            //\\TODO: multiselect
            xit("should display the valueNotFoundText for values not in the store if multiSelect", function() {
                makeComponent({
                    valueField: 'value',
                    forceSelection: true,
                    multiSelect: true,
                    valueNotFoundText: 'oops!',
                    renderTo: Ext.getBody()
                });
                component.setValue(['value 1', 'value not in store']);
                expect(component.inputElement.dom.value).toEqual('text 1, oops!');
            });
            xit("should not display the valueNotFoundText for values not in the store if not multiSelect", function() {
                makeComponent({
                    valueField: 'value',
                    forceSelection: true,
                    valueNotFoundText: 'oops!',
                    renderTo: Ext.getBody()
                });
                component.setValue(['value 1', 'value not in store']);
                expect(component.inputElement.dom.value).toEqual('text 1');
            });
            it("should update the expanded dropdown's selection - single select", function() {
                makeComponent({
                    valueField: 'value',
                    renderTo: Ext.getBody()
                });
                component.expand();
            
                waits(1);
                runs(function() {
                    component.setValue('value 2');
                    expect(component.getPicker().getSelections()).toEqual([store.getAt(1)]);
                });
                
            });
            //\\TODO: multiselect
            xit("should update the expanded dropdown's selection - multi select", function() {
                makeComponent({
                    valueField: 'value',
                    renderTo: Ext.getBody(),
                    multiSelect: true
                });
                component.expand();
                waits(1);
                runs(function() {
                    component.setValue(['value 1', 'value 3']);
                    expect(component.getPicker().getSelections()).toEqual([store.getAt(0), store.getAt(2)]);
                });

            });

            describe('change event', function() {
                it("should not fire the change event when the value stays the same - single value", function() {
                    var spy = jasmine.createSpy();
                    makeComponent({
                        valueField: 'value',
                        value: 'value1',
                        renderTo: Ext.getBody(),
                        listeners: {
                            change: spy
                        }
                    });
                    component.setValue('value1');
                    expect(spy).not.toHaveBeenCalled();
                });
                it("should fire the change event when the value changes - single value", function() {
                    var spy = jasmine.createSpy();
                    makeComponent({
                        valueField: 'value',
                        value: 'value1',
                        renderTo: Ext.getBody(),
                        listeners: {
                            change: spy
                        }
                    });
                    component.setValue('value2');
                    expect(spy).toHaveBeenCalled();
                    expect(spy.mostRecentCall.args[0]).toBe(component);
                    expect(spy.mostRecentCall.args[1]).toEqual('value2');
                    expect(spy.mostRecentCall.args[2]).toEqual('value1');
                });
                xit("should not fire the change event when the value stays the same - multiple values", function() {
                    var spy = jasmine.createSpy();
                    makeComponent({
                        multiSelect: true,
                        valueField: 'value',
                        value: ['value1', 'value2'],
                        renderTo: Ext.getBody(),
                        listeners: {
                            change: spy
                        }
                    });
                    component.setValue(['value1', 'value2']);
                    expect(spy).not.toHaveBeenCalled();
                });
                xit("should fire the change event when the value changes - multiple values", function() {
                    var spy = jasmine.createSpy();
                    makeComponent({
                        multiSelect: true,
                        valueField: 'value',
                        value: ['value1', 'value2'],
                        renderTo: Ext.getBody(),
                        listeners: {
                            change: spy
                        }
                    });
                    component.setValue(['value1', 'value3']);
                    expect(spy).toHaveBeenCalled();
                    expect(spy.mostRecentCall.args[0]).toBe(component);
                    expect(spy.mostRecentCall.args[1]).toEqual(['value1', 'value3']);
                    expect(spy.mostRecentCall.args[2]).toEqual(['value1', 'value2']);
                });
                it("should fire the refresh event when the value changes back to what the last *remotely* queried value was", function() {
                    var remoteStore = new Ext.data.Store({
                        fields: ['abbr', 'name'],
                            proxy: {
                                type: 'memory',
                                data: [{
                                    "abbr": "AL",
                                    "name": "Alabama"
                                }, {
                                    "abbr": "AK",
                                    "name": "Alaska"
                                }, {
                                    "abbr": "AZ",
                                    "name": "Arizona"
                                }]
                            }
                        }),
                        spy = jasmine.createSpy();

                    makeComponent({
                        displayField: 'name',
                        valueField: 'abbr',
                        minChars: 0,
                        queryMode: 'remote',
                        store: remoteStore,
                        renderTo: Ext.getBody()
                    });
                    component.getStore().on({
                        refresh: spy
                    });
                    doTyping('a');                    
                    waitsForSpy(spy, 'first refresh event');
                    runs(function() {
                        spy.reset();
                        doTyping('', true);
                    });
                    waitsForSpy(spy, 'second refresh event');
                    runs(function() {
                        spy.reset();
                        doTyping('a');
                    });
                    waitsForSpy(spy, 'third refresh event');
                });
            });
        });
    });

    describe('getting value', function() {
        beforeEach(function() {
            makeComponent({
                valueField: 'value',
                renderTo: Ext.getBody()
            });
        });

        // TODO: Should it, in Modern? We deal in records
        xit("should return the raw text field value if no selection has been made", function() {
            component.inputElement.dom.value = 'not-in-store';
            expect(component.getValue()).toEqual('not-in-store');
        });

        it("should return the valueField for an item selected from the list", function() {
            component.inputElement.dom.value = 'not-in-store';
            component.expand();
            waits(1);
            runs(function() {
                component.getPicker().select([store.findRecord('text', 'text 2')]);
                expect(component.getValue()).toEqual('value 2');
            });
        });

        // TODO: Should it, in Modern? We deal in records
        xit("should return the raw text field value if it is changed after selection", function() {
            component.inputElement.dom.value = 'not-in-store';
            component.expand();
            waits(1);
            runs(function() {
                component.getPicker().select([store.findRecord('text', 'text 2')]);
                component.inputElement.dom.value = 'text 2a';
                expect(component.getValue()).toEqual('value 2a');
            });
        });
    });

    describe("finding records", function() {
        beforeEach(function() {
            makeComponent({
                valueField: 'value',
                displayField: 'text'
            });
        });
    });

    describe("modifications via the text input", function() {
        it("should be able to requery when typing a value, choosing from a list then retyping the same value", function() {
            makeComponent({
                renderTo: Ext.getBody(),
                valueField: 'value',
                displayField: 'text',
                queryMode: 'local'
            });
            var filters = component._pickerStore.getFilters();

            doTyping('text 12');
            jasmine.fireMouseEvent(component.getTriggers().expand.el, 'click');
            clickListItem('value 1');

            // Clicking the trigger disables the primaryFilter
            expect(filters.first().getDisabled()).toBe(true);
            doTyping('text 12');

            // Check primaryFilter value
            expect(filters.first().getValue()).toBe('text 12');
        });

        describe("with queryMode: local", function() {
            it("should filter the store via the raw value", function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    valueField: 'value',
                    displayField: 'text',
                    queryMode: 'local'
                });
                doTyping('text 3');
                var filters = component._pickerStore.getFilters();
                expect(filters.getCount()).toBe(1);
                var filter = filters.getAt(0);
                expect(filter.getProperty()).toBe('text');
                expect(filter.getValue()).toBe('text 3');
                // Value not set during filtering.
                // TODO: Should it?
                // expect(component.getValue()).toBe('value 3');
            });

            it("should clear the value & disable primary filter when all text is removed", function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    valueField: 'value',
                    displayField: 'text',
                    queryMode: 'local',
                    value: 'text 3'
                });
                doTyping('', true);
                var filters = component._pickerStore.getFilters();
                expect(filters.first().getDisabled()).toBe(true);
                // Value not set during filtering.
                // TODO: Should it?
                // expect(component.getValue()).toBeNull();
            });

            describe("with enableRegex", function() {
                beforeEach(function() {
                    makeComponent({
                        renderTo: Ext.getBody(),
                        valueField: 'value',
                        displayField: 'text',
                        queryMode: 'local',
                        enableRegEx: true
                    });
                });

                it("should filter using the typed value", function() {
                    doTyping('te.*3');
                    expect(component._pickerStore.getCount()).toBe(5);
                });

                it("should ignore invalid inputs", function() {
                    expect(function() {
                        doTyping('*');
                    }).not.toThrow();
                    expect(component._pickerStore.getCount()).toBe(9);
                    expect(component._pickerStore.getFilters().first().getDisabled()).toBe(true);
                });
            });

            //\\TDODO: implement stripCharsRe?
            xdescribe("with stripCharsRe", function() {
                beforeEach(function() {
                    makeComponent({
                        renderTo: Ext.getBody(),
                        valueField: 'value',
                        displayField: 'text',
                        queryMode: 'local',
                        stripCharsRe: new RegExp('[^0123456789]', 'gi')
                    });
                });

                it("should remove unwanted characters", function(){
                    doTyping('a');
                    waits(100);
                    runs(function() {
                        expect(component.inputElement.dom.value).toBe('');
                    });
                });

                it("should keep lastValue clear if the text was stripped", function() {
                    doTyping('a');
                    waits(100);
                    runs(function() {
                        doTyping('a');
                    });
                    waits(100);
                    runs(function(){
                        expect(component.inputElement.dom.value).toBe('');
                    });
                });
            });

            it('should be able to reselect the same dropdown item after setting the value back to null', function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    valueField: 'value',
                    displayField: 'text',
                    queryMode: 'local'
                });
                var foo2Rec = findRecord('foo2');

                component.onExpandTap();
                clickListItem('foo2');
                expect(component.getValueCollection().contains(foo2Rec)).toBe(true);

                component.setValue(null);
                expect(component.getValueCollection().length).toBe(0);

                component.onExpandTap();
                clickListItem('foo2');
                expect(component.getValueCollection().contains(foo2Rec)).toBe(true);
            });

            it('should be able to reselect the same dropdown item after clearing the text', function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    valueField: 'value',
                    displayField: 'text',
                    queryMode: 'local'
                });
                var foo2Rec = findRecord('foo2');

                component.onExpandTap();
                clickListItem('foo2');
                expect(component.getValueCollection().contains(foo2Rec)).toBe(true);

                doTyping('');
                expect(component.getValueCollection().length).toBe(0);

                component.onExpandTap();
                clickListItem('foo2');
                expect(component.getValueCollection().contains(foo2Rec)).toBe(true);
            });

            it('should be able to reselect the same dropdown item after typing non-matching text - forceSelection: true', function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    valueField: 'value',
                    displayField: 'text',
                    queryMode: 'local',
                    forceSelection: true
                });
                var foo2Rec = findRecord('foo2');

                component.onExpandTap();
                clickListItem('foo2');
                expect(component.getValueCollection().contains(foo2Rec)).toBe(true);

                doTyping('flerpetty');

                // Non-matching typing does not update the value:
                expect(component.getValueCollection().contains(foo2Rec)).toBe(true);

                component.completeEdit();
                expect(component.getValueCollection().length).toBe(0);
                expect(component.getSelection()).toBe(null);

                component.onExpandTap();
                clickListItem('foo2');
                expect(component.getValueCollection().contains(foo2Rec)).toBe(true);
            });

            it('should be able to reselect the same dropdown item after typing non-matching text - forceSelection: false', function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    valueField: 'value',
                    displayField: 'text',
                    queryMode: 'local',
                    forceSelection: false
                });
                var foo2Rec = findRecord('foo2');

                component.onExpandTap();
                clickListItem('foo2');
                expect(component.getValueCollection().contains(foo2Rec)).toBe(true);

                doTyping('flerpetty');

                // It caused an isEntered record creation
                expect(component.getValueCollection().length).toBe(1);

                component.onExpandTap();
                clickListItem('foo2');
                expect(component.getValueCollection().contains(foo2Rec)).toBe(true);
            });
        });

        describe("clearing the value", function() {
            it("should set the value to null", function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    valueField: 'value',
                    displayField: 'text',
                    queryMode: 'local',
                    value: 'text 3'
                });
                doTyping('', true);
                // Value not set during filtering.
                // TODO: Should it?
                // expect(component.getValue()).toBeNull();
            });

            it("should be able to select after clearing the value", function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    valueField: 'value',
                    displayField: 'text',
                    queryMode: 'local',
                    value: 'text 3'
                });
                doTyping('', true);
                clickListItem('value 2');
                expect(component.getValue()).toBe('value 2');
                expect(getRawValue()).toBe('text 2');
            });

            it("should be able to select after clearing a cached value", function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    valueField: 'value',
                    displayField: 'text',
                    queryMode: 'local',
                    value: 'value 1'
                });
                doTyping('', true);
                doTyping('text 2');
                doTyping('', true);
                doTyping('text 2');
                clickListItem('value 2');
                expect(component.getValue()).toBe('value 2');
                expect(getRawValue()).toBe('text 2');
                expect(component.expanded).toBe(false);
            });

            it("should create the correct filter after clearing a cached value with a store filter", function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    valueField: 'value',
                    displayField: 'text',
                    queryMode: 'local'
                });
                store.addFilter(new Ext.util.Filter({
                    property: 'value',
                    value: 'foo',
                    anyMatch: true
                }));

                doTyping('f');
                doTyping('', true);
                doTyping('v');
                doTyping('', true);
                doTyping('v');
                
                expect(component.expanded).toBe(false);
                expect(component._pickerStore.getCount()).toBe(0);
            });
        });
    });

    xdescribe("growToLongestValue", function () {
        var bodyEl,
            beforeWidth,
            afterWidth,
            shortText = 'foo',
            longText = 'this text is veeeeeeeeeeeeeeeeeeeeeeeeeeeery long',
            longestText = 'this text is much, much, much, much, much, much, much, much, much much, much, much, much, much, much, much, much too long';

        describe("when true", function () {
            describe("adding a value to store", function () {
                it("should not grow when a longer record is added to store when not set to grow", function () {
                    makeComponent({
                        grow: false,
                        growToLongestValue: true,
                        renderTo: Ext.getBody()
                    });

                    bodyEl = component.bodyEl;

                    beforeWidth = bodyEl.getWidth();
                    store.add({text: longText, value: 'value 4'});
                    afterWidth = bodyEl.getWidth();

                    expect(beforeWidth).toEqual(afterWidth);
                });

                it("should grow when a longer record is added to store", function () {
                    makeComponent({
                        grow: true,
                        growToLongestValue: true,
                        renderTo: Ext.getBody()
                    });

                    bodyEl = component.bodyEl;

                    beforeWidth = bodyEl.getWidth();
                    store.add({text: longText, value: 'value 4'});
                    afterWidth = bodyEl.getWidth();

                    expect(afterWidth).toBeGreaterThan(beforeWidth);
                });

                it("should not grow when a shorter record is added to store", function () {
                    makeComponent({
                        grow: true,
                        growToLongestValue: true,
                        renderTo: Ext.getBody()
                    });

                    var inputEl = component.inputElement;

                    beforeWidth = inputEl.getWidth();
                    store.add({text: shortText, value: 'value 4'});
                    afterWidth = inputEl.getWidth();

                    expect(beforeWidth).toEqual(afterWidth);
                });

                it("should grow when growToLongestValue is set", function () {
                    makeComponent({
                        grow: true,
                        growToLongestValue: true,
                        renderTo: Ext.getBody()
                    });

                    bodyEl = component.bodyEl;

                    beforeWidth = bodyEl.getWidth();
                    store.add({text: longText, value: 'value 4'});
                    afterWidth = bodyEl.getWidth();

                    expect(afterWidth).toBeGreaterThan(beforeWidth);
                });

                it("should not grow when growToLongestValue isn't set", function () {
                    makeComponent({
                        grow: true,
                        growToLongestValue: false,
                        renderTo: Ext.getBody()
                    });

                    bodyEl = component.bodyEl;

                    beforeWidth = bodyEl.getWidth();
                    store.add({text: longText, value: 'value 4'});
                    afterWidth = bodyEl.getWidth();

                    expect(beforeWidth).toEqual(afterWidth);
                });

                it("should not grow larger than growMax when growMax is exceeded", function () {
                    makeComponent({
                        grow: true,
                        growMax: 200,
                        growToLongestValue: true,
                        renderTo: Ext.getBody()
                    });

                    store.add({text: longestText, value: 'value 4'});

                    expect(component.bodyEl.getWidth()).toEqual(component.growMax);
                });
            });

            describe('removing store values', function () {
                it('should shrink when largest item is removed', function () {
                    makeComponent({
                        grow: true,
                        growToLongestValue: true,
                        renderTo: Ext.getBody()
                    });

                    bodyEl = component.bodyEl;

                    store.add({text: longText, value: 'value 4'});
                    beforeWidth = bodyEl.getWidth();

                    store.removeAt(store.getCount() - 1);
                    afterWidth = bodyEl.getWidth();

                    expect(afterWidth).toBeLessThan(beforeWidth);
                });

                it('should not shrink when item other than largest item is removed', function () {
                    makeComponent({
                        grow: true,
                        growToLongestValue: true,
                        renderTo: Ext.getBody()
                    });

                    bodyEl = component.bodyEl;

                    store.add({text: longText, value: 'value 4'});
                    beforeWidth = bodyEl.getWidth();

                    store.removeAt(0);
                    afterWidth = bodyEl.getWidth();

                    expect(afterWidth).toEqual(beforeWidth);
                });

                it('should not shrink below growMin width', function () {
                    makeComponent({
                        grow: true,
                        growMin: 100,
                        growToLongestValue: true,
                        renderTo: Ext.getBody()
                    });

                    bodyEl = component.bodyEl;
                    store.add({text: longText, value: 'value 4'});

                    beforeWidth = bodyEl.getWidth();
                    store.removeAll();
                    afterWidth = bodyEl.getWidth();

                    expect(afterWidth).toEqual(component.growMin);
                });
            });
        });

        describe('when false', function () {
            beforeEach(function () {
                Ext.util.CSS.createStyleSheet(
                    // make the input el have a 9px character width
                    '.x-form-text { font:15px monospace;letter-spacing:0px; }',
                    'growStyleSheet'
                );
            });

            afterEach(function () {
                Ext.util.CSS.removeStyleSheet('growStyleSheet');
            });

            it('should start out at growMin', function () {
                makeComponent({
                    renderTo: document.body,
                    grow: true,
                    growToLongestValue: false,
                    growMin: 50
                });

                expect(component.getWidth()).toBe(50);
            });

            it('should initially render at the width of the text', function () {
                makeComponent({
                    renderTo: document.body,
                    value: 'mmmmmmmmmm',
                    grow: true,
                    growToLongestValue: false,
                    growMin: 50
                });

                expect(component.getWidth()).toBe(component.bodyEl.getWidth());
            });

            it('should initially render with a width of growMax if initial text width exceeds growMax', function () {
                makeComponent({
                    renderTo: document.body,
                    value: 'mmmmmmmmmmmmmmmmmmmmmmmmmmmmmm',
                    grow: true,
                    growToLongestValue: false,
                    growMax: 200
                });

                expect(component.getWidth()).toBe(200);
            });

            it('should grow and shrink', function () {
                makeComponent({
                    renderTo: document.body,
                    grow: true,
                    growToLongestValue: false,
                    triggers: {
                        foo: {}
                    },
                    growMin: 100,
                    growMax: 200
                });

                expect(component.getWidth()).toBe(100);

                component.setValue('mmmmmmmmmmmmmm');

                expect(component.getWidth()).toBe(component.bodyEl.getWidth());

                component.setValue('mmmmmmmmmmmmmmmmmmmmmmmmmmmmmm');

                expect(component.getWidth()).toBe(200);

                component.setValue('mmmmmmmmmmmmmm');

                expect(component.getWidth()).toBe(component.bodyEl.getWidth());

                component.setValue('m');

                expect(component.getWidth()).toBe(100);
            });
        });
    });

    describe("doFilter method", function() {
        it("should set the lastQuery property", function() {
            makeComponent();
            component.doFilter({
                query: 'foobar'
            });
            expect(component.lastQuery.query).toEqual('foobar');
        });

        it("should not clear remote store's filter", function() {
            makeComponent();
            spyOn(component.getStore(), 'clearFilter');
            component.doFilter({
                query: 'foobar'
            });
            expect(component.getStore().clearFilter).not.toHaveBeenCalled();
        });

        describe("local queryMode", function() {
            xit("should auto select if the last query is the same", function() {
                makeComponent({
                    renderTo: document.body,
                    queryMode: 'local',
                    displayField: 'value',
                    lastQuery: 'value 2'
                });

                spyOn(component, 'doAutoSelect');
                component.doFilter({
                    query: 'value 2'
                });

                expect(component.doAutoSelect).toHaveBeenCalled();
            });

            it("should filter the store based on the displayField", function() {
                makeComponent({
                    queryMode: 'local',
                    displayField: 'value'
                });
                var spy = jasmine.createSpy(),
                    store = component.getStore();
                    
                component._pickerStore.on('filterchange', spy);
                component.doFilter({
                    query: 'value 2'
                });

                expect(spy.callCount).toBe(1);
                expect(component._pickerStore.getCount()).toBe(1);
                expect(component._pickerStore.getAt(0).get('value')).toBe('value 2');
            });

            it("should filter the store when minChars not met if force = true", function() {
                makeComponent({
                    queryMode: 'local',
                    displayField: 'value',
                    minChars: 5
                });
                component.doFilter({
                    query: 'foo',
                    force: true
                });
                expect(component._pickerStore.getCount()).toEqual(2);
            });

            it("should filter the store when default minChars (1 for local) is met", function() {
                makeComponent({
                    queryMode: 'local',
                    displayField: 'value'
                });

                var filterSpy = spyOn(component._pickerStore, 'onFilterEndUpdate');

                // minChars is supposed to default to 1
                doTyping('f');

                // So it should filter
                expect(filterSpy).toHaveBeenCalled();
            });

            it("should add to existing filters", function(){
                makeComponent({
                    queryMode: 'local',
                    displayField: 'value'
                });    
                store.filter('value', 'value');
                component.doFilter({
                    query: 'value 3'
                });
                expect(component._pickerStore.getCount()).toBe(5);
            });
            
            it("should remove only the filters added by the combo", function(){
                makeComponent({
                    queryMode: 'local',
                    displayField: 'value'
                });
                store.filter('value', 'value');
                component.doFilter({
                    query: 'value 3'
                });
                component.doFilter({
                    force: true
                });
                expect(component._pickerStore.getCount()).toBe(7);
            });

            it("should return true if the query was not vetoed", function() {
                makeComponent({
                    queryMode: 'local',
                    displayField: 'value'
                });
                
                var ret = component.doFilter({
                    query: 'value 2'
                });
                
                expect(ret).toBe(true);
            });
        });

        describe("remote queryMode", function() {
            it("should call the store's load method", function() {
                makeComponent({
                    queryMode: 'remote',
                    displayField: 'value'
                });
                spyOn(component.getStore(), 'load');
                component.doFilter({
                    query: 'foobar'
                });
                expect(component.getStore().load.callCount).toEqual(1);
            });
            
            it("should return true if the query was not vetoed", function() {
                makeComponent({
                    queryMode: 'remote',
                    displayField: 'value'
                });
                
                var ret = component.doFilter({
                    query: 'blerg'
                });
                
                expect(ret).toBe(true);
            });

            it("should not filter the store until the default minChars (4 for remote) met", function() {
                makeComponent({
                    queryMode: 'remote',
                    displayField: 'value',
                    queryDelay: 0
                });
                var loadSpy = spyOn(component.getStore(), 'load');

                // minChars is supposed to default to 4, only type 3
                doTyping('foo');

                // Not yet
                expect(loadSpy).not.toHaveBeenCalled();

                // Now we typed 4 characters
                doTyping('foob');

                // It should have queried
                expect(loadSpy).toHaveBeenCalled();
            });
        });

        describe("beforeFilter event", function() {
            it("should fire the 'beforeFilter' event", function() {
                makeComponent();
                
                var spy = jasmine.createSpy(),
                    lastQuery = component.lastQuery;

                component.on('beforequery', spy);
                component.doFilter({
                    query: 'foobar',
                    force: true
                });
                expect(spy).toHaveBeenCalledWith({
                    filterGeneration: 6, // Magic number. This is where the filter ends up.
                    query: 'foobar',
                    lastQuery: lastQuery,
                    force: true,
                    combo: component,
                    cancel: false
                });
                expect(component.lastQuery).toBeDefined();
            });
            
            it("should not query if a 'beforeFilter' handler returns false", function() {
                makeComponent();
                
                component.on('beforequery', function() {
                    return false;
                });
                expect(component.hasOwnProperty('lastQuery')).toBe(false);
            });
            
            it("should not query if a 'beforeFilter' handler sets the query event object's cancel property to true", function() {
                makeComponent();
                
                component.on('beforequery', function(qe) {
                    qe.cancel = true;
                });
                expect(component.hasOwnProperty('lastQuery')).toBe(false);
            });
            
            it("should return false when local query was vetoed", function() {
                makeComponent({
                    queryMode: 'local',
                    displayField: 'value'
                });
                
                component.on('beforequery', function() {
                    return false;
                });
                
                var ret = component.doFilter({
                    query: 'bonzo'
                });
                
                expect(ret).toBe(false);
            });
            
            it("should return false when remote query was vetoed", function() {
                makeComponent({
                    queryMode: 'remote',
                    displayField: 'value'
                });
                
                component.on('beforequery', function() {
                    return false;
                });
                
                var ret = component.doFilter({
                    query: 'throbbe'
                });
                
                expect(ret).toBe(false);
            });
        });

        describe("minChars config", function() {
            it("should not query if the number of entered chars is less than the minChars config", function() {
                makeComponent({
                    minChars: 100
                });
                component.doFilter({
                    query: 'foobar'
                });
                expect(component.hasOwnProperty('lastQuery')).toBe(false);
            });
            it("should ignore the minChars if force = true", function() {
                makeComponent({
                    minChars: 100
                });
                component.doFilter({
                    query: '',
                    force: true
                });
                expect(component.hasOwnProperty('lastQuery')).not.toBe(false);
            });
        });

        it("should expand the dropdown", function() {
            makeComponent();
            spyOn(component, 'expand');
            component.doFilter({
                query: 'text'
            });
            expect(component.expand).toHaveBeenCalled();
        });
    });

    xdescribe('doAutoSelect method', function () {
        it('should highlight the selected item', function () {
            var node;

            makeComponent({
                queryMode: 'local',
                displayField: 'value',
                renderTo: Ext.getBody()
            });

            component.expand();
            component.setValue('value 32');
            node = component.getPicker().getNode(component.getPicker().selModel.lastSelected);

            spyOn(component.getPicker().getNavigationModel(), 'setPosition').andCallThrough();

            component.doAutoSelect();

            expect(component.getPicker().getNavigationModel().setPosition).toHaveBeenCalled();
            expect(Ext.fly(node).hasCls('x-boundlist-item-over')).toBe(true);
        });

        it('should scroll the selected item into view', function () {
            makeComponent({
                queryMode: 'local',
                displayField: 'value',
                renderTo: Ext.getBody()
            });

            component.expand();
            spyOn(component.getPicker().getScrollable(), 'ensureVisible');
            component.setValue('value 32');

            component.doAutoSelect();

            expect(component.getPicker().getScrollable().ensureVisible).toHaveBeenCalled();
        });
        
        it("should select first item when autoSelectLast == false", function() {
            makeComponent({
                autoSelectLast: false,
                queryMode: 'local',
                displayField: 'value',
                renderTo: Ext.getBody()
            });
            
            component.expand();
            component.setValue('value 32');
            
            spyOn(component.getPicker().getNavigationModel(), 'setPosition').andCallThrough();

            component.expand();
            component.doAutoSelect();

            expect(component.getPicker().getNavigationModel().setPosition).toHaveBeenCalled();
            
            var firstNode = component.getPicker().getNode(0);
            
            expect(Ext.fly(firstNode).hasCls('x-boundlist-item-over')).toBe(true);
        });

        it("should autoSelect if you type an blur", function() {
            makeComponent({
                autoSelectLast: false,
                queryMode: 'local',
                value: 'value 32',
                renderTo: Ext.getBody()
            });
            component.expand();
            jasmine.focusAndWait(component);

            runs(function() {
                doTyping('text 34');
            });

            jasmine.blurAndWait(component);

            waitsFor(function() {
                return !component.hasFocus;
            });

            runs(function() {
                component.expand();
                expect(component.getPicker().getSelectable().lastSelected.get('value')).toBe('value 34');
            });
        });
    });

    describe("doRawFilter method", function() {
        it("should call the doFilter method with the contents of the field", function() {
            makeComponent({
                renderTo: Ext.getBody()
            });
            spyOn(component, 'doFilter');
            component.inputElement.dom.value = 'foobar';
            component.doRawFilter();
            expect(component.doFilter).toHaveBeenCalledWith({
                query: 'foobar'
            });
        });
    });

    describe('trigger click', function() {
        it("should perform an 'all' query with the allQuery config if triggerAction='all'", function() {
            makeComponent({
                renderTo: Ext.getBody(),
                triggerAction: 'all',
                allQuery: 'the-all-query'
            });
            spyOn(component, 'doFilter');
            component.onExpandTap();
            expect(component.doFilter).toHaveBeenCalledWith({
                query: 'the-all-query',
                force: true
            });
        });

        it("should perform a query with the current field value if triggerAction='query'", function() {
            makeComponent({
                renderTo: Ext.getBody(),
                triggerAction: 'query',
                allQuery: 'the-all-query',
                value: 'value 2',
                valueField: 'value'
            });
            spyOn(component, 'doFilter');
            component.onExpandTap();
            expect(component.doFilter).toHaveBeenCalledWith({
                query: 'text 2'
            });
        });

        describe('emptyText list config and no store data', function () {
            var wasCalled = false,
                defaultCfg;

            beforeEach(function () {
                defaultCfg = {
                    queryMode: 'local',
                    store: new Ext.data.Store({
                        proxy: {
                            type: 'memory'
                        },
                        model: CBTestModel,
                        data: []
                    }),
                    renderTo: Ext.getBody()
                };
            });

            afterEach(function () {
                wasCalled = false;
            });

            it('should expand the bound list and display the empty text if configured', function () {
                makeComponent(Ext.apply(defaultCfg, {
                    floatedPicker: {
                        emptyText: 'derp',
                        type: 'floated'
                    }
                }));

                spyOn(component, 'expand').andCallThrough();
                component.onExpandTap();

                expect(component.expand).toHaveBeenCalled();
                expect(component.getPicker().getEmptyText()).toBe('derp');
            });

            it('should not expand the bound list and display the empty text if not configured', function () {
                makeComponent(defaultCfg);

                spyOn(component, 'expand');
                component.onExpandTap();
                expect(component.expand).not.toHaveBeenCalled();
            });

            it('should expand the bound list and fire the `expand` event if configured', function () {
                makeComponent(Ext.apply(defaultCfg, {
                    floatedPicker: {
                        emptyText: 'derp',
                        type: 'floated'
                    },
                    listeners: {
                        expand: function () {
                            wasCalled = true;
                        }
                    }
                }));

                spyOn(component, 'expand').andCallThrough();
                component.onExpandTap();

                expect(wasCalled).toBe(true);
            });
        });
    });

    xdescribe("keyboard input", function() {
        beforeEach(function() {
            makeComponent({
                renderTo: Ext.getBody(),
                queryMode: 'local',
                valueField: 'value',
                queryDelay: 1
            });
        });

        it("should initiate a query after the queryDelay", function() {
            runs(function() {
                spyOn(component, 'doFilter');
                component.inputElement.dom.value = 'foob';
                jasmine.fireKeyEvent(component.inputElement.dom, 'keyup', 66);
            });
            waitsFor(function() {
                return component.doFilter.callCount > 0;
            }, 'query not executed');
            runs(function() {
                expect(component.doFilter.mostRecentCall.args).toEqual([{query: 'foob'}]);
            });
        });
        it("should not respond to special keys", function() {
            component.inputElement.dom.value = 'foob';

            // Wait for async textinput event to fire on platforms where it fires.
            // It's not universally supported so we cannot use waitsFor
            waits(100);

            runs(function() {
                spyOn(component, 'doFilter');
                jasmine.fireKeyEvent(component.inputElement.dom, 'keyup', Ext.event.Event.DOWN);
            });
            waits(10);
            runs(function() {
                expect(component.doFilter).not.toHaveBeenCalled();
            });
        });
        it("should respond to backspace", function() {
            component.inputElement.dom.value = 'foob';

            // Wait for async textinput event to fire on platforms where it fires.
            // It's not universally supported so we cannot use waitsFor
            waits(100);

            runs(function() {
                spyOn(component, 'doFilter');
                jasmine.fireKeyEvent(component.inputElement.dom, 'keyup', Ext.event.Event.BACKSPACE);
            });
            waitsFor(function() {
                return component.doFilter.callCount > 0;
            }, 'query not executed');
        });
        it("should respond to delete", function() {
            component.inputElement.dom.value = 'foob';

            // Wait for async textinput event to fire on platforms where it fires.
            // It's not universally supported so we cannot use waitsFor
            waits(100);

            runs(function() {
                spyOn(component, 'doFilter');
                jasmine.fireKeyEvent(component.inputElement.dom, 'keyup', Ext.event.Event.DELETE);
            });
            waitsFor(function() {
                return component.doFilter.callCount > 0;
            }, 'query not executed');
        });

        // Explicitl blurring doesn't work on IE, so use itNotIE
        itNotIE('should select the value upon tab', function() {
            // FIXME the component.inputElement.dom.focus(); calls should not be necessary
            // Expand the picker
            component.inputElement.dom.focus();
            jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.DOWN);
            var selModel = component.getPicker().getSelectable(),
                hideSpy;

            // Picker should be visible
            expect(component.getPicker().isVisible()).toBe(true);
            hideSpy = spyOnEvent(component.getPicker(), 'hide');

            // But with no selection
            expect(selModel.getSelections().length).toBe(0);

            // This should select the first record, and hide the picker
            component.inputElement.dom.focus();
            jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.TAB);

            // We must wait until after the browser's TAB handling has blurred the field, and therefore hidden the picker
            waitsForSpy(hideSpy, 'hide', 'picker to hide');
            runs(function() {

                // First record should be selected
                expect(selModel.getSelections()[0] === store.getAt(0)).toBe(true);

                // The raw value of the input field should be the display field of the selected record
                expect(getRawValue()).toBe(selModel.getSelections()[0].get(component.displayField));
            });
        });
        
        describe("keyboard interaction", function() {
            var expandSpy, collapseSpy;
            
            function pressKey(key, options) {
                jasmine.asyncPressKey(component.inputElement, key, options);
            }
            
            function expectItem(wantText) {
                runs(function() {
                    var navModel = component.getPicker().getNavigationModel(),
                        rec = navModel.getRecord(),
                        haveText = rec && rec.get('text');
                
                    expect(haveText).toBe(wantText);
                });
            }
            
            beforeEach(function() {
                expandSpy = jasmine.createSpy('expand');
                collapseSpy = jasmine.createSpy('collapse');
                
                component.on({
                    expand: expandSpy,
                    collapse: collapseSpy
                });
            });
            
            afterEach(function() {
                expandSpy = collapseSpy = null;
            });
            
            describe("expand", function() {
                it("should expand on down arrow", function() {
                    pressKey('down');
                    
                    waitForSpy(expandSpy, 'expand');
                    
                    runs(function() {
                        expect(component.expanded).toBe(true);
                    });
                });
                
                it("should expand on alt-down arrow", function() {
                    pressKey('down', { alt: true });
                    
                    waitForSpy(expandSpy, 'expand');
                    
                    runs(function() {
                        expect(component.expanded).toBe(true);
                    });
                });
            });
            
            describe("collapse", function() {
                beforeEach(function() {
                    pressKey('down');
                    
                    waitForSpy(expandSpy, 'expand');
                });
                
                it("should collapse on Esc", function() {
                    pressKey('esc');
                    
                    waitForSpy(collapseSpy, 'collapse');
                    
                    runs(function() {
                        expect(component.expanded).toBe(false);
                    });
                });
                
                it("should collapse on Alt-Up arrow", function() {
                    pressKey('up', { alt: true });
                    
                    waitForSpy(collapseSpy, 'collapse');
                    
                    runs(function() {
                        expect(component.expanded).toBe(false);
                    });
                });
                
                it("should remove aria-activedescendant", function() {
                    pressKey('esc');
                    
                    waitForSpy(collapseSpy, 'collapse');
                    
                    runs(function() {
                        expect(component).not.toHaveAttr('aria-activedescendant');
                    });
                });
            });
            
            describe("arrow keys", function() {
                describe("down arrow", function() {
                    beforeEach(function() {
                        pressKey('down');
                        
                        waitForSpy(expandSpy, 'expand');
                    });
                    
                    describe("initial", function() {
                        it("should select first item", function() {
                            expectItem('text 1');
                        });
                        
                        it("should set aria-activedescendant to first item", function() {
                            var item = component.getPicker().getNode(0);
                        
                            // aria-activedescendant is set on the inputEl!
                            expect(component).toHaveAttr('aria-activedescendant', item.id);
                        });
                    });
                    
                    describe("subsequent", function() {
                        beforeEach(function() {
                            pressKey('down');
                            
                            jasmine.waitAWhile();
                        });
                        
                        it("should select 2nd item", function() {
                            expectItem('text 2');
                        });
                        
                        it("should set aria-activedescendant to 2nd item", function() {
                            var item = component.getPicker().getNode(1);
                            
                            expect(component).toHaveAttr('aria-activedescendant', item.id);
                        });
                    });
                });
                
                describe("alt-down arrow", function() {
                    beforeEach(function() {
                        pressKey('down', { alt: true });
                        
                        waitForSpy(expandSpy, 'expand', 1000);
                    });
                    
                    describe("initial", function() {
                        it("should not highlight items after expanding", function() {
                            expect(component.getPicker().highlightedItem).not.toBeDefined();
                        });
                    });
                    
                    describe("subsequent", function() {
                        beforeEach(function() {
                            pressKey('down', { alt: true });
                            
                            waitAWhile();
                        });
                        
                        it("should not highlight items on subsequent alt-down arrow key", function() {
                            expect(component.getPicker().highlightedItem).not.toBeDefined();
                        });
                    });
                });
                
                describe("up arrow", function() {
                    beforeEach(function() {
                        pressKey('down');
                        
                        waitForSpy(expandSpy, 'expand', 1000);
                        
                        pressKey('down');
                        pressKey('down');
                        pressKey('down');
                        pressKey('up');
                    });
                    
                    it("should select 3rd item", function() {
                        expectItem('text 3');
                    });
                    
                    it("should set aria-activedescendant to 3rd item", function() {
                        var item = component.getPicker().getNode(2);
                        
                        expect(component).toHaveAttr('aria-activedescendant', item.id);
                    });
                });
                
                describe("alt-up arrow", function() {
                    beforeEach(function() {
                        pressKey('down', { alt: true });
                        
                        waitForSpy(expandSpy, 'expand', 1000);
                    });
                    
                    describe("initial", function() {
                        beforeEach(function() {
                            pressKey('up', { alt: true });
                            
                            waitAWhile();
                        });
                        
                        it("should not highlight items", function() {
                            expect(component.getPicker().highlightedItem).not.toBeDefined();
                        });
                        
                        // This should operate on the closed picker
                        describe("subsequent", function() {
                            beforeEach(function() {
                                pressKey('up', { alt: true });
                                
                                waitAWhile();
                            });
                            
                            it("should not highlight items either", function() {
                                expect(component.getPicker().highlightedItem).not.toBeDefined();
                            });
                        });
                    });
                });
            });
        });
    });

    xdescribe("keyboard input with multiSelect", function() {
        beforeEach(function() {
            makeComponent({
                renderTo: Ext.getBody(),
                queryMode: 'local',
                valueField: 'value',
                multiSelect: true
            });
        });

        it('should select the value upon tab with multiSelect', function() {
            var sm,
                selected,
                rawVal = '',
                hideSpy;

            // Expand the picker
            jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.DOWN);

            // Picker should be visible
            expect(component.getPicker().isVisible()).toBe(true);
            hideSpy = spyOnEvent(component.getPicker(), 'hide');
            sm = component.getPicker().selModel;

            // But with no selection
            expect(sm.getSelections().length).toBe(0);

            // This should select the 1st record
            jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.ENTER);
            selected = sm.getSelections();
            expect(selected.length).toBe(1);
            expect(selected[0] === store.getAt(0)).toBe(true);

            // This should DEselect the 1st record
            jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.ENTER);

            // No select 2nd and 3rd records
            jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.DOWN);
            jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.ENTER);
            jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.DOWN);
            jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.ENTER);
            selected = sm.getSelections();
            expect(selected.length).toBe(2);
            expect(selected[0] === store.getAt(1)).toBe(true);
            expect(selected[1] === store.getAt(2)).toBe(true);

            // This should select the 4th record, and hide the picker
            jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.DOWN);
            jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.TAB);

            // Wait for the browser's TAB handling to complete and the picker to hide
            waitsForSpy(hideSpy, 'picker to hide');
            runs(function() {
                selected = sm.getSelections();

                // 4th record should now be selected
                expect(selected.length).toBe(3);
                expect(selected[2] === store.getAt(3)).toBe(true);

                for (var i = 0, len = selected.length; i < len; i++) {
                    if (i > 0) {
                        rawVal += ', ';
                    }
                    rawVal += selected[i].get(component.displayField);
                }

                // The raw value of the input field should be the display field of the selected record
                expect(getRawValue()).toEqual(rawVal);
            });
        });
    });

    xdescribe("forceSelection", function(){
        it('should not clear the raw value', function() {
            store.load();
            makeComponent({
                displayField: 'text',
                valueField: 'value',
                forceSelection: true,
                typeAhead: true,
                queryMode: 'local',
                renderTo: Ext.getBody()
            });

            var typeaheadSpy = spyOn(component, 'onTypeAhead').andCallThrough();
            setRawValue('t');
            component.doRawFilter();

            // EXTJS-15501 - It was the typeahead processing that broke it.
            waitsFor(function() {
                return typeaheadSpy.callCount > 0;
            });
            runs(function() {
                expect(component.inputElement.dom.value).toBe('text 1');
            });
        });

        describe("setting value to a value not in the Store with forceSelection: false", function() {
            it("should set passed value", function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    forceSelection: false
                });
                component.setValue("NOT IN STORE");
                expect(component.getValue()).toBe('NOT IN STORE');
            });

            it("should not collapse the list if there are items in the store", function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    forceSelection: false,
                    queryMode: 'remote'
                });
                component.expand();
                component.setValue('asdf');
                expect(component.getPicker().isVisible()).toBe(true);
            });
        });

        describe("not multi", function() {
            describe("with no value", function() {
                beforeEach(function() {
                    makeComponent({
                        displayField: 'text',
                        valueField: 'value',
                        forceSelection: true,
                        queryMode: 'local',
                        renderTo: Ext.getBody()
                    });
                });

                it("should set the underlying value on blur", function() {
                    jasmine.focusAndWait(component);
                    runs(function() {
                        doTyping('text 2');
                    });
                    jasmine.blurAndWait(component);
                    runs(function() {
                        expect(getRawValue()).toBe('text 2');
                        expect(component.getValue()).toBe('value 2');
                    });
                });

                it("should find the first matching text value", function() {
                    jasmine.focusAndWait(component);
                    runs(function() {
                        doTyping('Foo');
                    });
                    jasmine.blurAndWait(component);
                    runs(function() {
                        expect(getRawValue()).toBe('Foo');
                        expect(component.getValue()).toBe('foo1');
                    });
                });

                it("should empty the value if nothing matches", function() {
                    jasmine.focusAndWait(component);
                    runs(function() {
                        doTyping('bar');
                    });
                    jasmine.blurAndWait(component);
                    runs(function() {
                        expect(getRawValue()).toBe('');
                        expect(component.getValue()).toBeNull();
                    });
                });

                itNotIE9m("should not clear the combobox with custom itemTpl on blur after setValue", function() {
                    component.destroy();
                    makeComponent({
                        displayField: 'text',
                        valueField: 'value',
                        forceSelection: true,
                        queryMode: 'local',
                        renderTo: Ext.getBody(),
                        itemTpl: 'Id= {val} - {text}'
                    });
                    jasmine.focusAndWait(component, null, 'component to focus for the first time');
                    runs(function() {
                        component.setValue('value 2');
                    });
                    jasmine.blurAndWait(component, null, 'component to blur for the first time');

                    jasmine.focusAndWait(component, null, 'component to focus for the second time');

                    jasmine.blurAndWait(component, null, 'component to blur for the second time');

                    runs(function() {
                        expect(component.inputElement.dom.value).not.toBe('');
                    });
                });
            });

            describe("with a current value", function() {
                describe("via configuration", function() {
                    function makeWithValue(value, cfg) {
                        makeComponent(Ext.apply({
                            displayField: 'text',
                            valueField: 'value',
                            forceSelection: true,
                            queryMode: 'local',
                            value: value,
                            renderTo: Ext.getBody()
                        }, cfg));
                    }

                    it("should set the underlying value on blur", function() {
                        makeWithValue('value 31');
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('text 2');
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('text 2');
                            expect(component.getValue()).toBe('value 2');
                        });
                    });

                    it("should find the first matching text value", function() {
                        makeWithValue('value 31');
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('Foo');
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('Foo');
                            expect(component.getValue()).toBe('foo1');
                        });
                    });

                    it("should restore the previous value if nothing matches", function() {
                        makeWithValue('value 31');
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('bar');
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('text 31');
                            expect(component.getValue()).toBe('value 31');
                        });
                    });

                    it("should not overwrite a known value with a matching display value", function() {
                        makeWithValue('foo2');
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('Foo');
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('Foo');
                            expect(component.getValue()).toBe('foo2');
                        });
                    });

                    it("should restore the value if it has been cleared", function() {
                        makeWithValue('foo2');
                        component.setRequired(true);
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('', true);
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('Foo');
                            expect(component.getValue()).toBe('foo2');
                        });
                    });
                    
                    it("should not restore the value if it has been cleared and required is false", function() {
                        makeWithValue('foo2');
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('', true);
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('');
                            expect(component.getValue()).toBe(null);
                        });
                    });
                    
                    itNotIE9m('should not clear the combobox custom itemTpl and calling setValue on blur', function() {

                        makeWithValue('value 1',{
                            itemTpl: 'Id= {val} - {text}'
                        });
                        jasmine.focusAndWait(component);
                        runs(function() {
                            clickListItem('value 2', component.getStore());
                            component.setValue('value 2');
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(component.inputElement.dom.value).not.toBe('');
                        });
                    });
                });

                describe("value via selecting from the list", function() {
                    beforeEach(function() {
                        makeComponent({
                            displayField: 'text',
                            valueField: 'value',
                            forceSelection: true,
                            queryMode: 'local',
                            renderTo: Ext.getBody()
                        });
                    });

                    it("should set the underlying value on blur", function() {
                        clickListItem('value 31');
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('text 2');
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('text 2');
                            expect(component.getValue()).toBe('value 2');
                        });
                    });

                    it("should find the first matching text value", function() {
                        clickListItem('value 31');
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('Foo');
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('Foo');
                            expect(component.getValue()).toBe('foo1');
                        });
                    });

                    it("should restore the previous value if nothing matches", function() {
                        clickListItem('value 31');
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('bar');
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('text 31');
                            expect(component.getValue()).toBe('value 31');
                        });
                    });

                    it("should not overwrite a known value with a matching display value", function() {
                        clickListItem('foo2');
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('Foo');
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('Foo');
                            expect(component.getValue()).toBe('foo2');
                        });
                    });

                    it("should restore the value if it has been cleared", function() {
                        clickListItem('foo2');
                        component.setRequired(true);
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('', true);
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('Foo');
                            expect(component.getValue()).toBe('foo2');
                        });
                    });

                    it("should not restore the value if it has been cleared and required is false", function() {
                        clickListItem('foo2');
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('', true);
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('');
                            expect(component.getValue()).toBe(null);
                        });
                    });
                });

                describe("value via setValue", function() {
                    beforeEach(function() {
                        makeComponent({
                            displayField: 'text',
                            valueField: 'value',
                            forceSelection: true,
                            queryMode: 'local',
                            renderTo: Ext.getBody()
                        });
                    });

                    it("should set the underlying value on blur", function() {
                        component.setValue('value 31');
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('text 2');
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('text 2');
                            expect(component.getValue()).toBe('value 2');
                        });
                    });

                    it("should find the first matching text value", function() {
                        component.setValue('value 31');
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('Foo');
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('Foo');
                            expect(component.getValue()).toBe('foo1');
                        });
                    });

                    it("should restore the previous value if nothing matches", function() {
                        component.setValue('value 31');
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('bar');
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('text 31');
                            expect(component.getValue()).toBe('value 31');
                        });
                    });

                    it("should not overwrite a known value with a matching display value", function() {
                        component.setValue(store.last());
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('Foo');
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('Foo');
                            expect(component.getValue()).toBe('foo2');
                        });
                    });

                    it("should restore the value if it has been cleared", function() {
                        component.setValue('value 31');
                        component.setRequired(true);
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('', true);
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('text 31');
                            expect(component.getValue()).toBe('value 31');
                        });
                    });

                    it("should not restore the value if it has been cleared and required is false", function() {
                        component.setValue('value 31');
                        jasmine.focusAndWait(component);
                        runs(function() {
                            doTyping('', true);
                        });
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(getRawValue()).toBe('');
                            expect(component.getValue()).toBe(null);
                        });
                    });
                });

                describe("clearing the value", function() {
                    beforeEach(function() {
                        makeComponent({
                            displayField: 'text',
                            valueField: 'value',
                            forceSelection: true,
                            queryMode: 'local',
                            renderTo: Ext.getBody()
                        });
                    });

                    it("should not set the value after calling clearValue", function() {
                        component.setValue('value 1');
                        component.clearValue();
                        jasmine.focusAndWait(component);
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(component.getValue()).toBeNull();
                        });
                    });

                    it("should not set the value after calling setValue(null)", function() {
                        component.setValue('value 1');
                        component.setValue(null);
                        jasmine.focusAndWait(component);
                        jasmine.blurAndWait(component);
                        runs(function() {
                            expect(component.getValue()).toBeNull();
                        });
                    });
                });
            });

            describe("with remote loading", function() {
                beforeEach(function() {
                    MockAjaxManager.addMethods();
                });

                afterEach(function() {
                    MockAjaxManager.removeMethods();
                });

                // This test seems to randomly fail on FF in the test runner.
                // The component doesn't get focused and fails out
                (Ext.isGecko ? xit : it)("should clear an unmatched value when the store loads", function() {
                    store.destroy();
                    store = new Ext.data.Store({
                        model: CBTestModel,
                        proxy: {
                            type: 'ajax',
                            url: 'foo'
                        }
                    });
                    makeComponent({
                        store: store,
                        displayField: 'text',
                        valueField: 'value',
                        forceSelection: true,
                        queryMode: 'remote',
                        renderTo: Ext.getBody()
                    });
                    jasmine.focusAndWait(component);
                    runs(function() {
                        // Simulate user typing
                        setRawValue('foobar');
                        component.doRawFilter();
                        // Collapse to prevent focus issues
                        component.collapse();
                    });
                    jasmine.blurAndWait(component);
                    runs(function() {
                        Ext.Ajax.mockComplete({
                            status: 200,
                            responseText: '[]'
                        });
                        expect(component.getValue()).toBeNull();
                    });
                });
            });
        });

        describe("with remote loading", function() {
            beforeEach(function() {
                MockAjaxManager.addMethods();
            });

            afterEach(function() {
                MockAjaxManager.removeMethods();
            });

            function completeWithData(data) {
                Ext.Ajax.mockComplete({
                    status: 200,
                    responseText: Ext.JSON.encode(data || [])
                });
            }

            // Blurring doesn't work in IE, so use itNotIE
            itNotIE("should clear an unmatched value when the store loads, second version!", function() {
                store.destroy();
                store = new Ext.data.Store({
                    model: CBTestModel,
                    proxy: {
                        type: 'ajax',
                        url: 'foo'
                    }
                });
                makeComponent({
                    store: store,
                    displayField: 'text',
                    valueField: 'value',
                    forceSelection: true,
                    queryMode: 'remote',
                    renderTo: Ext.getBody()
                });
                jasmine.focusAndWait(component);
                runs(function() {
                    // Simulate user typing
                    setRawValue('foobar');
                    component.doRawFilter();
                    component.collapse();
                });
                jasmine.blurAndWait(component);
                runs(function() {
                    completeWithData();
                    expect(component.getValue()).toBeNull();
                });
            });

            it("should not clear an unmatched value while typing and forceSelection is true", function() {
                store.destroy();
                store = new Ext.data.Store({
                    model: CBTestModel,
                    proxy: {
                        type: 'ajax',
                        url: 'foo'
                    },
                    autoLoad: true
                });
                makeComponent({
                    store: store,
                    displayField: 'text',
                    valueField: 'value',
                    forceSelection: true,
                    queryMode: 'remote',
                    renderTo: Ext.getBody()
                });

                setRawValue('foobar');
                component.doFilter({
                    query: 'foobar'
                });
                completeWithData();
                setRawValue('foob');
                component.doFilter({
                    query: 'foob'
                });
                completeWithData();
                expect(component.inputElement.dom.value).toBe('foob');
            });
        });
    });

    describe('Always refilter if dropdown is visible, regardless of minChars threshold', function() {
        var combo;

        beforeEach(function() {
            combo = Ext.create('Ext.form.field.ComboBox', {
                renderTo: Ext.getBody(),
                options: ['first-1', 'first-2', 'first-3', 'first-4', 'first-5', 'does not match query'],
                queryMode: 'local',
                required: true,
                forceSelection: true,
                minChars: 7
            });
            Ext.override(combo, {
                beforeFilter: function() {
                    var result = Ext.field.ComboBox.prototype.beforeFilter.apply(this, arguments);
                    if (this.getPicker().isVisible()) {
                        result.cancel = false;
                    }
                    return result;
                }
            });
        });
        afterEach(function() {
            combo.destroy();
        });

        it('should refilter when querystring length < minChars if dropdown is visible', function() {
            combo.doFilter({
                query: 'first-1'
            });
            
            // Should filter out all except the 'first-1' value
            expect(combo._pickerStore.getCount()).toEqual(1);

            combo.doFilter({
                query: 'first'
            });
            
            // Should show all the values which match 'first' - that is 5 values
            expect(combo._pickerStore.getCount()).toEqual(5);
        });
    });

    describe('Using the "anyMatch" filter config', function() {
        var combo;

        beforeEach(function() {
            combo = Ext.create('Ext.form.field.ComboBox', {
                renderTo: Ext.getBody(),
                options: ['first-1', 'first-2', 'first-3', 'first-4', 'first-5', 'does not match query'],
                queryMode: 'local',
                required: true,
                forceSelection: true,
                minChars: 2,
                anyMatch: true
            });
        });
        afterEach(function() {
            combo.destroy();
        });

        it('should show all values which contain the query string', function() {
            combo.doFilter({
                query: 'rs'
            });
                        
            // Should show all the values which contain "rs" - that is 5 values
            expect(combo._pickerStore.getCount()).toEqual(5);
        });
    });

    describe('Using the "caseSensitive" filter config', function() {
        var combo;

        beforeEach(function() {
            combo = Ext.create('Ext.form.field.ComboBox', {
                renderTo: Ext.getBody(),
                options: ['first-1', 'first-2', 'first-3', 'first-4', 'first-5', 'does not match query'],
                queryMode: 'local',
                required: true,
                forceSelection: true,
                minChars: 2,
                caseSensitive: true
            });
        });
        afterEach(function() {
            combo.destroy();
        });

        it('should fail to match because caseSensitive is set', function() {
            combo.doFilter({
                query: 'FIRST'
            });
                        
            // Should do case sensitive filtering
            expect(combo._pickerStore.getCount()).toEqual(0);
        });
    });

    describe("setValue(null)", function() {
        function makeClearCombo(value) {
            var cfg = {
                displayField: 'text',
                valueField: 'value',
                renderTo: Ext.getBody()
            };
            if (value) {
                cfg.value = value;
            }
            makeComponent(cfg);
        }

        describe("with no value", function() {
            it("should have an empty value", function() {
                makeClearCombo();
                component.setValue(null);
                expect(getRawValue()).toBe('');
                expect(component.getValue()).toBe('');
            });
        });

        describe("with a current value", function() {
            describe("via configuration", function() {
                it("should have an empty value", function() {
                    makeClearCombo('value 31');
                    component.clearValue();
                    expect(getRawValue()).toBe('');
                    expect(component.getValue()).toBe('');
                });
            });

            describe("value via selecting from the list", function() {
                it("should have an empty value", function() {
                    makeClearCombo();
                    clickListItem('value 31');
                    component.clearValue();
                    expect(getRawValue()).toBe('');
                    expect(component.getValue()).toBe('');
                });
            });

            describe("value via setValue", function() {
                it("should have an empty value", function() {
                    makeClearCombo();
                    component.setValue('value 31');
                    component.clearValue();
                    expect(getRawValue()).toBe('');
                    expect(component.getValue()).toBe('');
                });
            });
        });
    });

    describe("reset", function() {
        describe("with no configured value", function() {
            beforeEach(function() {
                makeComponent({
                    displayField: 'text',
                    valueField: 'value',
                    renderTo: Ext.getBody()
                });
            });

            it("should restore the original value", function() {
                component.reset();
                expect(getRawValue()).toBe('');
                expect(component.getValue()).toBe('');
            });

            it("should restore the original value after selecting a list item", function() {
                clickListItem('value 1');
                component.reset();
                expect(getRawValue()).toBe('');
                expect(component.getValue()).toBe('');
            });

            it("should restore the original value after setting the value with setValue", function() {
                component.setValue('value 1');
                component.reset();
                expect(getRawValue()).toBe('');
                expect(component.getValue()).toBe('');
            });
        });

        describe("with a configured value", function() {
            beforeEach(function() {
                makeComponent({
                    displayField: 'text',
                    valueField: 'value',
                    value: 'value 31',
                    renderTo: Ext.getBody()
                });
            });

            it("should restore the original value", function() {
                component.reset();
                expect(getRawValue()).toBe('text 31');
                expect(component.getValue()).toBe('value 31');
            });

            it("should restore the original value after selecting a list item", function() {
                clickListItem('value 1');
                component.reset();
                expect(getRawValue()).toBe('text 31');
                expect(component.getValue()).toBe('value 31');
            });

            it("should restore the original value after setting the value with setValue", function() {
                component.setValue('value 1');
                component.reset();
                expect(getRawValue()).toBe('text 31');
                expect(component.getValue()).toBe('value 31');
            });
        });
    });

    describe("local filtering should not filter the supplied store", function() {
        it("should only filter the pickerStore, not the supplied store", function() {
            makeComponent({
                queryMode: 'local',
                renderTo: Ext.getBody()
            });
            var count = store.getCount();
            // Simulate user typing 'text 3'
            setRawValue('text 3');
            component.expand();
            component.doRawFilter();

            // PickerStore has fewer in it. Supplied store is unchanged
            expect(component._pickerStore.getCount()).toBe(5);
            expect(store.getCount()).toBe(count);
        });
    });

    describe('itemTpl', function() {
        describe('should create default', function() {
            beforeEach(function() {
                makeComponent();
            });

            it('itemTpl should be an XTemplate', function() {
                expect(component.getPicker().getItemTpl().isTemplate).toBe(true);
            });

            it('itemTpl html match', function() {
                expect(component.getPicker().getItemTpl().html).toBe('<span class="x-list-label">{' + component.getDisplayField() + ':htmlEncode}</span>');
            });
        });

        describe('should create from string', function () {
            beforeEach(function() {
                makeComponent({
                   itemTpl : '{[typeof values === "string" ? values : values["foo"]]}'
                });
            });

            it('itemTpl should be an XTemplate', function () {
                expect(component.getPicker().getItemTpl().isTemplate).toBe(true);
            });

            it('itemTpl html match', function () {
                expect(component.getPicker().getItemTpl().html).toBe('{[typeof values === "string" ? values : values["foo"]]}');
            });
        });

        describe('should create from array of strings', function () {
            beforeEach(function() {
                makeComponent({
                    itemTpl : [
                        '{[typeof values === "string" ? values : values["foo"]]}'
                    ]
                });
            });

            it('itemTpl should be an XTemplate', function () {
                expect(component.getPicker().getItemTpl().isTemplate).toBe(true);
            });

            it('itemTpl html match', function () {
                expect(component.getPicker().getItemTpl().html).toBe('{[typeof values === "string" ? values : values["foo"]]}');
            });
        });

        it("should have the correct display value when displayField is set in initialize", function() {
            var Cls = Ext.define(null, {
                extend: 'Ext.form.field.ComboBox',

                initialize: function() {
                    this.setValueField('text');
                    this.setDisplayField('value');
                    this.callParent();
                }
            });

            component = new Cls({
                renderTo: Ext.getBody(),
                store: store
            });

            component.setValue('text 31');
            expect(getRawValue()).toBe('value 31');
        });
    });

    describe("events", function() {
        var spy;

        beforeEach(function() {
            spy = jasmine.createSpy();
        });

        afterEach(function() {
            spy = null;
        });

        function makeEventCombo(cfg) {
            makeComponent(Ext.apply({
                renderTo: Ext.getBody(),
                valueField: 'value',
                displayField: 'text'
            }, cfg));
        }

        xdescribe("specialkey", function() {
            beforeEach(function(){
                makeEventCombo({
                    listeners: {
                        specialkey: spy
                    }
                });
            });

            it("should fire specialkey when collapsed", function() {
                jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.ENTER);
                expect(spy).toHaveBeenCalled();
            });

            it("should fire specialkey when expanded", function() {
                component.expand();
                jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.ENTER);
                expect(spy).toHaveBeenCalled();
            });
        });

        describe("change", function() {
            function expectArgs(newVal, oldVal) {
                var args = spy.mostRecentCall.args;
                expect(args[0]).toBe(component);
                expect(args[1]).toBe(newVal);
                expect(args[2]).toBe(oldVal);
            }

            describe("via setValue", function() {
                it("should not fire when configured with a value", function() {
                    makeEventCombo({
                        value: 'value 2',
                        listeners: {
                            change: spy
                        }
                    });
                    expect(spy).not.toHaveBeenCalled();
                });

                it("should fire once when setting an initial value", function() {
                    makeEventCombo();
                    component.on('change', spy);
                    component.setValue('value 1');
                    expect(spy.callCount).toBe(1);
                    expectArgs('value 1', null);
                });

                it("should fire once when modifying an existing value", function() {
                    makeEventCombo();
                    component.setValue('value 2');
                    component.on('change', spy);
                    component.setValue('value 1');
                    expect(spy.callCount).toBe(1);
                    expectArgs('value 1', 'value 2');
                }); 

                it("should fire once when nulling the value", function() {
                    makeEventCombo();
                    component.setValue('value 2');

                    var calls = 0;
                    component.on('change', function (c, v, old) {
                        expect(c).toBe(component);
                        expect(v).toBe('');
                        expect(old).toBe('value 2');
                        ++calls;
                    });

                    component.setValue(null);
                    expect(calls).toBe(1);
                });
            });

            describe("via user interaction", function() {
                it("should fire once when selecting an initial value", function() {
                    makeEventCombo();
                    component.on('change', spy);
                    clickListItem('value 1');
                    expect(spy.callCount).toBe(1);
                    expectArgs('value 1', null);
                });

                it("should fire once when modifying an existing value", function() {
                    makeEventCombo();
                    component.setValue('value 2');
                    component.on('change', spy);
                    clickListItem('value 1');
                    expect(spy.callCount).toBe(1);
                    expectArgs('value 1', 'value 2');
                }); 
            });
        });
    });

    describe("binding", function() {
        var viewModel, spy;

        beforeEach(function() {
            spy = jasmine.createSpy();
            viewModel = new Ext.app.ViewModel();
        });

        afterEach(function() {
            spy = viewModel = null;
        });

        function makeViewModelCombo(cfg) {
            makeComponent(Ext.apply({
                displayField: 'text',
                valueField: 'value',
                viewModel: viewModel,
                renderTo: Ext.getBody()
            }, cfg));
        }

        describe("view model selection", function() {
            function getByVal(val) {
                var index = store.findExact('value', val);
                return store.getAt(index);
            }

            function selectNotify(rec) {
                component.expand();
                component.getPicker().select(rec);
                viewModel.notify();
                component.collapse();
            }

            describe("reference", function() {
                describe("no initial value", function() {
                    beforeEach(function() {
                        viewModel.bind('{userList.selection}', spy);
                        makeViewModelCombo({
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
                        expect(component.getValue()).toBe('value 1');
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
                        expect(component.getValue()).toBe('value 2');
                    });

                    it("should publish the record when setting the value", function() {
                        component.setValue('value 1');
                        viewModel.notify();
                        var args = spy.mostRecentCall.args;
                        expect(args[0]).toBe(getByVal('value 1'));
                        expect(args[1]).toBeNull();
                    });

                    it("should publish the record when the value is changed", function() {
                        component.setValue('value 1');
                        viewModel.notify();
                        spy.reset();
                        component.setValue('value 2');
                        viewModel.notify();
                        var args = spy.mostRecentCall.args;
                        expect(args[0]).toBe(getByVal('value 2'));
                        expect(args[1]).toBe(getByVal('value 1'));
                    });

                    it("should publish the record when the value is cleared", function() {
                        component.setValue('value 1');
                        viewModel.notify();
                        spy.reset();
                        component.setValue(null);
                        viewModel.notify();
                        var args = spy.mostRecentCall.args;
                        expect(args[0]).toBeNull();
                        expect(args[1]).toBe(getByVal('value 1'));
                    });
                });

                describe("with initial value", function() {
                    beforeEach(function() {
                        viewModel.bind('{userList.selection}', spy);
                        makeViewModelCombo({
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
                        makeViewModelCombo({
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
                            component.setValue('value 1');
                            viewModel.notify();
                            var args = spy.mostRecentCall.args;
                            expect(args[0]).toBe(getByVal('value 1'));
                            expect(args[1]).toBeUndefined();
                        });

                        it("should trigger the binding when the value is changed", function() {
                            component.setValue('value 1');
                            viewModel.notify();
                            spy.reset();
                            component.setValue('value 2');
                            viewModel.notify();
                            var args = spy.mostRecentCall.args;
                            expect(args[0]).toBe(getByVal('value 2'));
                            expect(args[1]).toBe(getByVal('value 1'));
                        });

                        it("should trigger the binding when the value is cleared", function() {
                            component.setValue('value 1');
                            viewModel.notify();
                            spy.reset();
                            component.setValue(null);
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
                            expect(component.getValue()).toBe('value 1');
                        });

                        it("should set the value when updating the record", function() {
                            viewModel.set('foo', getByVal('value 1'));
                            viewModel.notify();
                            viewModel.set('foo', getByVal('value 2'));
                            viewModel.notify();
                            expect(component.getValue()).toBe('value 2');
                        });

                        it("should deselect when clearing the value", function() {
                            viewModel.set('foo', getByVal('value 1'));
                            viewModel.notify();
                            viewModel.set('foo', null);
                            viewModel.notify();
                            expect(component.getValue()).toBe('');
                        });
                    });
                });

                // Not sure if we want to support this, leave this out for now
                xdescribe("with initial value", function() {
                    it("should trigger the binding with an initial value in the combo", function() {
                        viewModel.bind('{foo}', spy);
                        makeViewModelCombo({
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
                        makeViewModelCombo({
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
                            component.expand();

                            Ext.Ajax.mockComplete({
                                status: 200,
                                responseText: Ext.encode([
                                    {id: 1, text: 'text 1', value: 'value 1'},
                                    {id: 2, text: 'text 2', value: 'value 2'}
                                ])
                            });

                            viewModel.notify();
                            component.expand();

                            // After the new record with the same ID arrives the selection must be
                            // synched to contain the new record by that ID.
                            expect(component.getPicker().getSelectable().getSelections()[0]).toBe(store.byValue.get('value 1'));
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
    
    describe("bindStore", function() {
        var newData, newStore;

        beforeEach(function() {
            newData = [
                {text: 'text 1', value: 1},
                {text: 'text 2', value: 2},
                {text: 'text 3', value: 3},
                {text: 'text 4', value: 4},
                {text: 'text 5', value: 5}
            ];
        });

        afterEach(function() {
            newStore = Ext.destroy(newStore);
        });
        
        it("should apply a filter when binding a new store", function() {
            makeComponent({
                queryMode: 'local',
                renderTo: Ext.getBody()
            });
            component.doFilter({
                query: 'text 3'
            });
            
            newStore = new Ext.data.Store({
                model: CBTestModel,
                data: newData
            });
            
            component.setStore(newStore);
            expect(component._pickerStore.getCount()).toBe(1);
        });
        
        it("should be able to filter the store after binding a new one", function() {
            makeComponent({
                queryMode: 'local',
                renderTo: Ext.getBody()
            });
            component.doFilter({
                query: 'text 3'
            });
            
            newStore = new Ext.data.Store({
                model: CBTestModel,
                data: newData
            });
            
            component.setStore(newStore);
            component.doFilter({
                query: 'text 2'
            });
            expect(component._pickerStore.getCount()).toBe(1);
        });

        it("should be able to select after binding a new store", function() {
            makeComponent({
                renderTo: Ext.getBody(),
                queryMode: 'local',
                displayField: 'text',
                valueField: 'value'
            });

            newStore = new Ext.data.Store({
                model: CBTestModel,
                data: newData
            });
            component.setStore(newStore);
            component.expand();
            clickListItem('2', newStore);
            expect(component.getValue()).toBe('2');
        });

        it("should be able to select after binding a store when one wasn't configured", function() {
            makeComponent({
                renderTo: Ext.getBody(),
                queryMode: 'local',
                displayField: 'text',
                valueField: 'value'
            }, true);

            newStore = new Ext.data.Store({
                model: CBTestModel,
                data: newData
            });
            component.setStore(newStore);
            component.expand();
            clickListItem('2', newStore);
            expect(component.getValue()).toBe('2');
        });
    });
    
    describe("setting value with different store states", function() {
        describe("with a store not bound", function() {
            it("should not display the raw value and resolve when the store is bound", function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    queryMode: 'local',
                    displayField: 'text',
                    forceSelection: true,
                    valueField: 'value'
                }, true);

                component.setValue('value 3');

                expect(component.getValue()).toBe('value 3');
                expect(getRawValue()).toBe('');

                component.setStore(store);
                expect(component.getValue()).toBe('value 3');
                expect(getRawValue()).toBe('text 3');
            });
        });

        describe("with a store populated via adding records", function() {
            it("should resolve the display value", function() {
                store.destroy();
                store = new Ext.data.Store({
                    model: CBTestModel
                });

                store.add([
                    {text: 'text 1', value: 'value 1'},
                    {text: 'text 2', value: 'value 2'},
                    {text: 'text 3', value: 'value 3'}
                ]);

                makeComponent({
                    renderTo: Ext.getBody(),
                    queryMode: 'local',
                    displayField: 'text',
                    valueField: 'value'
                });
                component.setValue('value 2');
                expect(component.getValue()).toBe('value 2');
                expect(getRawValue()).toBe('text 2');
            });
        });

        describe("setting a value with a remote store", function() {
            var fakeData, remoteStore,
                ComboModel;
            
            function createStore(cfg) {
                remoteStore = new Ext.data.Store(Ext.apply({
                    model: ComboModel,
                    proxy: {
                        type: 'ajax',
                        url: '/fake'
                    }
                }, cfg));
            }
            
            function completeWithData(data) {
                Ext.Ajax.mockComplete({
                    status: 200,
                    responseText: Ext.JSON.encode(data || fakeData)
                });
            }
            
            beforeEach(function() {
                MockAjaxManager.addMethods();
                ComboModel = Ext.define(null, {
                    extend: 'Ext.data.Model',
                    fields: ['id', 'name']
                });
                
                fakeData = [{
                    id: 1,
                    name: 'Foo'
                }, {
                    id: 2,
                    name: 'Bar'
                }, {
                    id: 3,
                    name: 'Baz'
                }];
            });
            
            afterEach(function() {
                Ext.destroy(remoteStore);
                MockAjaxManager.removeMethods();
                ComboModel = null;
            });
            
            describe("while the store is loading", function() {
                function makeLoadCombo(valueIsName, cfg) {
                    makeComponent(Ext.apply({
                        displayField: 'name',
                        valueField: valueIsName ? 'name' : 'id',
                        store: remoteStore,
                        renderTo: Ext.getBody()
                    }, cfg));
                }

                beforeEach(function() {
                    createStore();
                });
                
                it("should not trigger a second load", function() {
                    makeLoadCombo();
                    remoteStore.load();
                    spyOn(remoteStore, 'load');
                    component.setValue(1);
                    expect(remoteStore.load).not.toHaveBeenCalled();
                });
                
                it("should not trigger a second load with autoLoadOnValue", function() {
                    makeLoadCombo({
                        autoLoadOnValue: true
                    });
                    remoteStore.load();
                    spyOn(remoteStore, 'load');
                    component.setValue(1);
                    expect(remoteStore.load).not.toHaveBeenCalled();
                });
                
                describe("display value", function() {
                    it("should not put the id as the display value while loading", function() {
                        makeLoadCombo();
                        remoteStore.load();
                        component.setValue(1);
                        expect(getRawValue()).toBe('');
                    });

                    it("should use the model raw value as the display value while loading if a model is passed", function() {
                        var valueRecord = new ComboModel({
                            id: 1,
                            name: 'Foo'
                        });

                        makeLoadCombo();
                        remoteStore.load();
                        component.setValue(valueRecord);
                        expect(getRawValue()).toBe('Foo');
                        expect(component.getValue()).toBe(1);

                        // Until the store loads, we have the selection as the record that was passed to setValue.
                        expect(component.getSelection() === valueRecord).toBe(true);

                        // When the store loads, the cachedValue will be matched up with an incoming record
                        // and the selection will be updated.
                        completeWithData(fakeData);

                        // The actual loaded record must have taken over from the placeholder
                        // which was passed to setvalue.
                        expect(component.getSelection() === valueRecord).toBe(false);
                    });

                    it("should use the model raw value as the display value before a store arrives from a bind", function() {
                        var valueRecord = new ComboModel({
                            id: 1,
                            name: 'Foo'
                        });

                        makeLoadCombo(null, {
                            store: null
                        });

                        // Starts with no store
                        expect(component.getStore() == null).toBe(true);

                        remoteStore.load();

                        // Should set the value from the record we set into the selection via setValue
                        component.setValue(valueRecord);
                        expect(getRawValue()).toBe('Foo');
                        expect(component.getValue()).toBe(1);

                        // Until the store loads, we have the selection as the record that was passed to setValue.
                        expect(component.getSelection() === valueRecord).toBe(true);

                        // Mimic a store arriving from a bind after field config and rebder.
                        // When the store loads, the cachedValue will be matched up with an incoming record
                        // and the selection will be updated.
                        component.setStore(remoteStore);

                        completeWithData(fakeData);

                        // The actual loaded record must have taken over from the placeholder
                        // which was passed to setValue.
                        expect(component.getSelection() === valueRecord).toBe(false);
                    });


                    it("should use the store's model raw value as the display value before a store arrives from a bind", function() {
                        var valueRecord;

                        makeLoadCombo(null, {
                            store: null
                        });

                        // Starts with no store
                        expect(component.getStore() == null).toBe(true);

                        // Store starts loaded this time.
                        remoteStore.load();
                        completeWithData(fakeData);

                        // We prime the combobox with the real store record while it does
                        // not own a store.
                        valueRecord = remoteStore.first();

                        // Should set the value from the record we set into the selection via setValue
                        component.setValue(valueRecord);
                        expect(getRawValue()).toBe('Foo');
                        expect(component.getValue()).toBe(1);

                        // Until the store arrives, we have the selection as the record that was passed to setValue.
                        expect(component.getSelection() === valueRecord).toBe(true);

                        // Mimic a store arriving from a bind after field config and rebder.
                        // When the store loads, the cachedValue will be matched up with an incoming record
                        // and the selection will be updated.
                        component.setStore(remoteStore);

                        // The actual loaded record is the same as the selection
                        expect(component.getSelection() === valueRecord).toBe(true);
                    });

                    it("should update the display value when the store loads", function() {
                        makeLoadCombo();
                        remoteStore.load();
                        component.setValue(1);
                        completeWithData();
                        expect(getRawValue()).toBe('Foo');
                    });

                    it("should not set the raw value when the set value has no match in the store", function() {
                        makeLoadCombo(true);
                        remoteStore.load();
                        component.setValue('foo');
                        expect(getRawValue()).toBe('');
                    });

                    it("should not set the raw value when the set value has no match in the store, and value is provided by a bind", function () {
                        var vm;

                        makeLoadCombo(true, {
                            emptyText: 'Please select a name',
                            bind: {
                                value: '{user.name}'
                            },
                            viewModel: {}
                        });

                        vm = component.getViewModel();
                        vm.set('user.name', 'foo');
                        vm.notify();

                        expect(getRawValue()).toBe('');
                    });
                });
            });
            
            describe("while having a pending auto load", function() {
                var flushLoadSpy;

                beforeEach(function() {
                    synchronousLoad = false;
                    var extAsap = Ext.asap;
                    
                    flushLoadSpy = spyOn(Ext.data.Store.prototype, 'flushLoad').andCallThrough();

                    Ext.asap = function(fn, scope) {
                        return Ext.defer(fn, 100, scope);
                    };
                    createStore({
                        autoLoad: true
                    });
                    makeComponent({
                        displayField: 'name',
                        valueField: 'id',
                        store: remoteStore,
                        renderTo: Ext.getBody()
                    });
                    Ext.asap = extAsap;
                });
                afterEach(function() {
                    synchronousLoad = true;
                    Ext.data.ProxyStore.prototype.flushLoad = storeFlushLoad;
                });
                
                it("should not trigger a load", function() {
                    spyOn(remoteStore, 'load');
                    component.setValue(1);
                    expect(remoteStore.load).not.toHaveBeenCalled();
                });
                
                it("should not trigger a load with autoLoadOnValue", function() {
                    component.autoLoadOnValue = true;
                    spyOn(remoteStore, 'load');
                    component.setValue(1);
                    expect(remoteStore.load).not.toHaveBeenCalled();
                });
                
                it("should not put the id as the raw value while loading", function() {
                    spyOn(remoteStore, 'load');
                    component.setValue(1);
                    expect(getRawValue()).toBe('');
                });
                
                it("should update the display value when the store loads", function() {
                    component.setValue(1);
                    // Wait for autoLoad
                    waitsForSpy(flushLoadSpy);
                    runs(function() {
                        completeWithData();
                        expect(getRawValue()).toBe('Foo');
                    });
                });
            });
            
            describe("not loading & without autoLoad", function() {
                beforeEach(function() {
                    createStore();
                    makeComponent({
                        autoLoadOnValue: true,
                        displayField: 'name',
                        valueField: 'id',
                        store: remoteStore,
                        renderTo: Ext.getBody()
                    });
                });

                it("should not trigger a load with autoLoadOnValue: false", function() {
                    component.setAutoLoadOnValue(false);
                    spyOn(remoteStore, 'load');
                    component.setValue(1);
                    expect(remoteStore.load).not.toHaveBeenCalled();
                });
                
                it("should not trigger a load if the value is undefined", function() {
                    spyOn(remoteStore, 'load');
                    component.setValue(undefined);
                    expect(remoteStore.load).not.toHaveBeenCalled();
                });
                
                it("should not trigger a load if the value is null", function() {
                    spyOn(remoteStore, 'load');
                    component.setValue(null);
                    expect(remoteStore.load).not.toHaveBeenCalled();
                });

                it("should trigger a load", function() {
                    spyOn(remoteStore, 'load');
                    component.setValue(1);
                    expect(remoteStore.load).toHaveBeenCalled();
                });

                it("should not put the id as the raw value while loading", function() {
                    component.setValue(1);
                    expect(getRawValue()).toBe('');
                });

                it("should update the display value when the store loads", function() {
                    component.setValue(1);
                    completeWithData();
                    expect(getRawValue()).toBe('Foo');
                });

                it("should not update the selection when the store loads if the record is already selected", function() {
                    component.setValue(new ComboModel({
                        id: 4,
                        name: 'Not in payload'
                    }));
                    var doUpdateSelectionSpy = spyOn(component, "updateSelection").andCallThrough();

                    completeWithData();

                    // The value was set from a record.
                    // So it must not be overwritten by the autoLoadOnValue handling.
                    expect(doUpdateSelectionSpy).not.toHaveBeenCalled();
                    expect(getRawValue()).toBe('Not in payload');
                });

                it("should replace the selection when the store loads if the value is already set, and selection should be the newly matched record", function() {
                    var initialRec = new ComboModel({
                        id: 3,
                        name: 'Baz'
                    });
                    component.setValue(initialRec);

                    completeWithData();

                    expect(getRawValue()).toBe('Baz');

                    // The record will be the newly loaded record because the picker's
                    // selection model will resync on load, and the valueCollection *IS*
                    // the selModel's collection.
                    expect(component.getSelection() === initialRec).toBe(false);
                });
            });
            
            describe("while not having a store bound", function() {
                beforeEach(function() {
                    createStore();
                    makeComponent({
                        displayField: 'name',
                        valueField: 'id',
                        renderTo: Ext.getBody()
                    }, true);
                });
                
                it("should not put the id as the raw value when nothing is bound", function() {
                    component.setValue(1);
                    expect(getRawValue()).toBe('');
                });
                
                it("should update the display value when a loaded store is bound", function() {
                    remoteStore.load();
                    completeWithData();
                    component.setValue(1);
                    component.setStore(remoteStore);
                    expect(getRawValue()).toBe('Foo');
                });
                
                it("should update the display value when a loading store is bound", function() {
                    remoteStore.load();
                    component.setValue(1);
                    component.setStore(remoteStore);
                    completeWithData();
                    expect(getRawValue()).toBe('Foo');
                });
                
                describe("with unloaded store", function() {
                    it("should not trigger a load with autoLoadOnValue: false", function() {
                        component.setAutoLoadOnValue(false);
                        component.setOptions([{name: 'one', id: 1}]);
                        component.setValue(1);
                        spyOn(remoteStore, 'load');
                        component.setStore(remoteStore);
                        expect(remoteStore.load).not.toHaveBeenCalled();
                    });
                    
                    it("should trigger a load with autoLoadOnValue: true", function() {
                        component.setAutoLoadOnValue(true);
                        component.setValue(1);
                        spyOn(remoteStore, 'load');
                        component.setStore(remoteStore);
                        expect(remoteStore.load).toHaveBeenCalled();
                    });
                });
            });

            describe("chained stores", function() {
                var chained;

                it("should not update the display value if the source is loading", function() {
                    createStore();
                    chained = new Ext.data.ChainedStore({
                        source: remoteStore
                    });
                    remoteStore.load();
                    makeComponent({
                        displayField: 'name',
                        queryMode: 'local',
                        valueField: 'id',
                        renderTo: Ext.getBody(),
                        store: chained,
                        value: 2
                    });
                    expect(getRawValue()).toBe('');
                    completeWithData();
                    chained.destroy();
                });

                it("should not update the display value if the source has a pending autoLoad", function() {
                    createStore({
                        autoLoad: true
                    });
                    chained = new Ext.data.ChainedStore({
                        source: remoteStore
                    });
                    makeComponent({
                        displayField: 'name',
                        queryMode: 'local',
                        valueField: 'id',
                        renderTo: Ext.getBody(),
                        store: chained,
                        value: 2
                    });
                    expect(getRawValue()).toBe('');
                    chained.destroy();
                });

                it("should update the display value when the source store loads", function() {
                    createStore();
                    chained = new Ext.data.ChainedStore({
                        source: remoteStore
                    });
                    remoteStore.load();
                    makeComponent({
                        displayField: 'name',
                        queryMode: 'local',
                        valueField: 'id',
                        renderTo: Ext.getBody(),
                        store: chained,
                        value: 2
                    });
                    completeWithData();
                    expect(getRawValue()).toBe('Bar');
                    chained.destroy();
                });
            });
        });
    });

    describe("store modifications", function() {
        xdescribe("remove", function() {
            describe("with forceSelection: true", function() {
                it("should not change the value if the removed record is not selected", function() {
                    makeComponent({
                        renderTo: Ext.getBody(),
                        forceSelection: true,
                        displayField: 'text',
                        valueField: 'value',
                        value: 'value 3'
                    });
                    store.removeAt(0);
                    expect(component.getValue()).toBe('value 3');
                });

                it("should clear the value when removing the selected record", function() {
                    makeComponent({
                        renderTo: Ext.getBody(),
                        queryMode: 'local',
                        forceSelection: true,
                        displayField: 'text',
                        valueField: 'value',
                        value: 'value 3'
                    });
                    store.removeAt(2);
                    expect(getRawValue()).toBe('');
                    expect(component.getValue()).toBeNull();
                });
            });

            describe("with forceSelection: false", function() {
                it("should not clear the value when removing the selected record", function() {
                    makeComponent({
                        renderTo: Ext.getBody(),
                        forceSelection: false,
                        displayField: 'text',
                        valueField: 'value',
                        value: 'value 3'
                    });
                    store.removeAt(2);
                    expect(component.getValue()).toBe('value 3');
                });
            });
        });

        describe("update", function() {
            it("should update the raw value when the selected record text is changed", function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    queryMode: 'local',
                    forceSelection: true,
                    displayField: 'text',
                    valueField: 'value',
                    value: 'value 3'
                });
                store.getAt(2).set('text', 'Foo!');
                expect(getRawValue()).toBe('Foo!');
                expect(component.getValue()).toBe('value 3');
            });
        });

        describe("filtering", function() {
            it("should clear the selected value when the record is filtered out", function() {
                makeComponent({
                    renderTo: Ext.getBody(),
                    forceSelection: true,
                    displayField: 'text',
                    valueField: 'value',
                    value: 'value 3',
                    queryMode: 'local'
                });
                store.getFilters().add(function(rec) {
                    return rec.get('value') !== 'value 3';
                });
                expect(getRawValue()).toBe('');
                expect(component.getValue()).toBeNull();
            });
        });
    });

    describe("chained stores", function() {
        it("should allow a non-record value to be used with forceSelection: false", function() {
            var chained = new Ext.data.ChainedStore({
                source: store
            });
            makeComponent({
                store: chained,
                displayField: 'text',
                valueField: 'value',
                forceSelection: false,
                queryMode: 'local'
            });
            component.setValue('Foo');
            expect(component.getValue()).toBe('Foo');
        });
    });

    xdescribe('alternate components as the picker', function () {
        // See EXTJS-13089 and EXTJS-14151.
        var c, panel, dom;

        describe('grid as picker', function () {
            beforeEach(function () {
                c = new Ext.form.field.ComboBox({
                    createPicker: function () {
                        panel = new Ext.grid.Panel({
                            id: 'foo',
                            columns: [
                                { dataIndex: 'company', text: 'Company' },
                                { dataIndex: 'price', text: 'Price' }
                            ],
                            store: new Ext.data.ArrayStore({
                                fields: [
                                    {name: 'company'},
                                    {name: 'price', type: 'float'}
                                ],
                                data: [
                                    ['3m Co', 71.72],
                                    ['Alcoa Inc', 29.01],
                                    ['Boeing Co.', 75.43]
                                ]
                            }),
                            width: 250,
                            draggable: true,
                            simpleDrag: true,
                            floated: true
                        });

                        return panel;
                    },
                    renderTo: Ext.getBody()
                });

                c.expand();
            });

            afterEach(function () {
                Ext.destroy(c);
                c = panel = dom = null;
            });

            it('should have an ownerCmp reference to the combo', function () {
                expect(panel.ownerCmp === c).toBe(true);
            });

            it('should be able to be looked up by CQ', function () {
                expect(c.owns(panel.el)).toBe(true);
            });

            it('should be able to use the ghost panel in the CQ hierarchy when dragging', function () {
                dom = c.getPicker().header.el.dom;

                jasmine.fireMouseEvent(dom, 'mousedown');
                // Moving the dom element will trigger the ghost cmp.
                jasmine.fireMouseEvent(dom, 'mousemove', 0, 1000);

                // Now that the ghost cmp has been created, let's get a ref to it.
                dom = Ext.getCmp('foo-ghost').el.dom;

                expect(c.owns(Ext.fly(dom))).toBe(true);

                jasmine.fireMouseEvent(dom, 'mouseup');
            });

            it('should inject a getRefOwner API that returns a reference to the combo', function () {
                dom = c.getPicker().header.el.dom;

                jasmine.fireMouseEvent(dom, 'mousedown');
                // Moving the dom element will trigger the ghost cmp.
                jasmine.fireMouseEvent(dom, 'mousemove', 0, 1000);

                expect(Ext.getCmp('foo-ghost').getRefOwner()).toBe(c);

                jasmine.fireMouseEvent(dom, 'mouseup');
            });

            it('should share the same reference between the picker and the ghost panel', function () {
                dom = c.getPicker().header.el.dom;

                jasmine.fireMouseEvent(dom, 'mousedown');
                // Moving the dom element will trigger the ghost cmp.
                jasmine.fireMouseEvent(dom, 'mousemove', 0, 1000);

                expect(Ext.getCmp('foo-ghost').getRefOwner()).toBe(panel.ownerCmp);

                jasmine.fireMouseEvent(dom, 'mouseup');
            });
        });
    });

    describe('EXTJS-15045', function() {
        function completeWithData(data) {
            Ext.Ajax.mockComplete({
                status: 200,
                responseText: Ext.JSON.encode(data)
            });
        }

        beforeEach(function() {
            MockAjaxManager.addMethods();
        });

        afterEach(function() {
            MockAjaxManager.removeMethods();
        });

        it('should allow mouse selection', function() {
            store = new Ext.data.Store({
                proxy: {
                    type: 'ajax',
                    url: 'fakeUrl'
                },
                model: CBTestModel
            });
            makeComponent({
                renderTo: Ext.getBody(),
                minChars: 0,
                valueField: 'value',
                queryDelay: 1
            });
            
            jasmine.focusAndWait(component);

            doTyping('t', false, true);
            completeWithData([
                {text: 'text 10', value: 'value 10'},
                {text: 'text 11', value: 'value 11'},
                {text: 'text 12', value: 'value 12'},
                {text: 'text 31', value: 'value 31'},
                {text: 'text 32', value: 'value 32'},
                {text: 'text 33', value: 'value 33'},
                {text: 'text 34', value: 'value 34'}
            ]);
            expect(component.getPicker().getViewItems().length).toBe(7);

            doTyping('text 1', false);

            completeWithData([
                {text: 'text 10', value: 'value 10'},
                {text: 'text 11', value: 'value 11'},
                {text: 'text 12', value: 'value 12'}
            ]);
            expect(component.getPicker().getViewItems().length).toBe(3);

            clickListItem('value 10');

            expect(component.getPicker().isVisible()).toBe(false);
            expect(component.getValue()).toBe('value 10');
        });
    });

    xdescribe("getRecordDisplayData", function() {
        it("should call getRecordDisplayData to display the data", function() {
            makeComponent({
                renderTo: Ext.getBody(),
                displayField: 'text',
                valueField: 'value',
                getRecordDisplayData: function(record) {
                    var data = Ext.apply({}, record.data);
                    data.text += 'foo';
                    return data;
                }
            });
            component.setValue('value 2');
            expect(getRawValue()).toBe('text 2foo');
            expect(store.getAt(1).get('text')).toBe('text 2');
        });
    });

    describe('readOnly', function () {
        describe('should not react to mutation events', function () {
            function runTest(expectation, method, cfg) {
                it(expectation, function () {
                    makeComponent(Ext.apply({
                        readOnly: true,
                        renderTo: Ext.getBody()
                    }, cfg));

                    spyOn(component, method);

                    // Trigger a cross-browser field mutation event.
                    jasmine.fireKeyEvent(component.inputElement.dom, 'keyup', 65);

                    // The trick here is that we need to ensure that the method isn't called for readOnly components.
                    // Since it's called on a delayed task, we'll need to use waits() here, unfortunately.
                    waits(10);

                    runs(function () {
                        expect(component[method].callCount).toBe(0);
                    });
                });
            }

            runTest('should not query', 'doFilter', {
                queryDelay: 0,
                queryMode: 'local',
                value: 'Permanent Waves'
            });

            runTest('should not expand the picker', 'expand', {
                floatedPicker: {
                    emptyText: 'Exit... Stage Left',
                    type: 'floated'
                },
                queryDelay: 0,
                queryMode: 'local',
                value: 'Moving Pictures'
            });
        });
    });

    describe('checkValueOnChange triggered before store is loaded', function() {
        // EXTJS-16468
        var Color = Ext.define(null, {
            extend: 'Ext.data.Model',
            fields: ['name']
        });

        var panel;

        afterEach(function () {
            panel = Ext.destroy(panel);
        });

        function create (config) {
            var combo = Ext.merge({
                xtype: 'combobox',
                value: 'Red',

                fieldLabel: 'Chosen color',
                queryMode: 'local',
                forceSelection: true,
                displayField: 'name',
                valueField: 'name',
                store: {
                    autoLoad: false,
                    model: Color,
                    proxy: {
                        type: 'memory',
                        data: [{
                            id: '0xff0000',
                            name: 'Red'
                        }, {
                            id: '0x00ff00',
                            name: 'Green'
                        }, {
                            id: '0x0000ff',
                            name: 'Blue'
                        }]
                    }
                }
            }, config);

            panel = new Ext.panel.Panel({
                title: 'Combo test',
                renderTo: document.body,
                frame: true,
                height: 400,
                width: 600,
                items: [combo]
            });
        }

        it('should retain value until store load then clear if not a match', function() {
            create();

            var comboBox = panel.child('combobox'),
                store = comboBox.getStore();

            // The configured value cannot be propagated into the selection because there's still
            // not a loaded store to match it against
            expect(comboBox.getValue()).toBe('Red');

            var rec = comboBox.getSelection();
            expect(rec).toBe(null);

            store.addFilter({
                property: 'name',
                value: 'Blue'
            });
            // The configured value cannot be propagated into the selection because there's still
            // not a loaded store to match it against
            expect(comboBox.getValue()).toBe('Red');

            store.load();

            // After the load, we are able to ascertain that the configured value is not in the store
            // (It's filtered out), so the value should be null.
            expect(comboBox.getValue()).toBe(null);

            rec = comboBox.getSelection();
            expect(rec).toBe(null);
        });

        //TODO reconcile w/local queryMode fixes
        xit('should retain value until store load then keep if match is locally filtered', function() {
            create();

            component = panel.child('combobox');

            var store = component.getStore();

            // The configured value cannot be propagated into the selection because there's
            // still not a loaded store to match it against
            expect(component.getValue()).toBe('Red');

            var rec = component.getSelection();
            expect(rec).toBe(null);

            doTyping('Blue');

            // The configured value cannot be propagated into the selection because there's
            // still not a loaded store to match it against
            expect(component.getValue()).toBe('Red');

            component.doFilterTask.flush();

            // Now that the filter is applied, we should find the matching record in
            // the unfiltered source.
            expect(component.getValue()).toBe('Red');

            rec = component.getSelection();
            expect(rec.id).toBe('0xff0000');
        });
    });

    describe("complex binding", function() {
        var panel;
        afterEach(function() {
            if (panel) {
                panel.destroy();
                panel = null;
            }
        });

        var makePanel = function(autoLoad, differentFields, hasValue) {
            panel = new Ext.panel.Panel({
                renderTo: document.body,
                height: 400,
                width: 600,
                viewModel: {
                    stores: {
                        foo: {
                            autoLoad: autoLoad || false,
                            type: 'foo'
                        }
                    },
                    // ViewModel "bar" property is the selected foo *record*
                    data: {
                        bar: null
                    }
                },
                items: [{
                    xtype: 'combobox',
                    autoLoadOnValue: true,
                    queryMode: 'local',
                    forceSelection: true,
                    bind: {
                        store: '{foo}',
                        selection: '{bar}'
                    },
                    displayField: 'text',
                    valueField: differentFields ? 'value' : 'text',
                    value: hasValue ? (differentFields ? 'value 1' : 'text 1') : undefined
                }, {
                    itemId: 'target-comp',
                    xtype: 'component',
                    bind: {
                        record: '{bar}'
                    },
                    tpl: '{text}'
                }]
            });
        };

        it("should publish a selection when store provided by a bind is NOT autoloaded, value is configured and displayField === valueField", function() {
            makePanel(false, false, true);

            var targetComp = panel.child('#target-comp'),
                vm = panel.getViewModel(),
                store = vm.get('foo');

            vm.notify();
            
            expect(targetComp.getInnerHtmlElement().dom.innerHTML).toBe('text 1');
            expect(vm.get('bar')).toBe(store.byText.get('text 1'));
        });

        it("should publish a selection when store provided by a bind is autoloaded, value is configured and displayField === valueField", function() {
            makePanel(true, false, true);

            var targetComp = panel.child('#target-comp'),
                vm = panel.getViewModel(),
                store = vm.get('foo');

            vm.notify();
            
            expect(targetComp.getInnerHtmlElement().dom.innerHTML).toBe('text 1');
            expect(vm.get('bar')).toBe(store.byText.get('text 1'));
        });

        it("should publish a selection when store provided by a bind is NOT autoloaded, value is set post construction and displayField === valueField", function() {
            makePanel(false, false, false);

            var comboBox = panel.child('combobox'),
                targetComp = panel.child('#target-comp'),
                vm = panel.getViewModel(),
                store = vm.get('foo');

            comboBox.setValue('text 1');

            vm.notify();
            
            expect(targetComp.getInnerHtmlElement().dom.innerHTML).toBe('text 1');
            expect(vm.get('bar')).toBe(store.byText.get('text 1'));
        });

        it("should publish a selection when store provided by a bind is autoloaded, value is set post construction and displayField === valueField", function() {
            makePanel(true, false, false);

            var comboBox = panel.child('combobox'),
                targetComp = panel.child('#target-comp'),
                vm = panel.getViewModel(),
                store = vm.get('foo');

            comboBox.setValue('text 1');

            vm.notify();
            
            expect(targetComp.getInnerHtmlElement().dom.innerHTML).toBe('text 1');
            expect(vm.get('bar')).toBe(store.byText.get('text 1'));
        });

        it("should publish a selection when store provided by a bind is NOT autoloaded, value is configured and displayField !== valueField", function() {
            makePanel(false, true, true);

            var targetComp = panel.child('#target-comp'),
                vm = panel.getViewModel(),
                store = vm.get('foo');

            vm.notify();
            
            expect(targetComp.getInnerHtmlElement().dom.innerHTML).toBe('text 1');
            expect(vm.get('bar')).toBe(store.byText.get('text 1'));
        });

        it("should publish a selection when store provided by a bind is autoloaded, value is configured and displayField !== valueField", function() {
            makePanel(true, true, true);

            var targetComp = panel.child('#target-comp'),
                vm = panel.getViewModel(),
                store = vm.get('foo');

            vm.notify();

            expect(targetComp.getInnerHtmlElement().dom.innerHTML).toBe('text 1');
            expect(vm.get('bar')).toBe(store.byText.get('text 1'));
        });

        it("should publish a selection when store provided by a bind is NOT autoloaded, value is set post construction and displayField !== valueField", function() {
            makePanel(false, true, false);

            var comboBox = panel.child('combobox'),
                targetComp = panel.child('#target-comp'),
                vm = panel.getViewModel(),
                store = vm.get('foo');

            comboBox.setValue('value 1');
            vm.notify();

            expect(targetComp.getInnerHtmlElement().dom.innerHTML).toBe('text 1');
            expect(vm.get('bar')).toBe(store.byText.get('text 1'));
        });

        it("should publish a selection when store provided by a bind is autoloaded, value is set post construction and displayField !== valueField", function() {
            makePanel(true, true, false);

            var comboBox = panel.child('combobox'),
                targetComp = panel.child('#target-comp'),
                vm = panel.getViewModel(),
                store = vm.get('foo');

            comboBox.setValue('value 1');

            vm.notify();

            expect(targetComp.getInnerHtmlElement().dom.innerHTML).toBe('text 1');
            expect(vm.get('bar')).toBe(store.byText.get('text 1'));
        });

        it("should not collapse while typing and it matches a record", function() {
            var spy = jasmine.createSpy(),
                vm;

            makeComponent({
                queryMode: 'local',
                autoLoadOnValue: true,
                viewModel : {
                    data: {
                        name: null
                    }
                },
                bind: {
                    value : '{name}'
                },
                displayField: 'text',
                valueField: 'text',
                renderTo: Ext.getBody()
            });

            vm = component.getViewModel();
            component.on('collapse', spy);

            doTyping('Foo', false, true);

            expect(spy).not.toHaveBeenCalled();
            component.getPicker().refresh();

            // Should have filtered down to 2
            expect(component.getPicker().getViewItems().length).toBe(2);

/*TODO: enable when keyboard navigation is enabled for DataViews
            // Pick Foo.
            jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.DOWN);
            jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.TAB);

            vm.notify();
            expect(vm.get('name')).toBe('Foo');
*/
        });

        xit("should not clear the combobox while typing and forceSelection is true", function() {
            var vm;
            makeComponent({
                queryMode: 'local',
                autoLoadOnValue: true,
                forceSelection : true,
                viewModel : {
                    data: {
                        address: 1
                    }
                },
                bind: {
                    value : '{address}'
                },
                displayField: 'text',
                valueField: 'id',
                renderTo: Ext.getBody()
            });

            vm = component.getViewModel();
            
            component.setValue(9);
            vm.notify();

            jasmine.focusAndWait(component);

            runs(function(){
                doTyping('t', false, true);
                vm.notify();

                expect(vm.get('address')).toBe(9);
                expect(component.inputElement.dom.value).toBe('t');
            });
        });

        xit("should be able to set an Array value using binding while the field is focused", function() {
            var vm;
            makeComponent({
                queryMode: 'local',
                multiSelect: true,
                viewModel : {
                    data: {
                        foo: [1,2]
                    }
                },
                bind: {
                    value : '{foo}'
                },
                displayField: 'text',
                valueField: 'id',
                renderTo: Ext.getBody()
            });

            vm = component.getViewModel();
            vm.notify();

            jasmine.focusAndWait(component);

            runs(function(){
                vm.set('foo', [1]);
                vm.notify();
                expect(component.getValue()).toEqual([1]);
            });
        });
    });

    xdescribe("forceSelection: true", function() {
        beforeEach(function() {
            makeComponent({
                renderTo        : document.body,
                valueField      : 'value',
                displayField	: 'text',
                queryMode       : 'local',
                required        : true,
                forceSelection  : true,
                queryCaching    : false
            });
        });

        it("should not select the lastSelectedValue after a new value has been set", function() {
            jasmine.focusAndWait(component);

            runs(function() {
                // Should narrow down to 3, 31, 32 etc
                doTyping('text 3', false, true);
            });

            // Wait for the query timer to show the narrowed list
            waitsFor(function() {
                return component.getPicker() && component.getPicker().isVisible() === true;
            }, 'picker to show');
            
            runs(function() {
                // Down to the '31' value
                jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.DOWN);

                // Select 31 and blur
                jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.TAB);
            });
            jasmine.blurAndWait(component);

            runs(function() {
                expect(component.getValue()).toBe('value 31');
                component.setValue('');
                component.inputElement.dom.focus();
            });
            
            jasmine.focusAndWait(component);
            
            runs(function() {
                // Do not select a record
                jasmine.fireKeyEvent(component.inputElement, 'keydown', Ext.event.Event.TAB);
            });

            jasmine.blurAndWait(component);

            // The assertValue call on blur should NOT have imposed the last selected value
            // from the last time the field was used. The intervening setValue call clears it.
            runs(function() {
                expect(component.getValue()).toBeNull();
            });
            
        });
    });

    describe("destroy", function() {
        it("should not throw an exception when destroying on select", function() {
            makeComponent({
                renderTo: Ext.getBody(),
                valueField: 'value'
            });
            component.on('select', function() {
                this.destroy();
            }, component);
            expect(function() {
                clickListItem('value 2');
            }).not.toThrow();
        });
    });

    // This test is for mouse clicks
    if (!jasmine.supportsTouch) {
        describe("editable", function () {
            it("should expand on inputEl click when NOT editable", function () {
                makeComponent({
                    renderTo: document.body,
                    editable: false
                });

                // Not editable to begin with, should expand on inputEl Click
                jasmine.fireMouseEvent(component.inputElement, 'click');
                expect(component.expanded).toBe(true);
                component.collapse();

                component.setEditable(true);
                // Now it is editable, should NOT expand on inputEl Click
                jasmine.fireMouseEvent(component.inputElement, 'click');
                expect(component.expanded).toBe(false);

                component.setEditable(false);
                // Not edtable again, SHOULD expand
                jasmine.fireMouseEvent(component.inputElement, 'click');
                expect(component.expanded).toBe(true);
            });
        });
    }

    describe("typeahead", function() {
        it("should not extend the raw value with typeahead on erase", function() {
            var indices;

            store.load();
            makeComponent({
                displayField: 'text',
                valueField: 'value',
                typeAhead: true,
                minChars: 2,
                queryMode: 'local',
                renderTo: Ext.getBody()
            });
            doTyping('tex');

            // The typeahead setting will extend the raw value with a text selection
            waitsFor(function() {
                return getRawValue() === 'text 1';
            });
            
            runs(function() {
                // get initial selection indicies
                indices = getTextSelectionIndices(component.inputElement.dom);
                // The typeahead "t 1" should be selected - 3 to 6 *exclusive*
                expect(indices[0]).toBe(3);
                expect(indices[1]).toBe(6);

                // Erasing the selected typeahead chars "t 1", should not typeahead
                doTyping('tex', true);
            });
            
            // We are testing that nothing happens here, so we just have to wait.
            waits(100);
            
            // After the erasure, it must not have done typeahead.
            runs(function() {
                // Ensure no typeahead has been done
                expect(component.inputElement.dom.value.length).toBe(3);

                // Erasing the "x" chars "xt 1", should not typeahead
                doTyping('te', true);
            });

            // We are testing that nothing happens here, so we just have to wait.
            waits(100);

            // After the erasure, it must not have done typeahead.
            runs(function() {
                // Ensure no typeahead has been done
                expect(component.inputElement.dom.value.length).toBe(2);
            });
        });

        it("should clear any selected text after selection is made", function () {
            var indices;

            store.load();
            makeComponent({
                displayField: 'text',
                valueField: 'value',
                typeAhead: true,
                minChars: 2,
                queryMode: 'local',
                renderTo: Ext.getBody()
            });

            jasmine.focusAndWait(component);

            runs(function() {
                doTyping('tex');
            });

            // The typeahead setting will extend the raw value with a text selection
            waitsFor(function() {
                return getRawValue() === 'text 1';
            });
            
            runs(function() {
                // get initial selection indicies
                indices = getTextSelectionIndices(component.inputElement.dom);
                // The typeahead "text 1" should be selected - 3 to 6 *exclusive*
                expect(indices[0]).toBe(3);
                expect(indices[1]).toBe(6);
                // type ahead is complete; select the correct record matched by the typeAhead
                component.getPicker().select([store.findRecord('text', 'text 1')]);
                // get indicies again
                indices = getTextSelectionIndices(component.inputElement.dom);
                // selection is done; check raw value
                expect(getRawValue()).toBe('text 1');
                // previous text selection should be cleared and cursor placed at the end of the raw value
                expect(indices[0]).toBe(6);
                expect(indices[1]).toBe(6);
            });
        });

        it('should reopen picker after a previous typeahead query', function () {
            makeComponent({
                displayField: 'text',
                valueField: 'value',
                typeAhead: true,
                minChars: 2,
                queryMode: 'local',
                renderTo: Ext.getBody()
            });

            jasmine.focusAndWait(component);

            runs(function() {
                doTyping('tex');
            });

            waitsFor(function () {
                return getRawValue() === 'text 1';
            });

            jasmine.blurAndWait(component);

            runs(function () {
                component.onExpandTap();
            });

            waitsFor(function () {
                return component.getPicker().isVisible();
            });
        });

        var testTypeAheadOnBlur = function(enableForceSelection) {
            it("should select the appropriate record on blur with forceSelection: " + enableForceSelection, function() {
                var spy = jasmine.createSpy();
                store.load();
                makeComponent({
                    displayField: 'text',
                    valueField: 'value',
                    typeAhead: true,
                    minChars: 2,
                    queryMode: 'local',
                    forceSelection: enableForceSelection,
                    renderTo: Ext.getBody()
                });

                component.on('select', spy);

                jasmine.focusAndWait(component);

                runs(function() {
                    doTyping('tex');
                });

                // The typeahead setting will extend the raw value with a text selection
                waitsFor(function() {
                    return getRawValue() === 'text 1';

                    // An "isEntered" record will be selected
                    if (!enableForceSelection) {
                        expect(spy.callCount).toBe(1);
                    }
                });

                jasmine.blurAndWait(component);

                runs(function() {
                    expect(getRawValue()).toBe('text 1');
                    expect(component.getValue()).toBe('value 1');

                    // If forceSelection, the initial typing of "tex" will not have selected
                    // a temporary, "isEntered" record so selection count should be 1,
                    // If !forceSelection, there will be two select events.
                    expect(spy.callCount).toBe(enableForceSelection ? 1 : 2);
                });
            });

            it("should not fire events if we did not change values (no value) with forceSelection: " + enableForceSelection, function() {
                var spy = jasmine.createSpy();
                store.load();
                makeComponent({
                    displayField: 'text',
                    valueField: 'value',
                    typeAhead: true,
                    minChars: 2,
                    queryMode: 'local',
                    forceSelection: enableForceSelection,
                    renderTo: Ext.getBody()
                });

                component.on('select', spy);

                jasmine.focusAndWait(component);

                runs(function() {
                    doTyping('tex');
                });

                // The typeahead setting will extend the raw value with a text selection
                waitsFor(function() {
                    return getRawValue() === 'text 1';
                });

                runs(function() {
                    doTyping('', true);
                });

                jasmine.blurAndWait(component);

                runs(function() {
                    if (enableForceSelection) {
                        // selectfield casts '' into null for forceSelection: true
                        expect(component.getValue()).toBeNull();
                    } else {
                        // selectfield casts null into '' for forceSelection: false
                        expect(component.getValue()).toBe('');
                    }

                    // If !enableForceSelection, the typing of "text" will have selected a
                    // temporary "isEntered" record.
                    expect(spy.callCount).toBe(enableForceSelection ? 0 : 1);
                });
            });

            it("should not fire events if we did not change values (with value) with forceSelection: " + enableForceSelection, function() {
                var spy = jasmine.createSpy();
                store.load();
                makeComponent({
                    displayField: 'text',
                    valueField: 'value',
                    typeAhead: true,
                    minChars: 2,
                    queryMode: 'local',
                    value: 'value 1',
                    forceSelection: enableForceSelection,
                    renderTo: Ext.getBody()
                });

                component.on('select', spy);
                jasmine.focusAndWait(component);

                jasmine.blurAndWait(component);

                runs(function() {
                    expect(component.getValue()).toBe('value 1');
                    expect(spy.callCount).toBe(0);
                });
            });
        };

        testTypeAheadOnBlur(true);
        testTypeAheadOnBlur(false);
    });

    describe("collapse on scroll", function() {
        it("should collapse when the field scrolls out of view", function() {
            component = new Ext.panel.Panel({
                renderTo: document.body,
                title: 'Framed panel with normal child',
                width: 300,
                height: 300,
                html: null,
                layout: 'fit',
                items: [{
                    xtype: 'panel',
                    itemId: 'formPanel',
                    scrollable: true,
                    title: 'Non-framed child',
                    items: [{
                        xtype: 'textfield'
                    }, {
                        xtype: 'combobox',
                        itemId: 'combo1',
                        typeAhead: true,
                        triggerAction: 'all',
                        editable: true,
                        selectOnTab: true,

                        options: [
                            ['AA', 'AA'],
                            ['B Shady', 'B Shady'],
                            ['C or Shade', 'C or Shade'],
                            ['D Sunny', 'D Sunny'],
                            ['E', 'E']
                        ],
                        lazyRender: true
                    }, {
                        xtype: 'textfield'
                    }, {
                        xtype: 'textfield'
                    }, {
                        xtype: 'textfield'
                    }, {
                        xtype: 'textfield'
                    }, {
                        xtype: 'textfield'
                    }, {
                        xtype: 'textfield'
                    }, {
                        xtype: 'textfield'
                    }, {
                        xtype: 'combobox',
                        typeAhead: true,
                        triggerAction: 'all',
                        editable: true,
                        selectOnTab: true,
                        options: [
                            ['AA', 'AA'],
                            ['B Shady', 'B Shady'],
                            ['C or Shade', 'C or Shade'],
                            ['D Sunny', 'D Sunny'],
                            ['E', 'E']
                        ],
                        lazyRender: true
                    }]
                }]
            });
            var combo1 = component.down('#combo1'),
                formPanel = component.down('#formPanel'),
                scroller = formPanel.getScrollable(),
                hideSpy = spyOnEvent(combo1.getPicker(), 'hide');

            combo1.expand();
            scroller.scrollBy(0, 10);

            // Any scroll should cause the picker should be hidden
            waitsForSpy(hideSpy);
        });
    });

    describe("misc tests", function() {
        beforeEach(function() {
            makeComponent({
                displayField: 'text',
                valueField: 'id',
                queryMode: 'local',
                renderTo: Ext.getBody()
            });
        });
 
        it("should set the value correctly when the display value contains unix line feeds", function() {
            var rec = store.first();
 
            rec.set('text', 'foo\nbar');
            clickListItem(1);
            expect(component.getSelection()).toBe(rec);
            expect(component.getValue()).toBe(1);
        });
 
        it("should set the value correctly when the display value contains windows line feeds", function() {
            var rec = store.first();
 
            rec.set('text', 'foo\r\nbar');
            clickListItem(1);
            expect(component.getSelection()).toBe(rec);
            expect(component.getValue()).toBe(1);
        });
    });

    // https://sencha.jira.com/browse/EXTJS-20322
    if ((Ext.supports.PointerEvents || Ext.supports.MSPointerEvents) && Ext.getScrollbarSize().width) {
        describe('Tapping on an item', function() {
            beforeEach(function() {
                makeComponent({
                    renderTo: document.body,
                    floatedPicker: {
                        maxHeight: 50,
                        type: 'floated'
                    }
                });
            });

            it("should select when tapping on an item", function() {
                var triggerEl = component.getTriggers().expand.el,
                    boundList,
                    item,
                    x = triggerEl.getX() + triggerEl.getWidth() / 2,
                    y = triggerEl.getY() + triggerEl.getHeight() / 2;

                Ext.testHelper.tap(triggerEl, { x: x, y: y });

                boundList = component.getPicker();
                expect(boundList.isVisible()).toBe(true);
                component.collapse();
                expect(boundList.isVisible()).toBe(false);

                Ext.testHelper.tap(triggerEl, { x: x, y: y });
                expect(boundList.isVisible()).toBe(true);
                item = boundList.getViewItems()[0];

                expect(boundList.getSelections().length).toBe(0);

                x = item.el.getX() + item.el.getWidth() / 2;
                y = item.el.getY() + item.el.getHeight() / 2;
                Ext.testHelper.tap(item.el, { x: x, y: y });

                expect(boundList.getSelections().length).toBe(1);
            });
        });
    }

    describe('ComboBox in a Dialog', function() {
        var testWin, combo;

        beforeEach(function() {
            testWin = Ext.create('Ext.Dialog', {
                title: 'combo popup still shown after resize or move',
                width: 350,
                height: 200,
                hideAnimation: null,
                showAnimation: null,
                items: {
                    xtype: 'combobox',
                    padding: '10 10 10 10',
                    height: 15,
                    width: 200,
                    fieldLabel: 'Test Combo',
                    options: [1, 2, 3],
                    queryMode: 'local',
                    displayField: 'name',
                    valueField: 'abbr'
                }
            });
            testWin.showAt(10, 10);
            combo = testWin.down('combobox');
        });
        afterEach(function() {
            testWin.destroy();
        });

        it('should hide popup when window dragged', function() {
            var offset = 5;

            combo.expand();
            expect(combo.getPicker().isVisible()).toBe(true);

            jasmine.fireMouseEvent(testWin.getHeader().el, 'mouseover', offset, offset);
            testWin.el.dom.focus();
            jasmine.fireMouseEvent(testWin.getHeader().el, 'mousedown', offset, offset);
            jasmine.fireMouseEvent(testWin.getHeader().el, 'mousemove', 100, 0);

            waitsFor(function() {
                return combo.expanded === false && !combo.getPicker().pendingShow;
            });

            // Mouseup should not reshow
            runs(function() {
                jasmine.fireMouseEvent(testWin.getHeader().el, 'mouseup');
                expect(combo.getPicker().isVisible()).toBe(false);
            });
        });
    });
    describe("with queryMode:remote", function() {
        beforeEach(function() {
            MockAjaxManager.addMethods();
        });

        afterEach(function() {
            MockAjaxManager.removeMethods();
        });

        // https://sencha.jira.com/browse/EXTJS-25670
        it("should not collapse picker when it is expanded after initial selection", function() {

            // Test data must have IDs.
            // The picker collapses in singleSelect mode if there is a *new* value
            // selected. And tghat is determined by whether the id of the incoming
            // selected record matches the id of the oldSelected record.
            var testData = [
                { id: 1, value: 1, text: 'foo' },
                { id: 2, value: 2, text: 'bar' },
                { id: 3, value: 3, text: 'baz' },
                { id: 4, value: 4, text: 'qux' }
            ];

            var expandSpy = jasmine.createSpy('expand'),
                collapseSpy = jasmine.createSpy('collapse'),
                loadSpy = jasmine.createSpy('load');

            store.destroy();

            store = new Ext.data.Store({
                asynchronousLoad: true,
                autoLoad: true,
                model: CBTestModel,
                proxy: {
                    type: 'ajax',
                    url: 'fake'
                },
                listeners: {
                    load: loadSpy
                }
            });

            makeComponent({
                store: store,
                displayField: 'text',
                valueField: 'value',
                label: 'Foo',
                queryMode: 'remote',
                renderTo: Ext.getBody(),
                listeners: {
                    expand: expandSpy,
                    collapse: collapseSpy
                }
            });

            focusAndWait(component);

            runs(function() {
                // Can't just call component.expand() here!
                // ComboBox does filtering in onExpandTap()
                clickExpandTrigger();
            });

            waitForSpy(expandSpy);

            runs(function() {
                Ext.Ajax.mockComplete({
                    status: 200,
                    responseText: JSON.stringify(testData)
                });
            });

            waitForSpy(loadSpy);

            runs(function() {
                clickListItem('2');
            });

            waitForSpy(collapseSpy);

            runs(function() {
                expandSpy.reset();
                collapseSpy.reset();
                loadSpy.reset();

                clickExpandTrigger();
            });

            waitForSpy(expandSpy);

            runs(function() {
                Ext.Ajax.mockComplete({
                    status: 200,
                    responseText: JSON.stringify(testData)
                });
            });

            waitForSpy(loadSpy);

            // Can't wait for the spy *not* to fire
            waits(100);

            runs(function() {
                expect(collapseSpy).not.toHaveBeenCalled();
                expect(component.expanded).toBe(true);
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
                    xtype: 'combobox',
                    label: 'States',
                    reference: 'statesField',
                    valueField: 'abbr',
                    displayField: 'state',
                    forceSelection: true,
                    minChars: 2,
                    bind: {
                        store: '{states}',
                        placeholder: '{countryField.value === "USA" ? "Chose a state" : countryField.value === "Canada" ? "Chose a province" : ""}'
                    },
                    value: 'AL'
                }]
            });
            vm = panel.getViewModel();
            countries = panel.child('[reference=countryField]');
            component = states = panel.child('[reference=statesField]');

            // Flush ViewModel data
            vm.getScheduler().onTick();

        });
        afterEach(function() {
            Ext.destroy(panel);
            Ext.undefine('Ext.test.ChainedSelectTestCountries');
            Ext.undefine('Ext.test.ChainedSelectTestStates');
        });

        it('should clear the dependent field when its selected record is filtered out', function() {
            // Filter down to only Delaware. This will work because minChars is 2
            doTyping('De');

            // Wait for the filtered data to return.
            waitsFor(function() {
                return component.getPicker().isVisible() && component._pickerStore.getCount() === 1;
            });

            // When the picker is visible, pick the first record
            runs(function() {
                states.getPicker().getSelectable().select(0);
                expect(states.getValue()).toBe('DE');

                // This will refresh the picker's selmodel and evict the state.
                countries.setValue('Canada');
            });

            // When binding ticks, the states should be cleared because its
            // selected record is no longer in its store.
            waitsFor(function() {
                return states.getValue() == null;
            });
        });
    });

});
