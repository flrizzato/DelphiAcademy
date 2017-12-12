topSuite("Ext.app.bind.Binding", ['Ext.app.ViewModel', 'Ext.Panel', 'Ext.field.Text'], function() {
    var component, viewModel;

    afterEach(function() {
        Ext.destroy(component);
    });

    // Enforce EXTJS-25304. It was only manifesting in Classic, but the test is valid.
    // The null initial value was published upon spin up of the binding
    // which then contaminated the supposedly incoming value from the VM.
    // resulting in the field being empty.
    describe('TextField', function() {
        it("should not push a non-confgured null initial value out to a two way bind", function () {
            component = Ext.create('Ext.Panel', {
                renderTo: document.body,
                viewModel: {
                    data: {
                        testValue: 'test value'
                    }
                },
                title: 'Test textfield bind',
                items: [{
                    id: 'test-textfield',
                    xtype: 'textfield',
                    fieldLabel: 'Textfield',
                    publishes: 'value',
                    bind: '{testValue}'
                }]
            });

            viewModel = component.getViewModel();
            viewModel.notify();

            var textfield = Ext.getCmp('test-textfield');
            expect(textfield.getValue()).toBe('test value');
        });
    });

    describe('TextField subclass', function() {
        var TextSubclass = Ext.define(null, {
            extend: 'Ext.field.Text',

            value: {
                $value: undefined,
                lazy: false
            }
        });
        it("should not push a non-confgured null initial value out to a two way bind", function () {
            component = Ext.create('Ext.Panel', {
                renderTo: document.body,
                viewModel: {
                    data: {
                        testValue: 'test value'
                    }
                },
                title: 'Test textfield bind',
                items: [new TextSubclass({
                    id: 'test-textfield',
                    xtype: 'textsubclass',
                    fieldLabel: 'Textfield',
                    publishes: 'value',
                    bind: '{testValue}'
                })]
            });

            viewModel = component.getViewModel();
            viewModel.notify();

            var textfield = Ext.getCmp('test-textfield');
            expect(textfield.getValue()).toBe('test value');
        });
    });
});