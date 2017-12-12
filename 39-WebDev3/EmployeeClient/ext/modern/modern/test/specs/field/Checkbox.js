topSuite("Ext.field.Checkbox",
    ['Ext.form.Panel', 'Ext.form.FieldSet', 'Ext.field.*', 'Ext.app.ViewModel',
     'Ext.layout.VBox'],
function() {
    var field, container;

    function makeField(cfg) {
        field = new Ext.field.Checkbox(cfg);
        if (field.getFloated()) {
            field.show();
        } else {
            field.render(Ext.getBody());
        }
    }

    function makeForm() {
        container = new Ext.Container({
            items: [{
                xtype: 'formpanel',
                itemId: 'form1',
                items: [{
                    xtype: 'fieldset',
                    title: 'Action Item',
                    items: [{
                        xtype: 'radiofield',
                        itemId: 'status1',
                        name: 'status',
                        value: 'Open'
                    }, {
                        xtype: 'radiofield',
                        itemId: 'status2',
                        name: 'status',
                        value: 'Closed'
                    }]
                },{
                    xtype: 'fieldset',
                    title: 'Alt Item',
                    items: [{
                        xtype: 'radiofield',
                        itemId: 'status3',
                        name: 'status',
                        value: 'In Progress'
                    }, {
                        xtype: 'radiofield',
                        itemId: 'status4',
                        name: 'status',
                        value: 'Pending'
                    }]
                }]
            },
            {
                xtype: 'formpanel',
                itemId: 'form2',
                items: [{
                    xtype: 'fieldset',
                    items: [{
                        xtype: 'textfield',
                        itemId: 'status5',
                        name: 'status'
                    }]
                }]
            }]
        });
        container.render(Ext.getBody());
    } 

    function makeFieldset() {
        container = new Ext.Container({
            items: [{
                xtype: 'fieldset',
                itemId: 'fieldset1',
                items: [{
                    xtype: 'radiofield',
                    itemId: 'status1',
                    name: 'status',
                    value: 'Open'
                }, {
                    xtype: 'radiofield',
                    itemId: 'status2',
                    name: 'status',
                    value: 'Closed'
                }]
            },
            {
                xtype: 'fieldset',
                itemId: 'fieldset2',
                items: [{
                    xtype: 'textfield',
                    itemId: 'status5',
                    name: 'status'
                }]
            }]
        });
        container.render(Ext.getBody());
    }

    afterEach(function() {
        field = Ext.destroy(field);
        container = Ext.destroy(container);
    });

    describe("binding", function() {
        describe("publish with reference", function() {
            it("should publish the checked state if checked: false", function() {
                var vm;
                makeField({
                    reference: 'fooField',
                    viewModel: {}
                });
                vm = field.getViewModel();
                vm.notify();
                expect(vm.get('fooField.checked')).toBe(false);

            });

            it("should publish the checked state if checked: true", function() {
                var vm;
                makeField({
                    reference: 'fooField',
                    checked: true,
                    viewModel: {}
                });
                vm = field.getViewModel();
                vm.notify();
                expect(vm.get('fooField.checked')).toBe(true);
            });
        });
    });

    describe("getSameGroupFields", function() {
        describe("retrieving descendant components", function() {
            it("should return fields with the same name within the parent form", function() {
                var form, radio, children;
                makeForm();

                form = container.down('#form1');
                radio = container.down('#status1');

                children = radio.getSameGroupFields();

                expect(children.length).toBe(4);
            });

            it("should return only the checkboxes/radio fields", function() {
                var types=[],
                    form, radio, children;

                makeForm();

                form = container.down('#form1');
                radio = container.down('#status1');

                children = radio.getSameGroupFields();
                children.forEach(function(child) {
                    types.push(child.xtype); 
                });

                expect(types).toEqual(['radiofield', 'radiofield', 'radiofield', 'radiofield']);
            });
        });
    });

    describe('boxLabel', function() {
        var boxLabel = '<div style="width:50px;background:green;">&nbsp;</div>';

        it('should layout with boxLabelAlign: after', function() {
            makeField({
                inline: true,
                boxLabel: boxLabel
            });

            expect(field).toHaveLayout({
                element: { xywh: '0 0 70 24' },
                bodyWrapElement: { xywh: '0 0 70 24' },
                bodyElement: { xywh: '0 0 70 24' },
                labelElement: { d: false },
                inputElement: { xywh: '0 4 16 16' },
                boxWrapElement: { xywh: '0 0 70 24' },
                iconElement: { xywh: '0 4 16 16' },
                boxLabelElement: { xywh: '16 0 54 24' }
            });
        });

        it('should layout with boxLabelAlign: before', function() {
            makeField({
                inline: true,
                boxLabel: boxLabel,
                boxLabelAlign: 'before'
            });

            expect(field).toHaveLayout({
                element: { xywh: '0 0 70 24' },
                bodyWrapElement: { xywh: '0 0 70 24' },
                bodyElement: { xywh: '0 0 70 24' },
                labelElement: { d: false },
                inputElement: { xywh: '54 4 16 16' },
                boxWrapElement: { xywh: '0 0 70 24' },
                iconElement: { xywh: '54 4 16 16' },
                boxLabelElement: { xywh: '0 0 54 24' }
            });
        });

        describe("labeled", function() {
            it('should layout with boxLabelAlign: after', function () {
                makeField({
                    inline: true,
                    label: 'Foo',
                    boxLabel: boxLabel
                });

                expect(field).toHaveLayout({
                    element: { xywh: '0 0 170 24' },
                    bodyWrapElement: { xywh: '100 0 70 24' },
                    bodyElement: { xywh: '100 0 70 24' },
                    labelElement: { xywh: '0 0 100 24' },
                    inputElement: { xywh: '100 4 16 16' },
                    boxWrapElement: { xywh: '100 0 70 24' },
                    iconElement: { xywh: '100 4 16 16' },
                    boxLabelElement: { xywh: '116 0 54 24' }
                });
            });

            it('should layout with boxLabelAlign: before', function () {
                makeField({
                    inline: true,
                    label: 'Foo',
                    boxLabel: boxLabel,
                    boxLabelAlign: 'before'
                });

                expect(field).toHaveLayout({
                    element: { xywh: '0 0 170 24' },
                    bodyWrapElement: { xywh: '100 0 70 24' },
                    bodyElement: { xywh: '100 0 70 24' },
                    labelElement: { xywh: '0 0 100 24' },
                    inputElement: { xywh: '154 4 16 16' },
                    boxWrapElement: { xywh: '100 0 70 24' },
                    iconElement: { xywh: '154 4 16 16' },
                    boxLabelElement: { xywh: '100 0 54 24' }
                });
            });
        });
    });
});
