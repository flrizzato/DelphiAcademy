topSuite(
    'Ext.field.Manager',
    [
        'Ext.Container',
        'Ext.data.Model',
        'Ext.field.Container',
        'Ext.field.Checkbox',
        'Ext.field.Radio',
        'Ext.field.Text',
        'Ext.layout.Form'
    ],
    function () {
        var ct;

        function createContainer (cfg) {
            if (Ext.isArray(cfg)) {
                cfg = {
                    items: cfg
                };
            }

            return ct = new MyContainer(cfg);
        }

        function sortById (A, B) {
            if (A.id < B.id) {
                return -1;
            }
            if (A.id > B.id) {
                return 1;
            }
            return 0;
        }

        beforeAll(function () {
            Ext.define('MyContainer', {
                extend: 'Ext.Container',
                xtype: 'mycontainer',

                mixins: [
                    'Ext.field.Manager'
                ],

                nameHolder: true,

                defaultType: 'textfield',

                updateRecord: function (record) {
                    this.consumeRecord(record);
                },

                updateDisabled: function (newDisabled, oldDisabled) {
                    this.mixins.fieldmanager.updateDisabled.call(this, newDisabled, oldDisabled);

                    this.callParent([newDisabled, oldDisabled]);
                }
            });

            Ext.define('MyModel', {
                extend: 'Ext.data.Model',

                fields: ['foo', 'bar', 'baz']
            });
        });

        afterAll(function () {
            Ext.undefine('MyContainer');
            Ext.undefine('MyModel');

            delete window.MyContainer;
            delete window.MyModel;
        });

        afterEach(function () {
            ct = Ext.destroy(ct);
        });

        it('should get mixed in', function () {
            createContainer();

            expect(ct.mixins.fieldmanager).toBeTruthy();
        });

        describe('fillRecord', function () {
            it('should fill passed record', function () {
                var record = new MyModel({
                    id: 'mymodel'
                });

                createContainer([
                    { name: 'foo', value: 'foo value' },
                    { name: 'bar', value: 'bar value' },
                    {
                        xtype: 'container',
                        defaultType: 'checkboxfield',
                        items: [
                            { name: 'baz', value: 'baz 1', checked: true },
                            { name: 'baz', value: 'baz 2', checked: true },
                            { name: 'baz', value: 'baz 3' },
                            { name: 'baz', value: 'baz 4', checked: true }
                        ]
                    }
                ]);

                expect(ct.fillRecord(record)).toBe(ct);

                expect(record.data).toEqual({
                    bar: 'bar value',
                    foo: 'foo value',
                    id: 'mymodel',
                    baz: [
                        'baz 1',
                        'baz 2',
                        'baz 4'
                    ]
                });
            });

            it('should fill passed record overwriting values', function () {
                var record = new MyModel({
                    id: 'mymodel',
                    foo: 'blah',
                    baz: 'nothing'
                });

                createContainer([
                    { name: 'foo', value: 'foo value' },
                    { name: 'bar', value: 'bar value' },
                    {
                        xtype: 'container',
                        defaultType: 'checkboxfield',
                        items: [
                            { name: 'baz', value: 'baz 1', checked: true },
                            { name: 'baz', value: 'baz 2', checked: true },
                            { name: 'baz', value: 'baz 3' },
                            { name: 'baz', value: 'baz 4', checked: true }
                        ]
                    }
                ]);

                expect(ct.fillRecord(record)).toBe(ct);

                expect(record.data).toEqual({
                    bar: 'bar value',
                    foo: 'foo value',
                    id: 'mymodel',
                    baz: [
                        'baz 1',
                        'baz 2',
                        'baz 4'
                    ]
                });
            });

            it('should not fill undefined record', function () {
                createContainer();

                var spy = spyOn(ct, 'getValues');

                expect(ct.fillRecord()).toBe(ct);

                expect(spy).not.toHaveBeenCalled();
            });
        });

        describe('updateRecord', function () {
            it('should set values from a record', function () {
                createContainer();

                var record = new MyModel({
                        bar: 'bar value',
                        baz: 'baz value',
                        foo: 'foo value',
                        id: 'mymodel'
                    }),
                    spy = spyOn(ct, 'setValues');

                ct.updateRecord(record);

                expect(spy).toHaveBeenCalled();

                expect(spy.mostRecentCall.args[0]).toEqual({
                    bar: 'bar value',
                    baz: 'baz value',
                    foo: 'foo value',
                    id: 'mymodel'
                });
            });

            it('should not set values from an undefined record', function () {
                createContainer();

                var spy = spyOn(ct, 'setValues');

                ct.updateRecord();

                expect(spy).not.toHaveBeenCalled();
            });
        });

        describe('setValues', function () {
            it('should set values to all fields', function () {
                createContainer([
                    { name: 'foo' },
                    { name: 'bar' },
                    {
                        xtype: 'container',
                        defaultType: 'checkboxfield',
                        items: [
                            { name: 'baz', value: 'baz 1' },
                            { name: 'baz', value: 'baz 2' },
                            { name: 'baz', value: 'baz 3' },
                            { name: 'baz', value: 'baz 4' }
                        ]
                    },
                    {
                        xtype: 'container',
                        defaultType: 'radiofield',
                        items: [
                            { name: 'bazier', value: 'bazier 1', checked: true },
                            { name: 'bazier', value: 'bazier 2' },
                            { name: 'bazier', value: 'bazier 3' },
                            { name: 'bazier', value: 'bazier 4' }
                        ]
                    }
                ]);

                var values = {
                    bar: 'bar value',
                    bazier: 'bazier 3',
                    foo: 'foo value',
                    baz: [
                        'baz 1',
                        'baz 3'
                    ]
                };

                expect(ct.setValues(values)).toBe(ct);

                expect(ct.getValues()).toEqual({
                    bar: 'bar value',
                    bazier: 'bazier 3',
                    foo: 'foo value',
                    baz: [
                        'baz 1',
                        'baz 3'
                    ]
                });
            });

            it('should not set values if none passed', function () {
                createContainer([
                    { name: 'foo' },
                    { name: 'bar' },
                    {
                        xtype: 'container',
                        defaultType: 'checkboxfield',
                        items: [
                            { name: 'baz', value: 'baz 1' },
                            { name: 'baz', value: 'baz 2' },
                            { name: 'baz', value: 'baz 3' },
                            { name: 'baz', value: 'baz 4' }
                        ]
                    },
                    {
                        xtype: 'container',
                        defaultType: 'radiofield',
                        items: [
                            { name: 'bazier', value: 'bazier 1', checked: true },
                            { name: 'bazier', value: 'bazier 2' },
                            { name: 'bazier', value: 'bazier 3' },
                            { name: 'bazier', value: 'bazier 4' }
                        ]
                    }
                ]);

                expect(ct.setValues()).toBe(ct);

                expect(ct.getValues()).toEqual({
                    bar: null,
                    bazier: 'bazier 1',
                    foo: null,
                    baz: null
                });
            });
        });

        describe('getValues', function () {
            it('should get values from all fields', function () {
                createContainer([
                    { name: 'foo', value: 'foo value' },
                    { name: 'bar', value: 'bar value' },
                    {
                        xtype: 'container',
                        defaultType: 'checkboxfield',
                        items: [
                            { name: 'baz', value: 'baz 1', checked: true },
                            { name: 'baz', value: 'baz 2', checked: true },
                            { name: 'baz', value: 'baz 3' },
                            { name: 'baz', value: 'baz 4', checked: true }
                        ]
                    },
                    {
                        xtype: 'container',
                        defaultType: 'radiofield',
                        items: [
                            { name: 'bazier', value: 'bazier 1', checked: true },
                            { name: 'bazier', value: 'bazier 2' },
                            { name: 'bazier', value: 'bazier 3' },
                            { name: 'bazier', value: 'bazier 4' }
                        ]
                    }
                ]);

                expect(ct.getValues()).toEqual({
                    bar: 'bar value',
                    bazier: 'bazier 1',
                    foo: 'foo value',
                    baz: [
                        'baz 1',
                        'baz 2',
                        'baz 4'
                    ]
                });
            });
        });

        describe('reset', function () {
            it('should reset all fields', function () {
                createContainer([
                    { required: true, name: 'foo', value: 'foo value' },
                    { required: true, name: 'bar', value: 'bar value' },
                    {
                        xtype: 'container',
                        defaultType: 'checkboxfield',
                        items: [
                            { name: 'baz', value: 'baz 1', checked: true },
                            { name: 'baz', value: 'baz 2', checked: true },
                            { name: 'baz', value: 'baz 3' },
                            { name: 'baz', value: 'baz 4', checked: true }
                        ]
                    },
                    {
                        xtype: 'container',
                        defaultType: 'radiofield',
                        items: [
                            { name: 'bazier', value: 'bazier 1', checked: true },
                            { name: 'bazier', value: 'bazier 2' },
                            { name: 'bazier', value: 'bazier 3' },
                            { name: 'bazier', value: 'bazier 4' }
                        ]
                    }
                ]);

                var origValues = {
                        bar: 'bar value',
                        bazier: 'bazier 1',
                        foo: 'foo value',
                        baz: [
                            'baz 1',
                            'baz 2',
                            'baz 4'
                        ]
                    },
                    newValues = {
                        bar: '',
                        bazier: 'bazier 3',
                        foo: '',
                        baz: [
                            'baz 1',
                            'baz 3'
                        ]
                    },
                    foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar');

                expect(ct.getValues()).toEqual(origValues);

                expect(ct.setValues(newValues)).toBe(ct);

                expect(ct.getValues()).toEqual(newValues);

                expect(foo.getError()).toEqual(['This field is required']);
                expect(bar.getError()).toEqual(['This field is required']);

                expect(ct.reset()).toBe(ct);

                expect(ct.getValues()).toEqual(origValues);
            });

            it('should reset all fields and setError(null) each field', function () {
                createContainer([
                    { required: true, name: 'foo', value: 'foo value' },
                    { required: true, name: 'bar', value: 'bar value' },
                    {
                        xtype: 'container',
                        defaultType: 'checkboxfield',
                        items: [
                            { name: 'baz', value: 'baz 1', checked: true },
                            { name: 'baz', value: 'baz 2', checked: true },
                            { name: 'baz', value: 'baz 3' },
                            { name: 'baz', value: 'baz 4', checked: true }
                        ]
                    },
                    {
                        xtype: 'container',
                        defaultType: 'radiofield',
                        items: [
                            { name: 'bazier', value: 'bazier 1', checked: true },
                            { name: 'bazier', value: 'bazier 2' },
                            { name: 'bazier', value: 'bazier 3' },
                            { name: 'bazier', value: 'bazier 4' }
                        ]
                    }
                ]);

                var origValues = {
                        bar: 'bar value',
                        bazier: 'bazier 1',
                        foo: 'foo value',
                        baz: [
                            'baz 1',
                            'baz 2',
                            'baz 4'
                        ]
                    },
                    newValues = {
                        bar: '',
                        bazier: 'bazier 3',
                        foo: '',
                        baz: [
                            'baz 1',
                            'baz 3'
                        ]
                    },
                    foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar');

                expect(ct.getValues()).toEqual(origValues);

                expect(ct.setValues(newValues)).toBe(ct);

                expect(ct.getValues()).toEqual(newValues);

                expect(foo.getError()).toEqual(['This field is required']);
                expect(bar.getError()).toEqual(['This field is required']);

                expect(ct.reset(true)).toBe(ct);

                expect(ct.getValues()).toEqual(origValues);

                expect(foo.getError()).toBeNull();
                expect(bar.getError()).toBeNull();
            });
        });

        describe('updateDisabled', function () {
            it('should disable all fields', function () {
                createContainer([
                    { required: true, name: 'foo', value: 'foo value' },
                    { required: true, name: 'bar', value: 'bar value' },
                    {
                        xtype: 'container',
                        defaultType: 'checkboxfield',
                        items: [
                            { name: 'baz', value: 'baz 1', checked: true },
                            { name: 'baz', value: 'baz 2', checked: true },
                            { name: 'baz', value: 'baz 3' },
                            { name: 'baz', value: 'baz 4', checked: true }
                        ]
                    }
                ]);

                var foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar'),
                    baz = ct.lookupName('baz');

                expect(ct.setDisabled(true)).toBe(ct);

                expect(foo.getDisabled()).toBe(true);
                expect(bar.getDisabled()).toBe(true);

                baz.forEach(function (field) {
                    expect(field.getDisabled()).toBe(true);
                });
            });

            it('should enable all fields', function () {
                createContainer([
                    { required: true, name: 'foo', value: 'foo value', disabled: true },
                    { required: true, name: 'bar', value: 'bar value', disabled: true },
                    {
                        xtype: 'container',
                        defaultType: 'checkboxfield',
                        items: [
                            { name: 'baz', value: 'baz 1', disabled: true, checked: true },
                            { name: 'baz', value: 'baz 2', disabled: true, checked: true },
                            { name: 'baz', value: 'baz 3', disabled: true },
                            { name: 'baz', value: 'baz 4', disabled: true, checked: true }
                        ]
                    }
                ]);

                var foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar'),
                    baz = ct.lookupName('baz');

                expect(foo.getDisabled()).toBe(true);
                expect(bar.getDisabled()).toBe(true);

                baz.forEach(function (field) {
                    expect(field.getDisabled()).toBe(true);
                });

                expect(ct.setDisabled(false)).toBe(ct);

                expect(foo.getDisabled()).toBe(false);
                expect(bar.getDisabled()).toBe(false);

                baz.forEach(function (field) {
                    expect(field.getDisabled()).toBe(false);
                });
            });
        });

        describe('setErrors', function () {
            it('should set errors on all fields', function () {
                createContainer([
                    { name: 'foo' },
                    { name: 'bar' },
                    {
                        xtype: 'container',
                        defaultType: 'checkboxfield',
                        items: [
                            { name: 'baz', value: 'baz 1' },
                            { name: 'baz', value: 'baz 2' },
                            { name: 'baz', value: 'baz 3' },
                            { name: 'baz', value: 'baz 4' }
                        ]
                    },
                    {
                        xtype: 'container',
                        defaultType: 'radiofield',
                        items: [
                            { name: 'bazier', value: 'bazier 1' },
                            { name: 'bazier', value: 'bazier 2' },
                            { name: 'bazier', value: 'bazier 3' },
                            { name: 'bazier', value: 'bazier 4' }
                        ]
                    }
                ]);

                var foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar'),
                    baz = ct.lookupName('baz'),
                    bazier = ct.lookupName('bazier'),
                    errors = {
                        foo: 'Foo is in error!',
                        bar: [
                            'Bar is in error!',
                            'Bar still error!'
                        ],
                        baz: 'Baz is errored',
                        bazier: 'Bazier not very good'
                    };

                expect(ct.setErrors(errors)).toBe(ct);

                expect(foo.getError()).toBe('Foo is in error!');
                expect(bar.getError()).toEqual(['Bar is in error!', 'Bar still error!']);

                baz.forEach(function (check) {
                    expect(check.getError()).toBe('Baz is errored');
                });

                bazier.forEach(function (radio) {
                    expect(radio.getError()).toBe('Bazier not very good');
                });
            });

            it('should not set errors if an array is passed', function () {
                createContainer();

                var spy = spyOn(Ext, 'raise');

                ct.setErrors([]);

                expect(spy).toHaveBeenCalled();
            });

            it('should not set errors if a string is passed', function () {
                createContainer();

                var spy = spyOn(Ext, 'raise');

                ct.setErrors('foobar');

                expect(spy).toHaveBeenCalled();
            });

            it('should set errors on nested fields', function () {
                createContainer({
                    layout: {
                        type: 'form'
                    },
                    items: [{
                        xtype: 'containerfield',
                        label: 'Name',
                        defaults: {
                            flex: 1,
                            labelAlign: 'top'
                        },
                        items: [
                            { label: 'First', name: 'first' },
                            { label: 'Middle', name: 'middle' },
                            { label: 'Last', name: 'last' }
                        ]
                    }, {
                        xtype: 'textfield',
                        label: 'A longer label to test width',
                        name: 'text'
                    }]
                });

                var first = ct.lookupName('first'),
                    middle = ct.lookupName('middle'),
                    last = ct.lookupName('last'),
                    text = ct.lookupName('text'),
                    errors = {
                        text: 'Text value is bad!',
                        //containerfield errors below
                        first: 'First is bad!',
                        middle: 'Middle is bad!',
                        last: 'Last is bad!'
                    };

                expect(ct.setErrors(errors)).toBe(ct);

                expect(first.getError()).toBe('First is bad!');
                expect(middle.getError()).toBe('Middle is bad!');
                expect(last.getError()).toBe('Last is bad!');
                expect(text.getError()).toBe('Text value is bad!');
            });
        });

        describe('clearErrors', function () {
            it('should clear all field errors', function () {
                createContainer([
                    { name: 'foo', error: 'foo error' },
                    { name: 'bar', error: 'bar error' },
                    {
                        xtype: 'container',
                        defaultType: 'radiofield',
                        items: [
                            { name: 'bazier', value: 'bazier 1', error: 'bazier error' },
                            { name: 'bazier', value: 'bazier 2', error: 'bazier error' },
                            { name: 'bazier', value: 'bazier 3', error: 'bazier error' },
                            { name: 'bazier', value: 'bazier 4', error: 'bazier error' }
                        ]
                    }
                ]);

                var foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar'),
                    baz = ct.lookupName('baz'),
                    bazier = ct.lookupName('bazier');

                expect(foo.getError()).toBe('foo error');
                expect(bar.getError()).toBe('bar error');

                bazier.forEach(function (radio) {
                    expect(radio.getError()).toBe('bazier error');
                });

                expect(ct.clearErrors()).toBe(ct);

                expect(foo.getError()).toBeNull();
                expect(bar.getError()).toBeNull();

                bazier.forEach(function (radio) {
                    expect(radio.getError()).toBeNull();
                });
            });

            it('should not clear fields without a name', function () {
                createContainer([
                    { name: 'foo', error: 'foo error' },
                    { error: 'bar error' },
                    {
                        xtype: 'container',
                        defaultType: 'radiofield',
                        items: [
                            { name: 'bazier', value: 'bazier 1', error: 'bazier error' },
                            { name: 'bazier', value: 'bazier 2', error: 'bazier error' },
                            { name: 'bazier', value: 'bazier 3', error: 'bazier error' },
                            { name: 'bazier', value: 'bazier 4', error: 'bazier error' }
                        ]
                    }
                ]);

                var foo = ct.lookupName('foo'),
                    bar = ct.getAt(1),
                    baz = ct.lookupName('baz'),
                    bazier = ct.lookupName('bazier');

                expect(foo.getError()).toBe('foo error');
                expect(bar.getError()).toBe('bar error');

                bazier.forEach(function (radio) {
                    expect(radio.getError()).toBe('bazier error');
                });

                expect(ct.clearErrors()).toBe(ct);

                expect(foo.getError()).toBeNull();
                expect(bar.getError()).toBe('bar error');

                bazier.forEach(function (radio) {
                    expect(radio.getError()).toBeNull();
                });
            });
        });

        describe('getErrors', function () {
            it('should return field errors', function () {
                createContainer([
                    { name: 'foo', error: 'foo error' },
                    { name: 'bar' },
                    {
                        xtype: 'container',
                        defaultType: 'radiofield',
                        items: [
                            { name: 'bazier', value: 'bazier 1', error: 'bazier error' },
                            { name: 'bazier', value: 'bazier 2', error: 'bazier error' },
                            { name: 'bazier', value: 'bazier 3', error: 'bazier error' },
                            { name: 'bazier', value: 'bazier 4', error: 'bazier error' }
                        ]
                    }
                ]);

                expect(ct.getErrors()).toEqual({
                    foo: 'foo error',
                    bar: null,
                    bazier: 'bazier error'
                });
            });
        });

        describe('isValid', function () {
            it('should stop checking fields when a field is not valid', function () {

            it('should stop checking fields when a field is not valid', function () {
                createContainer([
                    { name: 'foo', error: 'foo error' },
                    { name: 'bar' }
                ]);

                var foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar'),
                    spy = spyOn(bar, 'isValid');

                expect(ct.isValid()).toBe(false);
                expect(spy).not.toHaveBeenCalled();
            });
            });

            it('should check all fields if valid', function () {
                createContainer([
                    { name: 'foo' },
                    { name: 'bar' }
                ]);

                var bar = ct.lookupName('bar'),
                    spy = spyOn(bar, 'isValid').andCallThrough();

                expect(ct.isValid()).toBe(true);
                expect(spy).toHaveBeenCalled();
            });
        });

        describe('validate', function () {
            it('should validate all fields', function () {
                createContainer([
                    { name: 'foo', required: true },
                    { name: 'bar' }
                ]);

                var foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar'),
                    spy = spyOn(bar, 'validate').andCallThrough();

                expect(ct.validate()).toBe(false);
                expect(spy).toHaveBeenCalled();
            });

            it('should be valid', function () {
                createContainer([
                    { name: 'foo', value: 'foo', required: true },
                    { name: 'bar' }
                ]);

                var foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar'),
                    spy = spyOn(bar, 'validate').andCallThrough();

                expect(ct.validate()).toBe(true);
                expect(spy).toHaveBeenCalled();
            });
        });

        describe('getFields', function () {
            beforeEach(function () {
                createContainer([
                    { name: 'foo' },
                    { name: 'bar' },
                    {
                        xtype: 'container',
                        defaultType: 'checkboxfield',
                        items: [
                            { name: 'baz', value: 'baz 1' },
                            { name: 'baz', value: 'baz 2' },
                            { name: 'baz', value: 'baz 3' },
                            { name: 'baz', value: 'baz 4' }
                        ]
                    },
                    {
                        xtype: 'container',
                        defaultType: 'radiofield',
                        items: [
                            { name: 'bazier', value: 'bazier 1' },
                            { name: 'bazier', value: 'bazier 2' },
                            { name: 'bazier', value: 'bazier 3' },
                            { name: 'bazier', value: 'bazier 4' }
                        ]
                    }
                ]);
            });

            describe('return object', function () {
                it('should return an object of fields by name', function () {
                    var fields = {
                            foo: ct.lookupName('foo'),
                            bar: ct.lookupName('bar'),
                            baz: jasmine.array.toMap(ct.lookupName('baz'), 'id'),
                            bazier: jasmine.array.toMap(ct.lookupName('bazier'), 'id')
                        },
                        found = ct.getFields();

                    found.baz = jasmine.array.toMap(found.baz, 'id');
                    found.bazier = jasmine.array.toMap(found.bazier, 'id');

                    expect(found).toEqual(fields);
                });

                it('should return an object of shallow fields by name', function () {
                    var fields = {
                        foo: ct.lookupName('foo'),
                        bar: ct.lookupName('bar')
                    };

                    expect(ct.getFields(null, false)).toEqual(fields);
                });
            });

            describe('return array', function () {
                it('should return an array if byName is false', function () {
                    var fields = jasmine.array.toMap([
                        ct.lookupName('foo'),
                        ct.lookupName('bar')
                    ].concat(
                        ct.lookupName('baz'),
                        ct.lookupName('bazier')
                    ), 'id');

                    expect(jasmine.array.toMap(ct.getFields(false), 'id')).toEqual(fields);
                });

                it('should return an array if name passed', function () {
                    var fields = jasmine.array.toMap(ct.lookupName('baz'), 'id');

                    expect(jasmine.array.toMap(ct.getFields('baz'), 'id')).toEqual(fields);
                });
            });

            describe('return instance', function () {
                it('should return an instance if name passed', function () {
                    var field = ct.lookupName('foo');

                    expect(ct.getFields('foo')).toBe(field);
                });

                it('should return a shallow instance if name is passed', function () {
                    var field = ct.lookupName('foo');

                    expect(ct.getFields('foo', false)).toBe(field);
                });
            });

            describe('return undefined', function () {
                it('should not return if name does not match', function () {
                    expect(ct.getFields('foobar')).toBeUndefined();
                });

                it('should not return if name is passed but is not shallow', function () {
                    expect(ct.getFields('baz', false)).toBeUndefined();
                });
            });
        });

        describe('getFocusedField', function () {
            it('should find a focused field', function () {
                createContainer({
                    renderTo: Ext.getBody(),
                    items: [
                        { name: 'foo' },
                        { name: 'bar' }
                    ]
                });

                var foo = ct.lookupName('foo');

                waitsFor(function () {
                    return ct.rendered;
                });

                runs(function () {
                    foo.onFocus();

                    expect(ct.getFocusedField()).toBe(foo);
                });
            });

            it('should not find a focused field', function () {
                createContainer({
                    renderTo: Ext.getBody(),
                    items: [
                        { name: 'foo' },
                        { name: 'bar' }
                    ]
                });

                var foo = ct.lookupName('foo');

                waitsFor(function () {
                    return ct.rendered;
                });

                runs(function () {
                    expect(ct.getFocusedField()).toBeNull();
                });
            });
        });

        describe('getNextField', function () {
            it('should find the next field after the focused field', function () {
                createContainer({
                    renderTo: Ext.getBody(),
                    items: [
                        { name: 'foo' },
                        { name: 'bar' }
                    ]
                });

                var foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar');

                waitsFor(function () {
                    return ct.rendered;
                });

                runs(function () {
                    foo.onFocus();

                    expect(ct.getNextField()).toBe(bar);
                });
            });

            it('should not find the next field after the focused field', function () {
                createContainer({
                    renderTo: Ext.getBody(),
                    items: [
                        { name: 'foo' },
                        { name: 'bar' }
                    ]
                });

                var foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar');

                waitsFor(function () {
                    return ct.rendered;
                });

                runs(function () {
                    expect(ct.getNextField()).toBe(false);
                });
            });

            it('should not find the next field if focused is last field', function () {
                createContainer({
                    renderTo: Ext.getBody(),
                    items: [
                        { name: 'foo' },
                        { name: 'bar' }
                    ]
                });

                var bar = ct.lookupName('bar');

                waitsFor(function () {
                    return ct.rendered;
                });

                runs(function () {
                    bar.onFocus();

                    expect(ct.getNextField()).toBe(false);
                });
            });
        });

        describe('focusNextField', function () {
            it('should focus next field', function () {
                createContainer({
                    renderTo: Ext.getBody(),
                    items: [
                        { name: 'foo' },
                        { name: 'bar' }
                    ]
                });

                var foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar'),
                    spy = spyOn(bar, 'focus');

                waitsFor(function () {
                    return ct.rendered;
                });

                runs(function () {
                    foo.onFocus();

                    expect(ct.focusNextField()).toBe(bar);
                    expect(spy).toHaveBeenCalled();
                });
            });

            it('should not focus next field if no field was focused', function () {
                createContainer({
                    renderTo: Ext.getBody(),
                    items: [
                        { name: 'foo' },
                        { name: 'bar' }
                    ]
                });

                var foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar'),
                    spy = spyOn(bar, 'focus');

                waitsFor(function () {
                    return ct.rendered;
                });

                runs(function () {
                    expect(ct.focusNextField()).toBe(false);
                    expect(spy).not.toHaveBeenCalled();
                });
            });

            it('should not focus next field if focused is last field', function () {
                createContainer({
                    renderTo: Ext.getBody(),
                    items: [
                        { name: 'foo' },
                        { name: 'bar' }
                    ]
                });

                var bar = ct.lookupName('bar');

                waitsFor(function () {
                    return ct.rendered;
                });

                runs(function () {
                    bar.onFocus();

                    expect(ct.focusNextField()).toBe(false);
                });
            });
        });

        describe('getPreviousField', function () {
            it('should get field before focused field', function () {
                createContainer({
                    renderTo: Ext.getBody(),
                    items: [
                        { name: 'foo' },
                        { name: 'bar' }
                    ]
                });

                var foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar');

                waitsFor(function () {
                    return ct.rendered;
                });

                runs(function () {
                    bar.onFocus();

                    expect(ct.getPreviousField()).toBe(foo);
                });
            });

            it('should not find the previous field if no field is focused', function () {
                createContainer({
                    renderTo: Ext.getBody(),
                    items: [
                        { name: 'foo' },
                        { name: 'bar' }
                    ]
                });

                var foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar');

                waitsFor(function () {
                    return ct.rendered;
                });

                runs(function () {
                    expect(ct.getPreviousField()).toBe(false);
                });
            });

            it('should not find the previous field if focused is first field', function () {
                createContainer({
                    renderTo: Ext.getBody(),
                    items: [
                        { name: 'foo' },
                        { name: 'bar' }
                    ]
                });

                var foo = ct.lookupName('foo');

                waitsFor(function () {
                    return ct.rendered;
                });

                runs(function () {
                    foo.onFocus();

                    expect(ct.getPreviousField()).toBe(false);
                });
            });
        });

        describe('focusPreviousField', function () {
                it('should focus previous field', function () {
                createContainer({
                    renderTo: Ext.getBody(),
                    items: [
                        { name: 'foo' },
                        { name: 'bar' }
                    ]
                });

                var foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar'),
                    spy = spyOn(foo, 'focus');

                waitsFor(function () {
                    return ct.rendered;
                });

                runs(function () {
                    bar.onFocus();

                    expect(ct.focusPreviousField()).toBe(foo);
                    expect(spy).toHaveBeenCalled();
                });
            });

            it('should not focus previous field if no field was focused', function () {
                createContainer({
                    renderTo: Ext.getBody(),
                    items: [
                        { name: 'foo' },
                        { name: 'bar' }
                    ]
                });

                var foo = ct.lookupName('foo'),
                    bar = ct.lookupName('bar'),
                    spy = spyOn(foo, 'focus');

                waitsFor(function () {
                    return ct.rendered;
                });

                runs(function () {
                    expect(ct.focusPreviousField()).toBe(false);
                    expect(spy).not.toHaveBeenCalled();
                });
            });

            it('should not focus previous field if focused is first field', function () {
                createContainer({
                    renderTo: Ext.getBody(),
                    items: [
                        { name: 'foo' },
                        { name: 'bar' }
                    ]
                });

                var foo = ct.lookupName('foo');

                waitsFor(function () {
                    return ct.rendered;
                });

                runs(function () {
                    foo.onFocus();

                    expect(ct.focusPreviousField()).toBe(false);
                });
            });
        });
    }
);
