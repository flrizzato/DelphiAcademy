topSuite('Ext.field.Container', ['Ext.JSON', 'Ext.field.Checkbox', 'Ext.field.Radio', 'Ext.field.Text'], function () {
    var ct;

    function createContainer (cfg) {
        if (Ext.isArray(cfg)) {
            cfg = {
                items: cfg
            };
        }

        return ct = new Ext.field.Container(Ext.apply({}, cfg));
    }

    afterEach(function () {
        ct = Ext.destroy(ct);
    });

    describe('proxied', function () {
        it('should have proxied container configs', function () {
            var proto = Ext.field.Container.prototype;

            expect(proto.hasConfig('defaults')).toBe(true);
            expect(proto.hasConfig('defaultType')).toBe(true);
            expect(proto.hasConfig('items')).toBe(true);
            expect(proto.hasConfig('layout')).toBe(true);

            expect(proto).toHaveProperties(
                'getDefaults',    'setDefaults',
                'getDefaultType', 'setDefaultType',
                'getItems',       'setItems',
                'getLayout',      'setLayout'
            );
        });

        it('should have proxied container methods', function () {
            var proto = Ext.field.Container.prototype;

            expect(proto).toHaveProperties('add', 'insert', 'remove', 'removeAll', 'getAt');
        });
    });

    describe('errorTarget', function () {
        it('should set errorTarget to "parent" of configured items', function () {
            createContainer([
                { label: 'foo', name: 'foo' },
                { label: 'bar', name: 'bar' },
                { label: 'baz', name: 'baz' }
            ]);

            var first = ct.getAt(0),
                second = ct.getAt(1),
                third = ct.getAt(2);

            expect(first.getErrorTarget()).toBe('parent');
            expect(second.getErrorTarget()).toBe('parent');
            expect(third.getErrorTarget()).toBe('parent');
        });

        it('should set errorTarget to null of added item', function () {
            createContainer();

            var item = ct.add({
                label: 'foo',
                name: 'foo'
            });

            expect(item.getErrorTarget()).toBe('parent');
        });

        it('should not set errorTarget of configured items', function () {
            createContainer([
                { label: 'foo', name: 'foo', errorTarget: 'qtip' },
                { label: 'bar', name: 'bar', errorTarget: 'side' },
                { label: 'baz', name: 'baz', errorTarget: 'under' }
            ]);

            var first = ct.getAt(0),
                second = ct.getAt(1),
                third = ct.getAt(2);

            expect(first.getErrorTarget()).toBe('qtip');
            expect(second.getErrorTarget()).toBe('side');
            expect(third.getErrorTarget()).toBe('under');
        });

        it('should not set errorTarget of added item', function () {
            createContainer();

            var item = ct.add({
                errorTarget: 'side',
                label: 'foo',
                name: 'foo'
            });

            expect(item.getErrorTarget()).toBe('side');
        });
    });

    describe('getValues', function () {
        it('should return an object as a value', function () {
            createContainer([
                { label: 'foo', name: 'foo', value: 'Foo' },
                { label: 'bar', name: 'bar', value: 'Bar' },
                { label: 'baz', name: 'baz', value: 'Baz' }
            ]);

            expect(ct.getValues()).toEqual({
                foo: 'Foo',
                bar: 'Bar',
                baz: 'Baz'
            });
        });

        it('should return an object as a value with checkboxfields', function () {
            createContainer([
                { label: 'foo', name: 'foo', value: 'Foo' },
                { xtype: 'checkboxfield', label: 'bar 1', name: 'bar', value: 'Bar 1', checked: true },
                { xtype: 'checkboxfield', label: 'bar 2', name: 'bar', value: 'Bar 2', checked: true },
                { label: 'baz', name: 'baz', value: 'Baz' }
            ]);

            expect(ct.getValues()).toEqual({
                foo: 'Foo',
                bar: ['Bar 1', 'Bar 2'],
                baz: 'Baz'
            });
        });

        it('should return an object as a value with radiofields', function () {
            createContainer([
                { label: 'foo', name: 'foo', value: 'Foo' },
                { xtype: 'radiofield', label: 'bar 1', name: 'bar', value: 'Bar 1' },
                { xtype: 'radiofield', label: 'bar 2', name: 'bar', value: 'Bar 2', checked: true },
                { label: 'baz', name: 'baz', value: 'Baz' }
            ]);

            expect(ct.getValues()).toEqual({
                foo: 'Foo',
                bar: 'Bar 2',
                baz: 'Baz'
            });
        });
    });

    describe('setValues', function () {
        it('should set values on all fields', function () {
            createContainer([
                { label: 'foo', name: 'foo' },
                { label: 'bar', name: 'bar' },
                { label: 'baz', name: 'baz' }
            ]);

            var first = ct.getAt(0),
                second = ct.getAt(1),
                third = ct.getAt(2),
                values = {
                    foo: 'Foo',
                    bar: 'Bar',
                    baz: 'Baz'
                };

            expect(ct.setValues(values)).toBe(ct);

            expect(first.getValue()).toBe('Foo');
            expect(second.getValue()).toBe('Bar');
            expect(third.getValue()).toBe('Baz');
        });

        it('should set values on some fields', function () {
            createContainer([
                { label: 'foo', name: 'foo' },
                { label: 'bar', name: 'bar' },
                { label: 'baz', name: 'baz' }
            ]);

            var first = ct.getAt(0),
                second = ct.getAt(1),
                third = ct.getAt(2),
                values = {
                    foo: 'Foo',
                    baz: 'Baz'
                };

            expect(ct.setValues(values)).toBe(ct);

            expect(first.getValue()).toBe('Foo');
            expect(second.getValue()).toBeNull();
            expect(third.getValue()).toBe('Baz');
        });
    });

    describe('onFieldErrorChange', function () {
        it('should get called when a field is marked with error', function () {
            createContainer([
                { label: 'foo', name: 'foo' },
                { label: 'bar', name: 'bar' },
                { label: 'baz', name: 'baz' }
            ]);

            var first = ct.getAt(0),
                spy = spyOn(ct, 'onFieldErrorChange');

            first.setError(['test error']);

            expect(spy).toHaveBeenCalled();
        });
    });

    describe('getRefItems', function () {
        it('should find a field via down()', function () {
            var container = new Ext.Container({
                    items: [
                        {},
                        {},
                        createContainer([
                            { label: 'foo', name: 'foo' },
                            { label: 'bar', name: 'bar' },
                            { label: 'baz', name: 'baz' }
                        ]),
                        {}
                    ]
                }),
                field = container.down('textfield'),
                first = ct.getAt(0);

            expect(field).toBe(first);

            Ext.destroy(container);
        });

        it('should find all fields via query()', function () {
            var container = new Ext.Container({
                    items: [
                        {},
                        {},
                        createContainer([
                            { label: 'foo', name: 'foo' },
                            { label: 'bar', name: 'bar' },
                            { label: 'baz', name: 'baz' }
                        ]),
                        {}
                    ]
                }),
                fields = container.query('textfield'),
                first = ct.getAt(0),
                second = ct.getAt(1),
                third = ct.getAt(2);

            expect(fields).toEqual([
                first,
                second,
                third
            ]);

            Ext.destroy(container);
        });
    });

    describe('getFocusEl', function () {
        it('should get first field\'s focus element', function () {
            createContainer([
                { label: 'foo', name: 'foo' },
                { label: 'bar', name: 'bar' },
                { label: 'baz', name: 'baz' }
            ]);

            var first = ct.getAt(0);

            expect(ct.getFocusEl()).toBe(first.getFocusEl());
        });
    });

    describe('reset', function () {
        it('should reset all fields', function () {
            createContainer([
                { label: 'foo', name: 'foo' },
                { label: 'bar', name: 'bar', value: 'bar' }
            ]);

            var first = ct.getAt(0),
                second = ct.getAt(1);

            first.setValue('foo');
            second.setValue('foobar');

            expect(ct.reset()).toBe(ct);

            expect(first.getValue()).toBe('');
            expect(second.getValue()).toBe('bar');
        });
    });

    describe('setErrors', function () {
        it('should only mark specified fields when passing an object', function () {
            createContainer([
                { placeholder: 'foo', name: 'foo' },
                { placeholder: 'bar', name: 'bar' },
                { placeholder: 'baz', name: 'baz' }
            ]);

            var first = ct.getAt(0),
                second = ct.getAt(1),
                third = ct.getAt(2),
                error = {
                    foo: 'Foo is in error',
                    baz: 'Baz has an error'
                };

            expect(ct.setErrors(error)).toBe(ct);

            expect(ct.getError()).toEqual([
                {
                    label: 'foo',
                    error: 'Foo is in error'
                },
                {
                    label: 'baz',
                    error: 'Baz has an error'
                }
            ]);

            expect(first.getError()).toEqual(['Foo is in error']);
            expect(second.getError()).toBeNull();
            expect(third.getError()).toEqual(['Baz has an error']);
        });

        it('should only mark specified fields when passing an object with nested array', function () {
            createContainer([
                { placeholder: 'foo', name: 'foo' },
                { placeholder: 'bar', name: 'bar' },
                { placeholder: 'baz', name: 'baz' }
            ]);

            var first = ct.getAt(0),
                second = ct.getAt(1),
                third = ct.getAt(2),
                error = {
                    foo: ['Foo is in error', 'Another Foo Error'],
                    baz: 'Baz has an error'
                };

            expect(ct.setErrors(error)).toBe(ct);

            expect(ct.getError()).toEqual([
                {
                    label: 'foo',
                    error: 'Foo is in error'
                },
                {
                    label: 'foo',
                    error: 'Another Foo Error'
                },
                {
                    label: 'baz',
                    error: 'Baz has an error'
                }
            ]);

            expect(first.getError()).toEqual(['Foo is in error', 'Another Foo Error']);
            expect(second.getError()).toBeNull();
            expect(third.getError()).toEqual(['Baz has an error']);
        });

        it('should clear all field invalids', function () {
            createContainer([
                { label: 'foo', name: 'foo' },
                { label: 'bar', name: 'bar' },
                { label: 'baz', name: 'baz' }
            ]);

            var first = ct.getAt(0),
                second = ct.getAt(1),
                third = ct.getAt(2),
                errors = {
                    foo: 'test error',
                    bar: 'test error',
                    baz: 'test error'
                };

            expect(ct.setErrors(errors)).toBe(ct);

            errors.foo = errors.bar = errors.baz = null;

            expect(ct.setErrors(errors)).toBe(ct);

            expect(ct.getError()).toBeNull();

            expect(first.getError()).toBeNull();
            expect(second.getError()).toBeNull();
            expect(third.getError()).toBeNull();
        });

        describe('with child errorTarget', function () {
            it('should only mark specified fields when passing an object', function () {
                createContainer([
                    { placeholder: 'foo', name: 'foo', errorTarget: 'side' },
                    { placeholder: 'bar', name: 'bar', errorTarget: 'side' },
                    { placeholder: 'baz', name: 'baz', errorTarget: 'side' }
                ]);

                var first = ct.getAt(0),
                    second = ct.getAt(1),
                    third = ct.getAt(2),
                    error = {
                        foo: 'Foo is in error',
                        baz: 'Baz has an error'
                    };

                expect(ct.setErrors(error)).toBe(ct);

                expect(ct.getError()).toBeNull();

                expect(first.getError()).toEqual(['Foo is in error']);
                expect(second.getError()).toBeNull();
                expect(third.getError()).toEqual(['Baz has an error']);
            });

            it('should only mark specified fields when passing an object with nested array', function () {
                createContainer([
                    { placeholder: 'foo', name: 'foo', errorTarget: 'side' },
                    { placeholder: 'bar', name: 'bar', errorTarget: 'side' },
                    { placeholder: 'baz', name: 'baz', errorTarget: 'side' }
                ]);

                var first = ct.getAt(0),
                    second = ct.getAt(1),
                    third = ct.getAt(2),
                    error = {
                        foo: ['Foo is in error', 'Another Foo Error'],
                        baz: 'Baz has an error'
                    };

                expect(ct.setErrors(error)).toBe(ct);

                expect(ct.getError()).toBeNull();

                expect(first.getError()).toEqual(['Foo is in error', 'Another Foo Error']);
                expect(second.getError()).toBeNull();
                expect(third.getError()).toEqual(['Baz has an error']);
            });

            it('should clear all field invalids', function () {
                createContainer([
                    { label: 'foo', name: 'foo', errorTarget: 'side' },
                    { label: 'bar', name: 'bar', errorTarget: 'side' },
                    { label: 'baz', name: 'baz', errorTarget: 'side' }
                ]);

                var first = ct.getAt(0),
                    second = ct.getAt(1),
                    third = ct.getAt(2),
                    errors = {
                        foo: 'test error',
                        bar: 'test error',
                        baz: 'test error'
                    };

                expect(ct.setErrors(errors)).toBe(ct);

                errors.foo = errors.bar = errors.baz = null;

                expect(ct.setErrors(errors)).toBe(ct);

                expect(ct.getError()).toBeNull();

                expect(first.getError()).toBeNull();
                expect(second.getError()).toBeNull();
                expect(third.getError()).toBeNull();
            });
        });
    });

    describe('isValid', function () {
        it('should return true if all fields are valid', function () {
            createContainer([
                { label: 'foo', name: 'foo' },
                { label: 'bar', name: 'bar' }
            ]);

            expect(ct.isValid()).toBe(true);
        });

        it('should return false when one field is invalid', function () {
            createContainer([
                { label: 'foo', name: 'foo', required: true },
                { label: 'bar', name: 'bar' }
            ]);

            ct.getAt(0).validate();

            expect(ct.isValid()).toBe(false);
        });

        it('should not check items after one field is found invalid', function () {
            createContainer([
                { label: 'foo', name: 'foo', required: true },
                { label: 'bar', name: 'bar' }
            ]);

            ct.getAt(0).validate();

            var spy = spyOn(ct.getAt(1), 'isValid');

            expect(ct.isValid()).toBe(false);

            expect(spy).not.toHaveBeenCalled();
        });
    });

    describe('validate', function () {
        it('should return true if all fields are valid', function () {
            createContainer([
                { label: 'foo', name: 'foo' },
                { label: 'bar', name: 'bar' }
            ]);

            expect(ct.validate()).toBe(true);
        });

        it('should return false when one field is invalid', function () {
            createContainer([
                { label: 'foo', name: 'foo', required: true },
                { label: 'bar', name: 'bar' }
            ]);

            expect(ct.validate()).toBe(false);
        });

        it('should validate all fields', function () {
            createContainer([
                { label: 'foo', name: 'foo', required: true },
                { label: 'bar', name: 'bar', required: true, requiredMessage: 'This field should not be empty' }
            ]);

            var first = ct.getAt(0),
                spy1 = spyOn(first, 'validate').andCallThrough(),
                second = ct.getAt(1),
                spy2 = spyOn(second, 'validate').andCallThrough();

            expect(ct.validate()).toBe(false);

            expect(first.getError()).toEqual(['This field is required']);
            expect(second.getError()).toEqual(['This field should not be empty']);

            expect(spy1).toHaveBeenCalled();
            expect(spy2).toHaveBeenCalled();
        });
    });

    describe('getFields', function () {
        it('should get all child fields', function () {
            createContainer([
                { label: 'foo', name: 'foo' },
                { label: 'bar', name: 'bar' },
                { label: 'baz', name: 'baz' }
            ]);

            var first = ct.getAt(0),
                second = ct.getAt(1),
                third = ct.getAt(2),
                fields = ct.getFields();

            //tests if the deep arg works
            expect(Object.keys(fields).length).toBe(3);

            expect(fields).toEqual({
                foo: first,
                bar: second,
                baz: third
            });
        });

        it('should get all child fields by name', function () {
            createContainer([
                { label: 'foo', name: 'foo' },
                { label: 'bar', name: 'bar' },
                { label: 'baz', name: 'baz' }
            ]);

            var first = ct.getAt(0),
                second = ct.getAt(1),
                third = ct.getAt(2);

            expect(ct.getFields('foo')).toBe(first);
            expect(ct.getFields('bar')).toBe(second);
            expect(ct.getFields('baz')).toBe(third);
        });

        it('should get all child fields as an array', function () {
            createContainer([
                { label: 'foo', name: 'foo' },
                { label: 'bar', name: 'bar' },
                { label: 'baz', name: 'baz' }
            ]);

            var first = ct.getAt(0),
                second = ct.getAt(1),
                third = ct.getAt(2),
                fields = ct.getFields(false);

            expect(fields.length).toBe(3);

            expect(fields).toEqual([
                first,
                second,
                third
            ]);
        });
    });
});
