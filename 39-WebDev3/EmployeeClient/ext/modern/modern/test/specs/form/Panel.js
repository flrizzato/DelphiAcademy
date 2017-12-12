topSuite("Ext.form.Panel",
    ['Ext.field.*', 'Ext.layout.VBox', 'Ext.direct.RemotingProvider'],
function() {
    var field, panel,
        create = function(config) {
            panel = Ext.create('Ext.form.Panel', config || {});
        };

    afterEach(function() {
        if (panel) {
            panel.destroy();
        }
    });

    describe("validation", function() {
        var azonly, nonblank, validated, noname;

        function messageToString(message) {
            if (message === null || message === undefined) {
                return message;
            }
            else {
                return message.toString();
            }
        }

        function activeError(field) {
            var e = field.getError();
            if (e) {
                return e[0];
            }
            else if (e === null ){
                return null;
            }
            else {
                return undefined;
            }
        }

        function testGetErrors(eazonly, enonblank, evalidated, enoname) {
            var errors = panel.getErrors();

            expect(messageToString(errors.azonly)).toBe(eazonly);
            expect(messageToString(errors.nonblank)).toBe(enonblank);
            expect(messageToString(errors.validated)).toBe(evalidated);
            expect(messageToString(activeError(noname))).toBe(enoname);
        }

        beforeEach(function() {
            create({
                renderTo: Ext.getBody(),
                width: 300,
                height: 300,
                defaults: {
                    xtype: 'textfield'
                },
                items: [
                    {
                        name: 'azonly',
                        label: 'a-z only'
                    },
                    {
                        name: 'nonblank',
                        required: true,
                        label: 'nonblank'
                    },
                    {
                        name: 'validated',
                        label: 'validator (upper  only)',
                        validators: /^[A-Z]*$/
                    },
                    {
                        itemId: 'noname',
                        label: 'noname',
                        validators: /^[0-9]*$/
                    }
                ]
            });

            azonly = panel.lookupName('azonly');
            nonblank = panel.lookupName('nonblank');
            validated = panel.lookupName('validated');
            noname = panel.down('#noname');
        });

        describe("setErrors", function() {
            it("should mark field invalid by name", function() {
                panel.setErrors({
                    azonly: 'dead beef'
                });
                testGetErrors('dead beef', null, null, null);
            });

            it("should not set error on field not found by name", function() {
                panel.setErrors({
                    notfound: 'dead beef'
                });

                testGetErrors(null, null, null, null);
            });

            it("should clear errors if setErrors with null message", function() {
                panel.setErrors({
                    azonly: 'dead beef11',
                    nonblank: 'dead beef22',
                    validated: 'dead beef33'
                });

                testGetErrors('dead beef11', 'dead beef22', 'dead beef33', null);

                panel.setErrors({
                    azonly: 'dead beef1',
                    nonblank: null,
                    validated: 'dead beef3'
                });

                testGetErrors('dead beef1', null, 'dead beef3', null);
            });

        });

        describe("getErrors", function() {
            it("should return null messages if getErrors called on valid form", function() {
                testGetErrors(null, null, null, null);
            });

            it("should validate only field with validators", function() {
                validated.setValue('abc');

                testGetErrors(null, null, 'Is in the wrong format', null);
            });

            it("should get manually invalidated field errors", function() {

                nonblank.markInvalid('dead beef222');
                testGetErrors(null, 'dead beef222', null, null);
            });

        });

        describe("clearErrors", function() {
            it("should clear all errors", function() {
                panel.setErrors({
                    azonly: 'dead beef11',
                    nonblank: 'dead beef22',
                    validated: 'dead beef33'
                });

                testGetErrors('dead beef11', 'dead beef22', 'dead beef33', null);
                panel.clearErrors();
                testGetErrors(null, null, null, null);
            });
        });

        describe("isValid", function() {
            it("should be valid before validating anything", function() {
                expect(panel.isValid()).toBe(true);
            });
            it("should obey manually invalidated fields", function() {
                expect(panel.isValid()).toBe(true);
                noname.markInvalid('is Invalid');
                expect(panel.isValid()).toBe(false);
            });
            it("should obey invalid fields marked invalid via setErrors", function() {
                expect(panel.isValid()).toBe(true);
                panel.setErrors({
                    azonly: 'dead beef1',
                    nonblank: null,
                    validated: 'dead beef3'
                });

                testGetErrors('dead beef1', null, 'dead beef3', null);
                expect(panel.isValid()).toBe(false);
            });
        });

        describe('validate', function() {
            it("should validate fields at the panel level", function() {
                expect(panel.isValid()).toBe(true);
                noname.setValue('abc');
                expect(panel.isValid()).toBe(false);

                noname.setValue('0');
                expect(panel.isValid()).toBe(true);

                noname.setValue('abc');
                expect(panel.isValid()).toBe(false);

                noname.clearInvalid();
                expect(panel.isValid()).toBe(true);

                panel.validate();
                expect(panel.isValid()).toBe(false);
            });

            it("should return true if form fields are all valid", function() {
                expect(panel.isValid()).toBe(true);
                noname.setValue('123');
                nonblank.setValue('0')
                expect(panel.validate()).toBe(true);
            });

            it("should return false if any form field is not valid", function() {
                expect(panel.isValid()).toBe(true);
                noname.setValue('abc');
                expect(panel.validate()).toBe(false);
            });
        });
    });

    describe("setDisabled", function() {
        var field;

        beforeEach(function() {
            field = Ext.create('Ext.field.Text', {
                name: 'test'
            });

            create({
                items: [field]
            });
        });

        it("should disable all fields", function() {
            expect(panel.getDisabled()).toBeFalsy();
            expect(field.getDisabled()).toBeFalsy();

            panel.setDisabled(true);

            expect(panel.getDisabled()).toBeTruthy();
            expect(field.getDisabled()).toBeTruthy();
        });
    });

    describe("getValues", function() {
        it("should return values for one field", function() {
            create({
                items: [
                    {
                        xtype: 'textfield',
                        name: 'one',
                        value: 'test1'
                    }
                ]
            });

            expect(panel.getValues()).toEqual({
                one: 'test1'
            });
        });

        it("should return values for two fields", function() {
            create({
                items: [
                    {
                        xtype: 'textfield',
                        name: 'one',
                        value: 'test1'
                    },
                    {
                        xtype: 'textfield',
                        name: 'two',
                        value: 'test2'
                    }
                ]
            });

            expect(panel.getValues()).toEqual({
                one: 'test1',
                two: 'test2'
            });
        });

        it("should return values for fields with a name with brackets", function() {
            create({
                items: [
                    {
                        xtype: 'textfield',
                        name: 'one[]',
                        value: 'test1'
                    },
                    {
                        xtype: 'textfield',
                        name: 'one[]',
                        value: 'test2'
                    }
                ]
            });

            expect(panel.getValues()).toEqual({
                'one[]': ['test1', 'test2']
            });
        });

        it("should return values for two fields with the same name", function() {
            create({
                items: [
                    {
                        xtype: 'textfield',
                        name: 'one',
                        value: 'test1'
                    },
                    {
                        xtype: 'textfield',
                        name: 'one',
                        value: 'test2'
                    }
                ]
            });

            expect(panel.getValues()).toEqual({
                one: ['test1', 'test2']
            });
        });

        it("should return values for checkbox fields", function() {
            create({
                items: [
                    {
                        xtype: 'checkboxfield',
                        name: 'one',
                        checked: true
                    },
                    {
                        xtype: 'checkboxfield',
                        name: 'two'
                    }
                ]
            });

            expect(panel.getValues()).toEqual({
                one: true,
                two: null
            });
        });

        it("should return values for checkbox fields with the same name", function() {
            create({
                items: [
                    {
                        xtype: 'checkboxfield',
                        name: 'one',
                        value: 'blue',
                        checked: true
                    },
                    {
                        xtype: 'checkboxfield',
                        name: 'one',
                        value: 'red',
                        checked: true
                    }
                ]
            });

            expect(panel.getValues()).toEqual({
                one: ['blue', 'red']
            });
        });

        it("should return values for radio fields fields, test 1", function() {
            create({
                items: [
                    {
                        xtype: 'radiofield',
                        name: 'color',
                        value: 'red'
                    },
                    {
                        xtype: 'radiofield',
                        name: 'color',
                        checked: true,
                        value: 'green'
                    },
                    {
                        xtype: 'radiofield',
                        name: 'color',
                        value: 'blue'
                    }
                ]
            });

            expect(panel.getValues()).toEqual({
                color: 'green'
            });
        });

        it("should return values for radio fields fields, test 2", function() {
            create({
                items: [
                    {
                        xtype: 'radiofield',
                        name: 'color',
                        value: 1
                    },
                    {
                        xtype: 'radiofield',
                        name: 'color',
                        checked: true,
                        value: 0
                    }
                ]
            });

            expect(panel.getValues()).toEqual({
                color: 0
            });
        });

        it("should return values for radio fields fields, test 3", function() {
            create({
                items: [
                    {
                        xtype: 'radiofield',
                        name: 'color',
                        value: 1,
                        checked: true
                    },
                    {
                        xtype: 'radiofield',
                        name: 'color',
                        value: 0
                    }
                ]
            });

            expect(panel.getValues()).toEqual({
                color: 1
            });
        });

        describe('containerfield', function () {
            it('should get values from Ext.field.Container child fields', function () {
                create({
                    items: [{
                        xtype: 'textfield',
                        name: 'foo',
                        value: 'Foo'
                    }, {
                        xtype: 'containerfield',
                        items: [{
                            name: 'bar',
                            value: 'Bar'
                        }, {
                            name: 'baz',
                            value: 'Baz'
                        }]
                    }, {
                        xtype: 'containerfield',
                        items: [{
                            xtype: 'checkboxfield',
                            name: 'checker',
                            value: 'one',
                            checked: true
                        }, {
                            xtype: 'checkboxfield',
                            name: 'checker',
                            value: 'two',
                            checked: true
                        }, {
                            xtype: 'checkboxfield',
                            name: 'checker',
                            value: 'three'
                        }, {
                            xtype: 'checkboxfield',
                            name: 'checker',
                            value: 'four'
                        }, {
                            xtype: 'checkboxfield',
                            name: 'checker',
                            value: 'five',
                            checked: true
                        }]
                    }]
                });

                expect(panel.getValues()).toEqual({
                    foo: 'Foo',
                    bar: 'Bar',
                    baz: 'Baz',
                    checker: [
                        'one',
                        'two',
                        'five'
                    ]
                });
            });
        });

        describe("enabled argument", function() {
            it("it should not return disabled fields, test 1", function() {
                create({
                    items: [
                        {
                            xtype: 'textfield',
                            name: 'one',
                            disabled: true,
                            value: 'test1'
                        },
                        {
                            xtype: 'textfield',
                            name: 'two',
                            value: 'test2'
                        }
                    ]
                });

                expect(panel.getValues(true)).toEqual({
                    two: 'test2'
                });
            });

            it("it should not return disabled fields, test 2", function() {
                create({
                    items: [
                        {
                            xtype: 'textfield',
                            name: 'one',
                            disabled: true,
                            value: 'test1'
                        },
                        {
                            xtype: 'textfield',
                            name: 'two',
                            value: 'test2'
                        },
                        {
                            xtype: 'textfield',
                            name: 'three',
                            value: 'test3'
                        }
                    ]
                });

                expect(panel.getValues(true)).toEqual({
                    two: 'test2',
                    three: 'test3'
                });
            });
        });
    });

    describe("setValues", function() {
        it("should set the values for one field", function() {
            create({
                items: [
                    {
                        xtype: 'textfield',
                        name: 'one'
                    }
                ]
            });

            panel.setValues({
                one: 'test1'
            });

            expect(panel.getValues()).toEqual({
                one: 'test1'
            });
        });

        it("should set the values for multiple fields", function() {
            create({
                items: [
                    {
                        xtype: 'textfield',
                        name: 'one'
                    },
                    {
                        xtype: 'textfield',
                        name: 'two'
                    }
                ]
            });

            panel.setValues({
                one: 'test1',
                two: 'test2'
            });

            expect(panel.getValues()).toEqual({
                one: 'test1',
                two: 'test2'
            });
        });

        it("should set values for fields with a name with brackets", function() {
            create({
                items: [
                    {
                        xtype: 'textfield',
                        name: 'one[]'
                    },
                    {
                        xtype: 'textfield',
                        name: 'one[]'
                    }
                ]
            });

            panel.setValues({
                'one[]': ['test1', 'test2']
            });

            expect(panel.getValues()).toEqual({
                'one[]': ['test1', 'test2']
            });
        });

        it("should set values for fields with a name with brackets (with empty value)", function() {
            create({
                items: [
                    {
                        xtype: 'textfield',
                        name: 'one[]'
                    },
                    {
                        xtype: 'textfield',
                        name: 'one[]'
                    }
                ]
            });

            panel.setValues({
                'one[]': ['test1']
            });

            expect(panel.getValues()).toEqual({
                'one[]': ['test1', '']
            });
        });

        it("should set values for one checkbox field, test 1", function() {
            create({
                items: [
                    {
                        xtype: 'checkboxfield',
                        name: 'one'
                    },
                    {
                        xtype: 'checkboxfield',
                        name: 'two'
                    }
                ]
            });

            panel.setValues({
                one: true
            });

            expect(panel.getValues()).toEqual({
                one: true,
                two: null
            });
        });

        it("should set values for one checkbox field, test 2", function() {
            create({
                items: [
                    {
                        xtype: 'checkboxfield',
                        name: 'one'
                    }
                ]
            });

            panel.setValues({
                one: true
            });

            expect(panel.getValues()).toEqual({
                one: true
            });
        });

        it("should set values for one checkbox field, test 3", function() {
            create({
                items: [
                    {
                        xtype: 'checkboxfield',
                        name: 'one',
                        checked: true
                    }
                ]
            });

            expect(panel.getValues()).toEqual({
                one: true
            });

            panel.setValues({
                one: false
            });

            expect(panel.getValues()).toEqual({
                one: null
            });
        });

        it("should set values for checkbox fields with the same name", function() {
            create({
                items: [
                    {
                        xtype: 'checkboxfield',
                        name: 'one',
                        value: 'blue',
                        checked: true
                    },
                    {
                        xtype: 'checkboxfield',
                        name: 'one',
                        value: 'red',
                        checked: true
                    }
                ]
            });

            panel.setValues({
                one: ['blue', 'red']
            });

            expect(panel.getValues()).toEqual({
                one: ['blue', 'red']
            });
        });
    });

    describe("load", function() {
        describe("direct", function() {
            var loadSpy;

            function createPanel(methodCfg, panelCfg) {
                methodCfg = Ext.apply({
                    name: 'load',
                    len: 0
                }, methodCfg);

                loadSpy.directCfg = loadSpy.$directCfg = {
                    action: 'TestDirect',
                    method: new Ext.direct.RemotingMethod(methodCfg)
                };

                window.TestDirect = {
                    load: loadSpy
                };

                panelCfg = Ext.apply({
                    api: {
                        load: loadSpy
                    },

                    items: [{
                        xtype: 'checkboxfield',
                        name: 'checkbox'
                    }, {
                        xtype: 'radiofield',
                        name: 'radio',
                        value: 'one'
                    }, {
                        xtype: 'radiofield',
                        name: 'radio',
                        value: 'two'
                    }, {
                        xtype: 'textfield',
                        name: 'text'
                    }]
                }, panelCfg);

                create(panelCfg);
            }

            beforeEach(function() {
                loadSpy = jasmine.createSpy('load fn');
            });

            afterEach(function() {
                loadSpy = window.TestDirect = null;
                delete window.TestDirect;
            });

            it("should not resolve load fn before first load attempt", function() {
                createPanel(null, {
                    api: {
                        load: 'TestDirect.load'
                    }
                });

                expect(panel.getApi().load).toBe('TestDirect.load');
            });

            it("should throw an exception if load fn cannot be resolved", function() {
                createPanel(null, {
                    api: {
                        load: 'bumble.zingbong'
                    }
                });

                var ex = "Cannot resolve Direct API method 'bumble.zingbong' for load action in Ext.form.Panel instance with id:";

                expect(function() {
                    panel.load();
                }).toThrow(ex);
            });

            it("should throw an exception if load fn is not defined", function() {
                createPanel(null, {
                    api: {
                    }
                });

                var ex = "Cannot find Ext Direct API method for load action";

                expect(function() {
                    panel.load();
                }).toThrow(ex);
            });

            it("should resolve load fn by name", function() {
                createPanel(null, {
                    api: {
                        load: 'TestDirect.load'
                    }
                });

                panel.load();

                expect(loadSpy).toHaveBeenCalled();
            });

            it("should resolve load fn by prefix and name", function() {
                createPanel(null, {
                    api: {
                        prefix: 'TestDirect',
                        load: 'load'
                    }
                });

                panel.load();

                expect(loadSpy).toHaveBeenCalled();
            });

            it("should invoke the 'load' function", function() {
                createPanel();
                panel.load();

                expect(loadSpy).toHaveBeenCalled();
            });

            it("should populate the fields with results", function() {
                loadSpy.andCallFake(function(cb, scope) {
                    Ext.callback(cb, scope, {
                        success: true,
                        result: {
                            checkbox: true,
                            radio: 'two',
                            text: 'blerg'
                        }
                    });

                    createPanel();
                    panel.load();

                    expect(panel.down('checkboxfield').getValue()).toBe(true);
                    expect(panel.query('radiofield')[1].getValue()).toBe(true);
                    expect(panel.down('textfield').getValue()).toBe('blerg');
                });
            });

            it("should pass no arguments fn with len = 0", function() {
                createPanel();
                panel.load({ params: { foo: 'bar' } });

                var args = loadSpy.mostRecentCall.args;

                expect(typeof args[0]).toBe('function');
            });

            it("should pass the params as single argument to named fn", function() {
                createPanel({
                    len: undefined,
                    params: [],
                    strict: false
                });

                panel.load({ params: { foo: 'bar', blerg: 'throbbe' } });

                var args = loadSpy.mostRecentCall.args;

                expect(args[0]).toEqual({
                    foo: 'bar',
                    blerg: 'throbbe'
                });

                expect(typeof args[1]).toBe('function');
            });

            it("should pass the params as single argument when paramsAsHash is true", function() {
                createPanel({
                    len: 1
                });

                panel.load({ params: { zumbo: 'gurgle', fred: 'zingbong' } });

                var args = loadSpy.mostRecentCall.args;

                expect(args[0]).toEqual({
                    zumbo: 'gurgle',
                    fred: 'zingbong'
                });

                expect(typeof args[1]).toBe('function');
            });

            it("should pass the params as ordered arguments with paramOrder", function() {
                createPanel({
                    len: 3
                }, {
                    paramOrder: ['plugh', 'ditto', 'mymse']
                });

                panel.load({
                    params: {
                        plugh: 'gonzo',
                        ditto: 42,
                        mymse: true
                    }
                });

                var args = loadSpy.mostRecentCall.args;

                expect(args[0]).toBe('gonzo');
                expect(args[1]).toBe(42);
                expect(args[2]).toBe(true);
                expect(typeof args[3]).toBe('function');
            });
        });
    });

    describe("submit", function() {
        describe("direct", function() {
            var submitSpy;

            function makePanel(panelCfg) {
                submitSpy = jasmine.createSpy('submit fn');

                submitSpy.directCfg = submitSpy.$directCfg = {
                    action: 'TestDirect',
                    method: new Ext.direct.RemotingMethod({
                        name: 'submit',
                        params: [],
                        strict: false
                    })
                };

                window.TestDirect = {
                    submit: submitSpy
                };

                panelCfg = Ext.apply({
                    api: {
                        submit: submitSpy
                    },

                    items: [{
                        xtype: 'checkboxfield',
                        name: 'checkbox'
                    }, {
                        xtype: 'radiofield',
                        name: 'radio',
                        value: 'one'
                    }, {
                        xtype: 'radiofield',
                        name: 'radio',
                        value: 'two'
                    }, {
                        xtype: 'textfield',
                        name: 'text'
                    }]
                }, panelCfg);

                create(panelCfg);
            }

            afterEach(function() {
                submitSpy = window.TestDirect = null;
                delete window.TestDirect;
            });

            describe("function resolution", function() {
                it("should not resolve function before first submit", function() {
                    makePanel({
                        api: {
                            submit: 'TestDirect.submit'
                        }
                    });

                    expect(panel.getApi().submit).toBe('TestDirect.submit');
                });

                it("should throw an exception if function cannot be resolved", function() {
                    makePanel({
                        api: {
                            submit: 'foo.blerg'
                        }
                    });

                    var ex = "Cannot resolve Direct API method 'foo.blerg' for submit action in Ext.form.Panel instance with id:";

                    expect(function() {
                        panel.submit();
                    }).toThrow(ex);
                });

                it("should throw an exception when function is not defined", function() {
                    makePanel({
                        api: {
                        }
                    });

                    var ex = "Cannot find Ext Direct API method for submit action";

                    expect(function() {
                        panel.submit();
                    }).toThrow(ex);
                });

                it("should resolve function by name at first submit", function() {
                    makePanel({
                        api: {
                            submit: 'TestDirect.submit'
                        }
                    });

                    panel.submit();

                    expect(panel.getApi().submit).toBe(submitSpy);
                });

                it("should resolve function by prefix and name", function() {
                    makePanel({
                        api: {
                            prefix: 'TestDirect',
                            submit: 'submit'
                        }
                    });

                    panel.submit();

                    expect(panel.getApi().submit).toBe(submitSpy);
                });
            });

            describe("parameter passing", function() {
                beforeEach(function() {
                    makePanel();
                });

                it("should pass the generated form element to submit fn", function() {
                    panel.submit();

                    var form = submitSpy.mostRecentCall.args[0];

                    expect(form.tagName).toBe('FORM');
                    expect(form).not.toBe(panel.element.dom);
                });

                it("should pass params to submit fn", function() {
                    panel.submit({
                        params: {
                            throbbe: 'zingbong'
                        }
                    });

                    var form = submitSpy.mostRecentCall.args[0];
                    var fields = form.querySelectorAll('[name=throbbe]');

                    expect(fields.length).toBe(1);
                    expect(fields[0].tagName).toBe('INPUT');
                    expect(fields[0].value).toBe('zingbong');
                });
            });
        });

        describe('standard', function () {
            var frameDom, frame, id;

            beforeEach(function () {
                frameDom = document.createElement('iframe');
                frame = Ext.get(frameDom);
                id = frame.id;
            });

            afterEach(function () {
                frame = panel = Ext.destroy(panel, frame);
            });

            it('should cause the frame to reload during submit', function () {
                var spy = jasmine.createSpy();

                create({
                    standardSubmit: true,
                    url: 'foo'
                });

                frame.set({
                    name: id,
                    src: Ext.SSL_SECURE_URL
                });

                document.body.appendChild(frameDom);
                if (document.frames) {
                    document.frames[id].name = id;
                }

                frame.on({
                    load: spy
                });

                panel.el.set({
                    target: id,
                    method: 'POST'
                });
                panel.submit();

                waitsForSpy(spy, 'form to submit');
            });
        });
    });

    describe("reset", function () {
        beforeEach(function () {
            create({
                items: [{
                    xtype: 'textfield',
                    name: 'name',
                    value: 'John Doe'
                },{
                    xtype: 'textareafield',
                    name: 'bio',
                    value: 'lorem ipsum'
                },{
                    xtype: 'checkboxfield',
                    name: 'favcolor',
                    value: 'blue',
                    checked: true
                },{
                    xtype: 'checkboxfield',
                    name: 'favcolor',
                    value: 'red',
                    checked: true
                },{
                    xtype: 'radiofield',
                    name: 'married',
                    value: 1,
                    checked: true
                },{
                    xtype: 'radiofield',
                    name: 'married',
                    value: 0,
                    checked: false
                }]
            });
        });

        it("should reset the values of all form fields", function () {
            var vals = {
                name: 'Jane Doe',
                bio: 'Bio information',
                favcolor: 'red',
                married: 0
            };

            panel.setValues(vals);

            expect(panel.getValues()).toEqual(vals);

            panel.reset();

            expect(panel.getValues()).toEqual({
                name: 'John Doe',
                bio: 'lorem ipsum',
                favcolor: ['blue','red'],
                married: 1
            });
        });
    });
});
