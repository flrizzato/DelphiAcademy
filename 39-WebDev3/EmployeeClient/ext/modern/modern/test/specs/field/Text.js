/* global expect, Ext, describe */

topSuite("Ext.field.Text", [
    'Ext.Container',
    'Ext.Button',
    'Ext.field.trigger.Expand',
    'Ext.field.InputMask',
    'Ext.app.ViewController',
    'Ext.app.ViewModel',
    'Ext.data.validator.*'
],
function () {
    var field, el, ct;

    function create(config) {
        field = Ext.create(Ext.apply({
            xtype: 'textfield',
            renderTo: Ext.getBody()
        }, config));

        el = field.el;
    }

    afterEach(function() {
        if (field) {
            field.destroy();
            field = null;
        }

        if (ct) {
            ct.destroy();
            ct = null;
        }
    });

    xdescribe('inputMask', function () {
        it('should create an InputMask', function () {
            create({
                inputMask: 'aaa-###'
            });

            field.setValue('Hello World');
            field.select(2, 5);
            var inputMask = field.getInputMask();
        });
    });

    describe("Validation", function() {
        it("should not validate disabled fields", function() {
            create({
                disabled: true,
                validators: /^[0-9]*$/
            });
            field.setValue('abc');
            expect(el.hasCls('x-invalid')).toBeFalsy();
        });

        it("should validate disabled fields if validateDisabled config is true", function() {
            create({
                disabled: true,
                validateDisabled: true,
                validators: /^[0-9]*$/
            });
            field.setValue('abc');
            expect(el.hasCls('x-invalid')).toBe(true);
        });

        it("should validate disabled fields made enabled", function() {
            create({
                disabled: true,
                validators: /^[0-9]*$/
            });
            field.setValue('abc');
            expect(el.hasCls('x-invalid')).toBeFalsy();
            field.setDisabled(false);
            expect(el.hasCls('x-invalid')).toBe(true);
        });

        it("should clear invalid styling if field made disabled", function() {
            create({
                validators: /^[0-9]*$/
            });
            field.setValue('abc');
            expect(el.hasCls('x-invalid')).toBe(true);
            field.setDisabled(true);
            expect(el.hasCls('x-invalid')).toBeFalsy();
        });

        it("should validate disabled fields if validateDisabled config is set to true", function() {
            create({
                disabled: true,
                validateDisabled: false,
                validators: /^[0-9]*$/
            });
            field.setValue('abc');
            expect(el.hasCls('x-invalid')).toBeFalsy();
            field.setValidateDisabled(true);
            expect(el.hasCls('x-invalid')).toBe(true);
        });

        it("should not allow empty if required: true", function() {
            create({
                required: true
            });
            expect(el.hasCls('x-invalid')).toBe(false);
            field.setValue('abc');
            expect(el.hasCls('x-invalid')).toBe(false);
            field.setValue('');
            expect(el.hasCls('x-invalid')).toBe(true);
            field.setValue('abc');
            expect(el.hasCls('x-invalid')).toBe(false);
            field.setValue(null);
            expect(el.hasCls('x-invalid')).toBe(true);
        });

        it("should allow empty if required: true", function() {
            create({
                required: false
            });
            expect(el.hasCls('x-invalid')).toBe(false);
            field.setValue('abc');
            expect(el.hasCls('x-invalid')).toBe(false);
            field.setValue('');
            expect(el.hasCls('x-invalid')).toBe(false);
            field.setValue('abc');
            expect(el.hasCls('x-invalid')).toBe(false);
            field.setValue(null);
            expect(el.hasCls('x-invalid')).toBe(false);
        });

        it("should validate using a function", function() {
            create({
                validators: function(newValue) {
                    if (/^[0-9]*$/.test(newValue)) {
                        return true;
                    }
                    else {
                        return "Invalid";
                    }
                }
            });
            field.setValue('abc');
            expect(el.hasCls('x-invalid')).toBe(true);
            field.setValue('123');
            expect(el.hasCls('x-invalid')).toBe(false);
        });

        it("should validate using an object with fn", function() {
            create({
                validators: {
                    fn: function(newValue) {
                        return  /^[0-9]*$/.test(newValue) ? true : 'Invalid';
                    }
                }
            });
            field.setValue('abc');
            expect(el.hasCls('x-invalid')).toBe(true);
            field.setValue('123');
            expect(el.hasCls('x-invalid')).toBe(false);
        });

        it("should validate using an fn in controller", function() {
            ct = Ext.create({
                xtype: 'container',
                renderTo: document.body,
                controller: {
                    v: function(newValue) {
                        return  /^[0-9]*$/.test(newValue) ? true : 'Invalid';
                    }
                },
                items: [{
                    xtype: 'textfield',
                    validators: {
                        fn: 'v'
                    }
                }]
            });

            field = ct.items.getAt(0);

            field.setValue('abc');
            expect(field.hasCls('x-invalid')).toBe(true);
            field.setValue('123');
            expect(field.hasCls('x-invalid')).toBe(false);
        });

        it("should validate using a RegEx", function() {
            create({
                validators: /^[0-9]*$/
            });
            field.setValue('abc');
            expect(el.hasCls('x-invalid')).toBe(true);
            field.setValue('123');
            expect(el.hasCls('x-invalid')).toBe(false);
        });

        it("should validate using a data validator, bound", function() {
            create({
                validators: {
                    type: 'bound',
                    min: 10,
                    max: 100
                }
            });
            field.setValue('1000');
            expect(el.hasCls('x-invalid')).toBe(true);
            field.setValue('100');
            expect(el.hasCls('x-invalid')).toBe(false);
        });

        it("should validate using an array of validators", function() {
            create({
                validators: [
                    {
                        type: 'bound',
                        min: 10,
                        max: 100
                    }
                ]
            });
            field.setValue('1000');
            expect(el.hasCls('x-invalid')).toBe(true);
            field.setValue('100');
            expect(el.hasCls('x-invalid')).toBe(false);
        });

        it("should validate using a data validator, email", function() {
            create({
                validators: {
                    type: 'email',
                    message: 'bogus email'
                }
            });
            field.setValue('me@nowhere');
            expect(el.hasCls('x-invalid')).toBe(true);
            field.setValue('me@nowhere.com');
            expect(el.hasCls('x-invalid')).toBe(false);
        });

        it("should validate using a data validator by string lookup, email", function() {
            create({
                validators: 'email'
            });
            field.setValue('me@nowhere');
            expect(el.hasCls('x-invalid')).toBe(true);
            field.setValue('me@nowhere.com');
            expect(el.hasCls('x-invalid')).toBe(false);
        });

        it("should remove invalid UI if clearInvalid called", function() {
            create({
                validators: /^[0-9]*$/
            });
            field.setValue('abc');
            expect(el.hasCls('x-invalid')).toBe(true);
            field.clearInvalid();
            expect(el.hasCls('x-invalid')).toBe(false);
        });

        it("should add invalid UI if markInvalid called", function() {
            create({
                validators: /^[0-9]*$/
            });
            field.setValue('123');
            expect(el.hasCls('x-invalid')).toBe(false);
            field.markInvalid('invalid');
            expect(el.hasCls('x-invalid')).toBe(true);
        });

        it("should report proper validity when isValid called", function() {
            create({
                validators: /^[0-9]*$/
            });
            field.setValue('abc');
            expect(field.isValid()).toBe(false);
            field.clearInvalid();
            expect(field.isValid()).toBe(true);
            field.validate();
            expect(field.isValid()).toBe(false);
        });

        it("should report proper validity when isValid called after validate is called", function() {
            create({
                validators: /^[0-9]*$/
            });
            field.setValue('abc');
            expect(field.isValid()).toBe(false);
            field.clearInvalid();
            expect(field.isValid()).toBe(true);
            field.validate();
            expect(field.isValid()).toBe(false);
        });

        it("should ignore validators of the bound record if modelValidation is false", function() {
            var Model = Ext.define('Model-' + this.getId(), {
                extend: 'Ext.data.Model',
                fields: [{
                    name: 'bar',
                    validators: {
                        type: 'format',
                        matcher: /^[0-9]*$/
                    }
                }]
            });

            ct = Ext.create({
                xtype: 'container',
                renderTo: document.body,
                modelValidation: false,
                viewModel: {
                    links: {
                        foo: {
                            type: Model,
                            create: true
                        }
                    }
                },
                items: [{
                    xtype: 'textfield',
                    bind: '{foo.bar}'
                }]
            });

            field = ct.child(0);

            var vm = ct.getViewModel();
            vm.notify();

            expect(field.isValid()).toBeTruthy();
            vm.get('foo').set('bar', 'abc');
            vm.notify();
            expect(field.isValid()).toBeTruthy();

            Ext.undefine(Model.$className);
        });

        it("should use validators of the bound record if modelValidation is true", function() {
            var Model = Ext.define('Model-' + this.getId(), {
                extend: 'Ext.data.Model',
                fields: [{
                    name: 'bar',
                    validators: {
                        type: 'format',
                        matcher: /^[0-9]*$/
                    }
                }]
            });

            ct = Ext.create({
                xtype: 'container',
                renderTo: document.body,
                modelValidation: true,
                viewModel: {
                    links: {
                        foo: {
                            type: Model,
                            create: true
                        }
                    }
                },
                items: [{
                    xtype: 'textfield',
                    bind: '{foo.bar}'
                }]
            });

            field = ct.child(0);

            var vm = ct.getViewModel();
            vm.notify();

            expect(field.isValid()).toBeTruthy();
            vm.get('foo').set('bar', 'abc');
            vm.notify();
            expect(field.isValid()).toBeFalsy();
            vm.get('foo').set('bar', '123');
            vm.notify();
            expect(field.isValid()).toBeTruthy();

            Ext.undefine(Model.$className);
        });
    });

    describe("events", function() {
        describe("change", function() {
            it("should fire when you change the value from null", function() {
                create();

                var fired = false;

                field.on('change', function() {
                    fired = true;
                }, this);

                field.setValue('test');

                expect(fired).toBeTruthy();
            });

            it("should fire when you change the value", function() {
                create({
                    value: 'test'
                });

                var fired = false;

                field.on('change', function() {
                    fired = true;
                }, this);

                field.setValue('test2');

                expect(fired).toBeTruthy();
            });

            it("should not fire when you change the value", function() {
                create({
                    value: 'test'
                });

                var fired = false;

                field.on('change', function() {
                    fired = true;
                }, this);

                field.setValue('test');

                expect(fired).toBeFalsy();
            });
        });
    });

    describe("configurations", function() {
        describe('clearable', function () {
            it('should respond properly to the clear trigger', function () {
                create({
                    clearable: true
                });

                var trigger = field.getTriggers().clear;

                field.setValue('hello');
                expect(trigger.isVisible()).toBe(true);
                Ext.testHelper.tap(trigger.element);

                var v = field.getValue();
                expect(v).toBe('');
                expect(trigger.isVisible()).toBe(false);
            });

            it("should be able to clear while focused and tapping the clear button", function() {
                create({
                    clearable: true
                });
                field.setValue('foo');
                jasmine.focusAndWait(field);
                runs(function() {
                    Ext.testHelper.tap(field.getTriggers().clear.element);
                    expect(field.getValue()).toBe('');
                    expect(field.inputElement.dom.value).toBe('');
                });
            });
        });

        describe("name", function() {
            var defaultConfig = {
                name: 'myname'
            };

            describe("configuration", function() {
                it("should add the name attribute to the inputEl", function() {
                    create(defaultConfig);

                    expect(field.inputElement).toHaveAttribute('name', 'myname');
                });
            });

            describe("method", function() {
                describe("setting", function() {
                    describe("before render", function() {
                        it("should add the name attribute to the inputEl", function() {
                            create();
                            field.setName('myname');

                            expect(field.inputElement).toHaveAttribute('name', 'myname');
                        });
                    });

                    describe("after render", function() {
                        it("should add the name attribute to the inputEl", function() {
                            create();
                            field.setName('myname');

                            expect(field.inputElement).toHaveAttribute('name', 'myname');
                        });
                    });
                });


                describe("removing", function() {
                    describe("before render", function() {
                        it("should remove the name attribute from the inputEl", function() {
                            create(defaultConfig);
                            field.setName(null);

                            expect(field.inputElement).not.toHaveAttribute('name');
                        });

                    });

                    describe("after render", function() {
                        it("should remove the name attribute from the inputEl", function() {
                            create(defaultConfig);
                            field.setName(null);

                            expect(field.inputElement).not.toHaveAttribute('name');
                        });
                    });
                });
            });
        });

        // TODO https://sencha.jira.com/browse/EXT-205
        describe("tabIndex", function() {
           var defaultConfig = {
               tabIndex: 10
           };

           describe("configuration", function() {
                it("should add the tabindex attribute to the inputEl", function() {
                    create(defaultConfig);

                    expect(field.inputElement).toHaveAttribute('tabindex', 10);
                });
            });

            describe("method", function() {
                describe("setting", function() {
                    describe("before render", function() {
                        it("should add the tabindex attribute to the inputEl", function() {
                            create();
                            field.setTabIndex(10);

                            expect(field.inputElement).toHaveAttribute('tabindex', 10);
                        });
                    });

                    describe("after render", function() {
                        it("should add the tabindex attribute to the inputEl", function() {
                            create();
                            field.setTabIndex(10);

                            expect(field.inputElement).toHaveAttribute('tabindex', 10);
                        });
                    });
                });


                describe("removing", function() {
                    describe("before render", function() {
                        it("should remove the tabindex attribute from the inputEl", function() {
                            create(defaultConfig);
                            field.setTabIndex(null);
                            waits(10);

                            runs(function() {
                                expect(field.inputElement).not.toHaveAttribute('tabindex');
                            });
                        });

                    });

                    describe("after render", function() {
                        it("should remove the tabindex attribute from the inputEl", function() {
                            create(defaultConfig);
                            field.setTabIndex(null);

                                expect(field.inputElement).not.toHaveAttribute('tabindex');
                        });
                    });
                });
            });
        });

        describe("maxLength", function() {
            var defaultConfig = {
                maxLength: 10
            };

            describe("configuration", function() {
                it("should add the maxlength attribute to the inputEl", function() {
                    create(defaultConfig);

                    expect(field.inputElement).toHaveAttribute('maxlength', '10');
                });
            });

            describe("method", function() {
                describe("setting", function() {
                    describe("before render", function() {
                        it("should add the maxlength attribute to the inputEl", function() {
                            create();
                            field.setMaxLength(10);

                            expect(field.inputElement).toHaveAttribute('maxlength', '10');
                        });
                    });

                    describe("after render", function() {
                        it("should add the maxlength attribute to the inputEl", function() {
                            create();
                            field.setMaxLength(10);

                            expect(field.inputElement).toHaveAttribute('maxlength', '10');
                        });
                    });
                });


                describe("removing", function() {
                    describe("before render", function() {
                        it("should remove the maxlength attribute from the inputEl", function() {
                            create(defaultConfig);
                            field.setMaxLength(null);

                            expect(field.inputElement).not.toHaveAttribute('maxlength');
                        });
                    });

                    describe("after render", function() {
                        it("should remove the maxlength attribute from the inputEl", function() {
                            create(defaultConfig);
                            field.setMaxLength(null);

                            expect(field.inputElement).not.toHaveAttribute('maxlength');
                        });
                    });
                });
            });
        });

        describe('placeholder', function() {
            var defaultConfig = {};

            defaultConfig.placeholder = 'foo';

            describe("configuration", function() {
                it("should add the placeholder attribute to the inputEl", function() {
                    create(defaultConfig);

                    // N.B. Regardless of the config name, attribute name is lowercase
                    expect(field.inputElement).toHaveAttribute('placeholder', 'foo');
                });
            });

            describe("method", function() {
                describe("setting", function() {
                    describe("before render", function() {
                        it("should add the placeholder attribute to the inputEl", function() {
                            create();

                            field.setPlaceholder('bar');

                            expect(field.inputElement).toHaveAttribute('placeholder', 'bar');
                        });
                    });

                    describe("after render", function() {
                        it("should add the placeholder attribute to the inputEl", function() {
                            create();

                            field.setPlaceholder('baz');

                            expect(field.inputElement).toHaveAttribute('placeholder', 'baz');
                        });
                    });
                });

                describe("removing", function() {
                    describe("before render", function() {
                        it("should remove the placeholder attribute from the inputEl", function() {
                            create(defaultConfig);

                            field.setPlaceholder(null);

                            expect(field.inputElement).not.toHaveAttribute('placeholder');
                        });

                    });

                    describe("after render", function() {
                        it("should remove the placeholder attribute from the inputEl", function() {
                            create(defaultConfig);

                            field.setPlaceholder(null);

                            expect(field.inputElement).not.toHaveAttribute('placeholder');
                        });
                    });
                });
            });
        });

        describe("labelAlign", function() {
            describe("placeholder", function() {
                var translateXRe = /translateX\((\d+)px\)/,
                    translateYRe = /translateY\((\d+)px\)/;

                function getTranslate(type) {
                    var style = field.labelElement.dom.style.transform,
                        key = 'translate' + type.toUpperCase(),
                        re = type === 'x' ? translateXRe : translateYRe,
                        ret = 0,
                        match;

                    if (style.indexOf(key) > -1) {
                        match = style.match(re);
                        if (match) {
                            ret = parseInt(match[1], 10);
                        }
                    }
                    return ret;
                }

                function expectTranslate(x, y) {
                    expect(getTranslate('x')).toBe(x);
                    expect(getTranslate('y')).toBe(y);
                }
                describe("without initial value", function() {
                    beforeEach(function() {
                        create({
                            labelAlign: 'placeholder',
                            label: 'foo'
                        });
                    });

                    it("should have the label inside the field initially", function() {
                        expectTranslate(7, 24);
                    });

                    it("should move the label out if a value is set", function() {
                        field.setValue('test');

                        waitsFor(function() {
                            return getTranslate('x') === 0 && getTranslate('y') === 0;
                        }, 2000);

                        runs(function() {
                            expect(field.labelTextElement.dom.innerHTML).toBe('foo');
                        });
                    });
                });

                describe("with initial value", function() {
                    beforeEach(function() {
                        create({
                            labelAlign: 'placeholder',
                            label: 'foo',
                            value: 'bar'
                        });
                    });

                    it("should have the label outside the field", function() {
                        expectTranslate(0, 0);
                    });

                    it("should hide the label and set the label as placeholder when clearing the field's value", function() {
                        field.setValue(null);

                        waitsFor(function() {
                            return getTranslate('x') === 7 && getTranslate('y') === 24;
                        }, 2000);

                        runs(function() {
                            expectTranslate(7, 24);
                        });
                    });
                });

                describe("with placeholder", function() {
                    var button;

                    afterEach(function() {
                        button = Ext.destroy(button);
                    });

                    describe("no required", function() {
                        beforeEach(function() {
                            create({
                                placeholder: 'throbbe',
                                labelAlign: 'placeholder',
                                label: 'foo'
                            });

                            button = new Ext.Button({
                                renderTo: document.body,
                                text: 'button'
                            });
                        });

                        it("should have the label inside the field initially", function() {
                        expectTranslate(7, 24);
                    });

                        it("should display placeholder when focused", function() {
                            focusAndWait(field);

                            waitsFor(function() {
                                return getTranslate('x') === 0 && getTranslate('y') === 0;
                            });

                            runs(function() {
                                expect(field.inputElement).toHaveAttr('placeholder', 'throbbe');
                            });
                        });

                        it("should display external label when value is defined", function() {
                            field.setValue('zingbong');

                            waitsFor(function() {
                                return getTranslate('x') === 0 && getTranslate('y') === 0;
                            });

                            runs(function() {
                                expectTranslate(0, 0);
                            });
                        });
                    });

                    describe("with required", function() {
                        beforeEach(function() {
                            create({
                                placeholder: 'throbbe',
                                labelAlign: 'placeholder',
                                label: 'blerg',
                                required: true
                            });

                            button = new Ext.Button({
                                renderTo: document.body,
                                text: 'button'
                            });
                        });

                        it("should not append required indicator to placeholder", function() {
                            focusAndWait(field);

                            runs(function() {
                                expect(field.inputElement).toHaveAttr('placeholder', 'throbbe');
                            });
                        });
                    });
                });
            });
        });

        describe("autoComplete", function() {
            describe("using value 'on'", function() {
                var defaultConfig = {
                    autoComplete: 'on'
                };

                describe("configuration", function() {
                    it("should add the autocomplete attribute to the inputEl", function() {
                        create(defaultConfig);

                        expect(field.inputElement).toHaveAttribute('autocomplete', 'on');
                    });
                });

                describe("method", function() {
                    describe("setting", function() {
                        describe("before render", function() {
                            it("should add the autocomplete attribute to the inputEl", function() {
                                create();
                                field.setAutoComplete('on');

                                expect(field.inputElement).toHaveAttribute('autocomplete', 'on');
                            });
                        });

                        describe("after render", function() {
                            it("should add the autocomplete attribute to the inputEl", function() {
                                create();
                                field.setAutoComplete('on');

                                expect(field.inputElement).toHaveAttribute('autocomplete', 'on');
                            });
                        });
                    });


                    describe("removing", function() {
                        describe("before render", function() {
                            it("should remove the autocomplete attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoComplete(null);

                                expect(field.inputElement).toHaveAttribute('autocomplete', 'off');
                            });

                        });

                        describe("after render", function() {
                            it("should remove the autocomplete attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoComplete(null);
                                
                                expect(field.inputElement).toHaveAttribute('autocomplete', 'off');
                            });
                        });
                    });
                });
            });

            describe("using value true", function() {
                var defaultConfig = {
                    autoComplete: true
                };

                describe("configuration", function() {
                    it("should add the autocomplete attribute to the inputEl", function() {
                        create(defaultConfig);

                        expect(field.inputElement).toHaveAttribute('autocomplete', 'on');
                    });
                });

                describe("method", function() {
                    describe("setting", function() {
                        describe("before render", function() {
                            it("should add the autocomplete attribute to the inputEl", function() {
                                create();
                                field.setAutoComplete(true);

                                expect(field.inputElement).toHaveAttribute('autocomplete', 'on');
                            });
                        });

                        describe("after render", function() {
                            it("should add the autocomplete attribute to the inputEl", function() {
                                create();
                                field.setAutoComplete(true);

                                expect(field.inputElement).toHaveAttribute('autocomplete', 'on');
                            });
                        });
                    });


                    describe("removing", function() {
                        describe("before render", function() {
                            it("should remove the autocomplete attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoComplete(null);

                                expect(field.inputElement).toHaveAttribute('autocomplete', 'off');
                            });

                        });

                        describe("after render", function() {
                            it("should remove the autocomplete attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoComplete(null);

                                expect(field.inputElement).toHaveAttribute('autocomplete', 'off');
                            });
                        });
                    });
                });
            });

            describe("using value 'off'", function() {
                var defaultConfig = {
                    autoComplete: 'off'
                };

                describe("configuration", function() {
                    it("should add the autocomplete attribute to the inputEl", function() {
                        create(defaultConfig);

                        expect(field.inputElement).toHaveAttribute('autocomplete', 'off');
                    });
                });

                describe("method", function() {
                    describe("setting", function() {
                        describe("before render", function() {
                            it("should add the autocomplete attribute to the inputEl", function() {
                                create();
                                field.setAutoComplete('off');

                                expect(field.inputElement).toHaveAttribute('autocomplete', 'off');
                            });
                        });
                        describe("after render", function() {
                            it("should add the autocomplete attribute to the inputEl", function() {
                                create();
                                field.setAutoComplete('off');

                                expect(field.inputElement).toHaveAttribute('autocomplete', 'off');
                            });
                        });
                    });
                    describe("removing", function() {
                        describe("before render", function() {
                            it("should remove the autocomplete attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoComplete(null);

                                expect(field.inputElement).toHaveAttribute('autocomplete', 'off');
                            });

                        });
                        describe("after render", function() {
                            it("should remove the autocomplete attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoComplete(null);

                                expect(field.inputElement).toHaveAttribute('autocomplete', 'off');
                            });
                        });
                    });
                });
            });

            describe("using value false", function() {
                var defaultConfig = {
                    autoComplete: false
                };

                describe("configuration", function() {
                    it("should add the autocomplete attribute to the inputEl", function() {
                        create(defaultConfig);

                        expect(field.inputElement).toHaveAttribute('autocomplete', 'off');
                    });
                });

                describe("method", function() {
                    describe("setting", function() {
                        describe("before render", function() {
                            it("should add the autocomplete attribute to the inputEl", function() {
                                create();
                                field.setAutoComplete(false);

                                expect(field.inputElement).toHaveAttribute('autocomplete', 'off');
                            });
                        });

                        describe("after render", function() {
                            it("should add the autocomplete attribute to the inputEl", function() {
                                create();
                                field.setAutoComplete(false);

                                expect(field.inputElement).toHaveAttribute('autocomplete', 'off');
                            });
                        });
                    });


                    describe("removing", function() {
                        describe("before render", function() {
                            it("should remove the autocomplete attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoComplete(null);

                                expect(field.inputElement).toHaveAttribute('autocomplete', 'off');
                            });

                        });

                        describe("after render", function() {
                            it("should remove the autocomplete attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoComplete(null);

                                expect(field.inputElement).toHaveAttribute('autocomplete', 'off');
                            });
                        });
                    });
                });
            });
        });

        describe("autoCapitalize", function() {
            describe("using value 'on'", function() {
                var defaultConfig = {
                    autoCapitalize: 'on'
                };

                describe("configuration", function() {
                    it("should add the autocapitalize attribute to the inputEl", function() {
                        create(defaultConfig);

                        expect(field.inputElement).toHaveAttribute('autocapitalize', 'on');
                    });
                });

                describe("method", function() {
                    describe("setting", function() {
                        describe("before render", function() {
                            it("should add the autocapitalize attribute to the inputEl", function() {
                                create();
                                field.setAutoCapitalize('on');

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'on');
                            });
                        });

                        describe("after render", function() {
                            it("should add the autocapitalize attribute to the inputEl", function() {
                                create();
                                field.setAutoCapitalize('on');

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'on');
                            });
                        });
                    });


                    describe("removing", function() {
                        describe("before render", function() {
                            it("should remove the autocapitalize attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCapitalize(null);

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'off');
                            });

                        });

                        describe("after render", function() {
                            it("should remove the autocapitalize attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCapitalize(null);

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'off');
                            });
                        });
                    });
                });
            });

            describe("using value true", function() {
                var defaultConfig = {
                    autoCapitalize: true
                };

                describe("configuration", function() {
                    it("should add the autocapitalize attribute to the inputEl", function() {
                        create(defaultConfig);

                        expect(field.inputElement).toHaveAttribute('autocapitalize', 'on');
                    });
                });

                describe("method", function() {
                    describe("setting", function() {
                        describe("before render", function() {
                            it("should add the autocapitalize attribute to the inputEl", function() {
                                create();
                                field.setAutoCapitalize(true);

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'on');
                            });
                        });

                        describe("after render", function() {
                            it("should add the autocapitalize attribute to the inputEl", function() {
                                create();
                                field.setAutoCapitalize(true);

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'on');
                            });
                        });
                    });


                    describe("removing", function() {
                        describe("before render", function() {
                            it("should remove the autocapitalize attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCapitalize(null);

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'off');
                            });

                        });

                        describe("after render", function() {
                            it("should remove the autocapitalize attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCapitalize(null);

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'off');
                            });
                        });
                    });
                });
            });

            describe("using value 'off'", function() {
                var defaultConfig = {
                    autoCapitalize: 'off'
                };

                describe("configuration", function() {
                    it("should add the autocapitalize attribute to the inputEl", function() {
                        create(defaultConfig);

                        expect(field.inputElement).toHaveAttribute('autocapitalize', 'off');
                    });
                });

                describe("method", function() {
                    describe("setting", function() {
                        describe("before render", function() {
                            it("should add the autocapitalize attribute to the inputEl", function() {
                                create();
                                field.setAutoCapitalize('off');

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'off');
                            });
                        });
                        
                        describe("after render", function() {
                            it("should add the autocapitalize attribute to the inputEl", function() {
                                create();
                                field.setAutoCapitalize('off');

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'off');
                            });
                        });
                    });
                    
                    describe("removing", function() {
                        describe("before render", function() {
                            it("should remove the autocapitalize attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCapitalize(null);

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'off');
                            });
                        });
                        
                        describe("after render", function() {
                            it("should remove the autocapitalize attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCapitalize(null);

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'off');
                            });
                        });
                    });
                });
            });

            describe("using value false", function() {
                var defaultConfig = {
                    autoCapitalize: false
                };

                describe("configuration", function() {
                    it("should add the autocapitalize attribute to the inputEl", function() {
                        create(defaultConfig);

                        expect(field.inputElement).toHaveAttribute('autocapitalize', 'off');
                    });
                });

                describe("method", function() {
                    describe("setting", function() {
                        describe("before render", function() {
                            it("should add the autocapitalize attribute to the inputEl", function() {
                                create();
                                field.setAutoCapitalize(false);

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'off');
                            });
                        });

                        describe("after render", function() {
                            it("should add the autocapitalize attribute to the inputEl", function() {
                                create();
                                field.setAutoCapitalize(false);

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'off');
                            });
                        });
                    });

                    describe("removing", function() {
                        describe("before render", function() {
                            it("should remove the autocapitalize attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCapitalize(null);

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'off');
                            });
                        });

                        describe("after render", function() {
                            it("should remove the autocapitalize attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCapitalize(null);

                                expect(field.inputElement).toHaveAttribute('autocapitalize', 'off');
                            });
                        });
                    });
                });
            });
        });


        describe("autoCorrect", function() {
            describe("using value 'on'", function() {
                var defaultConfig = {
                    autoCorrect: 'on'
                };

                describe("configuration", function() {
                    it("should add the autocorrect attribute to the inputEl", function() {
                        create(defaultConfig);

                        expect(field.inputElement).toHaveAttribute('autocorrect', 'on');
                    });
                });

                describe("method", function() {
                    describe("setting", function() {
                        describe("before render", function() {
                            it("should add the autocorrect attribute to the inputEl", function() {
                                create();
                                field.setAutoCorrect('on');

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'on');
                            });
                        });

                        describe("after render", function() {
                            it("should add the autocorrect attribute to the inputEl", function() {
                                create();
                                field.setAutoCorrect('on');

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'on');
                            });
                        });
                    });

                    describe("removing", function() {
                        describe("before render", function() {
                            it("should remove the autocorrect attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCorrect(null);

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'off');
                            });
                        });

                        describe("after render", function() {
                            it("should remove the autocorrect attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCorrect(null);

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'off');
                            });
                        });
                    });
                });
            });

            describe("using value true", function() {
                var defaultConfig = {
                    autoCorrect: true
                };

                describe("configuration", function() {
                    it("should add the autocorrect attribute to the inputEl", function() {
                        create(defaultConfig);

                        expect(field.inputElement).toHaveAttribute('autocorrect', 'on');
                    });
                });

                describe("method", function() {
                    describe("setting", function() {
                        describe("before render", function() {
                            it("should add the autocorrect attribute to the inputEl", function() {
                                create();
                                field.setAutoCorrect(true);

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'on');
                            });
                        });

                        describe("after render", function() {
                            it("should add the autocorrect attribute to the inputEl", function() {
                                create();
                                field.setAutoCorrect(true);

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'on');
                            });
                        });
                    });

                    describe("removing", function() {
                        describe("before render", function() {
                            it("should remove the autocorrect attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCorrect(null);

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'off');
                            });

                        });

                        describe("after render", function() {
                            it("should remove the autocorrect attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCorrect(null);

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'off');
                            });
                        });
                    });
                });
            });

            describe("using value 'off'", function() {
                var defaultConfig = {
                    autoCorrect: 'off'
                };

                describe("configuration", function() {
                    it("should add the autocorrect attribute to the inputEl", function() {
                        create(defaultConfig);

                        expect(field.inputElement).toHaveAttribute('autocorrect', 'off');
                    });
                });

                describe("method", function() {
                    describe("setting", function() {
                        describe("before render", function() {
                            it("should add the autocorrect attribute to the inputEl", function() {
                                create();
                                field.setAutoCorrect('off');

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'off');
                            });
                        });
                        describe("after render", function() {
                            it("should add the autocorrect attribute to the inputEl", function() {
                                create();
                                field.setAutoCorrect('off');

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'off');
                            });
                        });
                    });
                    
                    describe("removing", function() {
                        describe("before render", function() {
                            it("should remove the autocorrect attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCorrect(null);

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'off');
                            });

                        });
                        describe("after render", function() {
                            it("should remove the autocorrect attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCorrect(null);

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'off');
                            });
                        });
                    });
                });
            });

            describe("using value false", function() {
                var defaultConfig = {
                    autoCorrect: false
                };

                describe("configuration", function() {
                    it("should add the autocorrect attribute to the inputEl", function() {
                        create(defaultConfig);

                        expect(field.inputElement).toHaveAttribute('autocorrect', 'off');
                    });
                });

                describe("method", function() {
                    describe("setting", function() {
                        describe("before render", function() {
                            it("should add the autocorrect attribute to the inputEl", function() {
                                create();
                                field.setAutoCorrect(false);

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'off');
                            });
                        });

                        describe("after render", function() {
                            it("should add the autocorrect attribute to the inputEl", function() {
                                create();
                                field.setAutoCorrect(false);

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'off');
                            });
                        });
                    });

                    describe("removing", function() {
                        describe("before render", function() {
                            it("should remove the autocorrect attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCorrect(null);

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'off');
                            });
                        });

                        describe("after render", function() {
                            it("should remove the autocorrect attribute from the inputEl", function() {
                                create(defaultConfig);
                                field.setAutoCorrect(null);

                                expect(field.inputElement).toHaveAttribute('autocorrect', 'off');
                            });
                        });
                    });
                });
            });
        });
    });

    describe("methods", function() {
        describe("reset", function() {
            beforeEach(function () {
                create({
                    value: 'foo'
                });
                field.setValue('foobar');
            });

            it("should update the input field element", function () {
                field.reset();

                expect(field.inputElement.dom.value).toBe('foo');
            });

            it("should synchronize the value of the component with the field", function () {
                field.reset();

                expect(field.getValue()).toBe('foo');
            });

            it('should keep caret position', function () {
                field.focus();

                field.setCaretPos(1);

                expect(field.getCaretPos()).toBe(1);

                field.setValue('foobar');

                expect(field.getCaretPos()).toBe(1);
            });
        });
    });

    describe("triggers", function() {
        var triggers;

        function makeField(cfg) {
            field = Ext.create(Ext.merge({
                xtype: 'textfield',
                renderTo: document.body
            }, cfg));

            triggers = field.getTriggers();
        }

        it("should initialize with triggers", function() {
            makeField({
                triggers: {
                    foo: {
                        cls: 'foo'
                    },
                    bar: {
                        cls: 'bar'
                    }
                }
            });

            expect(Object.keys(triggers).length).toBe(3); // including clear trigger

            expect(triggers.foo instanceof Ext.field.trigger.Trigger).toBe(true);
            expect(triggers.foo.element).toHaveCls('foo');
            expect(triggers.bar instanceof Ext.field.trigger.Trigger).toBe(true);
            expect(triggers.bar.element).toHaveCls('bar');
        });

        it("should have a clear trigger", function() {
            makeField();

            expect(Object.keys(triggers).length).toBe(1);
            expect(triggers.clear instanceof Ext.field.trigger.Clear).toBe(true);
        });

        it("should not have a clear trigger if clearable is false", function() {
            makeField({
                clearable: false
            });

            expect(Object.keys(triggers).length).toBe(0);
        });

        it("should add a clear trigger when clearable is set to true after instantiation", function() {
            makeField({
                clearable: false
            });

            field.setClearable(true);

            expect(Object.keys(triggers).length).toBe(1);
            expect(triggers.clear instanceof Ext.field.trigger.Clear).toBe(true);
        });

        it("should remove the clear trigger when clearable is set to false after instantiation", function() {
            makeField();

            var clearTrigger = triggers.clear;

            field.setClearable(false);

            expect(Object.keys(triggers).length).toBe(0);
            expect(clearTrigger.destroyed).toBe(true);
        });

        it("should add a trigger using a config object", function() {
            makeField({
                triggers: {
                    foo: {
                        cls: 'foo'
                    }
                }
            });

            field.addTrigger('bar', {
                cls: 'bar'
            });

            expect(triggers.foo instanceof Ext.field.trigger.Trigger).toBe(true);
            expect(triggers.foo.element).toHaveCls('foo');
            expect(triggers.bar instanceof Ext.field.trigger.Trigger).toBe(true);
            expect(triggers.bar.element).toHaveCls('bar');
        });

        it("should add a trigger by type", function() {
            makeField({
                triggers: {
                    foo: {
                        cls: 'foo'
                    }
                }
            });

            field.addTrigger('bar', {
                type: 'expand',
                cls: 'bar'
            });

            expect(triggers.foo instanceof Ext.field.trigger.Trigger).toBe(true);
            expect(triggers.foo.element).toHaveCls('foo');
            expect(triggers.bar instanceof Ext.field.trigger.Expand).toBe(true);
            expect(triggers.bar.element).toHaveCls('bar');
        });

        it("should add a trigger by type string (no config object)", function() {
            makeField({
                triggers: {
                    foo: {
                        cls: 'foo'
                    }
                }
            });

            field.addTrigger('bar', 'expand');

            expect(triggers.foo instanceof Ext.field.trigger.Trigger).toBe(true);
            expect(triggers.foo.element).toHaveCls('foo');
            expect(triggers.bar instanceof Ext.field.trigger.Expand).toBe(true);
        });

        it("should add an already instanced trigger", function() {
            makeField({
                triggers: {
                    foo: {
                        cls: 'foo'
                    }
                }
            });

            var barTrigger = new Ext.field.trigger.Trigger({
                cls: 'bar'
            });

            field.addTrigger('bar', barTrigger);

            expect(triggers.foo instanceof Ext.field.trigger.Trigger).toBe(true);
            expect(triggers.foo.element).toHaveCls('foo');
            expect(triggers.bar).toBe(barTrigger);
        });

        it("should not allow addition of a trigger with the same key as an existing trigger", function() {
            makeField({
                triggers: {
                    foo: {
                        cls: 'foo'
                    }
                }
            });

            expect(function() {
                field.addTrigger('foo', {
                    cls: 'bar'
                });
            }).toThrow('Trigger with name "foo" already exists.');
        });

        it("should add a trigger when there are no existing triggers", function() {
            makeField({
                clearable: false
            });

            expect(Object.keys(triggers).length).toBe(0);

            field.addTrigger('bar', {
                cls: 'bar'
            });

            expect(Object.keys(triggers).length).toBe(1);
            expect(triggers.bar instanceof Ext.field.trigger.Trigger).toBe(true);
            expect(triggers.bar.element).toHaveCls('bar');
        });

        it("should throw an error if the first argument to addTrigger() is not a string", function() {
            makeField();

            expect(function() {
                field.addTrigger({}, {});
            }).toThrow('Cannot add trigger. Key must be a string');
        });

        it("should throw an error if the second argument to addTrigger() is not an object", function() {
            makeField();

            expect(function() {
                field.addTrigger('foo');
            }).toThrow('Cannot add trigger "foo". A trigger config or instance is required.');
        });

        it("should remove a trigger by key", function() {
            makeField({
                clearable: false,
                triggers: {
                    foo: {
                        cls: 'foo'
                    }
                }
            });

            var trigger = triggers.foo;
            var triggerEl = trigger.el.dom;

            field.removeTrigger('foo');
            expect(Object.keys(triggers).length).toBe(0);
            expect(triggerEl.parentNode).toBe(null);
            expect(trigger.destroyed).toBe(true);
        });

        it("should remove a trigger by reference", function() {
            makeField({
                triggers: {
                    foo: {
                        cls: 'foo'
                    }
                }
            });

            var trigger = triggers.foo;
            var triggerEl = trigger.el.dom;

            field.removeTrigger(triggers.foo);
            expect(Object.keys(triggers).length).toBe(1); // still has a clear icon
            expect(triggerEl.parentNode).toBe(null);
            expect(trigger.destroyed).toBe(true);
        });

        it("should prevent destruction of the trigger", function() {
            makeField({
                clearable: false,
                triggers: {
                    foo: {
                        cls: 'foo'
                    }
                }
            });

            var trigger = triggers.foo;

            field.removeTrigger(triggers.foo, false);
            expect(Object.keys(triggers).length).toBe(0);
            expect(trigger.el.dom.parentNode).toBe(null);
            expect(trigger.destroyed).toBe(false);

            trigger.destroy();
        });

        it("should throw an error if non-existent key is passed to removeTrigger", function() {
            makeField({
                triggers: {
                    foo: {
                        cls: 'foo'
                    }
                }
            });

            expect(function() {
                field.removeTrigger('bar');
            }).toThrow('Cannot remove trigger. Trigger with name "bar" not found.');
        });

        it("should throw an error if non-existent trigger is passed to removeTrigger()", function() {
            makeField({
                triggers: {
                    foo: {
                        cls: 'foo'
                    }
                }
            });

            var trigger = new Ext.field.trigger.Trigger();

            expect(function() {
                field.removeTrigger(trigger);
            }).toThrow('Trigger not found.');

            trigger.destroy();
        });

        it("should add triggers using setTrigger()", function() {
            makeField({
                triggers: {
                    foo: {
                        cls: 'foo'
                    }
                }
            });

            field.setTriggers({
                bar: {
                    cls: 'bar'
                },
                baz: {
                    type: 'expand'
                }
            });

            expect(Object.keys(triggers).length).toBe(4);
            expect(triggers.foo.el).toHaveCls('foo');
            expect(triggers.bar.el).toHaveCls('bar');
            expect(triggers.baz instanceof Ext.field.trigger.Expand).toBe(true);
        });

        it("should remove triggers using setTrigger()", function() {

        });

        it("should replace a trigger using setTrigger()", function() {
            makeField({
                triggers: {
                    foo: {
                        cls: 'foo'
                    }
                }
            });

            var originalFoo = triggers.foo;
            var originalFooEl = triggers.foo.el.dom;

            field.setTriggers({
                foo: {
                    cls: 'bar'
                }
            });

            expect(Object.keys(triggers).length).toBe(2); // including clear trigger
            expect(triggers.foo).not.toBe(originalFoo);
            expect(triggers.foo.el).toHaveCls('bar');
            expect(originalFooEl.parentNode).toBe(null);

            expect(originalFoo.destroyed).toBe(true);
        });

        it("should align triggers to the right", function() {
            makeField({
                width: 200,
                triggers: {
                    foo: {
                        cls: 'foo'
                    },
                    bar: {
                        cls: 'bar'
                    }
                }
            });

            expect(field).toHaveLayout({
                el: {
                    w: 200
                },
                '.foo': { x: 155 },
                '.bar': { x: 177 }
            });
        });

        it("should align triggers to the left", function() {
            makeField({
                width: 200,
                triggers: {
                    foo: {
                        cls: 'foo',
                        side: 'left'
                    },
                    bar: {
                        cls: 'bar',
                        side: 'left'
                    }
                }
            });

            expect(field).toHaveLayout({
                el: {
                    w: 200
                },
                '.foo': { x: 1 },
                '.bar': { x: 23 }
            });
        });

        it("should group triggers", function() {
            makeField({
                width: 200,
                triggers: {
                    grp: {},
                    foo: {
                        cls: 'foo',
                        group: 'grp'
                    },
                    bar: {
                        cls: 'bar'
                    },
                    baz: {
                        cls: 'baz',
                        group: 'grp'
                    }
                }
            });

            expect(triggers.grp.getTriggers()[0].el).toHaveCls('foo');
            expect(triggers.grp.getTriggers()[1].el).toHaveCls('baz');

            expect(triggers.foo.el.dom.parentNode).toBe(triggers.grp.el.dom);
            expect(triggers.bar.el.dom.parentNode).not.toBe(triggers.grp.el.dom);
            expect(triggers.baz.el.dom.parentNode).toBe(triggers.grp.el.dom);
        });

        it("should add an iconCls to a trigger using initial config", function() {
            makeField({
                width: 200,
                triggers: {
                    foo: {
                        iconCls: 'fooIcon'
                    }
                }
            });

            expect(triggers.foo.iconElement).toHaveCls('fooIcon');
        });

        it("should set the iconCls of a trigger after instantiation", function() {
            makeField({
                width: 200,
                triggers: {
                    foo: {}
                }
            });

            triggers.foo.setIconCls('fooIcon');

            expect(triggers.foo.iconElement).toHaveCls('fooIcon');
        });

        describe("weight", function() {
            it("should sort left-aligned triggers by weight", function() {
                makeField({
                    width: 200,
                    triggers: {
                        a: {
                            cls: 'a',
                            weight: 3
                        },
                        b: {
                            cls: 'b',
                            weight: -2
                        },
                        c: {
                            cls: 'c' // default weight of 0
                        }
                    }
                });

                expect(field).toHaveLayout({
                    el: {
                        w: 200
                    },
                    '.a': { x: 177 },
                    '.b': { x: 133 },
                    '.c': { x: 155 }
                });
            });

            it("should sort right-aligned triggers by weight", function() {
                makeField({
                    width: 200,
                    triggers: {
                        a: {
                            cls: 'a',
                            weight: 3,
                            side: 'left'
                        },
                        b: {
                            cls: 'b',
                            weight: -2,
                            side: 'left'
                        },
                        c: {
                            cls: 'c', // default weight of 0
                            side: 'left'
                        }
                    }
                });

                expect(field).toHaveLayout({
                    el: {
                        w: 200
                    },
                    '.a': { x: 45 },
                    '.b': { x: 1 },
                    '.c': { x: 23 }
                });
            });

            it("should sort by weight within a group", function() {
                makeField({
                    width: 200,
                    triggers: {
                        grp: {},
                        foo: {
                            cls: 'foo',
                            group: 'grp',
                            weight: 5
                        },
                        bar: {
                            cls: 'bar'
                        },
                        baz: {
                            cls: 'baz',
                            group: 'grp',
                            weight: 4
                        }
                    }
                });

                expect(triggers.grp.getTriggers()[0].el).toHaveCls('baz');
                expect(triggers.grp.getTriggers()[1].el).toHaveCls('foo');
            });
        });

        describe("handler", function() {
            it("should not have the x-interactive cls when there is no handler", function() {
                makeField({
                    triggers: {
                        foo: {}
                    }
                });

                expect(triggers.foo.el).not.toHaveCls('x-interactive');
            });

            it("should have the x-interactive cls when there is a handler", function() {
                makeField({
                    triggers: {
                        foo: {
                            handler: function() {}
                        }
                    }
                });

                expect(triggers.foo.el).toHaveCls('x-interactive');
            });

            it("should call the handler when the trigger is clicked", function() {
                var spy = jasmine.createSpy('trigger tap spy');

                makeField({
                    triggers: {
                        foo: {
                            handler: spy
                        }
                    }
                });

                Ext.testHelper.tap(triggers.foo.el);

                waitsForSpy(spy);
            });

            it("should use the field as the default scope", function() {
                var spy = jasmine.createSpy('trigger tap spy');

                makeField({
                    triggers: {
                        foo: {
                            handler: spy
                        }
                    }
                });

                Ext.testHelper.tap(triggers.foo.el);

                waitsForSpy(spy);

                runs(function() {
                    expect(spy.mostRecentCall.scope).toBe(field);
                });
            });

            it("should call the handler using the specified scope", function() {
                var spy = jasmine.createSpy(),
                    scope = new Ext.Component();

                makeField({
                    triggers: {
                        foo: {
                            handler: spy,
                            scope: scope
                        }
                    }
                });

                Ext.testHelper.tap(triggers.foo.el);

                waitsForSpy(spy);

                runs(function() {
                    expect(spy.mostRecentCall.scope).toBe(scope);

                    scope.destroy();
                });
            });
        });
    });
});
