topSuite("Ext.field.Number", function() {
    var field;

    function createField(config) {
        if (field) {
            field.destroy();
        }

        config = Ext.apply({
        }, config);

        field = new Ext.field.Number(config);
    }

    function render(f) {
        f = f || field;

        if (f.getFloated()) {
            f.show();
        } else {
            f.render(Ext.getBody());
        }
    }

    afterEach(function() {
        if (field) {
            field.destroy();
        }
    });

    describe("configurations", function() {
        describe("minValue text", function() {
            var defaultConfig = {
                minValue: 10,
                inputType: 'text'
            };

            describe('setValue', function () {
                it('should use minValue if value below minValue', function () {
                    createField(defaultConfig);
                    render();

                    field.setValue(5);

                    expect(field.getValue()).toBe(5);
                    expect(field.inputElement.dom.value).toBe('5');
                    expect(field.validate()).not.toBe(true);
                });
            });
        });

        describe("minValue number", function() {
            var defaultConfig = {
                minValue: 10,
                inputType: 'number'
            };

            describe('setValue', function () {
                it('should use minValue if value below minValue', function () {
                    createField(defaultConfig);
                    render();

                    field.setValue(5);

                    expect(field.getValue()).toBe(5);
                    expect(field.inputElement.dom.value).toBe('5');
                    expect(field.validate()).not.toBe(true);
                });
            });

            it('should allow typing negative value if minValue is negative', function () {
                createField({
                    minValue: -10
                });
                render();

                // Allow setting to minvalid values for a type="number" field for the sake
                // of exercising the code.
                field.inputElement.dom.removeAttribute('type');

                Ext.testHelper.doTyping(field.inputElement, '-');

                expect(field.getValue()).toBe(null);
                expect(field.inputElement.dom.value).toBe('-');

                Ext.testHelper.doTyping(field.inputElement, '5');

                expect(field.getValue()).toBe(-5);
                expect(field.inputElement.dom.value).toBe('-5');
            });

            it('should not throw when typing into a selected value', function () {
                var errorSpy = spyOn(window, 'onerror');

                createField({
                    minValue: -10,
                    value: 3
                });
                render();
                focusAndWait(field);

                runs(function() {
                    Ext.testHelper.select(field.inputElement);

                    jasmine.fireKeyEvent(field.inputElement, 'keydown', Ext.event.Event.ONE);

                    expect(errorSpy).not.toHaveBeenCalled();
                });
            });

            it('should not enforce non-negative if minValue is not configured', function() {
                createField({
                    value: -123.45
                });
                expect(field.isValid()).toBe(true);
            });
        });

        describe("maxValue number", function() {
            var defaultConfig = {
                maxValue: 10,
                inputType: 'number'
            };

            describe('setValue', function () {
                it('should use maxValue if value above maxValue', function () {
                    createField(defaultConfig);
                    render();

                    field.setValue(20);

                    expect(field.getValue()).toBe(20);
                    expect(field.inputElement.dom.value).toBe('20');
                    expect(field.validate()).not.toBe(true);
                });
            });
        });

        describe("maxValue text", function() {
            var defaultConfig = {
                maxValue: 10,
                inputType: 'text'
            };

            describe('setValue', function () {
                it('should use maxValue if value above maxValue', function () {
                    createField(defaultConfig);
                    render();

                    field.setValue(20);

                    expect(field.getValue()).toBe(20);
                    expect(field.inputElement.dom.value).toBe('20');
                    expect(field.validate()).not.toBe(true);
                });
            });
        });
    });

    describe("getValue number", function() {
        describe("when value is null", function() {
            beforeEach(function() {
                createField({
                    inputType: 'number'
                });
            });

            it("should return null", function() {
                expect(field.getValue()).toBeNull();
            });
        });

        describe("when value is a number", function() {
            beforeEach(function() {
                createField({
                    value: 123,
                    inputType: 'number'
                });
            });

            it("should return 123", function() {
                expect(field.getValue()).toEqual(123);
            });
        });

        describe("when value is 0", function() {
            beforeEach(function() {
                createField({
                    value: 0,
                    inputType: 'number'
                });
            });

            it("should return 0", function() {
                expect(field.getValue()).toEqual(0);
            });
        });

        describe("when value is -123", function() {
            beforeEach(function() {
                createField({
                    value: -123,
                    inputType: 'number'
                });
            });

            it("should return -123", function() {
                expect(field.getValue()).toEqual(-123);
            });
        });

        describe("when value is a string", function() {
            beforeEach(function() {
                createField({
                    value: '123',
                    inputType: 'number'
                });
            });

            it("should return 123", function() {
                expect(field.getValue()).toEqual(123);
            });
        });
    });

    describe("getValue text", function() {
        describe("when value is null", function() {
            beforeEach(function() {
                createField({
                    inputType: 'text'
                });
            });

            it("should return null", function() {
                expect(field.getValue()).toBeNull();
            });
        });

        describe("when value is a number", function() {
            beforeEach(function() {
                createField({
                    value: 123,
                    inputType: 'text'
                });
            });

            it("should return 123", function() {
                expect(field.getValue()).toEqual(123);
            });
        });

        describe("when value is 0", function() {
            beforeEach(function() {
                createField({
                    value: 0,
                    inputType: 'text'
                });
            });

            it("should return 0", function() {
                expect(field.getValue()).toEqual(0);
            });
        });

        describe("when value is -123", function() {
            beforeEach(function() {
                createField({
                    value: -123,
                    inputType: 'text'
                });
            });

            it("should return -123", function() {
                expect(field.getValue()).toEqual(-123);
            });
        });

        describe("when value is a string", function() {
            beforeEach(function() {
                createField({
                    value: '123',
                    inputType: 'text'
                });
            });

            it("should return 123", function() {
                expect(field.getValue()).toEqual(123);
            });
        });
    });

    describe("setValue input type number", function() {
        describe("null value", function() {
            beforeEach(function() {
                createField({
                    inputType: 'number'
                });
            });

            describe("when value is a number", function() {
                it("should set the value to 123", function() {
                    field.setValue(123);
                    expect(field.getValue()).toEqual(123);
                });
            });

            describe("when value is a string", function() {
                it("should set the value to 123", function() {
                    field.setValue('123');
                    expect(field.getValue()).toEqual(123);
                });
            });

            describe("when value is a negative value", function() {
                it("should set the value to -123", function() {
                    field.setValue(-123);
                    expect(field.getValue()).toEqual(-123);
                });
            });

            describe("when value is a negative value as as tring", function() {
                it("should set the value to -123", function() {
                    field.setValue('-123');
                    expect(field.getValue()).toEqual(-123);
                });
            });

            describe("when value is 0", function() {
                it("should set the value to 0", function() {
                    field.setValue(0);
                    expect(field.getValue()).toEqual(0);
                });
            });

            describe("when value is 0 as string", function() {
                it("should set the value to 0", function() {
                    field.setValue('0');
                    expect(field.getValue()).toEqual(0);
                });
            });
        });
    });

    describe("setValue input type text", function() {
        describe("null value", function() {
            beforeEach(function() {
                createField({
                    inputType: 'text'
                });
            });

            describe("when value is a number", function() {
                it("should set the value to 123", function() {
                    field.setValue(123);
                    expect(field.getValue()).toEqual(123);
                });
            });

            describe("when value is a string", function() {
                it("should set the value to 123", function() {
                    field.setValue('123');
                    expect(field.getValue()).toEqual(123);
                });
            });

            describe("when value is a negative value", function() {
                it("should set the value to -123", function() {
                    field.setValue(-123);
                    expect(field.getValue()).toEqual(-123);
                });
            });

            describe("when value is a negative value as as tring", function() {
                it("should set the value to -123", function() {
                    field.setValue('-123');
                    expect(field.getValue()).toEqual(-123);
                });
            });

            describe("when value is 0", function() {
                it("should set the value to 0", function() {
                    field.setValue(0);
                    expect(field.getValue()).toEqual(0);
                });
            });

            describe("when value is 0 as string", function() {
                it("should set the value to 0", function() {
                    field.setValue('0');
                    expect(field.getValue()).toEqual(0);
                });
            });
        });
    });

    describe("decimals", function() {
        it("should round the value to configured decimal precision / number", function() {
            createField({
                inputType: 'number',
                decimals: 1
            });
            field.setValue(0.1 + 0.2);

            expect(field.inputElement.dom.value).toBe('0.3');
        });
        it("should round the value to configured decimal precision / text", function() {
            createField({
                inputType: 'text',
                decimals: 1
            });
            field.setValue(0.1 + 0.2);

            expect(field.inputElement.dom.value).toBe('0.3');
        });
    });

    describe("typing", function() {
        it("should allow a decimal point", function() {
            createField({
                minValue: -10
            });
            render();

            // Allow setting to minvalid values for a type="number" field for the sake
            // of exercising the code.
            field.inputElement.dom.removeAttribute('type');
            field.focus();

            Ext.testHelper.doTyping(field.inputElement, '1');

            expect(field.getValue()).toBe(1);
            expect(field.inputElement.dom.value).toBe('1');

            Ext.testHelper.doTyping(field.inputElement, '.');

            expect(field.getValue()).toBe(1);
            expect(field.inputElement.dom.value).toBe('1.');

            Ext.testHelper.doTyping(field.inputElement, '3');

            expect(field.getValue()).toBe(1.3);
            expect(field.inputElement.dom.value).toBe('1.3');
        });
    });

    describe("empty value", function() {
        it("should be able to clear the value", function() {
            createField({
                value: 100
            });

            // Simulate selecting the text and backspacing it out
            // Firing key events for backspace don't end up triggering
            // onInput
            field.inputElement.dom.value = '';
            field.onInput({});

            expect(field.getValue()).toBeNull();
            expect(field.inputElement.dom.value).toBe('');
        });
    });
});
