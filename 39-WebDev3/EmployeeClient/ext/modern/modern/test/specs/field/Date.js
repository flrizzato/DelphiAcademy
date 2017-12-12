topSuite("Ext.field.Date", [
    'Ext.viewport.Viewport',
    'Ext.data.validator.Date'
], function() {
    jasmine.usesViewport(); // setup in beforeAll, teardown in afterAll

    var today = Ext.Date.clearTime(new Date()),
        field;

    function makeField(cfg) {
        field = new Ext.field.Date(cfg);
        if (field.getFloated()) {
            field.show();
        } else {
            field.render(Ext.getBody());
        }
    }

    afterEach(function() {
        field = Ext.destroy(field);
    });

    describe("setValue", function() {
        it("should take accept a date object", function() {
            makeField();
            field.setValue(new Date(2010, 0, 1));
            expect(field.getValue()).toEqual(new Date(2010, 0, 1));
        });

        it("should accept a string that matches the dateFormat", function() {
            makeField({
                dateFormat: 'Y-m-d'
            });
            field.setValue('2010-01-01');
            expect(field.getValue()).toEqual(new Date(2010, 0, 1));
        });

        it("should accept an object containing the year, month, and day", function () {
           makeField();
           field.setValue({year: 2010, month: 0, day: 1});
           expect(field.getValue()).toEqual(new Date(2010, 0, 1));
        });

        it("should return null for a string that cannot be parsed", function() {
            makeField({
                dateFormat: 'Y-m-d'
            });
            field.setValue('01/50/2010');
            expect(field.getValue()).toBe(null);
        });

        it("should return null for an object that cannot be parsed", function() {
            makeField();

            field.setValue({});
            expect(field.getValue()).toBe(null);
        });

        it("should update the text field with the formatted value when specifying a date", function() {
            makeField({
                dateFormat: 'Y-m-d'
            });
            field.setValue(new Date(2010, 0, 1));
            expect(field.inputElement.dom.value).toBe('2010-01-01');
        });

        it("should clear the text field when specifying null", function() {
            makeField({
                dateFormat: 'Y-m-d'
            });
            field.setValue(new Date(2010, 0, 1));
            field.setValue(null);
            expect(field.inputElement.dom.value).toBe('');
        });

        describe("events", function() {
            var spy;
            beforeEach(function() {
                spy = jasmine.createSpy();
                makeField();
                field.on('change', spy);
            });

            afterEach(function() {
                spy = null;
            });

            it("should fire the change event when setting a value", function() {
                field.setValue(new Date(2010, 0, 1));
                expect(spy.callCount).toBe(1);
                expect(spy.mostRecentCall.args[0]).toBe(field);
                expect(spy.mostRecentCall.args[1]).toEqual(new Date(2010, 0, 1));
                expect(spy.mostRecentCall.args[2]).toBeNull(field);
            });

            it("should fire the change event when changing a value", function() {
                field.setValue(new Date(2010, 0, 1));
                spy.reset();
                field.setValue(new Date(2010, 11, 31));
                expect(spy.callCount).toBe(1);
                expect(spy.mostRecentCall.args[0]).toBe(field);
                expect(spy.mostRecentCall.args[1]).toEqual(new Date(2010, 11, 31));
                expect(spy.mostRecentCall.args[2]).toEqual(new Date(2010, 0, 1));
            });

            it("should fire the change event when clearing a value", function() {
                field.setValue(new Date(2010, 0, 1));
                spy.reset();
                field.setValue(null);
                expect(spy.callCount).toBe(1);
                expect(spy.mostRecentCall.args[0]).toBe(field);
                expect(spy.mostRecentCall.args[1]).toBeNull();
                expect(spy.mostRecentCall.args[2]).toEqual(new Date(2010, 0, 1));
            });

            it("should not fire the change event when setting the same date", function() {
                field.setValue(new Date(2010, 0, 1));
                spy.reset();
                field.setValue(new Date(2010, 0, 1));
                expect(spy).not.toHaveBeenCalled();
            });
        });
    });

    describe("getValue", function() {
        it("should return a date object when configured with a value", function() {
            makeField({
                value: new Date(2010, 0, 1)
            });
            expect(field.getValue()).toEqual(new Date(2010, 0, 1));
        });

        it("should return a date object after having a value set", function() {
            makeField();
            field.setValue(new Date(2010, 0, 1));
            expect(field.getValue()).toEqual(new Date(2010, 0, 1));
        });

        it("should return null when not configured with a value", function() {
            makeField();
            expect(field.getValue()).toBeNull();
        });

        it("should return null after clearing a value", function() {
            makeField({
                value: new Date(2010, 0, 1)
            });
            field.setValue(null);
            expect(field.getValue()).toBeNull();
        });
    });

    describe("getFormattedValue", function() {
        it("should return the formatted value when configured with a value", function() {
            makeField({
                dateFormat: 'Y-m-d',
                value: new Date(2010, 0, 1)
            });
            expect(field.getFormattedValue()).toBe('2010-01-01');
        });

        it("should return the formatted value after having a value set", function() {
            makeField({
                dateFormat: 'Y-m-d'
            });
            field.setValue(new Date(2010, 0, 1));
            expect(field.getFormattedValue()).toBe('2010-01-01');
        });

        it("should favour a passed format over the class format", function() {
            makeField({
                dateFormat: 'd/m/Y'
            });
            field.setValue(new Date(2010, 0, 1));
            expect(field.getFormattedValue('Y-m-d')).toBe('2010-01-01');
        });

        it("should return '' when not configured with a value", function() {
            makeField();
            expect(field.getFormattedValue()).toBe('');
        });

        it("should return '' after clearing a value", function() {
            makeField({
                value: new Date(2010, 0, 1)
            });
            field.setValue(null);
            expect(field.getFormattedValue()).toBe('');
        });
    });

    describe("minDate", function() {
        it("should accept Date object", function() {
            makeField({
                minDate: new Date()
            });

            expect(field.getMinDate()).toEqual(today);
        });

        it("should accept string in dateFormat", function() {
            makeField({
                minDate: Ext.Date.format(today, Ext.util.Format.defaultDateFormat)
            });

            expect(field.getMinDate()).toEqual(today);
        });
    });

    describe("maxDate", function() {
        it("should accept Date object", function() {
            makeField({
                maxDate: new Date()
            });

            expect(field.getMaxDate()).toEqual(today);
        });

        it("should accept string in dateFormat", function() {
            makeField({
                maxDate: Ext.Date.format(today, Ext.util.Format.defaultDateFormat)
            });

            expect(field.getMaxDate()).toEqual(today);
        });
    });

    describe("picker", function() {
        var oldPlatformTags;

        beforeEach(function() {
            oldPlatformTags = Ext.merge({}, Ext.platformTags);
        });

        afterEach(function() {
            Ext.platformTags = oldPlatformTags;
        });

        it("should create only one date trigger", function() {
            makeField();
            expect(field.afterInputElement.dom.children.length).toBe(1);
        });

        it("should choose edge picker on a phone", function() {
            Ext.platformTags.phone = true;
            makeField();

            var picker = field.getPicker();

            expect(picker.xtype).toBe('datepicker');
        });

        it("should choose floated picker when not on a phone", function() {
            Ext.platformTags.phone = false;
            makeField();

            var picker = field.getPicker();

            expect(picker.xtype).toBe('datepanel');
        });

        it('should set value onto edge picker', function () {
            makeField();
            var date = new Date();

            date.setHours(0);
            date.setMinutes(0);
            date.setSeconds(0);
            date.setMilliseconds(0);

            Ext.platformTags.phone = true;

            field.setValue(date);

            field.expand();

            expect(field.getPicker().getValue()).toEqual(new Date(date));
        });

        it('should set value onto floated picker', function () {
            makeField();
            var date = new Date();

            date.setHours(0);
            date.setMinutes(0);
            date.setSeconds(0);
            date.setMilliseconds(0);

            field.setValue(date);

            field.expand();

            expect(field.getPicker().getValue()).toEqual(new Date(date));
        });

        describe("picker with invalid value", function() {
            function runIt(type) {
                var D = Ext.Date,
                    now = D.clearTime(new Date(), true);

                makeField({
                    picker: type
                });
                field.inputElement.dom.value = 'asdf';
                field.showPicker();
                expect(D.clearTime(field.getPicker().getValue(), true)).toEqual(now);
            }

            it("should set the current date with picker: edge", function() {
                runIt('edge');
            });

            it("should set the current date with picker: floated", function() {
                runIt('floated');
            });
        });
    });

    describe('validate', function () {
        it('should validate date object', function () {
            makeField({
                validators: 'date'
            });

            field.setValue(new Date());

            expect(field.validate()).toBe(true);
        });

        it('should validate date string', function () {
            makeField({
                validators: 'date'
            });

            field.setValue('01/01/2017');

            expect(field.validate()).toBe(true);
        });
    });

    describe("resetting", function() {
        it("should reset to the original value", function() {
            var spy = jasmine.createSpy();

            makeField({
                value: new Date('01/01/2017 00:00:00')
            });
            
            field.on('change', spy);
            field.setValue(new Date('02/02/2017 00:00:00'));

            expect(field.getValue()).toEqual(new Date('02/02/2017 00:00:00'));

            field.reset();

            expect(field.getValue()).toEqual(new Date('01/01/2017 00:00:00'));
            expect(spy.callCount).toBe(2);
        });

        it("should reset the field when the input is not valid", function() {
            var spy = jasmine.createSpy();

            makeField({
                value: new Date('01/01/2017 00:00:00')
            });
            field.on('change', spy);

            field.inputElement.dom.value = 'abcdefg';
            field.onInput({});

            expect(field.isValid()).toBe(false);

            field.reset();

            expect(field.getValue()).toEqual(new Date('01/01/2017 00:00:00'));
            expect(field.inputElement.dom.value).not.toBe('abcdefg');
            expect(spy.callCount).toBe(0);
            expect(field.isValid()).toBe(true);
        });
    });

    describe("empty value", function() {
        it("should be able to clear the value", function() {
            makeField({
                value: new Date()
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
