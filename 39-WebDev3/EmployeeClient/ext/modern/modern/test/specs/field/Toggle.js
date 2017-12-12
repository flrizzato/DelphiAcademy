topSuite("Ext.field.Toggle", function() {
    var field,
        createField = function(config) {
            if (field) {
                field.destroy();
            }

            field = Ext.create('Ext.field.Toggle', Ext.apply({
                renderTo: Ext.getBody()
            }, config));
        };

    afterEach(function() {
        if (field) {
            field.destroy();
        }
    });

    describe("methods", function() {
        describe("getValue", function() {
            describe("checked", function() {
                beforeEach(function() {
                    createField({
                        value: 1
                    });
                });

                it("should return a value", function() {
                    expect(field.getValue()).toBe(true);
                });
            });

            describe("unchecked", function() {
                beforeEach(function() {
                    createField();
                });

                it("should return a boolean", function() {
                    expect(field.getValue()).toBe(false);
                });
            });
        });
    });

    describe("events", function() {
        var spy;

        beforeEach(function() {
            createField();
            spy = jasmine.createSpy();
        });

        afterEach(function() {
            spy = null;
        });

        describe("change", function() {
            it("should fire when you call setValue", function() {
                field.on('change', spy);
                field.setValue(1);
                expect(spy.callCount).toBe(1);
                expect(spy.mostRecentCall.args[0]).toBe(field);
                expect(spy.mostRecentCall.args[1]).toBe(true);
                expect(spy.mostRecentCall.args[2]).toBe(false);
            });

            it("should fire when you call toggle", function() {
                field.on('change', spy);
                field.toggle();
                expect(spy.callCount).toBe(1);
                expect(spy.mostRecentCall.args[0]).toBe(field);
                expect(spy.mostRecentCall.args[1]).toBe(true);
                expect(spy.mostRecentCall.args[2]).toBe(false);
            });

            it("should fire on tap", function() {
                field.on('change', spy);
                jasmine.fireMouseEvent(field.getSlider().element, 'click');

                expect(spy.callCount).toBe(1);
                expect(spy.mostRecentCall.args[0]).toBe(field);
                expect(spy.mostRecentCall.args[1]).toBe(true);
                expect(spy.mostRecentCall.args[2]).toBe(false);

                spy.reset();
                jasmine.fireMouseEvent(field.getSlider().element, 'click');

                expect(spy.callCount).toBe(1);
                expect(spy.mostRecentCall.args[0]).toBe(field);
                expect(spy.mostRecentCall.args[1]).toBe(false);
                expect(spy.mostRecentCall.args[2]).toBe(true);
            });
        });
    });

    describe('boxLabel', function () {
        var boxLabel = '<div style="width:50px;background:green;">&nbsp;</div>';

        it('should layout with boxLabelAlign: after', function () {
            createField({
                inline: true,
                boxLabel: boxLabel
            });

            expect(field).toHaveLayout({
                element: { xywh: '0 0 82 24' },
                labelElement: { d: false },
                bodyWrapElement: { xywh: '0 0 82 24' },
                bodyElement: { xywh: '0 0 82 24' },
                boxWrapElement: { xywh: '0 0 82 24' },
                boxElement: { xywh: '0 0 26 24' },
                boxLabelElement: { xywh: '26 0 56 24' }
            });
        });

        it('should layout with boxLabelAlign: before', function () {
            createField({
                inline: true,
                boxLabel: boxLabel,
                boxLabelAlign: 'before'
            });

            expect(field).toHaveLayout({
                element: { xywh: '0 0 82 24' },
                labelElement: { d: false },
                bodyWrapElement: { xywh: '0 0 82 24' },
                bodyElement: { xywh: '0 0 82 24' },
                boxWrapElement: { xywh: '0 0 82 24' },
                boxElement: { xywh: '56 0 26 24' },
                boxLabelElement: { xywh: '0 0 56 24' }
            });
        });

        describe("labeled", function () {
            it('should layout with boxLabelAlign: after', function () {
                createField({
                    inline: true,
                    label: 'Foo',
                    boxLabel: boxLabel
                });

                expect(field).toHaveLayout({
                    element: { xywh: '0 0 182 24' },
                    labelElement: { xywh: '0 0 100 24' },
                    bodyWrapElement: { xywh: '100 0 82 24' },
                    bodyElement: { xywh: '100 0 82 24' },
                    boxWrapElement: { xywh: '100 0 82 24' },
                    boxElement: { xywh: '100 0 26 24' },
                    boxLabelElement: { xywh: '126 0 56 24' }
                });
            });

            it('should layout with boxLabelAlign: before', function () {
                createField({
                    inline: true,
                    label: 'Foo',
                    boxLabel: boxLabel,
                    boxLabelAlign: 'before'
                });

                expect(field).toHaveLayout({
                    element: { xywh: '0 0 182 24' },
                    labelElement: { xywh: '0 0 100 24' },
                    bodyWrapElement: { xywh: '100 0 82 24' },
                    bodyElement: { xywh: '100 0 82 24' },
                    boxWrapElement: { xywh: '100 0 82 24' },
                    boxElement: { xywh: '156 0 26 24' },
                    boxLabelElement: { xywh: '100 0 56 24' }
                });
            });
        });
    });
});