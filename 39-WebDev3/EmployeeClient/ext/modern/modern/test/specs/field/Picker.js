topSuite("Ext.field.Picker", ['Ext.Button', 'Ext.picker.Picker', 'Ext.form.Panel'], function() {
    var oldPlatformTags, field, picker;

    jasmine.usesViewport();

    function makeField(cfg) {
        field = new Ext.field.Picker(cfg);

        if (field.getFloated()) {
            field.show();
        }
        else {
            field.render(Ext.getBody());
        }
    }

    beforeEach(function () {
        oldPlatformTags = Ext.merge({}, Ext.platformTags);
    });

    afterEach(function() {
        Ext.platformTags = oldPlatformTags;

        field = picker = Ext.destroy(field);
    });

    describe("picker type", function() {
        beforeEach(function() {
            makeField({
                createEdgePicker: function() {
                    return new Ext.Component({
                        ownerField: this,
                        isViewportMenu: true,
                        where: 'edge'
                    });
                },

                createFloatedPicker: function() {
                    return new Ext.Component({
                        ownerField: this,
                        where: 'floated'
                    });
                }
            });
        });

        it("should choose edge picker on a phone", function() {
            Ext.platformTags.phone = true;

            picker = field.getPicker();

            expect(picker.where).toBe('edge');
            expect(field.pickerType).toBe('edge');
        });

        it("should choose floated picker when not on a phone", function() {
            Ext.platformTags.phone = false;

            picker = field.getPicker();

            expect(picker.where).toBe('floated');
            expect(field.pickerType).toBe('floated');
        });
    });

    describe('hidePicker', function() {
        var cmp,
            field,
            oldPlatformTags = Ext.platformTags.phone;

        beforeEach(function() {
            Ext.platformTags.phone = true;

            field = new Ext.field.Picker({
                createEdgePicker: function() {
                    return new Ext.picker.Picker({
                        ownerField: this,
                        isViewportMenu: true,
                        where: 'edge',
                        slots: [{
                            name: 'name',
                            data: [{
                                text: 'Bar',
                                value: 'bar'
                            }, {
                                text: 'Baz',
                                value: 'baz'
                            }, {
                                text: 'Foo',
                                value: 'foo'
                            }]
                        }]
                    });
                }
            });
            cmp = new Ext.form.Panel({
                title: 'Form Panel',
                renderTo: Ext.getBody(),
                width: 300,
                height: 300,
                items: [
                    field
                ]
            });

        });

        afterEach(function() {
            Ext.platformTags.phone = oldPlatformTags;
            cmp = field = Ext.destroy(cmp, field);
        });

        it("should hide picker when parent container is hidden", function() {
            field.setValue('foo');
            field.expand();
            picker = field.getPicker();

            expect(field.pickerType).toBe('edge');
            expect(picker.getValue(true)).toEqual({
                name: 'foo'
            });
            waitsFor(function() {
                return field.expanded;
            });
            runs(function() {
                cmp.hide();
            });
            waitsFor(function() {
                return !field.expanded;
            });
        });

        it("should destroy picker when parent container is destroyed", function() {
            field.setValue('foo');
            field.expand();
            picker = field.getPicker();

            expect(field.pickerType).toBe('edge');
            expect(picker.getValue(true)).toEqual({
                name: 'foo'
            });
            waitsFor(function() {
                return field.expanded;
            });
            runs(function() {
                cmp.destroy();
                cmp = null;
            });
            waitsFor(function() {
                return field.destroyed;
            });
        });
    });

    describe('showPicker', function () {
        beforeEach(function() {
            makeField({
                createEdgePicker: function() {
                    return new Ext.picker.Picker({
                        ownerField: this,
                        slots: [{
                            name: 'name',
                            data: [{
                                text: 'Bar',
                                value: 'bar'
                            }, {
                                text: 'Baz',
                                value: 'baz'
                            }, {
                                text: 'Foo',
                                value: 'foo'
                            }]
                        }]
                    });
                }
            });
        });

        it('should set value to picker on show', function () {
            Ext.platformTags.phone = true;

            field.setValue('foo');

            field.expand();

            picker = field.getPicker();

            expect(field.pickerType).toBe('edge');
            expect(picker.getValue(true)).toEqual({
                name: 'foo'
            });
        });
    });
    
    describe("readonly on first tap", function() {
        var inputEl, button, expandSpy, collapseSpy;
        
        beforeEach(function() {
            button = new Ext.Button({
                text: 'foo',
                renderTo: document.body
            });

            expandSpy = jasmine.createSpy('expand');
            collapseSpy = jasmine.createSpy('collapse');

            makeField({
                createEdgePicker: function() {
                    return new Ext.Component({
                        ownerField: this,
                        where: 'edge'
                    });
                },

                createFloatedPicker: function() {
                    return new Ext.Component({
                        ownerField: this,
                        where: 'floated'
                    });
                },

                listeners: {
                    expand: expandSpy,
                    collapse: collapseSpy
                }
            });

            inputEl = field.inputElement;
        });

        afterEach(function() {
            inputEl = button = expandSpy = collapseSpy = Ext.destroy(button);
        });
        
        (jasmine.supportsTouch ? describe : xdescribe)("Readonly on first tap", function() {
            describe("tap on unfocused field", function() {
                beforeEach(function() {
                    Ext.testHelper.tap(inputEl, { pointerType: 'touch'});
                });

                it("should expand the picker", function() {
                    inputEl.focus();
                    waitsForSpy(expandSpy);
                });
            });
        });

        (jasmine.supportsTouch ? xdescribe : describe)("No touch focusing", function() {
            it("should not set to readonly on mouse-induced focus", function() {
                jasmine.fireMouseEvent(inputEl, 'mousedown');

                // None of that game playing when using a mouse
                expect(inputEl.dom.getAttribute('readonly')).toBe(null);

                jasmine.fireMouseEvent(inputEl, 'mouseup');
            });
        });
    });
});
