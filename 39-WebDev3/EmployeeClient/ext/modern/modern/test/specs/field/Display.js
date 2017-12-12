topSuite("Ext.field.Display", [
], function () {
    var component;

    function makeComponent (config) {
        config = Ext.apply({
            name: 'fieldname',
            renderTo: Ext.getBody()
        }, config);

        component = new Ext.field.Display(config);
    }

    afterEach(function() {
        component = Ext.destroy(component);
    });

    it("should be registered as xtype 'displayfield'", function() {
        component = Ext.create("Ext.field.Display", {name: 'test'});
        expect(component instanceof Ext.field.Display).toBe(true);
        expect(Ext.getClass(component).xtype).toBe("displayfield");
    });

    describe("defaults", function() {
        beforeEach(function() {
            makeComponent();
        });

        it("should have encodeHtml set to true", function() {
            expect(component.getEncodeHtml()).toBe(true);
        });
    });


    describe("validation", function() {
        beforeEach(function() {
            makeComponent();
        });

        it("should always return true from the validate method", function() {
            expect(component.validate()).toBe(true);
        });

        it("should always return true from the isValid method", function() {
            expect(component.isValid()).toBe(true);
        });
    });

    describe("value getters", function() {
        describe("getValue", function() {
            it("should return the field's value", function() {
                makeComponent({value: 'the field value'});
                expect(component.getValue()).toEqual('the field value');
            });

            it("should return the same value when encodeHtml is true", function() {
                makeComponent({value: '<p>the field value</p>', encodeHtml: true});
                expect(component.getValue()).toEqual('<p>the field value</p>');
            });

            it("should keep an array value", function() {
                var arr = [];
                makeComponent({value: arr});
                expect(component.getValue()).toBe(arr);
            });

            it("should keep a numeric value", function() {
                makeComponent({value: 50});
                expect(component.getValue()).toBe(50);
            });

            it("should keep a boolean value", function() {
                makeComponent({value: true});
                expect(component.getValue()).toBe(true);
            });

            it("should keep false", function() {
                makeComponent({value: false});
                expect(component.getValue()).toBe(false);
            });

            it("should keep 0", function() {
                makeComponent({value: 0});
                expect(component.getValue()).toBe(0);
            });
        });
    });

    describe("resetting", function() {
        it("should reset the value", function() {
            makeComponent({value: 'foo'});
            component.setValue('bar');

            expect(component.getValue()).toBe('bar');

            component.reset();

            expect(component.getValue()).toBe('foo');
        });
    });

    describe("setting value", function() {
        describe("setValue", function() {
            it("should set the inputEl's innerHTML to the specified value", function() {
                makeComponent({value: 'the field value'});
                component.setValue('the new value');
                expect(component.inputElement.dom).hasHTML('the new value');
            });

            it("should html-encode the value by default", function() {
                makeComponent({value: 'the field value'});
                component.setValue('<p>the new value</p>');
                expect(component.inputElement.dom).hasHTML('&lt;p&gt;the new value&lt;/p&gt;');
            });

            it("should not html-encode the value with encodeHtml: false", function() {
                makeComponent({value: 'the field value', encodeHtml: false});
                component.setValue('<p>the new value</p>');
                expect(component.inputElement.dom).hasHTML('<p>the new value</p>');
            });

            it("should accept 0", function() {
                makeComponent({
                    value: 0
                });
                expect(component.getValue()).toBe(0);
            });

            it("should accept false", function() {
                makeComponent({
                    value: false
                });
                expect(component.getValue()).toBe(false);
            });

            it("should accept setting an array value", function() {
                makeComponent({
                    value: [1, 2, 3, 4],
                    renderer: function(v) {
                        return v.join(',');
                    }
                });
                expect(component.getValue().toString()).toBe('1,2,3,4');
            });

            it("should accept setting an object value", function() {
                makeComponent({
                    value: {
                        foo: true,
                        bar: true,
                        baz: true
                    },
                    renderer: function(v) {
                        return Ext.Object.getKeys(v).join(',');
                    }
                });
                expect(Ext.Object.getKeys(component.getValue()).join(',')).toBe('foo,bar,baz');
            });
        });

    });

    describe("renderer", function(){
        it("should set the innerHTML to the value specified by the renderer", function(){
            // component is not rendered at the time setRawValue() is called during construction
            // so this classic test fails in modern
            makeComponent({
                value: 'foo',
                renderer: function(v){
                    return v + 'bar';
                },
                renderTo: Ext.getBody()
            });
            expect(component.inputElement.dom.innerHTML).toBe('foobar');
        });


        it("should set the innerHTML to the value specified by changed renderer", function(){
            // component is not rendered at the time setRawValue() is called during construction
            // so this classic test fails in modern
            makeComponent({
                value: 'foo',
                renderer: function(v){
                    return v + 'bar';
                },
                renderTo: Ext.getBody()
            });
            expect(component.inputElement.dom.innerHTML).toBe('foobar');
            component.setRenderer(function(v) {
                return v + 'baz';
            });
            expect(component.inputElement.dom.innerHTML).toBe('foobaz');
        });

        it("should default the scope to the field", function(){
            var scope;
            makeComponent({
                value: 'foo',
                renderer: function(v){
                    scope = this;
                }
            });
            expect(scope).toBe(component);
        });

        it("should use the passed scope", function(){
            var o = {},
                scope;

            makeComponent({
                value: 'foo',
                scope: o,
                renderer: function(v){
                    scope = this;
                }
            });
            expect(scope).toBe(o);
        });

        it("should pass the raw value and the field to the renderer", function(){
            var arg1,
                arg2;

            makeComponent({
                value: 'foo',
                renderer: function(a, b){
                    arg1 = a;
                    arg2 = b;
                }
            });

            expect(arg1).toBe('foo');
            expect(arg2).toBe(component);
        });

        it("should pass an empty string to the renderer if the value is undefined", function() {
            var arg1;
            makeComponent({
                value: undefined,
                renderer: function(a) {
                    arg1 = a;
                }
            });
            expect(arg1).toBe('');
        });

    });

    describe("layout", function() {
        xit("should vertically align the value to the top when the height of the field is stretched", function() {
            // xit - getY() is only valid if component is floated.
            // https://sencha.jira.com/browse/EXTJS-13818
            makeComponent({
                height: 100,
                fieldLabel: 'foo',
                value: 'bar'
            });

            expect(component.inputElement.getY()).toBe(component.bodyEl.getY() + component.inputEl.getMargin('t'));
        });

        it("should be able to auto height with multi line text", function() {
            makeComponent({
                value: 'foo'
            });
            var height = component.inputElement.getHeight();
            component.destroy();
            makeComponent({
                value: 'foo<br>bar<br>baz',
                encodeHtml: false
            });
            expect(component.inputElement.getHeight()).toBeGreaterThan(height);
        });
    });

});
