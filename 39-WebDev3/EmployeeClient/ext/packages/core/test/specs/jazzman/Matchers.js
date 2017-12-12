if (window.__PARTIAL_UNIT_TEST_SUITE__) {
topSuite("Ext.jazzman.Matchers", [false], function() {
    var matcher, resultParams;
    
    jasmine.Spec.prototype.addResult = function(params) {
        this.results().addResult(new jasmine.ExpectationResult(params));
    };
    
    beforeEach(function() {
        var env = jasmine.getEnv();
        
        matcher = new jasmine.Matchers(env, null, env.currentSpec);
        matcher.not = new jasmine.Matchers(env, null, env.currentSpec, true);
        
        resultParams = {};
    });
    
    afterEach(function() {
        matcher = resultParams = null;
    });
    
    it("should fail", function() {
        expect(function() {
            throw "foo";
        }).not.toThrow();
    });
    
    xdescribe("toThrow", function() {
        beforeEach(function() {
            resultParams.matcherName = 'toThrow';
        });
        
        describe("w/o not", function() {
            it("should return true if test subject throws an exception", function() {
                matcher.actual = function() {
                    throw "foo";
                };
                
                resultParams.passed = matcher.toThrow('foo');
                
                this.addResult(resultParams);
            });
            
            it("should return false if test subject does not throw an exception", function() {
                matcher.actual = function() {};
                
                resultParams.passed = !matcher.toThrow();
                
                this.addResult(resultParams);
            });
        });
        
        describe("with not", function() {
            it("should return true if test subject does not throw an exception", function() {
                matcher.not.actual = function() {};
                
                resultParams.passed = matcher.not.toThrow();
                
                this.addResult(resultParams);
            });
            
            it("should return false if test subject throws an exception", function() {
                matcher.not.actual = function() {
                    throw "bar";
                };
                
                resultParams.passed = !matcher.not.toThrow();
                
                this.addResult(resultParams);
            });
        });
    });

    describe('toHaveProperty', function () {
        it('should return true if target has property', function () {
            var target = {
                foo: 'bar'
            };

            expect(target).toHaveProperty('foo');
        });

        it('should return true if target has property as function', function () {
            var target = {
                foo: function () {}
            };

            expect(target).toHaveProperty('foo');
        });

        it('should return true if target has property as undefined', function () {
            var target = {
                foo: undefined
            };

            expect(target).toHaveProperty('foo');
        });

        it('should return true if target does not have property', function () {
            var target = {
                foo: 'bar'
            };

            expect(target).not.toHaveProperty('bar');
        });
    });

    describe('toHaveProperties', function () {
        it('should return true if target has a property', function () {
            var target = {
                foo: 'bar'
            };

            expect(target).toHaveProperties('foo');
        });

        it('should return true if target has all properties', function () {
            var target = {
                foo: undefined,
                bar: undefined
            };

            expect(target).toHaveProperties('foo', 'bar');
        });

        it('should fail if one property is missing', function () {
            var target = {
                foo: 'bar'
            };

            expect(target).toHaveProperties('foo', 'bar');
        });

        it('should return true if target does not have a property', function () {
            var target = {
                foo: 'bar'
            };

            expect(target).not.toHaveProperties('bar');
        });

        it('should return true if target does not have all properties', function () {
            var target = {
                foo: 'bar'
            };

            expect(target).not.toHaveProperties('bar', 'baz');
        });

        it('should fail if target has one property', function () {
            var target = {
                foo: 'bar'
            };

            expect(target).not.toHaveProperties('foo', 'bar');
        });
    });
});
}
