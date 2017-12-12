if (window.__PARTIAL_UNIT_TEST_SUITE__) {
topSuite("Ext.jazzman.leaks.Stack", [false, 'Ext.Component'], function() {
    afterAll(function() {
        delete window.foo;
        delete window.bar;
    });
    
    describe("suite", function() {
        beforeAll(function() {
            window.foo = 'foo';
        });
        
        afterAll(function() {
            delete window.foo;
        });
        
        it("should not fail because of extra global variables", function() {
            expect(window.foo).toBe('foo');
        });
        
        describe("inner suite", function() {
            beforeAll(function() {
                window.bar = 'bar';
            });
            
            afterAll(function() {
                delete window.bar;
            });
            
            it("should not fail either", function() {
                expect(window.foo).toBe('foo');
                expect(window.bar).toBe('bar');
            });
            
            describe("inner inner suite", function() {
                beforeAll(function() {
                    window.baz = new Ext.Component();
                });
                
                afterAll(function() {
                    window.baz = Ext.destroy(window.baz);
                    delete window.baz;
                });
                
                it("should find window.foo", function() {
                    expect(window.foo).toBe('foo');
                });
                
                it("should find window.bar", function() {
                    expect(window.bar).toBe('bar');
                });
                
                it("should find window.baz", function() {
                    expect(window.baz.$className).toBe('Ext.Component');
                });
            });
        });
        
        it("should fail because of leaked bar", function() {
            expect(window.foo).toBe('foo');
        });
    });
});
}
