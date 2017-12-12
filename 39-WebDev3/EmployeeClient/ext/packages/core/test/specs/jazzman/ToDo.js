if (window.__PARTIAL_UNIT_TEST_SUITE__) {
topSuite("Ext.jazzman.ToDo", [false], function() {
    var ready = false;
    
    toDo(true, "foo").
    it("should fail-but-pass", function() {
        expect(true).toBe(false);
    });
    
    // This spec SHOULD fail, otherwise there's a problem!
    toDo("bar").
    it("should pass but report failed with TODO description", function() {
        expect(true).toBe(true);
    });
    
    // Ditto
    toDo(true, "bar").
    it("should pass but report failed with TODO description too", function() {
        expect(false).toBe(false);
    });
    
    toDo(false).
    it("should pass normally", function() {
        expect(false).toBe(false);
    });
    
    // This spec should fail normally, that's expected.
    toDo(false).
    it("should fail normally", function() {
        expect('white').toBe('black');
    });
    
    toDo(true).
    xit("should not run", function() {
        expect('black').toBe('white');
    });
    
    toDo(false).
    xit("should not run either", function() {
        expect('whatever').toBe('whichever');
    });
    
    toDo(function() { return ready; }).
    it("should fail but report passed", function() {
        expect(0).toBe(1);
    });
    
    toDo(function() { return ready; }).
    it("should pass but report failed", function() {
        expect(1).toBe(1);
    });
    
    toDo(function() { return !ready; }).
    it("should pass and report passed", function() {
        expect('foo').toBe('foo');
    });
    
    toDo(function() { return !ready; }).
    it("should fail and report failed", function() {
        expect('foo').toBe('bar');
    });
    
    toDo(true, "baz").
    describe("some specs fail but suite reported as passed", function() {
        it("is normal spec that passes", function() {
            expect(true).toBe(true);
        });
        
        it("is normal spec that fails", function() {
            expect(true).toBe(false);
        });
    });
    
    // This suite SHOULD fail, otherwise there's a problem!
    toDo("qux").
    describe("all specs pass but suite reported as failed", function() {
        it("is normal spec that passes, too", function() {
            expect(true).toBe(true);
        });
        
        it("is normal spec that also passes, too", function() {
            expect(false).toBe(false);
        });
    });
    
    toDo(function() { return ready; }).
    describe("some specs fail in this suite, should be reported as passed", function() {
        it("passes", function() {
            expect(null).toBe(null);
        });
        
        it("fails", function() {
            expect(NaN).toBe(NaN);
        });
    });
    
    toDo(function() { return ready; }).
    describe("all specs pass in this suite, should be reported as failed", function() {
        it("should pass 3", function() {
            expect(undefined).toBe(undefined);
        });
        
        it("should pass 4", function() {
            expect(true).toBe(true);
        });
    });
    
    toDo(function() { return !ready; }).
    describe("all specs pass in this suite, should be reported normally as passed", function() {
        it("should pass 5", function() {
            expect(false).toBe(false);
        });
        
        it("should pass 6", function() {
            expect(true).toBe(true);
        });
    });
    
    toDo(function() { return !ready; }).
    describe("one spec fails in this suite, should be reported normally as failed", function() {
        it("should pass 7", function() {
            expect('black').toBe('black');
        });
        
        it("should fail", function() {
            expect('black').toBe('white');
        });
    });
    
    toDo("zumbo").
    describe("all spec pass but nested suite fails, reported as passed", function() {
        it("should pass 8", function() {
            expect(false).toBe(false);
        });
        
        it("should pass 9", function() {
            expect(true).toBe(true);
        });
        
        describe("nested suite", function() {
            it("should fail-but-pass nestedly", function() {
                expect(true).toBe(false);
            });
            
            it("should pass-but-fail nestedly", function() {
                expect(1).toBe(1);
            });
        });
    });
    
    toDo("fumble", function() { return ready; }).
    describe("deferred resolution, should be todo and report as passed", function() {
        it("should pass 10", function() {
            expect(true).toBe(true);
        });
        
        it("should pass 11", function() {
            expect(false).toBe(false);
        });
        
        describe("nested suite with deferred todo", function() {
            it("should pass 12", function() {
                expect(true).toBe(true);
            });
            
            it("should fail here, too", function() {
                expect(true).toBe(false);
            });
        });
    });
    
    ready = true;
});
}
