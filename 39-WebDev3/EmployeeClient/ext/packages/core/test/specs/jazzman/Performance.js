if (window.__PARTIAL_UNIT_TEST_SUITE__) {
topSuite("Ext.jazzman.Performance", [false, 'Ext.grid.Panel'], function() {
    var outerCount = jasmine.browser.isIE8m ? 5 : 10;
    
    beforeEach(function() {
        // Do nothing to test just the Jazzman performance
    });
    
    afterEach(function() {
        // Ditto
    });
    
    beforeAll(function() {
    });
    
    afterAll(function() {
    });
    
    var wantArray = ['foo', 'bar'],
        haveArray = ['foo', 'bar'],
        simonSez = {
            eatThatQuestion: function() {
                return 42;
            }
        };
    
    for (var i = 0; i < outerCount; i++) {
        describe("outer suite " + i, function() {
            beforeAll(function() {
            });
            
            afterAll(function() {
            });
            
            beforeEach(function() {
            });
            
            afterEach(function() {
            });
            
            for (var j = 0; j < 10; j++) {
                describe("inner suite " + j, function() {
                    var quickAction;
                    
                    beforeAll(function() {
                    });
                    
                    afterAll(function() {
                    });
                    
                    beforeEach(function() {
                        quickAction = jasmine.createSpy('quick action');
                    });
                    
                    afterEach(function() {
                        quickAction = null;
                    });
                    
                    for (var k = 0; k < 10; k++) {
                        it("should do something " + k, function() {
                            expect(false).toBe(false);
                            expect(wantArray).toBeDefined();
                            expect(haveArray).toEqual(wantArray);
                            
                            if (k % 2) {
                                quickAction();
                                expect(quickAction).toHaveBeenCalled();
                            }
                            else {
                                expect(quickAction).not.toHaveBeenCalled();
                            }
                        });
                    }
                    
                    for (k = 0; k < 10; k++) {
                        describe("president suite " + k, function() {
                            var deepThought;
                            
                            beforeEach(function() {
                                deepThought = jasmine.createSpy('deep thought');
                            });
                            
                            afterEach(function() {
                                deepThought = null;
                            });
                            
                            it("should think of something " + (k + j), function() {
                                waitsForSpy(deepThought);
                                
                                waitsFor(function(satori) {
                                    satori();
                                });
                                
                                runs(function() {
                                    expect(false).not.toBe(true);
                                });
                                
                                deepThought();
                            });
                            
                            for (var l = 0; l < 10; l++) {
                                describe("zen suite " + (l + k + j), function() {
                                    var buddhaSmile;
                                    
                                    beforeEach(function() {
                                        buddhaSmile = spyOn(simonSez, 'eatThatQuestion').andCallFake(
                                            function(wowSeriously) {
                                                wowSeriously('no shit Sherlock!');
                                            });
                                    });
                                    
                                    for (var m = 0; m < 5; m++) {
                                        it("should achieve inner peace " + (m + l + k + j), function() {
                                            waitsFor(buddhaSmile);
                                            
                                            runs(function() {
                                                expect(true).toBe(true);
                                            });
                                        });
                                    }
                                });
                            }
                        });
                    }
                });
            }
        });
    }
});
}
