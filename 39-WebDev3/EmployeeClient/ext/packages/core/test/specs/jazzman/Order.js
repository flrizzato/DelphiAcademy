if (window.__PARTIAL_UNIT_TEST_SUITE__) {
topSuite("Ext.jazzman.Order", false, function() {
    function createSuite(async) {
        var rightOrder = [
            (async ? 'async' : 'sync') + ' first block',
            'foo beforeAll 1',
            'foo beforeAll 2',
            'foo beforeEach 1',
            'foo beforeEach 2',
            'foo it 1-1',
            'foo it 1-2',
            'foo it 1-3',
            'foo afterEach 1',
            'foo afterEach 2',
            'foo beforeEach 1',
            'foo beforeEach 2',
            'foo it 2-1',
            'foo it 2-2',
            'foo it 2-3',
            'foo afterEach 1',
            'foo afterEach 2',
            'foo beforeEach 1',
            'foo beforeEach 2',
            'foo it 3-1',
            'foo it 3-2',
            'foo it 3-2',
            'foo it 3-3',
            'foo afterEach 1',
            'foo afterEach 2',
            'bar beforeAll',
            'foo beforeEach 1',
            'foo beforeEach 2',
            'bar beforeEach runs',
            'bar beforeEach waitsFor',
            'bar it 1-1',
            'bar it 1-2',
            'bar it 1-3',
            'bar afterEach',
            'foo afterEach 1',
            'foo afterEach 2',
            'foo beforeEach 1',
            'foo beforeEach 2',
            'bar beforeEach runs',
            'bar beforeEach waitsFor',
            'bar it 2-1',
            'bar it 2-2',
            'bar it 2-2',
            'bar it 2-3',
            'bar afterEach',
            'foo afterEach 1',
            'foo afterEach 2',
            'foo beforeEach 1',
            'foo beforeEach 2',
            'bar beforeEach runs',
            'bar beforeEach waitsFor',
            'qux beforeEach 1',
            'qux beforeEach 2',
            'qux it 1',
            'qux it 1 fin',
            'qux afterEach',
            'bar afterEach',
            'foo afterEach 1',
            'foo afterEach 2',
            'foo beforeEach 1',
            'foo beforeEach 2',
            'bar beforeEach runs',
            'bar beforeEach waitsFor',
            'qux beforeEach 1',
            'qux beforeEach 2',
            'qux it 2-1',
            'qux it 2-2',
            'qux it 2-3',
            'qux afterEach',
            'bar afterEach',
            'foo afterEach 1',
            'foo afterEach 2',
            'foo beforeEach 1',
            'foo beforeEach 2',
            'bar beforeEach runs',
            'bar beforeEach waitsFor',
            'qux beforeEach 1',
            'qux beforeEach 2',
            'qux it 3-1',
            'qux it 3-2',
            'qux it 3-3',
            'qux it 3-4',
            'qux it 3-5',
            'qux it 3-6',
            'qux afterEach',
            'bar afterEach',
            'foo afterEach 1',
            'foo afterEach 2',
            'qux afterAll',
            'foo beforeEach 1',
            'foo beforeEach 2',
            'bar beforeEach runs',
            'bar beforeEach waitsFor',
            'bar it 3-1',
            'bar it 3-2',
            'bar it 3-3',
            'bar it 3-4',
            'bar afterEach',
            'foo afterEach 1',
            'foo afterEach 2',
            'foo beforeEach 1',
            'foo beforeEach 2',
            'bar beforeEach runs',
            'bar beforeEach waitsFor',
            'bar it 4-1',
            'bar it 4-2',
            'bar it 4-3',
            'bar afterEach',
            'foo afterEach 1',
            'foo afterEach 2',
            'foo beforeEach 1',
            'foo beforeEach 2',
            'bar beforeEach runs',
            'bar beforeEach waitsFor',
            'bar it 5',
            'bar afterEach',
            'foo afterEach 1',
            'foo afterEach 2',
            'bar afterAll',
            'foo beforeEach 1',
            'foo beforeEach 2',
            'foo it 4',
            'foo afterEach 1',
            'foo afterEach 2',
            'foo beforeEach 1',
            'foo beforeEach 2',
            'foo it 5',
            'foo afterEach 1',
            'foo afterEach 2',
            'foo afterAll'
        ];
        
        var order = [];
        
        var suite = describe((async ? "async" : "sync") + " foo", function() {
            beforeAll(function() {
                order.push('foo beforeAll 1');
            });
            
            beforeEach(function() {
                order.push('foo beforeEach 1');
            });
            
            beforeEach(function() {
                order.push('foo beforeEach 2');
            });
            
            beforeAll(function() {
                order.push('foo beforeAll 2');
            });
            
            afterAll(function() {
                order.push('foo afterAll');
            });
            
            afterEach(function() {
                order.push('foo afterEach 1');
            });
            
            afterEach(function() {
                order.push('foo afterEach 2');
            });
            
            it("foo 1", function() {
                runs(function() {
                    order.push('foo it 1-2');
                });
                
                order.push('foo it 1-1');
                
                // If there is a problem this waits() will break async flow
                waits(1);
                
                runs(function() {
                    order.push('foo it 1-3');
                });
            });
            
            spec = todo().
            it("foo 2", function() {
                // This is going to time out after 1 invocation
                waitsFor(function() {
                    order.push('foo it 2-2');
                    return false;
                }, 'foo 2 to run', 1, 10);
                
                order.push('foo it 2-1');

                // Don't try to do this at home!
                this.queue.insertNext(new jasmine.Block(this.env, function() {
                    order.push('foo it 2-3');
                }, this), true);
            });
            
            spec = todo().
            it("foo 3", function() {
                order.push('foo it 3-1');
                
                // This is going to time out after 2 invocations
                waitsFor(function() {
                    order.push('foo it 3-2');
                    return false;
                }, 'foo 3 to run', 20, 10);
                
                this.queue.insertNext(new jasmine.Block(this.env, function() {
                    order.push('foo it 3-3');
                }, this), true);
            });
            
            describe("bar", function() {
                beforeAll(function() {
                    order.push('bar beforeAll');
                });
                
                beforeEach(function() {
                    runs(function() {
                        order.push('bar beforeEach runs');
                    });
                    
                    waitsFor(function(done) {
                        order.push('bar beforeEach waitsFor');
                        done();
                    });
                });
                
                afterEach(function() {
                    order.push('bar afterEach');
                });
                
                afterAll(function() {
                    order.push('bar afterAll');
                });
                
                it("bar 1", function() {
                    waitsFor(function() {
                        order.push('bar it 1-2');
                        return true;
                    }, 'bar 1 to run', 10, 10);
                    
                    runs(function() {
                        order.push('bar it 1-3');
                    });
                    
                    order.push('bar it 1-1');
                });
                
                it("bar 2", function() {
                    this.counter = 0;
                    
                    waitsFor(function() {
                        order.push('bar it 2-2');
                        return this.counter++;
                    }, 'bar 1 to run', 100, 10);
                    
                    runs(function() {
                        order.push('bar it 2-3');
                    });
                    
                    order.push('bar it 2-1');
                });
                
                describe("qux", function() {
                    beforeEach(function() {
                        runs(function() {
                            order.push('qux beforeEach 1');
                        });
                        
                        waits(1);
                        
                        runs(function() {
                            order.push('qux beforeEach 2');
                        });
                    });
                    
                    afterEach(function() {
                        order.push('qux afterEach');
                    });
                    
                    afterAll(function() {
                        order.push('qux afterAll');
                    });
                    
                    todo().
                    it("qux 1", function() {
                        var spy = jasmine.createSpy('qux 1');
                        
                        order.push('qux it 1');
                        
                        // This is going to time out
                        waitsForSpy(spy, null, 10);

                        this.queue.insertNext(new jasmine.Block(this.env, function() {
                            order.push('qux it 1 fin');
                        }, this), true);
                    });
                    
                    it("qux 2", function() {
                        var spy = jasmine.createSpy('qux 2').andCallFake(function() {
                            order.push('qux it 2-2');
                        });
                        
                        order.push('qux it 2-1');
                        
                        waitsForSpy(spy, null, 10);
                        
                        runs(function() {
                            order.push('qux it 2-3');
                        });
                        
                        // waitsForSpy is going to be executed after spy was called
                        spy();
                    });
                    
                    it("qux 3", function() {
                        var spy = jasmine.createSpy('qux 3').andCallFake(function() {
                            order.push('qux it 3-4');
                        });
                        
                        order.push('qux it 3-1');
                        
                        runs(function() {
                            order.push('qux it 3-2');
                        });
                        
                        waits(1);
                        
                        waitsForSpy(spy, null, 100);
                        
                        runs(function() {
                            order.push('qux it 3-5');
                        });
                        
                        this.queue.insertNext(new jasmine.Block(this.env, function() {
                            order.push('qux it 3-6');
                        }, this), true);
                        
                        // This is going to kick in after waitsForSpy block was executed
                        // for the first time but before it timed out
                        var setTimeout = jasmine._setTimeout;
                        setTimeout(function() {
                            order.push('qux it 3-3');
                            spy();
                        }, 10);
                    });
                });
                
                it("bar 3", function() {
                    var spy = jasmine.createSpy('bar 3').andCallFake(function(done) {
                        order.push('bar it 3-3');
                        done();
                    });
                    
                    order.push('bar it 3-1');
                    
                    runs(function() {
                        order.push('bar it 3-2');
                    });
                    
                    waitsFor(spy, 'bar 3 spy to run', 10);
                    
                    runs(function() {
                        order.push('bar it 3-4');
                    });
                });
                
                todo().
                it("bar 4", function() {
                    order.push('bar it 4-1');
                    
                    // This is going to fail without timing out
                    waitsFor(function(done, fail) {
                        order.push('bar it 4-2');
                        fail();
                    }, 'bar 4 waitsFor', 10);
                    
                    this.queue.insertNext(new jasmine.Block(this.env, function() {
                        order.push('bar it 4-3');
                    }, this), true);
                });
                
                it("bar 5", function() {
                    order.push('bar it 5');
                });
            });
            
            it("foo 4", function() {
                order.push('foo it 4');
            });
            
            it("foo 5", function() {
                order.push('foo it 5');
            });
        });
        
        it("should finish in the right " + (async ? 'async' : 'sync') + " order", function() {
            expect(order).toEqual(rightOrder) ||
                (function() {
                    if (order.length !== rightOrder.length) {
                        expect('length: ' + order.length).toBe('length: ' + rightOrder.length);
                    }
                    
                    for (var i = 0, len = rightOrder.length; i < len; i++) {
                        if (order[i] !== rightOrder[i]) {
                            expect('element #' + i + ': ' + order[i]).
                                toBe('element #' + i + ': ' + rightOrder[i]);
                        }
                    }
                    
                    jasmine.console.log('right order:');
                    jasmine.console.dir(rightOrder);
                    jasmine.console.log('actual order:');
                    jasmine.console.dir(order);
                })();
        });
        
        suite.queue.blocks.push(new jasmine.Block(jasmine.getEnv(), function() {
            order.push((async ? 'async' : 'sync') + ' first block');
            jasmine.DEFAULT_UPDATE_INTERVAL = async ? 1 : 0;
        }, suite));
    }
    
    createSuite(false);
    createSuite(true);
});
}
