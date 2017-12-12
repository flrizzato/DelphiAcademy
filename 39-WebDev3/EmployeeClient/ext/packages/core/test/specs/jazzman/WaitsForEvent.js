if (window.__PARTIAL_UNIT_TEST_SUITE__) {
topSuite("Ext.jazzman.WaitsForEvent", [false, 'Ext.Container', 'Ext.Button'], function() {
    var setTimeout = jasmine._setTimeout,
        button, anotherButton;
    
    describe("HTMLElement", function() {
        beforeEach(function() {
            button = document.createElement('button');
            button.innerHTML = 'button';
            
            document.body.appendChild(button);
        });
        
        afterEach(function() {
            document.body.removeChild(button);
            button = null;
        });
        
        describe("focus", function() {
            it("should work for already focused element", function() {
                button.focus();
                
                // IE
                waitAWhile();
                
                waitsForFocus(button);
                
                runs(function() {
                    expectFocused(button, true);
                });
            });
            
            it("should work when element becomes focused later", function() {
                waitsForFocus(button);
                
                runs(function() {
                    expectFocused(button, true);
                });
                
                setTimeout(function() {
                    button.focus();
                }, 10);
            });
            
            it("should clean up the listener when timed out", function() {
                waitsForFocus(button, 'button to become focused', 1);
                
                // This hackery is to make sure the spec is genuinely passed or failed
                // without inverting results with todo()
                this.runs(function() {
                    var blocks = this.queue.blocks,
                        block, i, len;
                    
                    for (i = 0, len = blocks.length; i < len; i++) {
                        block = blocks[i];
                        
                        if (block.isWaitsForEventBlock) {
                            break;
                        }
                    }
                    
                    this.results().reset();
                    
                    expect(block.listenerInstalled).toBe(false);
                    expect(block.target).toBe(null);
                    expect(block.latchFunction).toBe(null);
                    expect(block.listenerFunction).toBe(null);
                }, null, true);
            });
        });
        
        describe("blur", function() {
            beforeEach(function() {
                anotherButton = document.createElement('button');
                anotherButton.innerHTML = 'another button';
                document.body.appendChild(anotherButton);
            });
            
            afterEach(function() {
                document.body.removeChild(anotherButton);
                anotherButton = null;
            });
            
            it("should work for already unfocused element", function() {
                anotherButton.focus();
                
                waitAWhile();
                
                waitsForBlur(button);
                
                runs(function() {
                    expectFocused(anotherButton, true);
                });
            });
            
            it("should work when element blurs later", function() {
                button.focus();
                
                waitAWhile();
                
                waitsForBlur(button);
                
                setTimeout(function() {
                    anotherButton.focus();
                }, 10);
                
                runs(function() {
                    expectFocused(anotherButton, true);
                });
            });
        });
    });
    
    describe("Component", function() {
        beforeEach(function() {
            button = new Ext.Button({
                text: 'button'
            });
        });
        
        afterEach(function() {
            button = Ext.destroy(button);
        });
        
        describe("rendered focus/blur", function() {
            describe("focus", function() {
                beforeEach(function() {
                    button.render(document.body);
                });
                
                it("should work for already focused component", function() {
                    button.focus();
                    
                    waitAWhile();
                    
                    waitsForFocus(button);
                    
                    runs(function() {
                        expectFocused(button, true);
                    });
                });
                
                it("should work when component becomes focused later", function() {
                    waitsForFocus(button);
                    
                    runs(function() {
                        expectFocused(button, true);
                    });
                    
                    setTimeout(function() {
                        button.focus();
                    }, 10);
                });
            });
            
            describe("blur", function() {
                beforeEach(function() {
                    button.render(document.body);
                    
                    anotherButton = new Ext.Button({
                        text: 'another button',
                        renderTo: document.body
                    });
                });
                
                afterEach(function() {
                    anotherButton = Ext.destroy(anotherButton);
                });
                
                it("should work for already unfocused component", function() {
                    anotherButton.focus();
                    
                    waitAWhile();
                    
                    waitsForBlur(button);
                    
                    runs(function() {
                        expectFocused(anotherButton, true);
                    });
                });
                
                it("should work when component blurs later", function() {
                    button.focus();
                    
                    waitAWhile();
                    
                    waitsForBlur(button);
                    
                    setTimeout(function() {
                        anotherButton.focus();
                    }, 10);
                    
                    runs(function() {
                        expectFocused(anotherButton, true);
                    });
                });
            });
        });
        
        if (!Ext.isModern) {
            describe("render", function() {
                it("should work when event fires", function() {
                    waitForEvent(button, 'render', null, 1000);
                    
                    runs(function() {
                        expect(true).toBe(true);
                    });
                    
                    setTimeout(function() {
                        button.render(document.body);
                    }, 10);
                });
                
                todo().
                it("should fail when event times out", function() {
                    waitForEvent(button, 'render', null, 10);
                    
                    runs(function() {
                        expect(true).toBe(true);
                    });
                });
            });
            
            describe("focus after rendering", function() {
                it("should work when component becomes focused", function() {
                    waitsForFocus(button);
                    
                    runs(function() {
                        expectFocused(button, true);
                    });
                    
                    setTimeout(function() {
                        button.render(document.body);
                    }, 10);
                    
                    setTimeout(function() {
                        button.focus();
                    }, 20);
                });
            });
        }
    });
    
    describe("Container with focus delegation", function() {
        var ct;
        
        beforeEach(function() {
            ct = new Ext.Container({
                renderTo: document.body,
                defaultFocus: 'button',
                items: [{
                    xtype: 'button',
                    text: 'button'
                }]
            });
            
            button = ct.down('button');
        });
        
        afterEach(function() {
            ct = Ext.destroy(ct);
        });
        
        describe("focus", function() {
            it("should work when container delegate is already focused", function() {
                ct.focus();
                
                waitAWhile();
                
                waitsForFocus(ct);
                
                runs(function() {
                    expectFocused(button, true);
                });
            });
            
            it("should work when container delegate becomes focused later", function() {
                waitsForFocus(ct);
                
                runs(function() {
                    expectFocused(button, true);
                });
                
                setTimeout(function() {
                    ct.focus();
                }, 10);
            });
        });
        
        describe("blur", function() {
            beforeEach(function() {
                anotherButton = new Ext.Button({
                    text: 'another button',
                    renderTo: document.body
                });
            });
            
            afterEach(function() {
                anotherButton = Ext.destroy(anotherButton);
            });
            
            it("should work for already unfocused component", function() {
                anotherButton.focus();
                
                waitAWhile();
                
                waitsForBlur(ct);
                
                runs(function() {
                    expectFocused(anotherButton, true);
                });
            });
            
            it("should work when component blurs later", function() {
                ct.focus();
                
                waitAWhile();
                
                waitsForBlur(ct);
                
                setTimeout(function() {
                    anotherButton.focus();
                }, 10);
                
                runs(function() {
                    expectFocused(anotherButton, true);
                });
            });
        });
    });
});
}
