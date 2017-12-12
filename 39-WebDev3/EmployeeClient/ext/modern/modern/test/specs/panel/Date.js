topSuite("Ext.panel.Date", function() {
    var today = Ext.Date.clearTime(new Date()),
        yesterday = Ext.Date.add(today, Ext.Date.DAY, -1),
        tomorrow = Ext.Date.add(today, Ext.Date.DAY, 1),
        panel;
    
    function makePanel(config) {
        config = Ext.apply({
            renderTo: document.body,
            animation: false
        }, config);
        
        panel = new Ext.panel.Date(config);
        
        return panel;
    }
    
    function clickCell(date, type) {
        var cell = Ext.isDate(date) ? panel.getCellByDate(date) : date;

        if (!cell) {
            throw new Error("Cannot find cell for date " + date);
        }
        
        jasmine.fireMouseEvent(cell, type || 'click');
    }

    function clickButton(btn, event) {
        if (typeof btn === 'string') {
            if (btn === 'today') {
                btn = panel.lookup('footer').isHidden() ? 'headerTodayButton' : 'footerTodayButton';
            }
            else {
                btn = btn + 'Button';
            }
            
            btn = panel.lookup(btn);
        }
        
        if (btn) {
            jasmine.fireMouseEvent(btn.el, event || 'click');
        }
    }
    
    function expectOffset(offset, position) {
        var layout = panel.getLayout(),
            front = layout.getFrontItem(),
            pane, index;
        
        if (position == null || position === 'front') {
            pane = front;
        }
        else {
            if (position === 'left') {
                index = layout.shiftIndex(layout.getItemIndex(front), -1);
            }
            else if (position === 'right') {
                index = layout.shiftIndex(layout.getItemIndex(front), 1);
            }

            pane = panel.getItems().getAt(index);
        }
        
        expect(pane.getMonthOffset()).toBe(offset);
    }
    
    afterEach(function() {
        panel = Ext.destroy(panel);
    });
    
    describe("configs", function() {
        it("should clear time from passed disabled date", function() {
            makePanel({
                disabledDates: [new Date()]
            });
            
            var dates = panel.getDisabledDates();
            
            expect(dates.dates[Ext.Date.clearTime(today).getTime()]).toBe(true);
        });
        
        it("should clear time from passed special date", function() {
            makePanel({
                specialDates: [new Date()]
            });
            
            var dates = panel.getSpecialDates();
            
            expect(dates.dates[Ext.Date.clearTime(today).getTime()]).toBe(true);
        });
    });
    
    describe("pointer interaction", function() {
        describe("today button", function() {
            describe("with 1 pane", function() {
                beforeEach(function() {
                    makePanel({
                        showTodayButton: true
                    });
                });
                
                it("should center on today from a month ahead", function() {
                    panel.switchPanes(1);
                    expectOffset(1);
                    
                    clickButton('today');
                    expectOffset(0);
                });
                
                it("should center on today from a month behind", function() {
                    panel.switchPanes(-1);
                    expectOffset(-1);
                    
                    clickButton('today');
                    expectOffset(0);
                });
                
                it("should center on today from a year ahead", function() {
                    panel.replacePanes(12);
                    expectOffset(12);
                    
                    clickButton('today');
                    expectOffset(0);
                });
                
                it("should center on today from a year behind", function() {
                    panel.replacePanes(-12);
                    expectOffset(-12);
                    
                    clickButton('today');
                    expectOffset(0);
                });
            });
            
            describe("with 3 panes", function() {
                beforeEach(function() {
                    makePanel({
                        panes: 3,
                        showTodayButton: true
                    });
                });
                
                it("should center on today from a month ahead", function() {
                    panel.switchPanes(1);
                    expectOffset(0, 'left');
                    expectOffset(1, 'front');
                    expectOffset(2, 'right');
                    
                    clickButton('today');
                    expectOffset(-1, 'left');
                    expectOffset(0, 'front');
                    expectOffset(1, 'right');
                });
                
                it("should center on today from a month behind", function() {
                    panel.switchPanes(-1);
                    expectOffset(-2, 'left');
                    expectOffset(-1, 'front');
                    expectOffset(0, 'right');
                    
                    clickButton('today');
                    expectOffset(-1, 'left');
                    expectOffset(0, 'front');
                    expectOffset(1, 'right');
                });
                
                it("should center on today from a year ahead", function() {
                    panel.replacePanes(12);
                    expectOffset(11, 'left');
                    expectOffset(12, 'front');
                    expectOffset(13, 'right');
                    
                    clickButton('today');
                    expectOffset(-1, 'left');
                    expectOffset(0, 'front');
                    expectOffset(1, 'right');
                });
                
                it("should center on today from a year behind", function() {
                    panel.replacePanes(-12);
                    expectOffset(-13, 'left');
                    expectOffset(-12, 'front');
                    expectOffset(-11, 'right');
                    
                    clickButton('today');
                    expectOffset(-1, 'left');
                    expectOffset(0, 'front');
                    expectOffset(1, 'right');
                });
            });
        });

        describe('cell click', function () {
            it('should handle clicking on inner cell', function () {
                makePanel();

                var cell = panel.getCellByDate(yesterday),
                    inner = Ext.fly(cell).child('.x-inner');

                clickCell(inner);

                waitsForEvent(cell, 'focus');

                expect(panel.getValue()).toEqual(yesterday);
            });

            it('should prevent clicking on disabled day', function () {
                makePanel({
                    autoConfirm: true,
                    disabledDays: [0, 6],
                    value: new Date(2010, 0, 13)
                });

                var cell = panel.getCellByDate(new Date(2010, 0, 16)),
                    inner = Ext.fly(cell).child('.x-inner'),
                    spy = jasmine.createSpy();

                panel.on('select', spy);
                clickCell(inner);

                expect(panel.getValue()).toEqual(new Date(2010, 0, 13));
                expect(spy).not.toHaveBeenCalled();
            });
        });
    });
    
    (jasmine.supportsTouch ? describe : xdescribe)("touch interaction", function() {
        beforeEach(function() {
            makePanel();
        });
        
        it("should focus the tapped cell", function() {
            var cell = panel.getCellByDate(yesterday);
            
            clickCell(cell);
            waitsForEvent(cell, 'focus');
        });
    });
});