/* global spyOn, expect, Ext */

topSuite("Ext.picker.Slot", ['Ext.viewport.Default', 'Ext.picker.Picker'], function() {
    var picker, viewport, slot;

    afterEach(function () {
        Ext.Viewport = viewport = picker = slot = Ext.destroy(slot, picker, viewport, Ext.Viewport);
    });

    function makePicker (value, dataSize) {
        dataSize = dataSize || 100;

        viewport = Ext.Viewport = new Ext.viewport.Default();
        picker = Ext.create('Ext.picker.Picker', {
            slots: [{
                name: 'slot1',
                data : (function() {
                    var data = [], i;
                    for(i=0; i<dataSize; i++) {
                        data.push({text: i, value: i});
                    }
                    return data;
                })()        
            }],
            value: value ? value : null
        });
        slot = picker.getAt(0);
        viewport.add(picker);       
    } 

    function getBarIndex (bar) {
        var y = slot.getScrollable().getPosition().y,
            barHeight = bar.dom.getBoundingClientRect().height;

        return Math.round(y/barHeight);
    }

    describe("initial value", function () {
        beforeEach(function () {
            makePicker({slot1: 45});
        });

        it("should be set when the element is initially resized", function () {
            var scrollComplete = false,
                spy;
            
            picker.show(false);

            spy = spyOn(slot, 'onResize').andCallThrough();
            slot.getScrollable().on('scrollend', function () {
                scrollComplete = true;
            });     

            waitsFor(function() {
                return scrollComplete;
            }, 'slot to scroll selection into view', 800);
            
            waitsForSpy(spy);

            runs(function () {
                expect(spy).toHaveBeenCalled();
                expect(spy.callCount).toBe(1);
            });
            
        });

        // The following 2 tests are really unreliable and they fail ~70%
        // of the time. Need to look into why
        xit("should be scrolled into view and aligned with bar", function () {
            var scrollComplete = false,
                bar, barIndex;

            picker.show(false);

            slot.getScrollable().on('scrollend', function () {
                scrollComplete = true;
            });                     

            waitsFor(function () {
                return scrollComplete;
            }, 'slot to scroll selection into view', 800);
            runs(function () {
                bar = picker.bar;
                barIndex = getBarIndex(bar);
                // bar should be aligned with the selected item
                expect(barIndex).toBe(45);
            });
        });

        xit("should scroll to selection if view is scrolled, no new selection is made, and picker is re-shown", function () {
            var bar, scrollable;

            picker.show(false);

            bar = picker.bar;
            scrollable = slot.getScrollable();
            
            waitsForEvent(scrollable, 'scrollend', 'slot to scroll selection into view', 800);

            runs(function () {
                // item 45 should be seleted (the default)
                expect(getBarIndex(bar)).toBe(45);

                // now let's mimic a scroll to the very top of the list
                scrollable.scrollTo(0, 0);
            });

            waitsForEvent(scrollable, 'scrollend', 'scroll to top of scrollable area', 800);

            runs(function () {
                expect(scrollable.getPosition().y).toBe(0);

                // now let's simulate the scroll to the top, but the picker is dimissed with no selection made
                picker.hide(false);
                // now let's re-open the picker
                picker.show(false);
            });
            
            waits(800);

            runs(function () {
                // Wait for the original selection should be scrolled into view, regardless of the previous scroll pos
                expect(getBarIndex(bar)).toBe(45);
            }, 'slot to scroll selection into view');
        });
    });

    describe("selection", function () {
        it("should make selection if value is found in store", function () {
            var scrollComplete = false,
                scrollable;

            makePicker({slot1: 45});
            spyOn(slot, 'select');
            scrollable = slot.getScrollable();
            scrollable.on('scrollend', function () {
                scrollComplete = true;
            });

            picker.show(false);

            waitsFor(function () {
                return scrollComplete;
            });
            runs(function () {
                expect(slot.select).toHaveBeenCalled();
            });         
        });

        it("should not make selection if value is not found in store", function () {
            makePicker({slot1: 255});
            spyOn(slot, 'select');

            picker.show(false);

            waitsFor(function () {
                // since the default index will be 0, no scrolling will occur
                // so we need to wait until view items are available and selectedIndex is set to 0
                return slot.getViewItems().length && slot.selectedIndex === 0;
            });
            runs(function () {
                expect(slot.select).not.toHaveBeenCalled();
            });
        });

        it('should not deselect a selected value', function () {
            var scrollable;

            makePicker({
                slot1: 45
            });

            picker.show(false);

            scrollable = slot.getScrollable();

            waitsForEvent(scrollable, 'scrollend', 'slot to scroll selection into view', 800);

            runs(function () {
                var item = slot.dataItems[45];

                jasmine.fireMouseEvent(item, 'click');

                expect(Ext.fly(item).hasCls('x-selected')).toBe(true);
            });
        });
    });
});
