topSuite("Ext.field.Slider", function() {
    var field;

    function createField(cfg) {
        field = new Ext.field.Slider(Ext.apply({
            renderTo: Ext.getBody(),
            width: 400
        }, cfg));
    }

    afterEach(function() {
        field = Ext.destroy(field);
    });

    describe('readOnly', function() {
        beforeEach(function() {
            createField({
                readOnly: true,
                value: 50,
                minValue: 0,
                maxValue: 100
            });
        });

        it('should not move thumb on tap', function() {
            var slider = field.getSlider();

            waitsFor(function() {
                return slider.getThumb().element.getX() > 0;
            });
            runs(function() {
                jasmine.fireMouseEvent(field.getSlider().element, 'click', 10, 10);
                expect(slider.getValue()).toBe(50);
            });
        });
    });
});
