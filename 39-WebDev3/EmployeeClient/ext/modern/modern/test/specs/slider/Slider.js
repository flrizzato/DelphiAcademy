topSuite("Ext.slider.Slider", function() {
    var slider;

    function createField(config) {
        slider = new Ext.slider.Slider(config);
    }

    afterEach(function() {
        slider = Ext.destroy(slider);
    });

    describe('value', function() {
        it('should be a number, in case a number was given', function() {
            createField({value: 50});
            var value = slider.getValue();

            expect(value).toBe(50);
        });

        it('should be an array, in case a number was given and `valueIsArray` is `true`', function() {
            createField({
                value: 50,
                valueIsArray: true
            });
            var value = slider.getValue();

            expect(value[0]).toBe(50);
        });

        it('should be an array, in case an array was given', function() {
            createField({value: [30, 70]});
            var value = slider.getValue();

            expect(value[0]).toBe(30);
            expect(value[1]).toBe(70);
        });

        it('should clamp value to minValue and maxValue', function() {
            createField({
                renderTo: document.body,
                width: 200,
                value: 50,
                minValue: 0,
                maxValue: 100
            });

            slider.setValue(200);
            expect(slider.getValue()).toBe(100);
            slider.setValue(-1);
            expect(slider.getValue()).toBe(0);
        });

        it("should position correctly immediately at render time", function() {
            createField({
                renderTo: Ext.getBody(),
                width: 200,
                value: 50
            });
            var t = slider.getThumb(),
                w = t.element.getWidth();

            expect(t.element.getX()).toBeApprox(100 - (w / 2), 2);
        });
    });

});