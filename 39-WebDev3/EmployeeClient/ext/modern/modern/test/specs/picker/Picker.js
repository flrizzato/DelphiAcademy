topSuite("Ext.picker.Picker", ['Ext.picker.Date', 'Ext.field.Select', 'Ext.viewport.Default'], function() {
    var vp, picker, selectField, datePicker;

    function createPicker (cfg) {
        return picker = Ext.factory(cfg, Ext.picker.Picker, picker);
    }

    beforeEach(function () {
        Ext.Viewport = new Ext.viewport.Default();
    });

    afterEach(function() {
        Ext.Viewport = picker = selectField = datePicker =
            Ext.destroy(picker, selectField, datePicker, Ext.Viewport);
    });

    describe('value', function () {
        it('should set value on construction', function () {
            createPicker({
                value: {
                    limit_speed: 100
                },
                slots: [{
                    name: 'limit_speed',
                    title: 'Speed',
                    data: [
                        { text : '50 KB/s', value: 50 },
                        { text : '100 KB/s', value: 100 },
                        { text : '200 KB/s', value: 200 },
                        { text : '300 KB/s', value: 300 }
                    ]
                }]
            });

            var slot = picker.down('[name=limit_speed]');

            expect(slot.getValue()).toBe(100);
        });

        it('should set value using setValue', function () {
            createPicker({
                slots: [{
                    name: 'limit_speed',
                    title: 'Speed',
                    data: [
                        { text : '50 KB/s', value: 50 },
                        { text : '100 KB/s', value: 100 },
                        { text : '200 KB/s', value: 200 },
                        { text : '300 KB/s', value: 300 }
                    ]
                }]
            });

            picker.setValue({
                limit_speed: 200
            });

            var slot = picker.down('[name=limit_speed]');

            expect(slot.getValue()).toBe(200);
        });
    });
});
