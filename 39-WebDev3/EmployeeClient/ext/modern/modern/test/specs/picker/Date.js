topSuite("Ext.picker.Date", [
    'Ext.viewport.Viewport'
], function() {
    var datePicker;

    jasmine.usesViewport();  // setup in beforeAll, teardown in afterAll

    function makeDatePicker (value, slotCfg) {
        var cfg = {
            value: value || null
        };

        if (slotCfg) {
            cfg.slotOrder = slotCfg;
        }

        datePicker = Ext.create('Ext.picker.Date', cfg);

        Ext.Viewport.add(datePicker);
    } 
    
    afterEach(function() {
        datePicker = Ext.destroy(datePicker);
    });

    describe("create", function() {
        it("should assign an initial value if one was specified in the config", function() {
            var date = new Date();

            makeDatePicker(date);

            datePicker.show(false);

            expect(datePicker.getValue()).toEqual(Ext.Date.clearTime(date));
        });
    });

    describe("configuration", function() {
        it("should allow to be created without a day slow", function() {
            var date = new Date();

            makeDatePicker(date, ["year", "month" ]);

            expect(function() {
                datePicker.show();
            }).not.toThrow();
        });
    });
});
