topSuite("Ext.field.Hidden", function() {
    var field;

    function makeField(cfg) {
        field = new Ext.field.Hidden(Ext.apply({
            renderTo: Ext.getBody()
        }, cfg));
    }

    it("should default hidden to true", function() {
        makeField();
        expect(field.getHidden()).toBe(true);
    });

    afterEach(function() {
        field = Ext.destroy(field);
    });
});