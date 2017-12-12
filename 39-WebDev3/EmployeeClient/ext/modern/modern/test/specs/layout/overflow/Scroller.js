topSuite("Ext.layout.overflow.Scroller", [
    'Ext.Container',
    'Ext.layout.HBox',
    'Ext.Button'
], function() {
    var ct;

    function makeItems(n) {
        var ret = [],
            i;

        for (i = 1; i <= n; ++i) {
            ret.push({
                xtype: 'button',
                text: 'Text' + i
            });
        }
        return ret;
    }

    afterEach(function() {
        ct = Ext.destroy(ct);
    });

    describe("destruction", function() {
        it("should clean up any tools", function() {
            var count = Ext.ComponentManager.getCount();

            ct = new Ext.Container({
                renderTo: Ext.getBody(),
                width: 400,
                items: makeItems(10),
                layout: {
                    type: 'hbox',
                    overflow: {
                        type: 'scroller',
                        arrows: true
                    }
                }
            });
            waitsFor(function() {
                return ct.query('tool').some(function(t) {
                    return t.isVisible();
                });
            });

            runs(function() {
                var overflow = ct.getLayout().getOverflow();
                ct.destroy();
                expect(overflow.destroyed).toBe(true);
                expect(Ext.ComponentManager.getCount()).toBe(count);
            });
        });
    });
});