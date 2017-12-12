topSuite("Ext.carousel.Carousel", 
    ['Ext.form.Panel', 'Ext.field.Text'],
 function() {
    var carousel;

    function makeCarousel(cfg) {
        carousel = new Ext.carousel.Carousel(Ext.apply({
            renderTo: Ext.getBody(),
            width: 400,
            height: 400
        }, cfg));
    }

    function makeCarouselWithItems(n, cfg) {
        var items = [],
            i;

        for (i = 1; i <= n; ++i) {
            items.push({
                html: 'Carousel' + i
            });
        }

        makeCarousel(Ext.apply({
            items: items
        }, cfg));
    }

    afterEach(function() {
        carousel = Ext.destroy(carousel);
    });

    describe("reference", function() {
        it("should be able to look up components by reference", function() {
            makeCarousel({
                referenceHolder: true,
                items: [{
                    reference: 'foo',
                    id: 'carouselFoo'
                }, {
                    reference: 'bar',
                    id: 'carouselBar'
                }]
            });

            expect(carousel.lookup('foo').id).toBe('carouselFoo');
            expect(carousel.lookup('bar').id).toBe('carouselBar');
        });
    });

    describe("querying items", function() {
        var form;

        beforeEach(function() {
            form = Ext.create('Ext.form.Panel',{
                renderTo: document.body,
                width: 500,
                height: 500
            });
        });

        function addPages(n, items) {
            for (var i = 1; i <= n; i++) {
                items.getAt(i).add([
                    { xtype: 'textfield', name: 'a', value: 'foo' },
                    { xtype: 'textfield', name: 'b', value: 'bar' },
                    { xtype: 'textfield', name: 'c', value: 'test' }
                ]);
            }
        }

        function runQueryTests(n) {
            beforeEach(function() {
                makeCarouselWithItems(n, {
                    renderTo: null
                });

                form.add(carousel);
            });

            afterEach(function() {
                form.destroy();
            });

            it("should work with Component Query " + n + " pages", function() {
                addPages(n, carousel.getItems());
                expect(Ext.ComponentQuery.query('carousel field').length).toBe(3 * n);
            });

            it("should work with form getValues" + n + " pages", function(){ 
                addPages(n, carousel.getItems());
                var values = form.getValues();
                expect((Ext.isString(values.a) ? [values.a] : values.a).length).toBe(n);
                expect((Ext.isString(values.b) ? [values.b] : values.b).length).toBe(n);
                expect((Ext.isString(values.c) ? [values.c] : values.c).length).toBe(n);
            });
        }

        runQueryTests(1);
        runQueryTests(2);
        runQueryTests(10);
    });
});
