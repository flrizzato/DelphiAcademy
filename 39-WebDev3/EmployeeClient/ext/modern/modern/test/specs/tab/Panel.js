topSuite('Ext.tab.Panel', ['Ext.Panel'], function() {
    var panel;
    
    function makePanel(config, items) {
        items = items || [{
            xtype: 'panel',
            itemId: 'foo',
            title: 'foo',
            html: 'lorem ipsum foo baroo'
        }, {
            xtype: 'panel',
            itemId: 'bar',
            title: 'bar',
            html: 'blergo zumbo shmorem gypsum'
        }];
        
        config = Ext.apply({
            renderTo: document.body,
            items: items
        }, config);
        
        panel = new Ext.tab.Panel(config);
        
        return panel;
    }
    
    afterEach(function() {
        panel = Ext.destroy(panel);
    });
    
    describe("card behavior", function() {
        var fooTab, fooCard, barTab, barCard;
        
        beforeEach(function() {
            makePanel();
            
            fooCard = panel.down('#foo');
            fooTab = fooCard.tab;
            
            barCard = panel.down('#bar');
            barTab = barCard.tab;
        });
        
        afterEach(function() {
            fooTab = fooCard = barTab = barCard = null;
        });
        
        describe("setTitle", function() {
            beforeEach(function() {
                fooCard.setTitle('throbbe');
            });
            
            it("should update tab text", function() {
                expect(fooTab.getText()).toBe('throbbe');
            });
        });
    });

    describe("active item", function() {
        it("should default the active item to the first item", function() {
            makePanel();

            expect(panel.getActiveItem()).toBe(panel.getInnerItems()[0]);
        });

        it("should be able to set the active item initially", function() {
            makePanel({
                activeItem: 1
            });

            expect(panel.getActiveItem()).toBe(panel.getInnerItems()[1]);
        });

        it("should set the last item as active if the active item is out of bounds", function() {
            makePanel({
                activeItem: 3
            });

            expect(panel.getActiveItem()).toBe(panel.getInnerItems()[1]);
        });

        it("should not change the active item if we add a new item", function() {
            makePanel({
                activeItem: 1
            });

            panel.add({
                xtype: 'panel',
                itemId: 'hello',
                title: 'hello',
                html: 'lorem ipsum foo baroo'
            });

            expect(panel.getActiveItem()).toBe(panel.getInnerItems()[1]);
        });
    });
    
    describe("closable tabs", function() {
        it("should support creating with closable child panel", function() {
            makePanel(null, {
                xtype: 'panel',
                title: 'foo',
                closable: true
            });
            
            expect(panel.getActiveItem()).toBe(panel.getInnerItems()[0]);
        });
    });
});
