topSuite("Ext.panel.Collapsible", [
    'Ext.app.ViewModel', 
    'Ext.Button',
    'Ext.panel.Collapser'
], function() {
    var panel;

    function makePanel(config) {
        config = config || {};

        // Render unless floated.
        // Floateds insert themselves into the DOM
        if (!config.floated && !config.hasOwnProperty('renderTo')) {
            config.renderTo = Ext.getBody();
        }

        panel = new Ext.panel.Panel(Ext.apply({
            width: 400,
            height: 400,
            title: 'Test Panel'
        }, config));
    }

    afterEach(function () {
        panel = Ext.destroy(panel);
    });

    // TODO: These tests need to be expanded on by a lot.

    describe("configuring", function() {
        it("should not be collapsible by default", function() {
            makePanel();
            expect(panel.getCollapsed()).toBe(false);
            expect(panel.getCollapsible()).toBeNull();
        });

        it("should have a collapser if true", function() {
            makePanel({
                collapsible: true
            });
            expect(panel.getCollapsed()).toBe(false);
            expect(panel.getCollapsible() instanceof Ext.panel.Collapser).toBe(true);
        });

        it("should be able to pass a direction", function() {
            makePanel({
                collapsible: 'left'
            });
            expect(panel.getCollapsed()).toBe(false);
            expect(panel.getCollapsible().getDirection()).toBe('left');
        });

        it("should be able to pass a config", function() {
            makePanel({
                collapsible: {
                    dynamic: true
                }
            });
            expect(panel.getCollapsed()).toBe(false);
            expect(panel.getCollapsible().getDynamic()).toBe(true);
        });

        it("should be able to start collapsed", function() {
            makePanel({
                collapsible: true,
                collapsed: true
            });
            expect(panel.element.getHeight()).toBeLessThan(400);
        });
    });
    
    describe('events', function () {
        function expandCollapse(collapse) {
            var action = collapse ? 'collapse' : 'expand',
                event  = 'before' + action,
                spy;
            
            describe(event, function () {
                it('should prevent ' + action + ' if returning false', function (done) {
                    makePanel({
                        collapsible : true,
                        collapsed   : !collapse
                    });
                    
                    spy = spyOnEvent(panel, event).andReturn(false);
                    panel.toggleCollapsed(collapse).then(function () {
                        expect(spy).toHaveBeenCalled();
                        expect(panel.getCollapsed()).toBe(!collapse);
                    }).then(done).done();
                });
                
                it('should not prevent ' + action + ' if nothing is returned', function (done) {
                    makePanel({
                        collapsible : true,
                        collapsed   : !collapse
                    });
                    
                    spy = spyOnEvent(panel, event).andCallThrough();
                    panel.toggleCollapsed(collapse).then(function () {
                        expect(spy).toHaveBeenCalled();
                        expect(panel.getCollapsed()).toBe(collapse);
                    }).then(done).done();
                });
            });
        }
        
        // beforecollapse
        expandCollapse(true);
        
        // beforeexpand
        expandCollapse(false);
    });
});
