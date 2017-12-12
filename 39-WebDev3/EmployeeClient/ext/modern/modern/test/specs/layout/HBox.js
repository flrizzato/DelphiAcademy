describe("Ext.layout.HBox", function(){
    var ct, c;

    function getX(index) {
        return ct.items.getAt(index).element.getX();
    }

    function getY(index) {
        return ct.items.getAt(index).element.getY();
    }

    function getWidth(index) {
        return ct.items.getAt(index).element.getWidth();
    }

    function getHeight(index) {
        return ct.items.getAt(index).element.getHeight();
    }

    afterEach(function(){
        ct = c = Ext.destroy(ct, c);
    });
    
    describe("defaults", function() {
        var layout;

        beforeEach(function() {
            ct = new Ext.container.Container({
                renderTo: Ext.getBody(),
                layout: 'hbox',
                width: 100,
                height: 100
            });
            layout = ct.getLayout();
        });

        afterEach(function() {
            layout = null;
        });
        
        it("should have align: stretch", function() {
            expect(layout.getAlign()).toBe('stretch');
        });
        
        it("should have constrainAlign: false", function() {
            expect(layout.getConstrainAlign()).toBe(false);
        });
        
        it("should have pack start", function() {
            expect(layout.getPack()).toBe('start');
        });
    });
    
    describe('sizes as percentages', function () {
        it('should correctly size items using percentages', function () {
            ct = Ext.widget({
                xtype: 'container',
                layout: {
                    type: 'hbox',
                    align: 'start'
                },
                width: 300,
                height: 200,
                renderTo: Ext.getBody(),
                autoSize: Ext.supports.PercentageSizeFlexBug ? false : null,
                items: [{
                    xtype: 'component',
                    width: '20%',
                    height: 100
                },{
                    xtype: 'component',
                    width: 30,
                    height: '75%'
                },{
                    xtype: 'component',
                    flex: 1,
                    height: '100%'
                },{
                    xtype: 'component',
                    flex: 2,
                    html: '<div style="height:50px"></div>'
                }]
            });

            expect(ct).toHaveLayout({
                el: { w: 300, h: 200 },
                items: {
                    0: { el: { xywh: '0 0 60 100' } },
                    1: { el: { xywh: '60 0 30 150' } },
                    2: { el: { xywh: '90 0 70 200' } },
                    3: { el: { xywh: '160 0 140 50' } }
                }
            });
        });
    });

    describe("nested box layouts", function () {
        var childConfig = {
            xtype : "component",
            style: 'border: 1px solid blue;',
            html : "child 1 content"
        };

        it("should handle auto width of nested boxes", function () {
            // make the child outside of any container and get its proper height:
            c = Ext.widget(childConfig);
            c.render(Ext.getBody());
            var height = c.getHeight();

            // now nest the child inside a box inside a box
            ct = Ext.widget({
                xtype: 'container',
                layout: "auto",
                style: 'padding: 10px',
                renderTo: Ext.getBody(),
                width: 500, height: 300,
                items: [{
                    xtype: 'container',
                    layout : {
                        type: "hbox",
                        align : "stretch"
                    },
                    style: 'border: 1px solid yellow',
                    items : [{
                        xtype: "container",
                        itemId: "column1Id",
                        layout: "vbox",
                        style: 'border: 1px solid red;',
                        flex : 1,
                        items: [Ext.apply({
                            itemId : "child1"
                        }, childConfig), {
                            xtype : "component",
                            itemId : "column1Child2Id",
                            style: 'border: 1px solid green;',
                            html : "child 2 content"
                        }]
                    },{
                        flex: 1,
                        style: 'border: 1px solid blue;'
                    }]
                }]
            });

            // make sure we get the same height for the nested version:
            var c1 = ct.down('#child1');
            expect(c1.getHeight()).toBe(height);
        });
    });
        
    it("should apply margin to components", function(){
        ct = new Ext.container.Container({
            width: 200,
            height: 200,
            renderTo: Ext.getBody(),
            defaultType: 'component',
            layout: {
                type: 'hbox',
                align: 'stretch'
            },
            defaults: {
                flex: 1,
                margin: 5
            },
            items: [{}, {}]
        });

        expect(getY(0)).toBe(5);
        expect(getX(0)).toBe(5);
        
        expect(getY(1)).toBe(5);
        expect(getX(1)).toBe(105);
    });
    
    describe("pack", function(){
        function makeCt (pack, cfg){
            ct = new Ext.container.Container(Ext.apply({
                defaultType: 'component',
                renderTo: Ext.getBody(),
                width: 600,
                height: 600,
                layout: {
                    type: 'hbox',
                    pack: pack
                },
                items: [{
                    width: 30
                }, {
                    width: 40
                }, {
                    width: 20
                }]
            }, cfg));
        }

        it("should pack at the left with pack: start", function(){
            makeCt('start');
            expect(getX(0)).toBe(0);
            expect(getX(1)).toBe(30);
            expect(getX(2)).toBe(70);
        });

        it("should pack in the middle with pack: center", function(){
            makeCt('center');
            expect(getX(0)).toBe(255);
            expect(getX(1)).toBe(285);
            expect(getX(2)).toBe(325);
        });

        it("should pack at the right with pack: end", function(){
            makeCt('end');
            expect(getX(0)).toBe(510);
            expect(getX(1)).toBe(540);
            expect(getX(2)).toBe(580);
        });

        it("should pack: justify", function() {
            makeCt('justify');
            expect(getX(0)).toBe(0);
            expect(getX(1)).toBe(285);
            expect(getX(2)).toBe(580);
        });

        it("should pack: space-between", function() {
            makeCt('space-between');
            expect(getX(0)).toBe(0);
            expect(getX(1)).toBe(285);
            expect(getX(2)).toBe(580);
        });

        it("should pack: space-around", function() {
            makeCt('space-around');
            expect(getX(0)).toBe(85);
            expect(getX(1)).toBe(285);
            expect(getX(2)).toBe(495);
        });

        it("should pack start with maxWidthed items", function () {
            // https://sencha.jira.com/browse/EXTJS-25260
            makeCt('start', {
                // These items must not have a "width", only a "maxWidth"
                items: [{
                    flex: 1,
                    maxWidth: 100
                }, {
                    flex: 1,
                    maxWidth: 100
                }, {
                    flex: 1,
                    maxWidth: 100
                }]
            });

            expect(getX(0)).toBe(0);
            expect(getX(1)).toBe(100);
            expect(getX(2)).toBe(200);

            expect(getWidth(0)).toBe(100);
            expect(getWidth(1)).toBe(100);
            expect(getWidth(2)).toBe(100);
        });

        it("should pack center with maxWidthed items", function() {
            // https://sencha.jira.com/browse/EXTJS-25260
            makeCt('center', {
                // These items must not have a "width", only a "maxWidth"
                items: [{
                    flex: 1,
                    maxWidth: 100
                }, {
                    flex: 1,
                    maxWidth: 100
                }, {
                    flex: 1,
                    maxWidth: 100
                }]
            });

            expect(getX(0)).toBe(150);
            expect(getX(1)).toBe(250);
            expect(getX(2)).toBe(350);

            expect(getWidth(0)).toBe(100);
            expect(getWidth(1)).toBe(100);
            expect(getWidth(2)).toBe(100);
        });

        it("should pack end with maxWidthed items", function () {
            // https://sencha.jira.com/browse/EXTJS-25260
            makeCt('end', {
                // These items must not have a "width", only a "maxWidth"
                items: [{
                    flex: 1,
                    maxWidth: 100
                }, {
                    flex: 1,
                    maxWidth: 100
                }, {
                    flex: 1,
                    maxWidth: 100
                }]
            });

            expect(getX(0)).toBe(300);
            expect(getX(1)).toBe(400);
            expect(getX(2)).toBe(500);

            expect(getWidth(0)).toBe(100);
            expect(getWidth(1)).toBe(100);
            expect(getWidth(2)).toBe(100);
        });
    });

    describe("reverse", function() {
        function makeCt (reverse){
            ct = new Ext.container.Container({
                defaultType: 'component',
                renderTo: Ext.getBody(),
                width: 600,
                height: 600,
                layout: {
                    type: 'hbox',
                    reverse: reverse
                },
                items: [{
                    flex: 1
                }, {
                    flex: 1
                }, {
                    flex: 1
                }]
            });
        }

        it("should not reverse with false", function() {
            makeCt(false);
            expect(getX(0)).toBe(0);
            expect(getX(1)).toBe(200);
            expect(getX(2)).toBe(400);
        });

        it("should reverse with true", function() {
            makeCt(true);
            expect(getX(2)).toBe(0);
            expect(getX(1)).toBe(200);
            expect(getX(0)).toBe(400);
        });
    });
    
    describe("align", function(){

        function makeCt(align, items, options) {
            options = options || {};
            ct = new Ext.container.Container({
                defaultType: 'component',
                renderTo: Ext.getBody(),
                width: ('width' in options) ? options.width : 600,
                height: ('height' in options) ? options.height : 600,
                autoScroll: !!options.autoScroll,
                autoSize: (options.autoSize != null) ? options.autoSize : null,
                layout: {
                    type: 'hbox',
                    align: align,
                    constrainAlign: !!options.constrainAlign
                },
                items: items
            });
        }
        
        describe('top/middle/bottom', function() {
        
            it("should keep items at the top when using align: start", function(){
                makeCt('start', [{
                    html: 'a'
                }, {
                    html: 'b'
                }]);
                expect(getY(0)).toBe(0);
                expect(getY(1)).toBe(0);
            });
        
            it("should align items in the middle when using align: center", function(){
                makeCt('center', [{
                    height: 100 
                }, {
                    height: 300
                }]);   
                expect(getY(0)).toBe(250);
                expect(getY(1)).toBe(150);
            });
            
             it("should keep items to the bottom when using align: end", function(){
                makeCt('end', [{
                    html: 'a'
                }, {
                    html: 'b'
                }]);
                expect(getY(0)).toBe(600 - getHeight(0));
                expect(getY(1)).toBe(600 - getHeight(1));
            });
            
            describe("constrainAlign", function(){
                function makeLongString(c, len) {
                    var out = [],
                        i = 0;
                        
                    for (; i < len; ++i) {
                        out.push(c);
                    }
                    return out.join('<br />');
                }
                
                it("should constrain a shrink wrapped item with align: top", function(){
                    makeCt('top', [{
                        html: makeLongString('A', 100)
                    }], {
                        constrainAlign: true,
                        autoSize: Ext.supports.PercentageSizeFlexBug ? false : null
                    });
                    expect(getHeight(0)).toBe(600);
                    expect(getY(0)).toBe(0);
                });
            
                it("should constrain a shrink wrapped item with align: middle", function(){
                    makeCt('middle', [{
                        html: makeLongString('A', 100)
                    }], {
                        constrainAlign: true,
                        autoSize: Ext.supports.PercentageSizeFlexBug ? false : null
                    });
                    expect(getHeight(0)).toBe(600);
                    expect(getY(0)).toBe(0);
                });
            
                it("should constrain a shrink wrapped item with align: bottom", function(){
                    makeCt('bottom', [{
                        html: makeLongString('A', 100)
                    }], {
                        constrainAlign: true,
                        autoSize: Ext.supports.PercentageSizeFlexBug ? false : null
                    });
                    expect(getHeight(0)).toBe(600);
                    expect(getY(0)).toBe(0);
                });
                
                it("should not constrain a fixed height item", function(){
                    makeCt('top', [{
                        html: 'A',
                        height: 1000,
                        autoSize: Ext.supports.PercentageSizeFlexBug ? false : null
                    }], {
                        constrainAlign: true
                    });
                    expect(getHeight(0)).toBe(1000);
                });
                
                it("should recalculate the left positions", function(){
                    makeCt('top', [{
                        html: makeLongString('A', 100)
                    }, {
                        html: 'B'
                    }], {
                        constrainAlign: true,
                        autoSize: Ext.supports.PercentageSizeFlexBug ? false : null
                    });
                    
                    expect(getX(0)).toBe(0);
                    expect(getX(1)).toBe(getWidth(0));  
                });
            });
        });
        
        describe("stretch", function() {
        
            it("should stretch all items to the size of the largest when using align: stretch", function(){
                c = new Ext.Component({
                    renderTo: Ext.getBody(),
                    html: 'a<br />b<br />c'
                });
            
                var expected = c.element.getHeight();
                c.destroy();

                makeCt('stretch', [{
                    html: 'a<br />b'
                }, {
                    html: 'a<br />b<br />c'
                }, {
                    html: 'a<br />b'
                }], { height: null });

                expect(getHeight(0)).toBe(expected);
                expect(getHeight(1)).toBe(expected);
                expect(getHeight(2)).toBe(expected);
            });
            
            it("should always use a fixed height over stretch", function(){
                makeCt('stretch', [{
                    height: 30
                }, {
                    html: 'a<br />b<br />c'
                }, {
                    html: 'a<br />b'
                }]);
            
                expect(getHeight(0)).toBe(30);
                expect(getHeight(1)).toBe(600);
                expect(getHeight(2)).toBe(600);
            });
            
            describe("minHeight", function() {
                it("should stretch an item with a minHeight", function(){
                    makeCt('stretch', [{
                        height: 30
                    }, {
                        minHeight: 5
                    }]);
                    expect(getHeight(0)).toBe(30);
                    expect(getHeight(1)).toBe(600);
                });
                
                it("should stretch to the item with the largest minHeight", function(){
                    makeCt('stretch', [{
                        minHeight: 30
                    }, {
                        minHeight: 50
                    }], { height: null });
                    expect(getHeight(0)).toBe(50);
                    expect(getHeight(1)).toBe(50);
                });
                
                it("should stretch a single item outside the bounds of the container", function(){
                    makeCt('stretch', [{
                        minHeight: 1000,
                        html: 'Content...'
                    }], {
                        scrollable: true
                    });
                    expect(getHeight(0)).toBe(1000);
                });
            });
            
            it("should respect a maxHeight", function(){
                makeCt('stretch', [{
                    height: 30
                }, {
                    maxHeight: 20
                }]);
                expect(getHeight(0)).toBe(30);
                expect(getHeight(1)).toBe(20);
            });
        });
        
        it("should stretch all items to the container height", function(){
            makeCt('stretch', [{
             }, {
             }]);
            expect(getHeight(0)).toBe(600);
            expect(getHeight(1)).toBe(600);
        });
    });
    
    describe("width", function(){
        function makeCt (items) {
            ct = new Ext.container.Container({
                renderTo: Ext.getBody(),
                width: 600,
                height: 100,
                defaultType: 'component',
                layout: {
                    type: 'hbox',
                    align: 'stretch'
                },
                items: items
            });
        }

        function getWidth (index) {
            return ct.items.getAt(index).el.getWidth();
        }
        
        describe("flex only", function(){
            it("should stretch a single flex item to the width of the container", function(){
                makeCt({
                    flex: 1
                });
                expect(getWidth(0)).toBe(600);
            });
        
            it("should stretch 3 equally flexed items equally", function(){
                makeCt([{
                    flex: 1
                }, {
                    flex: 1    
                }, {
                    flex: 1
                }]);
                expect(getWidth(0)).toBe(200);
                expect(getWidth(1)).toBe(200);
                expect(getWidth(2)).toBe(200);
            });
            
            it("should flex 2 items according to ratio", function(){
                makeCt([{
                    flex: 3    
                }, {
                    flex: 1
                }]);
                expect(getWidth(0)).toBe(450);
                expect(getWidth(1)).toBe(150);
            });
            
            it("should flex 4 items according to ratio", function(){
                makeCt([{
                    flex: 3    
                }, {
                    flex: 1
                }, {
                    flex: 3
                }, {
                    flex: 1
                }]);
                expect(getWidth(0)).toBe(225);
                expect(getWidth(1)).toBe(75);
                expect(getWidth(2)).toBe(225);
                expect(getWidth(3)).toBe(75);
            });
            
            it("should use flex as a ratio", function(){
                makeCt([{
                    flex: 4
                }, {
                    flex: 2
                }]);
                expect(getWidth(0)).toBe(400);
                expect(getWidth(1)).toBe(200);
            });
        });
        
        describe("fixed width only", function(){
            it("should set the width of a single item", function(){
                makeCt({
                    width: 200
                });    
                expect(getWidth(0)).toBe(200);
            });
            
            it("should set the width of multiple items", function(){
                makeCt([{
                    width: 500
                }, {
                    width: 50
                }]);    
                expect(getWidth(0)).toBe(500);
                expect(getWidth(1)).toBe(50);
            });
            
            it("should allow a single item to exceed the container width", function(){
                makeCt({
                    width: 900
                });
                expect(getWidth(0)).toBe(900);
            });

            it("should not allow a single item with shrink: 1 to exceed the container width", function () {
                makeCt({
                    width: 900,
                    flex: {
                        shrink: 1
                    }
                });
                expect(getWidth(0)).toBe(600);
            });
            
            it("should allow multiple items to exceed the container width", function(){
                makeCt([{
                    width: 400
                }, {
                    width: 400
                }]);
                expect(getWidth(0)).toBe(400);
                expect(getWidth(1)).toBe(400);
            });

            it("should not allow multiple items with shrink: 1 to exceed the container width", function () {
                makeCt([{
                    width: 400,
                    flex: {
                        shrink: 1
                    }
                }, {
                    width: 400,
                    flex: {
                        shrink: 1
                    }
                }]);
                expect(getWidth(0)).toBe(300);
                expect(getWidth(1)).toBe(300);
            });
        });

        describe("%age", function(){
            it("should be able to use %age width", function(){
                makeCt([{
                    width: '50%'
                }, {
                    width: '50%'
                }]);
                expect(getWidth(0)).toBe(300);
                expect(getWidth(1)).toBe(300);
            });
            
            it("should work with fixed width", function(){
                makeCt([{
                    width: '20%'
                }, {
                    width: 380
                }, {
                    width: 100
                }]);
                expect(getWidth(0)).toBe(120);
                expect(getWidth(1)).toBe(380);
                expect(getWidth(2)).toBe(100);
            });
            
            it("should work with flex", function(){
                makeCt([{
                    flex: 2
                }, {
                    width: '50%'
                }, {
                    flex: 1
                }]);    
                expect(getWidth(0)).toBe(200);
                expect(getWidth(1)).toBe(300);
                expect(getWidth(2)).toBe(100);
            });
        });
        
        describe("mixed", function(){
            it("should give any remaining space to a single flexed item", function(){
                makeCt([{
                    width: 200
                }, {
                    flex: 1
                }]);
                expect(getWidth(0)).toBe(200);
                expect(getWidth(1)).toBe(400);
            });
            
            it("should flex a single item with 2 fixed", function(){
                makeCt([{
                    width: 100
                }, {
                    flex: 1
                }, {
                    width: 300
                }]);
                expect(getWidth(0)).toBe(100);
                expect(getWidth(1)).toBe(200);
                expect(getWidth(2)).toBe(300);
            });
            
            it("should flex 2 items with 1 fixed", function(){
                makeCt([{
                    flex: 2
                }, {
                    width: 300
                }, {
                    flex: 1
                }]);    
                expect(getWidth(0)).toBe(200);
                expect(getWidth(1)).toBe(300);
                expect(getWidth(2)).toBe(100);
            });

            it("should give priority to flex over a fixed width", function(){
                makeCt([{
                    flex: 1,
                    width: 200
                }, {
                    flex: 1
                }]);

                expect(getWidth(0)).toBe(300);
                expect(getWidth(1)).toBe(300);
            });

        });

        describe("min/max", function(){
            it("should assign a 0 width if there is no more flex width", function(){
                makeCt([{
                    flex: 1
                }, {
                    width: 700
                }]);
                expect(getWidth(0)).toBe(0);
                expect(getWidth(1)).toBe(700);
            });
            
            it("should respect a minWidth on a flex even if there is no more flex width", function(){
                makeCt([{
                    flex: 1,
                    minWidth: 50
                }, {
                    width: 700
                }]);
                expect(getWidth(0)).toBe(50);
                expect(getWidth(1)).toBe(700);
            });
            
            it("should respect a minWidth on a flex even if there is no excess flex width", function(){
                makeCt([{
                    flex: 1,
                    maxWidth: 100
                }, {
                    width: 300
                }]);
                expect(getWidth(0)).toBe(100);
                expect(getWidth(1)).toBe(300);    
            });

            // Disabled in IE11 because it computes flex values incorrectly when using min-width
            (Ext.isIE11 ? xit : it)("should update flex values based on min constraint", function(){
                var c1 = new Ext.Component({
                    flex: 1,
                    minWidth: 500
                }), c2 = new Ext.Component({
                    flex: 1
                });
                makeCt([c1, c2]);
                expect(c1.el.getWidth()).toBe(500);
                expect(c2.el.getWidth()).toBe(100);
            });

            // Disabled in IE11 because it computes flex values incorrectly when using min-width
            (Ext.isIE11 ? xit : it)("should handle multiple min constraints", function(){
                 var c1 = new Ext.Component({
                    flex: 1,
                    minWidth: 250
                }), c2 = new Ext.Component({
                    flex: 1,
                    minWidth: 250
                }), c3 = new Ext.Component({
                    flex: 1
                });
                
                makeCt([c1, c2, c3]);
                expect(c1.el.getWidth()).toBe(250);
                expect(c2.el.getWidth()).toBe(250);
                expect(c3.el.getWidth()).toBe(100);
            });
            
            it("should update flex values based on max constraint", function(){
                var c1 = new Ext.Component({
                    flex: 1,
                    maxWidth: 100
                }), c2 = new Ext.Component({
                    flex: 1
                });
                makeCt([c1, c2]);
                expect(c1.el.getWidth()).toBe(100);
                expect(c2.el.getWidth()).toBe(500);
            });
            
            it("should update flex values based on multiple max constraints", function(){
                var c1 = new Ext.Component({
                    flex: 1,
                    maxWidth: 100
                }), c2 = new Ext.Component({
                    flex: 1,
                    maxWidth: 100
                }), c3 = new Ext.Component({
                    flex: 1
                });
                makeCt([c1, c2, c3]);
                expect(c1.el.getWidth()).toBe(100);
                expect(c2.el.getWidth()).toBe(100);
                expect(c3.el.getWidth()).toBe(400);
            });

            it("should give precedence to min constraints over flex when the min is the same", function() {
                var c1 = new Ext.Component({
                    flex: 1,
                    minWidth: 200
                }), c2 = new Ext.Component({
                    flex: 3,
                    minWidth: 200
                }), c3 = new Ext.Component({
                    flex: 1,
                    minWidth: 200
                });
                makeCt([c1, c2, c3]);
                expect(c1.el.getWidth()).toBe(200);
                expect(c2.el.getWidth()).toBe(200);
                expect(c3.el.getWidth()).toBe(200);
            });

            it("should give precedence to max constraints over flex when the max is the same", function() {
                var c1 = new Ext.Component({
                    flex: 1,
                    maxWidth: 100
                }), c2 = new Ext.Component({
                    flex: 3,
                    maxWidth: 100
                }), c3 = new Ext.Component({
                    flex: 1,
                    maxWidth: 100
                });
                makeCt([c1, c2, c3]);
                expect(c1.el.getWidth()).toBe(100);
                expect(c2.el.getWidth()).toBe(100);
                expect(c3.el.getWidth()).toBe(100);
            });

            describe("with %age", function() {
                it("should respect min constraints", function(){
                    makeCt([{
                        width: '10%',
                        minWidth: 400
                    },{
                        flex: 1
                    }]);
                    expect(getWidth(0)).toBe(400);
                    expect(getWidth(1)).toBe(200);
                });

                it("should respect max constraints", function(){
                    makeCt([{
                        width: '70%',
                        maxWidth: 200
                    },{
                        flex: 1
                    }]);
                    expect(getWidth(0)).toBe(200);
                    expect(getWidth(1)).toBe(400);
                });
            });
        });

        describe("flex config parsing", function() {
            var item;

            function makeFlex(flex) {
                ct = new Ext.Container({
                    renderTo: Ext.getBody(),
                    layout: 'hbox',
                    items: [{
                        xtype: 'component',
                        flex: flex || null
                    }]
                });

                item = ct.items.getAt(0);
            }

            it("should not set any flex values by default", function() {
                makeFlex();

                expect(item.el.getStyle('flex-grow')).toBe('0');
                expect(item.el.getStyle('flex-shrink')).toBe('0');
                expect(item.el.getStyle('flex-basis')).toBe('auto');
            });

            it("should parse flex-grow as a string", function() {
                makeFlex('1');

                expect(item.el.getStyle('flex-grow')).toBe('1');
                expect(item.el.getStyle('flex-shrink')).toBe('1');
                expect(parseInt(item.el.getStyle('flex-basis'))).toBe(0);
            });

            it("should parse flex-grow and flex-shrink as a string", function() {
                makeFlex('1 2');

                expect(item.el.getStyle('flex-grow')).toBe('1');
                expect(item.el.getStyle('flex-shrink')).toBe('2');
                expect(parseInt(item.el.getStyle('flex-basis'))).toBe(0);
            });

            it("should parse flex-grow, flex-shrink, and flex-basis as a string", function() {
                makeFlex('1 2 100px');

                expect(item.el.getStyle('flex-grow')).toBe('1');
                expect(item.el.getStyle('flex-shrink')).toBe('2');
                expect(item.el.getStyle('flex-basis')).toBe('100px');
            });

            it("should parse flex-grow as an object", function () {
                makeFlex({
                    grow: 1
                });

                expect(item.el.getStyle('flex-grow')).toBe('1');
                expect(item.el.getStyle('flex-shrink')).toBe('0');
                expect(item.el.getStyle('flex-basis')).toBe('auto');
            });

            it("should parse flex-shrink as an object", function () {
                makeFlex({
                    shrink: 2
                });

                expect(item.el.getStyle('flex-grow')).toBe('0');
                expect(item.el.getStyle('flex-shrink')).toBe('2');
                expect(item.el.getStyle('flex-basis')).toBe('auto');
            });

            it("should parse flex-basis as an object", function () {
                makeFlex({
                    basis: '100px'
                });

                expect(item.el.getStyle('flex-grow')).toBe('0');
                expect(item.el.getStyle('flex-shrink')).toBe('0');
                expect(item.el.getStyle('flex-basis')).toBe('100px');
            });

            it("should parse flex-grow, flex-shrink, and flex-basis as an object", function () {
                makeFlex({
                    grow: 1,
                    shrink: 2,
                    basis: '100px'
                });

                expect(item.el.getStyle('flex-grow')).toBe('1');
                expect(item.el.getStyle('flex-shrink')).toBe('2');
                expect(item.el.getStyle('flex-basis')).toBe('100px');
            });
        });
    });

    describe("shrink wrap width", function(){
        var testHtml = 'a a',
            measure;

        function makeCt (items) {
            ct = new Ext.Container({
                floated: true,
                hidden: false,
                height: 100,
                defaultType: 'component',
                style: 'font: 50px monospace', // 30px character width
                layout: {
                    type: 'hbox',
                    align: 'stretch'
                },
                items: items
            });
            var c = new Ext.Component({
                style: 'font: 50px monospace',
                floated: true,
                hidden: false,
                html: testHtml,
                hidden: false
            });
            measure = c.el.getWidth();
            c.destroy();
        }

        function getWidth (index) {
            return ct.items.getAt(index).el.getWidth();
        }
        
        describe("flex only", function(){
            it("should shrink wrap a single flex item", function(){
                makeCt({
                    flex: '1 1 auto',
                    html: testHtml // make sure we can shrinkwrap without wrapping the text
                });
                expect(getWidth(0)).toBe(measure);
                expect(ct.el.getWidth()).toBe(measure);
            });
        
            it("should shrink wrap 3 flexed items", function(){
                makeCt([{
                    flex: '1 1 auto',
                    html: testHtml // shrink wrap without wrapping the text
                }, {
                    flex: '1 1 auto',
                    html: testHtml // shrink wrap without wrapping the text
                }, {
                    flex: '2 1 auto',
                    html: testHtml // shrink wrap without wrapping the text
                }]);
                expect(getWidth(0)).toBe(measure);
                expect(getWidth(1)).toBe(measure);
                expect(getWidth(2)).toBe(measure);
                expect(ct.el.getWidth()).toBeWithin(2, measure * 3);
            });
        });
        
        describe("fixed width only", function(){
            it("should set the width of a single item", function(){
                makeCt({
                    width: 200
                });    
                expect(getWidth(0)).toBe(200);
                expect(ct.el.getWidth()).toBe(200);
            });
            
            it("should shrink wrap multiple items", function(){
                makeCt([{
                    width: 500
                }, {
                    width: 50
                }]);    
                expect(getWidth(0)).toBe(500);
                expect(getWidth(1)).toBe(50);
                expect(ct.el.getWidth()).toBe(550);
            });
        });
        
        describe("mixed", function(){
            it("should shrink wrap one flexed item, one auto-width item, and one fixed width item", function(){
                makeCt([{
                    flex: '1 1 auto',
                    html: testHtml // shrink wrap without wrapping the text
                }, {
                    flex: { shrink: 0 },
                    html: testHtml // shrink wrap without wrapping the text
                }, {
                    width: 200
                }]);
                expect(getWidth(0)).toBe(measure);
                expect(getWidth(1)).toBe(measure);
                expect(getWidth(2)).toBe(200);
                expect(ct.el.getWidth()).toBeWithin(1, measure * 2 + 200);
            });
        });
    });
    
    (Ext.isIE11 ? xit : it)("should size correctly with docked items & a configured parallel size & shrinkWrap perpendicular size", function() {
        ct = new Ext.Container({
            floated: true,
            hidden: false,
            border: false,
            layout: 'hbox',
            width: 150,
            dockedItems: [{
                dock: 'left',
                xtype: 'component',
                html: 'X'    
            }],
            items: [{
                xtype: 'component',
                html: '<div style="height: 50px;"></div>'
            }]
        });
        expect(ct.el.getWidth()).toBe(150);
        expect(ct.el.getHeight()).toBe(50);
    });
});
