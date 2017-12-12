describe("Ext.layout.VBox", function(){
    var ct, c, hasWebkitHeightConstraintBug;

    (function() {
        // Detects https://bugs.webkit.org/show_bug.cgi?id=167240
        // for which there is currently no workaround
        var el = Ext.getBody().appendChild({
            html: '<div style="width:10px;height:10px;display:flex;flex-direction:column;">' +
                      '<div style="height:20px;"></div>' +
                  '</div>'
        });

        hasWebkitHeightConstraintBug = (el.dom.firstChild.firstChild.offsetHeight === 20);

        el.destroy();
    })();

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
                layout: 'vbox',
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

        it("should have constrainAlign: false", function () {
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
                    type: 'vbox',
                    align: 'start'
                },
                height: 300,
                width: 200,
                renderTo: Ext.getBody(),
                autoSize: Ext.supports.PercentageSizeFlexBug ? false : null,
                items: [{
                    xtype: 'component',
                    height: '20%',
                    width: 100
                }, {
                    xtype: 'component',
                    height: 30,
                    width: '75%'
                }, {
                    xtype: 'component',
                    flex: 1,
                    width: '100%'
                }, {
                    xtype: 'component',
                    flex: 2,
                    html: '<div style="width:50px"></div>'
                }]
            });

            expect(ct).toHaveLayout({
                el: {w: 200, h: 300},
                items: {
                    0: {el: {xywh: '0 0 100 60'}},
                    1: {el: {xywh: '0 60 150 30'}},
                    2: {el: {xywh: '0 90 200 70'}},
                    3: {el: {xywh: '0 160 50 140'}}
                }
            });
        });
    });

    it("should apply margin to components", function(){
        ct = new Ext.container.Container({
            width: 200,
            height: 200,
            renderTo: Ext.getBody(),
            defaultType: 'component',
            layout: {
                type: 'vbox',
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
        
        expect(getY(1)).toBe(105);
        expect(getX(1)).toBe(5);
    });
    
    describe("pack", function(){
        function makeCt(pack, cfg) {
            ct = new Ext.container.Container(Ext.apply({
                defaultType: 'component',
                renderTo: Ext.getBody(),
                width: 600,
                height: 600,
                layout: {
                    type: 'vbox',
                    pack: pack
                },
                items: [{
                    height: 30
                }, {
                    height: 40
                }, {
                    height: 20
                }]
            }, cfg));
        }
            
        function getY(index) {
            return ct.items.getAt(index).el.getY();    
        }
        
        it("should pack at the top with pack: start", function(){
            makeCt('start');
            expect(getY(0)).toBe(0);
            expect(getY(1)).toBe(30);
            expect(getY(2)).toBe(70);
        });
        
        it("should pack in the middle with pack: center", function(){
            makeCt('center');
            expect(getY(0)).toBe(255);
            expect(getY(1)).toBe(285);
            expect(getY(2)).toBe(325);
        });
        
        it("should pack at the bottom with pack: cend", function(){
            makeCt('end');
            expect(getY(0)).toBe(510);
            expect(getY(1)).toBe(540);
            expect(getY(2)).toBe(580);
        });

        it("should pack: justify", function () {
            makeCt('justify');
            expect(getY(0)).toBe(0);
            expect(getY(1)).toBe(285);
            expect(getY(2)).toBe(580);
        });

        it("should pack: space-between", function () {
            makeCt('space-between');
            expect(getY(0)).toBe(0);
            expect(getY(1)).toBe(285);
            expect(getY(2)).toBe(580);
        });

        it("should pack: space-around", function () {
            makeCt('space-around');
            expect(getY(0)).toBe(85);
            expect(getY(1)).toBe(285);
            expect(getY(2)).toBe(495);
        });

        it("should pack start with maxHeighted items", function () {
            // https://sencha.jira.com/browse/EXTJS-25260
            makeCt('start', {
                // These items must not have a "height", only a "maxHeight"
                items: [{
                    flex: 1,
                    maxHeight: 100
                }, {
                    flex: 1,
                    maxHeight: 100
                }, {
                    flex: 1,
                    maxHeight: 100
                }]
            });

            expect(getY(0)).toBe(0);
            expect(getY(1)).toBe(100);
            expect(getY(2)).toBe(200);

            expect(getHeight(0)).toBe(100);
            expect(getHeight(1)).toBe(100);
            expect(getHeight(2)).toBe(100);
        });

        it("should pack center with maxHeighted items", function () {
            // https://sencha.jira.com/browse/EXTJS-25260
            makeCt('center', {
                // These items must not have a "height", only a "maxHeight"
                items: [{
                    flex: 1,
                    maxHeight: 100
                }, {
                    flex: 1,
                    maxHeight: 100
                }, {
                    flex: 1,
                    maxHeight: 100
                }]
            });

            expect(getY(0)).toBe(150);
            expect(getY(1)).toBe(250);
            expect(getY(2)).toBe(350);

            expect(getHeight(0)).toBe(100);
            expect(getHeight(1)).toBe(100);
            expect(getHeight(2)).toBe(100);
        });

        it("should pack end with maxHeighted items", function () {
            // https://sencha.jira.com/browse/EXTJS-25260
            makeCt('end', {
                // These items must not have a "height", only a "maxHeight"
                items: [{
                    flex: 1,
                    maxHeight: 100
                }, {
                    flex: 1,
                    maxHeight: 100
                }, {
                    flex: 1,
                    maxHeight: 100
                }]
            });

            expect(getY(0)).toBe(300);
            expect(getY(1)).toBe(400);
            expect(getY(2)).toBe(500);

            expect(getHeight(0)).toBe(100);
            expect(getHeight(1)).toBe(100);
            expect(getHeight(2)).toBe(100);
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
                    type: 'vbox',
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
            expect(getY(0)).toBe(0);
            expect(getY(1)).toBe(200);
            expect(getY(2)).toBe(400);
        });

        it("should reverse with true", function() {
            makeCt(true);
            expect(getY(2)).toBe(0);
            expect(getY(1)).toBe(200);
            expect(getY(0)).toBe(400);
        });
    });
    
    describe("align", function(){
        function makeCt(align, items, options) {
            options = options || {};
            ct = new Ext.container.Container({
                defaultType: 'component',
                renderTo: options.floated ? null : Ext.getBody(),
                width: ('width' in options) ? options.width : 600,
                height: ('height' in options) ? options.height : 600,
                autoScroll: !!options.autoScroll,
                floated: options.floated || null,
                hidden: false,
                autoSize: (options.autoSize != null) ? options.autoSize : null,
                layout: {
                    type: 'vbox',
                    align: align,
                    constrainAlign: !!options.constrainAlign
                },
                items: items
            });
        }
        
        describe("left/center/right", function() {
        
            it("should keep items at the left when using align: start", function(){
                makeCt('start', [{
                    html: '<div style="width: 20px"></div>'
                }, {
                    html: '<div style="width: 40px"></div>'
                }]);
                expect(getX(0)).toBe(0);
                expect(getX(1)).toBe(0);
            });
        
            it("should align items in the middle when using align: center", function(){
                makeCt('center', [{
                    width: 100 
                }, {
                    width: 300
                }]);   
                expect(getX(0)).toBe(250);
                expect(getX(1)).toBe(150);
            });
        
            it("should keep items to the right when using align: end", function(){
                makeCt('end', [{
                    html: '<div style="width: 20px"></div>'
                }, {
                    html: '<div style="width: 40px"></div>'
                }]);
                expect(getX(0)).toBe(600 - getWidth(0));
                expect(getX(1)).toBe(600 - getWidth(1));
            });
            
            describe("constrainAlign", function(){
                function makeLongString(c, len) {
                    var out = [],
                        i = 0;
                        
                    for (; i < len; ++i) {
                        out.push(c);
                    }
                    return out.join(' ');
                }
                
                it("should constrain a shrink wrapped item with align: left", function(){
                    makeCt('left', [{
                        html: makeLongString('A', 100)
                    }], {
                        constrainAlign: true,
                        autoSize: Ext.supports.PercentageSizeFlexBug ? false : null
                    });
                    expect(getWidth(0)).toBe(600);
                    expect(getX(0)).toBe(0);
                });
                
                it("should constrain a shrink wrapped item with align: center", function(){
                    makeCt('center', [{
                        html: makeLongString('A', 100)
                    }], {
                        constrainAlign: true,
                        autoSize: Ext.supports.PercentageSizeFlexBug ? false : null
                    });
                    expect(getWidth(0)).toBe(600);
                    expect(getX(0)).toBe(0);
                });
                
                it("should constrain a shrink wrapped item with align: right", function(){
                    makeCt('center', [{
                        html: makeLongString('A', 100)
                    }], {
                        constrainAlign: true,
                        autoSize: Ext.supports.PercentageSizeFlexBug ? false : null
                    });
                    expect(getWidth(0)).toBe(600);
                    expect(getX(0)).toBe(0);
                });
                
                it("should not constrain a fixed width item", function(){
                    makeCt('left', [{
                        html: 'A',
                        width: 1000
                    }], {
                        constrainAlign: false,
                        autoSize: Ext.supports.PercentageSizeFlexBug ? false : null
                    });
                    expect(getWidth(0)).toBe(1000);
                });
                
                it("should recalculate the top positions", function(){
                    makeCt('left', [{
                        html: makeLongString('A', 100)
                    }, {
                        html: 'B'
                    }], {
                        constrainAlign: true,
                        autoSize: Ext.supports.PercentageSizeFlexBug ? false : null
                    });
                    
                    expect(getY(0)).toBe(0);
                    expect(getY(1)).toBe(getHeight(0));  
                });
            });
        });
        
        describe("stretch", function() {
        
            it("should stretch all items to the size of the largest when using align: stretch", function(){
                makeCt('stretch', [{
                    html: 'foo'
                }, {
                    html: 'foo bar baz'
                }, {
                    html: 'foo'
                }], { width: null, floated: true });
            
                c = new Ext.Component({
                    html: 'foo bar baz',
                    floated: true,
                    hidden: false
                });

                var expected = c.element.getWidth();
                c.destroy();

                expect(getWidth(0)).toBe(expected);
                expect(getWidth(1)).toBe(expected);
                expect(getWidth(2)).toBe(expected);
            });
            
            it("should always use a fixed width over stretch", function(){
                makeCt('stretch', [{
                    width: 30
                }, {
                    html: 'foo bar baz blah long text'
                }, {
                    html: 'foo'
                }]);
            
                expect(getWidth(0)).toBe(30);
                expect(getWidth(1)).toBe(600);
                expect(getWidth(2)).toBe(600);
            });
            
            describe("minWidth", function() {
                it("should stretch an item with a minWidth", function(){
                    makeCt('stretch', [{
                        width: 30
                    }, {
                        minWidth: 5
                    }]);
                    expect(getWidth(0)).toBe(30);
                    expect(getWidth(1)).toBe(600);
                });
                
                it("should stretch to the item with the largest minWidth", function(){
                    makeCt('stretch', [{
                        minWidth: 30
                    }, {
                        minWidth: 50
                    }], { width: null, floated: true });
                    expect(getWidth(0)).toBe(50);
                    expect(getWidth(1)).toBe(50);
                });
                
                it("should stretch a single item outside the bounds of the container", function(){
                    makeCt('stretch', [{
                        minWidth: 1000,
                        html: 'Content...'
                    }], {
                        scrollable: true
                    });
                    expect(getWidth(0)).toBe(1000);
                });
            });
            
            it("should respect a maxWidth", function(){
                makeCt('stretch', [{
                    width: 30
                }, {
                    maxWidth: 20
                }]);
                expect(getWidth(0)).toBe(30);
                expect(getWidth(1)).toBe(20);
            });
        });
        
        it("should stretch all items to the container width", function(){
            makeCt('stretch', [{
             }, {
             }]);
            expect(getWidth(0)).toBe(600);
            expect(getWidth(1)).toBe(600);
        });
    });
    
    describe("height", function(){
        function makeCt(items, options) {
            options = options || {};
            ct = new Ext.container.Container({
                renderTo: Ext.getBody(),
                width: 100,
                height: 600,
                defaultType: 'component',
                autoSize: (options.autoSize != null) ? options.autoSize : null,
                layout: {
                    type: 'vbox',
                    align: 'stretch'
                },
                items: items
            });
        }

        describe("flex only", function(){
            it("should stretch a single flex item to the height of the container", function(){
                makeCt({
                    flex: 1
                });
                expect(getHeight(0)).toBe(600);
            });
        
            it("should stretch 3 equally flexed items equally", function(){
                makeCt([{
                    flex: 1
                }, {
                    flex: 1    
                }, {
                    flex: 1
                }]);
                expect(getHeight(0)).toBe(200);
                expect(getHeight(1)).toBe(200);
                expect(getHeight(2)).toBe(200);
            });
            
            it("should flex 2 items according to ratio", function(){
                makeCt([{
                    flex: 3    
                }, {
                    flex: 1
                }]);
                expect(getHeight(0)).toBe(450);
                expect(getHeight(1)).toBe(150);
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
                expect(getHeight(0)).toBe(225);
                expect(getHeight(1)).toBe(75);
                expect(getHeight(2)).toBe(225);
                expect(getHeight(3)).toBe(75);
            });
            
            it("should use flex as a ratio", function(){
                makeCt([{
                    flex: 4
                }, {
                    flex: 2
                }]);
                expect(getHeight(0)).toBe(400);
                expect(getHeight(1)).toBe(200);
            });
        });
        
        describe("fixed height only", function(){
            it("should set the height of a single item", function(){
                makeCt({
                    height: 200
                });    
                expect(getHeight(0)).toBe(200);
            });
            
            it("should set the height of multiple items", function(){
                makeCt([{
                    height: 500
                }, {
                    height: 50
                }]);    
                expect(getHeight(0)).toBe(500);
                expect(getHeight(1)).toBe(50);
            });
            
            it("should allow a single item to exceed the container height", function(){
                makeCt({
                    height: 900
                });
                expect(getHeight(0)).toBe(900);
            });

            (hasWebkitHeightConstraintBug ? xit : it)("should not allow a single item with shrink: 1 to exceed the container height", function(){
                makeCt({
                    height: 900,
                    flex: {
                        shrink: 1
                    }
                });
                expect(getHeight(0)).toBe(600);
            });

            it("should allow multiple items to exceed the container height", function(){
                makeCt([{
                    height: 400
                }, {
                    height: 400
                }]);
                expect(getHeight(0)).toBe(400);
                expect(getHeight(1)).toBe(400);
            });

            (hasWebkitHeightConstraintBug ? xit : it)("should not allow multiple items with shrink: 1 to exceed the container height", function(){
                makeCt([{
                    height: 400,
                    flex: {
                        shrink: 1
                    }
                }, {
                    height: 400,
                    flex: {
                        shrink: 1
                    }
                }]);
                expect(getHeight(0)).toBe(300);
                expect(getHeight(1)).toBe(300);
            });
        });

        describe("%age", function(){
            it("should be able to use %age height", function(){
                makeCt([{
                    height: '50%'
                }, {
                    height: '50%'
                }], {
                    autoSize: Ext.supports.PercentageSizeFlexBug ? false : null
                });
                expect(getHeight(0)).toBe(300);
                expect(getHeight(1)).toBe(300);
            });
            
            it("should work with fixed height", function(){
                makeCt([{
                    height: 100
                }, {
                    height: '20%'
                }, {
                    height: 380
                }], {
                    autoSize: Ext.supports.PercentageSizeFlexBug ? false : null
                });
                expect(getHeight(0)).toBe(100);
                expect(getHeight(1)).toBe(120);
                expect(getHeight(2)).toBe(380);
            });
            
            it("should work with flex", function(){
                makeCt([{
                    flex: 2
                }, {
                    height: '40%'
                }, {
                    flex: 1
                }], {
                    autoSize: Ext.supports.PercentageSizeFlexBug ? false : null
                });
                expect(getHeight(0)).toBe(240);
                expect(getHeight(1)).toBe(240);
                expect(getHeight(2)).toBe(120);
            });
        });
        
        describe("mixed", function(){
            it("should give any remaining space to a single flexed item", function(){
                makeCt([{
                    height: 200
                }, {
                    flex: 1
                }]);
                expect(getHeight(0)).toBe(200);
                expect(getHeight(1)).toBe(400);
            });
            
            it("should flex a single item with 2 fixed", function(){
                makeCt([{
                    height: 100
                }, {
                    flex: 1
                }, {
                    height: 300
                }]);
                expect(getHeight(0)).toBe(100);
                expect(getHeight(1)).toBe(200);
                expect(getHeight(2)).toBe(300);
            });
            
            it("should flex 2 items with 1 fixed", function(){
                makeCt([{
                    flex: 2
                }, {
                    height: 300
                }, {
                    flex: 1
                }]);    
                expect(getHeight(0)).toBe(200);
                expect(getHeight(1)).toBe(300);
                expect(getHeight(2)).toBe(100);
            });

            it("should give priority to flex over a fixed height", function(){
                makeCt([{
                    flex: 1,
                    height: 200
                }, {
                    flex: 1
                }]);

                expect(getHeight(0)).toBe(300);
                expect(getHeight(1)).toBe(300);
            });
        });

        describe("min/max", function(){
            it("should assign a 0 height if there is no more flex height", function(){
                makeCt([{
                    flex: 1,
                    style: 'line-height:0'
                }, {
                    height: 700
                }]);
                expect(getHeight(0)).toBe(0);
                expect(getHeight(1)).toBe(700);
            });
            
            it("should respect a minWidth on a flex even if there is no more flex width", function(){
                makeCt([{
                    flex: 1,
                    minHeight: 50
                }, {
                    height: 700
                }]);
                expect(getHeight(0)).toBe(50);
                expect(getHeight(1)).toBe(700);
            });
            
            it("should respect a minWidth on a flex even if there is no excess flex width", function(){
                makeCt([{
                    flex: 1,
                    maxHeight: 100
                }, {
                    height: 300
                }]);
                expect(getHeight(0)).toBe(100);
                expect(getHeight(1)).toBe(300);    
            });

            // Disabled in IE11 because it computes flex values incorrectly when using min-height
            (Ext.isIE11 ? xit: it)("should update flex values based on min constraint", function(){
                var c1 = new Ext.Component({
                    flex: 1,
                    minHeight: 500
                }), c2 = new Ext.Component({
                    flex: 1
                });
                makeCt([c1, c2]);
                expect(c1.el.getHeight()).toBe(500);
                expect(c2.el.getHeight()).toBe(100);
            });

            // Disabled in IE11 because it computes flex values incorrectly when using min-height
            (Ext.isIE11 ? xit: it)("should handle multiple min constraints", function(){
                 var c1 = new Ext.Component({
                    flex: 1,
                    minHeight: 250
                }), c2 = new Ext.Component({
                    flex: 1,
                    minHeight: 250
                }), c3 = new Ext.Component({
                    flex: 1
                });
                
                makeCt([c1, c2, c3]);
                expect(c1.el.getHeight()).toBe(250);
                expect(c2.el.getHeight()).toBe(250);
                expect(c3.el.getHeight()).toBe(100);
            });
            
            it("should update flex values based on max constraint", function(){
                var c1 = new Ext.Component({
                    flex: 1,
                    maxHeight: 100
                }), c2 = new Ext.Component({
                    flex: 1
                });
                makeCt([c1, c2]);
                expect(c1.el.getHeight()).toBe(100);
                expect(c2.el.getHeight()).toBe(500);
            });
            
            it("should update flex values based on multiple max constraints", function(){
                var c1 = new Ext.Component({
                    flex: 1,
                    maxHeight: 100
                }), c2 = new Ext.Component({
                    flex: 1,
                    maxHeight: 100
                }), c3 = new Ext.Component({
                    flex: 1
                });
                makeCt([c1, c2, c3]);
                expect(c1.el.getHeight()).toBe(100);
                expect(c2.el.getHeight()).toBe(100);
                expect(c3.el.getHeight()).toBe(400);
            });

            it("should give precedence to min constraints over flex when the min is the same", function() {
                var c1 = new Ext.Component({
                    flex: 1,
                    minHeight: 200
                }), c2 = new Ext.Component({
                    flex: 3,
                    minHeight: 200
                }), c3 = new Ext.Component({
                    flex: 1,
                    minHeight: 200
                });
                makeCt([c1, c2, c3]);
                expect(c1.el.getHeight()).toBe(200);
                expect(c2.el.getHeight()).toBe(200);
                expect(c3.el.getHeight()).toBe(200);
            });

            it("should give precedence to max constraints over flex when the max is the same", function() {
                var c1 = new Ext.Component({
                    flex: 1,
                    maxHeight: 100
                }), c2 = new Ext.Component({
                    flex: 3,
                    maxHeight: 100
                }), c3 = new Ext.Component({
                    flex: 1,
                    maxHeight: 100
                });
                makeCt([c1, c2, c3]);
                expect(c1.el.getHeight()).toBe(100);
                expect(c2.el.getHeight()).toBe(100);
                expect(c3.el.getHeight()).toBe(100);
            });

            describe("with %age", function() {
                it("should respect min constraints", function() {
                    document.documentElement.style.height = document.body.style.height = '100%';

                    makeCt([{
                        height: '10%',
                        minHeight: 250
                    },{
                        flex: 1
                    }]);
                    expect(getHeight(0)).toBe(250);
                    expect(getHeight(1)).toBe(350);

                    document.documentElement.style.height = document.body.style.height = '';
                });

                it("should respect max constraints", function() {
                    document.documentElement.style.height = document.body.style.height = '100%';
                    makeCt([{
                        height: '90%',
                        maxHeight: 100
                    },{
                        flex: 1
                    }], {
                        autoSize: Ext.supports.PercentageSizeFlexBug ? false : null
                    });
                    expect(getHeight(0)).toBe(100);
                    expect(getHeight(1)).toBe(500);
                    document.documentElement.style.height = document.body.style.height = '';
                });
            });
        });

        describe("flex config parsing", function () {
            var item;

            function makeFlex(flex) {
                ct = new Ext.Container({
                    renderTo: Ext.getBody(),
                    layout: 'vbox',
                    items: [{
                        xtype: 'component',
                        flex: flex || null
                    }]
                });

                item = ct.items.getAt(0);
            }

            it("should not set any flex values by default", function () {
                makeFlex();

                expect(item.el.getStyle('flex-grow')).toBe('0');
                expect(item.el.getStyle('flex-shrink')).toBe('0');
                expect(item.el.getStyle('flex-basis')).toBe('auto');
            });

            it("should parse flex-grow as a string", function () {
                makeFlex('1');

                expect(item.el.getStyle('flex-grow')).toBe('1');
                expect(item.el.getStyle('flex-shrink')).toBe('1');
                expect(parseInt(item.el.getStyle('flex-basis'))).toBe(0);
            });

            it("should parse flex-grow and flex-shrink as a string", function () {
                makeFlex('1 2');

                expect(item.el.getStyle('flex-grow')).toBe('1');
                expect(item.el.getStyle('flex-shrink')).toBe('2');
                expect(parseInt(item.el.getStyle('flex-basis'))).toBe(0);
            });

            it("should parse flex-grow, flex-shrink, and flex-basis as a string", function () {
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

});
