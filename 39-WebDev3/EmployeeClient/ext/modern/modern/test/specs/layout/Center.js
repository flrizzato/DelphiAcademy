describe("Ext.layout.Center", function() {
    var ct;

    afterEach(function() {
        if (ct) {
            ct.destroy();
            ct = null;
        }
    });

    it("should center an item with px size", function() {
        ct = Ext.create({
            xtype: 'container',
            layout: 'center',
            renderTo: Ext.getBody(),
            width: 200,
            height: 200,
            items: [{
                width: 100,
                height: 100
            }]
        });

        expect(ct).toHaveLayout({
            el: {
                h: 200,
                w: 200
            },
            items: {
                0: { el: { xywh: '50 50 100 100' } }
            }
        });
    });

    it("should center an item with % size", function() {
        ct = Ext.create({
            xtype: 'container',
            layout: 'center',
            renderTo: Ext.getBody(),
            width: 200,
            height: 200,
            autoSize: Ext.supports.PercentageSizeFlexBug ? false : null,
            items: [{
                width: '50%',
                height: '50%'
            }]
        });

        expect(ct).toHaveLayout({
            el: {
                h: 200,
                w: 200
            },
            items: {
                0: { el: { xywh: '50 50 100 100' } }
            }
        });
    });

    it("should center an auto-sized item", function() {
        ct = Ext.create({
            xtype: 'container',
            layout: 'center',
            renderTo: Ext.getBody(),
            width: 200,
            height: 200,
            items: [{
                style: 'display: table', // shrink wrap width
                html: '<div style="height:100px;width:100px;"></div>'
            }]
        });

        expect(ct).toHaveLayout({
            el: {
                h: 200,
                w: 200
            },
            items: {
                0: { el: { xywh: '50 50 100 100' } }
            }
        });
    });

    it("should center html content", function() {
        ct = Ext.create({
            xtype: 'container',
            layout: 'center',
            renderTo: Ext.getBody(),
            width: 200,
            height: 200,
            html: '<div style="height:100px;width:100px;"></div>'
        });

        expect(ct).toHaveLayout({
            el: {
                h: 200,
                w: 200
            },
            '.x-innerhtml': { xywh: '50 50 100 100' }
        });
    });

    it("should overflow vertically", function() {
        var scrollbarSize = Ext.getScrollbarSize();

        ct = Ext.create({
            xtype: 'container',
            layout: 'center',
            renderTo: Ext.getBody(),
            width: 200,
            height: 200,
            scrollable: true,
            items: [{
                height: 300,
                width: 100
            }]
        });

        expect(ct).toHaveLayout({
            el: {
                h: 200,
                w: 200
            },
            items: {
                0: {
                    el: {
                        x: [
                            Math.floor((100 - scrollbarSize.height) / 2),
                            Math.ceil((100 - scrollbarSize.height) / 2)
                        ],
                        y: 0,
                        w: 100,
                        h: 300
                    }
                }
            }
        });

        expect(ct.getScrollable().getSize()).toEqual({
            x: 200 - scrollbarSize.width,
            y: 300
        });
    });

    it("should overflow horizontally", function() {
        var scrollbarSize = Ext.getScrollbarSize();

        ct = Ext.create({
            xtype: 'container',
            layout: 'center',
            renderTo: Ext.getBody(),
            width: 200,
            height: 200,
            scrollable: true,
            items: [{
                height: 100,
                width: 300
            }]
        });

        expect(ct).toHaveLayout({
            el: {
                h: 200,
                w: 200
            },
            items: {
                0: {
                    el: {
                        x: 0,
                        // Conditional expectation for IE edge due to the following bug:
                        // https://developer.microsoft.com/en-us/microsoft-edge/platform/issues/10547765/
                        y: (Ext.isIE || Ext.isEdge) ? 50 : [
                            Math.floor((100 - scrollbarSize.height) / 2),
                            Math.ceil((100 - scrollbarSize.height) / 2)
                        ],
                        w: 300,
                        h: 100
                    }
                }
            }
        });

        expect(ct.getScrollable().getSize()).toEqual({
            x: 300,
            y: 200 - scrollbarSize.height
        });
    });
});