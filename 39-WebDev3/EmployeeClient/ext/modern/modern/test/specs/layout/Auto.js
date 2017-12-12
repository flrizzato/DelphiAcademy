describe("Ext.layout.Auto", function() {
    var scrollbarSize = Ext.getScrollbarSize(),
        scrollbarWidth = scrollbarSize.width,
        scrollbarHeight = scrollbarSize.height,
        ct;

    afterEach(function() {
        if (ct) {
            ct.destroy();
            ct = null;
        }
    });

    describe("docking", function() {
        // TODO


        describe("constraining docked item size", function() {
            // https://sencha.jira.com/browse/EXTJS-23904

            it("should constrain the height of a docked: left item", function() {
                ct = Ext.create({
                    xtype: 'container',
                    height: 200,
                    width: 400,
                    renderTo: Ext.getBody(),
                    style: 'background: yellow',
                    items: [{
                        xtype: 'component',
                        html: 'content'
                    }, {
                        xtype: 'component',
                        docked: 'right',
                        width: 100,
                        scrollable: true,
                        html: '<div style="height:400px;background: green;"></div>'
                    }]
                });

                expect(ct).toHaveLayout({
                    element: { xywh: '0 0 400 200' },
                    bodyElement: { xywh: '0 0 300 200' },
                    items: {
                        0: {
                            element: { xywh: '0 0 300 16' },
                            bodyElement: { xywh: '0 0 300 16' },
                            innerHtmlElement: { xywh: '0 0 300 16' }
                        },
                        1: {
                            element: { xywh: '300 0 100 200' },
                            bodyElement: { xywh: '300 0 100 200' },
                            innerHtmlElement: { x: 300, y: 0, w: 100 - scrollbarWidth, h: 400 }
                        }
                    }
                });
            });

            it("should constrain the height of a docked: right item", function() {
                ct = Ext.create({
                    xtype: 'container',
                    height: 200,
                    width: 400,
                    renderTo: Ext.getBody(),
                    style: 'background: yellow',
                    items: [{
                        xtype: 'component',
                        html: 'content'
                    }, {
                        xtype: 'component',
                        docked: 'left',
                        width: 100,
                        scrollable: true,
                        html: '<div style="height:400px;background: green;"></div>'
                    }]
                });

                expect(ct).toHaveLayout({
                    element: { xywh: '0 0 400 200' },
                    bodyElement: { xywh: '100 0 300 200' },
                    items: {
                        0: {
                            element: { xywh: '100 0 300 16' },
                            bodyElement: { xywh: '100 0 300 16' },
                            innerHtmlElement: { xywh: '100 0 300 16' }
                        },
                        1: {
                            element: { xywh: '0 0 100 200' },
                            bodyElement: { xywh: '0 0 100 200' },
                            innerHtmlElement: { x: 0, y: 0, w: 100 - scrollbarWidth, h: 400 }
                        }
                    }
                });
            });

            it("should constrain the width of a docked: top item", function() {
                ct = Ext.create({
                    xtype: 'container',
                    height: 400,
                    width: 200,
                    renderTo: Ext.getBody(),
                    style: 'background: yellow',
                    items: [{
                        xtype: 'component',
                        html: 'content'
                    }, {
                        xtype: 'component',
                        docked: 'top',
                        height: 100,
                        scrollable: true,
                        html: '<div style="width:400px;background: green;">&nbsp;</div>'
                    }]
                });

                expect(ct).toHaveLayout({
                    element: { xywh: '0 0 200 400' },
                    bodyElement: { xywh: '0 100 200 300' },
                    items: {
                        0: {
                            element: { xywh: '0 100 200 16' },
                            bodyElement: { xywh: '0 100 200 16' },
                            innerHtmlElement: { xywh: '0 100 200 16' }
                        },
                        1: {
                            element: { xywh: '0 0 200 100' },
                            bodyElement: { xywh: '0 0 200 100' },
                            innerHtmlElement: { xywh: '0 0 200 16' }
                        }
                    }
                });
            });

            it("should constrain the width of a docked: bottom item", function() {
                ct = Ext.create({
                    xtype: 'container',
                    height: 400,
                    width: 200,
                    renderTo: Ext.getBody(),
                    style: 'background: yellow',
                    items: [{
                        xtype: 'component',
                        html: 'content'
                    }, {
                        xtype: 'component',
                        docked: 'bottom',
                        height: 100,
                        scrollable: true,
                        html: '<div style="width:400px;background: green;">&nbsp;</div>'
                    }]
                });

                expect(ct).toHaveLayout({
                    element: { xywh: '0 0 200 400' },
                    bodyElement: { xywh: '0 0 200 300' },
                    items: {
                        0: {
                            element: { xywh: '0 0 200 16' },
                            bodyElement: { xywh: '0 0 200 16' },
                            innerHtmlElement: { xywh: '0 0 200 16' }
                        },
                        1: {
                            element: { xywh: '0 300 200 100' },
                            bodyElement: { xywh: '0 300 200 100' },
                            innerHtmlElement: { xywh: '0 300 200 16' }
                        }
                    }
                });
            });
        });
    });

    describe("percentage size items", function() {
        it("should size an item in percentages when container is explicitly sized", function() {
            ct = Ext.create({
                xtype: 'container',
                renderTo: document.body,
                height: 200,
                width: 200,
                autoSize: Ext.supports.PercentageSizeFlexBug ? false : null,
                items: [{
                    xtype: 'component',
                    width: '50%',
                    height: '25%'
                }]
            });

            expect(ct).toHaveLayout({
                element: {
                    xywh: '0 0 200 200'
                },
                bodyElement: {
                    xywh: '0 0 200 200'
                },
                items: {
                    0: {
                        element: {
                            xywh: '0 0 100 50'
                        },
                        bodyElement: {
                            xywh: '0 0 100 50'
                        }
                    }
                }
            });
        });

        (Ext.supports.PercentageSizeFlexBug ? xit : it)("should percentage height an item in a shrinkwrap width container", function () {
            ct = Ext.create({
                xtype: 'container',
                floated: true,
                hidden: false,
                height: 200,
                items: [{
                    xtype: 'component',
                    width: 200,
                    height: '25%'
                }]
            });

            expect(ct).toHaveLayout({
                element: {
                    xywh: '0 0 200 200'
                },
                bodyElement: {
                    xywh: '0 0 200 200'
                },
                items: {
                    0: {
                        element: {
                            xywh: '0 0 200 50'
                        },
                        bodyElement: {
                            xywh: '0 0 200 50'
                        }
                    }
                }
            });
        });

        (Ext.supports.PercentageSizeFlexBug ? xit : it)("should percentage width an item in a shrinkwrap height container", function () {
            ct = Ext.create({
                xtype: 'container',
                renderTo: document.body,
                width: 200,
                items: [{
                    xtype: 'component',
                    width: '50%',
                    height: 200
                }]
            });

            expect(ct).toHaveLayout({
                element: {
                    xywh: '0 0 200 200'
                },
                bodyElement: {
                    xywh: '0 0 200 200'
                },
                items: {
                    0: {
                        element: {
                            xywh: '0 0 100 200'
                        },
                        bodyElement: {
                            xywh: '0 0 100 200'
                        }
                    }
                }
            });
        });

        it("should size an item in percentages when flexed and stretched in an hbox", function() {
            ct = Ext.create({
                xtype: 'container',
                renderTo: document.body,
                height: 200,
                width: 200,
                layout: {
                    type: 'hbox',
                    align: 'stretch'
                },
                items: [{
                    xtype: 'container',
                    flex: 1,
                    autoSize: Ext.supports.PercentageSizeFlexBug ? false : null,
                    items: [{
                        xtype: 'component',
                        width: '50%',
                        height: '25%'
                    }]
                }]
            });

            expect(ct).toHaveLayout({
                element: {
                    xywh: '0 0 200 200'
                },
                bodyElement: {
                    xywh: '0 0 200 200'
                },
                items: {
                    0: {
                        element: {
                            xywh: '0 0 200 200'
                        },
                        bodyElement: {
                            xywh: '0 0 200 200'
                        },
                        items: {
                            0: {
                                element: {
                                    xywh: '0 0 100 50'
                                },
                                bodyElement: {
                                    xywh: '0 0 100 50'
                                }
                            }
                        }
                    }
                }
            });
        });

        it("should size an item in percentages when flexed and stretched in a vbox", function() {
            ct = Ext.create({
                xtype: 'container',
                renderTo: document.body,
                height: 200,
                width: 200,
                layout: {
                    type: 'vbox',
                    align: 'stretch'
                },
                items: [{
                    xtype: 'container',
                    flex: 1,
                    autoSize: Ext.supports.PercentageSizeFlexBug ? false : null,
                    items: [{
                        xtype: 'component',
                        width: '50%',
                        height: '25%'
                    }]
                }]
            });

            expect(ct).toHaveLayout({
                element: {
                    xywh: '0 0 200 200'
                },
                bodyElement: {
                    xywh: '0 0 200 200'
                },
                items: {
                    0: {
                        element: {
                            xywh: '0 0 200 200'
                        },
                        bodyElement: {
                            xywh: '0 0 200 200'
                        },
                        items: {
                            0: {
                                element: {
                                    xywh: '0 0 100 50'
                                },
                                bodyElement: {
                                    xywh: '0 0 100 50'
                                }
                            }
                        }
                    }
                }
            });
        });

        it("should size a dynamically added item in percentages", function () {
            ct = Ext.create({
                xtype: 'container',
                renderTo: document.body,
                height: 200,
                width: 200,
                autoSize: Ext.supports.PercentageSizeFlexBug ? false : null
            });

            ct.add({
                xtype: 'component',
                width: '50%',
                height: '25%'
            });

            expect(ct).toHaveLayout({
                element: {
                    xywh: '0 0 200 200'
                },
                bodyElement: {
                    xywh: '0 0 200 200'
                },
                items: {
                    0: {
                        element: {
                            xywh: '0 0 100 50'
                        },
                        bodyElement: {
                            xywh: '0 0 100 50'
                        }
                    }
                }
            });
        });
    });
});