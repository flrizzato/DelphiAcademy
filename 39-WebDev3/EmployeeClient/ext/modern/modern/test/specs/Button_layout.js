describe('Ext.Button_layout', function () {
    var textHtml = '<span style="display:inline-block;width:50px;background:green;">&nbsp;</span>',
        iconCls = 'x-fa fa-star',
        width = null,
        height = null,
        button, container;

    function create(config) {
        button = new Ext.Button(Ext.apply({
            renderTo: Ext.getBody(),
            width: width,
            height: height
        }, config));
    }

    afterEach(function () {
        button = container = Ext.destroy(button, container);

        width = null;
        height = null;
    });

    describe('auto size', function () {
        describe('text only', function () {
            it('should layout with textAlign: left', function () {
                create({
                    text: textHtml,
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 24' },
                    bodyElement: { xywh: '9 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with textAlign: center', function () {
                create({
                    text: textHtml,
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 24' },
                    bodyElement: { xywh: '9 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with textAlign: right', function () {
                create({
                    text: textHtml,
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 24' },
                    bodyElement: { xywh: '9 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { d: false }
                });
            });
        });

        describe('icon only', function () {
            it('should layout with iconAlign: left', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 24' },
                    bodyElement: { xywh: '4 4 16 16' },
                    iconElement: { xywh: '4 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'top'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 24' },
                    bodyElement: { xywh: '4 4 16 16' },
                    iconElement: { xywh: '4 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 24' },
                    bodyElement: { xywh: '4 4 16 16' },
                    iconElement: { xywh: '4 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 24' },
                    bodyElement: { xywh: '4 4 16 16' },
                    iconElement: { xywh: '4 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });
        });

        describe('icon and text', function () {
            it('should layout with iconAlign: left and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '9 4 16 16' },
                    textElement: { xywh: '30 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: left and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '9 4 16 16' },
                    textElement: { xywh: '30 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: left and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '9 4 16 16' },
                    textElement: { xywh: '30 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 4 16 16' },
                    textElement: { xywh: '9 22 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 4 16 16' },
                    textElement: { xywh: '9 22 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 4 16 16' },
                    textElement: { xywh: '9 22 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '64 4 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '64 4 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '64 4 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 22 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 22 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 22 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { d: false }
                });
            });
        });

        describe('icon and arrow', function () {
            it('should layout with iconAlign: left and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 45 24' },
                    bodyElement: { xywh: '4 4 16 16' },
                    iconElement: { xywh: '4 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '25 4 16 16' }
                });
            });

            it('should layout with iconAlign: left and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 40' },
                    bodyElement: { xywh: '4 4 16 16' },
                    iconElement: { xywh: '4 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '4 20 16 16' }
                });
            });

            it('should layout with iconAlign: top and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 45 24' },
                    bodyElement: { xywh: '4 4 16 16' },
                    iconElement: { xywh: '4 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '25 4 16 16' }
                });
            });

            it('should layout with iconAlign: top and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 40' },
                    bodyElement: { xywh: '4 4 16 16' },
                    iconElement: { xywh: '4 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '4 20 16 16' }
                });
            });

            it('should layout with iconAlign: right and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 45 24' },
                    bodyElement: { xywh: '4 4 16 16' },
                    iconElement: { xywh: '4 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '25 4 16 16' }
                });
            });

            it('should layout with iconAlign: right and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 40' },
                    bodyElement: { xywh: '4 4 16 16' },
                    iconElement: { xywh: '4 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '4 20 16 16' }
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 45 24' },
                    bodyElement: { xywh: '4 4 16 16' },
                    iconElement: { xywh: '4 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '25 4 16 16' }
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 40' },
                    bodyElement: { xywh: '4 4 16 16' },
                    iconElement: { xywh: '4 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '4 20 16 16' }
                });
            });
        });

        describe('text and arrow', function () {
            it('should layout with textAlign: left and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 24' },
                    bodyElement: { xywh: '9 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '64 4 16 16' }
                });
            });

            it('should layout with textAlign: left and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 40' },
                    bodyElement: { xywh: '9 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '26 20 16 16' }
                });
            });

            it('should layout with textAlign: center and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 24' },
                    bodyElement: { xywh: '9 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '64 4 16 16' }
                });
            });

            it('should layout with textAlign: center and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 40' },
                    bodyElement: { xywh: '9 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '26 20 16 16' }
                });
            });

            it('should layout with textAlign: right and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 24' },
                    bodyElement: { xywh: '9 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '64 4 16 16' }
                });
            });

            it('should layout with textAlign: right and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 40' },
                    bodyElement: { xywh: '9 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '26 20 16 16' }
                });
            });
        });

        describe('icon, text, and arrow', function () {
            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 110 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '9 4 16 16' },
                    textElement: { xywh: '30 4 50 16' },
                    arrowElement: { xywh: '85 4 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 40' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '9 4 16 16' },
                    textElement: { xywh: '30 4 50 16' },
                    arrowElement: { xywh: '37 20 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 110 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '9 4 16 16' },
                    textElement: { xywh: '30 4 50 16' },
                    arrowElement: { xywh: '85 4 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 40' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '9 4 16 16' },
                    textElement: { xywh: '30 4 50 16' },
                    arrowElement: { xywh: '37 20 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 110 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '9 4 16 16' },
                    textElement: { xywh: '30 4 50 16' },
                    arrowElement: { xywh: '85 4 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 40' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '9 4 16 16' },
                    textElement: { xywh: '30 4 50 16' },
                    arrowElement: { xywh: '37 20 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 4 16 16' },
                    textElement: { xywh: '9 22 50 16' },
                    arrowElement: { xywh: '64 13 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 58' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 4 16 16' },
                    textElement: { xywh: '9 22 50 16' },
                    arrowElement: { xywh: '26 38 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 4 16 16' },
                    textElement: { xywh: '9 22 50 16' },
                    arrowElement: { xywh: '64 13 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 58' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 4 16 16' },
                    textElement: { xywh: '9 22 50 16' },
                    arrowElement: { xywh: '26 38 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 4 16 16' },
                    textElement: { xywh: '9 22 50 16' },
                    arrowElement: { xywh: '64 13 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 58' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 4 16 16' },
                    textElement: { xywh: '9 22 50 16' },
                    arrowElement: { xywh: '26 38 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 110 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '64 4 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '85 4 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 40' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '64 4 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '37 20 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 110 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '64 4 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '85 4 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 40' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '64 4 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '37 20 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 110 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '64 4 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '85 4 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 40' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '64 4 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '37 20 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 22 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '64 13 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 58' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 22 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '26 38 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 22 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '64 13 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 58' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 22 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '26 38 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 22 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '64 13 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 58' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 22 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '26 38 16 16' }
                });
            });
        });
    });

    describe('configured width', function () {
        beforeEach(function () {
            width = 160;
        });

        describe('text only', function () {
            it('should layout with textAlign: left', function () {
                create({
                    text: textHtml,
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '9 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with textAlign: center', function () {
                create({
                    text: textHtml,
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '55 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '55 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with textAlign: right', function () {
                create({
                    text: textHtml,
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '101 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '101 4 50 16' },
                    arrowElement: { d: false }
                });
            });
        });

        describe('icon only', function () {
            it('should layout with iconAlign: left', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '72 4 16 16' },
                    iconElement: { xywh: '72 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'top'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '72 4 16 16' },
                    iconElement: { xywh: '72 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '72 4 16 16' },
                    iconElement: { xywh: '72 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '72 4 16 16' },
                    iconElement: { xywh: '72 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });
        });

        describe('icon and text', function () {
            it('should layout with iconAlign: left and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '9 4 16 16' },
                    textElement: { xywh: '30 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: left and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '45 4 71 16' },
                    iconElement: { xywh: '45 4 16 16' },
                    textElement: { xywh: '66 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: left and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '80 4 71 16' },
                    iconElement: { xywh: '80 4 16 16' },
                    textElement: { xywh: '101 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 4 16 16' },
                    textElement: { xywh: '9 22 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 42' },
                    bodyElement: { xywh: '55 4 50 34' },
                    iconElement: { xywh: '72 4 16 16' },
                    textElement: { xywh: '55 22 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 42' },
                    bodyElement: { xywh: '101 4 50 34' },
                    iconElement: { xywh: '118 4 16 16' },
                    textElement: { xywh: '101 22 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '64 4 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '45 4 71 16' },
                    iconElement: { xywh: '100 4 16 16' },
                    textElement: { xywh: '45 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '80 4 71 16' },
                    iconElement: { xywh: '135 4 16 16' },
                    textElement: { xywh: '80 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 22 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 42' },
                    bodyElement: { xywh: '55 4 50 34' },
                    iconElement: { xywh: '72 22 16 16' },
                    textElement: { xywh: '55 4 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 42' },
                    bodyElement: { xywh: '101 4 50 34' },
                    iconElement: { xywh: '118 22 16 16' },
                    textElement: { xywh: '101 4 50 16' },
                    arrowElement: { d: false }
                });
            });
        });

        describe('icon and arrow', function () {
            it('should layout with iconAlign: left and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '62 4 16 16' },
                    iconElement: { xywh: '62 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '140 4 16 16' }
                });
            });

            it('should layout with iconAlign: left and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'bottom'
                });

            });

            it('should layout with iconAlign: top and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '62 4 16 16' },
                    iconElement: { xywh: '62 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '140 4 16 16' }
                });
            });

            it('should layout with iconAlign: top and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 40' },
                    bodyElement: { xywh: '72 4 16 16' },
                    iconElement: { xywh: '72 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '72 20 16 16' }
                });
            });

            it('should layout with iconAlign: right and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '62 4 16 16' },
                    iconElement: { xywh: '62 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '140 4 16 16' }
                });
            });

            it('should layout with iconAlign: right and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 40' },
                    bodyElement: { xywh: '72 4 16 16' },
                    iconElement: { xywh: '72 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '72 20 16 16' }
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '62 4 16 16' },
                    iconElement: { xywh: '62 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '140 4 16 16' }
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 40' },
                    bodyElement: { xywh: '72 4 16 16' },
                    iconElement: { xywh: '72 4 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '72 20 16 16' }
                });
            });
        });

        describe('text and arrow', function () {
            it('should layout with textAlign: left and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '9 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '135 4 16 16' }
                });
            });

            it('should layout with textAlign: left and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 40' },
                    bodyElement: { xywh: '9 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '72 20 16 16' }
                });
            });

            it('should layout with textAlign: center and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '45 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '45 4 50 16' },
                    arrowElement: { xywh: '135 4 16 16' }
                });
            });

            it('should layout with textAlign: center and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 40' },
                    bodyElement: { xywh: '55 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '55 4 50 16' },
                    arrowElement: { xywh: '72 20 16 16' }
                });
            });

            it('should layout with textAlign: right and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '80 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '80 4 50 16' },
                    arrowElement: { xywh: '135 4 16 16' }
                });
            });

            it('should layout with textAlign: right and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 40' },
                    bodyElement: { xywh: '101 4 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '101 4 50 16' },
                    arrowElement: { xywh: '72 20 16 16' }
                });
            });
        });

        describe('icon, text, and arrow', function () {
            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '9 4 16 16' },
                    textElement: { xywh: '30 4 50 16' },
                    arrowElement: { xywh: '135 4 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 40' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '9 4 16 16' },
                    textElement: { xywh: '30 4 50 16' },
                    arrowElement: { xywh: '72 20 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '34 4 71 16' },
                    iconElement: { xywh: '34 4 16 16' },
                    textElement: { xywh: '55 4 50 16' },
                    arrowElement: { xywh: '135 4 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 40' },
                    bodyElement: { xywh: '45 4 71 16' },
                    iconElement: { xywh: '45 4 16 16' },
                    textElement: { xywh: '66 4 50 16' },
                    arrowElement: { xywh: '72 20 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '59 4 71 16' },
                    iconElement: { xywh: '59 4 16 16' },
                    textElement: { xywh: '80 4 50 16' },
                    arrowElement: { xywh: '135 4 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 40' },
                    bodyElement: { xywh: '80 4 71 16' },
                    iconElement: { xywh: '80 4 16 16' },
                    textElement: { xywh: '101 4 50 16' },
                    arrowElement: { xywh: '72 20 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 4 16 16' },
                    textElement: { xywh: '9 22 50 16' },
                    arrowElement: { xywh: '135 13 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 42' },
                    bodyElement: { xywh: '45 4 50 34' },
                    iconElement: { xywh: '62 4 16 16' },
                    textElement: { xywh: '45 22 50 16' },
                    arrowElement: { xywh: '135 13 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 58' },
                    bodyElement: { xywh: '55 4 50 34' },
                    iconElement: { xywh: '72 4 16 16' },
                    textElement: { xywh: '55 22 50 16' },
                    arrowElement: { xywh: '72 38 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 42' },
                    bodyElement: { xywh: '80 4 50 34' },
                    iconElement: { xywh: '97 4 16 16' },
                    textElement: { xywh: '80 22 50 16' },
                    arrowElement: { xywh: '135 13 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 58' },
                    bodyElement: { xywh: '101 4 50 34' },
                    iconElement: { xywh: '118 4 16 16' },
                    textElement: { xywh: '101 22 50 16' },
                    arrowElement: { xywh: '72 38 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '64 4 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '135 4 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 40' },
                    bodyElement: { xywh: '9 4 71 16' },
                    iconElement: { xywh: '64 4 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '72 20 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '34 4 71 16' },
                    iconElement: { xywh: '89 4 16 16' },
                    textElement: { xywh: '34 4 50 16' },
                    arrowElement: { xywh: '135 4 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 40' },
                    bodyElement: { xywh: '45 4 71 16' },
                    iconElement: { xywh: '100 4 16 16' },
                    textElement: { xywh: '45 4 50 16' },
                    arrowElement: { xywh: '72 20 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 24' },
                    bodyElement: { xywh: '59 4 71 16' },
                    iconElement: { xywh: '114 4 16 16' },
                    textElement: { xywh: '59 4 50 16' },
                    arrowElement: { xywh: '135 4 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 40' },
                    bodyElement: { xywh: '80 4 71 16' },
                    iconElement: { xywh: '135 4 16 16' },
                    textElement: { xywh: '80 4 50 16' },
                    arrowElement: { xywh: '72 20 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 42' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 22 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '135 13 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 58' },
                    bodyElement: { xywh: '9 4 50 34' },
                    iconElement: { xywh: '26 22 16 16' },
                    textElement: { xywh: '9 4 50 16' },
                    arrowElement: { xywh: '72 38 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 42' },
                    bodyElement: { xywh: '45 4 50 34' },
                    iconElement: { xywh: '62 22 16 16' },
                    textElement: { xywh: '45 4 50 16' },
                    arrowElement: { xywh: '135 13 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 58' },
                    bodyElement: { xywh: '55 4 50 34' },
                    iconElement: { xywh: '72 22 16 16' },
                    textElement: { xywh: '55 4 50 16' },
                    arrowElement: { xywh: '72 38 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 42' },
                    bodyElement: { xywh: '80 4 50 34' },
                    iconElement: { xywh: '97 22 16 16' },
                    textElement: { xywh: '80 4 50 16' },
                    arrowElement: { xywh: '135 13 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 58' },
                    bodyElement: { xywh: '101 4 50 34' },
                    iconElement: { xywh: '118 22 16 16' },
                    textElement: { xywh: '101 4 50 16' },
                    arrowElement: { xywh: '72 38 16 16' }
                });
            });
        });
    });

    describe('configured height', function () {
        beforeEach(function () {
            height = 160;
        });

        describe('text only', function () {
            it('should layout with textAlign: left', function () {
                create({
                    text: textHtml,
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 72 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with textAlign: center', function () {
                create({
                    text: textHtml,
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 72 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with textAlign: right', function () {
                create({
                    text: textHtml,
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 72 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { d: false }
                });
            });
        });

        describe('icon only', function () {
            it('should layout with iconAlign: left', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 160' },
                    bodyElement: { xywh: '4 72 16 16' },
                    iconElement: { xywh: '4 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'top'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 160' },
                    bodyElement: { xywh: '4 72 16 16' },
                    iconElement: { xywh: '4 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 160' },
                    bodyElement: { xywh: '4 72 16 16' },
                    iconElement: { xywh: '4 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 160' },
                    bodyElement: { xywh: '4 72 16 16' },
                    iconElement: { xywh: '4 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });
        });

        describe('icon and text', function () {
            it('should layout with iconAlign: left and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '9 72 16 16' },
                    textElement: { xywh: '30 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: left and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '9 72 16 16' },
                    textElement: { xywh: '30 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: left and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '9 72 16 16' },
                    textElement: { xywh: '30 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 63 16 16' },
                    textElement: { xywh: '9 81 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 63 16 16' },
                    textElement: { xywh: '9 81 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 63 16 16' },
                    textElement: { xywh: '9 81 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '64 72 16 16' },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '64 72 16 16' },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '64 72 16 16' },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 81 16 16' },
                    textElement: { xywh: '9 63 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 81 16 16' },
                    textElement: { xywh: '9 63 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 81 16 16' },
                    textElement: { xywh: '9 63 50 16' },
                    arrowElement: { d: false }
                });
            });
        });

        describe('icon and arrow', function () {
            it('should layout with iconAlign: left and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 45 160' },
                    bodyElement: { xywh: '4 72 16 16' },
                    iconElement: { xywh: '4 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '25 72 16 16' }
                });
            });

            it('should layout with iconAlign: left and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 160' },
                    bodyElement: { xywh: '4 64 16 16' },
                    iconElement: { xywh: '4 64 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '4 140 16 16' }
                });
            });

            it('should layout with iconAlign: top and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 45 160' },
                    bodyElement: { xywh: '4 72 16 16' },
                    iconElement: { xywh: '4 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '25 72 16 16' }
                });
            });

            it('should layout with iconAlign: top and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 160' },
                    bodyElement: { xywh: '4 64 16 16' },
                    iconElement: { xywh: '4 64 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '4 140 16 16' }
                });
            });

            it('should layout with iconAlign: right and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 45 160' },
                    bodyElement: { xywh: '4 72 16 16' },
                    iconElement: { xywh: '4 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '25 72 16 16' }
                });
            });

            it('should layout with iconAlign: right and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 160' },
                    bodyElement: { xywh: '4 64 16 16' },
                    iconElement: { xywh: '4 64 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '4 140 16 16' }
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 45 160' },
                    bodyElement: { xywh: '4 72 16 16' },
                    iconElement: { xywh: '4 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '25 72 16 16' }
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 24 160' },
                    bodyElement: { xywh: '4 64 16 16' },
                    iconElement: { xywh: '4 64 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '4 140 16 16' }
                });
            });
        });

        describe('text and arrow', function () {
            it('should layout with textAlign: left and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 72 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { xywh: '64 72 16 16' }
                });
            });

            it('should layout with textAlign: left and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 64 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 64 50 16' },
                    arrowElement: { xywh: '26 140 16 16' }
                });
            });

            it('should layout with textAlign: center and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 72 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { xywh: '64 72 16 16' }
                });
            });

            it('should layout with textAlign: center and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 64 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 64 50 16' },
                    arrowElement: { xywh: '26 140 16 16' }
                });
            });

            it('should layout with textAlign: right and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 72 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { xywh: '64 72 16 16' }
                });
            });

            it('should layout with textAlign: right and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 64 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 64 50 16' },
                    arrowElement: { xywh: '26 140 16 16' }
                });
            });
        });

        describe('icon, text, and arrow', function () {
            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 110 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '9 72 16 16' },
                    textElement: { xywh: '30 72 50 16' },
                    arrowElement: { xywh: '85 72 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 64 71 16' },
                    iconElement: { xywh: '9 64 16 16' },
                    textElement: { xywh: '30 64 50 16' },
                    arrowElement: { xywh: '37 140 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 110 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '9 72 16 16' },
                    textElement: { xywh: '30 72 50 16' },
                    arrowElement: { xywh: '85 72 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 64 71 16' },
                    iconElement: { xywh: '9 64 16 16' },
                    textElement: { xywh: '30 64 50 16' },
                    arrowElement: { xywh: '37 140 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 110 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '9 72 16 16' },
                    textElement: { xywh: '30 72 50 16' },
                    arrowElement: { xywh: '85 72 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 64 71 16' },
                    iconElement: { xywh: '9 64 16 16' },
                    textElement: { xywh: '30 64 50 16' },
                    arrowElement: { xywh: '37 140 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 63 16 16' },
                    textElement: { xywh: '9 81 50 16' },
                    arrowElement: { xywh: '64 72 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 55 50 34' },
                    iconElement: { xywh: '26 55 16 16' },
                    textElement: { xywh: '9 73 50 16' },
                    arrowElement: { xywh: '26 140 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 63 16 16' },
                    textElement: { xywh: '9 81 50 16' },
                    arrowElement: { xywh: '64 72 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 55 50 34' },
                    iconElement: { xywh: '26 55 16 16' },
                    textElement: { xywh: '9 73 50 16' },
                    arrowElement: { xywh: '26 140 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 63 16 16' },
                    textElement: { xywh: '9 81 50 16' },
                    arrowElement: { xywh: '64 72 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 55 50 34' },
                    iconElement: { xywh: '26 55 16 16' },
                    textElement: { xywh: '9 73 50 16' },
                    arrowElement: { xywh: '26 140 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 110 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '64 72 16 16' },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { xywh: '85 72 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 64 71 16' },
                    iconElement: { xywh: '64 64 16 16' },
                    textElement: { xywh: '9 64 50 16' },
                    arrowElement: { xywh: '37 140 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 110 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '64 72 16 16' },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { xywh: '85 72 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 64 71 16' },
                    iconElement: { xywh: '64 64 16 16' },
                    textElement: { xywh: '9 64 50 16' },
                    arrowElement: { xywh: '37 140 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 110 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '64 72 16 16' },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { xywh: '85 72 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 64 71 16' },
                    iconElement: { xywh: '64 64 16 16' },
                    textElement: { xywh: '9 64 50 16' },
                    arrowElement: { xywh: '37 140 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 81 16 16' },
                    textElement: { xywh: '9 63 50 16' },
                    arrowElement: { xywh: '64 72 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 55 50 34' },
                    iconElement: { xywh: '26 73 16 16' },
                    textElement: { xywh: '9 55 50 16' },
                    arrowElement: { xywh: '26 140 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 81 16 16' },
                    textElement: { xywh: '9 63 50 16' },
                    arrowElement: { xywh: '64 72 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 55 50 34' },
                    iconElement: { xywh: '26 73 16 16' },
                    textElement: { xywh: '9 55 50 16' },
                    arrowElement: { xywh: '26 140 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 89 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 81 16 16' },
                    textElement: { xywh: '9 63 50 16' },
                    arrowElement: { xywh: '64 72 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 68 160' },
                    bodyElement: { xywh: '9 55 50 34' },
                    iconElement: { xywh: '26 73 16 16' },
                    textElement: { xywh: '9 55 50 16' },
                    arrowElement: { xywh: '26 140 16 16' }
                });
            });
        });
    });

    describe('configured width and height', function () {
        beforeEach(function () {
            width = height = 160;
        });

        describe('text only', function () {
            it('should layout with textAlign: left', function () {
                create({
                    text: textHtml,
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '9 72 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with textAlign: center', function () {
                create({
                    text: textHtml,
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '55 72 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '55 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with textAlign: right', function () {
                create({
                    text: textHtml,
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '101 72 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '101 72 50 16' },
                    arrowElement: { d: false }
                });
            });
        });

        describe('icon only', function () {
            it('should layout with iconAlign: left', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '72 72 16 16' },
                    iconElement: { xywh: '72 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'top'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '72 72 16 16' },
                    iconElement: { xywh: '72 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '72 72 16 16' },
                    iconElement: { xywh: '72 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '72 72 16 16' },
                    iconElement: { xywh: '72 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { d: false }
                });
            });
        });

        describe('icon and text', function () {
            it('should layout with iconAlign: left and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '9 72 16 16' },
                    textElement: { xywh: '30 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: left and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '45 72 71 16' },
                    iconElement: { xywh: '45 72 16 16' },
                    textElement: { xywh: '66 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: left and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '80 72 71 16' },
                    iconElement: { xywh: '80 72 16 16' },
                    textElement: { xywh: '101 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 63 16 16' },
                    textElement: { xywh: '9 81 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '55 63 50 34' },
                    iconElement: { xywh: '72 63 16 16' },
                    textElement: { xywh: '55 81 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: top and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '101 63 50 34' },
                    iconElement: { xywh: '118 63 16 16' },
                    textElement: { xywh: '101 81 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '64 72 16 16' },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '45 72 71 16' },
                    iconElement: { xywh: '100 72 16 16' },
                    textElement: { xywh: '45 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: right and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '80 72 71 16' },
                    iconElement: { xywh: '135 72 16 16' },
                    textElement: { xywh: '80 72 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 81 16 16' },
                    textElement: { xywh: '9 63 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '55 63 50 34' },
                    iconElement: { xywh: '72 81 16 16' },
                    textElement: { xywh: '55 63 50 16' },
                    arrowElement: { d: false }
                });
            });

            it('should layout with iconAlign: bottom and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '101 63 50 34' },
                    iconElement: { xywh: '118 81 16 16' },
                    textElement: { xywh: '101 63 50 16' },
                    arrowElement: { d: false }
                });
            });
        });

        describe('icon and arrow', function () {
            it('should layout with iconAlign: left and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '62 72 16 16' },
                    iconElement: { xywh: '62 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '140 72 16 16' }
                });
            });

            it('should layout with iconAlign: left and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '72 64 16 16' },
                    iconElement: { xywh: '72 64 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with iconAlign: top and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '62 72 16 16' },
                    iconElement: { xywh: '62 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '140 72 16 16' }
                });
            });

            it('should layout with iconAlign: top and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '72 64 16 16' },
                    iconElement: { xywh: '72 64 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with iconAlign: right and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '62 72 16 16' },
                    iconElement: { xywh: '62 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '140 72 16 16' }
                });
            });

            it('should layout with iconAlign: right and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '72 64 16 16' },
                    iconElement: { xywh: '72 64 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '62 72 16 16' },
                    iconElement: { xywh: '62 72 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '140 72 16 16' }
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '72 64 16 16' },
                    iconElement: { xywh: '72 64 16 16' },
                    textElement: { d: false },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });
        });

        describe('text and arrow', function () {
            it('should layout with textAlign: left and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '9 72 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { xywh: '135 72 16 16' }
                });
            });

            it('should layout with textAlign: left and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '9 64 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '9 64 50 16' },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with textAlign: center and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '45 72 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '45 72 50 16' },
                    arrowElement: { xywh: '135 72 16 16' }
                });
            });

            it('should layout with textAlign: center and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '55 64 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '55 64 50 16' },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with textAlign: right and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '80 72 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '80 72 50 16' },
                    arrowElement: { xywh: '135 72 16 16' }
                });
            });

            it('should layout with textAlign: right and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '101 64 50 16' },
                    iconElement: { d: false },
                    textElement: { xywh: '101 64 50 16' },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });
        });

        describe('icon, text, and arrow', function () {
            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '9 72 16 16' },
                    textElement: { xywh: '30 72 50 16' },
                    arrowElement: { xywh: '135 72 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '9 64 71 16' },
                    iconElement: { xywh: '9 64 16 16' },
                    textElement: { xywh: '30 64 50 16' },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '34 72 71 16' },
                    iconElement: { xywh: '34 72 16 16' },
                    textElement: { xywh: '55 72 50 16' },
                    arrowElement: { xywh: '135 72 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '45 64 71 16' },
                    iconElement: { xywh: '45 64 16 16' },
                    textElement: { xywh: '66 64 50 16' },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '59 72 71 16' },
                    iconElement: { xywh: '59 72 16 16' },
                    textElement: { xywh: '80 72 50 16' },
                    arrowElement: { xywh: '135 72 16 16' }
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '80 64 71 16' },
                    iconElement: { xywh: '80 64 16 16' },
                    textElement: { xywh: '101 64 50 16' },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 63 16 16' },
                    textElement: { xywh: '9 81 50 16' },
                    arrowElement: { xywh: '135 72 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '9 55 50 34' },
                    iconElement: { xywh: '26 55 16 16' },
                    textElement: { xywh: '9 73 50 16' },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '45 63 50 34' },
                    iconElement: { xywh: '62 63 16 16' },
                    textElement: { xywh: '45 81 50 16' },
                    arrowElement: { xywh: '135 72 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '55 55 50 34' },
                    iconElement: { xywh: '72 55 16 16' },
                    textElement: { xywh: '55 73 50 16' },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '80 63 50 34' },
                    iconElement: { xywh: '97 63 16 16' },
                    textElement: { xywh: '80 81 50 16' },
                    arrowElement: { xywh: '135 72 16 16' }
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '101 55 50 34' },
                    iconElement: { xywh: '118 55 16 16' },
                    textElement: { xywh: '101 73 50 16' },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '9 72 71 16' },
                    iconElement: { xywh: '64 72 16 16' },
                    textElement: { xywh: '9 72 50 16' },
                    arrowElement: { xywh: '135 72 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '9 64 71 16' },
                    iconElement: { xywh: '64 64 16 16' },
                    textElement: { xywh: '9 64 50 16' },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '34 72 71 16' },
                    iconElement: { xywh: '89 72 16 16' },
                    textElement: { xywh: '34 72 50 16' },
                    arrowElement: { xywh: '135 72 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '45 64 71 16' },
                    iconElement: { xywh: '100 64 16 16' },
                    textElement: { xywh: '45 64 50 16' },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '59 72 71 16' },
                    iconElement: { xywh: '114 72 16 16' },
                    textElement: { xywh: '59 72 50 16' },
                    arrowElement: { xywh: '135 72 16 16' }
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '80 64 71 16' },
                    iconElement: { xywh: '135 64 16 16' },
                    textElement: { xywh: '80 64 50 16' },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '9 63 50 34' },
                    iconElement: { xywh: '26 81 16 16' },
                    textElement: { xywh: '9 63 50 16' },
                    arrowElement: { xywh: '135 72 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '9 55 50 34' },
                    iconElement: { xywh: '26 73 16 16' },
                    textElement: { xywh: '9 55 50 16' },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '45 63 50 34' },
                    iconElement: { xywh: '62 81 16 16' },
                    textElement: { xywh: '45 63 50 16' },
                    arrowElement: { xywh: '135 72 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '55 55 50 34' },
                    iconElement: { xywh: '72 73 16 16' },
                    textElement: { xywh: '55 55 50 16' },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '80 63 50 34' },
                    iconElement: { xywh: '97 81 16 16' },
                    textElement: { xywh: '80 63 50 16' },
                    arrowElement: { xywh: '135 72 16 16' }
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: { xywh: '0 0 160 160' },
                    bodyElement: { xywh: '101 55 50 34' },
                    iconElement: { xywh: '118 73 16 16' },
                    textElement: { xywh: '101 55 50 16' },
                    arrowElement: { xywh: '72 140 16 16' }
                });
            });
        });
    });

    describe('configured width - smaller than content', function () {
        beforeEach(function () {
            width = 50;
        });

        describe('text only', function () {
            it('should layout with textAlign: left', function () {
                create({
                    text: textHtml,
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with textAlign: center', function () {
                create({
                    text: textHtml,
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with textAlign: right', function () {
                create({
                    text: textHtml,
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {d: false}
                });
            });
        });

        describe('icon only', function () {
            beforeEach(function() {
                width = 20;
            });
            
            it('should layout with iconAlign: left', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 24'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: top', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'top'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 24'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 24'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 24'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });
        });

        describe('icon and text', function () {
            it('should layout with iconAlign: left and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 11 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: left and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 11 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: left and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'right'
                });

            });

            it('should layout with iconAlign: top and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'left'
                });

            });

            it('should layout with iconAlign: top and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 42'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 32 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: top and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 42'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 32 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {xywh: '25 4 16 16'},
                    textElement: {xywh: '9 4 11 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {xywh: '25 4 16 16'},
                    textElement: {xywh: '9 4 11 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {xywh: '25 4 16 16'},
                    textElement: {xywh: '9 4 11 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 42'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 42'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 42'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {d: false}
                });
            });
        });

        describe('icon and arrow', function () {
            it('should layout with iconAlign: left and arrowAlign: right', function () {
                width = 40;
                
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 40 24'},
                    bodyElement: {xywh: '4 4 11 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '20 4 16 16'}
                });
            });

            it('should layout with iconAlign: left and arrowAlign: bottom', function () {
                width = 20;
                
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 40'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '2 20 16 16'}
                });
            });

            it('should layout with iconAlign: top and arrowAlign: right', function () {
                width = 40;
                
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 40 24'},
                    bodyElement: {xywh: '4 4 11 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '20 4 16 16'}
                });
            });

            it('should layout with iconAlign: top and arrowAlign: bottom', function () {
                width = 20;

                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 40'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '2 20 16 16'}
                });
            });

            it('should layout with iconAlign: right and arrowAlign: right', function () {
                width = 40;
                
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 40 24'},
                    bodyElement: {xywh: '4 4 11 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '20 4 16 16'}
                });
            });

            it('should layout with iconAlign: right and arrowAlign: bottom', function () {
                width = 20;

                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 40'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '2 20 16 16'}
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: right', function () {
                width = 40;

                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 40 24'},
                    bodyElement: {xywh: '4 4 11 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '20 4 16 16'}
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: bottom', function () {
                width = 20;

                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 40'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '2 20 16 16'}
                });
            });
        });

        describe('text and arrow', function () {
            it('should layout with textAlign: left and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 11 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 11 16'},
                    arrowElement: {xywh: '25 4 16 16'}
                });
            });

            it('should layout with textAlign: left and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 40'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {xywh: '17 20 16 16'}
                });
            });

            it('should layout with textAlign: center and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 11 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 11 16'},
                    arrowElement: {xywh: '25 4 16 16'}
                });
            });

            it('should layout with textAlign: center and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 40'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {xywh: '17 20 16 16'}
                });
            });

            it('should layout with textAlign: right and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 11 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 11 16'},
                    arrowElement: {xywh: '25 4 16 16'}
                });
            });

            it('should layout with textAlign: right and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 40'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {xywh: '17 20 16 16'}
                });
            });
        });

        describe('icon, text, and arrow', function () {
            beforeEach(function() {
                width = 70;
            });

            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 24'},
                    bodyElement: {xywh: '9 4 31 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 10 16'},
                    arrowElement: {xywh: '45 4 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });
                
                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 40'},
                    bodyElement: {xywh: '9 4 52 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 31 16'},
                    arrowElement: {xywh: '27 20 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 24'},
                    bodyElement: {xywh: '9 4 31 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 10 16'},
                    arrowElement: {xywh: '45 4 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 40'},
                    bodyElement: {xywh: '9 4 52 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 31 16'},
                    arrowElement: {xywh: '27 20 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 24'},
                    bodyElement: {xywh: '9 4 31 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 10 16'},
                    arrowElement: {xywh: '45 4 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 40'},
                    bodyElement: {xywh: '9 4 52 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 31 16'},
                    arrowElement: {xywh: '27 20 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 42'},
                    bodyElement: {xywh: '9 4 31 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 31 16'},
                    arrowElement: {xywh: '45 13 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: bottom', function () {
                width = 50;
                
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 58'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 32 16'},
                    arrowElement: {xywh: '17 38 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 42'},
                    bodyElement: {xywh: '9 4 31 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 31 16'},
                    arrowElement: {xywh: '45 13 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: bottom', function () {
                width = 50;
                
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 58'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 32 16'},
                    arrowElement: {xywh: '17 38 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 42'},
                    bodyElement: {xywh: '9 4 31 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 31 16'},
                    arrowElement: {xywh: '45 13 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: bottom', function () {
                width = 50;

                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 58'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 32 16'},
                    arrowElement: {xywh: '17 38 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 24'},
                    bodyElement: {xywh: '9 4 31 16'},
                    iconElement: {xywh: '24 4 16 16'},
                    textElement: {xywh: '9 4 10 16'},
                    arrowElement: {xywh: '45 4 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 40'},
                    bodyElement: {xywh: '9 4 52 16'},
                    iconElement: {xywh: '45 4 16 16'},
                    textElement: {xywh: '9 4 31 16'},
                    arrowElement: {xywh: '27 20 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 24'},
                    bodyElement: {xywh: '9 4 31 16'},
                    iconElement: {xywh: '24 4 16 16'},
                    textElement: {xywh: '9 4 10 16'},
                    arrowElement: {xywh: '45 4 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 40'},
                    bodyElement: {xywh: '9 4 52 16'},
                    iconElement: {xywh: '45 4 16 16'},
                    textElement: {xywh: '9 4 31 16'},
                    arrowElement: {xywh: '27 20 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 24'},
                    bodyElement: {xywh: '9 4 31 16'},
                    iconElement: {xywh: '24 4 16 16'},
                    textElement: {xywh: '9 4 10 16'},
                    arrowElement: {xywh: '45 4 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 40'},
                    bodyElement: {xywh: '9 4 52 16'},
                    iconElement: {xywh: '45 4 16 16'},
                    textElement: {xywh: '9 4 31 16'},
                    arrowElement: {xywh: '27 20 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 42'},
                    bodyElement: {xywh: '9 4 31 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 31 16'},
                    arrowElement: {xywh: '45 13 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: bottom', function () {
                width = 50;
                
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 58'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {xywh: '17 38 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: right', function () {
                width = 50;
                
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 42'},
                    bodyElement: {xywh: '9 4 11 34'},
                    iconElement: {xywh: '7 22 16 16'},
                    textElement: {xywh: '9 4 11 16'},
                    arrowElement: {xywh: '25 13 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: bottom', function () {
                width = 50;

                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 58'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {xywh: '17 38 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 42'},
                    bodyElement: {xywh: '9 4 31 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 31 16'},
                    arrowElement: {xywh: '45 13 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: bottom', function () {
                width = 50;

                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 58'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {xywh: '17 38 16 16'}
                });
            });
        });
    });

    describe('flexed width', function () {
        function create(config) {
            button = new Ext.Button(Ext.apply({
                flex: 1
            }, config));

            container = new Ext.Container({
                renderTo: Ext.getBody(),
                layout: 'hbox',
                height: height,
                width: width,
                items: [button]
            });
        }

        beforeEach(function () {
            width = 160;
        });

        describe('text only', function () {
            it('should layout with textAlign: left', function () {
                create({
                    text: textHtml,
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '9 4 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with textAlign: center', function () {
                create({
                    text: textHtml,
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '55 4 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '55 4 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with textAlign: right', function () {
                create({
                    text: textHtml,
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '101 4 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '101 4 50 16'},
                    arrowElement: {d: false}
                });
            });
        });

        describe('icon only', function () {
            it('should layout with iconAlign: left', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '72 4 16 16'},
                    iconElement: {xywh: '72 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: top', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'top'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '72 4 16 16'},
                    iconElement: {xywh: '72 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '72 4 16 16'},
                    iconElement: {xywh: '72 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '72 4 16 16'},
                    iconElement: {xywh: '72 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });
        });

        describe('icon and text', function () {
            it('should layout with iconAlign: left and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '9 4 71 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: left and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '45 4 71 16'},
                    iconElement: {xywh: '45 4 16 16'},
                    textElement: {xywh: '66 4 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: left and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '80 4 71 16'},
                    iconElement: {xywh: '80 4 16 16'},
                    textElement: {xywh: '101 4 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: top and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 42'},
                    bodyElement: {xywh: '9 4 50 34'},
                    iconElement: {xywh: '26 4 16 16'},
                    textElement: {xywh: '9 22 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: top and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 42'},
                    bodyElement: {xywh: '55 4 50 34'},
                    iconElement: {xywh: '72 4 16 16'},
                    textElement: {xywh: '55 22 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: top and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 42'},
                    bodyElement: {xywh: '101 4 50 34'},
                    iconElement: {xywh: '118 4 16 16'},
                    textElement: {xywh: '101 22 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '9 4 71 16'},
                    iconElement: {xywh: '64 4 16 16'},
                    textElement: {xywh: '9 4 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '45 4 71 16'},
                    iconElement: {xywh: '100 4 16 16'},
                    textElement: {xywh: '45 4 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '80 4 71 16'},
                    iconElement: {xywh: '135 4 16 16'},
                    textElement: {xywh: '80 4 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 42'},
                    bodyElement: {xywh: '9 4 50 34'},
                    iconElement: {xywh: '26 22 16 16'},
                    textElement: {xywh: '9 4 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 42'},
                    bodyElement: {xywh: '55 4 50 34'},
                    iconElement: {xywh: '72 22 16 16'},
                    textElement: {xywh: '55 4 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 42'},
                    bodyElement: {xywh: '101 4 50 34'},
                    iconElement: {xywh: '118 22 16 16'},
                    textElement: {xywh: '101 4 50 16'},
                    arrowElement: {d: false}
                });
            });
        });

        describe('icon and arrow', function () {
            it('should layout with iconAlign: left and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '62 4 16 16'},
                    iconElement: {xywh: '62 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '140 4 16 16'}
                });
            });

            it('should layout with iconAlign: left and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'bottom'
                });

            });

            it('should layout with iconAlign: top and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '62 4 16 16'},
                    iconElement: {xywh: '62 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '140 4 16 16'}
                });
            });

            it('should layout with iconAlign: top and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 40'},
                    bodyElement: {xywh: '72 4 16 16'},
                    iconElement: {xywh: '72 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '72 20 16 16'}
                });
            });

            it('should layout with iconAlign: right and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '62 4 16 16'},
                    iconElement: {xywh: '62 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '140 4 16 16'}
                });
            });

            it('should layout with iconAlign: right and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 40'},
                    bodyElement: {xywh: '72 4 16 16'},
                    iconElement: {xywh: '72 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '72 20 16 16'}
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '62 4 16 16'},
                    iconElement: {xywh: '62 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '140 4 16 16'}
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 40'},
                    bodyElement: {xywh: '72 4 16 16'},
                    iconElement: {xywh: '72 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '72 20 16 16'}
                });
            });
        });

        describe('text and arrow', function () {
            it('should layout with textAlign: left and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '9 4 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 50 16'},
                    arrowElement: {xywh: '135 4 16 16'}
                });
            });

            it('should layout with textAlign: left and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 40'},
                    bodyElement: {xywh: '9 4 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 50 16'},
                    arrowElement: {xywh: '72 20 16 16'}
                });
            });

            it('should layout with textAlign: center and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '45 4 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '45 4 50 16'},
                    arrowElement: {xywh: '135 4 16 16'}
                });
            });

            it('should layout with textAlign: center and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 40'},
                    bodyElement: {xywh: '55 4 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '55 4 50 16'},
                    arrowElement: {xywh: '72 20 16 16'}
                });
            });

            it('should layout with textAlign: right and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '80 4 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '80 4 50 16'},
                    arrowElement: {xywh: '135 4 16 16'}
                });
            });

            it('should layout with textAlign: right and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 40'},
                    bodyElement: {xywh: '101 4 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '101 4 50 16'},
                    arrowElement: {xywh: '72 20 16 16'}
                });
            });
        });

        describe('icon, text, and arrow', function () {
            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '9 4 71 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 50 16'},
                    arrowElement: {xywh: '135 4 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 40'},
                    bodyElement: {xywh: '9 4 71 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 50 16'},
                    arrowElement: {xywh: '72 20 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '34 4 71 16'},
                    iconElement: {xywh: '34 4 16 16'},
                    textElement: {xywh: '55 4 50 16'},
                    arrowElement: {xywh: '135 4 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 40'},
                    bodyElement: {xywh: '45 4 71 16'},
                    iconElement: {xywh: '45 4 16 16'},
                    textElement: {xywh: '66 4 50 16'},
                    arrowElement: {xywh: '72 20 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '59 4 71 16'},
                    iconElement: {xywh: '59 4 16 16'},
                    textElement: {xywh: '80 4 50 16'},
                    arrowElement: {xywh: '135 4 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 40'},
                    bodyElement: {xywh: '80 4 71 16'},
                    iconElement: {xywh: '80 4 16 16'},
                    textElement: {xywh: '101 4 50 16'},
                    arrowElement: {xywh: '72 20 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 42'},
                    bodyElement: {xywh: '9 4 50 34'},
                    iconElement: {xywh: '26 4 16 16'},
                    textElement: {xywh: '9 22 50 16'},
                    arrowElement: {xywh: '135 13 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 42'},
                    bodyElement: {xywh: '45 4 50 34'},
                    iconElement: {xywh: '62 4 16 16'},
                    textElement: {xywh: '45 22 50 16'},
                    arrowElement: {xywh: '135 13 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 58'},
                    bodyElement: {xywh: '55 4 50 34'},
                    iconElement: {xywh: '72 4 16 16'},
                    textElement: {xywh: '55 22 50 16'},
                    arrowElement: {xywh: '72 38 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 42'},
                    bodyElement: {xywh: '80 4 50 34'},
                    iconElement: {xywh: '97 4 16 16'},
                    textElement: {xywh: '80 22 50 16'},
                    arrowElement: {xywh: '135 13 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 58'},
                    bodyElement: {xywh: '101 4 50 34'},
                    iconElement: {xywh: '118 4 16 16'},
                    textElement: {xywh: '101 22 50 16'},
                    arrowElement: {xywh: '72 38 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '9 4 71 16'},
                    iconElement: {xywh: '64 4 16 16'},
                    textElement: {xywh: '9 4 50 16'},
                    arrowElement: {xywh: '135 4 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 40'},
                    bodyElement: {xywh: '9 4 71 16'},
                    iconElement: {xywh: '64 4 16 16'},
                    textElement: {xywh: '9 4 50 16'},
                    arrowElement: {xywh: '72 20 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '34 4 71 16'},
                    iconElement: {xywh: '89 4 16 16'},
                    textElement: {xywh: '34 4 50 16'},
                    arrowElement: {xywh: '135 4 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 40'},
                    bodyElement: {xywh: '45 4 71 16'},
                    iconElement: {xywh: '100 4 16 16'},
                    textElement: {xywh: '45 4 50 16'},
                    arrowElement: {xywh: '72 20 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 24'},
                    bodyElement: {xywh: '59 4 71 16'},
                    iconElement: {xywh: '114 4 16 16'},
                    textElement: {xywh: '59 4 50 16'},
                    arrowElement: {xywh: '135 4 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 40'},
                    bodyElement: {xywh: '80 4 71 16'},
                    iconElement: {xywh: '135 4 16 16'},
                    textElement: {xywh: '80 4 50 16'},
                    arrowElement: {xywh: '72 20 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 42'},
                    bodyElement: {xywh: '9 4 50 34'},
                    iconElement: {xywh: '26 22 16 16'},
                    textElement: {xywh: '9 4 50 16'},
                    arrowElement: {xywh: '135 13 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 58'},
                    bodyElement: {xywh: '9 4 50 34'},
                    iconElement: {xywh: '26 22 16 16'},
                    textElement: {xywh: '9 4 50 16'},
                    arrowElement: {xywh: '72 38 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 42'},
                    bodyElement: {xywh: '45 4 50 34'},
                    iconElement: {xywh: '62 22 16 16'},
                    textElement: {xywh: '45 4 50 16'},
                    arrowElement: {xywh: '135 13 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 58'},
                    bodyElement: {xywh: '55 4 50 34'},
                    iconElement: {xywh: '72 22 16 16'},
                    textElement: {xywh: '55 4 50 16'},
                    arrowElement: {xywh: '72 38 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 42'},
                    bodyElement: {xywh: '80 4 50 34'},
                    iconElement: {xywh: '97 22 16 16'},
                    textElement: {xywh: '80 4 50 16'},
                    arrowElement: {xywh: '135 13 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 160 58'},
                    bodyElement: {xywh: '101 4 50 34'},
                    iconElement: {xywh: '118 22 16 16'},
                    textElement: {xywh: '101 4 50 16'},
                    arrowElement: {xywh: '72 38 16 16'}
                });
            });
        });
    });

    describe('flexed height', function () {
        function create(config) {
            button = new Ext.Button(Ext.apply({
                flex: 1
            }, config));

            container = new Ext.Container({
                renderTo: Ext.getBody(),
                layout: {
                    type: 'vbox',
                    align: 'start'
                },
                height: height,
                width: width,
                items: [button]
            });
        }

        beforeEach(function () {
            height = 160;
        });

        describe('text only', function () {
            it('should layout with textAlign: left', function () {
                create({
                    text: textHtml,
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 72 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 72 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with textAlign: center', function () {
                create({
                    text: textHtml,
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 72 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 72 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with textAlign: right', function () {
                create({
                    text: textHtml,
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 72 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 72 50 16'},
                    arrowElement: {d: false}
                });
            });
        });

        describe('icon only', function () {
            it('should layout with iconAlign: left', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 24 160'},
                    bodyElement: {xywh: '4 72 16 16'},
                    iconElement: {xywh: '4 72 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: top', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'top'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 24 160'},
                    bodyElement: {xywh: '4 72 16 16'},
                    iconElement: {xywh: '4 72 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 24 160'},
                    bodyElement: {xywh: '4 72 16 16'},
                    iconElement: {xywh: '4 72 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 24 160'},
                    bodyElement: {xywh: '4 72 16 16'},
                    iconElement: {xywh: '4 72 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });
        });

        describe('icon and text', function () {
            it('should layout with iconAlign: left and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 72 71 16'},
                    iconElement: {xywh: '9 72 16 16'},
                    textElement: {xywh: '30 72 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: left and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 72 71 16'},
                    iconElement: {xywh: '9 72 16 16'},
                    textElement: {xywh: '30 72 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: left and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 72 71 16'},
                    iconElement: {xywh: '9 72 16 16'},
                    textElement: {xywh: '30 72 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: top and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 63 50 34'},
                    iconElement: {xywh: '26 63 16 16'},
                    textElement: {xywh: '9 81 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: top and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 63 50 34'},
                    iconElement: {xywh: '26 63 16 16'},
                    textElement: {xywh: '9 81 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: top and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 63 50 34'},
                    iconElement: {xywh: '26 63 16 16'},
                    textElement: {xywh: '9 81 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 72 71 16'},
                    iconElement: {xywh: '64 72 16 16'},
                    textElement: {xywh: '9 72 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 72 71 16'},
                    iconElement: {xywh: '64 72 16 16'},
                    textElement: {xywh: '9 72 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 72 71 16'},
                    iconElement: {xywh: '64 72 16 16'},
                    textElement: {xywh: '9 72 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 63 50 34'},
                    iconElement: {xywh: '26 81 16 16'},
                    textElement: {xywh: '9 63 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 63 50 34'},
                    iconElement: {xywh: '26 81 16 16'},
                    textElement: {xywh: '9 63 50 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 63 50 34'},
                    iconElement: {xywh: '26 81 16 16'},
                    textElement: {xywh: '9 63 50 16'},
                    arrowElement: {d: false}
                });
            });
        });

        describe('icon and arrow', function () {
            it('should layout with iconAlign: left and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 45 160'},
                    bodyElement: {xywh: '4 72 16 16'},
                    iconElement: {xywh: '4 72 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '25 72 16 16'}
                });
            });

            it('should layout with iconAlign: left and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 24 160'},
                    bodyElement: {xywh: '4 64 16 16'},
                    iconElement: {xywh: '4 64 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '4 140 16 16'}
                });
            });

            it('should layout with iconAlign: top and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 45 160'},
                    bodyElement: {xywh: '4 72 16 16'},
                    iconElement: {xywh: '4 72 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '25 72 16 16'}
                });
            });

            it('should layout with iconAlign: top and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 24 160'},
                    bodyElement: {xywh: '4 64 16 16'},
                    iconElement: {xywh: '4 64 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '4 140 16 16'}
                });
            });

            it('should layout with iconAlign: right and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 45 160'},
                    bodyElement: {xywh: '4 72 16 16'},
                    iconElement: {xywh: '4 72 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '25 72 16 16'}
                });
            });

            it('should layout with iconAlign: right and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 24 160'},
                    bodyElement: {xywh: '4 64 16 16'},
                    iconElement: {xywh: '4 64 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '4 140 16 16'}
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 45 160'},
                    bodyElement: {xywh: '4 72 16 16'},
                    iconElement: {xywh: '4 72 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '25 72 16 16'}
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 24 160'},
                    bodyElement: {xywh: '4 64 16 16'},
                    iconElement: {xywh: '4 64 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '4 140 16 16'}
                });
            });
        });

        describe('text and arrow', function () {
            it('should layout with textAlign: left and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 72 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 72 50 16'},
                    arrowElement: {xywh: '64 72 16 16'}
                });
            });

            it('should layout with textAlign: left and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 64 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 64 50 16'},
                    arrowElement: {xywh: '26 140 16 16'}
                });
            });

            it('should layout with textAlign: center and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 72 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 72 50 16'},
                    arrowElement: {xywh: '64 72 16 16'}
                });
            });

            it('should layout with textAlign: center and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 64 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 64 50 16'},
                    arrowElement: {xywh: '26 140 16 16'}
                });
            });

            it('should layout with textAlign: right and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 72 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 72 50 16'},
                    arrowElement: {xywh: '64 72 16 16'}
                });
            });

            it('should layout with textAlign: right and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 64 50 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 64 50 16'},
                    arrowElement: {xywh: '26 140 16 16'}
                });
            });
        });

        describe('icon, text, and arrow', function () {
            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 110 160'},
                    bodyElement: {xywh: '9 72 71 16'},
                    iconElement: {xywh: '9 72 16 16'},
                    textElement: {xywh: '30 72 50 16'},
                    arrowElement: {xywh: '85 72 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 64 71 16'},
                    iconElement: {xywh: '9 64 16 16'},
                    textElement: {xywh: '30 64 50 16'},
                    arrowElement: {xywh: '37 140 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 110 160'},
                    bodyElement: {xywh: '9 72 71 16'},
                    iconElement: {xywh: '9 72 16 16'},
                    textElement: {xywh: '30 72 50 16'},
                    arrowElement: {xywh: '85 72 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 64 71 16'},
                    iconElement: {xywh: '9 64 16 16'},
                    textElement: {xywh: '30 64 50 16'},
                    arrowElement: {xywh: '37 140 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 110 160'},
                    bodyElement: {xywh: '9 72 71 16'},
                    iconElement: {xywh: '9 72 16 16'},
                    textElement: {xywh: '30 72 50 16'},
                    arrowElement: {xywh: '85 72 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 64 71 16'},
                    iconElement: {xywh: '9 64 16 16'},
                    textElement: {xywh: '30 64 50 16'},
                    arrowElement: {xywh: '37 140 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 63 50 34'},
                    iconElement: {xywh: '26 63 16 16'},
                    textElement: {xywh: '9 81 50 16'},
                    arrowElement: {xywh: '64 72 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 55 50 34'},
                    iconElement: {xywh: '26 55 16 16'},
                    textElement: {xywh: '9 73 50 16'},
                    arrowElement: {xywh: '26 140 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 63 50 34'},
                    iconElement: {xywh: '26 63 16 16'},
                    textElement: {xywh: '9 81 50 16'},
                    arrowElement: {xywh: '64 72 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 55 50 34'},
                    iconElement: {xywh: '26 55 16 16'},
                    textElement: {xywh: '9 73 50 16'},
                    arrowElement: {xywh: '26 140 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 63 50 34'},
                    iconElement: {xywh: '26 63 16 16'},
                    textElement: {xywh: '9 81 50 16'},
                    arrowElement: {xywh: '64 72 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 55 50 34'},
                    iconElement: {xywh: '26 55 16 16'},
                    textElement: {xywh: '9 73 50 16'},
                    arrowElement: {xywh: '26 140 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 110 160'},
                    bodyElement: {xywh: '9 72 71 16'},
                    iconElement: {xywh: '64 72 16 16'},
                    textElement: {xywh: '9 72 50 16'},
                    arrowElement: {xywh: '85 72 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 64 71 16'},
                    iconElement: {xywh: '64 64 16 16'},
                    textElement: {xywh: '9 64 50 16'},
                    arrowElement: {xywh: '37 140 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 110 160'},
                    bodyElement: {xywh: '9 72 71 16'},
                    iconElement: {xywh: '64 72 16 16'},
                    textElement: {xywh: '9 72 50 16'},
                    arrowElement: {xywh: '85 72 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 64 71 16'},
                    iconElement: {xywh: '64 64 16 16'},
                    textElement: {xywh: '9 64 50 16'},
                    arrowElement: {xywh: '37 140 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 110 160'},
                    bodyElement: {xywh: '9 72 71 16'},
                    iconElement: {xywh: '64 72 16 16'},
                    textElement: {xywh: '9 72 50 16'},
                    arrowElement: {xywh: '85 72 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 64 71 16'},
                    iconElement: {xywh: '64 64 16 16'},
                    textElement: {xywh: '9 64 50 16'},
                    arrowElement: {xywh: '37 140 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 63 50 34'},
                    iconElement: {xywh: '26 81 16 16'},
                    textElement: {xywh: '9 63 50 16'},
                    arrowElement: {xywh: '64 72 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 55 50 34'},
                    iconElement: {xywh: '26 73 16 16'},
                    textElement: {xywh: '9 55 50 16'},
                    arrowElement: {xywh: '26 140 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 63 50 34'},
                    iconElement: {xywh: '26 81 16 16'},
                    textElement: {xywh: '9 63 50 16'},
                    arrowElement: {xywh: '64 72 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 55 50 34'},
                    iconElement: {xywh: '26 73 16 16'},
                    textElement: {xywh: '9 55 50 16'},
                    arrowElement: {xywh: '26 140 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 89 160'},
                    bodyElement: {xywh: '9 63 50 34'},
                    iconElement: {xywh: '26 81 16 16'},
                    textElement: {xywh: '9 63 50 16'},
                    arrowElement: {xywh: '64 72 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 68 160'},
                    bodyElement: {xywh: '9 55 50 34'},
                    iconElement: {xywh: '26 73 16 16'},
                    textElement: {xywh: '9 55 50 16'},
                    arrowElement: {xywh: '26 140 16 16'}
                });
            });
        });
    });

    describe('flexed width - smaller than content', function () {
        function create(config) {
            button = new Ext.Button(Ext.apply({
                flex: 1
            }, config));

            container = new Ext.Container({
                renderTo: Ext.getBody(),
                layout: 'hbox',
                height: height,
                width: width,
                items: [button]
            });
        }

        beforeEach(function () {
            width = 50;
        });

        describe('text only', function () {
            it('should layout with textAlign: left', function () {
                create({
                    text: textHtml,
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with textAlign: center', function () {
                create({
                    text: textHtml,
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with textAlign: right', function () {
                create({
                    text: textHtml,
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {d: false}
                });
            });
        });

        describe('icon only', function () {
            beforeEach(function () {
                width = 20;
            });

            it('should layout with iconAlign: left', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 24'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: top', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'top'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 24'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 24'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    iconAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 24'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {d: false}
                });
            });
        });

        describe('icon and text', function () {
            it('should layout with iconAlign: left and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 11 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: left and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 11 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: left and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'left',
                    textAlign: 'right'
                });

            });

            it('should layout with iconAlign: top and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'left'
                });

            });

            it('should layout with iconAlign: top and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 42'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 32 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: top and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'top',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 42'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 32 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {xywh: '25 4 16 16'},
                    textElement: {xywh: '9 4 11 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {xywh: '25 4 16 16'},
                    textElement: {xywh: '9 4 11 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: right and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'right',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {xywh: '25 4 16 16'},
                    textElement: {xywh: '9 4 11 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom and textAlign: left', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'left'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 42'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom and textAlign: center', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'center'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 42'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {d: false}
                });
            });

            it('should layout with iconAlign: bottom and textAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    iconAlign: 'bottom',
                    textAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 42'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {d: false}
                });
            });
        });

        describe('icon and arrow', function () {
            it('should layout with iconAlign: left and arrowAlign: right', function () {
                width = 40;

                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 40 24'},
                    bodyElement: {xywh: '4 4 11 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '20 4 16 16'}
                });
            });

            it('should layout with iconAlign: left and arrowAlign: bottom', function () {
                width = 20;

                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 40'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '2 20 16 16'}
                });
            });

            it('should layout with iconAlign: top and arrowAlign: right', function () {
                width = 40;

                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 40 24'},
                    bodyElement: {xywh: '4 4 11 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '20 4 16 16'}
                });
            });

            it('should layout with iconAlign: top and arrowAlign: bottom', function () {
                width = 20;

                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'top',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 40'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '2 20 16 16'}
                });
            });

            it('should layout with iconAlign: right and arrowAlign: right', function () {
                width = 40;

                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 40 24'},
                    bodyElement: {xywh: '4 4 11 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '20 4 16 16'}
                });
            });

            it('should layout with iconAlign: right and arrowAlign: bottom', function () {
                width = 20;

                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 40'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '2 20 16 16'}
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: right', function () {
                width = 40;

                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 40 24'},
                    bodyElement: {xywh: '4 4 11 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '20 4 16 16'}
                });
            });

            it('should layout with iconAlign: bottom and arrowAlign: bottom', function () {
                width = 20;

                create({
                    iconCls: iconCls,
                    arrow: true,
                    iconAlign: 'bottom',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 20 40'},
                    bodyElement: {xywh: '4 4 12 16'},
                    iconElement: {xywh: '2 4 16 16'},
                    textElement: {d: false},
                    arrowElement: {xywh: '2 20 16 16'}
                });
            });
        });

        describe('text and arrow', function () {
            it('should layout with textAlign: left and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 11 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 11 16'},
                    arrowElement: {xywh: '25 4 16 16'}
                });
            });

            it('should layout with textAlign: left and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 40'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {xywh: '17 20 16 16'}
                });
            });

            it('should layout with textAlign: center and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 11 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 11 16'},
                    arrowElement: {xywh: '25 4 16 16'}
                });
            });

            it('should layout with textAlign: center and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 40'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {xywh: '17 20 16 16'}
                });
            });

            it('should layout with textAlign: right and arrowAlign: right', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 24'},
                    bodyElement: {xywh: '9 4 11 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 11 16'},
                    arrowElement: {xywh: '25 4 16 16'}
                });
            });

            it('should layout with textAlign: right and arrowAlign: bottom', function () {
                create({
                    text: textHtml,
                    arrow: true,
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 40'},
                    bodyElement: {xywh: '9 4 32 16'},
                    iconElement: {d: false},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {xywh: '17 20 16 16'}
                });
            });
        });

        describe('icon, text, and arrow', function () {
            beforeEach(function () {
                width = 70;
            });

            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 24'},
                    bodyElement: {xywh: '9 4 31 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 10 16'},
                    arrowElement: {xywh: '45 4 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 40'},
                    bodyElement: {xywh: '9 4 52 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 31 16'},
                    arrowElement: {xywh: '27 20 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 24'},
                    bodyElement: {xywh: '9 4 31 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 10 16'},
                    arrowElement: {xywh: '45 4 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 40'},
                    bodyElement: {xywh: '9 4 52 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 31 16'},
                    arrowElement: {xywh: '27 20 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 24'},
                    bodyElement: {xywh: '9 4 31 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 10 16'},
                    arrowElement: {xywh: '45 4 16 16'}
                });
            });

            it('should layout with iconAlign: left, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'left',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 40'},
                    bodyElement: {xywh: '9 4 52 16'},
                    iconElement: {xywh: '9 4 16 16'},
                    textElement: {xywh: '30 4 31 16'},
                    arrowElement: {xywh: '27 20 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 42'},
                    bodyElement: {xywh: '9 4 31 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 31 16'},
                    arrowElement: {xywh: '45 13 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: left, and arrowAlign: bottom', function () {
                width = 50;

                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 58'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 32 16'},
                    arrowElement: {xywh: '17 38 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 42'},
                    bodyElement: {xywh: '9 4 31 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 31 16'},
                    arrowElement: {xywh: '45 13 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: center, and arrowAlign: bottom', function () {
                width = 50;

                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 58'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 32 16'},
                    arrowElement: {xywh: '17 38 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 42'},
                    bodyElement: {xywh: '9 4 31 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 31 16'},
                    arrowElement: {xywh: '45 13 16 16'}
                });
            });

            it('should layout with iconAlign: top, textAlign: right, and arrowAlign: bottom', function () {
                width = 50;

                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'top',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 58'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 4 16 16'},
                    textElement: {xywh: '9 22 32 16'},
                    arrowElement: {xywh: '17 38 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 24'},
                    bodyElement: {xywh: '9 4 31 16'},
                    iconElement: {xywh: '24 4 16 16'},
                    textElement: {xywh: '9 4 10 16'},
                    arrowElement: {xywh: '45 4 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: left, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 40'},
                    bodyElement: {xywh: '9 4 52 16'},
                    iconElement: {xywh: '45 4 16 16'},
                    textElement: {xywh: '9 4 31 16'},
                    arrowElement: {xywh: '27 20 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 24'},
                    bodyElement: {xywh: '9 4 31 16'},
                    iconElement: {xywh: '24 4 16 16'},
                    textElement: {xywh: '9 4 10 16'},
                    arrowElement: {xywh: '45 4 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: center, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 40'},
                    bodyElement: {xywh: '9 4 52 16'},
                    iconElement: {xywh: '45 4 16 16'},
                    textElement: {xywh: '9 4 31 16'},
                    arrowElement: {xywh: '27 20 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 24'},
                    bodyElement: {xywh: '9 4 31 16'},
                    iconElement: {xywh: '24 4 16 16'},
                    textElement: {xywh: '9 4 10 16'},
                    arrowElement: {xywh: '45 4 16 16'}
                });
            });

            it('should layout with iconAlign: right, textAlign: right, and arrowAlign: bottom', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'right',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 40'},
                    bodyElement: {xywh: '9 4 52 16'},
                    iconElement: {xywh: '45 4 16 16'},
                    textElement: {xywh: '9 4 31 16'},
                    arrowElement: {xywh: '27 20 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 42'},
                    bodyElement: {xywh: '9 4 31 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 31 16'},
                    arrowElement: {xywh: '45 13 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: left, and arrowAlign: bottom', function () {
                width = 50;

                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'left',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 58'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {xywh: '17 38 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: right', function () {
                width = 50;

                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 42'},
                    bodyElement: {xywh: '9 4 11 34'},
                    iconElement: {xywh: '7 22 16 16'},
                    textElement: {xywh: '9 4 11 16'},
                    arrowElement: {xywh: '25 13 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: center, and arrowAlign: bottom', function () {
                width = 50;

                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'center',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 58'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {xywh: '17 38 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: right', function () {
                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'right'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 70 42'},
                    bodyElement: {xywh: '9 4 31 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 31 16'},
                    arrowElement: {xywh: '45 13 16 16'}
                });
            });

            it('should layout with iconAlign: bottom, textAlign: right, and arrowAlign: bottom', function () {
                width = 50;

                create({
                    iconCls: iconCls,
                    text: textHtml,
                    arrow: true,
                    iconAlign: 'bottom',
                    textAlign: 'right',
                    arrowAlign: 'bottom'
                });

                expect(button).toHaveLayout({
                    element: {xywh: '0 0 50 58'},
                    bodyElement: {xywh: '9 4 32 34'},
                    iconElement: {xywh: '17 22 16 16'},
                    textElement: {xywh: '9 4 32 16'},
                    arrowElement: {xywh: '17 38 16 16'}
                });
            });
        });
    });
});
