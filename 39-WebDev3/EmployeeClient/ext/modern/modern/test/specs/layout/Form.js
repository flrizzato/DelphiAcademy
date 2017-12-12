describe('Ext.layout.Form', function() {
    var container;

    afterEach(function() {
        container = Ext.destroy(container);
    });

    it('should layout with auto-widthed labels', function() {
        container = Ext.create({
            xtype: 'formpanel',
            layout: 'form',
            renderTo: Ext.getBody(),
            width: 800,
            items: [{
                xtype: 'textfield',
                label: 'A',
                errorTarget: 'side'
            }, {
                xtype: 'textfield',
                label: '<div style="display:inline-block;width:300px;background:green;">B</div>'
            }, {
                xtype: 'checkboxfield',
                labelWidth: 400, // should be ignored
                label: 'C',
                boxLabel: '<div style="display:inline-block;width:50px;background:yellow;">D</div>'
            }]
        });

        container.getAt(0).setError('Error');

        expect(container).toHaveLayout({
            element: { xywh: '0 0 800 104' },
            bodyWrapElement: { xywh: '0 0 800 104' },
            bodyElement: { xywh: '0 0 800 104' },
            items: {
                0: {
                    labelElement: { xywh: '8 8 306 24' },
                    bodyWrapElement: { xywh: '322 8 470 24' },
                    bodyElement: { xywh: '322 8 444 24' },
                    inputWrapElement: { xywh: '322 8 444 24' },
                    beforeInputElement: { xywh: '323 9 0 22' },
                    inputElement: { xywh: '323 9 442 22' },
                    afterInputElement: { xywh: '765 9 0 22' },
                    errorElement: { xywh: '766 8 26 24' },
                    errorIconElement: { xywh: '771 12 16 16' },
                    errorMessageElement: { d: false }
                },
                1: {
                    labelElement: { xywh: '8 40 306 24' },
                    bodyWrapElement: { xywh: '322 40 470 24' },
                    bodyElement: { xywh: '322 40 470 24' },
                    inputWrapElement: { xywh: '322 40 470 24' },
                    beforeInputElement: { xywh: '323 41 0 22' },
                    inputElement: { xywh: '323 41 468 22' },
                    afterInputElement: { xywh: '791 41 0 22' },
                    errorElement: { xywh: '792 40 0 24' },
                    errorIconElement: { d: false },
                    errorMessageElement: { d: false }
                },
                2: {
                    labelElement: { xywh: '8 72 306 24' },
                    bodyWrapElement: { xywh: '322 72 470 24' },
                    bodyElement: { xywh: '322 72 470 24' },
                    boxWrapElement: { xywh: '322 72 470 24' },
                    iconElement: { xywh: '322 76 16 16' },
                    inputElement: { xywh: '322 76 16 16' },
                    boxLabelElement: { xywh: '338 72 54 24' },
                    errorElement: { xywh: '792 72 0 24' },
                    errorIconElement: { d: false },
                    errorMessageElement: { d: false }
                }
            }
        });
    });

    it('should size all labels to the configured labelWidth', function() {
        container = Ext.create({
            xtype: 'formpanel',
            layout: {
                type: 'form',
                labelWidth: 300
            },
            renderTo: Ext.getBody(),
            width: 800,
            items: [{
                xtype: 'textfield',
                label: 'A',
                errorTarget: 'side'
            }, {
                xtype: 'textfield',
                label: 'B'
            }, {
                xtype: 'checkboxfield',
                label: 'Really really really very very very super uber ultra mega bigly long label text',
                boxLabel: '<div style="display:inline-block;width:50px;background:yellow;">D</div>'
            }]
        });

        container.getAt(0).setError('Error');

        expect(container).toHaveLayout({
            element: { xywh: '0 0 800 104' },
            bodyWrapElement: { xywh: '0 0 800 104' },
            bodyElement: { xywh: '0 0 800 104' },
            items: {
                0: {
                    labelElement: { xywh: '8 8 300 24' },
                    bodyWrapElement: { xywh: '316 8 476 24' },
                    bodyElement: { xywh: '316 8 450 24' },
                    inputWrapElement: { xywh: '316 8 450 24' },
                    beforeInputElement: { xywh: '317 9 0 22' },
                    inputElement: { xywh: '317 9 448 22' },
                    afterInputElement: { xywh: '765 9 0 22' },
                    errorElement: { xywh: '766 8 26 24' },
                    errorIconElement: { xywh: '771 12 16 16' },
                    errorMessageElement: { d: false }
                },
                1: {
                    labelElement: { xywh: '8 40 300 24' },
                    bodyWrapElement: { xywh: '316 40 476 24' },
                    bodyElement: { xywh: '316 40 476 24' },
                    inputWrapElement: { xywh: '316 40 476 24' },
                    beforeInputElement: { xywh: '317 41 0 22' },
                    inputElement: { xywh: '317 41 474 22' },
                    afterInputElement: { xywh: '791 41 0 22' },
                    errorElement: { xywh: '792 40 0 24' },
                    errorIconElement: { d: false },
                    errorMessageElement: { d: false }
                },
                2: {
                    labelElement: { xywh: '8 72 300 24' },
                    bodyWrapElement: { xywh: '316 72 476 24' },
                    bodyElement: { xywh: '316 72 476 24' },
                    boxWrapElement: { xywh: '316 72 476 24' },
                    iconElement: { xywh: '316 76 16 16' },
                    inputElement: { xywh: '316 76 16 16' },
                    boxLabelElement: { xywh: '332 72 54 24' },
                    errorElement: { xywh: '792 72 0 24' },
                    errorIconElement: { d: false },
                    errorMessageElement: { d: false }
                }
            }
        });
    });

    it('should allow text field with configured height', function() {
        container = Ext.create({
            xtype: 'formpanel',
            layout: 'form',
            renderTo: Ext.getBody(),
            width: 400,
            items: [{
                xtype: 'textfield',
                label: '<div style="display:inline-block;width:50px;background:green;">A</div>',
                height: 100
            }]
        });

        expect(container).toHaveLayout({
            element: { xywh: '0 0 400 116' },
            bodyWrapElement: { xywh: '0 0 400 116' },
            bodyElement: { xywh: '0 0 400 116' },
            items: {
                0: {
                    labelElement: { xywh: '8 8 56 100' },
                    bodyWrapElement: { xywh: '72 8 320 100' },
                    bodyElement: { xywh: '72 8 320 100' },
                    inputWrapElement: { xywh: '72 8 320 100' },
                    beforeInputElement: { xywh: '73 9 0 98' },
                    inputElement: { xywh: '73 9 318 98' },
                    afterInputElement: { xywh: '391 9 0 98' }
                }
            }
        });
    });

    it('should allow textarea with configured height', function() {
        container = Ext.create({
            xtype: 'formpanel',
            layout: 'form',
            renderTo: Ext.getBody(),
            width: 400,
            items: [{
                xtype: 'textareafield',
                label: '<div style="display:inline-block;width:50px;background:green;">A</div>',
                height: 100
            }]
        });

        expect(container).toHaveLayout({
            element: { xywh: '0 0 400 116' },
            bodyWrapElement: { xywh: '0 0 400 116' },
            bodyElement: { xywh: '0 0 400 116' },
            items: {
                0: {
                    labelElement: { xywh: '8 8 56 100' },
                    bodyWrapElement: { xywh: '72 8 320 100' },
                    bodyElement: { xywh: '72 8 320 100' },
                    inputWrapElement: { xywh: '72 8 320 100' },
                    beforeInputElement: { xywh: '73 9 0 98' },
                    inputElement: { xywh: '73 9 318 98' },
                    afterInputElement: { xywh: '391 9 0 98' }
                }
            }
        });
    });

    xit('should allow containerfield as a child item', function () {
        container = Ext.create({
            xtype: 'formpanel',
            layout: {
                type: 'form',
                labelWidth: 100
            },
            renderTo: Ext.getBody(),
            width: 400,
            items: [{
                xtype: 'containerfield',
                label: 'Test',
                labelAlign: 'left',
                items: [{
                    flex: 1,
                    placeholder: 'first'
                }, {
                    flex: 1,
                    placeholder: 'second'
                }]
            }]
        });

        expect(container).toHaveLayout({
            element: { xywh: '0 0 400 64' },
            bodyWrapElement: { xywh: '0 0 400 64' },
            bodyElement: { xywh: '0 0 400 64' },
            items: {
                0: {
                    labelElement: { xywh: '8 8 100 48' },
                    bodyWrapElement: { xywh: '116 8 276 48' },
                    bodyElement: { xywh: '116 8 276 48' },
                    _container: {
                        items: {
                            0: {
                                bodyWrapElement: { xywh: '6 0 270 24' },
                                bodyElement: { xywh: '6 0 270 24' },
                                inputWrapElement: { xywh: '6 0 270 24' },
                                beforeInputElement: { xywh: '7 1 0 22' },
                                inputElement: { xywh: '7 1 268 22' },
                                afterInputElement: { xywh: '275 1 0 22' }
                            },
                            1: {
                                bodyWrapElement: { xywh: '6 24 270 24' },
                                bodyElement: { xywh: '6 24 270 24' },
                                inputWrapElement: { xywh: '6 24 270 24' },
                                beforeInputElement: { xywh: '7 25 0 22' },
                                inputElement: { xywh: '7 25 268 22' },
                                afterInputElement: { xywh: '275 25 0 22' }
                            }
                        }
                    }
                }
            }
        });
    });

    describe('illegal configurations', function() {
        var field;

        afterEach(function() {
            field = Ext.destroy(field);
        });

        it('should not allow labelAlign: top', function() {
            container = Ext.create({
                xtype: 'container',
                layout: 'form'
            });

            field = Ext.create({
                xtype: 'textfield',
                labelAlign: 'top'
            });

            container.add(field);

            expect(field.getLabelAlign()).toBe('left');
        });

        it('should not allow labelAlign: right', function() {
            container = Ext.create({
                xtype: 'container',
                layout: 'form'
            });

            field = Ext.create({
                xtype: 'textfield',
                labelAlign: 'right'
            });

            container.add(field);

            expect(field.getLabelAlign()).toBe('left');
        });

        it('should not allow labelAlign: bottom', function() {
            container = Ext.create({
                xtype: 'container',
                layout: 'form'
            });

            field = Ext.create({
                xtype: 'textfield',
                labelAlign: 'bottom'
            });

            container.add(field);

            expect(field.getLabelAlign()).toBe('left');
        });

        it('should not allow errorTarget: under', function() {
            container = Ext.create({
                xtype: 'container',
                layout: 'form'
            });

            field = Ext.create({
                xtype: 'textfield',
                errorTarget: 'under'
            });

            container.add(field);

            expect(field.getErrorTarget()).toBe('side');
        });
    });

});
