describe('Ext.panel.Panel_layout', function () {
    var shortText = 'MMMMMMMM',
        mediumText = 'MMMMMMMMMMMMMMMMMMMM',
        longText = 'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM',
        iconCls = 'x-fa fa-home',
        width = null,
        height = null,
        style = null,
        title, panel, header, letterSpacing;

    function create(config) {
        panel = new Ext.Panel(Ext.apply({
            renderTo: Ext.getBody(),
            iconCls: iconCls,
            title: title,
            width: width,
            height: height,
            style: style,
            tools: [{
                type: 'maximize'
            }, {
                type: 'close'
            }]
        }, config));

        header = panel.getHeader();
    }

    beforeAll(function() {
        var el = document.createElement('div');

        el.style.display = 'inline-block';
        el.style.fontSize = '5px';
        el.style.fontFamily = 'monospace';
        el.style.letterSpacing = 0;
        el.innerHTML = 'MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM';

        document.body.appendChild(el);

        // calculate letter-spacing needed for 6px character width across all browsers
        letterSpacing = (600 - el.offsetWidth) / 100;

        document.body.removeChild(el);
    });

    beforeEach(function () {
        Ext.util.CSS.createStyleSheet(
            // make the title text el have a 6px character width so we get the same
            // measurements across all browsers
            '.x-paneltitle .x-text-el { font-size:5px!important;font-family:monospace!important;letter-spacing:' + letterSpacing + 'px!important; }',
            'panelTitleStyleSheet'
        );
    });

    afterEach(function () {
        Ext.util.CSS.removeStyleSheet('panelTitleStyleSheet');

        panel = Ext.destroy(panel);

        width = height = null;
    });

    describe('configured size', function() {
        beforeEach(function() {
            width = height = 200;
        });
        
        describe('short text', function() {
            beforeEach(function() {
                title = shortText;
            });
            
            describe('headerPosition: top', function() {
                it('should layout with titleAlign: left and iconAlign: left', function() {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'left',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '10 10 16 16' },
                                textElement: { xywh: '32 10 48 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: top', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'left',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '26 7 16 16' },
                                textElement: { xywh: '10 25 48 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: right', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'left',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '64 10 16 16' },
                                textElement: { xywh: '10 10 48 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'left',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '26 25 16 16' },
                                textElement: { xywh: '10 7 48 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: left', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'center',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '43 10 16 16' },
                                textElement: { xywh: '65 10 48 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: top', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'center',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '70 7 16 16' },
                                textElement: { xywh: '54 25 48 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: right', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'center',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '97 10 16 16' },
                                textElement: { xywh: '43 10 48 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'center',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '70 25 16 16' },
                                textElement: { xywh: '54 7 48 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: left', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'right',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '76 10 16 16' },
                                textElement: { xywh: '98 10 48 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: top', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'right',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '114 7 16 16' },
                                textElement: { xywh: '98 25 48 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: right', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'right',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '130 10 16 16' },
                                textElement: { xywh: '76 10 48 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'right',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '114 25 16 16' },
                                textElement: { xywh: '98 7 48 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });
            });

            describe('headerPosition: right', function () {
                beforeEach(function() {
                    width = height = 200;
                });

                it('should layout with titleAlign: left and iconAlign: left', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'left',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 10 30 136' },
                                iconElement: { xywh: '10 10 16 16' },
                                textElement: { xywh: '10 32 16 48' }
                            },
                            1: {
                                element: { xywh: '10 152 16 16' }
                            },
                            2: {
                                element: { xywh: '10 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: top', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'left',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 10 48 136' },
                                iconElement: { xywh: '25 26 16 16' },
                                textElement: { xywh: '7 10 16 48' }
                            },
                            1: {
                                element: { xywh: '16 152 16 16' }
                            },
                            2: {
                                element: { xywh: '16 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: right', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'left',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 10 30 136' },
                                iconElement: { xywh: '10 64 16 16' },
                                textElement: { xywh: '10 10 16 48' }
                            },
                            1: {
                                element: { xywh: '10 152 16 16' }
                            },
                            2: {
                                element: { xywh: '10 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'left',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 10 48 136' },
                                iconElement: { xywh: '7 26 16 16' },
                                textElement: { xywh: '25 10 16 48' }
                            },
                            1: {
                                element: { xywh: '16 152 16 16' }
                            },
                            2: {
                                element: { xywh: '16 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: left', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'center',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 10 30 136' },
                                iconElement: { xywh: '10 43 16 16' },
                                textElement: { xywh: '10 65 16 48' }
                            },
                            1: {
                                element: { xywh: '10 152 16 16' }
                            },
                            2: {
                                element: { xywh: '10 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: top', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'center',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 10 48 136' },
                                iconElement: { xywh: '25 70 16 16' },
                                textElement: { xywh: '7 54 16 48' }
                            },
                            1: {
                                element: { xywh: '16 152 16 16' }
                            },
                            2: {
                                element: { xywh: '16 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: right', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'center',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 10 30 136' },
                                iconElement: { xywh: '10 97 16 16' },
                                textElement: { xywh: '10 43 16 48' }
                            },
                            1: {
                                element: { xywh: '10 152 16 16' }
                            },
                            2: {
                                element: { xywh: '10 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'center',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 10 48 136' },
                                iconElement: { xywh: '7 70 16 16' },
                                textElement: { xywh: '25 54 16 48' }
                            },
                            1: {
                                element: { xywh: '16 152 16 16' }
                            },
                            2: {
                                element: { xywh: '16 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: left', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'right',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 10 30 136' },
                                iconElement: { xywh: '10 76 16 16' },
                                textElement: { xywh: '10 98 16 48' }
                            },
                            1: {
                                element: { xywh: '10 152 16 16' }
                            },
                            2: {
                                element: { xywh: '10 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: top', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'right',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 10 48 136' },
                                iconElement: { xywh: '25 114 16 16' },
                                textElement: { xywh: '7 98 16 48' }
                            },
                            1: {
                                element: { xywh: '16 152 16 16' }
                            },
                            2: {
                                element: { xywh: '16 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: right', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'right',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 10 30 136' },
                                iconElement: { xywh: '10 130 16 16' },
                                textElement: { xywh: '10 76 16 48' }
                            },
                            1: {
                                element: { xywh: '10 152 16 16' }
                            },
                            2: {
                                element: { xywh: '10 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'right',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 10 48 136' },
                                iconElement: { xywh: '7 114 16 16' },
                                textElement: { xywh: '25 98 16 48' }
                            },
                            1: {
                                element: { xywh: '16 152 16 16' }
                            },
                            2: {
                                element: { xywh: '16 174 16 16' }
                            }
                        }
                    });
                });
            });

            describe('headerPosition: bottom', function() {
                it('should layout with titleAlign: left and iconAlign: left', function() {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'left',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '10 10 16 16' },
                                textElement: { xywh: '32 10 48 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: top', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'left',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '26 7 16 16' },
                                textElement: { xywh: '10 25 48 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: right', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'left',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '64 10 16 16' },
                                textElement: { xywh: '10 10 48 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'left',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '26 25 16 16' },
                                textElement: { xywh: '10 7 48 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: left', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'center',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '43 10 16 16' },
                                textElement: { xywh: '65 10 48 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: top', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'center',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '70 7 16 16' },
                                textElement: { xywh: '54 25 48 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: right', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'center',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '97 10 16 16' },
                                textElement: { xywh: '43 10 48 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'center',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '70 25 16 16' },
                                textElement: { xywh: '54 7 48 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: left', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'right',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '76 10 16 16' },
                                textElement: { xywh: '98 10 48 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: top', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'right',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '114 7 16 16' },
                                textElement: { xywh: '98 25 48 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: right', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'right',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '130 10 16 16' },
                                textElement: { xywh: '76 10 48 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'right',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '114 25 16 16' },
                                textElement: { xywh: '98 7 48 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });
            });

            describe('headerPosition: left', function () {
                it('should layout with titleAlign: left and iconAlign: left', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'left',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 54 30 136' },
                                iconElement: { xywh: '10 174 16 16' },
                                textElement: { xywh: '10 120 16 48' }
                            },
                            1: {
                                element: { xywh: '10 32 16 16' }
                            },
                            2: {
                                element: { xywh: '10 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: top', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'left',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 54 48 136' },
                                iconElement: { xywh: '7 158 16 16' },
                                textElement: { xywh: '25 142 16 48' }
                            },
                            1: {
                                element: { xywh: '16 32 16 16' }
                            },
                            2: {
                                element: { xywh: '16 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: right', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'left',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 54 30 136' },
                                iconElement: { xywh: '10 120 16 16' },
                                textElement: { xywh: '10 142 16 48' }
                            },
                            1: {
                                element: { xywh: '10 32 16 16' }
                            },
                            2: {
                                element: { xywh: '10 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'left',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 54 48 136' },
                                iconElement: { xywh: '25 158 16 16' },
                                textElement: { xywh: '7 142 16 48' }
                            },
                            1: {
                                element: { xywh: '16 32 16 16' }
                            },
                            2: {
                                element: { xywh: '16 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: left', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'center',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 54 30 136' },
                                iconElement: { xywh: '10 141 16 16' },
                                textElement: { xywh: '10 87 16 48' }
                            },
                            1: {
                                element: { xywh: '10 32 16 16' }
                            },
                            2: {
                                element: { xywh: '10 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: top', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'center',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 54 48 136' },
                                iconElement: { xywh: '7 114 16 16' },
                                textElement: { xywh: '25 98 16 48' }
                            },
                            1: {
                                element: { xywh: '16 32 16 16' }
                            },
                            2: {
                                element: { xywh: '16 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: right', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'center',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 54 30 136' },
                                iconElement: { xywh: '10 87 16 16' },
                                textElement: { xywh: '10 109 16 48' }
                            },
                            1: {
                                element: { xywh: '10 32 16 16' }
                            },
                            2: {
                                element: { xywh: '10 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'center',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 54 48 136' },
                                iconElement: { xywh: '25 114 16 16' },
                                textElement: { xywh: '7 98 16 48' }
                            },
                            1: {
                                element: { xywh: '16 32 16 16' }
                            },
                            2: {
                                element: { xywh: '16 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: left', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'right',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 54 30 136' },
                                iconElement: { xywh: '10 108 16 16' },
                                textElement: { xywh: '10 54 16 48' }
                            },
                            1: {
                                element: { xywh: '10 32 16 16' }
                            },
                            2: {
                                element: { xywh: '10 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: top', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'right',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 54 48 136' },
                                iconElement: { xywh: '7 70 16 16' },
                                textElement: { xywh: '25 54 16 48' }
                            },
                            1: {
                                element: { xywh: '16 32 16 16' }
                            },
                            2: {
                                element: { xywh: '16 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: right', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'right',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 54 30 136' },
                                iconElement: { xywh: '10 54 16 16' },
                                textElement: { xywh: '10 76 16 48' }
                            },
                            1: {
                                element: { xywh: '10 32 16 16' }
                            },
                            2: {
                                element: { xywh: '10 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'right',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 54 48 136' },
                                iconElement: { xywh: '25 70 16 16' },
                                textElement: { xywh: '7 54 16 48' }
                            },
                            1: {
                                element: { xywh: '16 32 16 16' }
                            },
                            2: {
                                element: { xywh: '16 10 16 16' }
                            }
                        }
                    });
                });
            });
        });

        describe('long text', function() {
            beforeEach(function () {
                title = longText;
            });

            describe('headerPosition: top', function () {
                it('should layout with titleAlign: left and iconAlign: left', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'left',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '10 10 16 16' },
                                textElement: { xywh: '32 10 114 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: top', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'left',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '70 7 16 16' },
                                textElement: { xywh: '10 25 136 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: right', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'left',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '130 10 16 16' },
                                textElement: { xywh: '10 10 114 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'left',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '70 25 16 16' },
                                textElement: { xywh: '10 7 136 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: left', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'center',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '10 10 16 16' },
                                textElement: { xywh: '32 10 114 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: top', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'center',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '70 7 16 16' },
                                textElement: { xywh: '10 25 136 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: right', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'center',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '130 10 16 16' },
                                textElement: { xywh: '10 10 114 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'center',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '70 25 16 16' },
                                textElement: { xywh: '10 7 136 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: left', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'right',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '10 10 16 16' },
                                textElement: { xywh: '32 10 114 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: top', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'right',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '70 7 16 16' },
                                textElement: { xywh: '10 25 136 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: right', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'right',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 36' },
                        items: {
                            0: {
                                element: { xywh: '10 3 136 30' },
                                iconElement: { xywh: '130 10 16 16' },
                                textElement: { xywh: '10 10 114 16' }
                            },
                            1: {
                                element: { xywh: '152 10 16 16' }
                            },
                            2: {
                                element: { xywh: '174 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'top',
                        titleAlign: 'right',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 200 48' },
                        items: {
                            0: {
                                element: { xywh: '10 0 136 48' },
                                iconElement: { xywh: '70 25 16 16' },
                                textElement: { xywh: '10 7 136 16' }
                            },
                            1: {
                                element: { xywh: '152 16 16 16' }
                            },
                            2: {
                                element: { xywh: '174 16 16 16' }
                            }
                        }
                    });
                });
            });

            describe('headerPosition: right', function () {
                it('should layout with titleAlign: left and iconAlign: left', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'left',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 10 30 136' },
                                iconElement: { xywh: '10 10 16 16' },
                                textElement: { xywh: '10 32 16 114' }
                            },
                            1: {
                                element: { xywh: '10 152 16 16' }
                            },
                            2: {
                                element: { xywh: '10 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: top', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'left',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 10 48 136' },
                                iconElement: { xywh: '25 70 16 16' },
                                textElement: { xywh: '7 10 16 136' }
                            },
                            1: {
                                element: { xywh: '16 152 16 16' }
                            },
                            2: {
                                element: { xywh: '16 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: right', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'left',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 10 30 136' },
                                iconElement: { xywh: '10 130 16 16' },
                                textElement: { xywh: '10 10 16 114' }
                            },
                            1: {
                                element: { xywh: '10 152 16 16' }
                            },
                            2: {
                                element: { xywh: '10 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'left',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 10 48 136' },
                                iconElement: { xywh: '7 70 16 16' },
                                textElement: { xywh: '25 10 16 136' }
                            },
                            1: {
                                element: { xywh: '16 152 16 16' }
                            },
                            2: {
                                element: { xywh: '16 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: left', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'center',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 10 30 136' },
                                iconElement: { xywh: '10 10 16 16' },
                                textElement: { xywh: '10 32 16 114' }
                            },
                            1: {
                                element: { xywh: '10 152 16 16' }
                            },
                            2: {
                                element: { xywh: '10 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: top', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'center',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 10 48 136' },
                                iconElement: { xywh: '25 70 16 16' },
                                textElement: { xywh: '7 10 16 136' }
                            },
                            1: {
                                element: { xywh: '16 152 16 16' }
                            },
                            2: {
                                element: { xywh: '16 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: right', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'center',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 10 30 136' },
                                iconElement: { xywh: '10 130 16 16' },
                                textElement: { xywh: '10 10 16 114' }
                            },
                            1: {
                                element: { xywh: '10 152 16 16' }
                            },
                            2: {
                                element: { xywh: '10 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'center',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 10 48 136' },
                                iconElement: { xywh: '7 70 16 16' },
                                textElement: { xywh: '25 10 16 136' }
                            },
                            1: {
                                element: { xywh: '16 152 16 16' }
                            },
                            2: {
                                element: { xywh: '16 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: left', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'right',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 10 30 136' },
                                iconElement: { xywh: '10 10 16 16' },
                                textElement: { xywh: '10 32 16 114' }
                            },
                            1: {
                                element: { xywh: '10 152 16 16' }
                            },
                            2: {
                                element: { xywh: '10 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: top', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'right',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 10 48 136' },
                                iconElement: { xywh: '25 70 16 16' },
                                textElement: { xywh: '7 10 16 136' }
                            },
                            1: {
                                element: { xywh: '16 152 16 16' }
                            },
                            2: {
                                element: { xywh: '16 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: right', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'right',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 10 30 136' },
                                iconElement: { xywh: '10 130 16 16' },
                                textElement: { xywh: '10 10 16 114' }
                            },
                            1: {
                                element: { xywh: '10 152 16 16' }
                            },
                            2: {
                                element: { xywh: '10 174 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'right',
                        titleAlign: 'right',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 10 48 136' },
                                iconElement: { xywh: '7 70 16 16' },
                                textElement: { xywh: '25 10 16 136' }
                            },
                            1: {
                                element: { xywh: '16 152 16 16' }
                            },
                            2: {
                                element: { xywh: '16 174 16 16' }
                            }
                        }
                    });
                });
            });


            describe('headerPosition: bottom', function () {
                it('should layout with titleAlign: left and iconAlign: left', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'left',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: {xywh: '0 0 200 36'},
                        items: {
                            0: {
                                element: {xywh: '10 3 136 30'},
                                iconElement: {xywh: '10 10 16 16'},
                                textElement: {xywh: '32 10 114 16'}
                            },
                            1: {
                                element: {xywh: '152 10 16 16'}
                            },
                            2: {
                                element: {xywh: '174 10 16 16'}
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: top', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'left',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: {xywh: '0 0 200 48'},
                        items: {
                            0: {
                                element: {xywh: '10 0 136 48'},
                                iconElement: {xywh: '70 7 16 16'},
                                textElement: {xywh: '10 25 136 16'}
                            },
                            1: {
                                element: {xywh: '152 16 16 16'}
                            },
                            2: {
                                element: {xywh: '174 16 16 16'}
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: right', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'left',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: {xywh: '0 0 200 36'},
                        items: {
                            0: {
                                element: {xywh: '10 3 136 30'},
                                iconElement: {xywh: '130 10 16 16'},
                                textElement: {xywh: '10 10 114 16'}
                            },
                            1: {
                                element: {xywh: '152 10 16 16'}
                            },
                            2: {
                                element: {xywh: '174 10 16 16'}
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'left',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: {xywh: '0 0 200 48'},
                        items: {
                            0: {
                                element: {xywh: '10 0 136 48'},
                                iconElement: {xywh: '70 25 16 16'},
                                textElement: {xywh: '10 7 136 16'}
                            },
                            1: {
                                element: {xywh: '152 16 16 16'}
                            },
                            2: {
                                element: {xywh: '174 16 16 16'}
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: left', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'center',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: {xywh: '0 0 200 36'},
                        items: {
                            0: {
                                element: {xywh: '10 3 136 30'},
                                iconElement: {xywh: '10 10 16 16'},
                                textElement: {xywh: '32 10 114 16'}
                            },
                            1: {
                                element: {xywh: '152 10 16 16'}
                            },
                            2: {
                                element: {xywh: '174 10 16 16'}
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: top', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'center',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: {xywh: '0 0 200 48'},
                        items: {
                            0: {
                                element: {xywh: '10 0 136 48'},
                                iconElement: {xywh: '70 7 16 16'},
                                textElement: {xywh: '10 25 136 16'}
                            },
                            1: {
                                element: {xywh: '152 16 16 16'}
                            },
                            2: {
                                element: {xywh: '174 16 16 16'}
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: right', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'center',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: {xywh: '0 0 200 36'},
                        items: {
                            0: {
                                element: {xywh: '10 3 136 30'},
                                iconElement: {xywh: '130 10 16 16'},
                                textElement: {xywh: '10 10 114 16'}
                            },
                            1: {
                                element: {xywh: '152 10 16 16'}
                            },
                            2: {
                                element: {xywh: '174 10 16 16'}
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'center',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: {xywh: '0 0 200 48'},
                        items: {
                            0: {
                                element: {xywh: '10 0 136 48'},
                                iconElement: {xywh: '70 25 16 16'},
                                textElement: {xywh: '10 7 136 16'}
                            },
                            1: {
                                element: {xywh: '152 16 16 16'}
                            },
                            2: {
                                element: {xywh: '174 16 16 16'}
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: left', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'right',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: {xywh: '0 0 200 36'},
                        items: {
                            0: {
                                element: {xywh: '10 3 136 30'},
                                iconElement: {xywh: '10 10 16 16'},
                                textElement: {xywh: '32 10 114 16'}
                            },
                            1: {
                                element: {xywh: '152 10 16 16'}
                            },
                            2: {
                                element: {xywh: '174 10 16 16'}
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: top', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'right',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: {xywh: '0 0 200 48'},
                        items: {
                            0: {
                                element: {xywh: '10 0 136 48'},
                                iconElement: {xywh: '70 7 16 16'},
                                textElement: {xywh: '10 25 136 16'}
                            },
                            1: {
                                element: {xywh: '152 16 16 16'}
                            },
                            2: {
                                element: {xywh: '174 16 16 16'}
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: right', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'right',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: {xywh: '0 0 200 36'},
                        items: {
                            0: {
                                element: {xywh: '10 3 136 30'},
                                iconElement: {xywh: '130 10 16 16'},
                                textElement: {xywh: '10 10 114 16'}
                            },
                            1: {
                                element: {xywh: '152 10 16 16'}
                            },
                            2: {
                                element: {xywh: '174 10 16 16'}
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'bottom',
                        titleAlign: 'right',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: {xywh: '0 0 200 48'},
                        items: {
                            0: {
                                element: {xywh: '10 0 136 48'},
                                iconElement: {xywh: '70 25 16 16'},
                                textElement: {xywh: '10 7 136 16'}
                            },
                            1: {
                                element: {xywh: '152 16 16 16'}
                            },
                            2: {
                                element: {xywh: '174 16 16 16'}
                            }
                        }
                    });
                });
            });

            describe('headerPosition: left', function () {
                it('should layout with titleAlign: left and iconAlign: left', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'left',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 54 30 136' },
                                iconElement: { xywh: '10 174 16 16' },
                                textElement: { xywh: '10 54 16 114' }
                            },
                            1: {
                                element: { xywh: '10 32 16 16' }
                            },
                            2: {
                                element: { xywh: '10 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: top', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'left',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 54 48 136' },
                                iconElement: { xywh: '7 114 16 16' },
                                textElement: { xywh: '25 54 16 136' }
                            },
                            1: {
                                element: { xywh: '16 32 16 16' }
                            },
                            2: {
                                element: { xywh: '16 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: right', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'left',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 54 30 136' },
                                iconElement: { xywh: '10 54 16 16' },
                                textElement: { xywh: '10 76 16 114' }
                            },
                            1: {
                                element: { xywh: '10 32 16 16' }
                            },
                            2: {
                                element: { xywh: '10 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: left and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'left',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 54 48 136' },
                                iconElement: { xywh: '25 114 16 16' },
                                textElement: { xywh: '7 54 16 136' }
                            },
                            1: {
                                element: { xywh: '16 32 16 16' }
                            },
                            2: {
                                element: { xywh: '16 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: left', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'center',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 54 30 136' },
                                iconElement: { xywh: '10 174 16 16' },
                                textElement: { xywh: '10 54 16 114' }
                            },
                            1: {
                                element: { xywh: '10 32 16 16' }
                            },
                            2: {
                                element: { xywh: '10 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: top', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'center',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 54 48 136' },
                                iconElement: { xywh: '7 114 16 16' },
                                textElement: { xywh: '25 54 16 136' }
                            },
                            1: {
                                element: { xywh: '16 32 16 16' }
                            },
                            2: {
                                element: { xywh: '16 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: right', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'center',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 54 30 136' },
                                iconElement: { xywh: '10 54 16 16' },
                                textElement: { xywh: '10 76 16 114' }
                            },
                            1: {
                                element: { xywh: '10 32 16 16' }
                            },
                            2: {
                                element: { xywh: '10 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: center and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'center',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 54 48 136' },
                                iconElement: { xywh: '25 114 16 16' },
                                textElement: { xywh: '7 54 16 136' }
                            },
                            1: {
                                element: { xywh: '16 32 16 16' }
                            },
                            2: {
                                element: { xywh: '16 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: left', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'right',
                        iconAlign: 'left'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 54 30 136' },
                                iconElement: { xywh: '10 174 16 16' },
                                textElement: { xywh: '10 54 16 114' }
                            },
                            1: {
                                element: { xywh: '10 32 16 16' }
                            },
                            2: {
                                element: { xywh: '10 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: top', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'right',
                        iconAlign: 'top'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 54 48 136' },
                                iconElement: { xywh: '7 114 16 16' },
                                textElement: { xywh: '25 54 16 136' }
                            },
                            1: {
                                element: { xywh: '16 32 16 16' }
                            },
                            2: {
                                element: { xywh: '16 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: right', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'right',
                        iconAlign: 'right'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 36 200' },
                        items: {
                            0: {
                                element: { xywh: '3 54 30 136' },
                                iconElement: { xywh: '10 54 16 16' },
                                textElement: { xywh: '10 76 16 114' }
                            },
                            1: {
                                element: { xywh: '10 32 16 16' }
                            },
                            2: {
                                element: { xywh: '10 10 16 16' }
                            }
                        }
                    });
                });

                it('should layout with titleAlign: right and iconAlign: bottom', function () {
                    create({
                        headerPosition: 'left',
                        titleAlign: 'right',
                        iconAlign: 'bottom'
                    });

                    expect(header).toHaveLayout({
                        element: { xywh: '0 0 48 200' },
                        items: {
                            0: {
                                element: { xywh: '0 54 48 136' },
                                iconElement: { xywh: '25 114 16 16' },
                                textElement: { xywh: '7 54 16 136' }
                            },
                            1: {
                                element: { xywh: '16 32 16 16' }
                            },
                            2: {
                                element: { xywh: '16 10 16 16' }
                            }
                        }
                    });
                });
            });
        });
    });

    describe('shrink wrap size', function () {
        beforeEach(function () {
            title = mediumText;
            style = 'position:absolute';
        });

        describe('headerPosition: top', function () {
            it('should layout with titleAlign: left and iconAlign: left', function () {
                create({
                    headerPosition: 'top',
                    titleAlign: 'left',
                    iconAlign: 'left'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 206 36' },
                    items: {
                        0: {
                            element: { xywh: '10 3 142 30' },
                            iconElement: { xywh: '10 10 16 16' },
                            textElement: { xywh: '32 10 120 16' }
                        },
                        1: {
                            element: { xywh: '158 10 16 16' }
                        },
                        2: {
                            element: { xywh: '180 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: left and iconAlign: top', function () {
                create({
                    headerPosition: 'top',
                    titleAlign: 'left',
                    iconAlign: 'top'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 184 48' },
                    items: {
                        0: {
                            element: { xywh: '10 0 120 48' },
                            iconElement: { xywh: '62 7 16 16' },
                            textElement: { xywh: '10 25 120 16' }
                        },
                        1: {
                            element: { xywh: '136 16 16 16' }
                        },
                        2: {
                            element: { xywh: '158 16 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: left and iconAlign: right', function () {
                create({
                    headerPosition: 'top',
                    titleAlign: 'left',
                    iconAlign: 'right'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 206 36' },
                    items: {
                        0: {
                            element: { xywh: '10 3 142 30' },
                            iconElement: { xywh: '136 10 16 16' },
                            textElement: { xywh: '10 10 120 16' }
                        },
                        1: {
                            element: { xywh: '158 10 16 16' }
                        },
                        2: {
                            element: { xywh: '180 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: left and iconAlign: bottom', function () {
                create({
                    headerPosition: 'top',
                    titleAlign: 'left',
                    iconAlign: 'bottom'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 184 48' },
                    items: {
                        0: {
                            element: { xywh: '10 0 120 48' },
                            iconElement: { xywh: '62 25 16 16' },
                            textElement: { xywh: '10 7 120 16' }
                        },
                        1: {
                            element: { xywh: '136 16 16 16' }
                        },
                        2: {
                            element: { xywh: '158 16 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: left', function () {
                create({
                    headerPosition: 'top',
                    titleAlign: 'center',
                    iconAlign: 'left'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 206 36' },
                    items: {
                        0: {
                            element: { xywh: '10 3 142 30' },
                            iconElement: { xywh: '10 10 16 16' },
                            textElement: { xywh: '32 10 120 16' }
                        },
                        1: {
                            element: { xywh: '158 10 16 16' }
                        },
                        2: {
                            element: { xywh: '180 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: top', function () {
                create({
                    headerPosition: 'top',
                    titleAlign: 'center',
                    iconAlign: 'top'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 184 48' },
                    items: {
                        0: {
                            element: { xywh: '10 0 120 48' },
                            iconElement: { xywh: '62 7 16 16' },
                            textElement: { xywh: '10 25 120 16' }
                        },
                        1: {
                            element: { xywh: '136 16 16 16' }
                        },
                        2: {
                            element: { xywh: '158 16 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: right', function () {
                create({
                    headerPosition: 'top',
                    titleAlign: 'center',
                    iconAlign: 'right'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 206 36' },
                    items: {
                        0: {
                            element: { xywh: '10 3 142 30' },
                            iconElement: { xywh: '136 10 16 16' },
                            textElement: { xywh: '10 10 120 16' }
                        },
                        1: {
                            element: { xywh: '158 10 16 16' }
                        },
                        2: {
                            element: { xywh: '180 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: bottom', function () {
                create({
                    headerPosition: 'top',
                    titleAlign: 'center',
                    iconAlign: 'bottom'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 184 48' },
                    items: {
                        0: {
                            element: { xywh: '10 0 120 48' },
                            iconElement: { xywh: '62 25 16 16' },
                            textElement: { xywh: '10 7 120 16' }
                        },
                        1: {
                            element: { xywh: '136 16 16 16' }
                        },
                        2: {
                            element: { xywh: '158 16 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: left', function () {
                create({
                    headerPosition: 'top',
                    titleAlign: 'right',
                    iconAlign: 'left'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 206 36' },
                    items: {
                        0: {
                            element: { xywh: '10 3 142 30' },
                            iconElement: { xywh: '10 10 16 16' },
                            textElement: { xywh: '32 10 120 16' }
                        },
                        1: {
                            element: { xywh: '158 10 16 16' }
                        },
                        2: {
                            element: { xywh: '180 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: top', function () {
                create({
                    headerPosition: 'top',
                    titleAlign: 'right',
                    iconAlign: 'top'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 184 48' },
                    items: {
                        0: {
                            element: { xywh: '10 0 120 48' },
                            iconElement: { xywh: '62 7 16 16' },
                            textElement: { xywh: '10 25 120 16' }
                        },
                        1: {
                            element: { xywh: '136 16 16 16' }
                        },
                        2: {
                            element: { xywh: '158 16 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: right', function () {
                create({
                    headerPosition: 'top',
                    titleAlign: 'right',
                    iconAlign: 'right'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 206 36' },
                    items: {
                        0: {
                            element: { xywh: '10 3 142 30' },
                            iconElement: { xywh: '136 10 16 16' },
                            textElement: { xywh: '10 10 120 16' }
                        },
                        1: {
                            element: { xywh: '158 10 16 16' }
                        },
                        2: {
                            element: { xywh: '180 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: bottom', function () {
                create({
                    headerPosition: 'top',
                    titleAlign: 'right',
                    iconAlign: 'bottom'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 184 48' },
                    items: {
                        0: {
                            element: { xywh: '10 0 120 48' },
                            iconElement: { xywh: '62 25 16 16' },
                            textElement: { xywh: '10 7 120 16' }
                        },
                        1: {
                            element: { xywh: '136 16 16 16' }
                        },
                        2: {
                            element: { xywh: '158 16 16 16' }
                        }
                    }
                });
            });
        });

        describe('headerPosition: right', function () {
            it('should layout with titleAlign: left and iconAlign: left', function () {
                create({
                    headerPosition: 'right',
                    titleAlign: 'left',
                    iconAlign: 'left'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 36 206' },
                    items: {
                        0: {
                            element: { xywh: '3 10 30 142' },
                            iconElement: { xywh: '10 10 16 16' },
                            textElement: { xywh: '10 32 16 120' }
                        },
                        1: {
                            element: { xywh: '10 158 16 16' }
                        },
                        2: {
                            element: { xywh: '10 180 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: left and iconAlign: top', function () {
                create({
                    headerPosition: 'right',
                    titleAlign: 'left',
                    iconAlign: 'top'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 48 184' },
                    items: {
                        0: {
                            element: { xywh: '0 10 48 120' },
                            iconElement: { xywh: '25 62 16 16' },
                            textElement: { xywh: '7 10 16 120' }
                        },
                        1: {
                            element: { xywh: '16 136 16 16' }
                        },
                        2: {
                            element: { xywh: '16 158 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: left and iconAlign: right', function () {
                create({
                    headerPosition: 'right',
                    titleAlign: 'left',
                    iconAlign: 'right'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 36 206' },
                    items: {
                        0: {
                            element: { xywh: '3 10 30 142' },
                            iconElement: { xywh: '10 136 16 16' },
                            textElement: { xywh: '10 10 16 120' }
                        },
                        1: {
                            element: { xywh: '10 158 16 16' }
                        },
                        2: {
                            element: { xywh: '10 180 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: left and iconAlign: bottom', function () {
                create({
                    headerPosition: 'right',
                    titleAlign: 'left',
                    iconAlign: 'bottom'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 48 184' },
                    items: {
                        0: {
                            element: { xywh: '0 10 48 120' },
                            iconElement: { xywh: '7 62 16 16' },
                            textElement: { xywh: '25 10 16 120' }
                        },
                        1: {
                            element: { xywh: '16 136 16 16' }
                        },
                        2: {
                            element: { xywh: '16 158 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: left', function () {
                create({
                    headerPosition: 'right',
                    titleAlign: 'center',
                    iconAlign: 'left'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 36 206' },
                    items: {
                        0: {
                            element: { xywh: '3 10 30 142' },
                            iconElement: { xywh: '10 10 16 16' },
                            textElement: { xywh: '10 32 16 120' }
                        },
                        1: {
                            element: { xywh: '10 158 16 16' }
                        },
                        2: {
                            element: { xywh: '10 180 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: top', function () {
                create({
                    headerPosition: 'right',
                    titleAlign: 'center',
                    iconAlign: 'top'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 48 184' },
                    items: {
                        0: {
                            element: { xywh: '0 10 48 120' },
                            iconElement: { xywh: '25 62 16 16' },
                            textElement: { xywh: '7 10 16 120' }
                        },
                        1: {
                            element: { xywh: '16 136 16 16' }
                        },
                        2: {
                            element: { xywh: '16 158 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: right', function () {
                create({
                    headerPosition: 'right',
                    titleAlign: 'center',
                    iconAlign: 'right'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 36 206' },
                    items: {
                        0: {
                            element: { xywh: '3 10 30 142' },
                            iconElement: { xywh: '10 136 16 16' },
                            textElement: { xywh: '10 10 16 120' }
                        },
                        1: {
                            element: { xywh: '10 158 16 16' }
                        },
                        2: {
                            element: { xywh: '10 180 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: bottom', function () {
                create({
                    headerPosition: 'right',
                    titleAlign: 'center',
                    iconAlign: 'bottom'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 48 184' },
                    items: {
                        0: {
                            element: { xywh: '0 10 48 120' },
                            iconElement: { xywh: '7 62 16 16' },
                            textElement: { xywh: '25 10 16 120' }
                        },
                        1: {
                            element: { xywh: '16 136 16 16' }
                        },
                        2: {
                            element: { xywh: '16 158 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: left', function () {
                create({
                    headerPosition: 'right',
                    titleAlign: 'right',
                    iconAlign: 'left'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 36 206' },
                    items: {
                        0: {
                            element: { xywh: '3 10 30 142' },
                            iconElement: { xywh: '10 10 16 16' },
                            textElement: { xywh: '10 32 16 120' }
                        },
                        1: {
                            element: { xywh: '10 158 16 16' }
                        },
                        2: {
                            element: { xywh: '10 180 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: top', function () {
                create({
                    headerPosition: 'right',
                    titleAlign: 'right',
                    iconAlign: 'top'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 48 184' },
                    items: {
                        0: {
                            element: { xywh: '0 10 48 120' },
                            iconElement: { xywh: '25 62 16 16' },
                            textElement: { xywh: '7 10 16 120' }
                        },
                        1: {
                            element: { xywh: '16 136 16 16' }
                        },
                        2: {
                            element: { xywh: '16 158 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: right', function () {
                create({
                    headerPosition: 'right',
                    titleAlign: 'right',
                    iconAlign: 'right'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 36 206' },
                    items: {
                        0: {
                            element: { xywh: '3 10 30 142' },
                            iconElement: { xywh: '10 136 16 16' },
                            textElement: { xywh: '10 10 16 120' }
                        },
                        1: {
                            element: { xywh: '10 158 16 16' }
                        },
                        2: {
                            element: { xywh: '10 180 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: bottom', function () {
                create({
                    headerPosition: 'right',
                    titleAlign: 'right',
                    iconAlign: 'bottom'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 48 184' },
                    items: {
                        0: {
                            element: { xywh: '0 10 48 120' },
                            iconElement: { xywh: '7 62 16 16' },
                            textElement: { xywh: '25 10 16 120' }
                        },
                        1: {
                            element: { xywh: '16 136 16 16' }
                        },
                        2: {
                            element: { xywh: '16 158 16 16' }
                        }
                    }
                });
            });
        });

        describe('headerPosition: bottom', function () {
            it('should layout with titleAlign: left and iconAlign: left', function () {
                create({
                    headerPosition: 'bottom',
                    titleAlign: 'left',
                    iconAlign: 'left'
                });

                expect(header).toHaveLayout({
                    element: {xywh: '0 0 206 36'},
                    items: {
                        0: {
                            element: {xywh: '10 3 142 30'},
                            iconElement: {xywh: '10 10 16 16'},
                            textElement: {xywh: '32 10 120 16'}
                        },
                        1: {
                            element: {xywh: '158 10 16 16'}
                        },
                        2: {
                            element: {xywh: '180 10 16 16'}
                        }
                    }
                });
            });

            it('should layout with titleAlign: left and iconAlign: top', function () {
                create({
                    headerPosition: 'bottom',
                    titleAlign: 'left',
                    iconAlign: 'top'
                });

                expect(header).toHaveLayout({
                    element: {xywh: '0 0 184 48'},
                    items: {
                        0: {
                            element: {xywh: '10 0 120 48'},
                            iconElement: {xywh: '62 7 16 16'},
                            textElement: {xywh: '10 25 120 16'}
                        },
                        1: {
                            element: {xywh: '136 16 16 16'}
                        },
                        2: {
                            element: {xywh: '158 16 16 16'}
                        }
                    }
                });
            });

            it('should layout with titleAlign: left and iconAlign: right', function () {
                create({
                    headerPosition: 'bottom',
                    titleAlign: 'left',
                    iconAlign: 'right'
                });

                expect(header).toHaveLayout({
                    element: {xywh: '0 0 206 36'},
                    items: {
                        0: {
                            element: {xywh: '10 3 142 30'},
                            iconElement: {xywh: '136 10 16 16'},
                            textElement: {xywh: '10 10 120 16'}
                        },
                        1: {
                            element: {xywh: '158 10 16 16'}
                        },
                        2: {
                            element: {xywh: '180 10 16 16'}
                        }
                    }
                });
            });

            it('should layout with titleAlign: left and iconAlign: bottom', function () {
                create({
                    headerPosition: 'bottom',
                    titleAlign: 'left',
                    iconAlign: 'bottom'
                });

                expect(header).toHaveLayout({
                    element: {xywh: '0 0 184 48'},
                    items: {
                        0: {
                            element: {xywh: '10 0 120 48'},
                            iconElement: {xywh: '62 25 16 16'},
                            textElement: {xywh: '10 7 120 16'}
                        },
                        1: {
                            element: {xywh: '136 16 16 16'}
                        },
                        2: {
                            element: {xywh: '158 16 16 16'}
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: left', function () {
                create({
                    headerPosition: 'bottom',
                    titleAlign: 'center',
                    iconAlign: 'left'
                });

                expect(header).toHaveLayout({
                    element: {xywh: '0 0 206 36'},
                    items: {
                        0: {
                            element: {xywh: '10 3 142 30'},
                            iconElement: {xywh: '10 10 16 16'},
                            textElement: {xywh: '32 10 120 16'}
                        },
                        1: {
                            element: {xywh: '158 10 16 16'}
                        },
                        2: {
                            element: {xywh: '180 10 16 16'}
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: top', function () {
                create({
                    headerPosition: 'bottom',
                    titleAlign: 'center',
                    iconAlign: 'top'
                });

                expect(header).toHaveLayout({
                    element: {xywh: '0 0 184 48'},
                    items: {
                        0: {
                            element: {xywh: '10 0 120 48'},
                            iconElement: {xywh: '62 7 16 16'},
                            textElement: {xywh: '10 25 120 16'}
                        },
                        1: {
                            element: {xywh: '136 16 16 16'}
                        },
                        2: {
                            element: {xywh: '158 16 16 16'}
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: right', function () {
                create({
                    headerPosition: 'bottom',
                    titleAlign: 'center',
                    iconAlign: 'right'
                });

                expect(header).toHaveLayout({
                    element: {xywh: '0 0 206 36'},
                    items: {
                        0: {
                            element: {xywh: '10 3 142 30'},
                            iconElement: {xywh: '136 10 16 16'},
                            textElement: {xywh: '10 10 120 16'}
                        },
                        1: {
                            element: {xywh: '158 10 16 16'}
                        },
                        2: {
                            element: {xywh: '180 10 16 16'}
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: bottom', function () {
                create({
                    headerPosition: 'bottom',
                    titleAlign: 'center',
                    iconAlign: 'bottom'
                });

                expect(header).toHaveLayout({
                    element: {xywh: '0 0 184 48'},
                    items: {
                        0: {
                            element: {xywh: '10 0 120 48'},
                            iconElement: {xywh: '62 25 16 16'},
                            textElement: {xywh: '10 7 120 16'}
                        },
                        1: {
                            element: {xywh: '136 16 16 16'}
                        },
                        2: {
                            element: {xywh: '158 16 16 16'}
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: left', function () {
                create({
                    headerPosition: 'bottom',
                    titleAlign: 'right',
                    iconAlign: 'left'
                });

                expect(header).toHaveLayout({
                    element: {xywh: '0 0 206 36'},
                    items: {
                        0: {
                            element: {xywh: '10 3 142 30'},
                            iconElement: {xywh: '10 10 16 16'},
                            textElement: {xywh: '32 10 120 16'}
                        },
                        1: {
                            element: {xywh: '158 10 16 16'}
                        },
                        2: {
                            element: {xywh: '180 10 16 16'}
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: top', function () {
                create({
                    headerPosition: 'bottom',
                    titleAlign: 'right',
                    iconAlign: 'top'
                });

                expect(header).toHaveLayout({
                    element: {xywh: '0 0 184 48'},
                    items: {
                        0: {
                            element: {xywh: '10 0 120 48'},
                            iconElement: {xywh: '62 7 16 16'},
                            textElement: {xywh: '10 25 120 16'}
                        },
                        1: {
                            element: {xywh: '136 16 16 16'}
                        },
                        2: {
                            element: {xywh: '158 16 16 16'}
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: right', function () {
                create({
                    headerPosition: 'bottom',
                    titleAlign: 'right',
                    iconAlign: 'right'
                });

                expect(header).toHaveLayout({
                    element: {xywh: '0 0 206 36'},
                    items: {
                        0: {
                            element: {xywh: '10 3 142 30'},
                            iconElement: {xywh: '136 10 16 16'},
                            textElement: {xywh: '10 10 120 16'}
                        },
                        1: {
                            element: {xywh: '158 10 16 16'}
                        },
                        2: {
                            element: {xywh: '180 10 16 16'}
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: bottom', function () {
                create({
                    headerPosition: 'bottom',
                    titleAlign: 'right',
                    iconAlign: 'bottom'
                });

                expect(header).toHaveLayout({
                    element: {xywh: '0 0 184 48'},
                    items: {
                        0: {
                            element: {xywh: '10 0 120 48'},
                            iconElement: {xywh: '62 25 16 16'},
                            textElement: {xywh: '10 7 120 16'}
                        },
                        1: {
                            element: {xywh: '136 16 16 16'}
                        },
                        2: {
                            element: {xywh: '158 16 16 16'}
                        }
                    }
                });
            });
        });

        describe('headerPosition: left', function () {
            it('should layout with titleAlign: left and iconAlign: left', function () {
                create({
                    headerPosition: 'left',
                    titleAlign: 'left',
                    iconAlign: 'left'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 36 206' },
                    items: {
                        0: {
                            element: { xywh: '3 54 30 142' },
                            iconElement: { xywh: '10 180 16 16' },
                            textElement: { xywh: '10 54 16 120' }
                        },
                        1: {
                            element: { xywh: '10 32 16 16' }
                        },
                        2: {
                            element: { xywh: '10 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: left and iconAlign: top', function () {
                create({
                    headerPosition: 'left',
                    titleAlign: 'left',
                    iconAlign: 'top'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 48 184' },
                    items: {
                        0: {
                            element: { xywh: '0 54 48 120' },
                            iconElement: { xywh: '7 106 16 16' },
                            textElement: { xywh: '25 54 16 120' }
                        },
                        1: {
                            element: { xywh: '16 32 16 16' }
                        },
                        2: {
                            element: { xywh: '16 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: left and iconAlign: right', function () {
                create({
                    headerPosition: 'left',
                    titleAlign: 'left',
                    iconAlign: 'right'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 36 206' },
                    items: {
                        0: {
                            element: { xywh: '3 54 30 142' },
                            iconElement: { xywh: '10 54 16 16' },
                            textElement: { xywh: '10 76 16 120' }
                        },
                        1: {
                            element: { xywh: '10 32 16 16' }
                        },
                        2: {
                            element: { xywh: '10 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: left and iconAlign: bottom', function () {
                create({
                    headerPosition: 'left',
                    titleAlign: 'left',
                    iconAlign: 'bottom'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 48 184' },
                    items: {
                        0: {
                            element: { xywh: '0 54 48 120' },
                            iconElement: { xywh: '25 106 16 16' },
                            textElement: { xywh: '7 54 16 120' }
                        },
                        1: {
                            element: { xywh: '16 32 16 16' }
                        },
                        2: {
                            element: { xywh: '16 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: left', function () {
                create({
                    headerPosition: 'left',
                    titleAlign: 'center',
                    iconAlign: 'left'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 36 206' },
                    items: {
                        0: {
                            element: { xywh: '3 54 30 142' },
                            iconElement: { xywh: '10 180 16 16' },
                            textElement: { xywh: '10 54 16 120' }
                        },
                        1: {
                            element: { xywh: '10 32 16 16' }
                        },
                        2: {
                            element: { xywh: '10 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: top', function () {
                create({
                    headerPosition: 'left',
                    titleAlign: 'center',
                    iconAlign: 'top'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 48 184' },
                    items: {
                        0: {
                            element: { xywh: '0 54 48 120' },
                            iconElement: { xywh: '7 106 16 16' },
                            textElement: { xywh: '25 54 16 120' }
                        },
                        1: {
                            element: { xywh: '16 32 16 16' }
                        },
                        2: {
                            element: { xywh: '16 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: right', function () {
                create({
                    headerPosition: 'left',
                    titleAlign: 'center',
                    iconAlign: 'right'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 36 206' },
                    items: {
                        0: {
                            element: { xywh: '3 54 30 142' },
                            iconElement: { xywh: '10 54 16 16' },
                            textElement: { xywh: '10 76 16 120' }
                        },
                        1: {
                            element: { xywh: '10 32 16 16' }
                        },
                        2: {
                            element: { xywh: '10 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: center and iconAlign: bottom', function () {
                create({
                    headerPosition: 'left',
                    titleAlign: 'center',
                    iconAlign: 'bottom'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 48 184' },
                    items: {
                        0: {
                            element: { xywh: '0 54 48 120' },
                            iconElement: { xywh: '25 106 16 16' },
                            textElement: { xywh: '7 54 16 120' }
                        },
                        1: {
                            element: { xywh: '16 32 16 16' }
                        },
                        2: {
                            element: { xywh: '16 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: left', function () {
                create({
                    headerPosition: 'left',
                    titleAlign: 'right',
                    iconAlign: 'left'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 36 206' },
                    items: {
                        0: {
                            element: { xywh: '3 54 30 142' },
                            iconElement: { xywh: '10 180 16 16' },
                            textElement: { xywh: '10 54 16 120' }
                        },
                        1: {
                            element: { xywh: '10 32 16 16' }
                        },
                        2: {
                            element: { xywh: '10 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: top', function () {
                create({
                    headerPosition: 'left',
                    titleAlign: 'right',
                    iconAlign: 'top'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 48 184' },
                    items: {
                        0: {
                            element: { xywh: '0 54 48 120' },
                            iconElement: { xywh: '7 106 16 16' },
                            textElement: { xywh: '25 54 16 120' }
                        },
                        1: {
                            element: { xywh: '16 32 16 16' }
                        },
                        2: {
                            element: { xywh: '16 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: right', function () {
                create({
                    headerPosition: 'left',
                    titleAlign: 'right',
                    iconAlign: 'right'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 36 206' },
                    items: {
                        0: {
                            element: { xywh: '3 54 30 142' },
                            iconElement: { xywh: '10 54 16 16' },
                            textElement: { xywh: '10 76 16 120' }
                        },
                        1: {
                            element: { xywh: '10 32 16 16' }
                        },
                        2: {
                            element: { xywh: '10 10 16 16' }
                        }
                    }
                });
            });

            it('should layout with titleAlign: right and iconAlign: bottom', function () {
                create({
                    headerPosition: 'left',
                    titleAlign: 'right',
                    iconAlign: 'bottom'
                });

                expect(header).toHaveLayout({
                    element: { xywh: '0 0 48 184' },
                    items: {
                        0: {
                            element: { xywh: '0 54 48 120' },
                            iconElement: { xywh: '25 106 16 16' },
                            textElement: { xywh: '7 54 16 120' }
                        },
                        1: {
                            element: { xywh: '16 32 16 16' }
                        },
                        2: {
                            element: { xywh: '16 10 16 16' }
                        }
                    }
                });
            });
        });
    });
});